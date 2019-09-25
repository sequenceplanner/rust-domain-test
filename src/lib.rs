//! The SP domain

#![allow(unused_mut)] // for some reason I get not a correct warning for mut in macros

pub mod values;
pub use values::*;

pub mod predicates;
pub use predicates::*;

pub mod states;
pub use states::*;

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::error;
use std::fmt;



/// The SPNode is tracking the name and the local and global path of an item
/// The SPNode should be wrapped inside the item struct and the item should 
/// also impl the Noder trait.
#[derive(PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct SPNode {
    name: String,
    paths: SPPaths,
}

impl SPNode {
    pub fn new(name: &str) -> SPNode {
        SPNode {
            name: name.to_string(),
            paths: SPPaths::empty(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn local_path(&self) -> &Option<LocalPath> {
        &self.paths.local_path()
    }
    pub fn global_path(&self) -> &Option<GlobalPath> {
        &self.paths.global_path()
    }
    pub fn paths(&self) -> &SPPaths {
        &self.paths
    }

    pub fn update_path(&mut self, paths: &SPPaths) -> SPPaths {
        self.paths.upd(paths);
        self.paths.add(self.name.clone());
        self.paths().clone()
    }

    pub fn is_eq(&self, path: &SPPath) -> bool {
        self.paths().is_eq(path)
    }

    pub fn next_node_in_path(&self, path: &SPPath) -> Option<String> {
        self.paths().next_node_in_path(path)
    }

    fn write_nice(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}{}", self.name, self.paths())
    }
}



impl std::fmt::Display for SPNode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.write_nice(f)
    }
}

impl std::fmt::Debug for SPNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write_nice(f)
    }
}

pub trait Noder {
    fn node(&self) -> &SPNode;
    fn node_mut(&mut self) -> &mut SPNode;
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>>;
    fn update_path_children(&mut self, paths: &SPPaths);
    fn as_ref<'a>(&'a self) -> SPItemRef<'a>;

    fn name(&self) -> &str {&self.node().name}

    /// Finds the item with a specific SPPath. Will only find locals if asked from the
    /// correct resource (or below)
    fn find<'a>(&'a self, path: &SPPath) -> Option<SPItemRef<'a>>  {
        if self.node().is_eq(path) {
            return Some(self.as_ref());
        }
        let next = self.node().next_node_in_path(path);
        if next.is_none() {
            return None
        }
        self.find_child(&next.unwrap(), path)
    }

    /// updates the path of this item and its children
    fn update_path(&mut self, paths: &SPPaths) -> SPPaths {
        let paths = self.node_mut().update_path(paths);
        self.update_path_children(&paths);
        paths
    }
    
}


/// A method used by the items when impl the Noder trait
/// Tries to find an item with the path in a list that incl
/// items that impl Noder
fn find_in_list<'a, T>(
    xs: &'a [T], 
    next: &str, 
    path: &SPPath) 
-> Option<SPItemRef<'a>> where T: Noder {
    for i in xs.iter() {
        if i.node().name() == next {
            if let Some(x) = i.find(path) {
                return Some(x)
            }
        }
    }
    return None
}

/// A method used by the items when impl the Noder trait
/// Updates the path in items in the list of items impl Noder
fn update_path_in_list<'a, T>(
    xs: &'a mut [T], 
    paths: &SPPaths) where T: Noder {
    for i in xs.iter_mut() {
        i.update_path(paths);
    }
}



#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum SPItem {
    Model(Model),
    Resource(Resource),
    Message(Message),
    Topic(Topic),
    Variable(Variable),
    Operation(Operation),
    Ability(Ability),
    Transition(Transition),
    IfThen(IfThen),
    //SOP(SOP),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SPItemRef<'a> {
    Model(&'a Model),
    Resource(&'a Resource),
    Message(&'a Message),
    Topic(&'a Topic),
    Variable(&'a Variable),
    Operation(&'a Operation),
    Ability(&'a Ability),
    Transition(&'a Transition),
    IfThen(&'a IfThen),
    //SOP(SOP),
}

impl Noder for SPItem {
    fn node(&self) -> &SPNode {
        match self {
            SPItem::Model(x) => x.node(),
            SPItem::Resource(x) => x.node(),
            SPItem::Message(x) => x.node(),
            SPItem::Topic(x) => x.node(),
            SPItem::Variable(x) => x.node(),
            SPItem::Operation(x) => x.node(),
            SPItem::Ability(x) => x.node(),
            SPItem::Transition(x) => x.node(),
            SPItem::IfThen(x) => x.node(),
        }
    }
    fn node_mut(&mut self) -> &mut SPNode {
        match self {
            SPItem::Model(ref mut x) => x.node_mut(),
            SPItem::Resource(ref mut x) => x.node_mut(),
            SPItem::Message(ref mut x) => x.node_mut(),
            SPItem::Topic(ref mut x) => x.node_mut(),
            SPItem::Variable(ref mut x) => x.node_mut(),
            SPItem::Operation(ref mut x) => x.node_mut(),
            SPItem::Ability(ref mut x) => x.node_mut(),
            SPItem::Transition(ref mut x) => x.node_mut(),
            SPItem::IfThen(ref mut x) => x.node_mut(),
        }
    }
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        match self {
            SPItem::Model(x) => x.find_child(next, path),
            SPItem::Resource(x) => x.find_child(next, path),
            SPItem::Message(x) => x.find_child(next, path),
            SPItem::Topic(x) => x.find_child(next, path),
            SPItem::Variable(x) => x.find_child(next, path),
            SPItem::Operation(x) => x.find_child(next, path),
            SPItem::Ability(x) => x.find_child(next, path),
            SPItem::Transition(x) => x.find_child(next, path),
            SPItem::IfThen(x) => x.find_child(next, path),
        }
    }
    fn update_path_children(&mut self, paths: &SPPaths) {
        match self {
            SPItem::Model(x) => x.update_path_children(paths),
            SPItem::Resource(x) => x.update_path_children(paths),
            SPItem::Message(x) => x.update_path_children(paths),
            SPItem::Topic(x) => x.update_path_children(paths),
            SPItem::Variable(x) => x.update_path_children(paths),
            SPItem::Operation(x) => x.update_path_children(paths),
            SPItem::Ability(x) => x.update_path_children(paths),
            SPItem::Transition(x) => x.update_path_children(paths),
            SPItem::IfThen(x) => x.update_path_children(paths),
        }
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        match self {
            SPItem::Model(x) => x.as_ref(),
            SPItem::Resource(x) => x.as_ref(),
            SPItem::Message(x) => x.as_ref(),
            SPItem::Topic(x) => x.as_ref(),
            SPItem::Variable(x) => x.as_ref(),
            SPItem::Operation(x) => x.as_ref(),
            SPItem::Ability(x) => x.as_ref(),
            SPItem::Transition(x) => x.as_ref(),
            SPItem::IfThen(x) => x.as_ref(),
        }
    }
}


impl<'a> SPItemRef<'a> {
    pub fn node(&self) -> &SPNode {
        match self {
            SPItemRef::Model(x) => &x.node,
            SPItemRef::Resource(x) => &x.node,
            SPItemRef::Message(x) => &x.node,
            SPItemRef::Topic(x) => &x.node,
            SPItemRef::Variable(x) => &x.node,
            SPItemRef::Operation(x) => &x.node,
            SPItemRef::Ability(x) => &x.node,
            SPItemRef::Transition(x) => &x.node,
            SPItemRef::IfThen(x) => &x.node,
        }
    }
    pub fn name(&self) -> &str {
        &self.node().name
    }
}



#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Model {
    node: SPNode,
    items: Vec<SPItem>
}

impl Noder for Model {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        find_in_list(self.items.as_slice(), next, path)
    }
    fn update_path_children(&mut self, paths: &SPPaths) {
        update_path_in_list(self.items.as_mut_slice(), paths);
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Model(self)
    }
}

impl Model {
    pub fn new(name: &str, items: Vec<SPItem>) -> Model {
        let node = SPNode::new(name);
        Model {
            node,
            items
        }
    }
    pub fn new_root(name: &str, items: Vec<SPItem>) -> Model {
        let mut m = Model::new(name, items);
        m.update_path(&SPPaths::new(None, Some(GlobalPath::new())));
        m
    }

    pub fn items(&self) -> &[SPItem] {
        self.items.as_slice()
    }

    pub fn add_item(&mut self, mut item: SPItem) -> SPPaths {
        let paths = item.update_path(self.node.paths());
        self.items.push(item);
        paths
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Resource {
    node: SPNode,
    abilities: Vec<Ability>,
    parameters: Vec<Variable>,
    messages: Vec<Topic>, // Also include estimated here on an estimated topic
                              //pub comm: ResourceComm,
}

impl Noder for Resource {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        let res = find_in_list(self.abilities.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.parameters.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.messages.as_slice(), next, path);
        
        return res
    }
    fn update_path_children(&mut self, _paths: &SPPaths) { 
        let mut local = LocalPath::from(vec!(self.node.name.clone()));
        self.node.paths.upd_local(Some(local));
        let paths = self.node.paths();
        update_path_in_list(self.abilities.as_mut_slice(), &paths);
        update_path_in_list(self.parameters.as_mut_slice(), &paths);
        update_path_in_list(self.messages.as_mut_slice(), &paths);
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Resource(self)
    }
}

impl Resource {
    pub fn new(name: &str) -> Resource {
        let mut node = SPNode::new(name);
        let mut local = LocalPath::from(vec!(name.to_string()));
        node.update_path(&SPPaths::new(Some(local), None));
        Resource {
            node,
            ..Resource::default()
        }
    }

    pub fn abilities(&self) -> &[Ability] {self.abilities.as_slice()}
    pub fn add_ability(&mut self, mut ability: Ability) -> SPPaths {
        let paths = ability.update_path(self.node.paths());
        self.abilities.push(ability);
        paths
    }

    pub fn parameters(&self) -> &[Variable] {self.parameters.as_slice()}
    pub fn add_parameter(&mut self, mut parameter: Variable) -> SPPaths {
        let paths = parameter.update_path(self.node.paths());
        self.parameters.push(parameter);
        paths
    }

    pub fn messages(&self) -> &[Topic] {self.messages.as_slice()}
    pub fn add_message(&mut self, mut message: Topic) -> SPPaths {
        let paths = message.update_path(self.node.paths());
        self.messages.push(message);
        paths
    }
}


#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Topic {
    node: SPNode,
    msg: MessageField,
}

impl Noder for Topic {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        if self.msg.name() != next {return None}
        self.msg.find(path)
    }
    fn update_path_children(&mut self, paths: &SPPaths) { 
        self.msg.update_path(paths);
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Topic(self)
    }
}

impl Topic {
    pub fn new(name: &str, msg: MessageField) -> Topic {
        let node = SPNode::new(name);
        Topic {
            node,
            msg
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Message {
    node: SPNode,
    fields: Vec<MessageField>,  // note, the field name is in each node
}

impl Noder for Message {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        find_in_list(self.fields.as_slice(), next, path)
    }
    fn update_path_children(&mut self, paths: &SPPaths) { 
        update_path_in_list(self.fields.as_mut_slice(), paths);
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Message(self)
    }
}

impl Message {
    pub fn new(name: &str, fields: Vec<MessageField>) -> Message {
        let node = SPNode::new(name);
        Message {
            node,
            fields
        }
    }
    pub fn fields(&self) -> &[MessageField] {self.fields.as_slice()}
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum MessageField {
    Msg(Message),
    Var(Variable),
}

impl Noder for MessageField {
    fn node(&self) -> &SPNode {
        match self {
            MessageField::Msg(ref x) => x.node(),
            MessageField::Var(ref x) => x.node()
        }
    }
    fn node_mut(&mut self) -> &mut SPNode {
        match self {
            MessageField::Msg(ref mut x) => x.node_mut(),
            MessageField::Var(ref mut x) => x.node_mut()
        }
    }
    
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        match self {
            MessageField::Msg(ref x) => x.find_child(next, path),
            MessageField::Var(ref x) => x.find_child(next, path),
        }
    }
    fn update_path_children(&mut self, paths: &SPPaths) { 
        match self {
            MessageField::Msg(ref mut x) => x.update_path_children(paths),
            MessageField::Var(ref mut x) => x.update_path_children(paths),
        }
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        match self {
            MessageField::Msg(ref x) => x.as_ref(),
            MessageField::Var(ref x) => x.as_ref(),
        }
    }
}

impl Default for MessageField {
    fn default() -> Self {
        MessageField::Var(Variable::default())
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Variable {
    node: SPNode,
    type_: VariableType,
    value_type: SPValueType,
    initial_value: SPValue,
    domain: Vec<SPValue>,
}

impl Noder for Variable {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, _: &str, _: &SPPath) -> Option<SPItemRef<'a>> {
        None
    }
    fn update_path_children(&mut self, _paths: &SPPaths) {}
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Variable(self)
    }
}

impl Variable {
    pub fn new(name: &str, 
                type_: VariableType,
                value_type: SPValueType,
                initial_value: SPValue,
                domain: Vec<SPValue>,
    ) -> Variable {
        let node = SPNode::new(name);
        Variable {
            node,
            type_,
            value_type,
            initial_value,
            domain,
        }
    }
    pub fn new_boolean(name: &str, 
                type_: VariableType,
    ) -> Variable {
        Variable::new(
            name,
            type_,
            SPValueType::Bool,
            false.to_spvalue(),
            vec!(false.to_spvalue(), true.to_spvalue()),
        )
    }
}

/// The possible variable types used by operations to define parameters
/// Must be the same as Variable
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum VariableType {
    Measured,
    Estimated,
    Command,
    Parameter(Option<SPPath>),
    Predicate(Predicate)
}

impl Default for VariableType {
    fn default() -> Self {
        VariableType::Estimated
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Transition {
    node: SPNode,
    guard: Predicate,
    actions: Vec<Action>,
    effects: Vec<Action>,
}

impl Noder for Transition {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, _: &str, _: &SPPath) -> Option<SPItemRef<'a>> {
        None
    }
    fn update_path_children(&mut self, _paths: &SPPaths) { }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Transition(self)
    }
}

impl Transition {
    pub fn new(name: &str, 
               guard: Predicate,
               actions: Vec<Action>,
               effects: Vec<Action> ) -> Transition {
        let node = SPNode::new(name);
        Transition {
            node,
            guard,
            actions,
            effects
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Ability {
    node: SPNode,
    controlled: Vec<Transition>,
    uncontrolled: Vec<Transition>,
    predicates: Vec<Variable>
}

impl Noder for Ability {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        let res = find_in_list(self.controlled.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.uncontrolled.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.predicates.as_slice(), next, path);
        return res;
    }
    fn update_path_children(&mut self, paths: &SPPaths) { 
        update_path_in_list(self.controlled.as_mut_slice(), paths);
        update_path_in_list(self.uncontrolled.as_mut_slice(), paths);
        update_path_in_list(self.predicates.as_mut_slice(), paths);
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Ability(self)
    }
}

impl Ability {
    pub fn new(name: &str,
                controlled: Vec<Transition>,
                uncontrolled: Vec<Transition>,
                predicates: Vec<Variable>
    ) -> Ability {
        let node = SPNode::new(name);
        Ability {
            node,
            controlled,
            uncontrolled,
            predicates
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Operation {
    node: SPNode,
    precondition: Vec<Transition>,
    postcondition: Vec<Transition>,
    uncontrolled: Vec<Transition>,
    predicates: Vec<Variable>,
    goal: Option<IfThen>,
    invariant: Option<IfThen>,
}

impl Noder for Operation {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>> {
        let res = find_in_list(self.precondition.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.postcondition.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.uncontrolled.as_slice(), next, path);
        if res.is_some() {return res};
        let res = find_in_list(self.predicates.as_slice(), next, path);
        if res.is_some() {return res};
        let res = self.goal.as_ref().and_then(|x| x.find(path));
        if res.is_some() {return res};
        let res = self.invariant.as_ref().and_then(|ref x| x.find(path));
        return res;
    }
    fn update_path_children(&mut self, paths: &SPPaths) { 
        update_path_in_list(self.precondition.as_mut_slice(), paths);
        update_path_in_list(self.postcondition.as_mut_slice(), paths);
        update_path_in_list(self.uncontrolled.as_mut_slice(), paths);
        update_path_in_list(self.predicates.as_mut_slice(), paths);
        self.goal.as_mut().map(|mut x| x.update_path(paths));
        self.invariant.as_mut().map(|mut x| x.update_path(paths));
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Operation(self)
    }
}

impl Operation {
    pub fn new( name: &str,
                precondition: Vec<Transition>,
                postcondition: Vec<Transition>,
                uncontrolled: Vec<Transition>,
                predicates: Vec<Variable>,
                goal: Option<IfThen>,
                invariant: Option<IfThen>
    ) -> Operation {
        let node = SPNode::new(name);
        Operation {
            node,
            precondition,
            postcondition,
            uncontrolled,
            predicates,
            goal,
            invariant,
        }
    }
}

/// An IfThen is used by operaitons to define goals or invariants. When the if_
/// predicate is true, then the then_ predicate is either a goal or an invariant
/// that the planner will use for planning. TODO: Maybe we should have a better name?
#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct IfThen {
    node: SPNode,
    if_: Predicate,
    then_: Predicate,
}

impl Noder for IfThen {
    fn node(&self) -> &SPNode {
        &self.node
    }
    fn node_mut(&mut self) -> &mut SPNode { &mut self.node}
    fn find_child<'a>(&'a self, _: &str, _: &SPPath) -> Option<SPItemRef<'a>> {
        None
    }
    fn update_path_children(&mut self, _paths: &SPPaths) { }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::IfThen(self)
    }
}

impl IfThen {
    pub fn new( name: &str,
                if_: Predicate,
                then_: Predicate
    ) -> IfThen {
        let node = SPNode::new(name);
        IfThen {
            node,
            if_,
            then_
        }
    }
}






/// The SPPath is used for identifying all objects in a model. The path will be defined
/// based on where the item is in the model hierarchy
#[derive(Eq, Hash, PartialEq, Serialize, Deserialize, Clone)]
pub enum SPPath {
    LocalPath(LocalPath),
    GlobalPath(GlobalPath),
}

impl Default for SPPath {
    fn default() -> Self {
        SPPath::LocalPath(LocalPath{path: vec!()})
    }
}

impl std::fmt::Display for SPPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SPPath::LocalPath(x) => write!(f, "{}", x),
            SPPath::GlobalPath(x) => write!(f, "{}", x),
        }
    }
}
impl std::fmt::Debug for SPPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SPPath::LocalPath(x) => write!(f, "{}", x),
            SPPath::GlobalPath(x) => write!(f, "{}", x),
        }
    }
}

impl SPPath {
    pub fn new_local() -> SPPath {
        SPPath::LocalPath(LocalPath::new())
    }
    pub fn new_global() -> SPPath {
        SPPath::GlobalPath(GlobalPath::new())
    }
    pub fn add(&mut self, name: String) {
        match self {
            SPPath::LocalPath(ref mut xs) => xs.add(name),
            SPPath::GlobalPath(ref mut xs) => xs.add(name)
        }
    }
    pub fn from(xs: &[String]) -> SPPath {
        let v: Vec<String> = xs.iter().map(|s| s.to_string()).collect();
        SPPath::LocalPath(LocalPath::from(v))
    }
    pub fn from_array(xs: &[&str]) -> SPPath {
        let v: Vec<String> = xs.iter().map(|s| s.to_string()).collect();
        SPPath::LocalPath(LocalPath::from(v))
    }
    pub fn from_string(s: &str) -> Result<SPPath> {
        let what_type: Vec<&str> = s.split(":").collect();

        match what_type.as_slice() {
            ["L", tail] => {
                let res: Vec<&str> = tail.split("/").collect();
                return Ok(SPPath::from_array(&res));
            }
            ["G", tail] if tail != &"" => {
                let res: Vec<&str> = tail.split("/").filter(|x| !x.is_empty()).collect();
                return Ok(SPPath::from_array_to_global(&res));
            }
            _ => return Err(SPError::No(format!("Can not convert {} into a path", s))),
        }
    }
    pub fn from_to_global(n: &[String]) -> SPPath {
        let v: Vec<String> = n.iter().map(|s| s.to_string()).collect();
        SPPath::GlobalPath(GlobalPath::from(v))
    }
    pub fn from_array_to_global(n: &[&str]) -> SPPath {
        let v: Vec<String> = n.iter().map(|s| s.to_string()).collect();
        SPPath::GlobalPath(GlobalPath::from(v))
    }
    pub fn path(&self) -> Vec<String> {
        match self {
            SPPath::LocalPath(x) => x.path(),
            SPPath::GlobalPath(x) => x.path(),
        }
    }
    pub fn string_path(&self) -> String {
        format!("{}", self)
    }
    pub fn is_child_of(&self, other: &SPPath) -> bool {
        (self.path().len() > other.path().len())
            && other
                .path()
                .iter()
                .zip(self.path().iter())
                .all(|(a, b)| a == b)
    }

    /// returns the next name in the path of this SPPath based on a path
    /// that is the current parent to this path
    pub fn next_node_in_path(&self, parent_path: &SPPath) -> Option<String> {
        if self.is_child_of(parent_path) && self.as_slice().len() > parent_path.as_slice().len() {
            Some(self.as_slice()[parent_path.as_slice().len()].clone())
        } else {
            None
        }
    }

    pub fn as_sp(&self) -> SPPath {
        self.clone()
    }

    /// For internal use instead of cloning the path vec
    pub fn as_slice(&self) -> &[String] {
        match self {
            SPPath::LocalPath(x) => x.as_slice(),
            SPPath::GlobalPath(x) => x.as_slice(),
        }
    }
}

// Maybe refactor into a trait with methods that are the same
// pub trait Pather {
//     fn path(&self) -> &[String];
//     fn path_mut(&mut self) -> &mut [String];
// }

#[derive(Default, Eq, Hash, PartialEq, Serialize, Deserialize, Clone)]
pub struct LocalPath {
    path: Vec<String>,
}
impl LocalPath {
    pub fn new() -> LocalPath {
        LocalPath{path: vec!()}
    }
    pub fn path(&self) -> Vec<String> {
        self.path.clone()
    }
    pub fn from(path: Vec<String>) -> LocalPath {
        LocalPath{path}
    }
    pub fn add(&mut self, name: String) {
        self.path.push(name);
    }
    pub fn as_slice(&self) -> &[String] {
        self.path.as_slice()
    }
    pub fn as_sp(&self) -> SPPath {
        SPPath::LocalPath(self.clone())
    }
    pub fn is_child_of(&self, other: &LocalPath) -> bool {
        (self.as_slice().len() > other.as_slice().len())
            && other
                .as_slice()
                .iter()
                .zip(self.as_slice().iter())
                .all(|(a, b)| a == b)
    }
}
impl std::fmt::Display for LocalPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "L:{}", self.path.join("/"))
    }
}
impl std::fmt::Debug for LocalPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "L:{}", self.path.join("/"))
    }
}

#[derive(Default, Eq, Hash, PartialEq, Serialize, Deserialize, Clone)]
pub struct GlobalPath{
    path: Vec<String>,
}
impl GlobalPath {
    pub fn new() -> GlobalPath {
        GlobalPath{path: vec!()}
    }
    pub fn path(&self) -> Vec<String> {
        self.path.clone()
    }
    pub fn from(path: Vec<String>) -> GlobalPath {
        GlobalPath{path}
    }
    pub fn add(&mut self, name: String) {
        self.path.push(name);
    }
    pub fn as_slice(&self) -> &[String] {
        self.path.as_slice()
    }
    pub fn as_sp(&self) -> SPPath {
        SPPath::GlobalPath(self.clone())
    }
    pub fn is_child_of(&self, other: &GlobalPath) -> bool {
        (self.as_slice().len() > other.as_slice().len())
            && other
                .as_slice()
                .iter()
                .zip(self.as_slice().iter())
                .all(|(a, b)| a == b)
    }
}
impl std::fmt::Display for GlobalPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "G:{}", self.path.join("/"))
    }
}
impl std::fmt::Debug for GlobalPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}


#[derive(Debug, Eq, Hash, PartialEq, Serialize, Deserialize, Clone, Default)]
pub struct SPPaths {
    local: Option<LocalPath>,
    global: Option<GlobalPath>,
}

impl std::fmt::Display for SPPaths {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let l: String = self.local.as_ref().map(|x| format!("{}", x)).unwrap_or("".to_string());
        let g: String = self.global.as_ref().map(|x| format!("{}", x)).unwrap_or("".to_string());
        write!(f, "<{},{}>", l, g)
    }
}

impl SPPaths {
    pub fn new(local: Option<LocalPath>, global: Option<GlobalPath>) -> SPPaths {
        SPPaths {
            local,
            global
        }
    }
    pub fn empty() -> SPPaths {
        SPPaths::default()
    }
    pub fn local_path(&self) -> &Option<LocalPath> {
        &self.local
    }
    pub fn global_path(&self) -> &Option<GlobalPath> {
        &self.global
    }

    pub fn is_eq(&self, path: &SPPath) -> bool {
        match path {
            SPPath::LocalPath(p) => self.local.as_ref().map(|l| l == p).unwrap_or(false),
            SPPath::GlobalPath(p) => self.global.as_ref().map(|g| g == p).unwrap_or(false),
        }
    }
    pub fn is_parent_of(&self, path: &SPPath) -> bool {
        match path {
            SPPath::LocalPath(p) => self.local.as_ref().map(|x| p.is_child_of(&x)).unwrap_or(false),
            SPPath::GlobalPath(p) => self.global.as_ref().map(|x| p.is_child_of(&x)).unwrap_or(false),
        }
    }

    /// returns the next name in the path based on a local or gloabl path
    /// in this SPPaths
    pub fn next_node_in_path(&self, path: &SPPath) -> Option<String> {
        let l_len: usize = self.local.as_ref().map(|x| x.as_slice().len()).unwrap_or(0);
        let g_len: usize = self.global.as_ref().map(|x| x.as_slice().len()).unwrap_or(0);
        if self.is_parent_of(path){
            match path {
                SPPath::LocalPath(x) =>  {
                    Some(x.as_slice()[l_len].clone())
                },
                SPPath::GlobalPath(x) => {
                    Some(x.as_slice()[g_len].clone())
                }
            }      
        } else {
            None
        }
    }


    pub fn upd(&mut self, paths: &SPPaths) {
        self.local = paths.local.clone();
        self.global = paths.global.clone();
    }
    pub fn add(&mut self, name: String) {
        self.local.as_mut().map(|x| x.path.push(name.clone()));
        self.global.as_mut().map(|x| x.path.push(name));
    }
    pub fn upd_local(&mut self, path: Option<LocalPath>) {
        self.local = path;
    }
    pub fn upd_global(&mut self, path: Option<GlobalPath>) {
        self.global = path;
    }
}




type Result<T> = std::result::Result<T, SPError>;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum SPError {
    OverwriteDelay(states::Delay, AssignStateValue),
    OverwriteNext(states::Next, AssignStateValue),
    No(String),
    Undefined,
}

impl fmt::Display for SPError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SPError::OverwriteDelay(next, prev) => write!(
                f,
                "You are trying to overwrite a Delay in the State. current: {:?}, new: {:?} ",
                prev, next
            ),
            SPError::OverwriteNext(next, prev) => write!(
                f,
                "You are trying to overwrite a Next in the State. current: {:?}, new: {:?} ",
                prev, next
            ),
            SPError::Undefined => write!(f, "An undefined SP error!"),
            SPError::No(s) => write!(f, "Oh No: {}", s),
        }
    }
}

impl error::Error for SPError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}




#[cfg(test)]
mod tests_domain {
    use super::*;
    #[test]
    fn making() {
        let mut m = Model::new_root("model", vec!());
        let mut r1 = Resource::new("r1");
        
        let a = Topic::new("act", MessageField::Var(Variable::new(
            "data", 
            VariableType::Measured, 
            SPValueType::Int32,
            0.to_spvalue(), 
            vec!(0.to_spvalue(), 10.to_spvalue())
        )));
        let r = Topic::new("ref", MessageField::Var(Variable::new(
            "data", 
            VariableType::Command, 
            SPValueType::Int32,
            0.to_spvalue(), 
            vec!(0.to_spvalue(), 10.to_spvalue())
        )));
        let active = Topic::new("active", MessageField::Var(Variable::new_boolean(
            "data", 
            VariableType::Measured
        )));
        let activate = Topic::new("activate", MessageField::Var(Variable::new_boolean(
            "data", 
            VariableType::Command, 
        )));

        let mut a = r1.add_message(a).local_path().clone().unwrap();
        a.add("data".to_string());
        let mut r = r1.add_message(r).local_path().clone().unwrap();
        r.add("data".to_string());
        let mut active = r1.add_message(active).local_path().clone().unwrap();
        active.add("data".to_string());
        let mut activate = r1.add_message(activate).local_path().clone().unwrap();
        activate.add("data".to_string());

        

        let name = "r1";
        let upper = 10;
        let to_upper = Transition::new(
            &format!("{}_to_upper", name),
            p!(a == 0), // p!(r != upper), // added req on a == 0 just for testing
            vec!(a!(r = upper)),
            vec!(a!(a = upper)),
        );
        let to_lower = Transition::new(
            &format!("{}_to_lower", name),
            p!(a == upper), // p!(r != 0), // added req on a == upper just for testing
            vec!(a!(r = 0)),
            vec!(a!(a = 0)),
        );
        let t_activate = Transition::new(
            &format!("{}_activate", name),
            p!(!active),
            vec!(a!(activate)),
            vec!(a!(active)),
        );
        let t_deactivate = Transition::new(
            &format!("{}_deactivate", name),
            p!(active),
            vec!(a!(!activate)),
            vec!(a!(!active)),
        );

        let ability = Ability::new(
            "all", 
            vec!(t_activate, t_deactivate), 
            vec!(to_upper, to_lower), 
            vec!()
        );
        
        let ability = r1.add_ability(ability);

        let r1 = m.add_item(SPItem::Resource(r1)).global_path().clone().unwrap();
    

        let resource = if let Some(SPItemRef::Resource(r)) = m.find(&r1.as_sp()) {Some(r)} else {None};
        println!("");
        println!("resource: {:?}", resource);
        println!("");

        if let Some(SPItemRef::Resource(r)) = m.find(&r1.as_sp()) {
            let a_again = r.find(&a.as_sp());
            println!("the resource {:?}", r);
            println!("the a {:?}", a_again);
        }

        



        

        
        // let n = SPItem::Model(Model{
        //     node: SPNode::new("n"),
        //     items: vec!()
        // });

        // if let SPItem::Model(ref mut my) = m {
        //     my.add_item(n);
        // }
        
        println!("{:?}", m);
    }

    #[test]
    fn find() {
        let mut m = Model::new("m", vec!(
            SPItem::Model(Model::new("a", vec!(
                SPItem::Model(Model::new("b", vec!())),
                SPItem::Model(Model::new("c", vec!(
                    SPItem::Model(Model::new("d", vec!()))
                )))
            ))),
            SPItem::Model(Model::new("k", vec!(
                SPItem::Model(Model::new("l", vec!()))
            )))
        ));

        m.update_path(&SPPaths::new(None, Some(GlobalPath::new())));

        let g_ab = SPPath::from_array_to_global(&["m", "a", "b"]);
        let g_acd = SPPath::from_array_to_global(&["m", "a", "c", "d"]);
        let g_k = SPPath::from_array_to_global(&["m", "k"]);
        let ab = m.find(&g_ab);
        let acd = m.find(&g_acd);
        let k = m.find(&g_k);

        println!("{:?}", &ab);
        println!("{:?}", &acd);
        println!("{:?}", &k);

        assert!(ab.unwrap().name() == "b");
        assert!(acd.unwrap().name() == "d");
        assert!(k.unwrap().name() == "k");
    }
}




#[cfg(test)]
mod tests_paths {
    use super::*;
    #[test]
    fn making() {
        let g_ab = SPPath::from_array_to_global(&["a", "b"]);
        let l_ab = SPPath::from_array(&["a", "b"]);

        assert_eq!(g_ab.string_path(), "G:a/b".to_string());
        assert_eq!(l_ab.string_path(), "L:a/b".to_string());
        assert_eq!(SPPath::from_string("G:a/b"), Ok(g_ab.clone()));
        assert_eq!(SPPath::from_string("L:a/b"), Ok(l_ab.clone()));

        assert!(SPPath::from_string("G:").is_err());
        assert!(SPPath::from_string("G/no").is_err());
        assert!(SPPath::from_string("H:a/b/").is_err());
        assert!(SPPath::from_string("a/b/").is_err());
        assert!(SPPath::from_string("G:top_path").is_ok());
        assert_eq!(
            SPPath::from_string("G:a/b//k/"),
            Ok(SPPath::from_array_to_global(&["a", "b", "k"]))
        );
    }

    #[test]
    fn get_next_name() {
        let g_ab = SPPath::from_array_to_global(&["a", "b", "c"]);
        let l_ab = SPPath::from_array(&["a", "b", "c"]);

        let l_a = SPPath::from_array(&["a"]);
        let l_b = SPPath::from_array(&["a", "b"]);
        let g_a = SPPath::from_array_to_global(&["a"]);
        let g_b = SPPath::from_array_to_global(&["a", "b"]);
        let l_k = SPPath::from_array(&["k"]);

        assert_eq!(g_ab.next_node_in_path(&l_a), Some("b".to_string()));
        assert_eq!(g_ab.next_node_in_path(&l_b), Some("c".to_string()));
        assert_eq!(g_ab.next_node_in_path(&g_a), Some("b".to_string()));
        assert_eq!(g_ab.next_node_in_path(&g_b), Some("c".to_string()));
        assert_eq!(l_ab.next_node_in_path(&l_a), Some("b".to_string()));
        assert_eq!(l_ab.next_node_in_path(&l_b), Some("c".to_string()));
        assert_eq!(l_ab.next_node_in_path(&g_a), Some("b".to_string()));
        assert_eq!(l_ab.next_node_in_path(&g_b), Some("c".to_string()));
        assert_eq!(l_ab.next_node_in_path(&l_k), None);

    }
}
