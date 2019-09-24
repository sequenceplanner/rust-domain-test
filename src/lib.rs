//! The SP domain

#![allow(unused_mut)] // for some reason I get not a correct warning for mut in macros

mod values;
pub use values::*;

mod predicates;
pub use predicates::*;

mod states;
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
    local_path: SPPath,
    global_path: SPPath,
}

impl SPNode {
    pub fn new(name: &str) -> SPNode {
        SPNode {
            name: name.to_string(),
            local_path: SPPath::NoPath,
            global_path: SPPath::NoPath,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn local_path(&self) -> SPPath {
        self.local_path.clone()
    }
    pub fn global_path(&self) -> SPPath {
        self.global_path.clone()
    }

    pub fn update_path(&mut self, mut local: SPPath, mut global: SPPath) -> SPPaths {
        local.add(self.name.clone());
        global.add(self.name.clone());
        if let SPPath::GlobalPath(_) = local {
            panic!("Must assign local, not {}", local);
        }
        if let SPPath::LocalPath(_) = global {
            panic!("Must assign global, not {}", global);
        }
        self.local_path = local;
        self.global_path = global;
        SPPaths::new(self.local_path(), self.global_path())
    }

    pub fn find(&self, path: &SPPath) -> bool {
        path != &SPPath::NoPath && (&self.local_path == path || &self.global_path == path)
    }

    pub fn next_node_in_path(&self, path: &SPPath) -> Option<String> {
        match path {
            SPPath::GlobalPath(_) => path.next_node_in_path(&self.global_path),
            SPPath::LocalPath(_) => path.next_node_in_path(&self.local_path),
            SPPath::NoPath => None,
        }
    }

    fn write_nice(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut res = String::new();
        res.push_str(&format!("{}- ", self.name));
        if let SPPath::LocalPath(_) = self.local_path {
            res.push_str(&format!("{}, ", self.local_path));
        }
        if let SPPath::GlobalPath(_) = self.global_path {
            res.push_str(&format!("{}, ", self.global_path));
        }
        write!(f, "{}", res)
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath);
    fn as_ref<'a>(&'a self) -> SPItemRef<'a>;

    fn name(&self) -> &str {&self.node().name}

    /// Finds the item with a specific SPPath. If it
    /// is a global path, only one can exists, if it is 
    /// a local path, the first one is returned
    fn find<'a>(&'a self, path: &SPPath) -> Option<SPItemRef<'a>>  {
        if self.node().find(path) {
            return Some(self.as_ref());
        }
        let next = self.node().next_node_in_path(path);
        if next.is_none() {
            return None
        }
        self.find_child(&next.unwrap(), path)
    }

    /// updates the path of this item and its children
    fn update_path(&mut self, mut local: SPPath, mut global: SPPath) -> SPPaths {
        let paths = self.node_mut().update_path(local, global);
        self.update_path_children(paths.local_path(), paths.global_path());
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
    local: &SPPath, 
    global: &SPPath) where T: Noder {
    for i in xs.iter_mut() {
        i.update_path(local.clone(), global.clone());
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
    fn update_path_children(&mut self, mut local: SPPath, mut global: SPPath) {
        match self {
            SPItem::Model(x) => x.update_path_children(local, global),
            SPItem::Resource(x) => x.update_path_children(local, global),
            SPItem::Message(x) => x.update_path_children(local, global),
            SPItem::Topic(x) => x.update_path_children(local, global),
            SPItem::Variable(x) => x.update_path_children(local, global),
            SPItem::Operation(x) => x.update_path_children(local, global),
            SPItem::Ability(x) => x.update_path_children(local, global),
            SPItem::Transition(x) => x.update_path_children(local, global),
            SPItem::IfThen(x) => x.update_path_children(local, global),
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath) {
        update_path_in_list(self.items.as_mut_slice(), &local, &global);
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

    pub fn items(&self) -> &[SPItem] {
        self.items.as_slice()
    }

    pub fn add_item(&mut self, mut item: SPItem) -> SPPaths {
        let paths = item.update_path(self.node.local_path(), self.node.global_path());
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
    fn update_path_children(&mut self, _local: SPPath, global: SPPath) {
        let mut local = SPPath::new_local();
        local.add(self.name().to_string());
        update_path_in_list(self.abilities.as_mut_slice(), &local, &global);
        update_path_in_list(self.parameters.as_mut_slice(), &local, &global);
        update_path_in_list(self.messages.as_mut_slice(), &local, &global);
        self.node.local_path = local;
    }
    fn as_ref<'a>(&'a self) -> SPItemRef<'a> {
        SPItemRef::Resource(self)
    }
}

impl Resource {
    pub fn new(name: &str) -> Resource {
        let mut node = SPNode::new(name);
        node.update_path(SPPath::new_local(), SPPath::NoPath);
        Resource {
            node,
            ..Resource::default()
        }
    }

    pub fn abilities(&self) -> &[Ability] {self.abilities.as_slice()}
    pub fn add_ability(&mut self, mut ability: Ability) -> SPPaths {
        let paths = ability.update_path(self.node.local_path(), self.node.global_path());
        self.abilities.push(ability);
        paths
    }

    pub fn parameters(&self) -> &[Variable] {self.parameters.as_slice()}
    pub fn add_parameter(&mut self, mut parameter: Variable) -> SPPaths {
        let paths = parameter.update_path(self.node.local_path(), self.node.global_path());
        self.parameters.push(parameter);
        paths
    }

    pub fn messages(&self) -> &[Topic] {self.messages.as_slice()}
    pub fn add_message(&mut self, mut message: Topic) -> SPPaths {
        let paths = message.update_path(self.node.local_path(), self.node.global_path());
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath) {
        self.msg.update_path(local, global);
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath) {
        update_path_in_list(self.fields.as_mut_slice(), &local, &global);
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath) {
        match self {
            MessageField::Msg(ref mut x) => x.update_path_children(local, global),
            MessageField::Var(ref mut x) => x.update_path_children(local, global),
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
    fn update_path_children(&mut self, _: SPPath, _: SPPath) {}
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
    fn update_path_children(&mut self, _: SPPath, _: SPPath) {}
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath) {
        update_path_in_list(self.controlled.as_mut_slice(), &local, &global);
        update_path_in_list(self.uncontrolled.as_mut_slice(), &local, &global);
        update_path_in_list(self.predicates.as_mut_slice(), &local, &global);
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
    fn update_path_children(&mut self, local: SPPath, global: SPPath) {
        update_path_in_list(self.precondition.as_mut_slice(), &local, &global);
        update_path_in_list(self.postcondition.as_mut_slice(), &local, &global);
        update_path_in_list(self.uncontrolled.as_mut_slice(), &local, &global);
        update_path_in_list(self.predicates.as_mut_slice(), &local, &global);
        self.goal.as_mut().map(|mut x| x.update_path(local.clone(), global.clone()));
        self.invariant.as_mut().map(|mut x| x.update_path(local.clone(), global.clone()));
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
    fn update_path_children(&mut self, _: SPPath, _: SPPath) {}
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
#[derive(Debug, Eq, Hash, PartialEq, Serialize, Deserialize, Clone)]
pub enum SPPath {
    LocalPath(Vec<String>),
    GlobalPath(Vec<String>),
    NoPath,
}

impl Default for SPPath {
    fn default() -> Self {
        SPPath::NoPath
    }
}

impl std::fmt::Display for SPPath {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SPPath::LocalPath(xs) => write!(f, "L:{}", xs.join("/")),
            SPPath::GlobalPath(xs) => write!(f, "G:{}", xs.join("/")),
            SPPath::NoPath => write!(f, "NOPATH"),
        }
    }
}

impl SPPath {
    pub fn new() -> SPPath {
        SPPath::NoPath
    }
    pub fn new_local() -> SPPath {
        SPPath::LocalPath(vec!())
    }
    pub fn new_global() -> SPPath {
        SPPath::GlobalPath(vec!())
    }
    pub fn add(&mut self, name: String) {
        match self {
            SPPath::LocalPath(ref mut xs) => xs.push(name),
            SPPath::GlobalPath(ref mut xs) => xs.push(name),
            SPPath::NoPath => {},//println!("Tried to push {} to an NoPath. Probably ok", name)
        }
    }
    pub fn from(xs: &[String]) -> SPPath {
        let v: Vec<String> = xs.iter().map(|s| s.to_string()).collect();
        SPPath::LocalPath(v)
    }
    pub fn from_array(xs: &[&str]) -> SPPath {
        let v: Vec<String> = xs.iter().map(|s| s.to_string()).collect();
        SPPath::LocalPath(v)
    }
    pub fn from_string(s: &str) -> Result<SPPath> {
        if s == "NOPATH" {
            return Ok(SPPath::NoPath);
        }
        let what_type: Vec<&str> = s.split(":").collect();

        match what_type.as_slice() {
            ["NOPATH"] => return Ok(SPPath::NoPath),
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
        SPPath::GlobalPath(v)
    }
    pub fn from_array_to_global(n: &[&str]) -> SPPath {
        let v: Vec<String> = n.iter().map(|s| s.to_string()).collect();
        SPPath::GlobalPath(v)
    }
    pub fn path(&self) -> Vec<String> {
        match self {
            SPPath::LocalPath(xs) => xs.clone(),
            SPPath::GlobalPath(xs) => xs.clone(),
            SPPath::NoPath => vec![],
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
        if self.is_child_of(parent_path) && self.as_slize().len() > parent_path.as_slize().len() {
            Some(self.as_slize()[parent_path.as_slize().len()].clone())
        } else {
            None
        }
    }

    /// For internal use instead of cloning the path vec
    fn as_slize(&self) -> &[String] {
        match self {
            SPPath::LocalPath(xs) => xs.as_slice(),
            SPPath::GlobalPath(xs) => xs.as_slice(),
            SPPath::NoPath => &[],
        }
    }
}


#[derive(Debug, Eq, Hash, PartialEq, Serialize, Deserialize, Clone, Default)]
pub struct SPPaths {
    local: SPPath,
    global: SPPath,
}

impl std::fmt::Display for SPPaths {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{},{}>", self.local, self.global)
    }
}

impl SPPaths {
    pub fn new(local: SPPath, global: SPPath) -> SPPaths {
        let local = match local {
            SPPath::GlobalPath(_) => {
                panic!("DO NOT ADD GLOBAL TO LOCAL, CHECK YOUR CODE: {}", local)
            },
            _ => local,
        };
        let global = match global {
            SPPath::LocalPath(_) => {
                panic!("DO NOT ADD LOCAL TO GLOBAL, CHECK YOUR CODE: {}", global)
            },
            _ => global,
        };
        SPPaths {
            local,
            global
        }
    }
    pub fn local_path(&self) -> SPPath {self.local.clone()}
    pub fn global_path(&self) -> SPPath {self.global.clone()}
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
        let mut m = Model::new("model", vec!());
        m.update_path(SPPath::NoPath, SPPath::new_global());
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

        let a = r1.add_message(a);
        let r = r1.add_message(r);
        let active = r1.add_message(active);
        let activate = r1.add_message(activate);

        let r1 = m.add_item(SPItem::Resource(r1));

        if let Some(SPItemRef::Resource(r)) = m.find(&r1.local_path()) {
            let a_again = r.find(&a.local_path());
            println!("the resource {:?}", r);
            println!("the a {:?}", a_again);
        }

        



        // let to_upper = Transition::new(
        //         format!("{}_to_upper", name),
        //         p!(a == 0), // p!(r != upper), // added req on a == 0 just for testing
        //         vec!(a!(r = upper)),
        //         vec!(a!(a = upper)),
        //     );
        //     let to_lower = Transition::new(
        //         format!("{}_to_lower", name),
        //         p!(a == upper), // p!(r != 0), // added req on a == upper just for testing
        //         vec!(a!(r = 0)),
        //         vec!(a!(a = 0)),
        //     );
        //     let t_activate = Transition::new(
        //         format!("{}_activate", name),
        //         p!(!activated),
        //         vec!(a!(activate)),
        //         vec!(a!(activated)),
        //     );
        //     let t_deactivate = Transition::new(
        //         format!("{}_activate", name),
        //         p!(activated),
        //         vec!(a!(!activate)),
        //         vec!(a!(!activated)),
        //     );

        
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

        m.update_path(SPPath::NoPath, SPPath::new_global());

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
        assert_eq!(SPPath::NoPath.string_path(), "NOPATH".to_string());
        assert_eq!(SPPath::from_string("G:a/b"), Ok(g_ab.clone()));
        assert_eq!(SPPath::from_string("L:a/b"), Ok(l_ab.clone()));
        assert_eq!(SPPath::from_string("NOPATH"), Ok(SPPath::NoPath));
        assert_eq!(SPPath::from_string("NOPATH"), Ok(SPPath::NoPath));

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
