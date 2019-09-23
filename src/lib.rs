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
    //SOP(SOP),
}

impl SPItem {
    pub fn name(&self) -> &str {
        &self.node().name
    }
    pub fn node(&self) -> &SPNode {
        match self {
            SPItem::Model(x) => &x.node,
            SPItem::Resource(x) => &x.node,
            SPItem::Message(x) => &x.node,
            SPItem::Topic(x) => &x.node,
            SPItem::Variable(x) => &x.node,
            SPItem::Operation(x) => &x.node,
            SPItem::Ability(x) => &x.node,
            SPItem::Transition(x) => &x.node,
        }
    }
    // fn node_mut(&mut self) -> &mut SPNode {
    //     match self {
    //         SPItem::Model(x) => &mut x.node,
    //         SPItem::Resource(x) => &mut x.node,
    //         SPItem::Message(x) => &mut x.node,
    //         SPItem::Topic(x) => &mut x.node,
    //         SPItem::Variable(x) => &mut x.node,
    //         SPItem::Operation(x) => &mut x.node,
    //         SPItem::Ability(x) => &mut x.node,
    //         SPItem::Transition(x) => &mut x.node,
    //     }
    // }
    pub fn update_path(&mut self, mut local: SPPath, mut global: SPPath) -> (SPPath, SPPath) {
        match self {
            SPItem::Model(x) => x.update_path(local, global),
            SPItem::Resource(x) => x.update_path(local, global),
            SPItem::Message(x) => x.update_path(local, global),
            SPItem::Topic(x) => x.update_path(local, global),
            SPItem::Variable(x) => x.update_path(local, global),
            SPItem::Operation(x) => x.update_path(local, global),
            SPItem::Ability(x) => x.update_path(local, global),
            SPItem::Transition(x) => x.update_path(local, global),
        }

    }
}

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

    pub fn local_path(&self) -> SPPath {
        self.local_path.clone()
    }
    pub fn global_path(&self) -> SPPath {
        self.global_path.clone()
    }

    pub fn update_path(&mut self, mut local: SPPath, mut global: SPPath) -> (SPPath, SPPath) {
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
        (self.local_path(), self.global_path())
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

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Model {
    node: SPNode,
    items: Vec<SPItem>
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

    pub fn add_item(&mut self, mut item: SPItem) {
        item.update_path(self.node.local_path(), self.node.global_path());
        self.items.push(item);
    }

    pub fn update_path(&mut self, mut local: SPPath, mut global: SPPath) -> (SPPath, SPPath) {
        let (local, global) = self.node.update_path(local, global);
        self.items.iter_mut().for_each(|i| {
            i.update_path(local.clone(), global.clone());
        });
        (local, global)
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

impl Resource {
    
    pub fn new(name: &str) -> Resource {
        let node = SPNode::new(name);
        Resource {
            node,
            ..Resource::default()
        }
    }
    
    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        let (local, global) = self.node.update_path(local, global);

        self.abilities.iter_mut().for_each(|i| {
            i.update_path(local.clone(), global.clone());
        });
        self.parameters.iter_mut().for_each(|i| {
            i.update_path(local.clone(), global.clone());
        });
        self.messages.iter_mut().for_each(|i| {
            i.update_path(local.clone(), global.clone());
        });
        (local, global)
    }
}


#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Topic {
    node: SPNode,
    msg: Message,
}

impl Topic {
    pub fn new(name: &str, msg: Message) -> Topic {
        let node = SPNode::new(name);
        Topic {
            node,
            msg
        }
    }

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        let (local, global) = self.node.update_path(local, global);
        self.msg.update_path(local.clone(), global.clone());
        (local, global)
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Message {
    node: SPNode,
    fields: Vec<(String, MessageFields)>,
}

impl Message {
    pub fn new(name: &str) -> Message {
        let node = SPNode::new(name);
        Message {
            node,
            fields: vec!()
        }
    }

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        let (local, global) = self.node.update_path(local, global);
        self.fields.iter_mut().for_each(|(_, value)| {
            value.update_path(local.clone(), global.clone());
        });
        (local, global)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum MessageFields {
    Msg(Message),
    Var(Variable),
}

impl MessageFields {
    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        match self {
            MessageFields::Msg(ref mut x) => x.update_path(local, global),
            MessageFields::Var(ref mut x) => x.update_path(local, global)
        }
    }
}

impl Default for MessageFields {
    fn default() -> Self {
        MessageFields::Var(Variable::default())
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Variable {
    node: SPNode,
    pub type_: VariableType,
    pub initial_value: Option<SPValue>,
    pub domain: Vec<SPValue>,
}

impl Variable {
    pub fn new(name: &str, 
                type_: VariableType,
                initial_value: Option<SPValue>,
                domain: Vec<SPValue>,
                ) -> Variable {
        let node = SPNode::new(name);
        Variable {
            node,
            type_,
            initial_value,
            domain,
        }
    }

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        self.node.update_path(local, global)
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

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        self.node.update_path(local, global)
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Ability {
    node: SPNode,
    controlled: Vec<Transition>,
    uncontrolled: Vec<Transition>,
    predicates: Vec<Variable>
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

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        let (local, global) = self.node.update_path(local, global);
        self.controlled.iter_mut().for_each(|t| {
            t.update_path(local.clone(), global.clone());
        });
        self.uncontrolled.iter_mut().for_each(|t| {
            t.update_path(local.clone(), global.clone());
        });
        self.predicates.iter_mut().for_each(|v| {
            v.update_path(local.clone(), global.clone());
        });


        (local, global)
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

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        let (local, global) = self.node.update_path(local, global);
        self.precondition.iter_mut().for_each(|t| {
            t.update_path(local.clone(), global.clone());
        });
        self.postcondition.iter_mut().for_each(|t| {
            t.update_path(local.clone(), global.clone());
        });
        self.uncontrolled.iter_mut().for_each(|t| {
            t.update_path(local.clone(), global.clone());
        });
        self.predicates.iter_mut().for_each(|v| {
            v.update_path(local.clone(), global.clone());
        });
        if let Some(ref mut x) = self.goal {
            x.update_path(local.clone(), global.clone());
        }
        if let Some(ref mut x) = self.invariant {
            x.update_path(local.clone(), global.clone());
        }


        (local, global)
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

    pub fn update_path(&mut self, local: SPPath, global: SPPath) -> (SPPath, SPPath) {
        self.node.update_path(local, global)
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
    pub fn add(&mut self, name: String) {
        match self {
            SPPath::LocalPath(ref mut xs) => xs.push(name),
            SPPath::GlobalPath(ref mut xs) => xs.push(name),
            SPPath::NoPath => println!("Tried to push {} to an NoPath", name)
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
        (self.path().len() >= other.path().len())
            && other
                .path()
                .iter()
                .zip(self.path().iter())
                .all(|(a, b)| a == b)
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
        let mut m = SPItem::Model(Model{
                node: SPNode::new("m"),
                items: vec!()
            });

        m.update_path(SPPath::new_local(), SPPath::from_array_to_global(&["a", "b"]));
        
        let n = SPItem::Model(Model{
            node: SPNode::new("n"),
            items: vec!()
        });

        if let SPItem::Model(ref mut my) = m {
            my.add_item(n);
        }
        
        println!("{:?}", m);
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
}
