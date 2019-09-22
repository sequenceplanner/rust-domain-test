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
pub enum SPItem<'a> {
    Model(Model<'a>),
    Resource(Resource<'a>),
    Message(Message<'a>),
    Topic(Topic<'a>),
    Variable(Variable<'a>),
    Operation(Operation<'a>),
    Ability(Ability<'a>),
    Transition(Transition<'a>),
    //SOP(SOP),
}

impl<'a> SPItem<'a> {
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
    fn node_mut(&'a mut self) -> &'a mut SPNode {
        match self {
            SPItem::Model(x) => &mut x.node,
            SPItem::Resource(x) => &mut x.node,
            SPItem::Message(x) => &mut x.node,
            SPItem::Topic(x) => &mut x.node,
            SPItem::Variable(x) => &mut x.node,
            SPItem::Operation(x) => &mut x.node,
            SPItem::Ability(x) => &mut x.node,
            SPItem::Transition(x) => &mut x.node,
        }
    }
    pub fn upd_parent(&'a mut self, parent: &'a SPItem) {
        self.node_mut().upd_parent(parent)
    }
}

#[derive(Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct SPNode<'a> {
    name: String,
    #[serde(skip)]
    parent: Option<&'a SPItem<'a>>,
}

impl<'a> SPNode<'a> {
    pub fn new(name: &str) -> SPNode<'a> {
        SPNode {
            name: name.to_string(),
            parent: None,
        }
    }

    pub fn parent(&self) -> Option<&'a SPItem> {
        self.parent.clone()
    }

    pub fn upd_parent(&mut self, parent: &'a SPItem) {
        self.parent = Some(parent);
    }
}

// Never clone the parent since we do not want to point to another model
impl Clone for SPNode<'_> {
    fn clone(&self) -> Self {
        SPNode::new(&self.name)
    }
}

impl std::fmt::Display for SPNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.parent() {
            Some(p) => write!(f, "name:{}, p({})", self.name, p.name()),
            None => write!(f, "name:{}", self.name),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Model<'a> {
    node: SPNode<'a>,
    items: Vec<SPItem<'a>>
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Resource<'a> {
    node: SPNode<'a>,
    abilities: Vec<Ability<'a>>,
    parameters: Vec<Variable<'a>>,
    messages: Vec<Topic<'a>>, // Also include estimated here on an estimated topic
                              //pub comm: ResourceComm,
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Topic<'a> {
    node: SPNode<'a>,
    msg: Message<'a>,
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Message<'a> {
    node: SPNode<'a>,
    fields: Vec<(String, MessageFields<'a>)>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum MessageFields<'a> {
    Msg(Message<'a>),
    Var(Variable<'a>),
}

impl<'a> Default for MessageFields<'a> {
    fn default() -> Self {
        MessageFields::Var(Variable::default())
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Variable<'a> {
    node: SPNode<'a>,
    type_: VariableType,
    pub initial_value: Option<SPValue>,
    pub domain: Vec<SPValue>,
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

impl<'a> Default for VariableType {
    fn default() -> Self {
        VariableType::Estimated
    }
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Transition<'a> {
    node: SPNode<'a>,
    guard: Predicate,
    actions: Vec<Action>,
    effects: Vec<Action>,
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Ability<'a> {
    node: SPNode<'a>,
    controlled: Vec<Transition<'a>>,
    uncontrolled: Vec<Transition<'a>>,
    predicates: Vec<Variable<'a>>
}

#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct Operation<'a> {
    node: SPNode<'a>,
    precondition: Vec<Transition<'a>>,
    postcondition: Vec<Transition<'a>>,
    uncontrolled: Vec<Transition<'a>>,
    predicates: Vec<Variable<'a>>,
    goal: IfThen<'a>,
    invariant: IfThen<'a>,

}

/// An IfThen is used by operaitons to define goals or invariants. When the if_
/// predicate is true, then the then_ predicate is either a goal or an invariant
/// that the planner will use for planning. TODO: Maybe we should have a better name?
#[derive(Debug, PartialEq, Clone, Default, Serialize, Deserialize)]
pub struct IfThen<'a> {
    node: SPNode<'a>,
    if_: Predicate,
    then_: Predicate,
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        // let mut m = SPItem::Model(Model{
        //         node: SPNode::new("m"),
        //         items: vec!()
        //     });
        // let n = SPItem::Model(Model{
        //     node: SPNode::new("n"),
        //     items: vec!()
        // });

        // if let SPItem::Model(mut my) = m {
        //     my.items.push(n);
        //     my.node.upd_parent(parent: &'a SPItem)
        // }
        // m.upd_parent(&n);
        // m

        // let v = x();
        
        // println!("{:?}", v);
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
