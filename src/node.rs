//!
//! 


use super::*;



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
    pub fn paths_mut(&mut self) -> &mut SPPaths {
        &mut self.paths
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
    fn get_child<'a>(&'a self, next: &str, path: &SPPath) -> Option<SPItemRef<'a>>;
    fn find_item_among_childs<'a>(&'a self, name: &str) -> Option<SPItemRef<'a>>;
    fn update_path_children(&mut self, paths: &SPPaths);
    fn as_ref<'a>(&'a self) -> SPItemRef<'a>;
    fn paths(&self) -> &SPPaths {
        self.node().paths()
    }
    fn is_eq(&self, path: &SPPath) -> bool {
        self.paths().is_eq(path)
    }
    fn has_global(&self) -> bool {
        self.paths().global_path().is_some()
    }
    fn get_path(&self) -> SPPath {
        if let Some(g) = self.paths().global_path() {
            return g.to_sp().clone()
        }
        if let Some(l) = self.paths().local_path() {
            return l.to_sp().clone()
        }
        panic!("We do not have a path in {} and get_path", self.node());
    }

    fn name(&self) -> &str {
        &self.node().name
    }

    /// Finds the item with a specific SPPath. Will only find locals if asked from the
    /// correct resource (or below)
    fn get<'a>(&'a self, path: &SPPath) -> Option<SPItemRef<'a>>  {
        if self.node().is_eq(path) {
            return Some(self.as_ref());
        }
        let next = self.node().next_node_in_path(path);
        if next.is_none() {
            return None
        }
        self.get_child(&next.unwrap(), path)
    }
    /// Finds the first item with a specific name.
    fn find_item<'a>(&'a self, name: &str) -> Option<SPItemRef<'a>>  {
        if self.node().name() == name {
            return Some(self.as_ref());
        }
        self.find_item_among_childs(name)
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
pub fn get_from_list<'a, T>(
    xs: &'a [T], 
    next: &str, 
    path: &SPPath) 
-> Option<SPItemRef<'a>> where T: Noder {
    for i in xs.iter() {
        if i.node().name() == next {
            if let Some(x) = i.get(path) {
                return Some(x)
            }
        }
    }
    return None
}

/// A method used by the items when impl the Noder trait
/// Tries to find an item with the path in a list that incl
/// items that impl Noder
pub fn find_item_in_list<'a, T>(
    xs: &'a [T], 
    name: &str) 
-> Option<SPItemRef<'a>> where T: Noder {
    for i in xs.iter() {
        let res = i.find_item(name);
        if res.is_some() {
            return res
        }
    }
    return None
}

/// A method used by the items when impl the Noder trait
/// Updates the path in items in the list of items impl Noder
pub fn update_path_in_list<'a, T>(
    xs: &'a mut [T], 
    paths: &SPPaths) where T: Noder {
    for i in xs.iter_mut() {
        i.update_path(paths);
    }
}


#[cfg(test)]
mod node_tesing {
    use super::*;
    #[test]
    fn get() {
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
        let ab = m.get(&g_ab);
        let acd = m.get(&g_acd);
        let k = m.get(&g_k);

        println!("{:?}", &ab);
        println!("{:?}", &acd);
        println!("{:?}", &k);

        assert!(ab.unwrap().name() == "b");
        assert!(acd.unwrap().name() == "d");
        assert!(k.unwrap().name() == "k");
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
        let ab = m.find_item("b");
        let acd = m.find_item("d");
        let k = m.find_item("k");

        println!("{:?}", &ab);
        println!("{:?}", &acd);
        println!("{:?}", &k);

        assert!(ab.unwrap().node().global_path().as_ref().unwrap().to_sp() == g_ab);
        assert!(acd.unwrap().node().global_path().as_ref().unwrap().to_sp() == g_acd);
        assert!(k.unwrap().node().global_path().as_ref().unwrap().to_sp() == g_k);
    }

}