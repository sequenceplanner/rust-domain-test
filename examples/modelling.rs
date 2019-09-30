
use domain_test::*;

fn main() {
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
    

        let resource = if let Some(SPItemRef::Resource(r)) = m.find(&r1.to_sp()) {Some(r)} else {None};
        println!("");
        println!("resource: {:?}", resource);
        println!("");

        if let Some(SPItemRef::Resource(r)) = m.find(&r1.to_sp()) {
            let a_again = r.find(&a.to_sp());
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