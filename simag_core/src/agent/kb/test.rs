use super::repr::Representation;

use std::collections::HashSet;

#[test]
fn repr_inference_ask_pred() {
    let test_01 = "
        ( professor[$Lucy=1] )
    ";
    let q01_01 = "(professor[$Lucy=1] and person[$Lucy=1])";
    let q01_02 = "(professor[$Lucy=1])";
    let rep = Representation::default();
    rep.tell(test_01).unwrap();
    assert_eq!(rep.ask(q01_01).unwrap().get_results_single(), None);
    assert_eq!(rep.ask(q01_02).unwrap().get_results_single(), Some(true));

    let test_02 = "
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        ( let x in ( dean[x=1] := professor[x=1] ) )
        ( let x in ( professor[x=1] := person[x=1] ) )
    ";
    let q02_01 = "(professor[$Lucy>0] and person[$Lucy<1])";
    let q02_02 = "(person[$John=1])";
    let rep = Representation::default();
    rep.tell(test_02).unwrap();
    assert_eq!(rep.ask(q02_01).unwrap().get_results_single(), Some(false));
    assert_eq!(rep.ask(q02_02).unwrap().get_results_single(), Some(true));

    let test_03 = "
        ( fn::owns[$M1=1,$Nono] )
        ( missile[$M1=1] )
        ( american[$West=1] )
        ( fn::enemy[$Nono=1,$America] )
        ( let x, y, z in
            (( american[x=1] and weapon[y=1] and fn::sells[y=1,x,z] and hostile[z=1]  )
                := criminal[x=1] ))
        ( let x in
            (( fn::owns[x=1,$Nono] and missile[x=1] ) := fn::sells[x=1,$West,$Nono] ))
        ( let x in ( missile[x=1] := weapon[x=1] ) )
        ( let x in ( fn::enemy[x=1,$America] := hostile[x=1] ) )
    ";
    let q03_01 = "(criminal[$West=1]) and hostile[$Nono=1] and weapon[$M1=1]";
    let rep = Representation::default();
    rep.tell(test_03).unwrap();
    let answ = rep.ask(q03_01);
    assert_eq!(answ.unwrap().get_results_single(), Some(true));

    let test_04 = "
        # query for all 'professor'
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        ( let x in (dean[x=1] := professor[x=1]))
    ";
    let q04_01 = "(let x in (professor[x=1]))";
    let rep = Representation::default();
    rep.tell(test_04).unwrap();
    let answ = rep.ask(q04_01);
    let a04_01 = answ.unwrap().get_memberships();
    assert!(a04_01.contains_key("$Lucy"));
    assert!(a04_01.contains_key("$John"));

    let test_05 = "
        # query for all classes '$Lucy' is member of
        (professor[$Lucy=1])
        (let x in (professor[x=1] := person[x=1]))
        (ugly[$Lucy=0.2])
    ";
    let q05_01 = "(let x in (x[$Lucy>0.5]))";
    let rep = Representation::default();
    rep.tell(test_05).unwrap();
    let mut results = HashSet::new();
    results.insert("professor");
    results.insert("person");
    let answ = rep.ask(q05_01);
    let a05_01 = answ.unwrap().get_memberships();
    let mut cmp = HashSet::new();
    for a in &a05_01["$Lucy"] {
        cmp.insert(a.get_parent());
    }
    assert_eq!(results, cmp);
}

#[test]
fn repr_inference_ask_func() {
    let test_01 = "
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        ( fn::criticize[$John=1,$Lucy] )
    ";
    let q01_01 = "(fn::criticize[$John=1,$Lucy])";
    let rep = Representation::default();
    rep.tell(test_01).unwrap();
    assert_eq!(rep.ask(q01_01).unwrap().get_results_single(), Some(true));

    let test_02 = "
        ( animal[cow=1] )
        ( female[cow=1] )
        ( let x in (animal[x=1] and female[x=1]) := fn::produce[milk=1,x] )
    ";
    let q02_01 = "(fn::produce[milk=1,cow])";
    let rep = Representation::default();
    rep.tell(test_02).unwrap();
    assert_eq!(rep.ask(q02_01).unwrap().get_results_single(), Some(true));

    let test_03 = "
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        ( fn::criticize[$John=1,$Lucy] )
        ( let x in ( dean[x=1] := professor[x=1] ) )
        ( let x in ( professor[x=1] := person[x=1] ) )
        ( let x, y in
            (( person[x=1] and person[y=1] and dean[y=1] and fn::criticize[y=1,x] )
                := fn::friend[x=0,y] ))
    ";
    let q03_01 = "(fn::friend[$Lucy=0,$John])";
    let q03_02 = "(fn::friend[$Lucy<1,$John])";
    let rep = Representation::default();
    rep.tell(test_03).unwrap();
    assert_eq!(rep.ask(q03_01).unwrap().get_results_single(), Some(true));
    assert_eq!(rep.ask(q03_02).unwrap().get_results_single(), Some(true));

    let test_04 = "
        # retrieve all objs which fit to a criteria
        (fn::produce[milk=1,$Lulu])
        (cow[$Lucy=1])
        (goat[$Vicky=1])
        (let x in ((cow[x=1] or goat[x=1]) := (female[x=1] and animal[x=1])))
        (let x in ((female[x>0] and animal[x>0]) := fn::produce[milk=1,x]))
    ";
    let q04_01 = "(let x in (fn::produce[milk>0,x]))";
    let rep = Representation::default();
    rep.tell(test_04).unwrap();
    let answ = rep.ask(q04_01);
    let a04_01 = answ.unwrap().get_relationships();
    assert!(a04_01.contains_key("$Lucy"));
    assert!(a04_01.contains_key("$Lulu"));
    assert!(a04_01.contains_key("$Vicky"));

    let test_05 = "
        # retrieve all relations between objects
        (fn::loves[$Vicky=1,$Lucy])
        (fn::worships[$Vicky=1,cats])
        (fn::hates[$Vicky=0,dogs])
    ";
    let q05_01 = "(let x in (fn::x[$Vicky>0,$Lucy]))";
    let rep = Representation::default();
    rep.tell(test_05).unwrap();
    let mut results = HashSet::new();
    results.insert("loves");
    results.insert("worships");
    let answ = rep.ask(q05_01);
    let a05_01 = answ.unwrap().get_relationships();
    let mut cnt = 0;
    for a in &a05_01["$Vicky"] {
        cnt += 1;
        assert!(results.contains(a.get_name()));
        assert!(a.get_name() != "hates");
    }
    assert_eq!(cnt, 2);

    let q05_02 = "(let x, y in (fn::x[$Vicky=0,y]))";
    let answ = rep.ask(q05_02);
    let a05_02 = answ.unwrap().get_relationships();
    let mut cnt = 0;
    for a in &a05_02["$Vicky"] {
        cnt += 1;
        assert!(a.get_name() == "hates");
    }
    assert_eq!(cnt, 1);
}

#[test]
fn repr_inference_time_calc_1() {
    // Test 01
    // facts imply class, w/ t1 time arg set by antecedents & t2 set dynamically at resolution
    let test_01 = "
        ( let x, y, t1:time, t2:time='now' in
            (( dog[x=1] and meat[y=1] and fn::eat(where t1 is this.time)[y=1,x] and fn::time_calc(t1<t2) )
            := fat(where this.time is t2)[x=1] ))
        ( dog[$Pancho=1] )
        ( meat[$M1=1] )
        ( fn::eat(where this.time is '2014-07-05T10:25:00Z')[$M1=1,$Pancho] )
    ";
    let q01_01 = "(fat(where this.time is 'now')[$Pancho=1])";
    let rep = Representation::default();
    rep.tell(test_01).unwrap();
    assert_eq!(rep.ask(q01_01).unwrap().get_results_single(), Some(true));

    // Test 02
    // facts imply func, w/ t1 time arg set statically & t2 set statically by antecedent
    let test_02 = "
        ( let x, y, t1:time='2014-07-05T10:25:00Z', t2:time in
            ( ( dog[x=1] and meat[y=1] and fat(where t2 is this.time)[x=1] and fn::time_calc(t1<t2) )
            := fn::eat(where this.time is t1)[y=1,x]
            )
        )
        ( dog[$Pancho=1] )
        ( meat[$M1=1] )
        ( fat(where this.time is '2015-07-05T10:25:00Z')[$Pancho=1] )
    ";
    let rep = Representation::default();
    rep.tell(test_02).unwrap();
    let q02_01 = "(fn::eat(where this.time is 'now')[$M1=1,$Pancho])";
    let result = rep.ask(q02_01).unwrap().get_results_single();
    assert_eq!(result, Some(true));

    // Test 03
    let rep = Representation::default();
    let test_03_00 = "
        (meat[$M1=1])
        (dog[$Pancho=1])
    ";
    rep.tell(test_03_00).unwrap();

    // if fn(a) then cls(b=1) @ t1
    let test_03_01 = "
        (fn::eat(where this.time is '2015-01-01T00:00:00Z')[$M1=1,$Pancho])
        (let x, y in
            ((dog[x=1] and meat[y=1] and fn::eat[y=1,x])
            := fat[x=1]))
    ";
    rep.tell(test_03_01).unwrap();
    let q03_01 = "(fat[$Pancho=1])";
    assert_eq!(rep.ask(q03_01).unwrap().get_results_single(), Some(true));

    // if fn(b) then cls(b=0) @ t1 -- supercedes last statement
    let test_03_02 = "
        (run(where this.time is '2015-01-01T00:00:00Z')[$Pancho=1])
        (let x in (( dog[x=1] and run[x=1] ) := fat[x=0]))
    ";
    rep.tell(test_03_02).unwrap();
    let q03_02 = "(fat[$Pancho=0])";
    assert_eq!(rep.ask(q03_02).unwrap().get_results_single(), Some(true));

    // statement 1: fn(b) then cls(b=0) @ t1
    // statement 2: fn(a) then cls(b=1) @ t2
    // sta1=true if t1 > t2 else sta2=true
    let test_03_03 = "
        (run(where this.time is '2015-01-01T00:00:00Z')[$Pancho=1])
        (fn::eat(where this.time is '2015-02-01T00:00:00Z')[$M1=1,$Pancho])
        (let x, y, t1:time, t2:time in
            (run(where t1 is this.time)[x=1] and fn::eat(where t2 is this.time)[y=1,x]
            and dog[x=1] and meat[y=1] and fn::time_calc(t1<t2))
            := (fat[x=1] or fat[x=0]))
    ";
    rep.tell(test_03_03).unwrap();
    let q03_03 = "(fat[$Pancho=1])";
    assert_eq!(rep.ask(q03_03).unwrap().get_results_single(), Some(true));

    // both statements are told again and must override any previous statement
    // this should rollback
    let test_03_04 = "
        #(fn::eat(where this.time is '2015-01-02T00:00:00Z', ow)[$M1=1,$Pancho])
        (run(where this.time is '2015-02-02T00:00:00Z', ow)[$Pancho=1])
    ";
    rep.tell(test_03_04).unwrap();
    let q03_04 = "(fat[$Pancho=0])";
    // assert_eq!(rep.ask(q03_04).unwrap().get_results_single(), Some(true));
}

#[test]
fn repr_inference_time_calc_2() {
    // Test if a statement is true between time intervals
    let rep = Representation::default();
    let test_04_01 = "
        (let t1:time='2018-03-01T00:00:00Z', t2:time='2018-06-01T00:00:00Z' in
         (fat(since t1 until t2)[$Pancho=1]))
    ";
    rep.tell(test_04_01).unwrap();
    let q04_1_01 = "(fat(where this.time is '2018-04-01T00:00:00Z')[$Pancho=1])";
    assert_eq!(rep.ask(q04_1_01).unwrap().get_results_single(), Some(true));
    let q04_1_02 = "(fat(where this.time is '2018-07-01T00:00:00Z')[$Pancho=1])";
    assert_eq!(rep.ask(q04_1_02).unwrap().get_results_single(), None);
    let q04_1_03 = "(fat(where this.time is '2018-02-01T00:00:00Z')[$Pancho=1])";
    assert_eq!(rep.ask(q04_1_03).unwrap().get_results_single(), None);

    // Test if a fn is true between time intervals
    let test_04_02 = "
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        (let t1:time='2018-03-01T00:00:00Z', t2:time='2018-06-01T00:00:00Z' in
         (fn::criticize(since t1 until t2)[$John=1,$Lucy]))
    ";
    let rep = Representation::default();
    rep.tell(test_04_02).unwrap();
    let q04_2_01 = "(fn::criticize(where this.time is '2018-04-01T00:00:00Z')[$John=1,$Lucy])";
    assert_eq!(rep.ask(q04_2_01).unwrap().get_results_single(), Some(true));
    let q04_2_02 = "(fn::criticize(where this.time is '2018-07-01T00:00:00Z')[$John=1,$Lucy])";
    assert_eq!(rep.ask(q04_2_02).unwrap().get_results_single(), None);
    let q04_2_03 = "(fn::criticize(where this.time is '2018-02-01T00:00:00Z')[$John=1,$Lucy])";
    assert_eq!(rep.ask(q04_2_03).unwrap().get_results_single(), None);
}

#[test]
fn repr_eval_fol() {
    // indicative conditional
    // (true := true)
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] := ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=1] )
    ";
    let query = "( scum[$West=1] and good[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), Some(true));

    // (false := none)
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] := ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=0] )
    ";
    let query = "( scum[$West=1] and good[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), None);

    // material implication statements
    // true (true => true)
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] implies ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=1] and scum[$West=1] and good[$West=0] )
    ";
    let query = "( drugDealer[$West=1] and scum[$West=1] and good[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), Some(true));

    // true (false => true)
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] implies ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=0] and scum[$West=1] and good[$West=0] )
    ";
    let query = "( drugDealer[$West=0] and scum[$West=1] and good[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), Some(true));

    // false (true => false)
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] implies ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=1] and scum[$West=0] and good[$West=1] )
    ";
    let query0 = "( drugDealer[$West=1] )";
    let query1 = "( scum[$West=0] and good[$West=1] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query0).unwrap().get_results_single(), Some(true));
    assert_eq!(rep.ask(query1).unwrap().get_results_single(), None);

    // true (false => false)
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] implies ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=0] and scum[$West=0] and good[$West=1] )
    ";
    let query0 = "( drugDealer[$West=0] )";
    let query1 = "( scum[$West=0] and good[$West=1] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query0).unwrap().get_results_single(), Some(true));
    assert_eq!(rep.ask(query1).unwrap().get_results_single(), Some(true));

    // equivalence statements
    // is false (false <=> true )
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] equiv ( scum[$West=1] and good[$West=0] ) )
        ( scum[$West=1] )
        ( good[$West=0] )
        ( drugDealer[$West=0] )
    ";
    let query = "( drugDealer[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), None);

    // is false (true <=> false )
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] equiv ( scum[$West=1] and good[$West=0] ) )
        ( drugDealer[$West=1] )
        ( scum[$West=0] )
        ( good[$West=1] )
    ";
    let query = "( scum[$West=1] and good[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), None);

    // is true ( true <=> true )
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] equiv ( scum[$West=1] and good[$West=0] ) )
        ( scum[$West=1] )
        ( good[$West=0] )
        ( drugDealer[$West=1] )
    ";
    let query = "( drugDealer[$West=1] and scum[$West=1] and good[$West=0] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), Some(true));

    // is true ( false <=> false )
    let rep = Representation::default();
    let fol = "
        ( drugDealer[$West=1] equiv ( scum[$West=1] and good[$West=0] ) )
        ( scum[$West=0] )
        ( good[$West=1] )
        ( drugDealer[$West=0] )
    ";
    let query = "( drugDealer[$West=0] and scum[$West=0] and good[$West=1] )";
    rep.tell(fol).unwrap();
    assert_eq!(rep.ask(query).unwrap().get_results_single(), Some(true));
}
