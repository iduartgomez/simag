use super::repr::Representation;

use crate::agent::lang::Point;
use std::collections::HashSet;

#[test]
fn repr_inference_ask_pred() {
    // straight query of declared knowledge
    let test_01 = "
        ( professor[$Lucy=1] )
    ";
    let q01_01 = "(professor[$Lucy=1] and person[$Lucy=1])";
    let q01_02 = "(professor[$Lucy=1])";
    let rep = Representation::default();
    rep.tell(test_01).unwrap();
    assert_eq!(rep.ask(q01_01).unwrap().get_results_single(), None);
    assert_eq!(rep.ask(q01_02).unwrap().get_results_single(), Some(true));

    // produced by one or two log sentence
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

    // produced by a sequence of logical sentences
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
    let q03_01 = "(criminal[$West] and hostile[$Nono] and weapon[$M1])";
    let rep = Representation::default();
    rep.tell(test_03).unwrap();
    let answ = rep.ask(q03_01);
    assert_eq!(answ.unwrap().get_results_single(), Some(true));

    // retrieve all objs which fit a criteria
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

    // retrieve of all classes an object/subclass belongs to
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
    // straight query of declared knowledge
    let test_01 = "
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        ( fn::criticize[$John=1,$Lucy] )
    ";
    let q01_01 = "(fn::criticize[$John=1,$Lucy])";
    let rep = Representation::default();
    rep.tell(test_01).unwrap();
    assert_eq!(rep.ask(q01_01).unwrap().get_results_single(), Some(true));

    // produced by one log sentence
    let test_02 = "
        ( animal[cow=1] )
        ( female[cow=1] )
        ( let x in (animal[x=1] and female[x=1]) := fn::produce[milk=1,x] )
    ";
    let q02_01 = "(fn::produce[milk=1,cow])";
    let rep = Representation::default();
    rep.tell(test_02).unwrap();
    assert_eq!(rep.ask(q02_01).unwrap().get_results_single(), Some(true));

    // produced by a sequence of logical sentences
    let test_03 = "
        ( professor[$Lucy] )
        ( dean[$John] )
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

    // retrieve all objs which fit a criteria
    let test_04 = "
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

    // retrieve all relations between objects
    let test_05 = "
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

    // mixed query retrieve all relations which fit a criteria
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
            := fat(since t2)[x=1] ))
        ( dog[$Pancho=1] )
        ( meat[$M1=1] )
        ( fn::eat(since '2014-07-05T10:25:00Z')[$M1=1,$Pancho] )
    ";
    let q01_01 = "(fat(since 'now')[$Pancho=1])";
    let rep = Representation::default();
    rep.tell(test_01).unwrap();
    assert_eq!(rep.ask(q01_01).unwrap().get_results_single(), Some(true));
    // FIXME:
    // assert_eq!(
    //     rep.ask("(fat(since '2017-07-05T10:25:00Z')[$Pancho=1])")
    //         .unwrap()
    //         .get_results_single(),
    //     None
    // );

    // Test 02
    // facts imply func, w/ t1 time arg set statically & t2 set statically by antecedent
    let test_02 = "
        ( let x, y, t1:time='2017-07-05T10:25:00Z', t2:time in
            ( ( dog[x=1] and meat[y=1] and fat(where t2 is this.time)[x=1] and fn::time_calc(t1<t2) )
            := fn::eat(since t1)[y=1,x]
            )
        )
        ( dog[$Pancho=1] )
        ( meat[$M1=1] )
        ( fat(since '2018-07-05T10:25:00Z')[$Pancho=1] )
    ";
    let rep = Representation::default();
    rep.tell(test_02).unwrap();
    let q02_01 = "(fn::eat(since 'now')[$M1=1,$Pancho])";
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
        (fn::eat(since '2015-01-01T00:00:00Z')[$M1=1,$Pancho])
        (let x, y in
            ((dog[x=1] and meat[y=1] and fn::eat[y=1,x])
            := fat[x=1]))
    ";
    rep.tell(test_03_01).unwrap();
    let q03_01 = "(fat[$Pancho=1])";
    assert_eq!(rep.ask(q03_01).unwrap().get_results_single(), Some(true));

    // if fn(b) then cls(b=0) @ t1 -- supercedes last statement
    let test_03_02 = "
        (run(since '2015-01-01T00:00:00Z')[$Pancho=1])
        (let x in (( dog[x=1] and run[x=1] ) := fat[x=0]))
    ";
    rep.tell(test_03_02).unwrap();
    let q03_02 = "(fat[$Pancho=0])";
    assert_eq!(rep.ask(q03_02).unwrap().get_results_single(), Some(true));

    // statement 1: fn(b) then cls(b=0) @ t1
    // statement 2: fn(a) then cls(b=1) @ t2
    // sta1=true if t1 > t2 else sta2=true
    let test_03_03 = "
        (run(since '2015-01-01T00:00:00Z')[$Pancho=1])
        (fn::eat(since '2015-02-01T00:00:00Z')[$M1=1,$Pancho])
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
        (fn::eat(since '2015-01-02T00:00:00Z', ow)[$M1=1,$Pancho])
        (run(since '2015-02-02T00:00:00Z', ow)[$Pancho=1])
    ";
    rep.tell(test_03_04).unwrap();
    let _q03_04 = "(fat[$Pancho=0])";
    // FIXME:
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
    let q04_1_01 = "(fat(since '2018-04-01T00:00:00Z')[$Pancho=1])";
    assert_eq!(rep.ask(q04_1_01).unwrap().get_results_single(), Some(true));
    let q04_1_02 = "(fat(since '2018-07-01T00:00:00Z')[$Pancho=1])";
    assert_eq!(rep.ask(q04_1_02).unwrap().get_results_single(), None);
    let q04_1_03 = "(fat(since '2018-02-01T00:00:00Z')[$Pancho=1])";
    assert_eq!(rep.ask(q04_1_03).unwrap().get_results_single(), None);

    // TODO: "(let t0:time='2018-02-01T00:00:00Z' in fat(since t0)[$Pancho=1])"
    // "(let t0:time='2018-02-01T00:00:00Z', x in fat(since t0)[x=1])"
    // "(let t0:time='2018-02-01T00:00:00Z', x in x(since t0)[$Pancho=1])"

    // Test if a fn is true between time intervals
    let test_04_02 = "
        ( professor[$Lucy=1] )
        ( dean[$John=1] )
        (let t1:time='2018-03-01T00:00:00Z', t2:time='2018-06-01T00:00:00Z' in
         (fn::criticize(since t1 until t2)[$John=1,$Lucy]))
    ";
    let rep = Representation::default();
    rep.tell(test_04_02).unwrap();
    let q04_2_01 = "(fn::criticize(since '2018-04-01T00:00:00Z')[$John=1,$Lucy])";
    assert_eq!(rep.ask(q04_2_01).unwrap().get_results_single(), Some(true));
    let q04_2_02 = "(fn::criticize(since '2018-07-01T00:00:00Z')[$John=1,$Lucy])";
    assert_eq!(rep.ask(q04_2_02).unwrap().get_results_single(), None);
    let q04_2_03 = "(fn::criticize(since '2018-02-01T00:00:00Z')[$John=1,$Lucy])";
    assert_eq!(rep.ask(q04_2_03).unwrap().get_results_single(), None);

    // TODO: "(let t0:time='2018-02-01T00:00:00Z' in fn::criticize(since t0)[$John=1,$Lucy])"
    // "(let t0:time='2018-02-01T00:00:00Z', x in fn::x(since t0)[$John=1,$Lucy])"
    // "(let t0:time='2018-02-01T00:00:00Z', x in fn::criticize(since t0)[x=1,$Lucy])"
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

#[test]
fn repr_tell_record() {
    let rep = Representation::default();
    let source = "(
        $John = {
            fast=0,
            slow=0.5,
            dog,
            since '2015-01-02T00:00:00Z',
        }
    )";
    rep.tell(source).unwrap();
    let res = rep.ask("(fast(since '2015-01-02T00:00:00Z')[$John=0] and slow[$John=0.5])");
    assert_eq!(res.unwrap().get_results_single(), Some(true));

    let rep = Representation::default();
    let source = "(
        [$John, $Mary] = {
            fast=0,
            slow=0.5,
            cat,
            since '2015-01-02T00:00:00Z',
        }
    )";
    rep.tell(source).unwrap();
    let res = rep.ask("(fast[$Mary=0] and cat(since '2015-01-02T00:00:00Z')[$John=1.0])");
    assert_eq!(res.unwrap().get_results_single(), Some(true));

    /*
    //TODO: could be used for querying
    // defining more than one entity with similar values:
    [$john, $mary] = {
        ...
    }
    */
}

#[test]
fn repr_inference_spatial_calc() {
    let rep = Representation::default();

    // ask loc gr memb
    let source = "
        (run(at '0.0.0')[$Pancho] and sleep(at '1.1.0')[$Pancho])
    ";
    rep.tell(source).unwrap();
    let res = rep.ask("(run(at '1.1.0')[$Pancho])").unwrap();
    assert_eq!(res.get_results_single(), Some(false));
    let res = rep.ask("(sleep(at '1.1.0')[$Pancho])").unwrap();
    assert_eq!(res.get_results_single(), Some(true));

    // ask loc gr func
    let source = "
        (fn::eat(at '0.0.0')[$Pancho,meat] and fn::drink(at '1.1.0')[$Pancho,water])
    ";
    rep.tell(source).unwrap();
    let res = rep.ask("(fn::eat(at '1.1.0')[$Pancho,meat])").unwrap();
    assert_eq!(res.get_results_single(), Some(false));
    let res = rep.ask("(fn::drink(at '1.1.0')[$Pancho,water])").unwrap();
    assert_eq!(res.get_results_single(), Some(true));

    let source = "
        (fn::location($John at '0.0.0'))
        (run(at '0.0.0')[$John] and sleep(at '1.1.0')[$John])
        (let x, l1:location, l2:location, t0:time='2015-01-02T00:00:00Z' in
            (run(where l1 is this.loc)[x=1] and sleep(where l2 is this.loc)[x])
            := fn::move(from l1 to l2, since t0 until 'now')[x]
        )
    ";
    rep.tell(source).unwrap();

    // locate a concrete object at a given location right now
    let res = rep.ask("(fn::location($John at '1.1.0'))").unwrap();
    assert_eq!(res.get_results_single(), Some(true));
    let res = rep.ask("(fn::location($John at '0.0.0'))").unwrap();
    assert_eq!(res.get_results_single(), Some(false));

    // locate objects at a given location right now
    let res = rep.ask("(let x in fn::location(x at '1.1.0'))").unwrap();
    let answ = res.get_located_objects();
    assert!(answ.get(&Point::new(1, 1, 0)).is_some());
    assert!(answ.get(&Point::new(0, 0, 0)).is_none());
}
