use super::assertion::{class_decl, func_decl, FuncVariants};
use super::scope::{multiple_asserts, scope_var_decl, sentence};
use super::*;

#[test]
fn remove_comments() -> Result<(), nom::Err<ParseErrB<'static>>> {
    // remove comments:
    let source = b"
            /* stuff */
            # one line comment
            ( # first scope
                ( # second scope
                    let x, y in
                    professor[$Lucy=1]
                )
            )
            /*
                multi line
                comment
            */
        ";
    let clean = Parser::remove_comments(source)?;

    let expected = "((letx,yinprofessor[$Lucy=1]))";
    assert_eq!(
        str::from_utf8(&clean.1)
            .unwrap()
            .split_ascii_whitespace()
            .collect::<String>(),
        expected
    );
    Ok(())
}

#[test]
fn statements() -> Result<(), nom::Err<ParseErrB<'static>>> {
    let source = b"
            ( american[x=1] )
        ";
    multiple_asserts(source)?;
    Ok(())
}

#[test]
fn variables() -> Result<(), nom::Err<ParseErrB<'static>>> {
    let source = b"let a, b in";
    scope_var_decl(source)?;
    let source = b"exists a, b in";
    scope_var_decl(source)?;
    let source = b"let a, b and exist c, d in";
    scope_var_decl(source)?;

    let source = b"let a, b";
    assert!(scope_var_decl(source).is_err());
    let source = b"let a b in";
    assert!(scope_var_decl(source).is_err());
    let source = b"let a, b exist c, d in";
    assert!(scope_var_decl(source).is_err());

    Ok(())
}

#[test]
fn sentences() -> Result<(), nom::Err<ParseErrB<'static>>> {
    let source = b"
            ( american[x=1] and ( weapon[y=1] and hostile[z=1] ) )
        ";
    sentence(source)?;

    let source = b"
            ( ( american[x=1] and hostile[z=1] ) and hostile[z=1] )
        ";
    sentence(source)?;

    let source = b"
            ( american[x=1] and hostile[z=1] and ( weapon[y=1]) )
        ";
    let scanned = sentence(source);
    assert!(scanned.is_err());

    let source = b"
            ( ( american[x=1] ) and hostile[z=1] and weapon[y=1] )
        ";
    let scanned = sentence(source);
    assert!(scanned.is_err());

    let source = b"
            ( ( ( american[x=1] ) ) and hostile[z=1] and ( ( weapon[y=1] ) ) )
        ";
    let scanned = sentence(source);
    assert!(scanned.is_err());

    let source = b"
            ( american[x=1] and ( ( hostile[z=1] ) ) and weapon[y=1] )
        ";
    let scanned = sentence(source);
    assert!(scanned.is_err());

    let source = b"
        (let x, y in (american[x=1] and hostile[z=1]) := criminal[x=1])
        (let x, y in ((american[x=1] and hostile[z=1]) := criminal[x=1]))
        (let x, y in (american[x=1] and hostile[z=1]) := criminal[x=1])
        ";
    let scanned = Parser::get_blocks(source).unwrap();
    assert_eq!(scanned.len(), 3);

    Ok(())
}

macro_rules! assert_done_or_err {
    ($i:ident) => {{
        assert!(!$i.is_err());
    }};
}

#[test]
#[allow(clippy::cognitive_complexity)]
fn predicate() {
    let s1 = b"professor[$Lucy=1]";
    let s1_res = class_decl(s1);
    assert_done_or_err!(s1_res);
    let s1_res = s1_res.unwrap().1;
    assert_eq!(s1_res.name, TerminalBorrowed(b"professor"));
    assert_eq!(s1_res.args[0].term, TerminalBorrowed(b"$Lucy"));
    assert!(s1_res.args[0].uval.is_some());

    let s2 = b"missile[$M1 > -1.5]";
    let s2_res = class_decl(s2);
    assert_done_or_err!(s2_res);
    let s2_res = s2_res.unwrap().1;
    assert_eq!(s2_res.name, TerminalBorrowed(b"missile"));
    assert_eq!(s2_res.args[0].term, TerminalBorrowed(b"$M1"));
    let s2_uval = s2_res.args[0].uval.as_ref().unwrap();
    assert_eq!(s2_uval.op, Operator::More);
    assert_eq!(s2_uval.val, Number::SignedFloat(-1.5_f32));

    let s3 = b"dean(from t1 to t2, ow)[$John=0]";
    let s3_res = class_decl(s3);
    assert_done_or_err!(s3_res);
    let s3_res = s3_res.unwrap().1;
    assert_eq!(s3_res.name, TerminalBorrowed(b"dean"));
    assert_eq!(s3_res.args[0].term, TerminalBorrowed(b"$John"));
    assert!(s3_res.args[0].uval.is_some());
    assert_eq!(
        s3_res.op_args.as_ref().unwrap(),
        &vec![
            OpArgBorrowed {
                term: UnconstraintArg::Terminal(b"t1"),
                comp: Some((Operator::FromTo, UnconstraintArg::Terminal(b"t2"))),
            },
            OpArgBorrowed {
                term: UnconstraintArg::Keyword(b"ow"),
                comp: None,
            },
        ]
    );
}

#[test]
fn time_pred() {
    let s3 = b"animal(where t1 is this.time, where t1 is this.time)[cow, brown=0.5]";
    let s3_res = class_decl(s3);
    assert!(s3_res.is_err());

    let s4 = b"animal(where \"now\" is this.time and t1 is this.time, since t1)[cow, brown=0.5]";
    let s4_res = class_decl(s4);
    assert_done_or_err!(s4_res);
    let s4_res = s4_res.unwrap().1;
    assert_eq!(s4_res.args[1].term, TerminalBorrowed(b"brown"));
    assert!(s4_res.op_args.is_some());
    assert_eq!(
        s4_res.op_args.as_ref().unwrap(),
        &vec![
            OpArgBorrowed {
                term: UnconstraintArg::String(b"now"),
                comp: Some((Operator::TimeAssignment, UnconstraintArg::Keyword(b"time"))),
            },
            OpArgBorrowed {
                term: UnconstraintArg::Terminal(b"t1"),
                comp: Some((Operator::TimeAssignment, UnconstraintArg::Keyword(b"time"),)),
            },
            OpArgBorrowed {
                term: UnconstraintArg::Terminal(b"t1"),
                comp: Some((Operator::Since, UnconstraintArg::String(EMPTY))),
            }
        ]
    );

    let s6 = b"happy(where t1 is this.time, since t1 until t2)[x<=0.5]";
    let s6_res = class_decl(s6);
    assert_done_or_err!(s6_res);
    let s6_res = s6_res.unwrap().1;
    assert!(s6_res.op_args.is_some());
    assert_eq!(
        &s6_res.op_args.as_ref().unwrap()[1],
        &OpArgBorrowed {
            term: UnconstraintArg::Terminal(b"t1"),
            comp: Some((Operator::SinceUntil, UnconstraintArg::Terminal(b"t2"),)),
        }
    );

    let s7 = b"happy(where this.time is t1)[$John]";
    let s7_res = class_decl(s7);
    assert!(s7_res.is_err());
}

#[test]
fn spatial_pred() {
    let s0 = b"sleep(where 'x0.y0.z0' is this.loc)[$Mary]";
    let s0_res = class_decl(s0);
    assert_done_or_err!(s0_res);
    let s0_res = s0_res.unwrap().1;
    assert_eq!(
        s0_res.op_args.as_ref().unwrap(),
        &vec![OpArgBorrowed {
            term: UnconstraintArg::String(b"x0.y0.z0"),
            comp: Some((
                Operator::SpatialAssignment,
                UnconstraintArg::Keyword(b"loc")
            )),
        }]
    );

    let s1 = b"sleep(where l1 is this.loc)[$Mary]";
    let s1_res = class_decl(s1);
    assert_done_or_err!(s1_res);
    let s1_res = s1_res.unwrap().1;
    assert_eq!(
        s1_res.op_args.as_ref().unwrap(),
        &vec![OpArgBorrowed {
            term: UnconstraintArg::Terminal(b"l1"),
            comp: Some((
                Operator::SpatialAssignment,
                UnconstraintArg::Keyword(b"loc")
            )),
        }]
    );
}

#[test]
fn function() {
    let s1 = b"fn::criticize(since t1 until 'now')[$John=1,$Lucy]";
    let s1_res = func_decl(s1);
    assert_done_or_err!(s1_res);
    assert_eq!(s1_res.unwrap().1.variant, FuncVariants::Relational);

    let s2 = b"fn::takes[$analysis>0,$Bill]";
    let s2_res = func_decl(s2);
    assert_done_or_err!(s2_res);
    let s2_res = s2_res.unwrap().1;
    assert_eq!(s2_res.name, TerminalBorrowed(b"takes"));
    assert_eq!(s2_res.variant, FuncVariants::Relational);

    let s3 = b"fn::loves[cow=1,bull]";
    let s3_res = func_decl(s3);
    assert_done_or_err!(s3_res);
    assert_eq!(s3_res.unwrap().1.variant, FuncVariants::Relational);
}

#[test]
fn special_funcs() {
    // time_calc built-in function
    let s4 = b"fn::time_calc(t1<t2)";
    let s4_res = func_decl(s4);
    assert_done_or_err!(s4_res);
    assert_eq!(s4_res.unwrap().1.variant, FuncVariants::NonRelational);

    // move built-in function
    let s5 = b"fn::move(from l1 to '0.0.0')[x, y]";
    let s5_res = func_decl(s5);
    assert_done_or_err!(s5_res);

    let s6 = b"fn::move(from '0.0.0' to l1)[x, y]";
    let s6_res = func_decl(s6);
    assert_done_or_err!(s6_res);

    let s7 = b"fn::move(from l1 to '0.0.0', since t0 until t1)[y]";
    let s7_res = func_decl(s7);
    assert_done_or_err!(s7_res);
    let s7_res = s7_res.unwrap().1;
    assert_eq!(
        s7_res.op_args.as_ref().unwrap(),
        &vec![
            OpArgBorrowed {
                term: UnconstraintArg::Terminal(b"l1"),
                comp: Some((Operator::FromTo, UnconstraintArg::String(b"0.0.0"))),
            },
            OpArgBorrowed {
                term: UnconstraintArg::Terminal(b"t0"),
                comp: Some((Operator::SinceUntil, UnconstraintArg::Terminal(b"t1"),)),
            }
        ]
    );
    assert_eq!(s7_res.args.as_ref().unwrap().len(), 1);

    let s8 = b"fn::move(to '0.0.0', at t1)[x]";
    let s8_res = func_decl(s8);
    assert_done_or_err!(s8_res);

    let s9 = b"fn::move(from '0.0.0')[x]"; // is err
    let s9_res = func_decl(s9);
    assert!(s9_res.is_err());

    let _s10 = b"(fn::location($John at '0.0.0'))";
}

#[test]
fn declare_record() {
    //this is an entity and all the classes memberships in one go, e.g.:
    let source = b"(
        $John = {
            fast=0,
            slow=0.5,
            dog, 
            since 'now',
        }
    )";
    let result = record_decl(source);
    assert!(result.is_ok());
    let stmt = match result.unwrap().1 {
        ASTNode::Chain(nodes) => nodes
            .into_iter()
            .find(|decl| match decl {
                ASTNode::Assert(decl) => match decl {
                    AssertBorrowed::ClassDecl(cls) if cls.name == TerminalBorrowed(b"dog") => true,
                    _ => false,
                },
                _ => false,
            })
            .unwrap(),
        _ => panic!(),
    };
    match stmt {
        ASTNode::Assert(AssertBorrowed::ClassDecl(cls)) => {
            assert!(cls.args.len() == 1);
            assert_eq!(
                cls.args[0],
                ArgBorrowed {
                    term: TerminalBorrowed(b"$John"),
                    uval: None
                }
            );
            assert!(cls.op_args.unwrap().len() == 1);
        }
        _ => panic!(),
    }

    let source = b"(
        [$John, $Mary] = {
            fast=0,
            slow=0.5,
            cat,
            since '2015-01-02T00:00:00Z',
        }
    )";
    let result = record_decl(source);
    assert!(result.is_ok());
}
