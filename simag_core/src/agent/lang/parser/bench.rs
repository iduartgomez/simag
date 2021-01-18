use std::time::Instant;

use super::{ParseErrB, ParseTree, Parser};

#[test]
#[ignore]
fn remove_comments() -> Result<(), nom::Err<ParseErrB<'static>>> {
    const REPETITIONS: i32 = 10_000;
    const SOURCE: &[u8] = b"
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

    let t0 = Instant::now();
    for _ in 0..REPETITIONS {
        Parser::remove_comments(SOURCE)?;
    }
    let t1 = Instant::now();
    let iter_time = (t1 - t0).as_nanos() as f64 / 1e3 / REPETITIONS as f64;
    println!("took {} us/iter; {} iters/sec", iter_time, 1e6 / iter_time);
    Ok(())
}

const CLEAN_SOURCE: &[u8] = b"
    (let x, y in (american[x=1] and hostile[z=1]) := criminal[x=1])
    (let x, y in ((american[x=1] and hostile[z=1]) := criminal[x=1]))
    (let x, y in (american[x=1] and hostile[z=1]) := criminal[x=1])
";

#[test]
#[ignore]
fn form_ast() -> Result<(), ParseErrB<'static>> {
    const REPETITIONS: i32 = 10_000;

    let t0 = Instant::now();
    for _ in 0..REPETITIONS {
        Parser::get_blocks(CLEAN_SOURCE)?;
    }
    let t1 = Instant::now();
    let iter_time = (t1 - t0).as_nanos() as f64 / 1e3 / REPETITIONS as f64;
    println!("took {} us/iter; {} iters/sec", iter_time, 1e6 / iter_time);
    Ok(())
}

#[test]
#[ignore]
fn process_ast() -> Result<(), ParseErrB<'static>> {
    const REPETITIONS: usize = 1_000;
    let mut ast = vec![Parser::get_blocks(CLEAN_SOURCE)?; REPETITIONS];
    let t0 = Instant::now();
    for _ in 0..REPETITIONS {
        let _ = ast
            .pop()
            .unwrap()
            .into_iter()
            .map(|ast| ParseTree::process_ast(&ast, true))
            .collect::<Vec<_>>();
    }
    let t1 = Instant::now();
    let iter_time = (t1 - t0).as_nanos() as f64 / 1e3 / REPETITIONS as f64;
    println!("took {} us/iter; {} iters/sec", iter_time, 1e6 / iter_time);
    Ok(())
}
