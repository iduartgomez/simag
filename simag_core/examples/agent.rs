use std::{sync::Arc, time::Instant};

use simag_core::*;

const ASK_SETUP: &[&str] = &[
    // 0
    "( professor[$Lucy=1] )",
    // 1
    "
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
    ",
    // 2
    "
    (meat[$M1=1])
    (dog[$Pancho=1])
    (run(since '2015-01-01T00:00:00Z')[$Pancho=1])
    (fn::eat(since '2015-02-01T00:00:00Z')[$M1=1,$Pancho])
    (let x, y, t1:time, t2:time in
        (run(where t1 is this.time)[x=1] and fn::eat(where t2 is this.time)[y=1,x]
        and dog[x=1] and meat[y=1] and fn::time_calc(t1<t2))
        := (fat[x=1] or fat[x=0]))
    ",
];

const ASK_QUESTION: &[&str] = &[
    // 0
    "( professor[$Lucy=1] )",
    // 1
    "(criminal[$West=1] and hostile[$Nono=1] and weapon[$M1=1])",
    // 2
    "(fat[$Pancho=1])",
];

fn new_knowledge(agent: &mut Agent) {
    for _ in 0..10 {
        agent.clear();
        for s in ASK_SETUP {
            agent.tell(s).unwrap();
        }
        for q in ASK_QUESTION {
            let _r = agent.ask(q).unwrap();
            // eprintln!("query: {}, result: {:?}", q, _r.get_results_single());
        }
    }
}

fn old_knowledge(agent: Arc<Agent>, threads: usize, iters_per_reader: usize) {
    // load at 75% capacity
    let paralellism: usize = threads / 4 * 3;
    println!(
        "testing with {} parallel readers; {} asks each",
        paralellism, iters_per_reader
    );

    for s in ASK_SETUP {
        agent.tell(s).unwrap();
    }

    let mut threads = Vec::with_capacity(paralellism);
    let t0 = Instant::now();
    for _ in 0..paralellism {
        let ag_cl = agent.clone();
        threads.push(std::thread::spawn(move || {
            for _ in 0..iters_per_reader {
                for q in ASK_QUESTION {
                    ag_cl.ask(q).unwrap();
                }
            }
        }));
    }

    threads.into_iter().fold(Ok(()), |_, h| h.join()).unwrap();
    let t1 = Instant::now();

    let diff = (t1 - t0).as_nanos() as f64 / 1e9;
    let total = paralellism * iters_per_reader;
    println!(
        "took {:.2} secs to process a total of {} requests, ~{} req/sec",
        diff,
        total,
        (total as f64 / diff).round()
    );
}

fn main() {
    let threads = num_cpus::get();
    let mut agent = Agent::default().with_threads(threads);
    new_knowledge(&mut agent);
    old_knowledge(Arc::new(agent), threads, 10_000);
}
