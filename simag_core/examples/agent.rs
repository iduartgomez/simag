use simag_core::*;

const ASK_SETUP: &[&str] = &[
    // 0
    "( professor[$Lucy=1] )",
    // 1
    "( fn::owns[$M1=1,$Nono] )
    ( missile[$M1=1] )
    ( american[$West=1] )
    ( fn::enemy[$Nono=1,$America] )
    ( let x, y, z in
        (( american[x=1] and weapon[y=1] and fn::sells[y=1,x,z] and hostile[z=1]  )
            := criminal[x=1] ))
    ( let x in
        (( fn::owns[x=1,$Nono] and missile[x=1] ) := fn::sells[x=1,$West,$Nono] ))
    ( let x in ( missile[x=1] := weapon[x=1] ) )
    ( let x in ( fn::enemy[x=1,$America] := hostile[x=1] ) )",
    // 2
    "(run(since '2015-01-01T00:00:00Z')[$Pancho=1])
    (fn::eat(since '2015-02-01T00:00:00Z')[$M1=1,$Pancho])
    (let x, y, t1:time, t2:time in
        (run(where t1 is this.time)[x=1] and fn::eat(where t2 is this.time)[y=1,x]
        and dog[x=1] and meat[y=1] and fn::time_calc(t1<t2))
        := (fat[x=1] or fat[x=0]))",
];

const ASK_QUESTION: &[&str] = &[
    // 0
    "( professor[$Lucy=1] )",
    // 1
    "(criminal[$West=1]) and hostile[$Nono=1] and weapon[$M1=1]",
    // 2
    "(fat[$Pancho=1])",
];

fn new_knowledge(agent: &mut Agent) {
    for _ in 0..10_000 {
        for s in ASK_SETUP {
            agent.tell(s).unwrap();
        }
        for q in ASK_QUESTION {
            agent.ask(q).unwrap();
        }
        agent.clear();
    }
}

fn old_knowledge(agent: &Agent) {
    for _ in 0..100_000 {
        for q in ASK_QUESTION {
            agent.ask(q).unwrap();
        }
    }
}

fn main() {
    let mut agent = Agent::default();
    new_knowledge(&mut agent);
    old_knowledge(&agent);
}
