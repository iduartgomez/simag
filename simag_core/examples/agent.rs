use simag_core::*;

const ASK_SETUP: &[&str] = &[
    // 0
    "( professor[$Lucy,u=1] )",
    // 1
    "( fn::owns[$M1,u=1;$Nono] )
    ( missile[$M1,u=1] )
    ( american[$West,u=1] )
    ( fn::enemy[$Nono,u=1;$America] )
    (( let x, y, z )
        (( american[x,u=1] && weapon[y,u=1] && fn::sells[y,u=1;x;z] && hostile[z,u=1]  )
            := criminal[x,u=1] ))
    (( let x )
        (( fn::owns[x,u=1;$Nono] && missile[x,u=1] ) := fn::sells[x,u=1;$West;$Nono] ))
    (( let x ) ( missile[x,u=1] := weapon[x,u=1] ) )
    (( let x ) ( fn::enemy[x,u=1;$America] := hostile[x,u=1] ) )",
    // 2
    "(run(time='2015-01-01T00:00:00Z')[$Pancho,u=1])
    (fn::eat(time='2015-02-01T00:00:00Z')[$M1,u=1;$Pancho])
    ((let x, y, t1:time, t2:time)
        (run(t1=time)[x,u=1] && fn::eat(t2=time)[y,u=1;x]
        && dog[x,u=1] && meat[y,u=1] && fn::time_calc(t1<t2))
        := (fat[x,u=1] || fat[x,u=0]))",
];

const ASK_QUESTION: &[&str] = &[
    // 0
    "( professor[$Lucy,u=1] )",
    // 1
    "(criminal[$West,u=1]) && hostile[$Nono,u=1] && weapon[$M1,u=1]",
    // 2
    "(fat[$Pancho,u=1])",
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
