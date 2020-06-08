use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use simag_core::Agent;

const TELL_TEST: &[&str] = &[
    "( professor[$Lucy,u=1] )",
    "( fn::owns[$M1,u=1;$Nono] )",
    "((let x) (dean[x,u=1] := professor[x,u=1]))",
    "((let x, y, t1:time, t2:time)
        (run(t1=time)[x,u=1] && fn::eat(t2=time)[y,u=1;x]
        && dog[x,u=1] && meat[y,u=1] && fn::time_calc(t1<t2))
        := (fat[x,u=1] || fat[x,u=0]))",
];

fn setup(c: &mut Criterion) {
    c.bench_function("setup", |b| b.iter(|| Agent::default()));
}

fn tell_insert(c: &mut Criterion) {
    let mut group = c.benchmark_group("tell_insert");
    for (num, test) in TELL_TEST.iter().enumerate() {
        group.bench_with_input(BenchmarkId::from_parameter(num), test, |b, &t| {
            let mut rep = Agent::default();
            b.iter(|| {
                rep.tell(t).unwrap();
                rep.clear();
            });
        });
    }
    group.finish();
}

fn tell_upsert(c: &mut Criterion) {
    let mut group = c.benchmark_group("tell_upsert");
    for (num, test) in TELL_TEST.iter().enumerate() {
        group.bench_with_input(BenchmarkId::from_parameter(num), test, |b, &t| {
            let rep = Agent::default();
            b.iter(|| {
                rep.tell(t).unwrap();
            });
        });
    }
    group.finish();
}

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

fn ask_once(c: &mut Criterion) {
    let mut group = c.benchmark_group("ask_once");
    for (num, test) in ASK_QUESTION.iter().enumerate() {
        group.bench_with_input(BenchmarkId::from_parameter(num), test, |b, &t| {
            b.iter_custom(|iters| {
                let mut rep = Agent::default();
                let mut total = std::time::Duration::new(0, 0);
                for _ in 0..iters {
                    let start = std::time::Instant::now();
                    rep.tell(ASK_SETUP[num]).unwrap();
                    black_box(rep.ask(t).unwrap());
                    let t = start.elapsed();
                    total += t;
                    rep.clear();
                }
                total
            });
        });
    }
    group.finish();
}

fn ask_many(c: &mut Criterion) {
    let mut group = c.benchmark_group("ask_many");
    for (num, test) in ASK_QUESTION.iter().enumerate() {
        group.bench_with_input(BenchmarkId::from_parameter(num), test, |b, &t| {
            b.iter_custom(|iters| {
                let rep = Agent::default();
                rep.tell(ASK_SETUP[num]).unwrap();
                let start = std::time::Instant::now();
                for _ in 0..iters {
                    black_box(rep.ask(t).unwrap());
                }
                start.elapsed()
            });
        });
    }
    group.finish();
}

criterion_main!(repr);

criterion_group! {
    name = repr;
    config = Criterion::default();
    targets = setup, tell_insert, tell_upsert, ask_once, ask_many
}
