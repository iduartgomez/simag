use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use simag_core::Agent;

const TELL_TEST: &[&str] = &[
    "( professor[$Lucy=1] )",
    "( fn::owns[$M1=1,$Nono] )",
    "(let x in (dean[x=1] := professor[x=1]))",
    "(let x, y, t1:time, t2:time in
        (run(where t1 is this.time)[x=1] an fn::eat(where t2 is this.time)[y=1,x]
        an dog[x=1] an meat[y=1] an fn::time_calc(t1<t2))
        := (fat[x=1] an fat[x=0]))",
];

fn setup(c: &mut Criterion) {
    c.bench_function("setup", |b| b.iter(Agent::default));
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
    "( professor[$Lucy=1] )",
    // 1
    "( fn::owns[$M1=1,$Nono] )
    ( missile[$M1=1] )
    ( american[$West=1] )
    ( fn::enemy[$Nono=1,$America] )
    ( let x, y, z in
        (( american[x=1] an weapon[y=1] an fn::sells[y=1,x,z] an hostile[z=1]  )
            := criminal[x=1] ))
    ( let x in
        (( fn::owns[x=1,$Nono] an missile[x=1] ) := fn::sells[x=1,$West,$Nono] ))
    ( let x in ( missile[x=1] := weapon[x=1] ) )
    ( let x in ( fn::enemy[x=1,$America] := hostile[x=1] ) )",
    // 2
    "(run(since '2015-01-01T00:00:00Z')[$Pancho=1])
    (fn::eat(since '2015-02-01T00:00:00Z')[$M1=1,$Pancho])
    (let x, y, t1:time, t2:time in
        (run(where t1 is this.time)[x=1] an fn::eat(where t2 is this.time)[y=1,x]
        an dog[x=1] an meat[y=1] an fn::time_calc(t1<t2))
        := (fat[x=1] an fat[x=0]))",
];

const ASK_QUESTION: &[&str] = &[
    // 0
    "( professor[$Lucy=1] )",
    // 1
    "(criminal[$West=1]) an hostile[$Nono=1] an weapon[$M1=1]",
    // 2
    "(fat[$Pancho=1])",
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
