use simag::model::*;

use super::contvar_construct;

#[test]
fn build() {
    let vars = contvar_construct();

    let workload = &vars["recent_workload"];
    let sleep = &vars["hours_sleep"];
    let fitness = &vars["preflight_fitness"];
    let nighttime = &vars["nighttime_flight"];
    let alertness = &vars["crew_alertness"];
    let opload = &vars["operational_load"];
    let duty_period = &vars["fly_duty_period"];
    let rest_time = &vars["rest_time_on_flight"];

    let mut model = DefContModel::new();
    model.add_var(workload).unwrap();
    model.add_var(sleep).unwrap();
    model.add_var(fitness).unwrap();
    model.add_var(nighttime).unwrap();
    model.add_var(alertness).unwrap();
    model.add_var(opload).unwrap();
    model.add_var(duty_period).unwrap();
    model.add_var(rest_time).unwrap();

    model.add_parent(fitness, sleep, -0.9).unwrap();
    model.add_parent(fitness, workload, 0.9).unwrap();
    model.add_parent(alertness, fitness, 0.85).unwrap();
    model.add_parent(alertness, nighttime, -0.4).unwrap();
    model.add_parent(alertness, opload, -0.8).unwrap();
    model.add_parent(opload, nighttime, -0.95).unwrap();
    model.add_parent(opload, duty_period, 0.5).unwrap();
    model.add_parent(rest_time, duty_period, 0.8).unwrap();
}

#[ignore]
#[test]
fn sample() {
    unimplemented!()
}
