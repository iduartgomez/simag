mod continuous;
mod discrete;

use std::collections::HashMap;

use simag::dists::*;
use simag::model::*;

fn discvar_constructor() -> HashMap<&'static str, DefDiscreteVar> {
    let mut vars = HashMap::new();
    let cp = DType::Bernoulli(Bernoulli::new(0.6).unwrap());
    vars.insert("cloudy", DefDiscreteVar::with_dist(cp).unwrap());
    let cp = DType::Bernoulli(Bernoulli::new(0.1).unwrap());
    vars.insert("sprinkler", DefDiscreteVar::with_dist(cp).unwrap());
    let cp = DType::Bernoulli(Bernoulli::new(0.4).unwrap());
    vars.insert("rain", DefDiscreteVar::with_dist(cp).unwrap());
    let cp = DType::Bernoulli(Bernoulli::new(0.7).unwrap());
    vars.insert("wet_grass", DefDiscreteVar::with_dist(cp).unwrap());
    vars
}

fn contvar_construct() -> HashMap<&'static str, DefContVar> {
    let mut vars = HashMap::new();
    let cp = DType::Normal(Normal::std());
    vars.insert(
        "recent_workload",
        DefContVar::with_dist(cp.clone()).unwrap(),
    );
    vars.insert("hours_sleep", DefContVar::with_dist(cp.clone()).unwrap());
    vars.insert(
        "preflight_fitness",
        DefContVar::with_dist(cp.clone()).unwrap(),
    );
    vars.insert(
        "nighttime_flight",
        DefContVar::with_dist(cp.clone()).unwrap(),
    );
    vars.insert("crew_alertness", DefContVar::with_dist(cp.clone()).unwrap());
    vars.insert(
        "operational_load",
        DefContVar::with_dist(cp.clone()).unwrap(),
    );
    vars.insert(
        "fly_duty_period",
        DefContVar::with_dist(cp.clone()).unwrap(),
    );
    vars.insert("rest_time_on_flight", DefContVar::with_dist(cp).unwrap());
    vars
}
