use simag::model::*;

use super::discvar_constructor;

#[test]
fn build() {
    let vars = discvar_constructor();

    let wet_grass = &vars["wet_grass"];
    let sprinkler = &vars["sprinkler"];
    let rain = &vars["rain"];
    let cloudy = &vars["cloudy"];

    let mut model = DefDiscreteModel::new();
    model.add_var(wet_grass).unwrap();
    model.add_var(sprinkler).unwrap();
    model.add_var(rain).unwrap();
    model.add_var(cloudy).unwrap();

    let choices = vec![vec![0_u8], vec![1]];
    let elements = vec![vec![0.25_f64, 0.25], vec![0.4, 0.1]];
    let cpt = CPT::new(elements, choices).unwrap();
    model.add_parent(sprinkler, cloudy, cpt).unwrap();

    let choices = vec![vec![0_u8], vec![1]];
    let elements = vec![vec![0.45_f64, 0.05], vec![0.05, 0.45]];
    let cpt = CPT::new(elements, choices).unwrap();
    model.add_parent(rain, cloudy, cpt).unwrap();

    let choices = vec![vec![0_u8], vec![1]];
    let elements = vec![vec![0.15_f64, 0.25], vec![0.05, 0.55]];
    let cpt = CPT::new(elements, choices).unwrap();
    model.add_parent(wet_grass, rain, cpt).unwrap();

    let choices = vec![vec![0_u8, 0], vec![0, 1], vec![1, 0], vec![1, 1]];
    let elements = vec![vec![0.34_f64, 0.01],
                        vec![0.01, 0.14],
                        vec![0.05, 0.10],
                        vec![0.01, 0.34]];
    let cpt = CPT::new(elements, choices).unwrap();
    model.add_parent(wet_grass, sprinkler, cpt).unwrap();
}

#[ignore]
#[test]
fn sample() {
    unimplemented!()
}
