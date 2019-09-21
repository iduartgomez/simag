//! Models can be of three classed:
//!
//! -   Bayesian Belief Networks with encoded causality.
//! -   Full contiditional networks with non-encoded causality (locality
//!   is assumed and sampling from the Markob blanket).
//! -   A sample space with random variables and unknown non-encoded
//!   causality (with possible additional latent variables), non-locality
//!   assumed.
//!
//! At the same time they come with three variants:
//!
//! -   Pure discrete random variables models.
//! -   Pure continuous random variables models.
//! -   Hybrid discrete and continuous random variables models.
//!
//! Each of the above follows a distinc strategy for sampling.

mod continuous;
mod discrete;
mod hybrid;

pub use self::continuous::ExactNormalized as ContinuousMarginalExact;
pub use self::discrete::GibbsMarginal as DiscreteMarginalGibbs;
pub use self::hybrid::CCMRFMarginal;
pub use self::hybrid::ExactNormalized as HybridMarginalExact;

use model::HybridRes;

use rgsl::MatrixF64;
use std::collections::HashMap;

/// Computes the partial correlation given conditioning set variables.
/// Using this function recursivelly for all the random variables in a (gaussian) net
/// constructs the full joint correlation matrix for the net (must be topologically sorted
/// and called in order ancestor -> child).
///
/// arguments are:
///     - x: position (identifier) of x (conditioned var) within the array (system) of variables
///     - cond: slice of positions (identifiers) of the conditioning variables of x
///     - cached: a dynamic cache of the partial correlations between the different variables
///     - mtx: the correlation matrix for the full joint distribution
#[allow(dead_code)]
fn partial_correlation(
    x: usize,
    cond: &[usize],
    cached: &mut HashMap<(usize, usize), f64>,
    mtx: &mut MatrixF64,
) {
    if cond.is_empty() {
        return;
    } else if cond.len() < 2 {
        cached
            .entry((x, cond[0]))
            .or_insert_with(|| mtx.get(x, cond[0]));
        return;
    };

    for (i, y) in cond.iter().enumerate() {
        let cond = if cond.len() > i + 1 {
            &cond[i..]
        } else {
            cached.entry((x, *y)).or_insert_with(|| mtx.get(x, *y));
            break;
        };
        let y = *y;
        let z = cond[1];

        let key_xz = &(x, z);
        let key_yz = &(y, z);
        if !cached.contains_key(key_xz) {
            partial_correlation(x, &cond[1..], cached, mtx);
        }
        if !cached.contains_key(key_yz) {
            partial_correlation(y, &cond[1..], cached, mtx);
        }

        if !cached.contains_key(&(x, y)) {
            let rho_xy = mtx.get(x, y);
            let rho_xz = *cached.get(key_xz).unwrap();
            let rho_yz = *cached.get(key_yz).unwrap();
            let rho_xyz = (rho_xy - (rho_xz * rho_yz))
                / ((1.0 - rho_xz.powi(2)) * (1.0 - rho_yz.powi(2))).sqrt();
            mtx.set(x, y, rho_xyz);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rgsl::MatrixF64;
    use std::collections::HashMap;

    //#[test]
    fn _pt_cr() {
        println!();
        // vars = "a/0", "b/1", "c/2", "d/3"
        let corr: Vec<f64> = vec![0.8, -0.8, -0.5, 0.5]; // rho_dc, rho_db, rho_ca, rho_ba

        let mut cr_matrix = MatrixF64::new(4, 4).unwrap();
        cr_matrix.set_identity();
        let mut cached: HashMap<(usize, usize), f64> = HashMap::new();

        // for b->a
        cached.insert((1, 0), corr[3]);
        cr_matrix.set(1, 0, corr[3]);
        partial_correlation(1, &[0], &mut cached, &mut cr_matrix);
        // for c->a
        cached.insert((2, 0), corr[2]);
        cr_matrix.set(2, 0, corr[2]);
        partial_correlation(2, &[0], &mut cached, &mut cr_matrix);
        // for d->b
        cached.insert((3, 1), corr[1]);
        cr_matrix.set(3, 1, corr[1]);
        // for d->c
        cached.insert((3, 2), corr[0]);
        cr_matrix.set(3, 2, corr[0]);
        partial_correlation(3, &[2, 1], &mut cached, &mut cr_matrix);

        println!("correlation matrix:\n{:?}\n", cr_matrix);
        println!("cached: {:?}", cached);
    }
}
