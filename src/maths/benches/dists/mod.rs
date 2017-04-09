mod sample {
    use test::Bencher;
    use simag::dists::*;
    use simag::RGSLRng;

    // discrete:
    #[bench]
    fn categorical(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Categorical::new(vec![0.00392155_f64; 255]).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn bernoulli(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Bernoulli::new(0.5).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    // continuous:
    #[bench]
    fn cauchy(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Cauchy::std(1.).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn chisquared(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = ChiSquared::new(3).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn exponential(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Exponential::new(1.5).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn fdist(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = FDist::new(5.0, 2.0).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn gamma(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Gamma::new(1., 0.5).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn normal(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Normal::new(2., 2.).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn tdist(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = TDist::new(2.0).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn relaxed_bernoulli(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Bernoulli::new(0.7).unwrap();
        let relaxed = d.relaxed(None);
        b.iter(|| relaxed.sample(&mut rng));
    }
}

mod cdf {
    use test::Bencher;
    use simag::dists::*;

    #[bench]
    fn normal(b: &mut Bencher) {
        let d = Normal::new(2., 2.).unwrap();
        b.iter(|| d.cdf(3.));
    }

    #[bench]
    fn exponential(b: &mut Bencher) {
        let d = Exponential::new(1.5).unwrap();
        b.iter(|| d.cdf(3.));
    }
}
