mod sample {
    use test::Bencher;
    use simag::dists::*;
    use simag::RGSLRng;

    #[bench]
    fn categorical(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Categorical::new(vec![0.00392155_f64; 255]).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn binomial(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Binomial::new(0.5).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn normal(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Normal::new(2., 2.).unwrap();
        b.iter(|| d.sample(&mut rng));
    }

    #[bench]
    fn exponential(b: &mut Bencher) {
        let mut rng = RGSLRng::new();
        let d = Exponential::new(1.5).unwrap();
        b.iter(|| d.sample(&mut rng));
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
