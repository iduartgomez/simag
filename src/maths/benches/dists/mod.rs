mod sample {
    use test::Bencher;
    use simag::dists::*;

    #[bench]
    fn categorical(b: &mut Bencher) {
        let d = Categorical::new(vec![0.00392155_f64; 255]).unwrap();
        b.iter(|| d.sample());
    }

    #[bench]
    fn binomial(b: &mut Bencher) { 
        let d = Binomial::new(0.5).unwrap();
        b.iter(|| d.sample());
    }
}

mod compute {
    use test::Bencher;
    use simag::dists::*;

    #[bench]
    fn cdf(b: &mut Bencher) {
        let d = Normal::new(2., 2.).unwrap();
        b.iter(|| d.cdf(3.));
    }
}
