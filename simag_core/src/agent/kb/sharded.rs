use smallvec::SmallVec;

struct ShardedRepresentation {
    shards: SmallVec<[Representation; 1]>,
    total_ent: usize,
    total_cls: usize,
}

impl ShardedRepresentation {
    const RESIZE_LIMIT: usize = 1_000usize;

    fn new(threads: usize) -> ShardedRepresentation {
        ShardedRepresentation {
            shards: SmallVec::from_buf([Representation::new(threads)]),
            total_ent: 0,
            total_cls: 0,
        }
    }

    #[inline]
    fn ask(&self, source: &str) -> Result<kb::repr::Answer, kb::repr::QueryErr> {
        let l = self.shards.len();
        match l {
            1 => self.shards[0].ask(source),
            3 => todo!(),
            9 => todo!(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn tell(&self, source: &str) -> Result<(), Vec<ParseErrF>> {
        let l = self.shards.len();
        match l {
            1 => self.shards[0].tell(source),
            3 => todo!(),
            9 => todo!(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn set_thread_pool(&mut self, threads: usize) {
        self.shards[0].set_threads(threads);
    }

    #[inline]
    fn clear(&mut self) {
        self.shards[0].clear()
    }

    fn rebalance(&mut self) {
        if self.total_ent > Self::RESIZE_LIMIT || self.total_cls > Self::RESIZE_LIMIT {
            // schedule a re-balance of inner schards
        }
    }
}
