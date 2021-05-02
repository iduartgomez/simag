#!/bin/bash

# A straighforward and simple benchmark to measure the answer (aka reads) throughtput of an agent 
# with a mixed load under different configurations.


DIR=../"$( cd "$( dirname "${BASH_SOURCE[0]}" )" > /dev/null 2>&1 && pwd )"
cd "$DIR"

NUM_CPUS="$( nproc --all )"
cargo build --release --example agent_ask

printf "\nDefault number of cores, at 1.0, half and double capacity\n\n"
../target/release/examples/agent_ask 0.5
printf "\n"
../target/release/examples/agent_ask 
printf "\n"
../target/release/examples/agent_ask 2.0
printf "\n"
../target/release/examples/agent_ask 5.0

printf "\nHalf of cores, at 1.0, half and double capacity\n\n"
HALF_CORES=$(expr "$NUM_CPUS" / 2) 
../target/release/examples/agent_ask 0.5 $HALF_CORES
printf "\n"
../target/release/examples/agent_ask 1.0 $HALF_CORES
printf "\n"
../target/release/examples/agent_ask 2.0 $HALF_CORES
printf "\n"
../target/release/examples/agent_ask 5.0 $HALF_CORES

printf "\nFourth of all cores, at 1.0, half and double capacity\n\n"
FOURTH_CORES=$(expr "$NUM_CPUS" / 4)
../target/release/examples/agent_ask 0.5 $FOURTH_CORES
printf "\n"
../target/release/examples/agent_ask 1.0 $FOURTH_CORES
printf "\n"
../target/release/examples/agent_ask 2.0 $FOURTH_CORES
printf "\n"
../target/release/examples/agent_ask 5.0 $FOURTH_CORES
