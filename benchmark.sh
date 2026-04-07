#!/bin/bash

# Example run script for a 2 days simulation with daily restarts

submit_bench(){
    export basedir=$(realpath "icon-hybrid-${GPU_MODE}")
    EXP="${CASE}_${GPU_MODE}_${N_NODES}"
    [ -n "${1}" ] && export EXP+="_${1}" 
    echo "submitting ${EXP}"
    rsync -a --delete "experiments/" "${EXP}/"
    pushd ${EXP} 2>&1 >/dev/null
    sbatch --nodes=${N_NODES} --job-name=${EXP} exp.EXCLAIM_COUPLED.run hybrid --separate-io
    popd 2>&1 >/dev/null
}

export CASE="R02B10-R02B10"
export start_date="1979-01-01T00:00:00Z"
export end_date="1979-01-02T00:00:00Z"
export restart_interval="P1D"
export control_year=1979

export SBATCH_TIMELIMIT="02:00:00"
export CPUS_PER_TASK=16
export MAX_TASKS_PER_IO_NODE=4

export activate_output_dyamond="true"
export ARCHIVE="false"

#NODES=("200" "400" "800")
NODES=("160")

#for N_NODES in ${NODES[@]}; do
#    export activate_output="false"
#    export GPU_MODE="py-substitute"
#    submit_bench "NOIO_fill_cache"
#    export activate_output="true"
#    ((N_NODES += 8))
#    submit_bench "IO_fill_cache"
#done

for N_NODES in ${NODES[@]}; do
    # no IO
    # -----
    export activate_output="false"
    # py-substitute
    export GPU_MODE="py-substitute"
    export nblocks_c_atm=0
    export nblocks_e_atm=1
    submit_bench "NOIO"
    # acc (after build finished)
  # export GPU_MODE="acc"
  # export nblocks_c_atm=1
  # export nblocks_e_atm=0
  # submit_bench "NOIO"
    # activate IO
    # -----------
  # export activate_output="true"
  # ((N_NODES += 8))
  # # py-substitute
  # export GPU_MODE="py-substitute"
  # export nblocks_c_atm=0
  # export nblocks_e_atm=1
  # submit_bench "IO"
  # # acc
  # export GPU_MODE="acc"
  # export nblocks_c_atm=1
  # export nblocks_e_atm=0
  # submit_bench "IO"
done
