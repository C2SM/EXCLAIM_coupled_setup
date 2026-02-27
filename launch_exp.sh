#!/bin/bash

export CASE="R02B07-R02B07"
export start_date="1979-01-01T00:00:00Z"
export end_date="1979-01-02T00:00:00Z"
export restart_interval="P1D"
export control_year=1979

export SBATCH_TIMELIMIT="24:00:00"
N_NODES=16
export CPUS_PER_TASK=16
export MAX_TASKS_PER_IO_NODE=4

export GPU_MODE="py-substitute"
export basedir=$(realpath "icon-hybrid-${GPU_MODE}")

export activate_output="false"
export activate_output_dyamond="true"

export ARCHIVE="false"
export ARCHIVE_TYPE="sync"

EXP="${CASE}_${GPU_MODE}"
export ARCHIVE_DIR="/capstor/store1/cscs/userlab/cwp07/${USER}/${EXP}"

rsync -av --delete "experiments/" "${EXP}/"
pushd ${EXP} 2>&1 >/dev/null
sbatch --nodes=${N_NODES} exp.EXCLAIM_COUPLED.run hybrid --separate-io
popd 2>&1 >/dev/null
