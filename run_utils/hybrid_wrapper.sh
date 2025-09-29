#!/usr/local/bin/bash -l

# ICON
#
# ------------------------------------------
# Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
# Contact information: icon-model.org
# See AUTHORS.TXT for a list of authors
# See LICENSES/ for license information
# SPDX-License-Identifier: BSD-3-Clause
# ------------------------------------------

export LOCAL_RANK=$SLURM_LOCALID
export GLOBAL_RANK=$SLURM_PROCID

export NUMA_IDS=(0 1 2 3)
export NUMA_NODE=${NUMA_IDS[$LOCAL_RANK % 4]}

export CUDA_VISIBLE_DEVICES=$(($LOCAL_RANK % 4))

export MPICH_GPU_SUPPORT_ENABLED=1
export MPICH_GPU_IPC_ENABLED=0	

ulimit -s unlimited

if [[ $# -lt 2 ]]; then
    # only ICON binary as argument
    numactl --cpunodebind=$NUMA_NODE --membind=$NUMA_NODE bash -c "$@"
elif [[ "$1" = "--profile" ]]; then
    NSYS_WRAPPER_PATH="$(cd $(dirname $0); pwd)/nsys_wrapper.sh"

    shift
    numactl --cpunodebind=$NUMA_NODE --membind=$NUMA_NODE bash -c "${NSYS_WRAPPER_PATH} $@"
else
    echo "Error: Unrecognised command line option: $1"
    exit 1
fi
