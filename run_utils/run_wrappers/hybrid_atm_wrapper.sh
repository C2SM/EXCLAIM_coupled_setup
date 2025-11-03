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

LOCAL_RANK=$SLURM_LOCALID
GLOBAL_RANK=$SLURM_PROCID
N_SOCKETS=$(nvidia-smi --list-gpus | wc -l)

export NUMA_NODE=$((LOCAL_RANK % N_SOCKETS))

export CUDA_VISIBLE_DEVICES=$NUMA_NODE

export OMP_NUM_THREADS=1
export ICON_THREADS=1

if [ "$PROFILE" = true ]; then
    NSYS_WRAPPER_PATH="$(cd $(dirname $0); pwd)/nsys_wrapper.sh"

    numactl --cpunodebind=$NUMA_NODE --membind=$NUMA_NODE bash -c "${NSYS_WRAPPER_PATH} $@"
else
    # only ICON binary as argument
    numactl --cpunodebind=$NUMA_NODE --membind=$NUMA_NODE bash -c "$@"
fi
