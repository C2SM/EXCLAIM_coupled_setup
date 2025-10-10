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

if [[ $LOCAL_RANK -lt $ATM_COMP_TASKS_PER_NODE ]]; then
    export CUDA_VISIBLE_DEVICES=$NUMA_NODE
else
    export MPICH_OFI_NIC_POLICY=NUMA
    export CUDA_VISIBLE_DEVICES=""
fi

echo "G${GLOBAL_RANK} - L${LOCAL_RANK} -> NODE: ${SLURM_NODEID} - NUMA: ${NUMA_NODE}, CUDA: ${CUDA_VISIBLE_DEVICES}"

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
