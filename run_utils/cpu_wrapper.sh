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

GLOBAL_RANK=$SLURM_PROCID
N_SOCKETS=$(nvidia-smi --list-gpus | wc -l)

case "${TARGET}" in
    "hybrid")
        LOCAL_RANK=$((SLURM_LOCALID - ATM_COMP_TASKS_PER_NODE))

        ((NON_ATM_COMP_TASKS_PER_SOCKET=(TOT_TASKS_PER_NODE - ATM_COMP_TASKS_PER_NODE) / N_SOCKETS))

        export NUMA_NODE=$(((LOCAL_RANK / NON_ATM_COMP_TASKS_PER_SOCKET) % N_SOCKETS))

        export CUDA_VISIBLE_DEVICES=$NUMA_NODE
        ;;
    "cpu" | "cpu-cpu")
        LOCAL_RANK=$SLURM_LOCALID

        ((TOT_TASKS_PER_SOCKET=TOT_TASKS_PER_NODE / N_SOCKETS))

        export NUMA_NODE=$(((LOCAL_RANK / TOT_TASKS_PER_SOCKET) % N_SOCKETS))
        ;;
esac

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export ICON_THREADS=$SLURM_CPUS_PER_TASK

numactl --cpunodebind=$NUMA_NODE --membind=$NUMA_NODE bash -c "$@"