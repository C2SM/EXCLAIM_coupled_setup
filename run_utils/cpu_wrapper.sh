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

LOCAL_RANK=$((SLURM_LOCALID - ATM_COMP_TASKS_PER_NODE))
GLOBAL_RANK=$SLURM_PROCID
N_SOCKETS=$(nvidia-smi --list-gpus | wc -l)

export OMP_NUM_THREADS=1 # TODO: Adapt the whole thing to support OpenMP
export MPICH_OFI_NIC_POLICY=NUMA # TODO: Does this make sense

((NON_ATM_COMP_TASKS_PER_SOCKET=(TOT_TASKS_PER_NODE - ATM_COMP_TASKS_PER_NODE) / N_SOCKETS))

export NUMA_NODE=$((LOCAL_RANK / NON_ATM_COMP_TASKS_PER_SOCKET))

export CUDA_VISIBLE_DEVICES=""

echo "G${GLOBAL_RANK} - L${LOCAL_RANK} -> NODE: ${SLURM_NODEID} - NUMA: ${NUMA_NODE}"

numactl --cpunodebind=$NUMA_NODE --membind=$NUMA_NODE bash -c "$@"