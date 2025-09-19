#!/bin/bash
# Run a MPI task with the nsight systems profiler attached
# Execution:  srun -srun_options ./nsys_wrapper.sh path/to/icon
# 

if [[ -n "${SLURM_PROCID}" ]]; then
    # Use rank information from SLURM
    MPI_RANK=${SLURM_PROCID}
elif [[ -n "${OMPI_COMM_WORLD_RANK}" ]]; then
    # Use rank information from open MPI
    MPI_RANK=${OMPI_COMM_WORLD_RANK}
else
    echo "nsys_wrapper.sh: Can not detect current tasks MPI rank." >&2
    exit 1
fi

if [[ "${MPI_RANK}" == 0 ]]; then
    # for a comprehensive list of options, check out
    # https://docs.nvidia.com/nsight-systems/UserGuide/index.html
    nsys \
    profile \
    --trace openacc \
    --output nsys-profile-${SLURM_JOBID}-${MPI_RANK} \
    --force-overwrite true \
    --cuda-memory-usage true \
    "$@" 2>&1 | tee "LOG.nsys.${SLURM_JOBID}.${MPI_RANK}"
else
    "$@"
fi
