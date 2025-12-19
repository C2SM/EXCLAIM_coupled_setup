#!/bin/bash

# ============================================================================
#SBATCH --account=cwp07
#SBATCH --partition=xfer
#SBATCH --job-name=ARCHIVE
#SBATCH --output=LOG.ARCHIVE.%j
#SBATCH --error=LOG.ARCHIVE.%j
#SBATCH --time=01:00:00
#SBATCH --nodes=1


mkdir -p ${ARCHIVE_DIR}

# rclone command
rclone move --include "/${EXPNAME}*/${chunk_start_date}_${chunk_end_date}/**" --include "${EXPNAME}_restart_*_${chunk_start_date//[-:]}.nc" ./ ${ARCHIVE_DIR}/ 
