#!/bin/bash

export activate_output="false"
export CASE="R02B07-R02B07"
export end_date="2020-01-01T02:00:00Z"
export SBATCH_TIMELIMIT="01:00:00"
export CPUS_PER_TASK=16
export TOT_TASKS_PER_COMP_NODE=16
export MAX_TASKS_PER_IO_NODE=4
export basedir="../../icon-exclaim"
# export basedir="../../icon-xpp"
export icon_cpu="../../icon-exclaim/santis_cpu_nvhpc/bin/icon"
export icon_gpu="../../icon-exclaim/santis_gpu_nvhpc_py_substitute/bin/icon"

export PATH=$PATH:/user-environment/linux-sles15-neoverse_v2/gcc-13.3.0/uv-0.9.4-433hgpzs5kyl4kowwpxeydlhufh7p3wr/bin

#export PYTHONPATH=$PYTHONPATH:/user-environment/linux-sles15-neoverse_v2/gcc-13.3.0/py-cffi-1.17.1-w7rl2ld75dk5yfozbbv6vhuxuewg46rd/lib/python3.11/site-packages

#source /iopsstor/scratch/cscs/ajocksch/coupled_gt4py/icon-exclaim/santis_gpu_double_py_substitute/setting

#source /iopsstor/scratch/cscs/ajocksch/coupled_gt4py/EXCLAIM_coupled_setup/run_utils/py_run_utils/.venv/bin/activate
#source /user-environment/linux-sles15-neoverse_v2/gcc-13.3.0/icon4py-icon_20250328-qzqok2kcuvge6ru3e4axp5cnckcjklxn/share/venv/bin/activate
#source /user-environment/linux-sles15-neoverse_v2/gcc-13.3.0/icon4py-git.b6779bd83a21580ec7d02d7cdd59f15209a5d941_0.0.14-git.271-aqptuphysbtsekqourwzbixpu4ekvgcu/share/venv/bin/activate

#export PYTHONPATH=$PYTHONPATH:/iopsstor/scratch/cscs/ajocksch/coupled_gt4py/EXCLAIM_coupled_setup/run_utils/py_run_utils/.venv/lib/python3.10/site-packages
export DACE_compiler_cuda_block_size_limit=256

export nproma_atm=0
export nblocks_c_atm=0
export nblocks_e_atm=1
#export activate_output="true"
#export activate_output_dyamond="true"

pushd experiments 2>&1 >/dev/null
#sbatch --nodes=16 exp.EXCLAIM_COUPLED.run hybrid --separate-io
#sbatch --nodes=7 --power-cap=50 exp.EXCLAIM_COUPLED.run hybrid
sbatch --nodes=7 exp.EXCLAIM_COUPLED.run hybrid
#sbatch --ntasks=1008 --ntasks-per-node=36 exp.EXCLAIM_COUPLED.run cpu
#sbatch --nodes=28 exp.EXCLAIM_COUPLED.run hybrid
popd 2>&1 >/dev/null
