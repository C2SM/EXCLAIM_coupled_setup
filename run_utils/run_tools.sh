#!/usr/bin/bash


compute_task_distribution_variables(){


   if [ "$SEPARATE_IO" = true ]; then
      ((TOT_IO_TASKS = ATM_IO_TASKS + OCE_IO_TASKS))

      # Adding (MAX_TASKS_PER_IO_NODE - 1) to TOT_IO_TASKS to mimic rounding up the result of the division
      ((TOT_IO_NODES = (TOT_IO_TASKS + MAX_TASKS_PER_IO_NODE - 1) / MAX_TASKS_PER_IO_NODE ))
      ((TOT_COMP_NODES = SLURM_JOB_NUM_NODES - TOT_IO_NODES))

      ((ATM_TOT_TASKS = ATM_COMP_TASKS_PER_NODE * TOT_COMP_NODES + ATM_IO_TASKS))
      ((TOT_TASKS = TOT_TASKS_PER_COMP_NODE * TOT_COMP_NODES + TOT_IO_TASKS))
   else
      ((ATM_TOT_TASKS = ATM_COMP_TASKS_PER_NODE * SLURM_JOB_NUM_NODES + ATM_IO_TASKS))
      ((TOT_TASKS = TOT_TASKS_PER_NODE * SLURM_JOB_NUM_NODES))
   fi

   export TOT_TASKS
}

create_multiprog_file(){

   ATM_MIN_RANK=0
   ((ATM_MAX_RANK = ATM_TOT_TASKS - 1))
   ((OCE_MIN_RANK = ATM_MAX_RANK + 1))
   ((OCE_MAX_RANK = TOT_TASKS - 1))

   # Write multi-prog distribution to file
   if [[ "${TARGET}" == "hybrid" ]]; then
      cat > multi-prog.conf << EOF
${ATM_MIN_RANK}-${ATM_MAX_RANK} ../run_utils/run_wrappers/hybrid_atm_wrapper.sh ./icon_gpu
${OCE_MIN_RANK}-${OCE_MAX_RANK} ../run_utils/run_wrappers/hybrid_oce_wrapper.sh ./icon_cpu
EOF
      chmod 755 multi-prog.conf
   elif [[ "${TARGET}" == "cpu-cpu" ]]; then
      cat > multi-prog.conf << EOF
${ATM_MIN_RANK}-${ATM_MAX_RANK} ../run_utils/run_wrappers/cpu-cpu_atm_wrapper.sh ./icon_cpu
${OCE_MIN_RANK}-${OCE_MAX_RANK} ../run_utils/run_wrappers/cpu-cpu_oce_wrapper.sh ./icon_cpu
EOF
      chmod 755 multi-prog.conf
   fi
}

set_environment(){

   ulimit -s unlimited
   ulimit -c 0

   # Gt4Py
   # -----
   export CUDAARCHS=90
   export PYTHONOPTIMIZE=2
   export GT4PY_BUILD_CACHE_DIR=$basedir/build-gpu-py-substitute/bin
   export GT4PY_BUILD_CACHE_LIFETIME=persistent
   export GT4PY_UNSTRUCTURED_HORIZONTAL_HAS_UNIT_STRIDE=1

   # Dump SLURM environment variables
   # --------------------------------
   set | grep SLURM

   # Libfabric / Slingshot
   # ---------------------
   export FI_CXI_SAFE_DEVMEM_COPY_THRESHOLD=0
   export FI_CXI_RX_MATCH_MODE=software
   export FI_MR_CACHE_MONITOR=disabled
   export FI_MR_CACHE_MAX_COUNT=0
   export FI_CXI_OFLOW_BUF_COUNT=10

   # MPICH
   # -----
   if [[ "${TARGET}" == "hybrid" ]]; then
      export MPICH_GPU_SUPPORT_ENABLED=1
      # export MPICH_GPU_IPC_ENABLED=0
      export MPICH_RDMA_ENABLED_CUDA=1
      export MPICH_OFI_NIC_POLICY=GPU
   else
      export MPICH_OFI_NIC_POLICY=NUMA
   fi

   # NVHPC/CUDA
   # ----------
   if [[ "${TARGET}" == "hybrid" ]]; then
      export NVCOMPILER_ACC_SYNCHRONOUS=0
      export NVCOMPILER_ACC_DEFER_UPLOADS=1
      export NVCOMPILER_ACC_USE_GRAPH=1  # Harmless if cuda-graphs is disabled
      export NVCOMPILER_ACC_NOTIFY=0
      export NVCOMPILER_TERM=trace
      export CUDA_BUFFER_PAGE_IN_THRESHOLD_MS=0.001
      # export CRAY_CUDA_MPS=1  # Only needed if we oversubscribe the GPU
   fi

   # OpenMP
   # ------
   export OMP_SCHEDULE=guided,16
   export OMP_DYNAMIC="false"
   export OMP_STACKSIZE=200M

   # From MPIM
   # ---------
   # NOTE: All MKL*, OMPI*, UCX* KMP* variables from MPIM setup
   #       are irrelevant on Santis.
   # TODO: check if MALLOC_TRIM_THRESHOLD_="-1" is necessary
}

run_model(){
   set -x
   case "${TARGET}" in
      "hybrid")
         if [ "$SEPARATE_IO" = true ]; then
            srun \
               -l \
               --kill-on-bad-exit=1 \
               --distribution="arbitrary" \
               --hint="nomultithread" \
               --ntasks="${TOT_TASKS}" \
               --cpus-per-task="${CPUS_PER_TASK}" \
               --multi-prog multi-prog.conf
         else
            srun \
               -l \
               --kill-on-bad-exit=1 \
               --distribution="arbitrary" \
               --hint="nomultithread" \
               --ntasks="${TOT_TASKS}" \
               --ntasks-per-node="${TOT_TASKS_PER_NODE}" \
               --cpus-per-task="${CPUS_PER_TASK}" \
               --multi-prog multi-prog.conf
         fi
      ;;
      "cpu-cpu")
         srun \
            -l \
            --kill-on-bad-exit=1 \
            --distribution="arbitrary" \
            --hint="nomultithread" \
            --ntasks="${TOT_TASKS}" \
            --ntasks-per-node="${TOT_TASKS_PER_NODE}" \
            --cpus-per-task="${CPUS_PER_TASK}" \
            --multi-prog multi-prog.conf
      ;;
      "cpu")
         srun \
            -l \
            --kill-on-bad-exit=1 \
            --distribution="arbitrary" \
            --hint="nomultithread" \
            --ntasks="${TOT_TASKS}" \
            --ntasks-per-node="${TOT_TASKS_PER_NODE}" \
            --cpus-per-task="${CPUS_PER_TASK}" \
            ../run_utils/run_wrappers/cpu_atm-oce_wrapper.sh ./icon_cpu
      ;;
   esac
   status="$?"
   set +x
   return $status
}

restart_model(){
    atm_finish_status_file="finish_${atm_model_name}.status"
    oce_finish_status_file="finish_${oce_model_name}.status"
    
    if [ ! -f "${atm_finish_status_file}" ] || [ ! -f "${oce_finish_status_file}" ]; then
        echo
        echo "At least one of ${atm_finish_status_file} or ${oce_finish_status_file} file not found"
        echo
        echo "============================"
        echo "Script failed"
        echo "============================"
        echo
        exit 1
    fi

    atm_finish_status="$(cat "${atm_finish_status_file}" | xargs echo)"
    oce_finish_status="$(cat "${oce_finish_status_file}" | xargs echo)"
    
    if [ "${atm_finish_status}" != "${oce_finish_status}" ]; then
        echo
        echo "script ended with different finish status for ${atm_model_name} (${atm_finish_status}) and ${oce_model_name} (${oce_finish_status})"
        echo
        echo "============================"
        echo "Script failed"
        echo "============================"
        exit 1
    fi

    echo
    echo "finish status: ${atm_finish_status}"
    echo
    echo "============================"
    echo "Script ran successfully"
    echo "============================"
    echo
    echo

    echo " ==> Accounting"
    sacct -j "${SLURM_JOB_ID}" --format "JobID, JobName, AllocCPUs, Elapsed, ElapsedRaw, CPUTimeRAW, ConsumedEnergyRaw, MaxRSS, MaxVMSize, AveRSS"

    if [ "${atm_finish_status}" == "RESTART" ]; then
        unset SLURM_HOSTFILE
        export lrestart=.true.
        export chunk_start_date="${chunk_end_date}"
        export FIRST_RUN="false"
        [ -n "${SBATCH_TIMELIMIT}" ] && export SBATCH_TIMELIMIT
        echo
        SBATCH_OPTIONS="--nodes=${SLURM_NNODES}"
        submit_cmd="sbatch ${SBATCH_OPTIONS} ${RUNSCRIPT_PATH} ${TARGET} ${RUN_OPTIONS[@]}"
        echo " ==> submitting next chunk starting at ${chunk_start_date}"
        set -x
        ${submit_cmd}
        set +x
        rm -f ${atm_finish_status_file} ${oce_finish_status_file}
    fi
}

archive_output(){
    if [ "${ARCHIVE}" == "true" ]; then
        echo " ==> submiting archiving job for the current chunk"
        unset SLURM_HOSTFILE
        unset SLURM_DISTRIBUTION
        set -x
        sbatch ./archive.sh
        set +x
    fi
}

set_ocean_vertical_coordinate(){
   if [[ "${VERT_COR}" == 0 ]] ; then
      vert_cor_type=0
      select_lhs=2
      l_lhs_direct=.true.
      STRETCH_C=''
      limitice=0.8                #  12m mixed layer depth assumed, i.e. 80% maximum sea-ice depth
   else
      vert_cor_type=1
      select_lhs=1
      l_lhs_direct=.false.
      STRETCH_C=",'stretch_c'"    #  output of variable thicknes
      limitice=5.0                #  2m upper layer depth assumed, i.e. 10m maximum sea-ice depth
   fi

   case "${ocean_vertical_levels}" in
      "L40")
         n_zlev=40
         dzlev_m="12.,10.,10.,10.,10.,10.,13.,15.,20.,25.,30.,35.,40.,45.,50.,55.,60.,70.,80.,90.,
               100.,110.,120.,130.,140.,150.,170.,180.,190.,200.,220.,250.,270.,300.,350.,400.,
               450.,500.,500.,600."
         levidx_100m=9
         levidx_200m=12
         levidx_2000m=30
         minVerticalLevels=2
         ;;
      "L64")
         n_zlev=64
         dzlev_m="12.,10.,10.,10.,10.,10.,10.,10.,10.,10.,11.,12.,13.,14.,15.,16.,17.,18.,20.,22.,
                  24.,26.,28.,30.,32.,35.,38.,41.,45.,49.,53.,58.,62.,66.,71.,75.,80.,85.,91.,97.,
                  104.,111.,118.,125.,132.,138.,145.,152.,160.,167.,175.,182.,188.,195.,201.,208.,
                  213.,219.,224.,230.,235.,241.,250.,260."
         levidx_100m=10
         levidx_200m=17
         levidx_2000m=46
         minVerticalLevels=2
         ;;
      "L72")
         n_zlev=72
         dzlev_m="2.0,2.2,2.5,2.8,3.1,3.5,3.9,4.4,4.9,5.4,5.9,6.4,7.1,7.7,8.4,9.2,10.1,11.0,
             12.0,13.2,14.4,15.7,17.1,18.7,20.4,22.3,24.3,26.5,28.9,31.5,34.3,37.3,40.6,
             43.1,45.3,46.8,48.4,50.0,51.7,53.4,55.2,57.0,58.9,60.8,62.9,66.6,72.6,80.6,
             90.6,100.2,110.0,120.3,128.7,137.4,146.4,155.7,165.2,174.8,184.4,194.1,203.6,
             212.9,221.9,230.5,238.5,245.9,252.4,258.1,262.8,266.4,268.9,270.1"
         levidx_100m=18
         levidx_200m=25
         levidx_2000m=55
         minVerticalLevels=10
         ;;
      "L128")
         n_zlev=128
         dzlev_m="11.0,   9.0,     8.0,   8.0,     8.0,    8.0,    8.0,    8.0,    8.0,    8.0,\
            8.0,    8.0,    8.0,    8.25,   8.5,    8.75,   9.0,   9.25,    9.5,   9.75,\
            10.0,   10.0,   10.0,   10.0,   10.0,   10.0,   10.0,   10.0,   10.0,   10.0,\
            10.5,   11.0,   11.5,   12.0,   12.5,   13.0,   13.5,   14.0,   14.5,   15.0,\
            15.5,   16.0,   16.5,   17.0,   17.5,   18.0,   18.5,   19.0,   19.5,   20.0,\
            20.5,   21.0,   21.5,   22.0,   22.5,   23.0,   23.5,   24.0,   24.5,   25.0,\
            25.5,   26.0,   26.5,   27.0,   28.5,   29.0,   29.5,   30.0,   30.5,   31.0,\
            31.0,   32.0,   33.0,   34.0,   35.0,   36.0,   37.0,   38.0,   39.0,   40.0,\
            42.0,   44.0,   46.0,   48.0,   50.0,   52.0,   54.0,   56.0,   58.0,   60.0,\
            62.0,   64.0,   66.0,   68.0,   70.0,   72.0,   74.0,   76.0,   78.0,   80.0,\
            82.0,   84.0,   86.0,   88.0,   90.0,   92.0,   94.0,   96.0,   98.0,  100.0,\
            102.0,  104.0,  106.0,  108.0,  110.0,  112.0,  114.0,  116.0,  118.0,  200.0,\
            200.0,  200.0,  200.0,  200.0,  200.0,  200.0,  200.0,  200.0"
         levidx_100m=14
         levidx_200m=24
         levidx_2000m=92
         minVerticalLevels=2
         ;;
      "L128SMT")
         n_zlev=128
         dzlev_m="2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 3.0, 3.1, 3.2,\
            3.4, 3.5, 3.7, 3.9, 4.0, 4.2, 4.4, 4.6, 4.8, 5.0, 5.3, 5.5,\
            5.8, 6.0, 6.3, 6.6, 6.9, 7.2, 7.5, 7.8, 8.2, 8.5, 8.9, 9.3,\
            9.8, 10.2, 10.7, 11.1, 11.5, 11.9, 12.3, 12.7, 13.1, 13.5,\
            14.0, 14.5, 14.9, 15.4, 15.9, 16.5, 17.0, 17.6, 18.2, 18.8,\
            19.4, 20.0, 20.7, 21.4, 22.1, 22.8, 23.6, 24.4, 25.2, 26.0,\
            26.9, 27.8, 28.7, 29.7, 30.6, 31.7, 32.7, 33.8, 34.9, 36.1,\
            37.3, 38.5, 39.8, 41.1, 42.5, 43.9, 45.3, 46.8, 48.4, 50.0,\
            51.7, 53.4, 55.2, 57.0, 58.9, 60.8, 62.9, 64.9, 67.1, 69.3,\
            71.6, 74.0, 76.5, 79.0, 81.6, 84.3, 87.1, 90.0, 93.0, 96.1,\
            99.3, 102.6, 106.0, 109.5, 113.2, 116.9, 120.8, 124.8, 128.9,\
            133.2, 137.6, 142.2, 146.9, 151.8, 156.9, 162.1, 167.4, 173.0,\
            178.7, 184.7, 190.8, 197.1"
         levidx_100m=27
         levidx_200m=37
         levidx_2000m=96
         minVerticalLevels=12
         ;;
      *)
         echo "ERROR: unsupported ocean_vertical_levels ${ocean_vertical_levels}"
         exit 1
         ;;
   esac

   # FIXME: ocean_vertical_levels falls into none of the following cases because of the prepending "L"
   #        using n_zlev instead => to be checked
   # FIXME: nlev_eu appears unused
   if [[ "${use_hamocc}" == "yes" ]]; then
      lhamocc=".TRUE."
      lbgcadv=".TRUE."
      nlev_eu=${n_zlev}
      # set nlev_eu to level belonging to approx 500m
      case "${n_zlev}" in
         "40") nlev_eu=19 ;;
         "64") nlev_eu=26 ;;
         "20") nlev_eu=16 ;;
      esac
   else
      lhamocc=".FALSE."
      lbgcadv=".FALSE."
   fi
}

# Activate py_run_tools
pushd ../run_utils/py_run_utils 2>&1 >/dev/null || exit

if [ ${FIRST_RUN} == "true" ]; then
    echo " ==> Installing py_run_utils"
    rm -rf .venv uv.lock
    uv venv --relocatable --python="$(which python)"
    source .venv/bin/activate
    uv sync --no-cache --link-mode=copy --compile-bytecode --active --no-editable --inexact || exit
else
    source .venv/bin/activate || exit
fi
popd 2>&1 >/dev/null || exit

