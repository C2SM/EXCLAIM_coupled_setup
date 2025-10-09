#!/usr/bin/bash

compute_tasks_distribution(){

   ((NTASKS_PER_NODE = N_OCE_TASKS_PER_NODE + N_ATM_TASKS_PER_NODE))
   ((NTASKS = SLURM_JOB_NUM_NODES * NTASKS_PER_NODE))

   ATM_MIN_RANK=0
   #((ATM_MAX_RANK = N_ATM_TASKS_PER_NODE * SLURM_JOB_NUM_NODES - 1))
   #((OCE_MIN_RANK = ATM_MAX_RANK + 1))
   #((OCE_MAX_RANK = NTASKS - 1))

   # Write multi-prog distribution to file
   if [[ "${TARGET}" == "hybrid" ]]; then
#      cat > multi-prog.conf << EOF
#${ATM_MIN_RANK}-${ATM_MAX_RANK} ../run_utils/hybrid_wrapper.sh ${1-} ./icon_gpu
#${OCE_MIN_RANK}-${OCE_MAX_RANK} ../run_utils/hybrid_wrapper.sh ${1-} ./icon_cpu
#EOF
#      chmod 755 multi-prog.conf
############ to replace run_wrapper #####################

rm -f run_atmo_gpu.sh

cat > run_atmo_gpu.sh << EOF
#! /usr/bin/bash

lrank=\$SLURM_LOCALID%4

numanode=(0 1 2 3)
gpus=(0 1 2 3)
nics=(mlx5_0:1 mlx5_1:1 mlx5_2:1 mlx5_3:1)
cpu_cores=(0-3 72-75 144-147 216-220)
reorder=(0 1 2 3)

nic_reorder=(\${nics[\${reorder[0]}]}
             \${nics[\${reorder[1]}]}
             \${nics[\${reorder[2]}]}
             \${nics[\${reorder[3]}]})
numanode_reorder=(\${numanode[\${reorder[0]}]}
                  \${numanode[\${reorder[1]}]}
                  \${numanode[\${reorder[2]}]}
                  \${numanode[\${reorder[3]}]})

export UCX_NET_DEVICES=\${nic_reorder[lrank]}

#echo Atmo compute process \$SLURM_LOCALID on \$(hostname)
export CUDA_VISIBLE_DEVICES=\${gpus[\${reorder[lrank]}]}

#numactl --cpunodebind=\${numanode_reorder[\$lrank]} --membind=\${numanode_reorder[\$lrank]} numactl -s
#echo numactl --physcpubind=\${cpu_cores[\$lrank]} --membind=\${numanode_reorder[\$lrank]} ${ICONg}
#numactl --physcpubind=\${cpu_cores[\$lrank]} --membind=\${numanode_reorder[\$lrank]} ${ICONg}
numactl --cpunodebind=\${numanode_reorder[\$lrank]} --membind=\${numanode_reorder[\$lrank]} ${icon_gpu}
#${ICONg}

EOF

chmod 755 ./run_atmo_gpu.sh

################# Oce wrapper #########################

rm -f run_oce_cpu.sh

cat > run_oce_cpu.sh << EOF
#! /usr/bin/bash


export OMP_NUM_THREADS=1
export ICON_THREADS=1

#export OMP_WAIT_POLICY=active

#export OMP_PROC_BIND=close 
#export OMP_PLACES=cores

#export MPICH_OFI_NIC_POLICY="NUMA"

# The first 4 ranks are gpus so they do not count here
lrank=\$((SLURM_LOCALID - SLURM_GPUS_ON_NODE))

#echo Oce compute process \$SLURM_LOCALID on \$(hostname)

nOceranks=$N_OCE_TASKS_PER_NODE
nSockets=\$SLURM_GPUS_ON_NODE
nOcePerSocket=\$(( nOceranks / nSockets ))
nCoresPerSocket=72
noOceCoresPerSocket=\$(( nCoresPerSocket - nOcePerSocket*\${OMP_NUM_THREADS} ))
nOceRanksPerSocket=\$(( nCoresPerSocket - noOceCoresPerSocket ))

if [[ \$lrank -lt \$nOcePerSocket ]]; then
  core=\$(( \$noOceCoresPerSocket + \$lrank*\${OMP_NUM_THREADS} ))
  nic=('mlx5_0:1')
elif [[ \$lrank -lt \$(( \$nOcePerSocket * 2 )) ]]; then
  core=\$(( \$nCoresPerSocket + \$noOceCoresPerSocket + \$lrank*\${OMP_NUM_THREADS} - \$nOceRanksPerSocket ))
  nic=('mlx5_1:1')
elif [[ \$lrank -lt \$(( \$nOcePerSocket * 3 )) ]]; then
  core=\$(( \$nCoresPerSocket*2 + \$noOceCoresPerSocket + \$lrank*\${OMP_NUM_THREADS} - 2*\$nOceRanksPerSocket ))
  nic=('mlx5_2:1')
else
  core=\$(( \$nCoresPerSocket*3 + \$noOceCoresPerSocket + \$lrank*\${OMP_NUM_THREADS} - 3*\$nOceRanksPerSocket ))
  nic=('mlx5_3:1')
fi

numanode=(\$((\$core/72)))
#export UCX_NET_DEVICES=\${nic}

#export CUDA_VISIBLE_DEVICES=""
export CUDA_VISIBLE_DEVICES=\$numanode
#echo $CUDA_VISIBLE_DEVICES
#echo numactl --physcpubind=\${core}-\$(( core+3 )) --membind=\${numanode} ${ICONc}
#numactl --physcpubind=\${core}-\$(( core+3 )) --membind=\${numanode} ${ICONc}
numactl --cpunodebind=\${numanode} --membind=\${numanode} ${icon_cpu}
EOF

chmod 755 ./run_oce_cpu.sh


################## Hostfile ###############################

export SLURM_HOSTFILE="hostfile.$SLURM_JOB_ID"

/usr/bin/python3 -c "
import os
import re

def expandNG(nodestring):
    substrings = ['-','[',']',',']
    token = []
    pos = 0
    while pos >= 0:
        pat = re.compile('|'.join([re.escape(s) for s in substrings]))
        match = pat.search(nodestring)
        if match is None:
            pos = -1
            token.append(nodestring)
        else:
            pos = match.start()
            token.append(nodestring[0:pos])
            token.append(nodestring[pos])
        nodestring = nodestring[pos+1:]

    return parse(token)

def parse(token, inSquareBrackets=False):
    #print(token)
    if len(token) == 0:
        return []
    elif len(token) == 1:
        # Discard empty strings
        if len(token[0]) == 0:
            return []
        return token
    elif token[1] == ',':
        first = parse([token[0]])
        second = parse(token[2:], inSquareBrackets)
        return [*first, *second]
    elif token[1] == '[':
        end = token.index(']')
        res = parse(token[2:end], True)
        ret = []
        for r in res:
            ret.append(token[0]+r)
        second = parse(token[end+1:])
        return [*ret, *second]
    elif token[1] == '-':
        if inSquareBrackets:
            start = int(token[0])
            end = int(token[2])
            ret = []
            for i in range(start,end+1):
                ret.append(str(i).zfill(len(token[0])))
            second = parse(token[4:], inSquareBrackets)
            return [ *ret, *second ]
        else:
            # This might need additional parser passes
            first = [ token[0] + '-' + token[2] ]
            newtoken = [ *first, *(token[3:]) ]
            return parse(newtoken, inSquareBrackets)
    else:
        print(token, 'error')

SLURM_NODELIST          = os.environ['SLURM_NODELIST']
SLURM_GPUS_ON_NODE      = int(os.environ['SLURM_GPUS_ON_NODE'])
NCPU_RANKS_PER_NODE     = int(os.environ['NCPU_RANKS_PER_NODE'])

ns = expandNG(SLURM_NODELIST)

# Atmo, 4 GPUs per Node, all nodes filled
for host in ns:
    for i in range(SLURM_GPUS_ON_NODE):
        print(host)

# This time we're gonna put all out-ranks on one node, the last one.
host = ns[-1]
for i in range($ATM_IO_TASKS):
    print(host)


# OCE
for host in ns[1:-1]:
    for i in range(NCPU_RANKS_PER_NODE):
        print(host)

# This time we're gonna put all out-ranks on one node, the last one.
host = ns[-1]
for i in range($OCE_IO_TASKS):
    print(host)

" > $SLURM_HOSTFILE

##################### MPMD ################################

cat > multi-prog.conf << EOF
0-$((no_of_nodes * N_ATM_TASKS_PER_NODE -1)) ./run_atmo_gpu.sh
* ./run_oce_cpu.sh
EOF

   elif [[ "${TARGET}" == "cpu-cpu" ]]; then
      cat > multi-prog.conf << EOF
${ATM_MIN_RANK}-${ATM_MAX_RANK} ./icon_cpu
${OCE_MIN_RANK}-${OCE_MAX_RANK} ./icon_cpu
EOF
      chmod 755 multi-prog.conf
   fi
}

set_environment(){
   # TODO: Make wrapper target-specific and move environment settings there

   ulimit -s unlimited
   ulimit -c 0

   # Dump SLURM environment variables
   # --------------------------------
   set | grep SLURM

   # Libfabric / Slingshot
   # ---------------------
   if [[ "${TARGET}" == "hybrid" ]]; then
      export FI_CXI_SAFE_DEVMEM_COPY_THRESHOLD=0
      export FI_CXI_RX_MATCH_MODE=software
      export FI_MR_CACHE_MONITOR=disabled
   fi
   export FI_MR_CACHE_MAX_COUNT=0
   export FI_CXI_OFLOW_BUF_COUNT=10

   # MPICH
   # -----
   if [[ "${TARGET}" == "hybrid" ]]; then
      export MPICH_GPU_SUPPORT_ENABLED=1
      # export MPICH_GPU_IPC_ENABLED=0
      export MPICH_RDMA_ENABLED_CUDA=1
      export MPICH_OFI_NIC_POLICY=GPU
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
   export OMP_NUM_THREADS=1
   export ICON_THREADS=1
   export OMP_SCHEDULE=dynamic,1
   export OMP_DYNAMIC="false"
   export OMP_STACKSIZE=200M

   # From MPIM
   # ---------
   # NOTE: All MKL*, OMPI*, UCX* KMP* variables from MPIM setup
   #       are irrelevant on Santis.
   # TODO: check if MALLOC_TRIM_THRESHOLD_="-1" is necessary
}

run_model(){
   case "${TARGET}" in
      "hybrid")
         srun \
            -l \
            --export=ALL \
            --kill-on-bad-exit=1 \
            --distribution="arbitrary" \
            --hint="nomultithread" \
            --ntasks=${TOT_TASKS} \
            --ntasks-per-node=$((MPI_PROCS_PER_NODE)) \
            --cpus-per-task=2 \
            --threads-per-core=2 \
            --cpu-bind=v,none \
            --overcommit \
            --multi-prog multi-prog.conf
      ;;
      "cpu-cpu")
         srun \
            -l \
            --kill-on-bad-exit=1 \
            --nodes="${SLURM_JOB_NUM_NODES:-1}" \
            --distribution="block:cyclic" \
            --hint="nomultithread" \
            --ntasks="${NTASKS}" \
            --ntasks-per-node="${NTASKS_PER_NODE}" \
            --cpus-per-task="${OMP_NUM_THREADS}" \
            --multi-prog multi-prog.conf
      ;;
      "cpu")
         srun \
            -l \
            --kill-on-bad-exit=1 \
            --nodes="${SLURM_JOB_NUM_NODES:-1}" \
            --distribution="block:cyclic" \
            --hint="nomultithread" \
            --ntasks="${NTASKS}" \
            --ntasks-per-node="${NTASKS_PER_NODE}" \
            --cpus-per-task="${OMP_NUM_THREADS}" \
            "./icon_cpu"
      ;;
   esac
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
