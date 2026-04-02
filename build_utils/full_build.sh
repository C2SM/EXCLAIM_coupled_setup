#!/usr/bin/bash

#SBATCH --account=cwd01
#SBATCH --time=01:00:00
#SBATCH --output="full_build.%j.o"
#SBATCH --partition="shared"
#SBATCH --gpus-per-node=1

set -e

elapsed(){
    local seconds=$(($2 - $1))
    printf '%02d:%02d:%02d\n' $((seconds/3600)) $((seconds%3600/60)) $((seconds%60))
}


BUILD_TYPE="${BUILD_TYPE:-SPACK}"
GPU_MODE="${GPU_MODE:-py-substitute}"
CAO_BUILD_DIR="${CAO_BUILD_DIR:-/dev/shm/${USER}/coupled_setup}"
UENV=${UENV:-"icon-dsl/25.12:2410652750"}

# Set cloning urls with token
# ---------------------------
if [ -z "${GITLAB_DKRZ_TOKEN}" ] || [ -z "${GITHUB_TOKEN}" ]; then
    echo "ERROR: GITLAB_DKRZ_TOKEN and/or GITHUB_TOKEN unset"
    exit 1
fi
GIT_CONFIG_COUNT=2
GIT_CONFIG_KEY_0="url.https://oauth2:${GITLAB_DKRZ_TOKEN}@gitlab.dkrz.de/.insteadOf"
GIT_CONFIG_VALUE_0="git@gitlab.dkrz.de:"
GIT_CONFIG_KEY_1="url.https://oauth2:${GITHUB_TOKEN}@github.com/.insteadOf"
GIT_CONFIG_VALUE_1="git@github.com:"

# Set targets
# -----------
build_cpu="true"
build_gpu="true"
OPTIONS="$@"
if [ -n "${OPTIONS}" ]; then
    case "${OPTIONS}" in
        "--cpu-only") build_gpu="false" ;;
        "--gpu-only") build_cpu="false" ;;
        *) echo "ERROR: unrecognized argument ${OPTIONS}"; exit 1;;
    esac
fi
          

# Get script dir
# --------------
if [ -n "${SLURM_JOB_ID:-}" ]; then
    ON_COMPUTE_NODE="true"
    SCRIPT_PATH=$(scontrol show job "${SLURM_JOB_ID}" | awk -F= '/Command=/{print $2}')
else
    ON_COMPUTE_NODE="false"
    SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
fi
SCRIPT_PATH="$(realpath "${SCRIPT_PATH}")"
SCRIPT_DIR=$(dirname "${SCRIPT_PATH}")
echo "[CAO build] ... Using build scripts from ${SCRIPT_DIR}"
echo "[CAO build] ... Building in ${CAO_BUILD_DIR}"

rm -rf "${CAO_BUILD_DIR}"
mkdir -p "${CAO_BUILD_DIR}"
pushd "${CAO_BUILD_DIR}" 2>&1 >/dev/null

# Get ICON
# --------
start=$(date +%s)
echo "[CAO build] ... Getting ICON"

CAO_ICON_REPO='git@github.com:C2SM/icon-exclaim.git'
# CAO_ICON_BRANCH='icon-dsl'
CAO_ICON_BRANCH='cuda_mempool'
ALT_CPU="true"
CAO_ICON_BRANCH_ALT_CPU='icon-dsl'
# CAO_ICON_COMMIT='5c5b742a969af2bd491e26cd0a05a35838f121c4'
CAO_ICON_DIRNAME="icon-hybrid-${GPU_MODE}"

if [ -n "${CAO_ICON_COMMIT}" ]; then
    git clone -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIRNAME}"
    pushd "${CAO_ICON_DIRNAME}" >/dev/null 2>&1
    git reset --hard "${CAO_ICON_COMMIT}"
    git submodule update --init --depth 1
    popd >/dev/null 2>&1
else
    git clone --depth 1 --recurse-submodules --shallow-submodules -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIRNAME}"
fi

if [ "${ALT_CPU}" == "true" ]; then  # Hugly hack for cpu build
    CAO_ICON_DIRNAME_ALT_CPU="icon-hybrid-alt-cpu"
    git clone --depth 1 --recurse-submodules --shallow-submodules -b "${CAO_ICON_BRANCH_ALT_CPU}" "${CAO_ICON_REPO}" "${CAO_ICON_DIRNAME_ALT_CPU}"
fi
stop=$(date +%s)
echo "[CAO build] ... Getting ICON => done in $(elapsed $start $stop)"

# Apply patches
# -------------
# echo "[CAO build] ... Applying patches"
# pushd "${CAO_ICON_DIRNAME}" >/dev/null 2>&1
# git apply ${SCRIPT_DIR}/patches/gmean_acc.patch
# # - ML - Not sur this is still a good idea after the upgrade
# # pushd externals/jsbach >/dev/null 2>&1
# # git apply "${SCRIPT_DIR}/patches/mo_hsm_class.f90.patch"
# # popd >/dev/null 2>&1
# popd >/dev/null 2>&1

# Build
# -----
start=$(date +%s)
echo "[CAO build] ... Building ICON"

CPU_BUILD_DIR="${CAO_ICON_DIRNAME}/build-cpu"
GPU_BUILD_DIR="${CAO_ICON_DIRNAME}/build-gpu-${GPU_MODE}" 
mkdir -p ${CPU_BUILD_DIR} ${GPU_BUILD_DIR}

echo "[CAO build] ...... Customizing build settings and scripts"
rsync -av "${SCRIPT_DIR}/config_cscs/" "${CAO_ICON_DIRNAME}/config/cscs/"
if [ "${ALT_CPU}" == "true" ]; then  # Hugly hack for cpu build
    rsync -av "${SCRIPT_DIR}/config_cscs/" "${CAO_ICON_DIRNAME_ALT_CPU}/config/cscs/"
fi

if [ "${BUILD_TYPE}" ==  "SPACK" ]; then

    if [ "${build_cpu}" == "true" ]; then
        echo "[CAO build] ...... Building cpu"
        start_cpu=$(date +%s)
        pushd "${CPU_BUILD_DIR}" >/dev/null 2>&1
        if [ "${ALT_CPU}" == "true" ]; then  # Hugly hack for cpu build
            uenv run ${UENV} --view default -- time ../../${CAO_ICON_DIRNAME_ALT_CPU}/config/cscs/santis.cpu.nvhpc
        else
            uenv run ${UENV} --view default -- time ../config/cscs/santis.cpu.nvhpc
        fi
        popd >/dev/null 2>&1
        stop_cpu=$(date +%s)
        echo "[CAO build] ...... Building cpu => done in $(elapsed $start_cpu $stop_cpu)"
    fi

    if [ "${build_gpu}" == "true" ]; then
        echo "[CAO build] ...... Building gpu-${GPU_MODE}"
        start_gpu=$(date +%s)
        pushd "${GPU_BUILD_DIR}" >/dev/null 2>&1
        if [ "${GPU_MODE}" == "acc" ]; then
            uenv run ${UENV} --view default -- time ../config/cscs/santis.gpu.nvhpc
        elif [ "${GPU_MODE}" == "py-substitute" ]; then
            uenv run ${UENV} --view default -- time ../config/cscs/santis.gpu.nvhpc.py.substitute
        else
            echo "[CAO build] ERROR: unknown GPU_MODE ${GPU_MODE}"
            exit 1
        fi
        popd >/dev/null 2>&1
        stop_gpu=$(date +%s)
        echo "[CAO build] ...... Building gpu-${GPU_MODE} => done in $(elapsed $start_gpu $stop_gpu)"
    fi
    
elif [ "${BUILD_TYPE}" ==  "NOSPACK" ]; then

    if [ "${build_cpu}" == "true" ]; then
        echo "[CAO build] ...... Building cpu"
        start_cpu=$(date +%s)
        pushd "${CPU_BUILD_DIR}" >/dev/null 2>&1
        uenv run ${UENV} --view default -- time ../config/cscs/santis.cpu_nospack.nvhpc && make -j 24
        popd >/dev/null 2>&1
        stop_cpu=$(date +%s)
        echo "[CAO build] ...... Building cpu => done in $(elapsed $start_cpu $stop_cpu)"
    fi

    if [ "${build_gpu}" == "true" ]; then
        if [ "${GPU_MODE}" != "acc" ]; then
            echo "[CAO build] ERROR: only 'acc' GPU_MODE is available for NOSPACK build, got ${GPU_MODE}"
            exit 1
        fi
        echo "[CAO build] ...... Building gpu-${GPU_MODE}"
        start_gpu=$(date +%s)
        pushd "${GPU_BUILD_DIR}" >/dev/null 2>&1
        uenv run ${UENV} --view default -- time ../config/cscs/santis.gpu_nospack.nvhpc && make -j 24
        popd >/dev/null 2>&1
        stop_gpu=$(date +%s)
        echo "[CAO build] ...... Building gpu-${GPU_MODE} => done in $(elapsed $start_gpu $stop_gpu)"
    fi

else
    
    echo "[CAO build] ERROR: unknown BUILD_TYPE ${BUILD_TYPE}"
    exit 1
    
fi

stop=$(date +%s)
echo "[CAO build] ... Building ICON => done in $(elapsed $start $stop)"

# Retreive and clean
# ------------------
start=$(date +%s)
echo "[CAO build] ... retreiving build from ${CAO_BUILD_DIR}"

popd >/dev/null 2>&1
time rsync -a --delete "${CAO_BUILD_DIR}/${CAO_ICON_DIRNAME}/" "${CAO_ICON_DIRNAME}/"

stop=$(date +%s)
echo "[CAO build] ... retreiving => done in $(elapsed $start $stop)"

echo "[CAO build] ... cleaning ${CAO_BUILD_DIR}"
rm -rf "${CAO_BUILD_DIR}/${CAO_ICON_DIRNAME}"

echo "[CAO build] ... build complete"

if [ "${ON_COMPUTE_NODE}" == "true" ]; then
    sacct -j "${SLURM_JOB_ID}" --format "JobID, JobName, AllocCPUs, Elapsed, ElapsedRaw, CPUTimeRAW, ConsumedEnergyRaw, MaxRSS, MaxVMSize, AveRSS"
fi
