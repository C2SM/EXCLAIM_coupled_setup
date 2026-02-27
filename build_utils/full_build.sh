#!/usr/bin/bash

#SBATCH --account=cwd01
#SBATCH --partition=tier0
#SBATCH --time=02:00:00
#SBATCH --output="full_build.o"

set -e

BUILD_TYPE="${BUILD_TYPE:-SPACK}"
GPU_MODE="${GPU_MODE:-py-substitute}"
CAO_BUILD_DIR="${CAO_BUILD_DIR:-/dev/shm/${USER}/coupled_setup}"
UENV="icon-dsl/25.12:2346172108"

# Get script dir
# --------------
if [ -n "${SLURM_JOB_ID:-}" ]; then
    SCRIPT_PATH=$(scontrol show job "${SLURM_JOB_ID}" | awk -F= '/Command=/{print $2}')
else
    SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
fi
SCRIPT_PATH="$(realpath "${SCRIPT_PATH}")"
SCRIPT_DIR=$(dirname "${SCRIPT_PATH}")
echo "[CAO build] ... Using build scripts from ${SCRIPT_DIR}"
echo "[CAO build] ... Building in ${CAO_BUILD_DIR}"

mkdir -p "${CAO_BUILD_DIR}"
pushd "${CAO_BUILD_DIR}" 2>&1 >/dev/null

# Get ICON
# --------
echo "[CAO build] ... Getting ICON"
CAO_ICON_REPO='git@github.com:C2SM/icon-exclaim.git'
CAO_ICON_BRANCH='icon-dsl'
# CAO_ICON_COMMIT='2902a0412e6092be63bd048a438eaac2fb642d9b'
CAO_ICON_DIR="icon-hybrid-${GPU_MODE}"

if [ -n "${CAO_ICON_COMMIT}" ]; then
    git clone -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
    pushd "${CAO_ICON_DIR}" >/dev/null 2>&1
    git reset --hard "${CAO_ICON_COMMIT}"
    git submodule update --init --depth 1
    popd >/dev/null 2>&1
else
    git clone --depth 1 --recurse-submodules --shallow-submodules -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
fi

# Apply patches
# -------------
echo "[CAO build] ... Applying patches"
pushd "${CAO_ICON_DIR}" >/dev/null 2>&1
git apply ${SCRIPT_DIR}/patches/gmean_acc.patch
pushd externals/jsbach >/dev/null 2>&1
git apply "${SCRIPT_DIR}/patches/mo_hsm_class.f90.patch"
popd >/dev/null 2>&1
popd >/dev/null 2>&1

# Build
# -----
echo "[CAO build] ... Building ICON"
CPU_BUILD_DIR="${CAO_ICON_DIR}/build-cpu"
GPU_BUILD_DIR="${CAO_ICON_DIR}/build-gpu-${GPU_MODE}" 
mkdir -p ${CPU_BUILD_DIR} ${GPU_BUILD_DIR}

echo "[CAO build] ...... Customizing build settings and scripts"
rsync -av "${SCRIPT_DIR}/config_cscs/" "${CAO_ICON_DIR}/config/cscs/"

if [ "${BUILD_TYPE}" ==  "SPACK" ]; then
    
    echo "[CAO build] ...... Building cpu"
    pushd "${CPU_BUILD_DIR}" >/dev/null 2>&1
    uenv run ${UENV} --view default -- ../config/cscs/santis.cpu.nvhpc
    popd >/dev/null 2>&1
    
    echo "[CAO build] ...... Building gpu-${GPU_MODE}"
    pushd "${GPU_BUILD_DIR}" >/dev/null 2>&1
    if [ "${GPU_MODE}" == "acc" ]; then
        uenv run ${UENV} --view default -- ../config/cscs/santis.gpu.nvhpc
    elif [ "${GPU_MODE}" == "py-substitute" ]; then
        uenv run ${UENV} --view default -- ../config/cscs/santis.gpu.nvhpc.py.substitute
    else
        echo "[CAO build] ERROR: unknown GPU_MODE ${GPU_MODE}"
        exit 1
    fi
    popd >/dev/null 2>&1
    
elif [ "${BUILD_TYPE}" ==  "NOSPACK" ]; then

    if [ "${GPU_MODE}" != "acc" ]; then
        echo "[CAO build] ERROR: only 'acc' GPU_MODE is available for NOSPACK build, got ${GPU_MODE}"
        exit 1
    fi
    echo "[CAO build] ...... Building cpu"
    pushd "${CPU_BUILD_DIR}" >/dev/null 2>&1
    uenv run ${UENV} --view default -- ../config/cscs/santis.cpu_nospack.nvhpc && make -j 24
    popd >/dev/null 2>&1
    
    echo "[CAO build] ...... Building gpu-${GPU_MODE}"
    pushd "${GPU_BUILD_DIR}" >/dev/null 2>&1
    uenv run ${UENV} --view default -- ../config/cscs/santis.gpu_nospack.nvhpc && make -j 24
    popd >/dev/null 2>&1

else
    
    echo "[CAO build] ERROR: unknown BUILD_TYPE ${BUILD_TYPE}"
    exit 1
    
fi

popd >/dev/null 2>&1

# Retreive and clean
# ------------------
echo "[CAO build] ... retreiving build from ${CAO_BUILD_DIR}"
rsync -a --delete "${CAO_BUILD_DIR}/${CAO_ICON_DIR}/" "${CAO_ICON_DIR}/"

echo "[CAO build] ... cleaning ${CAO_BUILD_DIR}"
rm -rf "${CAO_BUILD_DIR}/${CAO_ICON_DIR}"

echo "[CAO build] ... build complete"
