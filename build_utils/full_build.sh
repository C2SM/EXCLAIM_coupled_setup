#!/usr/bin/bash

#SBATCH --account=cwd01
#SBATCH --partition=tier0
#SBATCH --time=02:00:00
#SBATCH --uenv=icon/25.2:v3
#SBATCH --view=default
#SBATCH --output="full_build.o"

set -e

BUILD_TYPE="${BUILD_TYPE:-SPACK}"
CAO_BUILD_DIR="${CAO_BUILD_DIR:-/dev/shm/${USER}/coupled_setup}"

# Get script dir
if [ -n "${SLURM_JOB_ID:-}" ]; then
    SCRIPT_PATH=$(scontrol show job "${SLURM_JOB_ID}" | awk -F= '/Command=/{print $2}')
else
    SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
fi
SCRIPT_PATH="$(realpath "${SCRIPT_PATH}")"
SCRIPT_DIR=$(dirname "${SCRIPT_PATH}")
echo "[CAO build] Using build scripts from ${SCRIPT_DIR}"
echo "[CAO build] Building in ${CAO_BUILD_DIR}"

mkdir -p "${CAO_BUILD_DIR}"
pushd "${CAO_BUILD_DIR}" 2>&1 >/dev/null

# Get ICON
echo "[CAO build] Getting ICON"
CAO_ICON_REPO='git@gitlab.dkrz.de:icon/icon-nwp.git'
CAO_ICON_BRANCH='master'
CAO_ICON_COMMIT='bb4e1d8dc67545860e365841fde94be77d91e234'
CAO_ICON_DIR="icon-hybrid"

if [ -n "${CAO_ICON_COMMIT}" ]; then
    git clone -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
    pushd "${CAO_ICON_DIR}" 2>&1 >/dev/null
    git reset --hard "${CAO_ICON_COMMIT}"
    git submodule update --init --depth 1
    pushd externals/jsbach 2>&1 >/dev/null
    git apply "${SCRIPT_DIR}/mo_hsm_class.f90.patch"
    popd 2>&1 >/dev/null
    popd 2>&1 >/dev/null
else
    git clone --depth 1 --recurse-submodules --shallow-submodules -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
fi

# Build
echo "[CAO build] Building ICON"
mkdir -p ${CAO_ICON_DIR}/build-cpu ${CAO_ICON_DIR}/build-gpu
if [ "${BUILD_TYPE}" ==  "NOSPACK" ]; then
    echo "[CAO build]    Building cpu"
    cp "${SCRIPT_DIR}/santis.cpu_nospack.nvhpc" "${CAO_ICON_DIR}/config/cscs/."
    pushd "${CAO_ICON_DIR}/build-cpu" 2>&1 >/dev/null
    ../config/cscs/santis.cpu_nospack.nvhpc && make -j 24
    popd 2>&1 >/dev/null
    echo "[CAO build]    Building gpu"
    cp "${SCRIPT_DIR}/santis.gpu_nospack.nvhpc" "${CAO_ICON_DIR}/config/cscs/."
    pushd "${CAO_ICON_DIR}/build-gpu" 2>&1 >/dev/null
    ../config/cscs/santis.gpu_nospack.nvhpc && make -j 24
    popd 2>&1 >/dev/null
elif [ "${BUILD_TYPE}" ==  "SPACK" ]; then
    echo "[CAO build]    Building cpu"
    cp "${SCRIPT_DIR}/spack_cpu.yaml" "${CAO_ICON_DIR}/config/cscs/spack/santis_cpu_double/spack.yaml"
    pushd "${CAO_ICON_DIR}/build-cpu" 2>&1 >/dev/null
    ../config/cscs/santis.cpu.nvhpc
    popd 2>&1 >/dev/null
    echo "[CAO build]    Building gpu"
    cp "${SCRIPT_DIR}/spack_gpu.yaml" "${CAO_ICON_DIR}/config/cscs/spack/santis_gpu_double/spack.yaml"
    pushd "${CAO_ICON_DIR}/build-gpu" 2>&1 >/dev/null
    ../config/cscs/santis.gpu.nvhpc
    popd 2>&1 >/dev/null
else
    echo "ERROR: unknown BUILD_TYPE ${BUILD_TYPE}"
    exit 1
fi

popd 2>&1 >/dev/null

echo "[CAO build] retreiving build from ${CAO_BUILD_DIR}"
rsync -a "${CAO_BUILD_DIR}/${CAO_ICON_DIR}" .

echo "[CAO build] cleaning ${CAO_BUILD_DIR}"
rm -rf "${CAO_BUILD_DIR}/${CAO_ICON_DIR}"

echo "[CAO build] build successful"
