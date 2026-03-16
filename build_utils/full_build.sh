#!/usr/bin/bash

#SBATCH --account=cwd01
#SBATCH --partition=tier0
#SBATCH --time=01:00:00
#SBATCH --output="full_build.%j.o"
#SBATCH --partition="shared"
#SBATCH --gpus-per-node=1

set -e

BUILD_TYPE="${BUILD_TYPE:-SPACK}"
GPU_MODE="${GPU_MODE:-py-substitute}"
CAO_BUILD_DIR="${CAO_BUILD_DIR:-/dev/shm/${USER}/coupled_setup}"
# UENV=${UENV:-"icon-dsl/25.12:v1"}
UENV=${UENV:-"icon-dsl/25.12:v2"}

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
CAO_ICON_COMMIT='5c5b742a969af2bd491e26cd0a05a35838f121c4'
CAO_ICON_DIR="icon-hybrid-${GPU_MODE}"

CAO_ICON4PY_REPO='git@github.com:C2SM/icon4py.git'
CAO_ICON4PY_BRANCH='main'
CAO_ICON4PY_COMMIT='9a7f7d68f0e8be18f746044879c49e4d87e20ff6'
CAO_ICON4PY_DIR="icon4py"

rm -rf "${CAO_ICON_DIR}" "${CAO_ICON4PY_DIR}"

echo "[CAO build] ...... Getting icon-exclaim"
if [ -n "${CAO_ICON_COMMIT}" ]; then
    git clone -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
    pushd "${CAO_ICON_DIR}" >/dev/null 2>&1
    git reset --hard "${CAO_ICON_COMMIT}"
    git submodule update --init --depth 1
    popd >/dev/null 2>&1
else
    git clone --depth 1 --recurse-submodules --shallow-submodules -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
fi

echo "[CAO build] ...... Getting icon4py"
git clone -b "${CAO_ICON4PY_BRANCH}" "${CAO_ICON4PY_REPO}" "${CAO_ICON4PY_DIR}"
pushd "${CAO_ICON4PY_DIR}" >/dev/null 2>&1
git reset --hard "${CAO_ICON4PY_COMMIT}"
popd >/dev/null 2>&1

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

    if [ "${build_cpu}" == "true" ]; then
        echo "[CAO build] ...... Building cpu"
        pushd "${CPU_BUILD_DIR}" >/dev/null 2>&1
        uenv run ${UENV} --view default -- ../config/cscs/santis.cpu.nvhpc
        popd >/dev/null 2>&1
    fi

    if [ "${build_gpu}" == "true" ]; then
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
    fi
    
elif [ "${BUILD_TYPE}" ==  "NOSPACK" ]; then

    if [ "${build_cpu}" == "true" ]; then
        echo "[CAO build] ...... Building cpu"
        pushd "${CPU_BUILD_DIR}" >/dev/null 2>&1
        uenv run ${UENV} --view default -- ../config/cscs/santis.cpu_nospack.nvhpc && make -j 24
        popd >/dev/null 2>&1
    fi

    if [ "${build_gpu}" == "true" ]; then
        if [ "${GPU_MODE}" != "acc" ]; then
            echo "[CAO build] ERROR: only 'acc' GPU_MODE is available for NOSPACK build, got ${GPU_MODE}"
            exit 1
        fi
        echo "[CAO build] ...... Building gpu-${GPU_MODE}"
        pushd "${GPU_BUILD_DIR}" >/dev/null 2>&1
        uenv run ${UENV} --view default -- ../config/cscs/santis.gpu_nospack.nvhpc && make -j 24
        popd >/dev/null 2>&1
    fi

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
