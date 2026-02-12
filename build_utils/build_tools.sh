#!/bin/bash


CAO_BASEDIR="$(pwd)"

CAO_ICON_REPO='git@gitlab.dkrz.de:icon/icon-nwp.git'
CAO_ICON_BRANCH='master'
CAO_ICON_COMMIT='bb4e1d8dc67545860e365841fde94be77d91e234'

CAO_BUILD_UTILS_DIR="${CAO_BASEDIR}/EXCLAIM_coupled_setup/build_utils/"
CAO_ICON_DIR="${CAO_BASEDIR}/icon-hybrid"

CAO_BUILD_DIRS=(
  'build-cpu'
  'build-gpu'
)

CAO_CONFIG_FILES=(
  'santis.cpu_nospack.nvhpc'
  'santis.gpu_nospack.nvhpc'
)

CAO_SPACK_YAML_FILES=(
  'spack_cpu.yaml'
  'spack_gpu.yaml'
)

CAO_CONFIG_NAMES=()
for config_file in "${CAO_CONFIG_FILES[@]}"; do
  config_name=${config_file#*.}
  config_name=${config_name%%.*}
  CAO_CONFIG_NAMES+=("$config_name")
done
for spack_yaml_file in "${CAO_SPACK_YAML_FILES[@]}"; do
  config_name=${spack_yaml_file#*_}
  config_name=${config_name%%.*}
  CAO_CONFIG_NAMES+=("$config_name")
done


cao_init() {
  if [ -n "${CAO_ICON_COMMIT}" ]; then
    git clone -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
    pushd "${CAO_ICON_DIR}" 2>&1 >/dev/null
    git reset --hard "${CAO_ICON_COMMIT}"
    git submodule update --init --depth 1
    popd 2>&1 >/dev/null
  else
    git clone --depth 1 --recurse-submodules --shallow-submodules -b "${CAO_ICON_BRANCH}" "${CAO_ICON_REPO}" "${CAO_ICON_DIR}"
  fi

  pushd ${CAO_ICON_DIR}/externals/jsbach
  git apply "${CAO_BUILD_UTILS_DIR}/mo_hsm_class.f90.patch"
  popd

  pushd ${CAO_ICON_DIR}
    git apply ${CAO_BUILD_UTILS_DIR}/gmean_acc.patch
  popd

  for config_file in "${CAO_CONFIG_FILES[@]}"; do
    cp ${CAO_BUILD_UTILS_DIR}/${config_file} ${CAO_ICON_DIR}/config/cscs/.
  done

  for spack_yaml_file in "${CAO_SPACK_YAML_FILES[@]}"; do
    config_name=${spack_yaml_file#*_}
    config_name=${config_name%%.*}
    cp ${CAO_BUILD_UTILS_DIR}/${spack_yaml_file} ${CAO_ICON_DIR}/config/cscs/spack/santis_${config_name}_double/spack.yaml
  done

  pushd ${CAO_ICON_DIR}
    for build_dir in "${CAO_BUILD_DIRS[@]}"; do
      mkdir -p ${build_dir}
    done
  popd
}

cao_build() {
  if [[ "$(pwd)" == "${CAO_ICON_DIR}" ]]; then
    ./config/cscs/santis.${1}.nvhpc
    CONFIG_STATUS=$?
  else
    ../config/cscs/santis.${1}.nvhpc
    CONFIG_STATUS=$?
  fi

  if [[ $CONFIG_STATUS == 0 && ${1} == *"nospack"* ]]; then
    make -j 24
  fi
}
complete -W "${CAO_CONFIG_NAMES[*]}" cao_build


alias cao_rebuild='make -j 24'
