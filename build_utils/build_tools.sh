#!/bin/bash


CAO_BASEDIR="$(pwd)"

CAO_ICON_REPO='git@gitlab.dkrz.de:icon/icon-nwp.git'
CAO_ICON_BRANCH='master'

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
  git clone --recurse-submodules -b ${CAO_ICON_BRANCH} ${CAO_ICON_REPO} ${CAO_ICON_DIR}

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
