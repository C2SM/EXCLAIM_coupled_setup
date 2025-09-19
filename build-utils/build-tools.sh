#!/bin/bash


CAO_BASEDIR="$(pwd)"

CAO_ICONXPP_BRANCH='icon-XPP-20250319'

CAO_BUILD_UTILS_DIR="${CAO_BASEDIR}/EXCLAIM_coupled_setup/build-utils/"
CAO_ICON_DIR="${CAO_BASEDIR}/icon-hybrid"

CAO_BUILD_DIRS=(
  'build-cpu'
  'build-gpu'
)

CAO_CONFIG_FILES=(
  'santis.cpu_ocean.nvidia'
  'santis.gpu_coupled.nvidia'
)

CAO_CONFIG_NAMES=()
for config_file in "${CAO_CONFIG_FILES[@]}"; do
  config_name=${config_file#*.}
  config_name=${config_name%%.*}
  CAO_CONFIG_NAMES+=("$config_name")
done


cao_init() {
  git clone --recurse-submodules -b ${CAO_ICONXPP_BRANCH} git@gitlab.dkrz.de:icon/icon-mpim.git ${CAO_ICON_DIR}
  
  for config_file in "${CAO_CONFIG_FILES[@]}"; do
    cp ${CAO_BUILD_UTILS_DIR}/${config_file} ${CAO_ICON_DIR}/config/cscs/.
    sed -i -e 's|ICON_DIR=$(cd "${SCRIPT_DIR}/../../"; pwd)|ICON_DIR=$(cd "${SCRIPT_DIR}/../../../"; pwd)|' ${CAO_ICON_DIR}/config/cscs/${config_file}
  done
  
  pushd ${CAO_ICON_DIR}
    git apply ${CAO_BUILD_UTILS_DIR}/icon-hybrid.patch
  
    for build_dir in "${CAO_BUILD_DIRS[@]}"; do
      mkdir -p ${build_dir} && cp -r config/ ${build_dir}
    done
  popd
}

cao_build() {
  if [[ "$(pwd)" == *"build"* ]]; then
    ./config/cscs/santis.${1}.nvidia && make -j 24
  else
    echo "ERROR: You should run this within a build folder!"
  fi
}
complete -W "${CAO_CONFIG_NAMES[*]}" cao_build


alias cao_rebuild='make -j 24'