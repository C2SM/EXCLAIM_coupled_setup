# EXCLAIM Coupled Setup Utilities

Collection of utility scripts to build and run the EXCLAIM coupled atmosphere-ocean setup.

## Build

To build the coupled setup follow these steps:

```bash
# Start UENV
uenv start --view=default icon/25.2:v3

# Create a base directory
mkdir -p coupled-setup

# Clone this repository in the base directory
cd coupled-setup && git clone git@github.com:C2SM/EXCLAIM_coupled_setup.git

# Copy the build_tools into the base directory
cp EXCLAIM_coupled_setup/build_utils/build_tools.sh .

# Source the build_tools and initialize
source build_tools.sh
cao_init

# Build CPU and GPU
pushd icon-hybrid/build-cpu && cao_build cpu_ocean && popd
pushd icon-hybrid/build-gpu && cao_build gpu_coupled && popd
```

If you only want to rebuild (e.g. after you made some changes in the code), enter the build directories and run
`cao_rebuild`.

```bash
# Make sure your UENV is active first
uenv status

# Rebuild CPU and GPU executables
cd build-cpu && cao_rebuild && cd ..
cd build-gpu && cao_rebuild && cd ..
```

> Note that to have the build utilities (e.g. `cao_init`, `cao_build`, `cao_rebuild`, ...) available you should source
*build_tools.sh* in all new shells.

## Run

To run the coupled setup follow these steps:

```bash
# From the base directory navigate to the R02B07 directory
cd EXCLAIM_coupled_setup/R02B07

# install python run tools
./install_py_run_utils.sh

# Run the experiment for one of the three possible targets: 'hybrid', 'cpu', 'cpu-cpu'
jid=$(sbatch --uenv="icon/25.2:v3,$PWD/venv.squashfs:$PWD/.venv" --parsable exp.EXCLAIM_COUPLED_R02B07.run hybrid)

# Inspect the logfile at the end
less LOG.EXCLAIM_COUPLED_R02B07.${jid}
```
