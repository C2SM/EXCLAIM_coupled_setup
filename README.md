# EXCLAIM Coupled Setup Utilities

Collection of utility scripts to build and run the EXCLAIM coupled atmosphere-ocean setup.

## Build

### Step by step

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

# Build CPU and GPU with Spack
pushd icon-hybrid/build-cpu && cao_build cpu && popd
pushd icon-hybrid/build-gpu && cao_build gpu && popd
```

Otherwise, if you want to build without Spack, but using our custom build scripts you can run the following instead:

```bash
# Build CPU and GPU without Spack
pushd icon-hybrid/build-cpu && cao_build cpu_nospack && popd
pushd icon-hybrid/build-gpu && cao_build gpu_nospack && popd
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

### Full build script

To directly do a clean build of the cpu and gpu executables, just execute

``` bash
BUILD_TYPE="SPACK" sbatch ./build_utils/full_build.sh 
```
from the repository root. You can also use `BUILD_TYPE="NOSPACK"`.

You can monitor the build with `tail -f full_build.o`. The build happens on `/dev/shm` on the login nodes. The icon clone will be retrieved at the end of the build as `icon-hybrid` containing the 2 `build-cpu` and `build-gpu` subdirectories. 

## Run

To run the coupled setup follow these steps:

```bash
# From the base directory navigate to the EXCLAIM_coupled_setup/experiments directory
cd EXCLAIM_coupled_setup/experiments

# Select the case you want to run: 'R02B07-R02B07' (default), 'R02B08-R02B09', 'R02B10-R02B10'
export CASE='R02B07-R02B07'

# Run the experiment for one of the three possible targets: 'hybrid', 'cpu', 'cpu-cpu'
jid=$(sbatch --parsable exp.EXCLAIM_COUPLED.run hybrid)

# Inspect the logfile at the end
less LOG.EXCLAIM_COUPLED.${jid}
```

### Run Options

We provide the following running options:

- `--profile`: Run the Nsight Systems profiler and generate a profile file that can be inspected with *nsys-ui*.
- `--separate-io`: Distribute the IO tasks of both ocean and atmosphere components on nodes that do not have compute tasks.

You can add these options at the end of the `sbatch` command in any order. All of them are turned off by default if not provided.

```bash
jid=$(sbatch --parsable exp.EXCLAIM_COUPLED.run hybrid --profile)
```
