# EXCLAIM Coupled Setup Utilities

Collection of utility script to build and run the EXCLAIM coupled atmosphere-ocean setup.

## Build

To build the coupled setup follow these steps:

```bash
# Create a base directory
mkdir -p coupled-setup

# Clone this repository in the base directory
cd coupled-setup && git clone git@github.com:C2SM/EXCLAIM_coupled_setup.git

# Copy the build-tools into the base directory
cp EXCLAIM_coupled_setup/build-utils/build-tools.sh .

# Source the build-tools and initialize
source build-tools.sh
cao_init

# Build CPU and GPU
cd build-cpu && cao_build cpu_ocean && cd ..
cd build-gpu && cao_build gpu_coupled && cd ..
```

If you only want to rebuild (e.g. after you made some changes in the code), enter the build directories and run
`cao_rebuild`.

```bash
cd build-cpu && cao_rebuild cpu_ocean && cd ..
cd build-gpu && cao_rebuild gpu_coupled && cd ..
```

> Note that to have the build utilities (e.g. `cao_init`, `cao_build`, `cao_rebuild`, ...) available you should source
*build-tools.sh* in all new shells.

## Run

To run the coupled setup follow these steps:

```bash
# From the base directory navigate to the R02B07 directory
cd EXCLAIM_coupled_setup/R02B07

# Run the experiment for one of the three possible targets: 'hybrid', 'cpu', 'cpu-cpu'
jid=$(sbatch --parsable exp.EXCLAIM_COUPLED_R02B07.run hybrid)

# Inspect the logfile at the end
vim LOG.EXCLAIM_COUPLED_R02B07.${jid}
```