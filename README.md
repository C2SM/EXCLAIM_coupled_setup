# EXCLAIM Coupled Setup Utilities

Collection of utility scripts to build and run the EXCLAIM coupled atmosphere-ocean setup.

## Build

To directly do a clean build of the cpu and gpu executables:
1. get tokens for github.com and gitlab.dkrz.de that give you read access to all icon-exclaim + submodules repos 
2.  Execute from the repository root
``` bash
GITLAB_DKRZ_TOKEN="$(cat path/to/gitlab_dkrz_token)" GITHUB_TOKEN="$(cat path/to/github_token)" sbatch build_utils/full_build.sh 
```
By default:
  - the script will compile the `cpu` and `gpu` targets. `--cpu-only` or `--gpu-only` can be used if needed.
  - the gpu variant is `GPU_MODE="py-substitute"` (compiling with `icon4py` granules). It can be otherwise set with to `GPU_MODE="acc"` for pure fortran OpenACC implementation.
  - the build type is set to `BUILD_TYPE="SPACK"`. A `BUILD_TYPE="NOSPACK"` option is also available but not with `GPU_MODE="py-substitute"`.
  
`BUILD_TYPE` and `GPU_MODE` can be exported or set on the same line as the `sbatch` command.

You can monitor the build with `tail -f full_build.<jobid>.o`. The build happens on `/dev/shm` on the compute nodes. The icon repo will be retrieved at the end of the build as `icon-hybrid-${GPU_MODE}` containing the 2 `build-cpu` and `build-gpu-${GPU_MODE}` subdirectories.

## Run

### Basic runs

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

### Example script

In order to keep a clean git history an example `launch_exp.sh` script is provided. It contains some of the possible settings (case, nodes, walltime, period, restart interval, etc ...). Copy it and modify at will.
