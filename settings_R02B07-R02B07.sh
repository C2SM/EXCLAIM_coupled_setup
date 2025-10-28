#!/usr/bin/bash

# Grids
# -----
atmos_gridID="0061"             #  icon-nwp grid
atmos_refinement="R02B07"
ocean_gridID="0062"             #  icon-oce r2b7 grid
ocean_refinement="R02B07"

# Dates and intervals
# -------------------
export restart_interval=${restart_interval:-"P1M"}

# Time steps
# ----------
atmTimeStep="PT150S"            # atmos time step (for coupler)  (same as dtime!!)
dtime=150                       # NWP atmospheric timestep (s)   (same as in atmTimeStep!!)
dt_rad=300.                     # NWP radiation timestep (s) - must match coupling/ocean time step
oceTimeStep="PT5M"              # corresponds to "fromClimatology" case, ocean time step (20min for r2b7)
atm_oce_coupling_timestep="PT10M"     # coupling time step atm<->oce (for ocets pt20m and atmts=PT450S)

# Task distribution
# -----------------
case "${TARGET}" in
    "hybrid")
        export ATM_COMP_TASKS_PER_NODE=4
        export TOT_TASKS_PER_NODE=16
        export CPUS_PER_TASK=16
        ;;
    "cpu" | "cpu-cpu")
        export ATM_COMP_TASKS_PER_NODE=256
        export TOT_TASKS_PER_NODE=288
        export CPUS_PER_TASK=1
        ;;
esac
export ATM_IO_TASKS=0
export OCE_IO_TASKS=0
export ATM_RST_TASKS=0
export OCE_RST_TASKS=0

# output intervals
#all_output_interval="PT30M"    # short test
#all_output_interval="PT60M"     # short test
#all_output_interval="P1D"      # daily output
all_output_interval="PT300S"      # monthly output - production

# Atmosphere settings
# -------------------
case "${TARGET}" in
    "hybrid")
        nproma_atm=83752
        nproma_sub=10469
        ecrad_isolver=2  # (0 for CPU/vector, 2 for GPU)
        ;;
    "cpu" | "cpu-cpu")
        nproma_atm=16
        nproma_sub=16
        ecrad_isolver=0  # (0 for CPU/vector, 2 for GPU)
        ;;
esac        # 3 for inwp forcing; 0 for no forcing

# output intervals
atm_file_interval="P1M"
atm_output_interval=${all_output_interval}
atm_hfreq_output_interval="PT6H"

# Ocean settings
# --------------
nproma_oce=16

# output intervals
oce_file_interval="P1M"
oce_output_interval=${all_output_interval}
oce_output_interval_def="P1D"
