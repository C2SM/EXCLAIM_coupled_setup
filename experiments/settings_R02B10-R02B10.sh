#!/usr/bin/bash

# Grids
# -----
atmos_gridID="0056"             #  icon-nwp grid
atmos_refinement="R02B10"
atmos_refinement_short="r2b10"
ocean_gridID="0059"             #  icon-oce r2b7 grid
ocean_refinement="R02B10"

# Dates and intervals
# -------------------
export restart_interval=${restart_interval:-"P1M"}

# Time steps
# ----------
atmTimeStep="PT20S"            #10s from dyamonds runs # atmos time step (for coupler)  (same as dtime!!)
dtime=20                       # NWP atmospheric timestep (s)   (same as in atmTimeStep!!)
dt_rad=600.                     # NWP radiation timestep (s) - must match coupling/ocean time step
oceTimeStep="PT2M"              # corresponds to "fromClimatology" case, ocean time step (20min for r2b7)
atm_oce_coupling_timestep="PT10M"     # coupling time step atm<->oce (for ocets pt20m and atmts=PT450S)

# Atmosphere settings
# -------------------
case "${TARGET}" in
    "hybrid")
        # nproma_atm=40000
        nproma_atm=${nproma_atm:-0}
        nblocks_c_atm=${nblocks_c_atm:-0}
        nblocks_e_atm=${nblocks_e_atm:-1}
        nproma_sub=6000
        ecrad_isolver=2  # (0 for CPU/vector, 2 for GPU)
        ;;
    "cpu" | "cpu-cpu")
        nproma_atm=${nproma_atm:-16}
        nblocks_c_atm=${nblocks_c_atm:-0}
        nblocks_e_atm=${nblocks_e_atm:-0}
        nproma_sub=16
        ecrad_isolver=0  # (0 for CPU/vector, 2 for GPU)
        ;;
esac        # 3 for inwp forcing; 0 for no forcing

# inputs
datadir_hd_tag="r0100"
bc_land_hd_name="mc_s_v1"
ic_land_hd_name="mc_s_v1_1"

# Ocean settings
# --------------
nproma_oce=16
GMRedi_configuration=0
initial_state_sub_path="rhaak/R2B10L72_spinup.nc"

# Land settings
# --------------
extpar_tag="20250509"

# Output Streams
# --------------
atm_streams=(
    "output_atm_mon"
    "output_atm_mon2d"
    "output_atm_mean"
    # "output_atm_mean3d"
    "output_energy_budget"
    # "output_atm_latlon"
    # "output_jsb_2d"
    "output_lnd_mon"
    # "output_lnd_dbg"
    # "output_hyd_dbg"
)
oce_streams=(
    # "output_oce_fx"
    "output_oce_3h"
    "output_oce_day"
    "output_oce_ice"
    "output_oce_flux"
    "output_oce_mon"
    "output_oce_mon2d"
    "output_oce_moc"
)
dyamond_streams=(
    "dyamond_stream_1_1"
    "dyamond_stream_1_2"
    "dyamond_stream_1_3"
    "dyamond_stream_1_4"
    "dyamond_stream_1_5"
    "dyamond_stream_2"
    "dyamond_stream_3" 
    "dyamond_stream_4"
    "dyamond_stream_5"
    "dyamond_stream_6"
    "dyamond_stream_7"
    "dyamond_stream_8"
    "dyamond_stream_9"
    "dyamond_stream_10"
    "dyamond_stream_11"
    "dyamond_stream_12"
    "dyamond_stream_13"
    "dyamond_stream_14"
    # "dyamond_stream_15_1"
    # "dyamond_stream_15_2"
    # "dyamond_stream_15_3"
    # "dyamond_stream_15_4"
    # "dyamond_stream_15_5"
    # "dyamond_stream_15_6"
    # "dyamond_stream_15_7"
    # "dyamond_stream_15_8"
    # "dyamond_stream_15_9"
    # "dyamond_stream_15_10"
    # "dyamond_stream_15_11"
    # "dyamond_stream_15_12"
    # "dyamond_stream_15_13"
)
