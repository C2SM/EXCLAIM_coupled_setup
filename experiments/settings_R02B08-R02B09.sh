#!/usr/bin/bash

# Grids
# -----
atmos_gridID="0033"             #  icon-nwp r2b8 grid
atmos_refinement="R02B08"
atmos_refinement_short="r2b8"
ocean_gridID="0016"             #  icon-oce r2b8 grid
ocean_refinement="R02B09"

# Dates and intervals
# -------------------
export restart_interval=${restart_interval:-"P1M"}

# Time steps
# ----------
atmTimeStep="PT60S"            # atmos time step (for coupler)  (same as dtime!!)
dtime=60                       # NWP atmospheric timestep (s)   (same as in atmTimeStep!!)
dt_rad=120.                     # NWP radiation timestep (s) - must match coupling/ocean time step
oceTimeStep="PT2M"              # corresponds to "fromClimatology" case, ocean time step (20min for r2b7)
atm_oce_coupling_timestep="PT20M"     # coupling time step atm<->oce (for ocets pt20m and atmts=PT450S)

# Atmosphere settings
# -------------------
case "${TARGET}" in
    "hybrid")
        # nproma_atm=83752
        nproma_atm=0
        nblocks_c=1
        nproma_sub=10469
        ecrad_isolver=2  # (0 for CPU/vector, 2 for GPU)
        ;;
    "cpu" | "cpu-cpu")
        nproma_atm=16
        nblocks_c=0
        nproma_sub=16
        ecrad_isolver=0  # (0 for CPU/vector, 2 for GPU)
        ;;
esac        # 3 for inwp forcing; 0 for no forcing

# inputs
datadir_hd_tag="r0002"
bc_land_hd_name="mc_maxl_s_v1"
ic_land_hd_name="mc_maxl_s_v1_1"

# Ocean settings
# --------------
nproma_oce=16
GMRedi_configuration=0
initial_state_sub_path="rcscs/tsi_oras5_icon_icon_grid_${ocean_gridID}_${ocean_refinement}_O_${ocean_vertical_levels}.nc_${start_year}-01-01"

# Land settings
# --------------
extpar_tag="20250708"

# Output Streams
# --------------
atm_streams=(
    "output_atm_mon"
    "output_atm_mon2d"
    "output_atm_mean"
    # "output_atm_mean3d"
    "output_energy_budget"
    # "output_atm_latlon"
    "output_jsb_2d"
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
