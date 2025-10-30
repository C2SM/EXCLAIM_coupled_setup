#!/usr/bin/bash

# Grids
# -----
atmos_gridID="0061"             #  icon-nwp grid
atmos_refinement="R02B07"
atmos_refinement_short="r2b7"
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
        export ATM_COMP_TASKS_PER_NODE=108
        export TOT_TASKS_PER_NODE=144
        export CPUS_PER_TASK=1
        ;;
esac
export ATM_IO_TASKS=2
export OCE_IO_TASKS=2
export ATM_RST_TASKS=0
export OCE_RST_TASKS=0

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

# inputs
datadir_hd_tag="r0100"
bc_land_hd_name="mc_maxl_s_v1"
ic_land_hd_name="mc_maxl_s_v1"

# Ocean settings
# --------------
nproma_oce=16
GMRedi_configuration=1
initial_state_sub_path="rcscs/tsi_oras5_icon_icon_grid_${ocean_gridID}_${ocean_refinement}_O_${ocean_vertical_levels}.nc_${start_year}-01-01"

# Land settings
# --------------
extpar_tag="20250509"

# Output Streams
# --------------
coupled_streams(){
    # Atmosphere / Land
    # -----------------
    output_atm_mon
    output_atm_2d
    output_energy_budget
    output_atm_hfreq
    output_atm_pres
    output_atm_geopot
    output_atm_temp
    output_atm_u
    output_atm_v
    output_atm_qv
    output_atm_rh
    output_atm_clc
    output_atm_tot_qc_dia
    output_atm_tot_qi_dia
    output_atm_latlon
    # output_atm_icon
    # output_atm_spot
    output_jsb_2d
    output_lnd_mon
    # output_lnd_dbg
    # output_hyd_dbg
    # output_lnd_wat
    # output_atm_alb

    # Ocean
    # -----
    output_oce_fx
    output_oce_def
    output_oce_ice
    output_oce_flux
    output_oce_mon
    output_oce_moc
}
