#!/usr/bin/bash

# ================================================================
# Icon master

icon_master_nml(){
    cat > ${master_namelist} << EOF
&master_nml
 lrestart               = ${lrestart:-.false.}
 read_restart_namelists = ${read_restart_namelists:-.true.}
/
&master_time_control_nml
 calendar             = 'proleptic gregorian'
 checkpointTimeIntval = "${checkpoint_interval}"
 restartTimeIntval    = "${restart_interval}"
 experimentStartDate  = "${start_date}"
 experimentStopDate   = "${end_date}"
/
&time_nml
 is_relative_time = .false.
/
&master_model_nml
  model_type = 1
  model_name = "atmo"
  model_namelist_filename = "${atm_namelist}"
  model_min_rank=${ATM_MIN_RANK}
  model_max_rank=${ATM_MAX_RANK}
  model_inc_rank=1
/
&master_model_nml
  model_type=2
  model_name="ocean"
  model_namelist_filename="${oce_namelist}"
  model_min_rank=${OCE_MIN_RANK}
  model_max_rank=${OCE_MAX_RANK}
  model_inc_rank=1
/
&jsb_control_nml
 is_standalone      = .false.
 restart_jsbach     = ${restart}
 debug_level        = 0
 timer_level        = 0
/
&jsb_model_nml
 model_id = 1
 model_name = "JSBACH"
 model_shortname = "jsb"
 model_description = 'JSBACH land surface model'
 model_namelist_filename = "${lnd_namelist}"
/
EOF
}

# ================================================================
# Coupling

coupling_yaml(){
  cat > ${cpl_yaml} << EOF
definitions:
  atm2oce: &atm2oce
    src_component: atmo
    src_grid: icon_atmos_grid
    tgt_component: ocean
    tgt_grid: icon_ocean_grid
    time_reduction: average
    src_lag: ${atm_lag}
    tgt_lag: ${oce_lag}
  oce2atm: &oce2atm
    src_component: ocean
    src_grid: icon_ocean_grid
    tgt_component: atmo
    tgt_grid: icon_atmos_grid
    time_reduction: average
    src_lag: ${oce_lag}
    tgt_lag: ${atm_lag}
  interp_stacks:
    hcsbb_interp_stack: &hcsbb_interp_stack
      interpolation:
        - bernstein_bezier
        - nnn:
            n: 4
            weighted: arithmetic_average
        - fixed:
            user_value: -999.9
    conserv_interp_stack: &conserv_interp_stack
      interpolation:
        - conservative:
            order: 1
            enforced_conservation: false
            partial_coverage: true
            normalisation: fracarea
        - fixed:
            user_value: -999.9
    conserv_interp_dest: &conserv_interp_dest
      interpolation:
        - conservative:
            order: 1
            enforced_conservation: false
            partial_coverage: true
            normalisation: destarea
    spmap_interp_stack: &spmap_interp_stack
      interpolation:
        - source_to_target_map:
            spread_distance: 0.3
            max_search_distance: 9
        - fixed:
            user_value: 0.0

timestep_unit: ISO_format
calendar: proleptic-gregorian
coupling:
  - <<: [ *atm2oce, *hcsbb_interp_stack ]
    coupling_period: ${atm_oce_coupling_timestep}
    field: [surface_downward_eastward_stress,
            surface_downward_northward_stress]
  - <<: [ *atm2oce, *conserv_interp_stack ]
    coupling_period: ${atm_oce_coupling_timestep}
    field: [surface_fresh_water_flux,
            total_heat_flux,
            atmosphere_sea_ice_bundle]
    #weight_file_name: w_atm2oce.nc
  - <<: [ *oce2atm, *conserv_interp_stack ]
    coupling_period: ${atm_oce_coupling_timestep}
    field: [sea_surface_temperature,
            ocean_sea_ice_bundle]
    #weight_file_name: w_oce2atm.nc
  - <<: [ *oce2atm, *conserv_interp_stack ]
    coupling_period: ${atm_oce_coupling_timestep}
    field: [eastward_sea_water_velocity,
            northward_sea_water_velocity]
    #weight_file_name: w_riv2oce.nc
  - <<: [ *atm2oce, *spmap_interp_stack ]
    coupling_period: ${atm_oce_coupling_timestep}
    field: river_runoff
EOF
}

# ================================================================
# Atmosphere

atmo_nml(){
  # Best sleve parameters for 90/130 vertical levels:
  case "${nlev}" in
      "90")
          MAXLAYTH=400.                           # maximum layer thickness below htop_thcknlimit
          TOPTHLIM=14000.                         # top limit for max_lay_thckn
          STRETFAC=0.9                            # stretch factor
          FLHEIGHT=16000.                         # flat height
          ;;
      "130")
          MAXLAYTH=500.
          TOPTHLIM=35000.
          STRETFAC=1.2
          FLHEIGHT=35000.
          ;;
      *)
          echo "ERROR: unsupported nlev: ${nlev}"
          exit 1
          ;;
  esac
  cat > ${atm_namelist} << EOF
!
&coupling_mode_nml
 coupled_to_ocean        = .TRUE.
 !NB comment for HDint
 !coupled_to_hydrodisc    = .FALSE.
/
!
&parallel_nml
 nproma                  = ${nproma_atm}
 !nblocks_c               = 1
 !nproma_sub              = 1000
 p_test_run              = .false.
 l_fast_sum              = .false.
 l_test_openmp           = .false.
 l_log_checks            = .false.
 num_io_procs            = ${mpi_atm_io_procs}
 pio_type                = 1                   ! default 1: asynchron io
 num_restart_procs       = ${num_rest_atm_mpi} ! number of procs for multifile restart
 num_dist_array_replicas = ${replicate_grid-1} ! can be 1 iff passive or active (see HAVE_SLOW_PASSIVE_TARGET_ONESIDED) target RMA works well
/
&grid_nml
 dynamics_grid_filename  = "${atmos_grid_target}"
 dynamics_parent_grid_id = 0,1
 !lredgrid_phys           = .true.
 lredgrid_phys           = .false.
 !radiation_grid_filename = "$rad_grid_target"
 radiation_grid_filename = " "
 lfeedback               = .false.
/
&run_nml
 num_lev                 = ${nlev}              ! number of full levels of vertical grid
 modelTimeStep           = "${atmTimeStep}"
 ldynamics               = .TRUE.               ! dynamics
 ltransport              = .TRUE.               ! transport
 iforcing                = ${iforcing}          !
 ntracer                 =  5                   ! number of tracers - default 0
 ltimer                  = .true.               !
 ltestcase               = .false.              ! initialize with real data
 timers_level            = 10
 msg_level               = 10                    ! detailed report during integration (5-7 or 13 incl. seaice prints)
 output                  = 'nml'
 check_uuid_gracefully   = .true.
 Restart_filename        = "${EXPNAME}_restart_atm_<rsttime>.nc"
 activate_sync_timers = .TRUE.
/
&dynamics_nml
 iequations              = ${atmo_model_equations}
 lcoriolis               = .TRUE.
 divavg_cntrwgt          = 0.50
/
&diffusion_nml
 hdiff_order             = 5
 hdiff_efdt_ratio        = 24.0
 hdiff_smag_fac          = 0.025
 lhdiff_vn               = .TRUE.
 lhdiff_temp             = .TRUE.
 hdiff_multfac           = 1.0
 hdiff_tv_ratio          = 1.0
/
&nwp_phy_nml
 !icalc_reff              = 100         ! For Modis cdnc
 !icpl_rad_reff           = 0           ! For Modis cdnc
 inwp_gscp               = 2
 inwp_convection         = 1
 inwp_radiation          = 4
 inwp_cldcover           = 1
 inwp_satad              = 1
 inwp_sso                = 1
 inwp_gwd                = 1
 latm_above_top          = .false.
 efdt_min_raylfric       = 7200.
 itype_z0                = 2
 icapdcycl               = 3
 icpl_aero_conv          = 0           ! 1 if irad_aero=6 or 9 is used - 0 for irad_aero=12
 icpl_aero_gscp          = 0           ! 1 if irad_aero=6 or 9 is used - 0 for irad_aero=12
 icpl_o3_tp              = 1
 !lshallow_liquid_only   = .true.
 dt_rad                  = ${dt_rad}
 dt_conv                 = ${dtime}      !NB can make these longer ! time step for convection in s (domain specific)
 dt_sso                  = ${dtime}       ! time step for SSO parameterization
 dt_gwd                  = ${dtime}       ! time step for gravity wave drag parameterization
 !                                     ! parameters for jsbach/vdiff land model:
 inwp_surface            = 2           ! 1 = terra; 2 = JSBACH
 inwp_turb               = 6           ! 1 = terra; 6 = VDIFF
 ! NB adding tuning from Dragon runscript
 mu_rain                 = 0.5     ! new tuning becoming operational in July 2018
 rain_n0_factor          = 0.1
/
&nwp_tuning_nml
 tune_rprcon             = 1.0e-3
 itune_albedo            = 0                         ! 0: no MODIS tuning (def); 1: dimmed Sahara; 2: +brighter AA
 tune_zceff_min          = 0.01                     ! ** default value (0.01) to be used for R3B7; use 0.025 for R2B6
 tune_gkdrag             = 0.09                     ! R2B6: 0.075
 tune_gkwake             = 1.8                       ! R2B6: 1.5
 tune_gfrcrit            = 0.333                     ! R2B6: 0.425
 tune_grcrit             = 0.25
 tune_grcrit_enh         = 0.25
 tune_dust_abs           = 0.
 tune_box_liq_asy        = 3.0    ! 3.5              ! oper global: 3.0 , oper D2: 3.25, default: 2.5
 tune_box_liq            = 0.07
 tune_rcucov             = 0.075
 tune_rhebc_land         = 0.825
 tune_gust_factor        = 7.0
 icpl_turb_clc           = 1
 lcalib_clcov            = .false.                   ! turn off TCC, HCC, MCC, LCC tuning
 tune_eiscrit            = 7.0                       ! to switch off conv param in stratocumulus regions
 tune_sc_eis             = 7.             ! ! default: 1000. - exec newer 633d375ad0
 tune_zvz0i              = 0.9    ! 1.2 !  0.6      ! default: 0/.85   ; Terminal fall velocity of ice
 tune_entrorg            = 1.75e-3        !  3.0e-3   ! default: 1.95e-3; Entrainment parameter valid for dx=20 km
 tune_sc_eis             = 7.            !  7.       ! default: 1000. - exec newer 633d375ad0
/
&transport_nml
 ivadv_tracer            = 3,3,3,3,3
 itype_hlimit            = 4,4,4,4,4
 ihadv_tracer            = 2,2,2,2,2
 itype_vlimit            = 1,1,1,1,1
 ivlimit_selective       = 1,1,1,1,1
 llsq_svd                = .true.
/
&interpol_nml
 nudge_zone_width        = 8
 l_intp_c2l              = .true.
 rbf_scale_mode_ll       = 2
/
&nonhydrostatic_nml
 iadv_rhotheta           = 2
 ivctype                 = 2
 itime_scheme            = 4
 exner_expol             = 0.333
 vwind_offctr            = 0.2                       ! 0.2 for R2B6 and higher resolution, 0.3 for lower resolution
 damp_height             = 50000.
 rayleigh_coeff          = 0.50001                       ! default: 0.05
 divdamp_order           = 24                        ! 2 ass, 24 fc
 divdamp_type            = 32                        ! optional: 2 assimilation cycle, 32 forecast
 divdamp_fac             = 0.003                     ! 0.004 for R2B6; recommendation for R3B7: 0.003
 divdamp_trans_start     = 12500
 divdamp_trans_end       = 17500
 igradp_method           = 3
 l_zdiffu_t              = .true.
 thslp_zdiffu            = 0.02
 thhgtd_zdiffu           = 125.
 htop_moist_proc         = 22500.
 hbot_qvsubstep          = 16000.
/
&sleve_nml
 top_height          = 75000.                         ! model top
 min_lay_thckn       = 20.                            ! lowest level thickness (between half-levels)
 decay_scale_1       = 4000.
 decay_scale_2       = 2500.
 decay_exp           = 1.2
 max_lay_thckn       = ${MAXLAYTH}                    ! maximum layer thickness below htop_thcknlimit
 htop_thcknlimit     = ${TOPTHLIM}                    ! top limit for max_lay_thckn
 stretch_fac         = ${STRETFAC}
 flat_height         = ${FLHEIGHT}
/
&io_nml
 lflux_avg               = .FALSE.                   ! true: averaged (ashfl_s), false: accumulated fluxes (accshfl_s)
 itype_pres_msl          = 5                         ! (1) 3: IFS-type extrapolation
 itype_rh                = 1                         ! (1) 2: mixed phase (water and ice)
 inextra_3d              = 2                         ! 3D extra variables
 inextra_2d              = 10                        ! 2D extra variables
 restart_write_mode      = "joint procs multifile"   ! asynchron multifile restart handling; 'sync' for single file writing
 lkeep_in_sync           = .TRUE.                    ! sync after each timestep
 lnetcdf_flt64_output    = .FALSE.                   ! T: 64 bit output in all files
/
EOF
}

# ================================================================
# Atmosphere output

output_atm_mon(){
  # monitoring file for atmos
  stream="${EXPNAME}_atm_mon"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 mode                    = 1                         ! 1: forecast mode (relative t-axis)
 operation               = "mean"                    ! works on icon grid only (remap=0)
 remap                   = 0                         ! 1: latlon,  0: native grid
 include_last            = .FALSE.                   ! flag whether to include the last time step
 output_grid             = .FALSE.                   ! flag whether grid information is added to output.
 filename_format         = "${stream}/${stream}_<datetime2>"
 ml_varlist              = 'tas_gmean','rsdt_gmean','rsut_gmean','rlut_gmean','radtop_gmean','prec_gmean','evap_gmean','pme_gmean'
/
EOF
}

output_atm_2d(){
  # native time mean output 2-dim averaged over OUTPUT_INTERVAL:
  stream="${EXPNAME}_atm_2d_ml"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist              = 'pres_msl', 'pres_sfc', 't_s', 'clct', 'tot_prec_rate',
                           'tqv', 'tqv_dia', 'tqc_dia', 'tqi_dia', 'umfl_s', 'vmfl_s', 'sp_10m', 't_2m',
                           'sob_t', 'sod_t', 'sou_t', 'thb_t', 'sob_s', 'sou_s', 'thb_s', 'shfl_s', 'lhfl_s',
                           'snow_con_rate', 'snow_gsp_rate', 'ice_gsp_rate', 'qifl_s', 'qhfl_s',
                           't_seasfc', 'fr_land', 'fr_seaice', 'condhf_ice', 'meltpot_ice', 't_ice', 'h_ice',
                           'albvisdif', 'albvisdir', 'albnirdif', 'albnirdir'
/
EOF
}

output_energy_budget(){
  stream="${EXPNAME}_energy_budget"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "P1Y"                     ! the output interval and
 file_interval           = "P1Y"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !reg_lon_def             = 0.,1.,360.
 !reg_lat_def             = -90.,1.,90.
 operation               = "mean"                    ! works on icon grid only (remap=0)
   ml_varlist = 'group:jsb_seb_basic', 'shfl_s', 'lhfl_s', 'thb_s', 'sou_s', 'sob_s', 'thb_t', 'sou_t', 'sod_t', 'sob_t'
/
EOF
}

output_atm_hfreq(){
  # native high-frequency time mean output 3-dim averaged:
  stream="${EXPNAME}_atm_hfreq_ml"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_hfreq_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 operation               = "mean"                    ! works on icon grid only (remap=0)
 !ml_varlist              = 'pres', 'geopot', 'temp', 'u', 'v', 'w','rho'
 ml_varlist              = 'pres_sfc', 'pres', 'geopot', 'temp', 'u', 'v', 'w' ! , 'rho'
/
EOF
}


output_atm_3d(){
  # native time mean output 3-dim averaged over OUTPUT_INTERVAL:
  stream="${EXPNAME}_atm_3d_ml"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 operation               = "mean"                    ! works on icon grid only (remap=0)
 !ml_varlist              = 'pres', 'geopot', 'temp', 'u', 'v', 'qv', 'rh', 'clc', 'tot_qc_dia', 'tot_qi_dia'
 ml_varlist              = 'pres', 'geopot', 'temp', 'u', 'v', 'qv', 'rh', 'clc', 'tot_qc_dia', 'tot_qi_dia', 'runoff_s','runoff_g'
/
EOF
}

output_atm_latlon(){
  # interpolated lat-lon output 2d and 3d, no time average:
  stream="${EXPNAME}_atm_latlon"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml       !! interpolated lat-lon output
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    = 1                         ! 1: forecast mode (relative t-axis)
 include_last            = .FALSE.                   ! set to false for asynchron output
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 1                         ! 1: latlon,  0: native grid
 reg_lon_def             = 0.,1.,360.
 reg_lat_def             = -90.,1.,90.
 ml_varlist              = 'clct', 'tqv', 'tqc_dia', 'tqi_dia', 'sp_10m', 't_2m' ,'t_g', 'qv_2m', 'h_ice', 't_ice',
                           'accthb_s','accthb_t','accsob_s','accsob_t','accshfl_s','acclhfl_s','accumfl_s','accvmfl_s',
                           'pres_sfc', 'tot_prec', 't_seasfc', 'fr_seaice',
                           'fr_land', 'fr_lake', 'fr_seaice', 't_seasfc'
 !hl_varlist              = 'temp', 'u'
 !h_levels                = 10, 500, 5000
 !m_levels                = "5...(nlev+1)"
/
EOF
}

output_atm_icon(){
  # native time mean output (debug), averaged over OUTPUT_INTERVAL:
  stream="${EXPNAME}_atm_icon"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    = 1                         ! 1: forecast mode (relative t-axis)
 include_last            = .FALSE.                   ! set to false for asynchron output
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist              = 'pres_sfc', 'tot_prec_rate', 'sp_10m', 't_2m', 't_g', 'tqv', 'clct', 'h_ice', 't_ice',
                           'shfl_s', 'lhfl_s', 'thb_s', 'sob_s','condhf_ice'
 !                         'shfl_s_t_7', 'shfl_s_t_9', 'lhfl_s_t_7', 'lhfl_s_t_9',
 !                         'thb_s_t_7', 'thb_s_t_9', 'sob_s_t_7', 'sob_s_t_9',
 !                         'frac_t_7', 'frac_t_8', 'frac_t_9'
/
EOF
}

output_atm_spot(){
  # native spot value (debug) output 2d and
  stream="${EXPNAME}_atm_spot"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    = 1                         ! 1: forecast mode (relative t-axis)
 include_last            = .FALSE.                   ! set to false for asynchron output
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist              = 'pres_sfc', 'tot_prec_rate', 'sp_10m', 't_2m', 't_g', 'tqv', 'clct', 'h_ice', 't_ice',
                           'shfl_s', 'lhfl_s', 'thb_s', 'sob_s','condhf_ice',
                           'fr_land', 'fr_lake', 'fr_seaice', 't_seasfc', 'lsm_switch', 'lsm_ctr_c', 'topography_c'
 !                         'shfl_s_t_7', 'shfl_s_t_9', 'lhfl_s_t_7', 'lhfl_s_t_9',
 !                         'thb_s_t_7', 'thb_s_t_9', 'sob_s_t_7', 'sob_s_t_9',
 !                         'condhf_ice', 'frac_t_7', 'frac_t_8', 'frac_t_9',
/
EOF
}

output_jsb_2d(){
  stream="${EXPNAME}_jsb_2d"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 operation               = "mean"                    ! works on icon grid only (remap=0)
   ml_varlist = 'group:jsb_seb_basic'!,'group:jsb_all_basic'
/
EOF
}

output_lnd_mon(){
  stream="${EXPNAME}_lnd_mon"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml ! 'lnd_mon'
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 operation               = "mean"                    ! works on icon grid only (remap=0)
  ! ml_varlist = 'hd_water_error_gsum_box',
    ml_varlist = 'hydro_trans_gmean_box', 'hydro_evapotrans_gmean_box',
                 'hydro_weq_land_gsum_box', 'hydro_discharge_ocean_gsum_box',
                 'hydro_wtr_rootzone_rel_gmean_box',
                 'hydro_fract_snow_gsum_box', 'hydro_weq_snow_gsum_box',
                 'hydro_weq_balance_err_gsum_box', 'pheno_lai_ta_gmean_box',
                 'pheno_fract_fpc_gmean_box', 'seb_t_gmean_box'
/

EOF
}

output_lnd_dbg(){
  # output lnd_dbg
  stream="${EXPNAME}_lnd_dbg"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist              = 'u', 'v', 'w', 'temp', 'theta_v', 'exner', 'pres', 'pres_msl', 'rho', 'rho_ic',
                           'group:precip_vars', 'group:additional_precip_vars', 'clc',
                           'group:land_vars', 'group:rad_vars', 'group:dwd_fg_sfc_vars',
                           'group:vdiff', 'group:vdiff-sft', 'clct', 'tot_prec_rate',
                           'tqv', 'tqc', 'tqi', 'tqr', 'tqs', 'total_water_var',
                           'qv', 'qc', 'gust10', 'rcld', 'ddt_temp_turb',
                           't_2m', 'tmin_2m', 'tmax_2m', 'td_2m', 'z_ifc', 'z_mc',
                           'tqv_dia', 'tqc_dia', 'tqi_dia', 'umfl_s', 'vmfl_s', 'sp_10m', 't_2m',
                           'sob_t', 'thb_t', 'sob_s', 'thb_s', 'shfl_s', 'lhfl_s', 'qhfl_s', 't_seasfc', 'fr_seaice'
/
EOF
}

output_hyd_dbg(){
  stream="${EXPNAME}_lnd_dbg"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist		 = 'group:land_vars'

/
EOF
}

output_lnd_wat(){
  # instantaneous output of water content box:
  stream="${EXPNAME}_jsb_wbal"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !operation               = "mean"                    ! works on icon grid only (remap=0)
 !ml_varlist              = 'hydro_water_content_box'
 ml_varlist              = 'hydro_weq_fluxes_box', 'hydro_weq_land_box', 'hydro_weq_balance_err_box',
                           'hydro_runoff_box'   , 'hydro_drainage_box',
                           'hd_water_budget_box', 'hd_local_wbal_error_box'
/
EOF
}

output_atm_alb(){
# additional output of atmosphere albedo: albnir
  stream="${EXPNAME}_atm_alb"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 4                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist              = 'albvisdif', 'albvisdir', 'albnirdif', 'albnirdir', 'albdif'
/
EOF
}

# ================================================================
# Land

lnd_nml(){
  cat > ${lnd_namelist} << EOF
&jsb_model_nml
  usecase              = 'jsbach_pfts'
  use_lakes            = .TRUE.
  fract_filename       = '${jsbach_lnd_frac}'
  !output_tiles         = ${output_tiles}     ! List of tiles to output
  !NB important to initialize jsbach from its own initial file, not ifs
  init_from_ifs        = .FALSE.               ! needs ifs_filename
  ifs_filename         = '${ifs_fname}'
/
&jsb_seb_nml
  bc_filename          = '${jsbach_bc_phys}'
  ic_filename          = '${jsbach_ic_soil}'
/
&jsb_rad_nml
  use_alb_veg_simple   = .FALSE.          ! Use TRUE for jsbach_lite, FALSE for jsbach_pfts
  bc_filename          = '${jsbach_bc_phys}'
  ic_filename          = '${jsbach_ic_soil}'
/
&jsb_turb_nml
  bc_filename          = '${jsbach_bc_phys}'
  ic_filename          = '${jsbach_ic_soil}'
/
&jsb_sse_nml
  l_heat_cap_map       = .FALSE.
  l_heat_cond_map      = .FALSE.
  l_heat_cap_dyn       = .TRUE.
  l_heat_cond_dyn      = .TRUE.
  l_snow               = .TRUE.
  l_dynsnow            = .TRUE.
  l_freeze             = .TRUE.
  l_supercool          = .TRUE.
  bc_filename          = '${jsbach_bc_soil}'
  ic_filename          = '${jsbach_ic_soil}'
/
&jsb_hydro_nml
  l_organic            = .FALSE.
  bc_filename          = '${jsbach_bc_soil}'
  ic_filename          = '${jsbach_ic_soil}'
  bc_sso_filename      = '${jsbach_sso}'
  l_read_initial_moist = .TRUE.  !NB redundant if init_from_ifs=false
/
&jsb_assimi_nml
  active               = .TRUE.              ! Use FALSE for jsbach_lite, TRUE for jsbach_pfts
/
&jsb_pheno_nml
  active               = .TRUE.
  scheme               = 'logrop'            ! scheme = logrop / climatology; use climatology for jsbach_lite
  bc_filename          = '${jsbach_bc_phys}'
  ic_filename          = '${jsbach_ic_soil}'
/
&jsb_carbon_nml
  active               = .TRUE.              ! true: use ccycle namelist
  !bc_filename          = '${jsbach_bc_carbon}'
  !ic_filename          = '${jsbach_ic_carbon}'
/
&jsb_fuel_nml
  active               = .FALSE.
  fuel_algorithm       = 1
/
&jsb_disturb_nml
  active               = .FALSE.
  ic_filename          = '${jsbach_ic_soil}'
  bc_filename          = '${jsbach_bc_phys}'
  fire_algorithm       = 1
  windbreak_algorithm  = 1
  lburn_pasture        = .FALSE.
/
&jsb_hd_nml ! for HD intern
  active               = .TRUE.
  routing_scheme       = 'full'               ! 'zero' no HD, runoff=0; 'full' incl. full HD model; 'weighted_to_coast'
  !active		= .FALSE.  !NB add to test
  !routing_scheme	= 'zero'  !NB add to test
  bc_filename          = 'bc_land_hd.nc'
  diag_water_budget    = .FALSE.
  debug_hd             = .FALSE.
  enforce_water_budget = 'error'          ! TRUE: stop in case of water conservation problem NB: this changed to 'error','warning',
  read_initial_reservoirs = .TRUE. !NB added - this is for the hdext input files from Tom Riddick
  use_bifurcated_rivers = .TRUE. !NB added - from Tom Riddick
/
EOF
}

# ================================================================
# Ocean
oce_nml(){
  cat > ${oce_namelist} << EOF
&coupling_mode_nml
  coupled_to_atmo            = .TRUE.
/
&parallel_nml
 nproma                      = ${nproma_oce}
 num_io_procs                = ${mpi_oce_io_procs}
 io_proc_chunk_size          = 8
 pio_type                    = 1                                ! default 1: asynchron io
 num_restart_procs           = ${num_rest_oce_mpi}              ! number of procs for multifile restart
 num_prefetch_proc           = 0
 p_test_run                  = .FALSE.
 l_fast_sum                  = .FALSE.
/
&grid_nml
 dynamics_grid_filename      = "${ocean_grid_target}"
 use_dummy_cell_closure      = .TRUE.
 use_duplicated_connectivity = .FALSE.
/
&dynamics_nml
 iequations                  = -1                               ! -1: hydrost. ocean model
/
&run_nml
 modelTimeStep               = "${oceTimeStep}"
 output                      = 'nml'                            ! namelist controlled output scheme
 activate_sync_timers        = .TRUE.
 profiling_output            = 1                                ! aggregated: 1; detailed: 2; in files: 3
 msg_timestamp               = .FALSE.
 timers_level                = 10
 debug_check_level           = 1
 Restart_filename            = "${EXPNAME}_restart_oce_<rsttime>.nc"
/
&dbg_index_nml
  idbg_mxmn                  = 1                                ! initialize MIN/MAX  debug output
  idbg_val                   = 0                                ! initialize one cell debug
  idbg_slev                  = 1                                ! initialize start level for debug output
  idbg_elev                  = 3                                ! initialize end level for debug output
  dbg_lat_in                 = 30.0                             ! latitude location of one cell debug output
  dbg_lon_in                 = -30.0                            ! longitude location of one cell debug output
  str_mod_tst                ='oceanCouplng'                    ! define modules to print out in debug mode
  !str_mod_tst                = 'all'                            ! define modules to print out in debug mode
/
&ocean_dynamics_nml
!
! zlev or zstar:
  vert_cor_type = ${vert_cor_type}
  minVerticalLevels = ${minVerticalLevels}

!
! Number of vertical levels
  n_zlev                =  $n_zlev
  dzlev_m(1:${n_zlev})  =  $dzlev_m
!
  l_edge_based                               = .FALSE.     ! edge- or cell-based mimetic discretization
! l_partial_cells                            = .FALSE.     ! partial bottom cells=TRUE: local varying bottom depth
  select_solver                              = 4           ! 1=gmres_oce_old; 2=ocean_restart_gmres, 3=mixed precisison restart
                                                           ! 4=CG (default) 5=CGJ 6=BiCG 7=GMRES restart (legacy) 8=MINRES
  use_absolute_solver_tolerance              = .TRUE.
  solver_tolerance                           = 1.0E-8      ! 1e-10 in omip-YVF - enlarged to 1e-9 (LL opt)
  select_lhs                        = ${select_lhs}        ! 1=operator based (default) 2=matrix based - 2=error?
  l_lhs_direct                      = ${l_lhs_direct}      ! .true.= use lhs implementation directly  .false.= matrix scanner (default)
  solver_FirstGuess                          = 2           ! 0=start from zeros 1=last timestep smoothed 2=last timestep (default)

! solver_max_iter_per_restart                = 14
! solver_max_restart_iterations              = 100         ! outer (restart solver)

  fast_performance_level                     = 200         ! performance level 12: for cell-based; 5: default
  use_continuity_correction                  = .TRUE.      ! height adjustment according to vertical velocity in dynamics
  cfl_check                                  = .FALSE.
  cfl_write                                  = .FALSE.

  i_bc_veloc_top                             = 1
  i_bc_veloc_bot                             = 1           ! 0: (def) bottom friction off, 1: on
/
&ocean_tracer_transport_nml
  flux_calculation_horz                      = 5           ! 1=upwind, 2=central, 3=Lax-Friedrichs,
                                                           ! 4=Miura, 5=FCT with Zalesak limiter (default)
  flux_calculation_vert                      = 7           ! 6=adpo; 7=upwind biased ppm (default); 8=FCT with zalesak limiter
  ! define low and high order methods to be used in
  ! horizontal flux corrected transport methods
  ! (flux_calculation_horz=4,5)
  fct_low_order_flux                         = 1           ! horizontal low  order method: 1=upwind (def), no other implemented
  fct_high_order_flux                        = 5           ! horizontal high order method: 1=upwind, 2=central, 3=lax_friedrichs, 4=miura_order1
  fct_limiter_horz                           = 100         ! zalesak
/
&ocean_horizontal_diffusion_nml
  laplacian_form                             = 1           ! 1=curlcurl-graddiv
  VelocityDiffusion_order                    = 2           ! 21=biharmonic+laplacian (for the laplacian leith)
!
! BiharmonicViscosity_scaling                = 1           ! 1: no scaling used in XPP1 for r2b6
! BiharmonicViscosity_reference              = 3.5E12      ! [m2/s] constant biharmonic horizontal viscosity coefficient for velocity
  BiharmonicViscosity_background             = 0.0
  BiharmonicViscosity_scaling                =  4          ! 4: scaling with edge-length - used for XPP2
  BiharmonicViscosity_reference              =  2.7E-2     ! [m2/s] horizontal viscosity coefficient for velocity scaled
!
  HarmonicViscosity_scaling                  = 1
  HarmonicViscosity_reference                = 0.0         ! [m2/s] constant horizontal viscosity coefficient for velocity
  HarmonicViscosity_background               = 0.0
!
  TracerHorizontalDiffusion_scaling          = 1
  Temperature_HorizontalDiffusion_Background = 0.0
  Temperature_HorizontalDiffusion_Reference  = 0
  Salinity_HorizontalDiffusion_Background    = 0.0
  Salinity_HorizontalDiffusion_Reference     = 0
/
&ocean_vertical_diffusion_nml
  vert_mix_type                              = 2           ! 1: PP; 2: TKE
  PPscheme_type                              = 0           ! 4: latest PPscheme - 0: switched off
!
!  Parameters for TKE-type vertical mixing
  ! cvmix/tke parameters
  c_k                                        = 0.1
  c_eps                                      = 0.7
  alpha_tke                                  = 30.0
  mxl_min                                    = 1.d-8
  kappaM_min                                 = 0.0
  kappaM_max                                 = 100.0
  cd                                         = 3.75
  tke_min                                    = 1.d-6  ! 2.d-6 !  default=1.d-6, increased
  tke_mxl_choice                             = 2
  tke_surf_min                               = 1.d-4
  only_tke                                   = .TRUE.
  l_lc                                       = .TRUE.      ! Use Langmuir parameterisation (Axell, 2002)
  clc                                        = 0.15        ! Factor in the vertical velocity profile of the Langmuir
                                                           ! circulation (default clc=0.1 in Axell (2002))
  use_ubound_dirichlet                       = .FALSE.
  use_lbound_dirichlet                       = .FALSE.
/
&ocean_GentMcWilliamsRedi_nml
  GMRedi_configuration                       = 1           ! 0=cartesian diffusion; 1=GM-Redi: bolus advection + isopycnal diffusion
  tapering_scheme                            = 1
  GMRedi_usesRelativeMaxSlopes               = .FALSE.
  S_max                                      = 1.0e-3      ! 3e-5
  S_d                                        = 1.0e-4      ! 1e-3 to 1e-4
                                                           !
  k_tracer_GM_kappa_parameter                =  400.0      !
  k_tracer_isoneutral_parameter              =  400.0      ! value for cell-based cartesian diffusion - mpiom: 1000/400km = 400/160km
  k_tracer_dianeutral_parameter              = 0.0         ! 1.0E-5  !
                                                           !
  switch_off_diagonal_vert_expl              = .TRUE.
  gmredi_combined_diagnostic                 = .FALSE.
! switch_on_redi_balance_diagnostic          = .FALSE.     ! not yet available in icon-aes-dyamond++
  revert_vertical_recon_and_transposed       = .TRUE.
  slope_calc_via_temperture_salinity         = .TRUE.
  include_slope_squared_implicit             = .TRUE.      ! think of l_with_vert_tracer_diffusion
  switch_on_tapering_horizontal_diffusion    = .TRUE.
/
&ocean_physics_nml
  i_sea_ice                                  = 1           ! 0 = no sea ice; 1 = sea ice model on; default=1
  lhamocc                                    = ${lhamocc}
  lbgcadv                                    = ${lbgcadv}
/
&sea_ice_nml
  i_ice_therm                                = 1           ! 1=zero-layer (default), 2=Winton, 0/2: not allowed
  i_ice_dyn                                  = 1           ! 1/0=switch on/off AWI ice dynamics
  !i_Qio_type                                 = 3           ! 3 (default): energy of whole grid-area used for melting (MPIOM-type)
  !use_constant_tfreez                        = .TRUE.      ! default: TRUE
  !use_no_flux_gradients                      = .FALSE.     ! default: TRUE
  !hmin                                       = 0.05        ! default: 0.05 - minimum sea-ice value for ice
  stress_ice_zero                            = .FALSE.     ! default: TRUE
  leadclose_1                                = 0.25        ! default: 0.5 - value of MPIOM: 0.25
  leadclose_2n                               = 0.0 ! 0.666 ! default: 0.0 - value of MPIOM: 2/3
  albedoW_sim                                = 0.10        ! albedo of the ocean used in sea ice model
  albs                                       = 0.81 ! 0.80 ! Albedo of snow (not melting) ! 0.81 hel24182
  albsm                                      = 0.77 ! 0.65 ! Albedo of snow (melting)     ! 0.77 hel24182
  albi                                       = 0.70        ! Albedo of ice (not melting)
  albim                                      = 0.68 ! 0.65 ! Albedo of ice (melting)
  !pstar                                      = 40000.      ! default: 27500.; MPIOM=20000.
  luse_replacement_pressure                  = .TRUE.      ! hel24182 - def: F
  delta_min                                  = 2.0e-9      ! hel24182 - def: 2.e-11
  cd_io                                      = 5.5e-3      ! hel24182 - def: 3.0e-3
/
&ocean_forcing_nml
  iforc_oce                                  = 14          ! ocean forcing: 14 from coupling via YAC
  type_surfRelax_Temp                        = -1          ! -1: use net surface heat flux from atmosphere
                                                           !  0: no relaxation used
                                                           !  1: relaxation switched on for reading (init_oce_relax=1)
                                                           !     or some testcases only
  forcing_enable_freshwater                  = .TRUE.      ! enable/disable freshwater flux
  forcing_windstress_u_type                  = 2           ! 0: zero wind stress, 1: read from file, 2: none
  forcing_windstress_v_type                  = 2           ! 0: zero wind stress, 1: read from file, 2: none
                                                           ! salt-change due to internal fluxes only
  limit_seaice                               = .TRUE.      ! default: TRUE
  seaice_limit                               = ${limitice} ! hard limit set to factor of flat upper layer thickness for sea ice
  lfix_salt_content                          = .TRUE.
  !NB this is an option to run with hd not working
  limit_elevation                            = .FALSE.     ! true: adjust daily to zero, default=false: free surface
! lswr_jerlov                                = .FALSE.     ! SW-penetration (Jerlov-radiation, default=T)
/
EOF

  if [[ "${initialiseOcean}" == "fromRestart" ]] || [[ "${initialiseOcean}" == "fromRestartwithHamocc" ]]; then
cat >> ${oce_namelist} << EOF
&ocean_initialConditions_nml
  initial_salinity_type                      = 0                ! 0: none, 1: read S from initial_state.nc
  initial_temperature_type                   = 0                ! 0: none, 1: read T from initial_state.nc
  !sea_surface_height_type                    = 0                ! >= 200 sea_surface_height
  !initial_velocity_type                      = 0
  initialize_fromRestart                     = .TRUE.
/
EOF
  elif [[ "${initialiseOcean}" == "fromClimatology" ]]; then
cat >> ${oce_namelist} << EOF
&ocean_initialConditions_nml
  initial_salinity_type                      = 1                ! 0: none, 1: read S from initial_state.nc
  initial_temperature_type                   = 1                ! 0: none, 1: read T from initial_state.nc
  !sea_surface_height_type                    = 1               ! not for reading T and S from ts_phc!
  !initial_velocity_type                      = 1
  initialize_fromRestart                     = .FALSE.
/
EOF
  else
      cat >> ${oce_namelist} << EOF
&ocean_initialConditions_nml
  initial_salinity_type                      = 0                ! 0: none, 1: read S from initial_state.nc
  initial_temperature_type                   = 0                ! 0: none, 1: read T from initial_state.nc
  sea_surface_height_type                    = 0                ! 0: no reset
  initial_velocity_type                      = 0                ! 0: no reset
  initialize_fromRestart                     = .FALSE.
/
EOF
  fi

  cat >> ${oce_namelist} << EOF
&ocean_diagnostics_nml
  diagnostics_level                          = 1
  diagnose_for_horizontalVelocity            = .FALSE.
  diagnose_for_heat_content                  = .TRUE.
/
&io_nml
  restart_file_type                          = 5
  !write_last_restart                         = .TRUE.           ! def=F; T: force writing restart at end of job
  restart_write_mode                         = "joint procs multifile"  ! not yet available in ocean model
  lnetcdf_flt64_output                       = .FALSE.          ! T: 64 bit output in all files
  lkeep_in_sync                              = .TRUE.           ! sync after each timestep
/
EOF
}

# ================================================================
# Ocean output

output_oce_fx(){
  stream="${EXPNAME}_oce_fx"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
 &output_nml
  filetype         =  5                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  filename_format  = "${stream}/${stream}_<datetime2>"
  output_start     = "${start_fx}"                    ! start date in ISO-format
  output_end       = "${start_fx}"                    ! end date in ISO-format
  output_interval  = "${oce_output_interval}"         ! interval in ISO-format
  file_interval    = "${oce_file_interval}"           ! interval in ISO-format
  output_grid      = .TRUE.
  mode             =  1                               ! 1: forecast mode (relative t-axis); 2: climate mode
  include_last     = .FALSE.
  !ml_varlist       =  'wet_c','basin_c','regio_c','lsm_ctr_c'
  ml_varlist       = 'lsm_ctr_c','lsm_c','lsm_e','surface_cell_sea_land_mask','surface_edge_sea_land_mask',
                     'surface_vertex_sea_land_mask','vertex_bottomLevel','basin_c','regio_c','bottom_thick_c',
                     'bottom_thick_e','column_thick_c','column_thick_e','wet_c','wet_e','wet_halo_zero_c',
                     'wet_halo_zero_e','prism_thick_c','invConstantPrismThickness','prism_volume','prism_thick_e',
                     'prism_thick_flat_sfc_c','prism_thick_flat_sfc_e','inverse prism_thick_c','prism_center_dist_c',
                     'constantPrismCenters_Zdistance','constantPrismCenters_invZdistance','inv_prism_thick_e',
                     'inv_prism_center_dist_c','inv_prism_center_dist_e','depth_CellMiddle'
/
EOF
}

output_oce_def(){
  stream="${EXPNAME}_oce_def"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  filetype                   = 5
  filename_format            = "${stream}/${stream}_<datetime2>"
  output_start               = "${start_date}"                  ! start in ISO-format
  output_end                 = "${end_date}"                    ! end in ISO-format
  output_interval            = "${oce_output_interval_def}"     ! interval in ISO-format
  file_interval              = "${oce_file_interval}"
  mode                       = 1                                ! 1: forecast mode (relative t-axis)
                                                                ! 2: climate mode (absolute t-axis)
  include_last               = .FALSE.
  output_grid                = .TRUE.
  operation                  = "mean"
  !ml_varlist                 = 'group:oce_default', 'group:oce_essentials'
  ml_varlist                 = 'group:oce_default', 'group:oce_essentials','A_veloc_v','A_tracer_v_to'
/
EOF
}

output_oce_ice(){
  stream="${EXPNAME}_oce_ice"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  output_start     = "${start_date}"                  ! start date in ISO-format
  output_end       = "${end_date}"                    ! end date in ISO-format
  output_interval  = "${oce_output_interval}"         ! interval in ISO-format
  file_interval    = "${oce_file_interval}"           ! interval in ISO-format
  mode             =  1                               ! 1: forecast mode (relative t-axis), 2: climate mode (absolute t-axis)
  operation        = 'mean'                           ! mean over output interval
  include_last     = .FALSE.                          ! set to false for asynchron output
  output_grid      = .TRUE.
  filename_format  = "${stream}/${stream}_<datetime2>"
  filetype         =  5                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  !m_levels         = "1...9,23,30,34"                 ! surface and some subsurface levels only
  !ml_varlist       =  draftave, hi, hs, conc, to, so, mld, Qtop, Qbot, u, v, condep ${STRETCH_C}
  m_levels         = "1...10,${levidx_100m},${levidx_200m},${levidx_2000m}"  ! surface and 200, 1000, 2000m levels only
  ml_varlist       =  'draftave','hi','hs','conc','verticallyTotal_mass_flux_e', !'ice_u','ice_v',
                      'to','so', 'mlotst', 'zos', 'Qtop', 'Qbot', 'u', 'v', 'condep' ${STRETCH_C}
/
EOF
}

output_oce_mon(){
  stream="${EXPNAME}_oce_mon"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  filetype                   = 4
  filename_format            = "${stream}/${stream}_<datetime2>"
  output_start               = "${start_date}"                  ! start in ISO-format
  output_end                 = "${end_date}"                    ! end in ISO-format
  !output_interval            = "${mon_output_interval}"
  output_interval            = "${oce_output_interval}"
  file_interval              = "${oce_file_interval}"
  mode                       = 1                                ! 1: forecast mode (relative t-axis)
                                                                ! 2: climate mode (absolute t-axis)
  include_last               = .FALSE.
  output_grid                = .FALSE.
  operation                  = "mean"
  ml_varlist                 = 'group:ocean_monitor'
/
EOF
}

output_oce_moc(){
  stream="${EXPNAME}_oce_moc"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  filetype                   = 5
  output_filename            = "${EXPNAME}_oce_moc"
  filename_format            = "<output_filename>_<datetime2>"
  output_start               = "${start_date}"                  ! start in ISO-format
  output_end                 = "${end_date}"                    ! end in ISO-format
  filename_format            = "${stream}/${stream}_<datetime2>"
  mode                       = 1                                ! 1: forecast mode (relative t-axis)
                                                                ! 2: climate mode (absolute t-axis)
  include_last               = .FALSE.
  output_grid                = .FALSE.
  operation                  = "mean"
  ml_varlist                 = 'group:ocean_moc'
/
EOF
}

output_oce_dbg(){
  stream="${EXPNAME}_oce_dbg"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  output_start     = "${start_date}"                  ! start date in ISO-format
  output_end       = "${end_date}"                    ! end date in ISO-format
  output_interval  = "${oce_output_interval}"         ! interval in ISO-format
  file_interval    = "${oce_file_interval}"           ! interval in ISO-format
  mode             =  1                               ! 1: forecast mode (relative t-axis), 2: climate mode (absolute t-axis)
  !operation        = 'mean'                           ! mean over output interval
  include_last     = .FALSE.                          ! set to false for asynchron output
  output_grid      = .TRUE.
  filename_format  = "${stream}/${stream}_<datetime2>"
  filetype         =  5                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  ml_varlist       =  'zos'
/
EOF
}

output_oce_zos(){
  stream="${EXPNAME}_oce_zos"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  output_start     = "${start_date}"                  ! start date in ISO-format
  output_end       = "${end_date}"                    ! end date in ISO-format
  filename_format  = "${stream}/${stream}_<datetime2>"
  mode             =  1                               ! 1: forecast mode (relative t-axis), 2: climate mode (absolute t-axis)
  !operation        = 'mean'                           ! mean over output interval
  include_last     = .FALSE.                          ! set to false for asynchron output
  output_grid      = .TRUE.
  output_filename  = "${EXPNAME}_oce_zos"
  filename_format  = "<output_filename>_<datetime2>"
  filetype         =  5                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  ml_varlist       =  'zos'
/
EOF
}

output_oce_ssh(){
  stream="${EXPNAME}_oce_ssh"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  output_start     = "${start_date}"                  ! start date in ISO-format
  output_end       = "${end_date}"                    ! end date in ISO-format
  output_interval  = "${oce_output_interval}"         ! interval in ISO-format
  file_interval    = "${oce_file_interval}"           ! interval in ISO-format
  mode             =  1                               ! 1: forecast mode (relative t-axis), 2: climate mode (absolute t-axis)
  !operation        = 'mean'                           ! mean over output interval
  include_last     = .FALSE.                          ! set to false for asynchron output
  output_grid      = .FALSE.
  filename_format  = "${stream}/${stream}_<datetime2>"
  filetype         =  4                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  ml_varlist       =  'ssh_global'
/
EOF
}

output_oce_grib(){
  stream="${EXPNAME}_oce_grb"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  filetype                   = 2
  filename_format            = "${stream}/${stream}_<datetime2>"
  output_start               = "${start_date}"                  ! start in ISO-format
  output_end                 = "${end_date}"                    ! end in ISO-format
  output_interval            = "${oce_output_interval_def}"     ! interval in ISO-format
  file_interval              = "${oce_file_interval}"
  mode                       = 1                                ! 1: forecast mode (relative t-axis)
                                                                ! 2: climate mode (absolute t-axis)
  include_last               = .FALSE.
  output_grid                = .TRUE.
  ml_varlist                 = 'zos','to','so','u','v','hi','hs','conc','column_thick_c','rho','rhopot','delhi','ice_u','ice_v'
/
EOF
}
