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
  cat > ./coupling.yaml << EOF
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
          FLHEIGHT=35000. #! Height above whihc coordinatre surfaces are flat  (default value)
          ;;
      "120")
          MAXLAYTH=400.
          TOPTHLIM=15000.
          STRETFAC=0.9
          FLHEIGHT=25000.
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
 !nblocks_c               = 0
 !nblocks_e               = 1
 nproma_sub              = ${nproma_sub}
 p_test_run              = .false.
 !l_fast_sum              = .false.
 l_test_openmp           = .false.
 l_log_checks            = .false.
 num_io_procs            = ${ATM_IO_TASKS}
 pio_type                = 1                   ! default 1: asynchron io
 num_restart_procs       = ${ATM_RST_TASKS} ! number of procs for multifile restart
 num_dist_array_replicas = ${replicate_grid-1} ! can be 1 iff passive or active (see HAVE_SLOW_PASSIVE_TARGET_ONESIDED) target RMA works well
 io_proc_chunk_size      = 12              ! Used for Large Data writing requiring large memory (eg., 3D files)
 iorder_sendrecv         = 3               ! From CLM namelist  (isend/irec)
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
&initicon_nml
 ! initialization mode (2 for IFS ana, 1 for DWD ana, 4=cosmo, 2=ifs, 3=combined 
 !NB: addition from dyamond. Suspicious that this may break the HD budget as wwe saw previously
 init_mode               = 2
 ifs2icon_filename       = "${ifs_fname}"
 zpbl1                   = 500.    ! NEW Works  !(CLM) bottom height (AGL) of layer used for gradient computation
 zpbl2                   = 1000.   ! NEW Works    !(CLM) top height (AGL) of layer used for gradient computation
 ltile_init              =.true.   ! NEW Works   !(CLM) True: initialize tiled surface fields from a first guess coming from a run without tiles.
 ltile_coldstart         =.true.   ! NEW Works  ! (CLM) If true, tiled surface fields are initialized with tile-averaged fields from a previous run with tiles.
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
 timers_level            = 11
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
 itype_vn_diffu          = 1        ! (u,v reconstruction atvertices only)  Default of CLM
 itype_t_diffu           = 2        ! (Discritization of temp diffusion, default value of CLM)
 hdiff_efdt_ratio        = 32.0     ! Ratio iof e-forlding time to time step, recomemded values above 30 (CLM value)
 hdiff_smag_fac          = 0.025
 lhdiff_vn               = .TRUE.
 lhdiff_temp             = .TRUE.
 !hdiff_multfac           = 1.0
 !hdiff_tv_ratio          = 1.0
/
&nwp_phy_nml
 !icalc_reff              = 100         ! For Modis cdnc
 !icpl_rad_reff           = 0           ! For Modis cdnc
 inwp_gscp               = 2
 inwp_convection         = 0
 inwp_radiation          = 4
 inwp_cldcover           = 1
 inwp_satad              = 1
 inwp_sso                = 0
 inwp_gwd                = 0
 latm_above_top          = .false.
 efdt_min_raylfric       = 7200.
 itype_z0                = 2
 icapdcycl               = 3
 icpl_aero_conv          = 0           ! 1 if irad_aero=6 or 9 is used - 0 for irad_aero=12
 icpl_aero_gscp          = 0           ! 1 if irad_aero=6 or 9 is used - 0 for irad_aero=12
 !icpl_o3_tp              = 1
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
 ldetrain_conv_prec = .false.   ! Detraintment of convective rain and snow. (for inwp_convection = 1)
/
&nwp_tuning_nml
 !tune_rprcon             = 1.0e-3
 itune_albedo            = 0                         ! 0: no MODIS tuning (def); 1: dimmed Sahara; 2: +brighter AA
 !tune_zceff_min          = 0.01                     ! ** default value (0.01) to be used for R3B7; use 0.025 for R2B6
 tune_gkdrag             = 0.075                     ! R2B6: 0.075
 tune_gkwake             = 1.5                       ! R2B6: 1.5
 tune_gfrcrit            = 0.425                     ! R2B6: 0.425
 !tune_grcrit             = 0.25
 !tune_grcrit_enh         = 0.25
 !tune_dust_abs           = 0.
 tune_box_liq_asy        = 3.25    ! 3.5              ! oper global: 3.0 , oper D2: 3.25, default: 2.5
 !tune_box_liq            = 0.07
 tune_rcucov             = 0.075
 tune_rhebc_land         = 0.825
 tune_gust_factor        = 7.0
 !icpl_turb_clc           = 1
 !lcalib_clcov            = .false.                   ! turn off TCC, HCC, MCC, LCC tuning
 !tune_eiscrit            = 7.0                       ! to switch off conv param in stratocumulus regions
 !tune_sc_eis             = 7.             ! ! default: 1000. - exec newer 633d375ad0
 !tune_zvz0i              = 0.9    ! 1.2 !  0.6      ! default: 0/.85   ; Terminal fall velocity of ice
 !tune_entrorg            = 1.75e-3        !  3.0e-3   ! default: 1.95e-3; Entrainment parameter valid for dx=20 km
 !tune_sc_eis             = 7.            !  7.       ! default: 1000. - exec newer 633d375ad0
/
&transport_nml
 ivadv_tracer            = 3,3,3,3,3,3   ! (AD recomendaiton)gdm: 52 combination of hybrid FFSL/Miura3 with subcycling
 itype_hlimit            = 4,4,4,4,4,4
 ihadv_tracer            = 2,2,2,2,2,2
 itype_vlimit            = 1,1,1,1,1,1
 ivlimit_selective       = 1,1,1,1,1,1
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
 damp_height             = 30000.                    ! AD recomendation (rayeigh damping starts at this lelev in meters)
 rayleigh_coeff          = 0.5                       ! default: 0.05
 divdamp_order           = 24                        ! 2 ass, 24 fc
 divdamp_type            = 32                        ! optional: 2 assimilation cycle, 32 forecast
 divdamp_fac             = 0.004                     ! 0.004 for R2B6; recommendation for R3B7: 0.003
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
 top_height          = 85000.                         ! model top AD recomendation
 min_lay_thckn       = 50.                            ! Layer thickness of lowermost layer (CLM recommendation)
 decay_scale_1       = 4000.
 decay_scale_2       = 2500.
 decay_exp           = 1.2
 !max_lay_thckn       = ${MAXLAYTH}                    ! maximum layer thickness below htop_thcknlimit
 htop_thcknlimit     = ${TOPTHLIM}                    ! top limit for max_lay_thckn
 stretch_fac         = ${STRETFAC}
 flat_height         = ${FLHEIGHT}
/
&io_nml
 lflux_avg               = .true.                   ! true: averaged (ashfl_s), false: accumulated fluxes (accshfl_s)
 itype_pres_msl          = 5                         ! (1) 3: IFS-type extrapolation
 itype_rh                = 1                         ! (1) 2: mixed phase (water and ice)
 inextra_3d              = 2                         ! 3D extra variables
 inextra_2d              = 10                        ! 2D extra variables
 restart_write_mode      = "joint procs multifile"   ! asynchron multifile restart handling; 'sync' for single file writing
 lkeep_in_sync           = .TRUE.                    ! sync after each timestep
 lnetcdf_flt64_output    = .FALSE.                   ! T: 64 bit output in all files
 precip_interval         = "PT15M"   ! NEW ! Works The precipitation value is accumulated in these interval otherwise accumulated fromm begining of the run
 runoff_interval         = "PT3H"    ! NEW ! Works The runoff is accumalted in this inetrval else accumulated from bengining.
 maxt_interval           = "PT3H"    ! NEW ! Works Interval at which Max/Min 2m temperture are calculated
 melt_interval           = "PT3H"    ! NEW ! Works CLM community has this , Can not find discription
 lmask_boundary          = .true.    ! NEW ! Works if interpolation zone should be masked in triangular output.
 dt_hailcast             = 900.
/
&dbg_index_nml
  idbg_mxmn              = 1                          ! initialize MIN/MAX  debug output
  idbg_val               = 0                          ! initialize one cell debug output
  idbg_slev              = 1                          ! initialize start level for debug output
  idbg_elev              = 3                          ! initialize start level for debug output
/
&extpar_nml
 itopo                   = 1
 n_iter_smooth_topo      = 1
 heightdiff_threshold    = 3000.
 !pp_sso                 = 2 !NB this is in tpp runscript but mentions MERIT REMA topography, which is not used here
 hgtdiff_max_smooth_topo = 750.    ! RMS height difference to neighbor grid points at which the smoothing pre-factor fac_smooth_topo reaches its maximum value (CLM value)
 itype_vegetation_cycle  = 3       ! NEW  (CLM value , but not defined. Annual cycle of Leaf Area Index, use T2M to get realistic values)
 !itype_lwemiss           = 2      !NB: not included in extpar ! NEW  Type of data for Long wave surfae emissitvity (Read from monthly climatologoies from expar file)
/
#many removed, final 4 added to match DRAGON namelists
&lnd_nml
 !nlev_snow               = 3
 !lmulti_snow             = .false.
 !itype_heatcond          = 3
 !idiag_snowfrac          = 20
 !lsnowtile               = .true.
 !llake                   = .true.
 !itype_lndtbl            = 4
 !itype_evsl              = 4
 !itype_trvg              = 3
 !itype_root              = 2
 !cwimax_ml               = 5.e-4
 !c_soil                  = 1.25
 !c_soil_urb              = 0.5
 !itype_snowevap          = 2
 lprog_albsi             = .true.      ! default = F
 lseaice                 = .true.
 hice_min                = 0.05        ! default of nwp sea-ice model - 0.05 in sea-ice model of icon-o
 hice_max                = 10.0        ! must correspont to seaice_limit in icon-o in % of upper layer thickness
 sstice_mode             = 1           ! 1 for coupled
 !                                     ! parameters for jsbach land model:
 ntiles                  = 1           ! 1 for jsbach, 3 for terra?
 !albsi_snow_max = 0.81 ! Maximum albedo of snow over sea ice
 !albsi_snow_min = 0.77 ! Minimum albedo of snow over sea ice
 !albsi_max      = 0.70 ! Maximum albedo of sea ice
 !albsi_min      = 0.68 ! Minimum albedo of sea ice
/
&turb_vdiff_nml
 fsl                     = 0.8         ! for lowest model layer thickness of 20m
 pr0                     = 1.0        ! default: 0.68; neutral limit Prandtl number (0.6 to 1.0)
 f_theta_limit_fraction  = 0.1
 f_theta_decay           = 4.0         ! default: 4.0
 ek_ep_ratio_stable      = 2.33        ! default: 3 - Mauritzen: 1/(0.3 +- 0.1) -1 = 1.5 to 4
 ek_ep_ratio_unstable    = 1.0         ! default: 2 - Mauritzen: 1
/
&radiation_nml
 !isolrad                 = 1           ! 1: SSI from Coddington (def); 2: SSI monthly mean time series from file
 irad_o3                 = 5           ! 0: no ozon (def); 5: transient 3-dim; 79: GEMS/MACC #NB was running with 79
 irad_aero               = ${irad_aero}! NB to test
 ghg_filename            = './bc_greenhouse_gases.nc'
 irad_co2                = ${ighg1}
 irad_ch4                = ${ighg2}
 irad_n2o                = ${ighg2}
 irad_cfc11              = ${ighg1}
 irad_cfc12              = ${ighg1}
 !zenith                  = 4           ! 4: NWP (def), 3: no annual cycle
 irad_o2                 = 2

 !NB update to year of simulation - O3
 vmr_co2                 = 336.6e-06   ! values for 1979 CE ! 284.3e-06   ! values for 1850 CE
 vmr_ch4                 = 1566.2e-09  ! values for 1979 CE ! 808.2e-09   ! values for 1850 CE
 vmr_n2o                 = 300.4e-09   ! values for 1979 CE ! 273.0e-09   ! values for 1850 CE
 vmr_o2                  = 0.20946     ! preindustrial
 vmr_cfc11               = 157.6e-12   ! values for 1979 ! 0.0 ! preindustrial
 vmr_cfc12               = 286.5e-12   ! values for 1979 ! 0.0 ! preindustrial
 !NB double check file
 albedo_type             = 1          ! 1: dry soil (def); 2: Modis albedo
 direct_albedo           = 4
 !direct_albedo_water     = 3
 albedo_whitecap         = 1
 !ecrad_llw_cloud_scat    = .true.
 ecrad_data_path         = 'ecrad_data'
 ecrad_isolver           = ${ecrad_isolver}
 !decorr_pole             =  780        ! default: 2000
 !decorr_equator          = 2000        ! default: 2000
/
&ccycle_nml
  ccycle_config%iccycle  = 2            ! 0: vmr_co2=384 for jsbach (def); 2: ccycle namelist values used
  ccycle_config%ico2conc = ${ighg1}     ! 2: use vmr_co2 of ccycle; 4: use values of GHG file
  ccycle_config%vmr_co2  = 336.6e-06    ! 284.3e-06    ! same value as in radiation_nml
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
  # interpolated lat-lon output 2d and 3d:
  stream="${EXPNAME}_atm_latlon"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml       !! interpolated lat-lon output
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "P1D"  ! the output interval and
 file_interval           = "P1M"    ! the file interval
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    = 1                         ! 1: forecast mode (relative t-axis)
 include_last            = .FALSE.                   ! set to false for asynchron output
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 1                         ! 1: latlon,  0: native grid
 reg_lon_def             = 0.,1.,360.
 reg_lat_def             = -90.,1.,90.
 !operation               = 'mean'                   !mean operation doesn't seem to work with interpolated output
 ml_varlist              = 'clct', 'tqv', 'tqc_dia', 'tqi_dia', 'sp_10m', 't_2m' ,'t_g', 'qv_2m', 'h_ice', 't_ice',
                           !'accthb_s','accthb_t','accsob_s','accsob_t','accshfl_s','acclhfl_s','accumfl_s','accvmfl_s',
                           'thb_s','thb_t','sob_s','sob_t','shfl_s','lhfl_s','umfl_s','vmfl_s',
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
  stream="${EXPNAME}_hyd_dbg"
  mkdir -p "${stream}"
  cat >> ${atm_namelist} << EOF
&output_nml
 output_start            = "${start_date}"
 output_end              = "${end_date}"
 output_interval         = "${atm_output_interval}"  ! the output interval and
 file_interval           = "${atm_file_interval}"    ! the file interval
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
 dom                     = -1
 mode                    =  1                        ! 1: forecast mode (relative t-axis); 2: climate mode
 include_last            = .FALSE.                   ! flag whether to include the last time step
 filename_format         = "${stream}/${stream}_<datetime2>"
 output_grid             = .TRUE.                    ! flag whether grid information is added to output.
 remap                   = 0                         ! 1: latlon,  0: native grid
 !operation               = "mean"                    ! works on icon grid only (remap=0)
 ml_varlist              = 'group:land_vars'

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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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
 filetype                = 5                         ! output format: 2=GRIB2, 4=NETCDFv2
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

#DYAMOND output from PPK
output_stream_1_1(){
    # FOR DYAMOND PROTOCOL # 3D Variables on native grid, 3 hourly (as per the
    # Dyamond Protocol 6 hourly), 37 pressure levels.
    # => This needs to be interpolated onto 10KM (25KM for Dyamond)
    stream="${EXPNAME}_dyamond_atm_1_1"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_1_1/${EXPNAME}_out_1_1_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 include_last    = .true.
 pl_varlist      = 'geopot','qv','rh'
 p_levels        = 100,200,300,500,700,1000,2000,3000,5000,7000,10000,12500,15000,17500,20000,22500,25000,30000,35000,40000,45000,50000,55000,60000,65000,70000,75000,77500,80000,82500,85000,87500,90000,92500,95000,97500,100000
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_1_2(){
    # FOR DYAMOND PROTOCOL # 3D Variables on native grid, 3 hourly (as per the
    # Dyamond Protocol 6 hourly), 37 pressure levels.
    # => This needs to be interpolated onto 10KM (25KM for Dyamond)
    stream="${EXPNAME}_dyamond_atm_1_2"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_1_2/${EXPNAME}_out_1_2_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 include_last    = .true.
 pl_varlist      = 'qc','qr','qi'
 p_levels        = 100,200,300,500,700,1000,2000,3000,5000,7000,10000,12500,15000,17500,20000,22500,25000,30000,35000,40000,45000,50000,55000,60000,65000,70000,75000,77500,80000,82500,85000,87500,90000,92500,95000,97500,100000
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_1_3(){
    stream="${EXPNAME}_dyamond_atm_1_3"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_1_3/${EXPNAME}_out_1_3_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 include_last    = .true.
 pl_varlist      = 'qs','qg','temp'
 p_levels        = 100,200,300,500,700,1000,2000,3000,5000,7000,10000,12500,15000,17500,20000,22500,25000,30000,35000,40000,45000,50000,55000,60000,65000,70000,75000,77500,80000,82500,85000,87500,90000,92500,95000,97500,100000
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_1_4(){
    stream="${EXPNAME}_dyamond_atm_1_4"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_1_4/${EXPNAME}_out_1_4_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 include_last    = .true.
 pl_varlist      = 'u','v','w'
 p_levels        = 100,200,300,500,700,1000,2000,3000,5000,7000,10000,12500,15000,17500,20000,22500,25000,30000,35000,40000,45000,50000,55000,60000,65000,70000,75000,77500,80000,82500,85000,87500,90000,92500,95000,97500,100000
 output_grid     = .true.
 mode            = 1
/
EOF
}


output_stream_1_5(){
    stream="${EXPNAME}_dyamond_atm_1_5"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_1_5/${EXPNAME}_out_1_5_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 include_last    = .true.
 pl_varlist      = 'omega','rho','pv','tke'
 p_levels        = 100,200,300,500,700,1000,2000,3000,5000,7000,10000,12500,15000,17500,20000,22500,25000,30000,35000,40000,45000,50000,55000,60000,65000,70000,75000,77500,80000,82500,85000,87500,90000,92500,95000,97500,100000
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_2(){
    stream="${EXPNAME}_dyamond_atm_2"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_2/${EXPNAME}_out_2_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT15M"
 file_interval   = "P1D"
 ml_varlist      = 'tot_prec','DHAIL_MX'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_3(){
    stream="${EXPNAME}_dyamond_atm_3" 
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_3/${EXPNAME}_out_3_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 ml_varlist      = 'pres_sfc','pres_msl','u_10m','v_10m','qv_2m','t_2m','tqc','tqi','tqv','tqr','h_snow','gust10'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_4(){
    stream="${EXPNAME}_dyamond_atm_4"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_4/<output_filename>${EXPNAME}_out_4_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 hl_varlist      = 'u','v','w'
 h_levels        =  10.0, 500.0, 2500, 5000, 7500
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_5(){
    stream="${EXPNAME}_dyamond_atm_5"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_5/${EXPNAME}_out_5_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 ml_varlist      = 'qhfl_s','lhfl_s','shfl_s', 'umfl_s','vmfl_s','pres_sfc','pres_msl'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_6(){
    stream="${EXPNAME}_dyamond_atm_6"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_6/${EXPNAME}_out_6_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 ml_varlist      = 'thu_s','sob_s','sob_t','sod_t','sodifd_s','thb_s','sou_s','thb_t','sobclr_s','sou_t','thbclr_s'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
    # sod_s mot found, sodifu_s not found, sodird_s not found, thd_s not found,
    # sobclr_t not found, thbclr_t not found! 'sodifu_s','sodird_s' not found
    # In the out7, we are trying to output the Radiation terms which are 3D
    # i.e., written on the model levels. (lwflx_dn_clr, lwflx_dn, lwflx_up_clr,
    # lwflx_up, lwflxall, swflx_dn_clr, swflx_dn, swflx_up_clr, swflx_up)
}


output_stream_7(){
    stream="${EXPNAME}_dyamond_atm_7"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_7/${EXPNAME}_out_7_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 ml_varlist      = 'clct','clcm','clcl','clch','qv_2m','rh_2m','t_2m','t_g','td_2m','u_10m','v_10m','sp_10m','gust10'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_8(){
    stream="${EXPNAME}_dyamond_atm_8"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_8/${EXPNAME}_out_8_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 include_last    = .true.
 ml_varlist      = 'cape_ml','cape','lcl_ml','lfc_ml','cin_ml','DBZ_CMAX','GRAUPEL_GSP'
 output_grid     = .true.
 mode            = 1
/
EOF
    # LPI and LPI_MAX do not work, as they are only ported on Reduced Grid, as MeteoSwiss uses reduced grid.
}

output_stream_9(){
    stream="${EXPNAME}_dyamond_atm_9"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_9/${EXPNAME}_out_9_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'smi','w_i','t_so','w_so','freshsnow','rho_snow','w_snow','t_s','t_g'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_10(){
    stream="${EXPNAME}_dyamond_atm_10"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_10/${EXPNAME}_out_10_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'runoff_g','runoff_s','snow_gsp','snow_melt'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_11(){
    stream="${EXPNAME}_dyamond_atm_11"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_11/${EXPNAME}_out_11_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 pl_varlist      = 'geopot','temp','u','v','qv'
 p_levels        =  20000,50000,85000
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_12(){
    stream="${EXPNAME}_dyamond_atm_12"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_12/${EXPNAME}_out_12_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'tmax_2m','tmin_2m', 'lai', 'plcov', 'rootdp',
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_13(){
    stream="${EXPNAME}_dyamond_atm_13"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_13/${EXPNAME}_out_13_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'pres'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_14(){
    stream="${EXPNAME}_dyamond_atm_14"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_14/${EXPNAME}_out_14_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT1H"
 file_interval   = "P1D"
 ml_varlist      = 'lwflx_up', 'lwflx_dn', 'swflx_up', 'swflx_dn', 'lwflx_up_clr', 'lwflx_dn_clr', 'swflx_up_clr', 'swflx_dn_clr'
 m_levels        = "1,nlev"
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
/
EOF
}

output_stream_15_1(){
    stream="${EXPNAME}_dyamond_atm_15_1"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_1/${EXPNAME}_out_15_1_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'geopot'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_2(){
    stream="${EXPNAME}_dyamond_atm_15_2"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_2/${EXPNAME}_out_15_2_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'qv'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_3(){
    stream="${EXPNAME}_dyamond_atm_15_3"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_3/${EXPNAME}_out_15_3_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'qc'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_4(){
    stream="${EXPNAME}_dyamond_atm_15_4"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_4/${EXPNAME}_out_15_4_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'qr'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_5(){
    stream="${EXPNAME}_dyamond_atm_15_5"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_5/${EXPNAME}_out_15_5_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'qi'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_6(){
    stream="${EXPNAME}_dyamond_atm_15_6"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_6/${EXPNAME}_out_15_6_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'qs'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_7(){
    stream="${EXPNAME}_dyamond_atm_15_7"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_7/${EXPNAME}_out_15_7_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'qg'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_8(){
    stream="${EXPNAME}_dyamond_atm_15_8"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_8/${EXPNAME}_out_15_8_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'temp'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_9(){
    stream="${EXPNAME}_dyamond_atm_15_9"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_9/${EXPNAME}_out_15_9_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'u'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_10(){
    stream="${EXPNAME}_dyamond_atm_15_10"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_10/${EXPNAME}_out_15_10_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'v'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_11(){
    stream="${EXPNAME}_dyamond_atm_15_11"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_11/${EXPNAME}_out_15_11_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'w'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_12(){
    stream="${EXPNAME}_dyamond_atm_15_12"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_12/${EXPNAME}_out_15_12_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'rho'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
/
EOF
}

output_stream_15_13(){
    stream="${EXPNAME}_dyamond_atm_15_13"
    mkdir -p "${stream}"
    cat >> ${atmo_namelist} << EOF

&output_nml
 filename_format = "out_15_13/${EXPNAME}_out_15_13_<datetime2>"
 filetype        = 5 ! NetCDF4
 output_start    = "${start_date}"
 output_end      = "${end_date}"
 output_interval = "PT3H"
 file_interval   = "P1D"
 ml_varlist      = 'tke'
 include_last    = .true.
 output_grid     = .true.
 mode            = 1
 m_levels        = "60...nlev"
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
 num_io_procs                = ${OCE_IO_TASKS}
 io_proc_chunk_size          = 8
 pio_type                    = 1                                ! default 1: asynchron io
 num_restart_procs           = ${OCE_RST_TASKS}                 ! number of procs for multifile restart
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
 timers_level                = 11
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
  GMRedi_configuration                       = 0           ! 0=cartesian diffusion; 1=GM-Redi: bolus advection + isopycnal diffusion
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
  output_interval  = "P50Y"         ! interval in ISO-format
  file_interval    = "P50Y"           ! interval in ISO-format
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
  output_interval            = "PT6H"     ! interval in ISO-format
  file_interval              = "P1D"
  mode                       = 1                                ! 1: forecast mode (relative t-axis)
                                                                ! 2: climate mode (absolute t-axis)
  include_last               = .FALSE.
  output_grid                = .TRUE.
  operation                  = "mean"
  m_levels         = "1...10,${levidx_100m},${levidx_200m},${levidx_500m},${levidx_1000m},${levidx_1500m},${levidx_2000m},${levidx_3000m}"  ! surface and 100, 200,500, 1000,1500, 2000m, 300m levels only
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
  filetype         = 5
  output_start     = "${start_date}"                  ! start date in ISO-format
  output_end       = "${end_date}"                    ! end date in ISO-format
  output_interval  = "PT6H"         ! interval in ISO-format
  file_interval    = "P1D"           ! interval in ISO-format
  mode             =  1                               ! 1: forecast mode (relative t-axis), 2: climate mode (absolute t-axis)
  operation        = 'mean'                           ! mean over output interval
  include_last     = .FALSE.                          ! set to false for asynchron output
  output_grid      = .TRUE.
  filename_format  = "${stream}/${stream}_<datetime2>"
  filetype         =  5                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  !m_levels         = "1...10,${levidx_100m},${levidx_200m},${levidx_500m},${levidx_1000m},${levidx_1500m},${levidx_2000m},${levidx_3000m}"  ! surface and 100, 200,500, 1000,1500, 2000m, 300m levels only
  ml_varlist       =  'draftave','hi','hs','conc','verticallyTotal_mass_flux_e', 'ice_u','ice_v',
                      'Qtop', 'Qbot'
/
EOF
}

output_oce_flux(){
  stream="${EXPNAME}_oce_flux"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  filetype         = 5
  output_start     = "${start_date}"                  ! start date in ISO-format
  output_end       = "${end_date}"                    ! end date in ISO-format
  output_interval  = "P1D"         ! interval in ISO-format
  file_interval    = "P1D"           ! interval in ISO-format
  mode             =  1                               ! 1: forecast mode (relative t-axis), 2: climate mode (absolute t-axis)
  operation        = 'mean'                           ! mean over output interval
  include_last     = .FALSE.                          ! set to false for asynchron output
  output_grid      = .TRUE.
  filename_format  = "${stream}/${stream}_<datetime2>"
  filetype         =  5                               ! output format: 2=GRIB2, 4=NETCDFv2, 5=NETCDFv4
  !m_levels         = "1...10,${levidx_100m},${levidx_200m},${levidx_500m},${levidx_1000m},${levidx_1500m},${levidx_2000m},${levidx_3000m}"  ! surface and 100, 200,500, 1000,1500, 2000m, 300m levels only
  ml_varlist       = 'HeatFlux_Total','atmos_fluxes_HeatFlux_ShortWave','atmos_fluxes_HeatFlux_LongWave',
                      'HeatFlux_ShortWave','HeatFlux_LongWave','HeatFlux_Sensible','HeatFlux_Latent',
                      'FrshFlux_Runoff','FrshFlux_Precipitation','FrshFlux_Evaporation','FrshFlux_SnowFall',
                      'FrshFlux_TotalOcean','FrshFlux_VolumeIce'
/
EOF
}

output_oce_mon(){
  stream="${EXPNAME}_oce_mon"
  mkdir -p "${stream}"
  cat >> ${oce_namelist} << EOF
&output_nml
  filetype                   = 5
  filename_format            = "${stream}/${stream}_<datetime2>"
  output_start               = "${start_date}"                  ! start in ISO-format
  output_end                 = "${end_date}"                    ! end in ISO-format
  output_interval            = "P1M"
  file_interval              = "P1M"
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
  filename_format            = "${stream}/${stream}_<datetime2>"
  output_start               = "${start_date}"                  ! start in ISO-format
  output_end                 = "${end_date}"                    ! end in ISO-format
  output_interval            = "P1M"
  file_interval              = "P1M"
  mode                       = 1                                ! 1: forecast mode (relative t-axis)
                                                                ! 2: climate mode (absolute t-axis)
  include_last               = .FALSE.
  output_grid                = .FALSE.
  operation                  = "mean"
  ml_varlist                 = 'group:ocean_moc'
/
EOF
}


