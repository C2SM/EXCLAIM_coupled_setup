#!/bin/bash

atm_inputs(){
    ln -sf "${atmos_grid_folder}/${atmos_grid_target}" .

    # NOTE: From add_required_atmo_non-hydrostatic_files
    cp "${basedir}/data/rrtmg_lw.nc" .
    cp "${basedir}/data/rrtmg_sw.nc" .
    cp "${basedir}/data/ECHAM6_CldOptProps.nc" .
    cp "${basedir}/data/dmin_wetgrowth_lookup.nc" .   #for inwp_gscp==4
    cp "${basedir}/data/pmod.txt" .
    ln -sf "${basedir}/externals/ecrad/data" ecrad_data

    revision="rcscs"
    datadir_aerosol_kinne="${atmo_data_InputFolder}/aerosol_kinne/${revision}"
    datadir_aerosol_volcanic="${clim_data_poolFolder}/atmo/common/aerosol_volcanic_cmip6"
    datadir_plumes="${clim_data_poolFolder}/atmo/common/MACv2_simple_plumes_merged"
    datadir_ozone="${atmo_data_InputFolder}/ozone/${revision}"
    datadir_rad="${clim_data_poolFolder}/atmo/common/solar_radiation"
    datadir_ghg="${clim_data_poolFolder}/atmo/common/greenhouse_gases"
    datadir_hd="${icon_data_poolFolder}/atmo/${atmos_gridID}-${ocean_gridID}/hd/r0100"

    case "${exptype}" in
        "control")
            #aerosols - irad_aero=12 - 2 common files + annual file
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_${control_year}.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            #ozone constant historical value
            for ((year=start_year-1; year<=end_year; year++)); do 
                ln -sf "${datadir_ozone}/bc_ozone_historical_${control_year}.nc" ./"bc_ozone_${year}.nc"
            done
            #solar irradiance - from file with constant annual values. This file must be made for each control year. see datadir_rad
            ln -sf "${datadir_rad}/swflux_14band_cmip6_${control_year}ADconst_999-2301-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            #GHGs from ighg1=2 and ighg2=3 - use values from vmr_{ghg} in radiation_nml
            ;;
        "picontrol")
            #aerosols - irad_aero=12 - 2 common files + annual file
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_1850.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            #ozone 1850 constant
            for ((year=start_year-1; year<=end_year; year++)); do 
                ln -sf "${datadir_ozone}/bc_ozone_historical_1850.nc" ./"bc_ozone_${year}.nc"
            done
            #solar irradiance
            ln -sf "${datadir_rad}/swflux_14band_cmip6_1850ADconst_999-2301-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            #GHGs from ighg1=2 and ighg2=3 - use values from vmr_{ghg} in radiation_nml
            ;;
        *) #transient
            #aerosols irad_aero=18 # background aero. from Kinne + volcanic aero. + anthropogenic aero. from Simple Plume
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_1850.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            # add volcanic aerosols #NOTE: no projections yet
            for ((year=start_year-1; year<=end_year; year++)); do
                if [[ $year -eq 1849 ]]; then
                    ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_1850.nc" ./bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc
                elif [[ $year -le 2014 ]]; then
                    ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc" ./bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc
                else
                    echo "WARNING: volcanic aerosols will use constant 2014 values"
                    ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_2014.nc" ./bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc
                fi
            done
            #add anthropogenic aerosols from simple plumes
            #TODO add ssp
            ln -sf "${datadir_plumes}/MACv2.0-SP-merged-historical-and-SSP3-70_v1.nc" ./MACv2.0-SP_v1.nc
            #ozone depending on year
            for ((year=start_year-1; year<=end_year; year++)); do
                if  ((year < 2014)) ; then  # historical
                    ln -sf "${datadir_ozone}/bc_ozone_historical_${year}.nc" ./bc_ozone_"${year}".nc
                else  # NOTE: no projections yet
                    echo "WARNING: ozone will use constant 2014 values"
                    ln -sf "${datadir_ozone}/bc_ozone_historical_2014.nc" ./bc_ozone_"${year}".nc
                    #ln -sf "${datadir_ozone}/bc_ozone_ssp${ssp}_${year}.nc" ./bc_ozone_"${year}".nc
                fi
            done
            #solar irradiance
            ln -sf ${datadir_rad}/swflux_14band_cmip6_1849-2299-v3.2.nc ./bc_solar_irradiance_sw_b14.nc
            #GHGs with ighg1=4 and 1ghg2=4 - greenhouse gases from external file
            # TODO: implement current_year to have historical, projections, and different scenarios. For now, historic+ssp245.
            ${datadir_ghg}/greenhouse_historical_plus.nc ./bc_greenhouse_gases.nc
            ;;
    esac
    #shorter and lowercase grid refinement for the hd files
    aref_very_short="${atmos_refinement//"0"}"
    aref_vs_lc="${aref_very_short,,}"

    ln -sf "${datadir_hd}/hdpara_${aref_vs_lc}_${atmos_gridID}_${ocean_gridID}_mc_maxl_s_v1.nc" ./bc_land_hd.nc   #  mask for routing_scheme='full'
    ln -sf "${datadir_hd}/hdstart_${aref_vs_lc}_${atmos_gridID}_${ocean_gridID}_mc_maxl_s_v1.nc" ./ic_land_hd.nc
}

oce_inputs(){
    ln -sf ${ocean_grid_folder}/${ocean_grid_target} .

    case "${initialiseOcean}" in
        "fromClimatology")
            ln -sf "${ocean_grid_folder}/ocean/initial_conditions/rcscs/tsi_oras5_icon_icon_grid_${ocean_gridID}_${ocean_refinement}_O_${ocean_vertical_levels}.nc_${start_year}-01-01" ./initial_state.nc
            ;;
        "fromRestart")
            rm -f ./multifile_restart_ocean.mfr
            ln -sf ${restart_dir}/${restart_exp}_restart_oce_${restart_date}T000000Z.nc ./multifile_restart_ocean.mfr
            ;;
        *)
            echo "ERROR: initialiseOcean mode not supported (yet?): ${initialiseOcean}"
            ;;
    esac

    if [[ "${use_hamocc}" == "yes" ]]; then
        echo "ERROR: hammoc not supported"
        exit 1
        #datadir="/work/mh0727/m300732/input/0036/ocean/hamocc"
        #ln -sf "${datadir}/MAHOWALDDUST_icon_grid_0036_R02B04_O_remapbil1.nc" ./dust.nc  # iron deposition
        #ln -sf "${datadir}/ndepo_1-0_gr_185001-185012-clim_icon_grid_0036_R02B04_O.nc" ./nitrogen.nc  # nitrate deposition
    fi
}

lnd_inputs(){
    datadir_land="${icon_data_poolFolder}/atmo/${atmos_gridID}-${ocean_gridID}/land/rcscs"
    datadir_init="${atmo_data_InputFolder}/initial_conditions/rcscs"
    
    #edit atmos_refinement for ifs_fname
    short_atmos_refinement=$(echo "${atmos_refinement}" | cut -d "0" -f1)$(echo "${atmos_refinement}" | cut -d "0" -f2-)
    #used to initialize jsbach from ifs data
    ifs_fname="ifs2icon_${short_atmos_refinement}_DOM01.nc"  # NOTE: do not change name
    
    # TODO: add picontrol mechanism
    ifs_origin_name="ifs2icon_${start_year}010100_${atmos_gridID}_${atmos_refinement}_G.nc"
    ln -sf "${datadir_init}/${ifs_origin_name}" "${ifs_fname}"

    ln -sf "${basedir}/externals/jsbach/data/lctlib_nlct21.def" .

    # choose year for land fraction file based on exptype
    if [[ "$exptype" == "control"  ]]; then
        year="${control_year}"  # constant historical value
    elif [[ "$exptype" == "picontrol"  ]]; then
        year="1850"  # preindustrial constant
    else
        # FIXME: year mechanism not implemented yet, this will fail
        # NOTE: restart every year max to get the right land frac
        year="${current_year}" # transient
    fi
    jsbach_lnd_frac="bc_land_frac_11pfts_${year}.nc"
    ln -sf "${datadir_land}/${jsbach_lnd_frac}" .

    # jsbach land data:
    jsbach_bc_phys="bc_land_phys.nc"
    jsbach_bc_soil="bc_land_soil.nc"
    jsbach_sso="bc_land_sso.nc"
    # - jsbach soil init data
    jsbach_ic_soil="ic_land_soil.nc"

    ln -sf "${datadir_land}/${jsbach_bc_phys}" .
    ln -sf "${datadir_land}/${jsbach_bc_soil}" .
    ln -sf "${datadir_land}/${jsbach_ic_soil}" .
    ln -sf "${datadir_land}/${jsbach_sso}" .

    extpar_filename="${datadir_land}/icon_extpar4jsbach_${atmos_gridID}_20250509_tiles_jsb.nc"
    extpar_targetname="extpar_icon_grid_${atmos_gridID}_${atmos_refinement}_G.nc"
    ln -sf "${extpar_filename}" "${extpar_targetname}"
}
