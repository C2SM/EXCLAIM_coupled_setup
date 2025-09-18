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
    datadir_aerosol_volcanic="${clim_data_poolFolder}/common/aerosol_volcanic_cmip6"
    datadir_plumes="${clim_data_poolFolder}/common/MACv2_simple_plumes_merged"
    datadir_ozone="${atmo_data_InputFolder}/ozone/${revision}"
    datadir_rad="${clim_data_poolFolder}/common/solar_radiation"
    datadir_ghg="${clim_data_poolFolder}/common/greenhouse_gases"
    datadir_hd="${icon_data_poolFolder}/${atmos_gridID}-${ocean_gridID}/hd/r0100"

    case "${exptype}" in
        "control")
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_${control_year}.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            for ((year=start_year-1; year<=end_year; year++)); do  # constant historical value
                ln -s "${datadir_ozone}/bc_ozone_historical_${control_year}.nc" "./bc_ozone_${year}.nc"
            done
            ln -sf "${datadir_rad}/swflux_14band_cmip6_${control_year}ADconst_999-2301-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            # FIXME: overrides link above
            ln -sf "${datadir_rad}/swflux_14band_cmip6_1849-2299-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            ;;
        "picontrol")
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_1850.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            for ((year=start_year-1; year<=end_year; year++)); do  # 1850 constant
                ln -sf "${datadir_ozone}/bc_ozone_historical_1850.nc" "./bc_ozone_${year}.nc"
            done
            ln -sf "${datadir_rad}/swflux_14band_cmip6_1850ADconst_999-2301-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            # FIXME: overrides link above
            ln -sf "${datadir_rad}/swflux_14band_cmip6_1849-2299-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            ;;
        *)
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_1850.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            # NOTE: check => this loop does not start at start_year-1 like the others
            for ((year=start_year; year<=end_year; year++)); do
                if [[ $year -eq 1849 ]]; then
                    ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_1850.nc" ./bc_aeropt_cmip6_volc_lw_b16_sw_b14_1849.nc
                elif [[ $year -le 2014 ]]; then
                    ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc" ./bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc
                else
                    ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_2014.nc" ./bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc
                fi
            done
            ln -sf "${datadir_plumes}/MACv2.0-SP-merged-historical-and-SSP3-70_v1.nc" ./MACv2.0-SP_v1.nc
            for ((year=start_year-1; year<=end_year; year++)); do
                if  ((year < 2014)) ; then  # historical
                    ln -s "${datadir_ozone}/bc_ozone_historical_${year}.nc" ./bc_ozone_"${year}".nc
                else  # projected
                    # FIXME: ssp is undefined
                    ln -s "${datadir_rad}/bc_ozone_ssp${ssp}_${year}.nc" ./bc_ozone_"${year}".nc
                fi
            done
            ln -sf ${datadir_rad}/swflux_14band_cmip6_1849-2299-v3.2.nc ./bc_solar_irradiance_sw_b14.nc
            ${datadir_ghg}/greenhouse_historical_plus.nc ./bc_greenhouse_gases.nc
            ;;
    esac

    ln -sf "${datadir_hd}/hdpara_r2b7_${atmos_gridID}_${ocean_gridID}_mc_maxl_s_v1.nc" ./bc_land_hd.nc   #  mask for routing_scheme='full'
    ln -sf "${datadir_hd}/hdstart_r2b7_${atmos_gridID}_${ocean_gridID}_mc_maxl_s_v1.nc" ./ic_land_hd.nc
}

oce_inputs(){
    ln -sf ${ocean_grid_folder}/${ocean_grid_target} .

    if [[ "${initialiseOcean}" == "fromClimatology" ]]; then
        ln -sf "${ocean_grid_folder}/ocean/initial_conditions/rcscs/tsi_oras5_icon_icon_grid_0062_R02B07_O_L72.nc_1979-01-01" ./initial_state.nc
    else
        echo "ERROR: initialiseOcean mode not supported (yet?): ${initialiseOcean}"
        exit 1
    fi

    if [[ "${use_hamocc}" == "yes" ]]; then
        # FIXME: this is still a private path
        datadir="/work/mh0727/m300732/input/0036/ocean/hamocc"
        ln -sf "${datadir}/MAHOWALDDUST_icon_grid_0036_R02B04_O_remapbil1.nc" ./dust.nc  # iron deposition
        ln -sf "${datadir}/ndepo_1-0_gr_185001-185012-clim_icon_grid_0036_R02B04_O.nc" ./nitrogen.nc  # nitrate deposition
    fi
}

lnd_inputs(){
    datadir_land="${icon_data_poolFolder}/${atmos_gridID}-${ocean_gridID}/land/rcscs"

    ifs_fname="ifs2icon_R2B07_DOM01.nc"  # NOTE: do not change name
    ifs_indir="${clim_data_poolFolder}/${atmos_gridID}/initial_conditions/rcscs"
    ifs_origin_name="ifs2icon_1979010100_${atmos_gridID}_${atmos_refinement}_G.nc"
    ln -sf "${ifs_indir}/${ifs_origin_name}" "${ifs_fname}"

    ln -sf "${basedir}/externals/jsbach/data/lctlib_nlct21.def" .

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

    # - jsbach land data:
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
