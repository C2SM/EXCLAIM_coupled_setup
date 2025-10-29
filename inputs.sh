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
    datadir_aerosol_volcanic="${common_data_poolFolder}/aerosol_volcanic_cmip6"
    datadir_plumes="${common_data_poolFolder}/MACv2_simple_plumes_merged"
    datadir_ozone="${atmo_data_InputFolder}/ozone/${revision}"
    datadir_rad="${common_data_poolFolder}/solar_radiation"
    datadir_ghg="${common_data_poolFolder}/greenhouse_gases"
    datadir_hd="${icon_data_poolFolder}/atmo/${atmos_gridID}-${ocean_gridID}/hd/${datadir_hd_tag}"

    case "${exptype}" in
        "control")
            # aerosols - irad_aero=12 - 2 common files + annual file
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_${control_year}.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            # ozone constant historical value
            ln -sf "${datadir_ozone}/bc_ozone_historical_${control_year}.nc" ./"bc_ozone_${chunk_start_year}.nc"
            ln -sf "${datadir_ozone}/bc_ozone_historical_${control_year}.nc" ./"bc_ozone_$((chunk_start_year-1)).nc"
            # solar irradiance - from file with constant annual values. This file must be made for each control year. see datadir_rad
            ln -sf "${datadir_rad}/swflux_14band_cmip6_${control_year}ADconst_999-2301-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            # GHGs from ighg1=2 and ighg2=3 - use values from vmr_{ghg} in radiation_nml
            ;;
        "hist") # transient
            # aerosols irad_aero=18 # background aero. from Kinne + volcanic aero. + anthropogenic aero. from Simple Plume
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_lw_b16_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_coa.nc" .
            ln -sf "${datadir_aerosol_kinne}/bc_aeropt_kinne_sw_b14_fin_1850.nc" ./bc_aeropt_kinne_sw_b14_fin.nc
            # add volcanic aerosols
            # NOTE: no projections yet
            if (( chunk_start_year == 1849 )); then
                year=1850
                prev_year=1850
            elif (( chunk_start_year <= 2014 )); then
                year="${chunk_start_year}"
                (( prev_year = year -1 ))
            else
                echo "WARNING: volcanic aerosols will use constant 2014 values"
                year=2014
                prev_year=2013
            fi
            ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_${year}.nc" "./bc_aeropt_cmip6_volc_lw_b16_sw_b14_${chunk_start_year}.nc"
            ln -sf "${datadir_aerosol_volcanic}/bc_aeropt_cmip6_volc_lw_b16_sw_b14_${prev_year}.nc" "./bc_aeropt_cmip6_volc_lw_b16_sw_b14_$((chunk_start_year-1)).nc"
            
            # add anthropogenic aerosols from simple plumes
            # TODO add ssp
            ln -sf "${datadir_plumes}/MACv2.0-SP-merged-historical-and-SSP3-70_v1.nc" ./MACv2.0-SP_v1.nc
            # ozone depending on year
            if (( chunk_start_year < 2014 )); then
                year="${chunk_start_year}"
            else
                echo "WARNING: ozone will use constant 2014 values"
                year="2014"
            fi
            ln -sf "${datadir_ozone}/bc_ozone_historical_${year}.nc" "./bc_ozone_${chunk_start_year}.nc"
            ln -sf "${datadir_ozone}/bc_ozone_historical_$((year-1)).nc" "./bc_ozone_$((chunk_start_year-1)).nc"
            # solar irradiance
            ln -sf "${datadir_rad}/swflux_14band_cmip6_1849-2299-v3.2.nc" ./bc_solar_irradiance_sw_b14.nc
            # GHGs with ighg1=4 and 1ghg2=4 - greenhouse gases from external file
            # TODO: implement current_year to have historical, projections, and different scenarios. For now, historic+ssp245.
            ln -sf "${datadir_ghg}/greenhouse_historical_plus.nc" ./bc_greenhouse_gases.nc
            ;;
        *)
            "ERROR: unsupported experiment type: ${exptype}"
            exit 1
            ;;
    esac
    
    ln -sf "${datadir_hd}/hdpara_${atmos_refinement_short}_${atmos_gridID}_${ocean_gridID}_${bc_land_hd_name}.nc" ./bc_land_hd.nc   #  mask for routing_scheme='full'
    ln -sf "${datadir_hd}/hdstart_${atmos_refinement_short}_${atmos_gridID}_${ocean_gridID}_${ic_land_hd_name}.nc" ./ic_land_hd.nc
}

oce_inputs(){
    ln -sf ${ocean_grid_folder}/${ocean_grid_target} .

    if [[ "${initialiseOcean}" == "fromClimatology" ]]; then
        ln -sf "${ocean_grid_folder}/ocean/initial_conditions/${initial_state_sub_path}" ./initial_state.nc
    else
        echo "ERROR: initialiseOcean mode not supported (yet?): ${initialiseOcean}"
        exit 1
    fi

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
    
    #used to initialize jsbach from ifs data
    ifs_fname="ifs2icon_R2B07_DOM01.nc"  # NOTE: do not change name
    
    # TODO: add picontrol mechanism
    ifs_origin_name="ifs2icon_${start_year}010100_${atmos_gridID}_${atmos_refinement}_G.nc"
    ln -sf "${datadir_init}/${ifs_origin_name}" "${ifs_fname}"

    ln -sf "${basedir}/externals/jsbach/data/lctlib_nlct21.def" .

    # choose year for land fraction file based on exptype
    case "$exptype" in
        "control") year="${control_year}" ;;
        "hist") year="${chunk_start_year}" ;;
        *) echo "ERROR: unsupported experiment type: ${exptype}"; exit 1 ;;
    esac
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

    extpar_filename="${datadir_land}/icon_extpar4jsbach_${atmos_gridID}_${extpar_tag}_tiles_jsb.nc"
    extpar_targetname="extpar_icon_grid_${atmos_gridID}_${atmos_refinement}_G.nc"
    ln -sf "${extpar_filename}" "${extpar_targetname}"
}
