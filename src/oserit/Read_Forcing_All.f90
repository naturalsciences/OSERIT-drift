
SUBROUTINE init_forcings
  USE grid_module
  IMPLICIT NONE
  INTEGER :: n

  ALLOCATE(domains(nogrids))
  ALLOCATE(ncids(nogrids,4))

END SUBROUTINE

SUBROUTINE close_ncid_forcings
  USE grid_module
  USE netCDF
  USE oseswitches
  USE parampars
  IMPLICIT NONE
  INTEGER :: i, status
  DO i =1,nogrids

      IF(oserit_param%has_cur(i))THEN
        status = nf90_close(ncids(i,hydro_id))
        if (status /= nf90_noerr) stop 'unable to close hydro forcing'
      END IF
      IF(oserit_param%has_bat(i))THEN
        status = nf90_close(ncids(i,bat_id))
        if (status /= nf90_noerr) stop 'unable to close bathymetry'
      END IF

      IF(iopt_wave == 1 .AND. oserit_param%has_wave(i))THEN
        status = nf90_close(ncids(i,wave_id))
        if (status /= nf90_noerr) stop 'unable to close wave forcing'
      END IF

      IF(iopt_meteo == 1 .AND. oserit_param%has_wind(i))THEN
        status = nf90_close(ncids(i,wind_id))
        if (status /= nf90_noerr) stop 'unable to close wind forcing'
      END IF
  END DO
  print*, "forcings closed"
  DEALLOCATE(ncids)

END SUBROUTINE close_ncid_forcings

SUBROUTINE compute_grid_size
!compute the size of the grid for the entire forcing object
  USE grid_module
  USE oseswitches
  USE parampars
  IMPLICIT NONE
  INTEGER :: n

  DO n =1,nogrids

    domains(n)%lon_max = 360
    domains(n)%lat_max = 360
    domains(n)%lon_min = -360
    domains(n)%lat_min = -360

    IF(oserit_param%has_cur(n))THEN
      domains(n)%lon_max = domains(n)%forcings(hydro_id)%lons(size(domains(n)%forcings(hydro_id)%lons))
      domains(n)%lat_max = domains(n)%forcings(hydro_id)%lats(size(domains(n)%forcings(hydro_id)%lats))
      domains(n)%lon_min = domains(n)%forcings(hydro_id)%lons(1)
      domains(n)%lat_min = domains(n)%forcings(hydro_id)%lats(1)
    END IF

    ! no needs to use the wave and wind grid min and max if the forcing is not used
    IF(iopt_wave == 1 .AND. oserit_param%has_wave(n))THEN
      domains(n)%lon_max = MINVAL([domains(n)%lon_max,domains(n)%forcings(wave_id)%lons(size(domains(n)%forcings(wave_id)%lons))])
      domains(n)%lat_max = MINVAL([domains(n)%lat_max,domains(n)%forcings(wave_id)%lats(size(domains(n)%forcings(wave_id)%lats))])
      domains(n)%lon_min = MAXVAL([domains(n)%lon_min,domains(n)%forcings(wave_id)%lons(1)])
      domains(n)%lat_min = MAXVAL([domains(n)%lat_min,domains(n)%forcings(wave_id)%lats(1)])
    END IF
    IF(iopt_meteo == 1 .AND. oserit_param%has_wind(n))THEN
      domains(n)%lon_max = MINVAL([domains(n)%lon_max,domains(n)%forcings(wind_id)%lons(size(domains(n)%forcings(wind_id)%lons))])
      domains(n)%lat_max = MINVAL([domains(n)%lat_max,domains(n)%forcings(wind_id)%lats(size(domains(n)%forcings(wind_id)%lats))])
      domains(n)%lon_min = MAXVAL([domains(n)%lon_min,domains(n)%forcings(wind_id)%lons(1)])
      domains(n)%lat_min = MAXVAL([domains(n)%lat_min,domains(n)%forcings(wind_id)%lats(1)])
    END IF
    PRINT*, 'Domain ',n,' ready, size: lon min :',domains(n)%lon_min,', lon max :',domains(n)%lon_max, &
            &', lat min :',domains(n)%lat_min,', lat max :',domains(n)%lat_max
  END DO

END SUBROUTINE


SUBROUTINE read_bathy_all
  !read the bathymetry forcing from all the grids
  USE grid_module
  USE parampars
  IMPLICIT NONE
  INTEGER :: n

  DO n =1,nogrids
    IF(oserit_param%has_bat(n))CALL read_bath_grid(n)
  END DO

END SUBROUTINE


SUBROUTINE allocate_wave_grids
  !load the wave forcing for all grids
  USE netcdf
  USE parampars
  USE grid_module
  USE time_routines
  IMPLICIT NONE
  INTEGER :: n !id of the grid
  INTEGER :: status, nDims, nVars, nGlobalAtts, unlimDimID
  character(len = nf90_max_name):: name, test
  INTEGER :: dim_lats, dim_lons, dim_depth, dim_time
  REAL :: values

  DO n =1,nogrids
    IF(oserit_param%has_wave(n)) THEN
      print*,"Working on domain ",n
      status = nf90_open(get_forcing_file_path(n, wave_id, IDateTime) &
                    &,NF90_NOWRITE, ncids(n,wave_id))
      if (status /= nf90_noerr)then
        print*, "Failed to open : "// get_forcing_file_path(n, wave_id, IDateTime), "  Domain:", n 
        stop 'Unable to open wave forcing file for allocation'
      endif
      call prepare_grid(domains(n)%forcings(wave_id), ncids(n,wave_id), oserit_param%lookup_wav(n,:),&
                        & dim_lats, dim_lons)

      allocate(domains(n)%forcings(wave_id)%vars(nbr_wave_vars))

      domains(n)%forcings(wave_id)%vars(wave_Tm1_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      domains(n)%forcings(wave_id)%vars(wave_hstt_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      domains(n)%forcings(wave_id)%vars(wave_dirw_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      domains(n)%forcings(wave_id)%vars(wave_u_stockes_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      domains(n)%forcings(wave_id)%vars(wave_v_stockes_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      print*,"Successful wave allocation for domain",n
    END IF
  END DO
END SUBROUTINE

SUBROUTINE allocate_bathy_grids
!load the wind forcing for all grids
USE netcdf
USE parampars
USE grid_module
IMPLICIT NONE
INTEGER :: n !id of the grid
INTEGER :: status, nDims, nVars, nGlobalAtts, unlimDimID
character(len = nf90_max_name):: name
INTEGER :: dim_lats, dim_lons
REAL :: values

DO n =1,nogrids
  IF(oserit_param%has_bat(n)) THEN
    print*,"Working on domain ",n
    status = nf90_open(oserit_param%io_bat_filename(n),NF90_NOWRITE, ncids(n,bat_id))
    if (status /= nf90_noerr) stop 'unable to open bathymetry file forcing for allocation'

    dim_lons = size(domains(n)%forcings(hydro_id)%lons)
    dim_lats = size(domains(n)%forcings(hydro_id)%lats)

    ALLOCATE(domains(n)%forcings(hydro_id)%depth_data%depth_mean(dim_lons,dim_lats))

    domains(n)%forcings(hydro_id)%depth_data%depth_mean = 0
    print*,"Successful bathymmetry allocation for domain",n
  END IF

END DO
END SUBROUTINE


SUBROUTINE allocate_wind_grids
!load the wind forcing for all grids
USE netcdf
USE parampars
USE grid_module
USE oseswitches
USE time_routines
USE time_routines_ose
IMPLICIT NONE
INTEGER :: n !id of the grid
INTEGER :: status, nDims, nVars, nGlobalAtts, unlimDimID
character(len = nf90_max_name):: name
INTEGER :: dim_lats, dim_lons, dim_time,tmp
REAL :: values

DO n =1,nogrids
  IF(oserit_param%has_wind(n)) THEN
    print*,"Working on domain ",n
    status = nf90_open(get_forcing_file_path(n, wind_id, IDateTime) &
                      &,NF90_NOWRITE, ncids(n,wind_id))
    if (status /= nf90_noerr)THEN
      print*, "Failed to open : "// get_forcing_file_path(n, wind_id, IDateTime), "  Domain:", n
      stop 'unable to open wind forcing file for allocation'
    END IF
    call prepare_grid(domains(n)%forcings(wind_id), ncids(n,wind_id), oserit_param%lookup_win(n,:),&
                  & dim_lats, dim_lons)

    allocate(domains(n)%forcings(wind_id)%vars(nbr_meteo_vars))

    domains(n)%forcings(wind_id)%vars(meteo_temp_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
    domains(n)%forcings(wind_id)%vars(meteo_uwind_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
    domains(n)%forcings(wind_id)%vars(meteo_vwind_id) = init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)

    !allocate the cloud cover only if it is used
    IF(iopt_cloud == 1)THEN
      domains(n)%forcings(wind_id)%vars(meteo_low_cloud_cover_id) = &
                                                    & init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      domains(n)%forcings(wind_id)%vars(meteo_mid_cloud_cover_id) = &
                                                    & init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
      domains(n)%forcings(wind_id)%vars(meteo_high_cloud_cover_id) = &
                                                    & init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)
    END IF
  END IF
  print*,"Successful meteo allocation for domain",n
END DO
END SUBROUTINE


SUBROUTINE allocate_current_grids
  !load the current forcing for all grids
  USE netcdf
  USE parampars
  USE grid_module
  USE timepars
  USE time_routines
  IMPLICIT NONE
  INTEGER :: n !id of the grid
  INTEGER :: status, nDims, nVars, nGlobalAtts, unlimDimID
  character(len = nf90_max_name):: name
  INTEGER :: dim_lats, dim_lons, dim_depth, dim_time
  INTEGER :: values

  REAL,ALLOCATABLE, DIMENSION(:) :: time

  DO n =1,nogrids
    IF(oserit_param%has_cur(n)) THEN
      print*,"Working on domain ",n
      status = nf90_open(get_forcing_file_path(n, hydro_id, IDateTime) &
                        &,NF90_NOWRITE, ncids(n,hydro_id))
      if (status /= nf90_noerr)then
        print*, "Failed to open : "// get_forcing_file_path(n, hydro_id, IDateTime), "  Domain:", n
        stop 'unable to open current forcing file for allocation'
      END IF

      call prepare_grid(domains(n)%forcings(hydro_id), ncids(n,hydro_id), &
                                                              & [oserit_param%lookup_cur(n,1), &
                                                                & oserit_param%lookup_cur(n,2), &
                                                                & oserit_param%lookup_cur(n,3), &
                                                                & oserit_param%lookup_cur(n,5), &
                                                                & oserit_param%lookup_cur(n,6), &
                                                                & oserit_param%lookup_cur(n,7)], &
                                      & dim_lats, dim_lons)

      status = nf90_inquire_dimension(ncids(n,hydro_id), oserit_param%lookup_cur(n,4), name, dim_depth)
      if (status /= nf90_noerr) stop 'unable to inquire current depth dimensions for allocation'
      print*, "dim of (depth)", dim_depth,"  variable name:",name

      IF(oserit_param%iopt_uniform_depth(n) == 0)THEN !read the sigma layers
        ALLOCATE(domains(n)%forcings(hydro_id)%depth_data%sigma_layers(dim_depth))
        status = nf90_get_var(ncids(n,hydro_id), oserit_param%lookup_cur(n,8), &
                                & domains(n)%forcings(hydro_id)%depth_data%sigma_layers)

        domains(n)%forcings(hydro_id)%depth_data%sigma_layers = ABS(domains(n)%forcings(hydro_id)%depth_data%sigma_layers)

        if (status /= nf90_noerr) stop 'unable to inquire sigma layers'
        print*, "value of (sigma_layers):", domains(n)%forcings(hydro_id)%depth_data%sigma_layers
      END IF

      allocate(domains(n)%forcings(hydro_id)%vars(nbr_hydro_vars))

      domains(n)%forcings(hydro_id)%vars(hydro_uvel_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)
      domains(n)%forcings(hydro_id)%vars(hydro_vvel_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)
      domains(n)%forcings(hydro_id)%vars(hydro_wvel_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)

      domains(n)%forcings(hydro_id)%depth_data%depth = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)

      domains(n)%forcings(hydro_id)%depth_data%depth_tot = &
                                    &init_data_forcing_2d(dim_lons,dim_lats, oserit_param%iopt_backtrack)

      domains(n)%forcings(hydro_id)%vars(hydro_temp_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)
      domains(n)%forcings(hydro_id)%vars(hydro_sal_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)
      domains(n)%forcings(hydro_id)%vars(hydro_dens_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)

      domains(n)%forcings(hydro_id)%vars(hydro_K_z_id) = &
                                    &init_data_forcing_3d(dim_lons,dim_lats,dim_depth, oserit_param%iopt_backtrack)

      domains(n)%forcings(hydro_id)%depth_data%depth_set = .FALSE.
      print*,"Successful current allocation for domain:",n
    END IF
  END DO
END SUBROUTINE

SUBROUTINE read_bath_grid(n)
!load the bathymetry grid
USE grid_module
USE timepars_ose
USE netcdf
USE parampars
IMPLICIT NONE
INTEGER, INTENT(IN) :: n !id of the grid
INTEGER, DIMENSION(2) :: len_array ! dimension of the arrays (lon,lat)
INTEGER :: status

len_array = SHAPE(domains(n)%forcings(hydro_id)%depth_data%depth_mean)
status = nf90_get_var(ncids(n,bat_id), oserit_param%lookup_bat(n,5), domains(n)%forcings(hydro_id)%depth_data%depth_mean, &
                      & start=(/1,1/), count =(/len_array(1),len_array(2)/))
if (status /= nf90_noerr) stop "unable to load depth_mean"

END SUBROUTINE
