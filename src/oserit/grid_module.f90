MODULE grid_module

!type for the grids

USE datatypes_ose

IMPLICIT NONE

integer, parameter :: hydro_id = 1, wind_id = 2, wave_id = 3, bat_id = 4
integer, parameter :: hydro_uvel_id = 1, hydro_vvel_id = 2, hydro_wvel_id = 3, &
                      & hydro_temp_id = 4, hydro_sal_id = 5, hydro_dens_id = 6, hydro_K_z_id = 7
integer, parameter :: meteo_temp_id = 1, meteo_uwind_id = 2, meteo_vwind_id = 3, meteo_low_cloud_cover_id = 4, &
                      & meteo_mid_cloud_cover_id = 5, meteo_high_cloud_cover_id = 6
integer, parameter :: wave_hstt_id = 1, wave_Tm1_id = 2, wave_dirw_id = 3, wave_u_stockes_id = 4, wave_v_stockes_id = 5 
integer, parameter :: nbr_hydro_vars = 7, nbr_meteo_vars = 6, nbr_wave_vars = 5


INTEGER, DIMENSION(:,:), ALLOCATABLE:: ncids! current =1, wind = 2, waves = 3
INTEGER, PUBLIC :: nogrids !number of grids

TYPE (Domain), ALLOCATABLE, DIMENSION(:) :: domains
!array of the grids


CONTAINS

FUNCTION find_file(start_s, date, file_duration)
  !return the date (example 20220530) needed to read the time asked
  !file_duration is the length of the file (for instance 24*3600s)
  USE timepars
  USE time_routines_ose
  IMPLICIT NONE
  REAL, INTENT(IN) ::start_s
  INTEGER, DIMENSION(7), INTENT(IN) :: date
  INTEGER, DIMENSION(7) :: date_output
  CHARACTER(8) :: find_file
  INTEGER :: full_number
  INTEGER :: second_today
  INTEGER, INTENT(IN):: file_duration

  !time already in the day today
  second_today = date(4)*3600 + date(5)*60 + date(6)

  !the file start after 00:00
  if (start_s>= 0) then
    IF (second_today >= start_s) THEN
      full_number = date(1)*10000+date(2)*100+date(3)
    ELSE
      CALL add_secs_to_date_int(date,date_output,-1,file_duration)
      full_number = date_output(1)*10000+date_output(2)*100+date_output(3)
    ENDIF
  !the file start before 00:00
  else
    IF (second_today <= file_duration+start_s) THEN
      full_number = date(1)*10000+date(2)*100+date(3)
    ELSE
      CALL add_secs_to_date_int(date,date_output,1,file_duration)
      full_number = date_output(1)*10000+date_output(2)*100+date_output(3)
    ENDIF
  end if

  write(find_file,'(i8.8)') full_number
END FUNCTION

FUNCTION get_forcing_file_path(n, grid_type, date)
  !return the path of the file for the defined forcing at the defined date
  USE oseswitches
  USE parampars
  IMPLICIT NONE
  INTEGER :: n, grid_type
  CHARACTER(LEN=max_length_string), ALLOCATABLE :: get_forcing_file_path
  INTEGER, DIMENSION(7) :: date
  SELECT CASE (grid_type)
      CASE(wave_id)
        get_forcing_file_path = TRIM(oserit_param%io_wav_filename(n))&
                          & //find_file(oserit_param%start_time_wave(n),date,oserit_param%file_duration_wave(n))&
                          & //".nc"
      CASE(hydro_id)
        get_forcing_file_path = TRIM(oserit_param%io_cur_filename(n))&
                          & //find_file(oserit_param%start_time_hydro(n),date,oserit_param%file_duration_hydro(n))&
                          & //".nc"
      CASE(wind_id)
        get_forcing_file_path = TRIM(oserit_param%io_win_filename(n))&
                          & //find_file(oserit_param%start_time_meteo(n),date,oserit_param%file_duration_meteo(n))&
                          & //".nc"
  END SELECT

END FUNCTION

FUNCTION get_nearest_neighbor(loc, lon_ar, lat_ar, depth_ar)
  !return the nearest neighbor of the loc in the fourth direction
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL, DIMENSION(:), ALLOCATABLE :: dist_array
  REAL, DIMENSION(:), INTENT(IN):: lon_ar, lat_ar
  REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN):: depth_ar
  INTEGER, DIMENSION(3) :: get_nearest_neighbor
  INTEGER, DIMENSION(4) :: vert_id ! the id of the top of 4 each column
  REAL :: sq_lat_up, sq_lat_down, sq_lon_up, sq_lon_down
  REAL, DIMENSION(4) :: sq_depth_up, sq_depth_down
  INTEGER, DIMENSION(1) :: id
  get_nearest_neighbor(1) = get_id_from_vec(loc%x_lon, lon_ar)

  sq_lon_down = (loc%x_lon -lon_ar(get_nearest_neighbor(1)))*(loc%x_lon -lon_ar(get_nearest_neighbor(1)))
  sq_lon_up = (loc%x_lon -lon_ar(get_nearest_neighbor(1)+1))*(loc%x_lon -lon_ar(get_nearest_neighbor(1)+1))

  get_nearest_neighbor(2) = get_id_from_vec(loc%y_lat, lat_ar)

  sq_lat_down = (loc%y_lat -lat_ar(get_nearest_neighbor(2)))*(loc%y_lat -lat_ar(get_nearest_neighbor(2)))
  sq_lat_up = (loc%y_lat -lat_ar(get_nearest_neighbor(2)+1))*(loc%y_lat -lat_ar(get_nearest_neighbor(2)+1))

  vert_id = 1
  IF (PRESENT(depth_ar)) THEN
    !find the interval for all the 4 column
    vert_id(1) = get_depth_id(loc%z_depth, -depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2),:))

    vert_id(2) = get_depth_id(loc%z_depth, -depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2),:))

    vert_id(3) = get_depth_id(loc%z_depth, -depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2)+1,:))

    vert_id(4) = get_depth_id(loc%z_depth, -depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2)+1,:))


    !compute the distance for the 8 points
    sq_depth_down(1) = (loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2),vert_id(1)+1))&
                        & * (loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2),vert_id(1)+1))
    sq_depth_up(1) = (loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2),vert_id(1)))&
                        & *(loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2),vert_id(1)))

    sq_depth_down(2) = (loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2),vert_id(2)+1))&
                        & * (loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2),vert_id(2)+1))
    sq_depth_up(2) = (loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2),vert_id(2)))&
                        & *(loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2),vert_id(2)))

    sq_depth_down(3) = (loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2)+1,vert_id(3)+1))&
                        & * (loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2)+1,vert_id(3)+1))
    sq_depth_up(3) = (loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2)+1,vert_id(3)))&
                          & *(loc%z_depth +depth_ar(get_nearest_neighbor(1),get_nearest_neighbor(2)+1,vert_id(3)))

    sq_depth_down(4) = (loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2)+1,vert_id(4)+1))&
                        & * (loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2)+1,vert_id(4)+1))
    sq_depth_up(4) = (loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2)+1,vert_id(4)))&
                          & *(loc%z_depth +depth_ar(get_nearest_neighbor(1)+1,get_nearest_neighbor(2)+1,vert_id(4)))

    ALLOCATE(dist_array(8))
    dist_array(1)= sq_lat_down + sq_lon_down + sq_depth_up(1)
    dist_array(2)= sq_lat_up + sq_lon_down + sq_depth_up(3)
    dist_array(3)= sq_lat_down + sq_lon_up+ sq_depth_up(2)
    dist_array(4)= sq_lat_up + sq_lon_up + sq_depth_up(4)
    dist_array(5)= sq_lat_down + sq_lon_down + sq_depth_down(1)
    dist_array(6)= sq_lat_up + sq_lon_down + sq_depth_down(3)
    dist_array(7)= sq_lat_down + sq_lon_up + sq_depth_down(2)
    dist_array(8)= sq_lat_up + sq_lon_up + sq_depth_down(4)
  ELSE
    ALLOCATE(dist_array(4))
    dist_array(1)= sq_lat_down + sq_lon_down
    dist_array(2)= sq_lat_up + sq_lon_down
    dist_array(3)= sq_lat_down + sq_lon_up
    dist_array(4)= sq_lat_up + sq_lon_up
  END IF

  id = MINLOC(dist_array)

  SELECT CASE (id(1))
    CASE(1)
      get_nearest_neighbor(3) = vert_id(1)
    CASE(2)
      get_nearest_neighbor(1) = get_nearest_neighbor(1) +1
      get_nearest_neighbor(3) = vert_id(3)
    CASE(3)
      get_nearest_neighbor(2) = get_nearest_neighbor(2) +1
      get_nearest_neighbor(3) = vert_id(2)
    CASE(4)
      get_nearest_neighbor(1) = get_nearest_neighbor(1) +1
      get_nearest_neighbor(2) = get_nearest_neighbor(2) +1
      get_nearest_neighbor(3) = vert_id(4)
    CASE(5)
      get_nearest_neighbor(3) = vert_id(1) +1
    CASE(6)
      get_nearest_neighbor(1) = get_nearest_neighbor(1) +1
      get_nearest_neighbor(3) = vert_id(3) +1
    CASE(7)
      get_nearest_neighbor(2) = get_nearest_neighbor(2) +1
      get_nearest_neighbor(3) = vert_id(2) +1
    CASE(8)
      get_nearest_neighbor(1) = get_nearest_neighbor(1) +1
      get_nearest_neighbor(2) = get_nearest_neighbor(2) +1
      get_nearest_neighbor(3) = vert_id(4) +1

  END SELECT

END FUNCTION

FUNCTION get_id_from_vec(val, vec)
  !Return the id of the value just under the "val" in the "vec", the vector should
  !be in increasing order with a more or less constant scale, val must be under the
  !upper value of vec and above the lowest value of vec
  REAL, INTENT(IN):: val
  REAL, DIMENSION(:), INTENT(IN) :: vec
  INTEGER :: get_id_from_vec

  !first approximation
  get_id_from_vec = INT((val - vec(1))/(vec(2) - vec(1)))

  IF (get_id_from_vec > SIZE(vec)) get_id_from_vec = SIZE(vec)
  !increase if too small
  DO WHILE(vec(get_id_from_vec+1) < val)
    get_id_from_vec = get_id_from_vec + 1
  END DO
  !decrease if too big
  DO WHILE(vec(get_id_from_vec) > val)
    get_id_from_vec = get_id_from_vec - 1
  END DO

END FUNCTION

FUNCTION get_forcing_id(loc, lookup)
  !Get the first forcing in which the location is located, only if lookup is defined
  !in this grid. If there is no grid with the data, return -1
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc
  INTEGER, DIMENSION(:):: lookup
  INTEGER :: get_forcing_id, it
  LOGICAL :: is_defined
  is_defined = .false.
  get_forcing_id = 1
  IF (nogrids > 1) THEN
    DO it = 1, nogrids
      get_forcing_id = it !it will be added 1 after the end of the loop
      IF(loc%y_lat > domains(get_forcing_id)%lat_min .AND. loc%y_lat < domains(get_forcing_id)%lat_max &
        & .AND. loc%x_lon > domains(get_forcing_id)%lon_min .AND. loc%x_lon < domains(get_forcing_id)%lon_max &
            & .AND. lookup(get_forcing_id) >= 0) THEN
        is_defined = .true.
        EXIT
      END IF
    END DO
  ELSE IF(lookup(get_forcing_id) >= 0) THEN  ! only one grid
       IF(loc%y_lat > domains(get_forcing_id)%lat_min .AND. loc%y_lat < domains(get_forcing_id)%lat_max &
        & .AND. loc%x_lon > domains(get_forcing_id)%lon_min .AND. loc%x_lon < domains(get_forcing_id)%lon_max &
            & .AND. lookup(get_forcing_id) >= 0) THEN
        is_defined = .true.
        END IF
  END IF
  IF(is_defined .eqv. .false.)THEN
      get_forcing_id = -1
  END IF

END FUNCTION

FUNCTION scale_and_offset(file_id, var_id)
  !look in the netcdf file if there is a scale factor(1) and a offset(2)
  USE netcdf
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: file_id
  INTEGER, INTENT(IN) :: var_id
  REAL, DIMENSION(2) :: scale_and_offset
  INTEGER :: status

  status = nf90_get_att(file_id,var_id, "scale_factor", scale_and_offset(1))
  if (status /= nf90_noerr) scale_and_offset(1) = 1
  status = nf90_get_att(file_id,var_id, "add_offset", scale_and_offset(2))
  if (status /= nf90_noerr) scale_and_offset(2) = 0

END FUNCTION


FUNCTION get_depth_id(depth, depth_ar)
  !index for the depth. The depth_ar must in increassing or decreasing order
  !the function always return the index under the value of depth
  IMPLICIT NONE
  REAL, DIMENSION(:), INTENT(IN) :: depth_ar
  REAL, INTENT(IN) :: depth
  INTEGER :: get_depth_id

  get_depth_id = 1
  !the array start at the surface
  IF(depth_ar(1) > depth_ar(2))THEN

    DO WHILE(depth_ar(get_depth_id+1) > depth .AND. get_depth_id+1<SIZE(depth_ar))
      get_depth_id = get_depth_id + 1
    END DO
  !the array start at the bottom
  ELSE
    DO WHILE(depth_ar(get_depth_id+1) < depth .AND. get_depth_id+1<SIZE(depth_ar))
      get_depth_id = get_depth_id + 1
    END DO

  END IF

END FUNCTION


FUNCTION interp_tmstp(prev, tmstp, next)
  !Distance between prev and tmstp normalized in rapport to next-prev
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: tmstp, prev, next
  REAL :: delta_var, dist
  REAL :: interp_tmstp
  delta_var = tmstp-prev
  dist = next - prev
  interp_tmstp = delta_var / dist
END FUNCTION

FUNCTION get_value(loc, grid_parameters, array_2D, array_3D, depth_ar)
  !return the value at the location loc
  !of the array_2D or 3D (only one value should be provided), and if the array
  ! is 3D, the depth_ar should also be provided
  USE oseswitches
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc
  TYPE(ForcingType), INTENT(IN) :: grid_parameters
  REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL :: array_2D
  REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: array_3D
  REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: depth_ar
  integer :: id1, id2, id3, id4 ! the id of the top of 4 each column
  REAL:: get_value
  REAL:: xd, yd, zd, opxd, opyd
  REAL:: c00, c01, c10, c11, c0, c1
  REAL :: z00, z01, z10, z11, z0, z1
  INTEGER :: i, j
  INTEGER, DIMENSION(3) :: nearest_neighbor

  SELECT CASE(iopt_grid_interp)
    CASE(0)!nearest neighbor
      IF(PRESENT(array_2D))THEN !2D array
        nearest_neighbor = get_nearest_neighbor(loc, grid_parameters%lons, grid_parameters%lats)
        get_value = array_2D(nearest_neighbor(1), nearest_neighbor(2))
      ELSE !3D array
        nearest_neighbor = get_nearest_neighbor(loc, grid_parameters%lons, grid_parameters%lats, depth_ar)
        get_value = array_3D(nearest_neighbor(1), nearest_neighbor(2), nearest_neighbor(3))
      END IF

    CASE(2)!Trilinear interpolation https://en.wikipedia.org/wiki/Trilinear_interpolation
      i = get_id_from_vec(loc%x_lon, grid_parameters%lons)
      xd = (loc%x_lon - grid_parameters%lons(i)) / (grid_parameters%lons(i+1) - grid_parameters%lons(i))
      opxd = 1-xd

      j = get_id_from_vec(loc%y_lat, grid_parameters%lats)
      yd = (loc%y_lat - grid_parameters%lats(j)) / (grid_parameters%lats(j+1) - grid_parameters%lats(j))
      opyd = 1-yd
      
      IF(PRESENT(array_2D))THEN !Bilinear interpolation
        c0 = (opxd) * array_2D(i, j) + xd * array_2D(i+1, j)
        c1 = (opxd) * array_2D(i, j+1) + xd * array_2D(i+1, j+1)
        get_value = (opyd) * c0 + yd * c1

      ELSE ! Trilinear interpolation

        !find the interval for all the 4 column
        id1 = get_depth_id(loc%z_depth, -depth_ar(i,j,:))

        id2 = get_depth_id(loc%z_depth, -depth_ar(i+1,j,:))

        id3 = get_depth_id(loc%z_depth, -depth_ar(i,j+1,:))

        id4 = get_depth_id(loc%z_depth, -depth_ar(i+1,j+1,:))

        !because the depth is not uniform. - because the depth is negative
        z00 = -((opxd) * (depth_ar(i, j, id1)) + xd * (depth_ar(i+1, j, id2)))
        z01 = -((opxd) * (depth_ar(i, j, id1+1)) + xd * (depth_ar(i+1, j, id2+1)))
        z10 = -((opxd) * (depth_ar(i, j+1, id3)) + xd * (depth_ar(i+1, j+1, id4)))
        z11 = -((opxd) * (depth_ar(i, j+1, id3+1)) + xd * (depth_ar(i+1, j+1, id4+1)))

        c00 = (opxd) * array_3D(i, j, id1) + xd * array_3D(i+1, j, id2)
        c01 = (opxd) * array_3D(i, j, id1+1) + xd * array_3D(i+1, j, id2+1)
        c10 = (opxd) * array_3D(i, j+1, id3) + xd * array_3D(i+1, j+1, id4)
        c11 = (opxd) * array_3D(i, j+1, id3+1) + xd * array_3D(i+1, j+1, id4+1)

        c0 = (opyd) * c00 + yd * c10
        c1 = (opyd) * c01 + yd * c11

        z0 = (opyd) * z00 + yd * z10
        z1 = (opyd) * z01 + yd * z11

        zd = (loc%z_depth - z0) / (z1-z0)
        !clamping zd
        if (zd < 0.0) then
            zd = 0.0
        else if (zd > 1.0) then
            zd = 1.0
        end if

        !zd = MAX(0.0,MIN((loc%z_depth - z0) / (z1-z0),1.0))!

        get_value = (1-zd) * c0 + zd * c1

      END IF

  END SELECT


END FUNCTION

SUBROUTINE update_depth_forcing(grid_id)
  USE parampars
  USE timepars_ose
  USE netcdf
  !update the depth if needed
  INTEGER, INTENT(IN) :: grid_id
  REAL, ALLOCATABLE, DIMENSION(:) :: depth_tmp
  REAL, DIMENSION(2) :: ar_sc_off
  real :: T1, T2
  INTEGER, ALLOCATABLE, DIMENSION(:) :: len_array
  INTEGER :: x, y, z, status
  INTEGER :: time_depth_old_before, time_depth_before !before the reloading of the depth, to check if it needs to be multiplied by it

  call cpu_time(T1)

  !only one array for the depth
  IF((oserit_param%iopt_uniform_depth(grid_id) == 1) .AND. &
      & (domains(grid_id)%forcings(hydro_id)%depth_data%depth_set .eqv. .FALSE.))THEN !transform the vector into the matrix
    len_array = SHAPE(domains(grid_id)%forcings(hydro_id)%depth_data%depth%var)
    ALLOCATE(depth_tmp(len_array(3)))
    status = nf90_get_var(ncids(grid_id,hydro_id), oserit_param%lookup_cur(grid_id,8), depth_tmp)
    if (status /= nf90_noerr) stop 'unable to load depth'
    ar_sc_off = scale_and_offset(ncids(grid_id,hydro_id),oserit_param%lookup_cur(grid_id,8))
    depth_tmp = depth_tmp * ar_sc_off(1) + ar_sc_off(2)
    DO x = 1, len_array(1)
      DO y = 1, len_array(2)
        domains(grid_id)%forcings(hydro_id)%depth_data%depth%var(x,y,:) = depth_tmp
      END DO
    END DO
    domains(grid_id)%forcings(hydro_id)%depth_data%depth%var_old = domains(grid_id)%forcings(hydro_id)%depth_data%depth%var
    domains(grid_id)%forcings(hydro_id)%depth_data%depth_set = .TRUE.

    domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%var(:,:,1) = &
                                                                & domains(grid_id)%forcings(hydro_id)%depth_data%depth_mean

    domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%var_old(:,:,1) = &
                                                                & domains(grid_id)%forcings(hydro_id)%depth_data%depth_mean

    DEALLOCATE(depth_tmp)
  ELSE IF(oserit_param%iopt_uniform_depth(grid_id) == 0) THEN! it is already (lon,lat,depth)

    time_depth_old_before =  domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time_old
    time_depth_before =  domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time

    CALL update_forcing(grid_id, hydro_id, oserit_param%lookup_cur(grid_id,12), & 
                        & domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot)
    !If a new depth has been read
    IF(time_depth_old_before /=  domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time_old &
            & .OR. time_depth_before /=  domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time)THEN
        !Just copied
        IF( domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time_old == time_depth_before)THEN
           domains(grid_id)%forcings(hydro_id)%depth_data%depth%var_old =  domains(grid_id)%forcings(hydro_id)%depth_data%depth%var
           domains(grid_id)%forcings(hydro_id)%depth_data%depth%time_old = domains(grid_id)%forcings(hydro_id)%depth_data%depth%time
        ELSE IF( domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time_old /= time_depth_old_before)THEN!a totally new one
          domains(grid_id)%forcings(hydro_id)%depth_data%depth%var_old = 0

          domains(grid_id)%forcings(hydro_id)%depth_data%depth%time_old = &
                                                            & domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time_old
          
          len_array = SHAPE( domains(grid_id)%forcings(hydro_id)%depth_data%depth%var_old)
          DO x = 1, len_array(1)
            DO y = 1, len_array(2)
              IF ( domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%var_old(x,y,1) > 0)THEN
                DO z = 1, len_array(3)
                   domains(grid_id)%forcings(hydro_id)%depth_data%depth%var_old(x,y,z) = &
                              &  domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%var_old(x,y,1) &
                              & *  domains(grid_id)%forcings(hydro_id)%depth_data%sigma_layers(z)
                END DO
              END IF
            END DO
          END DO
        END IF

        IF(domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time /= time_depth_before)THEN!a new one
          domains(grid_id)%forcings(hydro_id)%depth_data%depth%var = 0
          domains(grid_id)%forcings(hydro_id)%depth_data%depth%time = domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%time
          len_array = SHAPE(domains(grid_id)%forcings(hydro_id)%depth_data%depth%var)
          DO x = 1, len_array(1)
            DO y = 1, len_array(2)
              IF (domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%var(x,y,1) > 0)THEN
                DO z = 1, len_array(3)
                   domains(grid_id)%forcings(hydro_id)%depth_data%depth%var(x,y,z) =  &
                                    &  domains(grid_id)%forcings(hydro_id)%depth_data%depth_tot%var(x,y,1) &
                                    & *  domains(grid_id)%forcings(hydro_id)%depth_data%sigma_layers(z)
                END DO
              END IF
            END DO
          END DO
        END IF

    END IF

  END IF

  call cpu_time(T2)

  time_interp_update = time_interp_update + T2 - T1

END SUBROUTINE

SUBROUTINE update_forcing(grid_id, grid_type, lookup, f_var, read_T_input)
  USE timepars_ose
  USE parampars
  IMPLICIT NONE
  !WILL RETRIEVE ARRAY IN 2D OR 3D.
  !If array_2D, array_old_2D are present, will work in 2D
  !If array_3D, array_old_3D are present, will work in 3D

  INTEGER, INTENT(IN) :: grid_id, grid_type, lookup
  type(ForcingVar), intent(inout) :: f_var
  INTEGER :: file_duration, lookup_time
  INTEGER :: id_time, id_time_prev
  INTEGER :: time, time_prev, time_array, time_array_old
  INTEGER :: last_time_nextf !next time for the next array in case of backaward in time, to find the upper bound
  LOGICAL, INTENT(IN), OPTIONAL :: read_T_input !if true will convert to K from °C
  LOGICAL :: read_T

  !find which time in the forcing we should be at
  SELECT CASE (grid_type)
      CASE(wave_id)
        file_duration = oserit_param%file_duration_wave(grid_id)
        lookup_time = oserit_param%lookup_wav(grid_id,4)
      CASE(hydro_id)
        file_duration = oserit_param%file_duration_hydro(grid_id)
        lookup_time = oserit_param%lookup_cur(grid_id,5)
      CASE(wind_id)
        file_duration = oserit_param%file_duration_meteo(grid_id)
        lookup_time = oserit_param%lookup_win(grid_id,4)
  END SELECT

  last_time_nextf = -HUGE(last_time_nextf)

  IF(oserit_param%iopt_backtrack == 1 &
    &.AND. domains(grid_id)%forcings(grid_type)%times(1) /= domains(grid_id)%forcings(grid_type)%times_old(1))THEN
      last_time_nextf = domains(grid_id)%forcings(grid_type)%times(SIZE(domains(grid_id)%forcings(grid_type)%times))
  END IF
  time_prev = -1
  id_time_prev = get_id_time(time_since_epoch_prev, domains(grid_id)%forcings(grid_type)%times_old,&
                              &file_duration, oserit_param%iopt_backtrack, last_time_nextf)

  IF(id_time_prev > 0 .AND. id_time_prev <= SIZE(domains(grid_id)%forcings(grid_type)%times_old))THEN
      time_prev = domains(grid_id)%forcings(grid_type)%times_old(id_time_prev)
  !will happens after copying the data and moving to the next file for the old_array. This allows to looks for time in the next array
  ELSE IF(id_time_prev <= 0 .AND. oserit_param%iopt_backtrack == 1 &
        &.OR. id_time_prev > SIZE(domains(grid_id)%forcings(grid_type)%times_old) &
        & .AND. oserit_param%iopt_backtrack == 0) THEN

    id_time_prev = get_id_time(time_since_epoch_prev, domains(grid_id)%forcings(grid_type)%times,&
                                & file_duration, oserit_param%iopt_backtrack)
    IF(id_time_prev > 0 .AND. id_time_prev <= SIZE(domains(grid_id)%forcings(grid_type)%times))THEN

        time_prev = domains(grid_id)%forcings(grid_type)%times(id_time_prev)
        DEALLOCATE(domains(grid_id)%forcings(grid_type)%times_old)
        ALLOCATE(domains(grid_id)%forcings(grid_type)%times_old(SIZE(domains(grid_id)%forcings(grid_type)%times)))
        domains(grid_id)%forcings(grid_type)%times_old = domains(grid_id)%forcings(grid_type)%times
    END IF
  END IF

  time = -1
  id_time = get_id_time(time_since_epoch, domains(grid_id)%forcings(grid_type)%times, file_duration, 1-oserit_param%iopt_backtrack)
  IF(id_time > 0 .AND. id_time <= SIZE(domains(grid_id)%forcings(grid_type)%times)) &
      &time = domains(grid_id)%forcings(grid_type)%times(id_time)

  IF(time_prev == f_var%time_old)THEN
      !the prev array does not needs reload
  ELSE IF(time_prev == f_var%time)THEN
    !the prev array can be put at the new one
    f_var%var_old = f_var%var
    f_var%time_old = f_var%time
  ELSE
    IF(PRESENT(read_T_input))THEN
      read_T = read_T_input
    ELSE
      read_T = .FALSE.
    END IF
    !need reload the grid (maybe) and the var(for sure)
    CALL load_grid(grid_id, grid_type, time_since_epoch_prev, oserit_param%iopt_backtrack, last_time_nextf)
    IF(f_var%is_3D)THEN
      CALL load_var(grid_id, grid_type, lookup, time_since_epoch_prev, oserit_param%iopt_backtrack, &
              &read_T, f_var%time_old, array_3D = f_var%var_old)
    ELSE
      CALL load_var(grid_id, grid_type, lookup, time_since_epoch_prev, oserit_param%iopt_backtrack,&
                    &read_T, f_var%time_old, array_2D = f_var%var_old(:,:,1))
    END IF


    DEALLOCATE(domains(grid_id)%forcings(grid_type)%times_old)
    ALLOCATE(domains(grid_id)%forcings(grid_type)%times_old(SIZE(domains(grid_id)%forcings(grid_type)%times)))
    domains(grid_id)%forcings(grid_type)%times_old = domains(grid_id)%forcings(grid_type)%times
    !update the time, because there is now a new file that has been read
    time = -1
    id_time = get_id_time(time_since_epoch, domains(grid_id)%forcings(grid_type)%times, &
                          & file_duration, 1-oserit_param%iopt_backtrack)
    IF(id_time > 0 .AND. id_time <= SIZE(domains(grid_id)%forcings(grid_type)%times)) &
        &time = domains(grid_id)%forcings(grid_type)%times(id_time)

  END IF
  IF(time == f_var%time)THEN
      !the prev array does not needs reload
  ELSE
    IF(PRESENT(read_T_input))THEN
      read_T = read_T_input
    ELSE
      read_T = .FALSE.
    END IF
    !need reload the grid (maybe) and the var(for sure)
    CALL load_grid(grid_id, grid_type, time_since_epoch, 1-oserit_param%iopt_backtrack)
    
    IF(f_var%is_3D)THEN
      CALL update_depth_forcing(grid_id)
      CALL load_var(grid_id, grid_type, lookup, time_since_epoch, 1-oserit_param%iopt_backtrack,&
                    &read_T, f_var%time, array_3D = f_var%var)
    ELSE
      CALL load_var(grid_id, grid_type, lookup, time_since_epoch, 1-oserit_param%iopt_backtrack,&
              &read_T, f_var%time, array_2D = f_var%var(:,:,1))
    END IF

  END IF

END SUBROUTINE

function update_and_interp(loc, grid_id, grid_type, lookup, time, f_var, need_update, read_T) result(value)
  ! update the variable if needed and interpolate it
  use timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !location
  integer, intent(in) :: grid_id !index of the grid
  integer, intent(in) :: grid_type !wave, wind ...
  integer, intent(in) :: lookup ! lookup in the netcdf
  integer, intent(in) :: time ! time to interpolate the time on it
  logical, optional, intent(in) :: read_T ! if this is a temperature, by default .false.
  logical, optional, intent(in) :: need_update ! if the forcing should be updated, by default .true.
  real :: T1, T2
  !only one of the 2 should be given
  type(ForcingVar) :: f_var

  real, dimension(:,:,:), allocatable :: depth_ar, depth_ar_old

  real :: value, ratio, next_val, old_val
  logical :: need_update_val, read_T_val


  CALL cpu_time(T1)


  need_update_val = .true.
  read_T_val = .false.

  if(present(need_update))  need_update_val = need_update
  if(present(read_T))  read_T_val = read_T

  if(need_update_val) CALL update_forcing(grid_id, grid_type, lookup, f_var, read_T_input = read_T_val)

  if(f_var%is_3D)then
    !depth only from hydro for now
    next_val = get_value(loc, domains(grid_id)%forcings(grid_type), array_3D = f_var%var, &
                          & depth_ar = domains(grid_id)%forcings(hydro_id)%depth_data%depth%var)
    old_val = get_value(loc, domains(grid_id)%forcings(grid_type), array_3D = f_var%var_old, &
                          & depth_ar = domains(grid_id)%forcings(hydro_id)%depth_data%depth%var_old)
  else 
    next_val = get_value(loc, domains(grid_id)%forcings(grid_type), array_2D = f_var%var(:,:,1))
    old_val = get_value(loc, domains(grid_id)%forcings(grid_type), array_2D = f_var%var_old(:,:,1))
  end if

  ratio = interp_tmstp(f_var%time_old, time, f_var%time)
  value = old_val * (1-ratio) + next_val * ratio


  call cpu_time(T2)

  time_interp_update = time_interp_update + T2 - T1

end function

function to_east_trigo(raw_angle, grid, params) result(angle)
  ! this will transform the raw angle (REAL in degree) to an angle usable by the model
  ! this means the angle toward the east in rotation coucnterclockwise. params is the simulation parameters
  ! grid is the id of the grid
  use datatypes_ose
  implicit none
  real, intent(in) :: raw_angle
  type(oserit_parameters), intent(in) :: params
  integer, intent(in) :: grid
  real :: angle

  angle = raw_angle + params%direction_dirw(grid) !to put toward the east
  
  if (params%iopt_clockwise_dirw(grid)) angle = 360 - angle !to convert to counterclockwise

end function

SUBROUTINE load_grid(grid_id, grid_type, tg_time, next_time, last_prev)
  !wrapper for load_grid_any
  USE partpars
  USE netcdf
  USE parampars
  USE time_routines_ose
  USE timepars_ose
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: grid_id, grid_type, tg_time, next_time ! next_time is if at the end (1) or at the start (0) of the interval
  INTEGER, OPTIONAL, INTENT(IN) :: last_prev !last value of the previous file, if known. Allow to detect for upper bound in a previous file for backward

  integer :: previous_last_time, id_time, size_time
  integer :: lookup_time_len, lookup_time
  integer :: status

  CHARACTER (LEN=max_length_string), ALLOCATABLE :: error
  character(len = nf90_max_name):: name

  REAL :: T1, T2
  INTEGER :: id_to_read, dim_time, forcing_time, file_duration

  previous_last_time = -1

  IF(PRESENT(last_prev)) previous_last_time = last_prev

  SELECT CASE (grid_type)
      CASE(wave_id)
        file_duration = oserit_param%file_duration_wave(grid_id)
        lookup_time = oserit_param%lookup_wav(grid_id,4)
        lookup_time_len = oserit_param%lookup_wav(grid_id,1)
      CASE(hydro_id)
        file_duration = oserit_param%file_duration_hydro(grid_id)
        lookup_time = oserit_param%lookup_cur(grid_id,5)
        lookup_time_len = oserit_param%lookup_cur(grid_id,1)
      CASE(wind_id)
        file_duration = oserit_param%file_duration_meteo(grid_id)
        lookup_time = oserit_param%lookup_win(grid_id,4)
        lookup_time_len = oserit_param%lookup_win(grid_id,1)

  END SELECT

  id_time = get_id_time(tg_time, domains(grid_id)%forcings(grid_type)%times, file_duration, next_time)
  size_time = SIZE(domains(grid_id)%forcings(grid_type)%times)
  !A new forcing file must be read
  IF(id_time < 1 .OR. id_time > size_time)THEN
    CALL cpu_time(T1)
    IF(next_time == 0)THEN! bottom of the bound
      forcing_time = tg_time
    ELSE IF(id_time > size_time)THEN
      forcing_time = tg_time + (domains(grid_id)%forcings(grid_type)%times(size_time) &
                                  & - domains(grid_id)%forcings(grid_type)%times(size_time-1)-1)
    ELSE
      STOP 'There is a bakcward time for the upper bound in reading of the forcings'
    END IF
    status = nf90_close(ncids(grid_id,grid_type))
    if (status /= nf90_noerr) stop 'unable to close forcing'
    status = nf90_open(get_forcing_file_path(grid_id, grid_type, epoch_to_date(forcing_time)) &
                      &,NF90_NOWRITE, ncids(grid_id,grid_type))
    CALL cpu_time(T2)
    time_loading_file = time_loading_file + T2 - T1
    if (status /= nf90_noerr)THEN
      PRINT*,"Unable to open grid", grid_id, ' grid_type ', grid_type, &
                        & get_forcing_file_path(grid_id, grid_type, epoch_to_date(forcing_time))
      stop nf90_strerror(status)
    END IF
    print*,'new forcing loaded, grid', grid_id, ' grid_type ', grid_type, &
        &' path ',get_forcing_file_path(grid_id, grid_type, epoch_to_date(forcing_time))

    !reallocating the time
    DEALLOCATE(domains(grid_id)%forcings(grid_type)%times)
    status = nf90_inquire_dimension(ncids(grid_id,grid_type), lookup_time_len, name, dim_time)
    if (status /= nf90_noerr) stop 'unable to inquire time dimension'
    print*, "dim of (time)", dim_time
    ALLOCATE(domains(grid_id)%forcings(grid_type)%times(dim_time))

    status = nf90_get_var(ncids(grid_id,grid_type), lookup_time, domains(grid_id)%forcings(grid_type)%times)
    if (status /= nf90_noerr)then
       print*,"unable to load times "
       stop nf90_strerror(status)
    end if


  END IF

END SUBROUTINE


SUBROUTINE load_var(grid_id, grid_type, lookup, tg_time,next_time, read_T, new_time, array_2D, array_3D)
  USE partpars
  USE netcdf
  USE parampars
  USE timepars_ose
  USE time_routines_ose
  !array_2D OR array_3D must be present, not both!!!

  INTEGER, INTENT(IN) :: grid_id, grid_type, lookup, tg_time,next_time
  REAL, DIMENSION(:,:,:), INTENT(INOUT), OPTIONAL :: array_3D
  REAL, DIMENSION(:,:), INTENT(INOUT), OPTIONAL :: array_2D
  INTEGER, INTENT(OUT) :: new_time ! the time of the varialb read
  REAL, DIMENSION(2) :: ar_sc_off !scale and offset of the variables
  INTEGER, ALLOCATABLE, DIMENSION(:) :: len_array ! dimension of the arrays (lon,lat,depth)
  INTEGER :: id_time, lookup_time, status, file_duration
  REAL :: T1, T2
  CHARACTER (LEN=99) :: temperature_units
  LOGICAL, INTENT(IN) :: read_T

  IF(PRESENT(array_2D))THEN
    len_array = SHAPE(array_2D)
  ELSE
    len_array = SHAPE(array_3D)
  END IF

  SELECT CASE (grid_type)
      CASE(wave_id)
        file_duration = oserit_param%file_duration_wave(grid_id)
        lookup_time = oserit_param%lookup_wav(grid_id,4)
      CASE(hydro_id)
        file_duration = oserit_param%file_duration_hydro(grid_id)
        lookup_time = oserit_param%lookup_cur(grid_id,5)
      CASE(wind_id)
        file_duration = oserit_param%file_duration_meteo(grid_id)
        lookup_time = oserit_param%lookup_win(grid_id,4)
  END SELECT

  id_time = get_id_time(tg_time, domains(grid_id)%forcings(grid_type)%times, file_duration, next_time)
  new_time = domains(grid_id)%forcings(grid_type)%times(id_time)

  CALL cpu_time(T1)
  IF(PRESENT(array_2D))THEN
    status = nf90_get_var(ncids(grid_id,grid_type), lookup, array_2D, start=(/1,1,id_time/) &
                          &, count =(/len_array(1),len_array(2),1/))
    !10^20
    WHERE(array_2D > 100000000000000000000.0)array_2D = 0.
    WHERE(array_2D < -100000000000000000000.0)array_2D = 0.
  ELSE
    status = nf90_get_var(ncids(grid_id,grid_type), lookup, array_3D, start=(/1,1,1,id_time/) &
                          &, count =(/len_array(1),len_array(2),len_array(3),1/))
    !10^20
    WHERE(array_3D > 100000000000000000000.0) array_3D = 0.
    WHERE(array_3D < -100000000000000000000.0) array_3D = 0.
  END IF
  CALL cpu_time(T2)
  time_loading_var = time_loading_var + T2 - T1
  if (status /= nf90_noerr) then
    PRINT*,"unable to load variable, lookup id:",lookup," grid_id:", grid_id," grid_type:", grid_type
    PRINT*, "lookup is the variable id in the netcdf, grid_id is the number of the grid defined in the cfg file &
          & and grid_type is: hydro: ",hydro_id,"  wind: ", wind_id, "  wave: ", wave_id
    PRINT*,"The dimension of the variable should be ",len_array,"The model tried to read the time:",id_time
    PRINT*,"error:",status," which should mean:",nf90_strerror(status)
    stop nf90_strerror(status)
  END IF
  ar_sc_off = scale_and_offset(ncids(grid_id,grid_type),lookup)

  IF(read_T .EQV. .TRUE.)THEN !needs to be added if not in K but in °C
    status = nf90_get_att(ncids(grid_id,grid_type),lookup, "units", temperature_units)
    if (status /= nf90_noerr) stop "unable to load temp units"
    IF(temperature_units == "degree_C" .OR. temperature_units == "degree_Celsius"&
      .OR. temperature_units == "degrees_C" .OR. temperature_units == "degrees_Celsius")THEN
      ar_sc_off(2) = ar_sc_off(2) + 273.15
    ELSE IF(temperature_units /= "K")THEN
      print*, "bad temperature units:", temperature_units, "in domains:",grid_id,"with type:", grid_type, "lookup:",lookup
      stop "temperature units not recognized, should be degree_C, degree_Celsius, or K"
    END IF
  END IF
  IF(PRESENT(array_2D))THEN
    array_2D = array_2D * ar_sc_off(1) + ar_sc_off(2)
  ELSE
    array_3D = array_3D * ar_sc_off(1) + ar_sc_off(2)
  END IF
END SUBROUTINE


FUNCTION get_id_time(tg_time, time_forcing, file_duration, next_time, last_prev) RESULT(id_time)
  !Find the time should be read at which index in a forcing file

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: tg_time !the time target (since epoch)
  INTEGER, DIMENSION(:), INTENT(IN) :: time_forcing !the times of the forcing file (since epoch)
  INTEGER, INTENT(IN) :: file_duration ! the duration of the file in second
  INTEGER, INTENT(IN) :: next_time !if needs to take the next (1) or the previous (0) value of the interval in time
  INTEGER, OPTIONAL, INTENT(IN) :: last_prev !last value of the previous file, if known. Allow to detect for upper bound in a previous file for backward
  INTEGER :: id_time, previous_last_time

  previous_last_time = -1

  IF(PRESENT(last_prev)) previous_last_time = last_prev

  IF(next_time == 0)THEN !Read the bottom part
    IF(tg_time >= time_forcing(1)+file_duration)THEN
      id_time = SIZE(time_forcing)+1 !time outside of the vec
    ELSE IF(tg_time < time_forcing(1))THEN
      id_time = 0
    ELSE
      id_time = SIZE(time_forcing)
      DO WHILE(tg_time < time_forcing(id_time))
        id_time = id_time-1
      END DO
    END IF
  ELSE! Read the top part
    IF(tg_time > time_forcing(SIZE(time_forcing)))THEN
      id_time = SIZE(time_forcing)+1!wrong file
    ELSE IF(tg_time <= previous_last_time)THEN
      id_time = 0
    ELSE
      id_time = 1
      DO WHILE (tg_time > time_forcing(id_time))
        id_time = id_time+1
      END DO
    END IF
  END IF



  IF(id_time < 1 .OR. id_time > SIZE(time_forcing))THEN
     PRINT*,"A new file must be opened"
     PRINT*,"The index for reading the time in the netcdf (id_time) is ", id_time, &
            & "it should between 1 and nbr_times (length of the times in the netcdf) :",SIZE(time_forcing)
     PRINT*,"tg_time (time that must be read) is:",tg_time," start_time_forcing(start of the time in the file):",time_forcing(1),&
           & "file_duration:",file_duration, "next_time(if must read the time above(1) or below(0)):",next_time,&
           & "position in the file:",tg_time - time_forcing(1)
  END IF
END FUNCTION

SUBROUTINE prepare_grid(g_par, ncid, lookup, dim_lats, dim_lons)
  !allocate lon lat and time
  use datatypes_ose
  use netcdf
  implicit none
  type(ForcingType), intent(inout) :: g_par
  integer, intent(in) :: ncid
  integer, intent(out) :: dim_lats, dim_lons
  integer, dimension(:) :: lookup 
  integer :: status, nDims, nVars, nGlobalAtts, unlimDimID, dim_time
  character(len = nf90_max_name):: name

  status = nf90_inquire(ncid, ndims, nvars, nglobalatts, unlimdimid)
  if (status /= nf90_noerr) stop 'unable to inquire dimensions for allocation'
  status = nf90_inquire(ncid, ndimensions = ndims, unlimiteddimid = unlimdimid)
  if (status /= nf90_noerr) stop 'unable to inquire n dimensions for allocation'

  status = nf90_inquire_dimension(ncid, lookup(3), name, dim_lats)
  if (status /= nf90_noerr) stop 'unable to inquire lat dimensions for allocation'
  if (name /= 'latitude' .AND. name /= 'lat') stop 'latitude is not corresponding to the name of the dim'
  print*, "dim of (lats)", dim_lats,"  variable name:",name
  ALLOCATE(g_par%lats(dim_lats))
  status = nf90_get_var(ncid, lookup(6), g_par%lats,count=(/dim_lats/))
  if (status /= nf90_noerr) stop "unable to load lats"

  status = nf90_inquire_dimension(ncid, lookup(2), name, dim_lons)
  if (status /= nf90_noerr) stop 'unable to inquire lons dimensions for allocation'
  if (name /= 'longitude' .AND. name /= 'lon') stop 'longitude is not corresponding to the name of the dim'
  print*, "dim of (lons)", dim_lons,"  variable name:",name
  ALLOCATE(g_par%lons(dim_lons))
  status = nf90_get_var(ncid, lookup(5), g_par%lons,count=(/dim_lons/))
  if (status /= nf90_noerr) stop "unable to load lons"

  status = nf90_inquire_dimension(ncid, lookup(1), name, dim_time)
  if (status /= nf90_noerr) stop 'unable to inquire time dimensions for allocation'
  print*, "dim of (time)", dim_time,"  variable name:",name
  ALLOCATE(g_par%times(dim_time))
  ALLOCATE(g_par%times_old(dim_time))
  status = nf90_get_var(ncid,lookup(4), g_par%times,count=(/dim_time/))
  if (status /= nf90_noerr) stop "unable to load times waves"
  print*, "value of times ",g_par%times

  g_par%times_old = g_par%times

END SUBROUTINE

function init_data_forcing_2d(lons, lats, backtrack) result(data_2d)
  ! allocate, initialise and return a type ForcingVar for a 2d variable
  use datatypes_ose
  implicit none
  integer, intent(in) :: lons, lats, backtrack ! nbr of lons, lat and if the sim is backtract or forward
  type(ForcingVar) :: data_2d
  
  data_2d = init_data_forcing_3d(lons, lats, 1, backtrack)
  data_2d%is_3D = .false.
end function

function init_data_forcing_3d(lons, lats, depths, backtrack) result(data_3d)
  ! allocate, initialise and return a type ForcingVar for a 3d variable
  use datatypes_ose
  implicit none
  integer, intent(in) :: lons, lats, depths, backtrack ! nbr of lons, lat, depths and if the sim is backtract or forward
  type(ForcingVar):: data_3d
  ALLOCATE(data_3d%var(lons, lats, depths))
  ALLOCATE(data_3d%var_old(lons, lats, depths))
  data_3d%var = 0.
  data_3d%var_old = 0.
  data_3d%is_3D = .true. ! overwritten in init_data_forcing_2d for 2d vars
  if(backtrack == 0) then
    data_3d%time = -HUGE(data_3d%time)
    data_3d%time_old = -HUGE(data_3d%time_old)
  else
    data_3d%time = HUGE(data_3d%time)
    data_3d%time_old = HUGE(data_3d%time_old)
  end if

end function



END MODULE grid_module
