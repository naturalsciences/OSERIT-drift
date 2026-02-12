module get_vars

use grid_module

implicit none

contains

FUNCTION get_dens_loc(loc)
  USE parampars
  USE timepars_ose
  USE osephyspars
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_dens_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_cur(:,15))
  IF(n > -1)THEN
    get_dens_loc = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,15), &
                                      & time_since_epoch, domains(n)%forcings(hydro_id)%vars(hydro_dens_id))

  ELSE
    get_dens_loc = rho_w * 1000
  END IF


END FUNCTION


FUNCTION get_depth_mean_loc(loc)
  USE parampars
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_depth_mean_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_bat(:,5))
  IF(n > -1)THEN
      get_depth_mean_loc = get_value(loc, domains(n)%forcings(hydro_id), &
                                      & array_2D = domains(n)%forcings(bat_id)%depth_data%depth_mean)
  ELSE
      get_depth_mean_loc = 0
  END IF

END FUNCTION

FUNCTION get_depth_tot_loc(loc)
USE parampars
USE timepars_ose
IMPLICIT NONE
TYPE(interp_loc), INTENT(IN) :: loc !object
REAL :: get_depth_tot_loc, ratio, depth_tot, depth_tot_old
INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_cur(:,12))

  IF(n > -1)THEN
      CALL update_depth_forcing(n)

      depth_tot = get_value(loc, domains(n)%forcings(hydro_id), &
                              & array_2D = domains(n)%forcings(hydro_id)%depth_data%depth_tot%var(:,:,1))
      depth_tot_old = get_value(loc, domains(n)%forcings(hydro_id), &
                                & array_2D = domains(n)%forcings(hydro_id)%depth_data%depth_tot%var_old(:,:,1))
      !TODO VERIFY IF THIS WORK with uniform and non uniform depth!!!!
      IF(depth_tot == depth_tot_old)THEN
          get_depth_tot_loc = depth_tot_old
      ELSE
        ratio = interp_tmstp(domains(n)%forcings(hydro_id)%depth_data%depth_tot%time_old, &
                              & time_since_epoch, &
                              & domains(n)%forcings(hydro_id)%depth_data%depth_tot%time)
        get_depth_tot_loc = depth_tot_old * (1-ratio) + depth_tot * ratio
      END IF
  ELSE
        get_depth_tot_loc = get_depth_mean_loc(loc)
  END IF
END FUNCTION


FUNCTION get_depth_tot_loc_prev(loc)
!SHOULD ALWAYS BE CALLED AFTER get_depth_tot_loc(loc) AT THE SAME LOCATION
USE parampars
USE timepars_ose
IMPLICIT NONE
TYPE(interp_loc), INTENT(IN) :: loc !object
REAL :: get_depth_tot_loc_prev, ratio, depth_tot, depth_tot_old
INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_cur(:,12))

  IF(n > -1)THEN
      depth_tot = get_value(loc, domains(n)%forcings(hydro_id), &
                              & array_2D = domains(n)%forcings(hydro_id)%depth_data%depth_tot%var(:,:,1))
      depth_tot_old = get_value(loc, domains(n)%forcings(hydro_id), &
                              & array_2D = domains(n)%forcings(hydro_id)%depth_data%depth_tot%var_old(:,:,1))
      !TODO VERIFY IF THIS WORK with uniform and non uniform depth!!!!
      IF(depth_tot == depth_tot_old)THEN
          get_depth_tot_loc_prev = depth_tot_old
      ELSE
        ratio = interp_tmstp(domains(n)%forcings(hydro_id)%depth_data%depth_tot%time_old, &
                      & time_since_epoch_prev, &
                      & domains(n)%forcings(hydro_id)%depth_data%depth_tot%time)
        get_depth_tot_loc_prev = depth_tot_old * (1-ratio) + depth_tot * ratio
      END IF
  ELSE
      !trying with the uniform depth if there are not a forcing with zeta
      get_depth_tot_loc_prev = get_depth_mean_loc(loc)
  END IF
END FUNCTION


FUNCTION get_mask_loc(loc)
USE parampars
IMPLICIT NONE
TYPE(interp_loc), INTENT(IN) :: loc !object
LOGICAL :: get_mask_loc
REAL :: depth
INTEGER, DIMENSION(3) :: nearest_neighbor ! indexes of the closest neighbor
INTEGER :: n ! forcing used for the loc

IF(n > -1)THEN
    depth = get_depth_mean_loc(loc)
    IF (depth <= 0 .OR. depth > 10000) THEN
      get_mask_loc = .FALSE.
    ELSE
      get_mask_loc = .TRUE.
    END IF
ELSE
    get_mask_loc = .FALSE.
END IF

END FUNCTION

FUNCTION get_K_z_loc(loc)
USE parampars
USE timepars_ose
IMPLICIT NONE
TYPE(interp_loc), INTENT(IN) :: loc !object
REAL :: get_K_z_loc
INTEGER :: n ! forcing used for the loc

n = get_forcing_id(loc, oserit_param%lookup_cur(:,16))
IF(n > -1)THEN
    get_K_z_loc = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,16), &
        & time_since_epoch, domains(n)%forcings(hydro_id)%vars(hydro_K_z_id))
ELSE
    get_K_z_loc = 0
END IF

END FUNCTION


FUNCTION get_u_cur_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_u_cur_loc, ratio, uvel, uvel_old
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_cur(:,9))

  IF(n > -1)THEN
      get_u_cur_loc = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,9), &
              & time_since_epoch, domains(n)%forcings(hydro_id)%vars(hydro_uvel_id))
  ELSE
      get_u_cur_loc = 0
  END IF
END FUNCTION

FUNCTION get_u_cur_loc_prev(loc)
  !get u cur at previous timestep, should ALWAYS follow a call of get_u_cur_loc at sime location
  USE parampars
  USE timepars_ose
  USE timepars
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_u_cur_loc_prev
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_cur(:,9))

  IF(n > -1)THEN
      get_u_cur_loc_prev = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,9), &
              & time_since_epoch_prev, domains(n)%forcings(hydro_id)%vars(hydro_uvel_id), need_update = .false.)
  ELSE
      get_u_cur_loc_prev = 0
  END IF
END FUNCTION


FUNCTION get_v_cur_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_v_cur_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_cur(:,10))
  IF(n > -1)THEN
      get_v_cur_loc = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,10), &
            & time_since_epoch, domains(n)%forcings(hydro_id)%vars(hydro_vvel_id))
  ELSE
      get_v_cur_loc = 0
  END IF
END FUNCTION


FUNCTION get_v_cur_loc_prev(loc)
  USE parampars
  USE timepars_ose
  !get u cur at previous timestep, should ALWAYS follow a call of get_v_cur_loc at same location
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_v_cur_loc_prev
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_cur(:,10))
  IF(n > -1)THEN
    get_v_cur_loc_prev = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,10), &
              & time_since_epoch_prev, domains(n)%forcings(hydro_id)%vars(hydro_vvel_id), need_update = .false.)
  ELSE
      get_v_cur_loc_prev = 0
  END IF

END FUNCTION

FUNCTION get_w_cur_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_w_cur_loc
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_cur(:,11))
  IF(n > -1)THEN
      get_w_cur_loc = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,11), &
                  & time_since_epoch, domains(n)%forcings(hydro_id)%vars(hydro_wvel_id))
  ELSE
      get_w_cur_loc = 0
  END IF
END FUNCTION


FUNCTION get_T_water_loc(loc) ![K]
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_T_water_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc,oserit_param%lookup_cur(:,13))
  IF(n > -1)THEN
      get_T_water_loc = update_and_interp(loc, n, hydro_id, oserit_param%lookup_cur(n,13), &
                        & time_since_epoch, domains(n)%forcings(hydro_id)%vars(hydro_temp_id), read_T = .true.)
  ELSE
      get_T_water_loc = 0
  END IF
END FUNCTION


FUNCTION get_u_wind_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_u_wind_loc, ratio, u_wind, u_wind_old
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_win(:,7))
  IF(n > -1)THEN
    get_u_wind_loc = update_and_interp(loc, n, wind_id, oserit_param%lookup_win(n,7), &
                              & time_since_epoch, domains(n)%forcings(wind_id)%vars(meteo_uwind_id))
  ELSE
      get_u_wind_loc = 0
  END IF
END FUNCTION

FUNCTION get_u_wind_loc_prev(loc)
  !get u wind at previous timestep, should ALWAYS be called after get_u_wind_loc at the same location
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_u_wind_loc_prev, ratio, u_wind, u_wind_old
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_win(:,7))
  IF(n > -1)THEN
      get_u_wind_loc_prev = update_and_interp(loc, n, wind_id, oserit_param%lookup_win(n,7), &
                                  & time_since_epoch_prev, domains(n)%forcings(wind_id)%vars(meteo_uwind_id), need_update = .false.)
  ELSE
      get_u_wind_loc_prev = 0
  END IF
END FUNCTION


FUNCTION get_v_wind_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_v_wind_loc, ratio, v_wind, v_wind_old
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_win(:,8))
  IF(n > -1)THEN
    get_v_wind_loc = update_and_interp(loc, n, wind_id, oserit_param%lookup_win(n,8), &
                              & time_since_epoch, domains(n)%forcings(wind_id)%vars(meteo_vwind_id))
  ELSE
      get_v_wind_loc = 0
  END IF
END FUNCTION

FUNCTION get_v_wind_loc_prev(loc)
  !get u wind at previous timestep, should ALWAYS be called after get_v_wind_loc at the same location
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_v_wind_loc_prev
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_win(:,8))
  IF(n > -1)THEN
      get_v_wind_loc_prev = update_and_interp(loc, n, wind_id, oserit_param%lookup_win(n,8), &
                                  & time_since_epoch_prev, domains(n)%forcings(wind_id)%vars(meteo_vwind_id), need_update = .false.)
  ELSE
      get_v_wind_loc_prev = 0
  END IF
END FUNCTION



FUNCTION get_air_temp_loc(loc)
  !return the air temperature [K]
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_air_temp_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_win(:,9))
  IF(n > -1)THEN
    get_air_temp_loc = update_and_interp(loc, n, wind_id, oserit_param%lookup_win(n,9), &
                            & time_since_epoch, domains(n)%forcings(wind_id)%vars(meteo_temp_id), read_T = .true.)
  ELSE
      get_air_temp_loc = 283.15
  END IF
END FUNCTION

FUNCTION get_cloud_cover_loc(loc)
  !return the cloud cover (0->100)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_cloud_cover_loc, ratio
  REAL :: lcc, mcc, hcc
  REAL :: lcc_new, lcc_old, mcc_new, mcc_old, hcc_new, hcc_old
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_win(:,10))!must contain all the clouds cover

  IF(n > -1)THEN
      CALL update_forcing(n, wind_id, oserit_param%lookup_win(n,10), domains(n)%forcings(wind_id)%vars(meteo_low_cloud_cover_id))
      CALL update_forcing(n, wind_id, oserit_param%lookup_win(n,11), domains(n)%forcings(wind_id)%vars(meteo_mid_cloud_cover_id))
      CALL update_forcing(n, wind_id, oserit_param%lookup_win(n,12), domains(n)%forcings(wind_id)%vars(meteo_high_cloud_cover_id))

      lcc_new = get_value(loc, domains(n)%forcings(wind_id), &
                            & array_2D = domains(n)%forcings(wind_id)%vars(meteo_low_cloud_cover_id)%var(:,:,1))
      lcc_old = get_value(loc, domains(n)%forcings(wind_id), &
                            & array_2D = domains(n)%forcings(wind_id)%vars(meteo_low_cloud_cover_id)%var_old(:,:,1))
      ratio = interp_tmstp(domains(n)%forcings(wind_id)%vars(meteo_low_cloud_cover_id)%time_old,time_since_epoch, &
                            &  domains(n)%forcings(wind_id)%vars(meteo_low_cloud_cover_id)%time)
      lcc = lcc_old * (1-ratio) + mcc_new * ratio

      mcc_new = get_value(loc, domains(n)%forcings(wind_id), &
                            & array_2D = domains(n)%forcings(wind_id)%vars(meteo_mid_cloud_cover_id)%var(:,:,1))
      mcc_old = get_value(loc, domains(n)%forcings(wind_id), &
                            & array_2D = domains(n)%forcings(wind_id)%vars(meteo_mid_cloud_cover_id)%var_old(:,:,1))
      ratio = interp_tmstp(domains(n)%forcings(wind_id)%vars(meteo_mid_cloud_cover_id)%time_old,time_since_epoch, &
                            &  domains(n)%forcings(wind_id)%vars(meteo_mid_cloud_cover_id)%time)
      mcc = mcc_old * (1-ratio) + mcc_new * ratio

      hcc_new = get_value(loc, domains(n)%forcings(wind_id), &
                            & array_2D = domains(n)%forcings(wind_id)%vars(meteo_high_cloud_cover_id)%var(:,:,1))
      hcc_old = get_value(loc, domains(n)%forcings(wind_id), &
                            & array_2D = domains(n)%forcings(wind_id)%vars(meteo_high_cloud_cover_id)%var_old(:,:,1))
      ratio = interp_tmstp(domains(n)%forcings(wind_id)%vars(meteo_high_cloud_cover_id)%time_old,time_since_epoch, &
                            &  domains(n)%forcings(wind_id)%vars(meteo_high_cloud_cover_id)%time)
      hcc = hcc_old * (1-ratio) + hcc_new * ratio

      lcc = 1-lcc/100
      mcc = 1-mcc/100
      hcc = 1-hcc/100
      get_cloud_cover_loc = 100 * (1-(lcc*mcc*hcc))! between 0-100
  ELSE
     get_cloud_cover_loc = 0
  END IF
END FUNCTION

FUNCTION get_dirw_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_dirw_loc, ratio, dirw, dirw_old
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_wav(:,9))
  IF(n > -1)THEN
    get_dirw_loc = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,9), &
                            & time_since_epoch, domains(n)%forcings(wave_id)%vars(wave_dirw_id))
                                  
    get_dirw_loc = to_east_trigo(get_dirw_loc, n, oserit_param) * pi_d/180
  ELSE
      get_dirw_loc = 0
  END IF
END FUNCTION

FUNCTION get_dirw_loc_prev(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_dirw_loc_prev
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_wav(:,9))
  IF(n > -1)THEN
    get_dirw_loc_prev = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,9), &
                                  & time_since_epoch_prev, domains(n)%forcings(wave_id)%vars(wave_dirw_id), need_update = .false.)

    get_dirw_loc_prev = to_east_trigo(get_dirw_loc_prev, n, oserit_param) * pi_d/180
  ELSE
      get_dirw_loc_prev = 0
  END IF
END FUNCTION



FUNCTION get_Tm_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_Tm_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_wav(:,8))
  IF(n > -1)THEN
      get_Tm_loc = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,8), &
                                  & time_since_epoch, domains(n)%forcings(wave_id)%vars(wave_Tm1_id))
  ELSE
      get_Tm_loc = 0
  END IF
END FUNCTION

FUNCTION get_Tm_loc_prev(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_Tm_loc_prev,ratio, Tm, Tm_old
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_wav(:,8))
  IF(n > -1)THEN
      get_Tm_loc_prev = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,8), &
                                  & time_since_epoch_prev, domains(n)%forcings(wave_id)%vars(wave_Tm1_id), need_update = .false.)
  ELSE
      get_Tm_loc_prev = 0
  END IF
END FUNCTION

FUNCTION get_hstt_loc(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_hstt_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_wav(:,7))
  IF(n > -1)THEN
      get_hstt_loc = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,7), &
                                      & time_since_epoch, domains(n)%forcings(wave_id)%vars(wave_hstt_id))
  ELSE
      get_hstt_loc = 0
  END IF
END FUNCTION

FUNCTION get_hstt_loc_prev(loc)
  USE parampars
  USE timepars_ose
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_hstt_loc_prev
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_wav(:,7))
  IF(n > -1)THEN
    get_hstt_loc_prev = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,7), time_since_epoch_prev, &
                                            & domains(n)%forcings(wave_id)%vars(wave_hstt_id), need_update = .false.)
  ELSE
      get_hstt_loc_prev = 0
  END IF
END FUNCTION

FUNCTION get_u_stockes_loc(loc)
  !return a NaN if the value is not avaialble
  USE parampars
  USE timepars_ose
  use, intrinsic :: ieee_arithmetic
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_u_stockes_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_wav(:,10))
  IF(n > -1)THEN
      get_u_stockes_loc = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,10), &
                                      & time_since_epoch, domains(n)%forcings(wave_id)%vars(wave_u_stockes_id))
  ELSE
      get_u_stockes_loc = ieee_value(get_u_stockes_loc, ieee_quiet_nan)
  END IF
END FUNCTION

FUNCTION get_u_stockes_loc_prev(loc)
  !return a NaN if the value is not avaialble
  USE parampars
  USE timepars_ose
  use, intrinsic :: ieee_arithmetic
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_u_stockes_loc_prev
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_wav(:,10))
  IF(n > -1)THEN
    get_u_stockes_loc_prev = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,10), time_since_epoch_prev, &
                                            & domains(n)%forcings(wave_id)%vars(wave_u_stockes_id), need_update = .false.)
  ELSE
      get_u_stockes_loc_prev = ieee_value(get_u_stockes_loc_prev, ieee_quiet_nan)
  END IF
END FUNCTION

FUNCTION get_v_stockes_loc(loc)
  !return a NaN if the value is not avaialble
  USE parampars
  USE timepars_ose
  use, intrinsic :: ieee_arithmetic
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_v_stockes_loc
  INTEGER :: n ! forcing used for the loc

  n = get_forcing_id(loc, oserit_param%lookup_wav(:,11))
  IF(n > -1)THEN
      get_v_stockes_loc = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,11), &
                                      & time_since_epoch, domains(n)%forcings(wave_id)%vars(wave_v_stockes_id))
  ELSE
      get_v_stockes_loc = ieee_value(get_v_stockes_loc, ieee_quiet_nan)
  END IF
END FUNCTION

FUNCTION get_v_stockes_loc_prev(loc)
  !return a NaN if the value is not avaialble
  USE parampars
  USE timepars_ose
  use, intrinsic :: ieee_arithmetic
  IMPLICIT NONE
  TYPE(interp_loc), INTENT(IN) :: loc !object
  REAL :: get_v_stockes_loc_prev
  INTEGER :: n ! forcing used for the loc
  n = get_forcing_id(loc, oserit_param%lookup_wav(:,11))
  IF(n > -1)THEN
    get_v_stockes_loc_prev = update_and_interp(loc, n, wave_id, oserit_param%lookup_wav(n,11), time_since_epoch_prev, &
                                            & domains(n)%forcings(wave_id)%vars(wave_v_stockes_id), need_update = .false.)
  ELSE
      get_v_stockes_loc_prev = ieee_value(get_v_stockes_loc_prev, ieee_quiet_nan)
  END IF
END FUNCTION




end module get_vars