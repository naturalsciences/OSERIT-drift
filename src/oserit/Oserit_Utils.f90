!************************************************************************
!
! *Oserit_Utils* Oserit utility subprograms
!
! Author -
!
! $Revision$
!
! $LastChangedDate$
!
! Description -
!
! Reference -
!
! Routines -
!
!************************************************************************
!

SUBROUTINE deallocate_ose_arrays
!************************************************************************
!
! *deallocate_ose_arrays*
!
! Author -
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program - simulation_end
!
!************************************************************************
!TODO redondant?
USE grid_module
USE oseswitches
USE partpars
USE parampars
USE time_routines, ONLY: log_timer_in, log_timer_out
USE timepars_ose

IMPLICIT NONE

INTEGER :: i, nij

CALL log_timer_in()

CALL close_ncid_forcings
DEALLOCATE (part,part_old,part_drift)

!IF (iopt_out_part.NE.0) THEN
!   DEALLOCATE (ose_tsrvars,ose_tsrgpars,ose_tsrgrd)
!   DEALLOCATE (ose_tsr1d)
!ENDIF

!deallocate forcing grids
DO i = 1, nogrids


  IF(iopt_wave == 1 .AND. oserit_param%has_wave(i))THEN
    !DEALLOCATE (forcings(i)%wave_f%Tm1%data_ar, forcings(i)%wave_f%Tm1%data_ar_old)
    !DEALLOCATE (forcings(i)%wave_f%dirw%data_ar, forcings(i)%wave_f%dirw%data_ar_old)
    !DEALLOCATE (forcings(i)%wave_f%hstt%data_ar, forcings(i)%wave_f%hstt%data_ar_old) !, wavegrid)
  END IF


  !IF (difstepin_ose) DEALLOCATE (forcings(i)%hydro_f%K_z%data_ar)

  IF(iopt_meteo == 1.AND. oserit_param%has_wind(i))THEN
    !DEALLOCATE(forcings(i)%meteo_f%uwind%data_ar, forcings(i)%meteo_f%vwind%data_ar, forcings(i)%meteo_f%airtemp%data_ar, &
    !&  forcings(i)%meteo_f%uwind%data_ar_old, forcings(i)%meteo_f%vwind%data_ar_old, forcings(i)%meteo_f%airtemp%data_ar_old &
    !&  ,forcings(i)%wind_times,forcings(i)%wind_lats,forcings(i)%wind_lons&
    !&  )
    IF(iopt_cloud == 1)THEN
      !DEALLOCATE(forcings(i)%meteo_f%low_cloud_cover%data_ar, forcings(i)%meteo_f%low_cloud_cover%data_ar_old, &
      !          &forcings(i)%meteo_f%mid_cloud_cover%data_ar, forcings(i)%meteo_f%mid_cloud_cover%data_ar_old,&
      !          &forcings(i)%meteo_f%high_cloud_cover%data_ar, forcings(i)%meteo_f%high_cloud_cover%data_ar_old)
    END IF
  END IF
!  IF(iopt_wave == 1.AND. oserit_param%has_wave(i))THEN
!    DEALLOCATE (forcings(i)%wave_times,forcings(i)%wave_lats, forcings(i)%wave_lons)
!  END IF



  IF(oserit_param%has_cur(i))THEN
    !DEALLOCATE(forcings(i)%hydro_f%uvel%data_ar, forcings(i)%hydro_f%vvel%data_ar, forcings(i)%hydro_f%wvel%data_ar,&
    !& forcings(i)%hydro_f%uvel%data_ar_old, forcings(i)%hydro_f%vvel%data_ar_old, forcings(i)%hydro_f%wvel%data_ar_old,&
    !& forcings(i)%g_par(hydro_times, forcings(i)%hydro_lats, forcings(i)%hydro_lons, &
    !&  forcings(i)%hydro_f%depth%data_ar, forcings(i)%hydro_f%depth_tot%data_ar_old)
  END IF

  IF(oserit_param%has_bat(i))THEN
     !DEALLOCATE (forcings(i)%bathy_f%depth_mean)
  END IF


ENDDO
DEALLOCATE(domains)


CALL log_timer_out()

RETURN

END SUBROUTINE deallocate_ose_arrays

SUBROUTINE update_loc_latlon_sph(dplx,dply,loc)
!************************************************************************
!
! *update_part_hcoord_sph* update the particle horizontal coordinate on a
!                      uniform sperical grid (iopt_grid_htype = 1;
!                      iopt_grid_sph = 1)
!
! Author - Valerie Duliere
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program -
!
!************************************************************************
!

!USE iopars
USE partpars
USE physpars
USE syspars

IMPLICIT NONE


REAL, INTENT(IN) :: dplx, dply
TYPE(interp_loc), INTENT(INOUT) :: loc
REAL :: dplx_deg, dply_deg
REAL :: x_lon, y_lat

! Transform to [deg]

dplx_deg = dplx/(degtorad_d*Rearth *COS(loc%y_lat*degtorad_d))
dply_deg = dply/(degtorad_d*Rearth)

loc%x_lon  = loc%x_lon+dplx_deg
loc%y_lat  = loc%y_lat+dply_deg


RETURN

END SUBROUTINE update_loc_latlon_sph

!========================================================================

SUBROUTINE update_loc_latlon_cart(dplx,dply,loc)
!************************************************************************
!
! *update_part_hcoord_cart* update the particle horizontal coordinate on a
!                      uniform sperical grid (iopt_grid_htype = 1;
!                      iopt_grid_sph = 0)
!
! Author - Valerie Duliere
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program -
!
!************************************************************************
!

!USE iopars
USE partpars
!USE physpars
!USE syspars

IMPLICIT NONE


REAL, INTENT(IN) :: dplx, dply
TYPE(interp_loc), INTENT(INOUT) :: loc

loc%x_lon  = loc%x_lon+dplx
loc%y_lat  = loc%y_lat+dply

RETURN

END SUBROUTINE update_loc_latlon_cart

!========================================================================

SUBROUTINE update_depth(dplz,nij,z_turb)
!************************************************************************
!
! *update_vert_position* Update particle depth
!
! Author -
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program -
!
!************************************************************************
!
USE get_vars
USE parampars
USE iopars
USE partpars
USE physpars
USE syspars
USE timepars
USE modstate
# ifdef OHNS
    USE ohns
# endif

IMPLICIT NONE


REAL, INTENT(IN) :: dplz
INTEGER, INTENT(IN) :: nij
real, intent(in) :: z_turb ! vertical displacement due to the turbulence

part(nij)%z_depth = part(nij)%z_depth + dplz + z_turb

!IF above or at the surface, put at the surface, if under, put in the water column
IF(part(nij)%state == st_colmn .AND. part(nij)%z_depth >= 0)THEN
  !only resurfacing if it is activated

  if(oserit_param%iopt_resurfacing == 1)then
    if(oserit_param%iopt_prevent_dif_surface == 0)then
        part(nij)%state = st_surfc
        part(nij)%z_depth = 0
    else !a particle cannot be put at the surface due to the turbulence
        if(part(nij)%z_depth < z_turb)then
          part(nij)%z_depth = part(nij)%z_depth-z_turb
        else 
          part(nij)%state = st_surfc
          part(nij)%z_depth = 0
        end if
    end if
  else
    part(nij)%z_depth = 0
  endif

ELSE IF(part(nij)%state == st_surfc .AND. part(nij)%z_depth < 0)THEN
  part(nij)%state = st_colmn
END IF


IF (part(nij)%depth_tot < -part(nij)%z_depth .AND. part(nij)%state /= st_atm &
                                &  .AND. part(nij)%state /= st_dis &
                                &  .AND. part(nij)%state /= st_disp) THEN !If the particle is at the bottom and not evaporated
  if(oserit_param%iopt_prevent_dif_seabed == 0)then
    part(nij)%state = st_seabed
    part(nij)%drift_state = drst_stop
  else !a particle cannot be put at the seabed due to the turbulence
    if(part(nij)%depth_tot > -(part(nij)%z_depth-z_turb))then
      part(nij)%z_depth = part(nij)%z_depth-z_turb
    else
      part(nij)%state = st_seabed
      part(nij)%drift_state = drst_stop
    endif
  end if

ELSE IF(part(nij)%depth_tot < -part(nij)%z_depth)THEN
   part(nij)%z_depth = -part(nij)%depth_tot
END IF

# ifdef OHNS
  call sinking_ohns(nij)
#endif

!if the particle is not evaporated, it should be at the surface at most
IF(part(nij)%state /= st_atm .AND. part(nij)%z_depth > 0)THEN
  part(nij)%z_depth = 0
END IF


RETURN

END SUBROUTINE update_depth

!========================================================================

SUBROUTINE update_depth_tot
!************************************************************************
!
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program -
!
!************************************************************************
!

USE get_vars
USE partpars
USE modstate

IMPLICIT NONE

INTEGER :: nij


DO nij=1,nopart
  IF(part(nij)%state /= st_out)  part(nij)%depth_tot = get_depth_tot_loc(part(nij)%interp_loc)
END DO

END SUBROUTINE update_depth_tot

!========================================================================

SUBROUTINE new_release
!************************************************************************
!
! *new_release*
!
! Author -
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program -
!
!************************************************************************
!

USE parampars
USE partpars
USE timepars
USE time_routines
USE modstate

INTEGER :: nij

SELECT CASE (oserit_param%iopt_backtrack)

  CASE (0)
     DO nij=1,nopart
       IF((part(nij)%drift_state.EQ.drst_not_released).AND.  &
         & (part_drift(nij)%CStartDateTime.nolater.CDateTime)) &
         &  part(nij)%drift_state=drst_drift
     ENDDO
  CASE (1)
     DO nij=1,nopart
       IF((part(nij)%drift_state.EQ.drst_not_released).AND.   &
          & (part_drift(nij)%CStartDateTime.noearlier.CDateTime)) &
          &  part(nij)%drift_state=drst_drift
   ENDDO
END SELECT

RETURN

END SUBROUTINE new_release

!========================================================================

SUBROUTINE set_drift_coef
!************************************************************************
!
! *get_drift_coef* Compute drift coefficients
!
! Author - S. Legrand
!
! Last update -
!
! Description - Initialize particles with the downwind and crosswind leeway coefficient in particles
!               with uniform random beetwen max and min, with a random sign for leeway
!
! Reference -
!
! Calling program - oserit_start
!
!************************************************************************
!

USE osephyspars
USE datatypes_ose
USE partpars
USE parampars
USE rng_library

IMPLICIT NONE

REAL, ALLOCATABLE, DIMENSION(:) :: direction_ccw
ALLOCATE(direction_ccw(nopart))
CALL rng_uniform_arr(part_drift(:)%cdw,nopart,main_numgen,oserit_param%dwl_min,oserit_param%dwl_max)
CALL rng_uniform_arr(part_drift(:)%ccw,nopart,main_numgen,oserit_param%cwl_min,oserit_param%cwl_max)
CALL rng_uniform_arr(direction_ccw,nopart,main_numgen,-1.0,1.0)
WHERE(direction_ccw < 0)
  part_drift(:)%ccw = -part_drift(:)%ccw
ENDWHERE

DEALLOCATE(direction_ccw)
END SUBROUTINE set_drift_coef




SUBROUTINE print_ose_timer
  USE timepars_ose
  IMPLICIT NONE

  PRINT*,"Time drifting (including forcings reading): ",time_drift," s"
  PRINT*,"Time weathering (including forcings reading): ",time_weath," s"
  print*,"Time printing in files: ", time_w_out, " s"
  PRINT*,"-------------------------------------------"
  PRINT*,"Time reading forcings (including interpolation)",time_interp_update," s"
  PRINT*,"Time drifting + weathering without forcings", time_drift + time_weath - time_interp_update,"s"
  PRINT*,"Time reading forcings + drifting + weathering: ",time_drift + time_weath," s"
  PRINT*,"Time loading var: ",time_loading_var," s"
  PRINT*,"Time loading file: ",time_loading_file," s"
  

END SUBROUTINE
!========================================================================
