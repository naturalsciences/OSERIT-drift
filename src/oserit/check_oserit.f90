MODULE check_oserit
!************************************************************************
!
! *check_oserit* Check oserit model parameters and arrays for errors
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

IMPLICIT NONE

CONTAINS

!========================================================================

SUBROUTINE check_ose_params
   !************************************************************************
   !
   ! *check_ose_params* Check Oserit model parameters
   !
   ! Author -
   !
   ! Last update -
   !
   ! Description -
   !
   ! Reference -
   !
   !************************************************************************
   !
   USE partpars
   USE timepars
   USE timepars_ose
   USE time_routines
   USE oseswitches
   USE parampars
   USE grid_module
   USE partpars
   USE error_routines, ONLY: error_lbound_var

   IMPLICIT NONE

   CHARACTER (LEN=lentime) :: cdatetimex
   CHARACTER (LEN=15) :: cval
   integer :: i

   IF (oserit_param%iopt_backtrack.EQ.0) THEN
      CALL error_lbound_var_date(CEndDateTime,'CEndDateTime',CStartDateTime,.FALSE.)
   ELSE
      CALL error_ubound_var_date(CEndDateTime,'CEndDateTime',CStartDateTime,.FALSE.)
   ENDIF

   CALL num_time_steps(CStartDateTime,CEndDateTime,delt2d,nstep)
   nstep = ABS(nstep)

   CALL error_lbound_var(nstep,'nstep',0,.FALSE.)

   CALL add_secs_to_date(CStartDateTime,cdatetimex,ind_backtrack*nstep,delt2d)

  IF (iopt_wave_drift == 1 .AND. iopt_wave == 0)stop 'wave needed for wave drift'
  IF (iopt_wind_drift == 1 .AND. iopt_meteo == 0)stop 'wind needed for wind drift'
  IF (iopt_cloud == 1 .AND. iopt_meteo == 0)stop 'wind forcings needed for cloud cover'
      
  if(oserit_param%iopt_3D == 1)then
    
    IF(oserit_param%iopt_dif_coef_v == 3 .and. oserit_param%iopt_wave == 0) stop 'iopt_dif_coef_v == 3 needs the waves to work'

  end if


  DO i = 1, nogrids
    if(oserit_param%has_bat(i) .and. (oserit_param%has_cur(i) .eqv. .false.))then
      print*, "The domain",i,"has a bathymetry but no current affected..."
      stop "All domain with bathymmetry should have corresponding current"
    end if
    if(oserit_param%has_cur(i) .and. (oserit_param%has_bat(i) .eqv. .false.))then
      print*, "The domain",i,"has a hydro but no bathymmetry affected..."
      stop "All domain with hydro should have corresponding bathymetry"
    end if
  END DO


  if(oserit_param%iopt_grid_interp == 1)then
      STOP 'nearest neighbor optimised for speed not yet implemented'
  end if

RETURN

END SUBROUTINE check_ose_params



END MODULE check_oserit
