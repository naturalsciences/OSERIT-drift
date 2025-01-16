!************************************************************************
!
! *Oserit_Start* Oserit program
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
! Routines - oserit_model
!
!************************************************************************
!

!========================================================================

SUBROUTINE oserit_start
!************************************************************************
!
! *Oserit_Start*
!
! Author - Valérie Dulière
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program - coherens_main, initialise_model
!
!************************************************************************
!

USE partpars
USE oseswitches
USE timepars
USE parampars
USE timepars_ose
USE time_routines
USE time_routines_ose
USE iopars
USE particle_cloud
USE osephyspars
USE rng_library
USE grid_module
USE modstate

#ifdef OHNS
   use ohns
#endif

IMPLICIT NONE

INTEGER :: nij, iunit
INTEGER, DIMENSION(nopart) :: test_land
CHARACTER :: coma = ','

delt2d = oserit_param%timestep_duration
CALL num_time_steps(IStartDateTime,IEndDateTime,delt2d,nstep)


time_since_epoch = date_to_epoch(IDateTime)
IF(oserit_param%iopt_backtrack == 0)THEN
  time_since_epoch_prev = time_since_epoch - INT(delt2d)
ELSE
  time_since_epoch_prev = time_since_epoch + INT(delt2d)
END IF


!
! Forcing
!---------------

CALL init_forcings
PRINT*,""
PRINT*,"Allocate currents"
CALL allocate_current_grids


IF(iopt_meteo == 1)THEN
   PRINT*,""
   PRINT*,"Allocate meteo"
   CALL allocate_wind_grids
END IF

PRINT*,""
PRINT*,"Allocate and read bathymmetry"
CALL allocate_bathy_grids
CALL read_bathy_all

IF(iopt_wave == 1)then
   PRINT*,""
   PRINT*,"Allocate wave"
   CALL allocate_wave_grids
END IF
print*,""
CALL compute_grid_size
print*,""

!
! Generate cloud particles
!--------------------------------
!
!---read or generate some of the particle attributes
IF (iopt_gen_cld.GT.0) THEN
   print*,"Using the cloud generator is deprecated. Please create a particle cloud json file instead."
   CALL cld_generator
ELSE
   CALL read_cld
ENDIF

!---initialize other particle attributes
CALL particle_init

#ifdef OHNS
   call init_OHNS
#endif

!---update drifting status
CALL new_release

!---check wether particles are withing the model domain
CALL out_of_domain

!---check wether particles are on dry point
CALL stranding

!---check if at least some particles are in the water, not on land
test_land(:)=0
WHERE(part(:)%state.EQ.st_out) test_land(:)=1
IF(SUM(test_land(:)).EQ.nopart) GOTO 1002

!---Get drift coefficients

!CALL get_drift_coef
CALL set_drift_coef



RETURN

1002 nerrs = 1
IF (errchk.AND.nerrs.LE.maxerrors) THEN
   WRITE (ioerr,'(A)') 'Particles all out of domain '
ENDIF

END SUBROUTINE oserit_start
