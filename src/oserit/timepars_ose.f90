MODULE timepars_ose
!************************************************************************
!
! *timepars* Time parameters for oserit
!
! Author - V Duliere
!
! $Revision$
!
! $LastChangedDate$
!
! Description -
!
!************************************************************************
!
USE syspars

IMPLICIT NONE

INTEGER :: ind_backtrack
INTEGER :: time_since_epoch = 0, time_since_epoch_prev = 0
! times relative to the benchmark
REAL ::  time_drift = 0, time_weath = 0, time_w_out = 0, &
        & time_interp_update = 0, time_loading_var = 0, time_loading_file = 0

SAVE

!
! Name              Type     Purpose
!-------------------------------------------------------------------------------
!*delt2d_ose*       REAL     Time step for 2-D mode   !!!NOT USED!!!                  [s]
!*curstepin_ose*    LOGICAL  .TRUE. for current input time steps
!*metstepin_ose*    LOGICAL  .TRUE. for meteo input time steps
!*wavestepin_ose*   LOGICAL  .TRUE. for wave input time steps
!*curINNstepin_ose* LOGICAL  .TRUE. for current input time steps for
!                           inner model domain
!*metINNstepin_ose* LOGICAL  .TRUE. for meteo input time steps for
!                           inner model domain
!*waveINNstepin_ose*LOGICAL  .TRUE. for wave input time steps for
!                           inner model domain
!*ind_backtrack*    INTERGER  indice for forecast of backtrack (+1/-1)
!
!************************************************************************
!

END MODULE timepars_ose
