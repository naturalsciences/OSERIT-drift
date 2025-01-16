MODULE modstate
!************************************************************************
!
! *modstate* Model variable states
!
! Author - Ludovic Lepers
!
! $Date: 2021-12-23
!
! Description -
!
! Reference -
!
!************************************************************************
!

IMPLICIT NONE


integer, parameter :: drst_stop = 0, drst_not_released = 1, drst_drift = 2
integer, parameter :: st_surfc = 0, st_colmn = 1, st_disp = 2, st_strand = 3, &
                    & st_seabed = 4, st_out = 5, st_atm = 6, st_dis = 7, st_deg = 8 
integer, parameter :: nbr_state = 9


CONTAINS


END MODULE modstate
