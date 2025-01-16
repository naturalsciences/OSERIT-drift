MODULE partpars
!************************************************************************
!
! *partpars* Parameters for particles
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
USE datatypes_ose

IMPLICIT NONE

!
!1. General parameters
!---------------------
REAL :: eps1 = 1.0E-15
INTEGER :: norelease, nopart, norelease_moving
INTEGER :: lagr_part_state_init = 1
INTEGER :: lagr_part_submerged_init
CHARACTER (LEN=lentime) :: CStartDateTime_ose, CEndDateTime_ose, &
     & CDTimeRelease_ose
TYPE (PartDrift), ALLOCATABLE, DIMENSION(:) :: part_drift
TYPE (PartMain), ALLOCATABLE, DIMENSION(:) :: part, part_old

SAVE

NAMELIST /nopartpars_nml/ CStartDateTime_ose, CEndDateTime_ose, &
     & CDTimeRelease_ose, norelease, nopart, lagr_part_state_init
!
!
! Name                    Type     Purpose
!------------------------------------------------------------------------------
!*eps1*                    REAL    constant
!*norelease*
!*nopart*
!*norelease_moving*        INT     number of releases along path for moving
!                               source only
!*lagr_part_state_init*    INT
!*lagr_part_submerged_init*INT
!*nocomponent*             INT How much component are in the simulation
!*CStartDateTime_ose*      CHAR
!*CEndDateTime_ose*        CHAR
!*CDTimeRelease_ose*       CHAR
!*part_drift*              DERIVED Attributes of drifting particles
!*part*                    DERIVED Attributes of particles
!*part_old*                DERIVED Attributes of pariles at previous time step
!*part_weath*              DERIVED Attributes of particles relative to
!                                  weathering
!*list_comp*               DERIVED Array with all the compoennts in the simulation
!*oil*                     DERIVED Attributes of oil
!
!------------------------------------------------------------------------------

END MODULE partpars
