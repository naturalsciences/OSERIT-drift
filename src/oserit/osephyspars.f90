MODULE osephyspars
!************************************************************************
!
! *osephyspars* Parameters for Oserit physical constant
!
! Author -
!
! $Revision$
!
! $LastChangedDate$
!
! Last update -
!
! Description -
!
!************************************************************************
!

IMPLICIT NONE

!
!1. General parameters
!---------------------
!

REAL, parameter :: alpha_c = 1.0
REAL, parameter :: grav_const = 9.81
REAL, parameter :: rho_w = 1.033
REAL, parameter :: visco_w = 0.000001

!
!2. Drift/Leeway parameters
!
REAL :: cdw, scdw, ccw, sccw
REAL :: ddw1, ddw2, dcw1, dcw2

!
!3. vertical motion
!
REAL :: w_imposed

!
!3. Turbulent diffusion
!

REAL :: K_dif_x ! = 0.25    !.5 !11. ! Murray et al. (1972)
REAL :: K_dif_y ! = 0.25    ! .5 !11. ! Murray et al. (1972)
REAL :: K_dif_z ! = 0.00043 ! First Guess, Scory - PARCEL report, 1992

!
!4. Vertical spreading
!

REAL :: ke_vs    = 0.3  ! coef. evaluated from experiments
REAL :: alpha_vs = 1.50 ! Dimensionless scaling factor depending
                     ! on the sea state (Delvigne and Sweeney, 1988)
REAL :: Low_vs   = 1.   ! vertical length-scale parameter depending on the breaking wave [m]


SAVE

!
! Name            Type     Purpose
!------------------------------------------------------------------------------
!*alpha_c*        REAL     current drift coefficient
!*grav_const*     REAL     gravitationnal acceleration (m/s²)
!*rho_w*          REAL     sea water density (kg/m³)
!*visco_w*        REAL     water molecular viscosity (m²/s)
!*w_imposed*      REAL     imposed vertical velocity (m/s) positive if upwards
!*K_dif_x*        REAL     Horizontal turbulent diffusivity coefficient in x-direction
!*K_dif_y*        REAL     Horizontal turbulent diffusivity coefficient in y-direction
!*K_dif_z*        REAL     Vertical turbulent diffusivity coefficient
!*ke_vs*          REAL     Coef. evaluated from experiments
!*alpha_vs*       REAL     Dimensionless scaling factor depending
!                          on the sea state (Delvigne and Sweeney, 1988)
!*Low_vs*         REAL     vertical length-scale parameter depending
!                          on the breaking wave [m]
!*thick_spread*   REAL     slick thickness thresholds for surface spreading [mm]
!*visco_spread*   REAL     oil viscosities linked to the above slick
!                          thickness thresholds [cSt]
!------------------------------------------------------------------------------

END MODULE osephyspars
