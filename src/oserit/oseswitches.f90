MODULE oseswitches
!************************************************************************
!
! *switches* Oserit switches
!
! Author - Valerie Duliere
!
! $Revision$
!
! $LastChangedDate$
!
! Description -
!
!************************************************************************
!
IMPLICIT NONE

!---LAGRANGIAN MODULE
!--------------------
!
!---Lagrangian module
INTEGER :: iopt_lag = 0
!---Numerical scheme (1/2 - Eulerian/Runge-Kutta)
INTEGER :: iopt_lag_scheme = 2
!---Advection module
INTEGER :: iopt_adv = 1
!---sign change in Leeway drift
INTEGER :: iopt_leeway = 0
!---2D/3D drift
INTEGER :: iopt_3D = 1
!---Stranding
INTEGER :: iopt_stranding = 1
!---Horizontal turbulent diffusivity coefficient
INTEGER :: iopt_dif_coef_h = 1
!---Vertical turbulent diffusivity coefficient
INTEGER :: iopt_dif_coef_v = 1
!---Generator of cloud particle (1 - instead of reading a pre-defined cloud - 0)
INTEGER :: iopt_gen_cld = 1
!---The meteo will be read or not
INTEGER :: iopt_meteo = 0
!---Cartesian or spherical (0/1)
!INTEGER :: iopt_grid_sph = 1


!---FORCING
!------------
!
!---Use ocean current to drive particle drift
INTEGER :: iopt_cur_drift = 0
INTEGER :: iopt_current = 1!TODO REMOVE
!---Current-test follows circular (circ) or straight (stra) lines
CHARACTER (LEN=4) :: iopt_cur_shape = 'circ'

!---Use wind current to drive particle drift
INTEGER :: iopt_wind_drift = 1


INTEGER :: iopt_cloud = 0

!---load the wind
INTEGER :: iopt_wave = 1
!---Use wave to drive particle drift
INTEGER :: iopt_wave_drift = 1

INTEGER :: iopt_grid_interp = 2

SAVE

!NAMELIST /oseswitches_nml/ iopt_lag, iopt_leeway, &
!  & iopt_3D, iopt_weath, iopt_evap,iopt_evap_param,iopt_evap_stranded,  &
!  & iopt_emuls, iopt_dis, iopt_vola, iopt_deg, &
!  & iopt_gen_cld, iopt_current, iopt_gen_cur, iopt_cur_shape, &
!  & iopt_wind, iopt_gen_win, iopt_win_shape, &
!  & iopt_test_case, iopt_lag_scheme

!
! Name                  Type    Purpose
!-------------------------------------------------------------------------------
!*iopt_lag*        INTEGER Disables/enables activation of the Lagrangian
!                               particle module (drift) (0/1)
!*iopt_lag_scheme* INTEGER Choses between numerical schemes (1/2 - Euler/RK)
!*iopt_adv*        INTEGER Disables/enables activation of an advection
!                               particle module (drift) (0/1)
!*iopt_leeway*     INTEGER Allows leeway drift to change sign randomnly
!                               (0/1)
!*iopt_3D*         INTEGER Choses between 2D and 3D drift (0/1)
!*iopt_stranding*  INTEGER allow particle to strand (1)
!*iopt_buoyancy*   INTEGER includes buoyancy effect
!*iopt_dif*        INTEGER Disables/enables activation of a diffusion
!                               particle module (drift) (0/1)
!*iopt_dif_coef_h* INTEGER Uses nul horizontal diffusivity coefficients (0),
!                               predefined horizontal diffusivity coefficients (1),
!                               gets it from the hydrodynamics (2)
!*iopt_dif_coef_v* INTEGER Uses nul vertical diffusivity coefficients (0),
!                               predefined vertical diffusivity coefficients (1),
!                               gets it from the hydrodynamics (2),
!                               or computes it as in Johansen (1982) from wave data (3)
!*iopt_horiz_spread* INTEGER no horizontal spreading (0), (1) - approximated by a diffusion process)
!*iopt_gravity_inertia* INTEGER Disables/enables activation of horizontal surface
!                               spreading due to gravity-inertia (0/1)
!                               Only works if iopt_horiz_spread = 1
!*iopt_nat_disp*   INTEGER No natural dispersion (0)- natural vertical dispersion
!                               based on Tkalich and Chan (2002) (1) - natural vertical
!                               dispersion with predefined and constant dispersion rate (2)
!*iopt_gen_cld*    INTEGER generates a cloud of particle (1 - instead of
!                               reading a pre-defined cloud from an ascii file - 0)
!*iopt_euler*           INTEGER Disables/enables Eulerian module to compute the
!                               advection-diffusion of gridded oil
!                               concentrations for
!                               chemically dispersed oil only
!                                 0 - Disables
!                                 1 - Enables but oil has not been dispersed yet
!                                 2 - Enables and active
!*iopt_weath*      INTEGER Disables/enables activation of a weathering
!                               module (fate) (0/1)
!*iopt_evap*       INTEGER Disables/enables activation of the evaporation
!                               module (fate) (0/1)
!*iopt_evap_param* INTEGER Jones (1) or Brighton (2) parameterization
!                               for oil evaporation
!*iopt_evap_stranded* INTEGER 1 means particle beached will evaporate
!
!*iopt_emuls*      INTEGER Disables/enables activation of the emulsification
!                               module (fate) (0/1)
!*iopt_dis*        INTEGER Disables/enables activation of the dissolution
!                               module (fate) (0/1)
!*iopt_vola*       INTEGER Disables/enables activation of the volatilization
!                               module (fate) (0/1)
!*iopt_deg*        INTEGER Disables/enables activation of the degradation
!                               module (fate) (0/1)
!*iopt_current*    INTEGER Disables/enables ocean currents input
!*iopt_gen_cur*    INTEGER Disables/enables activation of a generator
!                               of ocean current (0/1)
!*iopt_cur_shape*  CHAR    Defines the shape of the current to be
!                               generated - circular or
!                               straight lines (circ/stra)
!*iopt_wind*       INTEGER Disables/enables wind input
!*iopt_gen_win*    INTEGER Disables/enables activation of a generator
!                               of wind (0/1)
!*iopt_win_shape*  CHAR    Defines the shape of the wind to be
!                               generated - circular or
!                               straight lines (circ/stra)
!*iopt_wave_drift* INTEGER Disables/enables wave input
!*iopt_test_cases* INTEGER Disables/enables test-cases simulations
!                               0 - no test-case
!                               1 - test-case 1 - circular ocean current
!                                   on a cartesian grid
!                               2 - test-case 2
!************************************************************************
!

END MODULE oseswitches
