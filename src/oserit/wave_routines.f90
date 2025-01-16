MODULE wave_routines
!************************************************************************
!
! *wave_routines*
!
! Author - V Duliere
!
! $Revision$
!
! $LastChangedDate$
!
! Generic routines - date_to_minute
!
! Defined operators -
!
! Routines -
!
!************************************************************************
!

IMPLICIT NONE

!
! Interfaces
!

CONTAINS

!=================================================================================

SUBROUTINE wave_velocity(loc, itime,u_wave,v_wave)
!*********************************************************************************
!
! *wave_velocity*
!
! Author - V Duliere
!
! Last update -
!
! Description -  Fredsoe & Deigaard (1992); Daniel et al. (2003)
!
! Module calls -
!
!************************************************************************
!
USE oseswitches
USE partpars
USE get_vars
USE timepars
USE parampars

IMPLICIT NONE

!
!*Arguments
!

TYPE(interp_loc), INTENT(IN) :: loc
REAL, INTENT(INOUT) :: u_wave, v_wave
INTEGER, INTENT(IN) :: itime

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*nij*       INTEGER  Particle index
!*itime*    INTEGER   Time switch
!                       = 0 uses variables of previous time step
!                       = 1 uses variables of current time step
!*u_wave*   REAL      Wave velocity in U-direction at particle location
!*v_wave*   REAL      Wave velocity in V-direction at particle locatio
!------------------------------------------------------------------------------
!
!* Local variables
!
REAL :: z, depth_tot, sinh_kdep
REAL :: H_wave, T_wave, k_wave, c_wave
REAL :: vel_wave, L_wave, angle
REAL :: exp_dec

!
! Name          Type     Purpose
!------------------------------------------------------------------------------
!*icoord*    INTEGER  Particle X-index coordinate with respect to lower left
!*jcoord*    INTEGER  Particle Y-index coordinate with respect to lower left
!*xcoord*    REAL     Particle normalised X-coordinate within local reference grid
!*ycoord*    REAL     Particle normalised Y-coordinate within local reference
!                     grid
!*z*            REAL     Particle depth (0 at sea bed)
!*depth_tot*      REAL     Mean water depth at particle location
!*H_wave*       REAL     Wave height (m)
!*T_wave*       REAL     Wave period
!*k_wave*       REAL     Wave number
!*c_wave*       REAL     Wave celerity
!*norm_wave_U*  REAL
!*norm_wave_V*  REAL
!*vel_wave*     REAL     Norm of wave velocity at particle location
!*L_wave*       REAL


!---get wave characteristics at particle location
CALL wave_characteristics(loc, itime,H_wave,T_wave,k_wave,c_wave)



!WARNING = WAM domain does not fully cover the NOS domain
!where data are missing, wave variables are set to 0
IF (T_wave.GT.0.) THEN

  
  u_wave = 0.0
  v_wave = 0.0

   !---depth corrections
   !In very shallow waters, wave velocity explodes.
   !To avoid this, depth (z and depth_tot) are set no lower than 1.m
   !z = MIN(depth_tot, z) ! if the particle is too high
   !z=MAX(z,1.)
   !depth_tot=MAX(depth_tot,1.)

   !--- Compute wave celerity
   !sinh_kdep = sinh(k_wave*depth_tot)
   !vel_wave = 1./(2.*c_wave+eps1)*(pi_d*H_wave/T_wave)*(pi_d*H_wave/T_wave)
   !vel_wave = vel_wave * cosh(2.*k_wave*z)/(sinh_kdep*sinh_kdep)
   !--- Compute wave direction
    IF(itime == 0)THEN
      !angle= get_dirw_loc_prev(loc)
      u_wave = get_u_stockes_loc_prev(loc)
      v_wave = get_v_stockes_loc_prev(loc)
    ELSE
      !angle= get_dirw_loc(loc)
      u_wave = get_u_stockes_loc(loc)
      v_wave = get_v_stockes_loc(loc)
    END IF

    exp_dec = exp(min(loc%z_depth,0.0)*k_wave) !we must be at most at the surface

    u_wave = u_wave * exp_dec
    v_wave = v_wave * exp_dec

    ! stockes must be computed on the fly if not available
    if (isnan(u_wave) .OR. isnan(v_wave))then
      depth_tot = get_depth_tot_loc(loc)

      z = depth_tot + loc%z_depth
      u_wave = 0.0
      v_wave = 0.0

      !---depth corrections
      !In very shallow waters, wave velocity explodes.
      !To avoid this, depth (z and depth_tot) are set no lower than 1.m
      z = MIN(depth_tot, z) ! if the particle is too high
      z=MAX(z,1.)
      depth_tot=MAX(depth_tot,1.)

      !--- Compute wave celerity
      sinh_kdep = sinh(k_wave*depth_tot)
      vel_wave = 1./(2.*c_wave+eps1)*(pi_d*H_wave/T_wave)*(pi_d*H_wave/T_wave)
      vel_wave = vel_wave * cosh(2.*k_wave*z)/(sinh_kdep*sinh_kdep)
      !--- Compute wave direction
      IF(itime == 0)THEN
        angle= get_dirw_loc_prev(loc)
      ELSE
        angle= get_dirw_loc(loc)
      END IF

      u_wave = vel_wave * COS(angle)
      v_wave = vel_wave * SIN(angle)

      !If the bathymmetry used to compute the wave is not the same as the one used here, risk of nan/infinity
      IF(isnan(u_wave) .OR. isnan(v_wave))THEN !check if nan
        u_wave = 0
        v_wave = 0
      ELSE IF(u_wave > oserit_param%max_stockes .OR. v_wave > oserit_param%max_stockes .OR.&
                & u_wave < -oserit_param%max_stockes .OR. v_wave < -oserit_param%max_stockes)THEN !check if infinity
        u_wave = 0
        v_wave = 0
      END IF

    end if


ENDIF

RETURN

END SUBROUTINE wave_velocity

!========================================================================

SUBROUTINE wavecel(T,h,c)
!************************************************************************
!
! *wavecel* calculates the wave celerity using direct solution of
!  hunt j.n., reading university
!
! Author - V Duliere
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
USE syspars
USE osephyspars

IMPLICIT none

REAL :: h,c,sigma,T
REAL :: b,y,d,e,a

! Name        Type    Purpose
!------------------------------------------------------------------------------
! grav_const  REAL    gravity acceleration
! h           REAL    mean water depth
! c           REAL    wave celerity
! sigma       REAL    angular wave frequency
! T           REAL    wave period
! b           REAL    dummy variable
! y           REAL    dummy variable
! d           REAL    dummy variable
! e           REAL    dummy variable
! a           REAL    dummy variable
!------------------------------------------------------------------------------
!

sigma = 2.*pi_d/T

!--- calculation using power terms - formulae of hunt, j.n.

! correction since when h> 1., resulting value for c does not make sense
b=grav_const*MAX(1.,h)
y=sigma*sigma*h/grav_const
d=0.1608+y*(0.06321+y*(0.02744+y*0.01))
e=1.0+y*(0.6667+y*(0.3556+y*d))
a=1.0/e
c=sqrt(b/(y+a))

RETURN

END SUBROUTINE wavecel


!=================================================================================

SUBROUTINE wave_characteristics(loc, itime,H_wave,T_wave,k_wave,c_wave)
!*********************************************************************************
!
! *wave_characteristics*
!
! Author -
!
! Last update -
!
! Description -  Fredsoe & Deigaard (1992); Daniel et al. (2003)
!
! Module calls -
!
!************************************************************************
!
USE osephyspars
USE oseswitches
USE partpars
USE get_vars
IMPLICIT NONE

!
!*Arguments
!
INTEGER, INTENT(IN) :: itime
TYPE(interp_loc), INTENT(IN) :: loc
REAL, INTENT(OUT) ::  H_wave, T_wave
REAL, INTENT(OUT) ::  k_wave, c_wave

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*icoord*    INTEGER  Particle X-index coordinate with respect to lower left
!*jcoord*    INTEGER  Particle Y-index coordinate with respect to lower left
!*xcoord*    REAL     Particle normalised X-coordinate within local reference
!                     grid
!*ycoord*    REAL     Particle normalised Y-coordinate within local reference
!                     grid
!*domain*    INTEGER  Particle domain
!*itime*     INTEGER   Time switch
!                       = 0 uses variables of previous time step
!                       = 1 uses variables of current time step

!------------------------------------------------------------------------------
!
!* Local variables
!

REAL :: L_wave, depth_tot

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*H_wave*    REAL     wave height
!*T_wave*    REAL     wave period
!*k_wave*    REAL     wave number
!*c_wave*    REAL     wave celerity

!--- Particle depth
depth_tot =  get_depth_tot_loc(loc)
IF(itime == 0)THEN
  H_wave = get_hstt_loc_prev(loc)
  T_wave = get_Tm_loc_prev(loc)
ELSE
  H_wave = get_hstt_loc(loc)
  T_wave = get_Tm_loc(loc)
END IF

!CALL H_T_wave(icoord,jcoord,xcoord,ycoord,domain,itime,H_wave,T_wave,-1.,-1.)

IF (T_wave.GT.0.) THEN
   !--- Compute wave celerity
   CALL wavecel(T_wave,depth_tot,c_wave)
   L_wave = c_wave * T_wave
   k_wave = 2.*pi_d/L_wave
ELSE
   c_wave = 0.
   L_wave = 0.
   k_wave = 0.
ENDIF

RETURN

END SUBROUTINE wave_characteristics

!========================================================================

END MODULE wave_routines
