!************************************************************************
!
! *Lagrangian_Drift* Solve particle transport equations
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

!========================================================================

SUBROUTINE lagr_drift
!************************************************************************
!
! *lagr_drift*
!
! Author -
!
! Last update -
!
! Description -
!
!************************************************************************
!
USE partpars
USE syspars
USE physpars
USE iopars
USE oseswitches
USE timepars
USE timepars_ose
USE osephyspars
USE modstate
USE parampars

#ifdef OHNS
  USE ohns
#endif

IMPLICIT NONE

!
!*Local variables
!

REAL :: dist_r, dist_x, dist_y
INTEGER :: i, j, n, nij, iunit, submerged
REAL :: T1, T2
REAL :: dplx, dply, dplz
REAL :: mu_i, mu_j, sig_i, sig_j, dt

!
! Name          Type  Purpose
!************************************************************************
CALL cpu_time(T1)

!
!---Particle transport due to diffusion
!

CALL diffusion_h_part(delt2d)

!
!---Vertical processes + resuspension
! 
!
IF(iopt_3D.GT.0) THEN
# ifdef OHNS
    call dispersion_OHNS
# endif
  CALL vertical_processes
  IF(oserit_param%iopt_resuspension == 1) CALL resuspension
END IF

!
!---Horizontal transport due to advection
!
CALL advection_h_part
!
!---Update particles position (lat-lon)
!

CALL update_part_h_pos


!
!---check if particle leaves the model domain
!

CALL out_of_domain

!
!---particle seafloor depth
!
CALL update_depth_tot
!
!---Stranding
!

CALL stranding


CALL cpu_time(T2)
time_drift = time_drift+(T2-T1)

RETURN

END SUBROUTINE lagr_drift

!========================================================================

SUBROUTINE advection_h_part

!************************************************************************
!
! *advection_part*
!
! Author - V. Duliere
!
! Last update -
!
! Description - computes the horizontal advection of Lagrangian particles
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************

USE partpars
USE get_vars
USE iopars
USE osephyspars
USE oseswitches
USE physpars
USE rng_library
USE parampars
USE syspars
USE timepars
USE wave_routines
USE modstate

IMPLICIT NONE

!----------------------------------------------------------------!
!*Local variables                                                !
!----------------------------------------------------------------!
!
INTEGER :: nij, drift_state, part_state

REAL :: u_wind, v_wind, u_wind_temp, v_wind_temp
REAL :: u_cur, v_cur, w_cur
REAL :: u_wave, v_wave
REAL :: uvel_part, vvel_part, wvel_part
REAL :: cur_part, wind_part, wave_part
REAL :: dpli, dplj

REAL :: ddw, dcw

REAL, DIMENSION(4) :: dplx, dply, vx, vy
REAL, DIMENSION(4) :: uvel_k, vvel_k
REAL, DIMENSION(4) :: wind_k, cur_k, wave_k
REAL, DIMENSION(4) :: dplx_deg, dply_deg
REAL :: xkrk, ykrk, ct
INTEGER :: k, krk
REAL :: u_cur1, u_cur2, v_cur1, v_cur2, u_wind1, u_wind2, v_wind1, v_wind2
REAL :: u_wave1, v_wave1, u_wave2, v_wave2
REAL :: x_lon, y_lat
REAL :: xran, cback
INTEGER :: it

TYPE(interp_loc) :: loc_krk

! Name        Type    Purpose
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!

!
!---Leeway crosswind direction changes sign with a probability of
!   4% per hour

DO nij=1,nopart
  IF(iopt_leeway == 1)THEN
   CALL rng_standard_uniform(xran,main_numgen)
   part_drift(nij)%ccw = part_drift(nij)%ccw*SIGN(1.,xran-0.04*(delt2d/3600.))
 END IF
ENDDO

!
!---track / backtrack switch
!

cback = -1.*SIGN(1,oserit_param%iopt_backtrack-1)

SELECT CASE(iopt_lag_scheme)
!
! Euler scheme
!--------------------
!
  CASE (1)
    DO nij=1,nopart

!      ---Check wheter the particle has been released yet or not
!      AND if its state is on
       drift_state=part(nij)%drift_state

       IF (drift_state.EQ.2) THEN

          u_cur = 0.
          v_cur = 0.
          IF(iopt_cur_drift.GT.0 .and. part(nij)%state /= st_atm)THEN
            u_cur=get_u_cur_loc(part(nij)%interp_loc)
            v_cur=get_v_cur_loc(part(nij)%interp_loc)
          END IF
!
!         ---interpolate wind forcing at particle location

          u_wind = 0.
          v_wind = 0.
          ! only read the wind at the surface/in the atmosphere
          IF(iopt_wind_drift .GT.0 .and. (part(nij)%state == st_surfc .or. part(nij)%state == st_atm))THEN
            u_wind=get_u_wind_loc(part(nij)%interp_loc) * part_drift(nij)%added_airdrift
            v_wind=get_v_wind_loc(part(nij)%interp_loc) * part_drift(nij)%added_airdrift

            !if not in atmosphere, have cdw and ccw
            if (part(nij)%state == st_surfc)then
              u_wind = part_drift(nij)%cdw *  u_wind + &
                  &   part_drift(nij)%ccw *  v_wind
              v_wind = part_drift(nij)%cdw * v_wind - &
                  &   part_drift(nij)%ccw * u_wind
            endif
          END IF

!         ---get drift due to wave
          u_wave = 0.
          v_wave = 0.
          IF (iopt_wave_drift.GT.0 .and. part(nij)%state /= st_atm) THEN
             CALL wave_velocity(part(nij)%interp_loc,1,u_wave,v_wave)
          ENDIF
!         
          part_drift(nij)%added_airdrift = 1.

!         ---compute the particle advection velocity [m/s]
          uvel_part = alpha_c * u_cur + u_wind + u_wave
          vvel_part = alpha_c * v_cur + v_wind + v_wave

       ELSE
          uvel_part = 0.0
          vvel_part = 0.0
          cur_part  = 0.0
          wind_part = 0.0
          wave_part = 0.0
       ENDIF
!
!      ---save drift values for possible output
       part_drift(nij)%uvel_adv   = uvel_part
       part_drift(nij)%vvel_adv   = vvel_part

       IF(part(nij)%state == st_atm) THEN
         part_drift(nij)%driftv_curh   = 0
         part_drift(nij)%driftv_wind   = 0
         part_drift(nij)%driftv_wave   = 0
       ELSE
         part_drift(nij)%driftv_curh   = SQRT((alpha_c*u_cur)*(alpha_c*u_cur)+ &
             &    (alpha_c*v_cur)*(alpha_c*v_cur))
         part_drift(nij)%driftv_wind   = iopt_wind_drift*SQRT((u_wind*u_wind)+ &
             &     (v_wind*v_wind))
         part_drift(nij)%driftv_wave   = iopt_wave_drift*SQRT(u_wave*u_wave+v_wave*v_wave)
       ENDIF
    ENDDO
!
! Runge-Kutta scheme
!--------------------
!
  CASE(2)

  DO nij=1,nopart
!   Check wheter the particle has been released yet or not
!   AND if its state is on
    drift_state=part(nij)%drift_state

    IF (drift_state.EQ.2) THEN

      cur_k = 0.
      wind_k = 0.
      wave_k = 0.

      DO k=1,4
        dplx_deg(k) = 0.
        dply_deg(k) = 0.
        vx(k)   = 0.
        vy(k)   = 0.
      ENDDO

      DO krk = 1,4
        xkrk = part(nij)%x_lon
        ykrk = part(nij)%y_lat
        IF (krk.eq.2.OR.krk.eq.3) THEN
          xkrk = part(nij)%x_lon + cback*0.5*dplx_deg(krk-1)
          ykrk = part(nij)%y_lat + cback*0.5*dply_deg(krk-1)
        ENDIF
        IF (krk.eq.4) THEN
          xkrk = part(nij)%x_lon + cback*dplx_deg(krk-1)
          ykrk = part(nij)%y_lat + cback*dply_deg(krk-1)
        ENDIF

        IF(krk.EQ.1) THEN
          ct = 0.
        ELSE IF (krk.eq.2.OR.krk.EQ.3) THEN
          ct = 0.5
        ELSE
          ct = 1.
        ENDIF

        loc_krk%x_lon = xkrk
        loc_krk%y_lat = ykrk
        loc_krk%z_depth = part(nij)%z_depth

        !current
        u_cur = 0.
        v_cur = 0.

        IF (iopt_cur_drift.GT.0 .and. part(nij)%state /= st_atm) THEN
          u_cur2=get_u_cur_loc(loc_krk)
          u_cur1=get_u_cur_loc_prev(loc_krk)
          !
          v_cur2=get_v_cur_loc(loc_krk)
          v_cur1=get_v_cur_loc_prev(loc_krk)

          u_cur=u_cur1+ct*(u_cur2-u_cur1)
          v_cur=v_cur1+ct*(v_cur2-v_cur1)

          cur_k(krk) =  alpha_c*SQRT((u_cur*u_cur)+(v_cur*v_cur))
        END IF
! 
        !wind
        u_wind = 0.
        v_wind = 0.

        IF (iopt_wind_drift.GT.0 .and. (part(nij)%state == st_surfc .or. part(nij)%state == st_atm)) THEN
          u_wind2=get_u_wind_loc(loc_krk) * part_drift(nij)%added_airdrift
          u_wind1=get_u_wind_loc_prev(loc_krk) * part_drift(nij)%added_airdrift

          v_wind2=get_v_wind_loc(loc_krk) * part_drift(nij)%added_airdrift
          v_wind1=get_v_wind_loc_prev(loc_krk) * part_drift(nij)%added_airdrift

          u_wind=u_wind1+ct*(u_wind2-u_wind1)
          v_wind=v_wind1+ct*(v_wind2-v_wind1)

          !if not in atmosphere, have cdw and ccw
          if (part(nij)%state == st_surfc)then
            u_wind = part_drift(nij)%cdw *  u_wind + &
              &   part_drift(nij)%ccw *  v_wind
            v_wind = part_drift(nij)%cdw * v_wind - &
              &   part_drift(nij)%ccw * u_wind
          end if

          wind_k(krk)= SQRT((u_wind*u_wind)+(v_wind*v_wind))
        ENDIF

        !wave
        u_wave = 0.
        v_wave = 0.
!
        IF (iopt_wave_drift.GT.0 .and. part(nij)%state /= st_atm) THEN
          CALL wave_velocity(loc_krk,1,u_wave2,v_wave2)
          CALL wave_velocity(loc_krk,0,u_wave1,v_wave1)

          u_wave = u_wave1+ct*(u_wave2-u_wave1)
          v_wave = v_wave1+ct*(v_wave2-v_wave1)

          wave_k(krk)= SQRT(u_wave*u_wave+v_wave*v_wave)
        ENDIF

!        ---compute the particle advection velocity [m/s]
        uvel_k(krk) = alpha_c * u_cur + u_wind + u_wave
        vvel_k(krk) = alpha_c * v_cur + v_wind + v_wave
        
        dplx_deg(krk) = uvel_k(krk) * delt2d
        dply_deg(krk) = vvel_k(krk) * delt2d

        !IF (iopt_grid_sph.EQ.1) THEN
          dplx_deg(krk) = dplx_deg(krk) /  &
                &   (degtorad_d*Rearth *COS(y_lat*degtorad_d))
          dply_deg(krk) = dply_deg(krk) / (degtorad_d*Rearth)
        !ENDIF
      ENDDO
      uvel_part = (uvel_k(1)+2.*uvel_k(2)+2.*uvel_k(3)+uvel_k(4))/6.
      vvel_part = (vvel_k(1)+2.*vvel_k(2)+2.*vvel_k(3)+vvel_k(4))/6.
    
      part_drift(nij)%added_airdrift = 1.

      cur_part = (cur_k(1)+2.*cur_k(2)+2.*cur_k(3)+cur_k(4))/6.
      wind_part = (wind_k(1)+2.*wind_k(2)+2.*wind_k(3)+wind_k(4))/6.
      wave_part = (wave_k(1)+2.*wave_k(2)+2.*wave_k(3)+wave_k(4))/6.


    ELSE
      uvel_part = 0.0
      vvel_part = 0.0
      cur_part  = 0.0
      wind_part = 0.0
      wave_part = 0.0
    ENDIF
!
    part_drift(nij)%uvel_adv   = uvel_part
    part_drift(nij)%vvel_adv   = vvel_part
    part_drift(nij)%driftv_wind   = wind_part
    part_drift(nij)%driftv_curh   = cur_part
    part_drift(nij)%driftv_wave   = wave_part
!
  ENDDO
!
END SELECT

RETURN

END SUBROUTINE advection_h_part

!========================================================================

SUBROUTINE advection_v_part

!************************************************************************
!
! *advection_part*
!
! Author - V. Duliere
!
! Last update -
!
! Description - compute the vertical drift of Lagrangian particles
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************
USE partpars
USE parampars
USE get_vars
USE iopars
USE osephyspars
USE oseswitches
USE physpars
USE syspars
USE timepars
USE modstate

# ifdef OHNS
    USE ohns
# endif

IMPLICIT NONE
!
!*Local variables
!
integer :: nij

part_drift%driftv_wo = 0.
part_drift%driftv_curv = 0.

DO nij=1,nopart
!  Check wheter the particle has been released yet or not
!  AND if its state is on
   IF (part(nij)%drift_state == drst_drift) THEN
      IF(part(nij)%state == st_dis .OR. part(nij)%state == st_colmn .OR. &
              & part(nij)%state == st_disp .OR. part(nij)%state == st_deg) THEN

       if(iopt_cur_drift == 1) part_drift(nij)%driftv_curv = get_w_cur_loc(part(nij)%interp_loc)
#      ifdef OHNS
          part_drift%driftv_wo = compute_buoyancy_ohns(nij)
#      endif
     END IF
   ENDIF
ENDDO

part_drift%wvel_adv = part_drift%driftv_wo + part_drift%driftv_curv

END SUBROUTINE advection_v_part


!========================================================================

SUBROUTINE diffusion_h_part(dt)

!************************************************************************
!
! *diffusion_part* computes the particle velocities associated to
!                  processes of diffusion.
!
! Author - V. Duliere
!
! Last update -
!
! Description - computes the particle velocities associated to
!                  processes of diffusion.
!                  Note that if horizontal surface spreading occurs :
!                  1. If iopt_horiz_spread.EQ.1, the diffusivity
!                     coefficients for surface particle are set to 0.
!                  2. If iopt_horiz_spread.EQ.2, the diffusivity
!                     coefficients are computed from spreading processe
!                     only
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************

USE osephyspars
USE oseswitches
USE parampars
USE partpars
USE syspars
USE rng_library
USE wave_routines
USE timepars
USE modstate
USE time_routines
USE get_vars

# ifdef OHNS
    USE ohns
# endif

IMPLICIT NONE

!
!*Local variables
!
INTEGER :: nij
REAL, DIMENSION(nopart) :: Rn, phi
REAL, DIMENSION(nopart) :: K_x, K_y
REAL, INTENT(IN) :: dt
INTEGER :: itime, drift_state
REAL :: uvel_dif, vvel_dif
REAL :: volume, coef_a, Drho, rho_o, rho_water, abs_drho
INTEGER (KIND=kndilong) :: time_since_release

! Name        Type    Purpose
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!

part_drift%uvel_dif=0.
part_drift%vvel_dif=0.

CALL rng_normal_arr(Rn,nopart,main_numgen,0.,1.,limit=5.)
CALL rng_uniform_arr(phi,nopart,main_numgen,0.,2*pi)

K_x(:) = 0.
K_y(:) = 0.

!
!---Horizontal diffusion
!
IF (iopt_dif_coef_h == 1)THEN
  K_x(:) = K_dif_x
  K_y(:) = K_dif_y

  WHERE(part%state.EQ.st_atm .OR.  part%state.EQ.st_seabed .OR. part%state.EQ.st_out .OR. part%state.EQ.st_strand)
        K_x(:) = 0.
        K_y(:) = 0.
  ENDWHERE
END IF



!
!---Horizontal spreading expressed as a diffusion term
!
# ifdef OHNS
  call spreading_ohns(K_x, K_y, dt)
#endif
!
!---Diffusion velocities
!

WHERE (part%drift_state.EQ.drst_drift)
    part_drift%uvel_dif=Rn*SQRT(4.*(K_x)/dt)*cos(phi)
    part_drift%vvel_dif=Rn*SQRT(4.*(K_y)/dt)*sin(phi)
ENDWHERE

RETURN

END SUBROUTINE diffusion_h_part

!========================================================================

SUBROUTINE diffusion_v_part(dt)

!************************************************************************
!
! *diffusion_part* computes the particle velocities associated to
!                  processes of diffusion.
!
! Author - V. Duliere
!
! Last update -
!
! Description - computes the particle velocities associated to
!                  processes of vertical diffusion.
!
!                  Vertical diffusion for surface particles is set to 0.
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************

USE osephyspars
USE oseswitches
USE parampars
USE partpars
USE syspars
USE rng_library
USE wave_routines
USE timepars
USE modstate
USE get_vars

IMPLICIT NONE

!
!*Local variables
!
INTEGER :: nij
REAL, DIMENSION(nopart) :: Rn, phi, K_spread
REAL, DIMENSION(nopart) :: K_z
REAL, INTENT(IN) :: dt
INTEGER :: drift_state, submerged
REAL :: z_part
REAL :: wvel_dif
REAL :: depmean, H_wave, T_wave, k_wave, c_wave
REAL :: volume, coef_a, rho_o, rho_water


! Name        Type    Purpose
!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
!

part_drift%wvel_dif=0.

K_z(:) = 0.

!
!---Vertical diffusion
!
SELECT CASE(iopt_dif_coef_v)
   CASE(1)
       K_z(:) = K_dif_z
       WHERE(part%state.EQ.st_surfc .OR. part%state.EQ.st_atm .OR. &
              & part%state.EQ.st_seabed .OR. part%state.EQ.st_out .OR. part%state.EQ.st_strand)
          K_z(:) = 0.
       ENDWHERE
   CASE(2)
       DO nij=1,nopart
         IF(part(nij)%drift_state ==  drst_drift) THEN
          IF(part(nij)%state == st_colmn .OR. part(nij)%state == st_disp .OR. &
              & part(nij)%state == st_dis .OR. part(nij)%state == st_deg)THEN
            K_z(nij) = MAX(1.0E-6,get_K_z_loc(part(nij)%interp_loc))
            IF(isnan(K_z(nij))) K_z(nij) = 1.0E-6
          END IF
          END IF
       ENDDO
    CASE(3)
      !Johanssen_1982 needs the waves to work"
      DO nij=1,nopart
        IF(part(nij)%drift_state ==  drst_drift) THEN
          IF(part(nij)%state == st_colmn .OR. part(nij)%state == st_disp .OR. &
              & part(nij)%state == st_dis .OR. part(nij)%state == st_deg)THEN
              
            CALL wave_characteristics(part(nij)%interp_loc,1,H_wave,T_wave,k_wave,c_wave)
            z_part = MAX(0.,-1.*part(nij)%z_depth)
            K_z(nij) = MAX(0.,K_z(nij) + 0.028*(H_wave*H_wave/T_wave)*EXP(-2.*k_wave*z_part))
          END IF
        END IF
      ENDDO
END SELECT



!
!---Diffusion velocities
!
part_drift%wvel_dif=0
part_drift%K_z_prev = 0

CALL rng_normal_arr(Rn,nopart,main_numgen,0.,1.,limit=5.)

WHERE (part%drift_state.EQ.2)
    part_drift%wvel_dif=Rn*SQRT(2.*K_z/dt)
    part_drift%K_z_prev=part_drift%K_z
    part_drift%K_z=K_z
ENDWHERE


RETURN

END SUBROUTINE diffusion_v_part

!========================================================================

SUBROUTINE vertical_processes
!---apply the vertical processes
  USE timepars
  USE partpars
  USE parampars
  USE modstate
  USE osephyspars
  use rng_library
  USE oseswitches
  IMPLICIT NONE
  REAL :: dt, cback
  INTEGER :: i, nij
  REAL, ALLOCATABLE, DIMENSION(:) :: dplz


  ALLOCATE(dplz(nopart))
  dplz = part_drift%intrus_depth

    
  cback = -1.*SIGN(1,oserit_param%iopt_backtrack-1)
  dt = delt2d / oserit_param%nbr_subtmstp_vertical

  DO i = 1,oserit_param%nbr_subtmstp_vertical
    IF(iopt_dif_coef_v /= 0) CALL diffusion_v_part(dt)
    CALL advection_v_part
    CALL grid_velocity
    DO nij = 1,nopart
       IF(part(nij)%drift_state .EQ. drst_drift)THEN ! particle not released/stopped shouldn't move
         dplz(nij) = dplz(nij) &
            & + (cback * dt * (part_drift(nij)%wvel_adv + w_imposed + part_drift(nij)%grid_velocity))
       CALL update_depth(dplz(nij),nij, cback * dt * part_drift(nij)%wvel_dif)
     END IF
    ENDDO
    dplz = 0.0
  END DO
  
  DEALLOCATE(dplz)

END SUBROUTINE


SUBROUTINE update_part_h_pos
  USE timepars
  USE partpars
  USE parampars
  USE oseswitches
  INTEGER :: nij
  REAL :: cback, dplx, dply
  cback = -1.*SIGN(1,oserit_param%iopt_backtrack-1)

  DO nij = 1,nopart
    dplx = cback * delt2d * (part_drift(nij)%uvel_adv + part_drift(nij)%uvel_dif)
    dply = cback * delt2d * (part_drift(nij)%vvel_adv + part_drift(nij)%vvel_dif)
    CALL update_distance(dplx, dply, nij)
    CALL update_loc_latlon_sph(dplx,dply,part(nij)%interp_loc)
  ENDDO

END SUBROUTINE

SUBROUTINE stranding

!************************************************************************
!
! *stranding*
!
! Author - V. Duliere
!
! Last update -
!
! Description -
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************

USE get_vars
USE partpars
USE oseswitches
USE modstate
IMPLICIT NONE

INTEGER :: nij
INTEGER :: A_temp, B_temp, C_temp, D_temp, tot_temp
INTEGER :: state, drift_state

SELECT CASE (iopt_stranding)

  CASE(0)
    DO nij=1,nopart
      IF (part(nij)%state /= st_atm .AND. part(nij)%drift_state /= drst_stop) THEN
        IF (.NOT.get_mask_loc(part(nij)%interp_loc)) THEN
            part(nij)             = part_old(nij)
        ENDIF
      ENDIF
    ENDDO
  CASE(1)
    DO nij=1,nopart
      IF (part(nij)%state /= st_atm .AND. part(nij)%drift_state /= drst_stop) THEN
        IF (.NOT.get_mask_loc(part(nij)%interp_loc)) THEN
          part(nij)              = part_old(nij)
          IF(part(nij)%state == st_surfc)THEN
            part(nij)%drift_state = drst_stop
            part(nij)%state      = st_strand
          END IF
        ENDIF
      ENDIF
    ENDDO
END SELECT

RETURN

END SUBROUTINE stranding

!========================================================================

SUBROUTINE out_of_domain

!************************************************************************
!
! *out_of_domain*
!
! Author - V. Duliere
!
! Last update -
!
! Description - check whether the Lagrangian particles moved out
!               of the model domain
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************

USE partpars
USE modstate
USE grid_module

IMPLICIT NONE

INTEGER :: nij

DO nij=1,nopart
  IF(part(nij)%state /= st_out)THEN
    IF(part(nij)%x_lon <= domains(nogrids)%lon_min .OR. part(nij)%x_lon >= domains(nogrids)%lon_max &
         & .OR. part(nij)%y_lat <= domains(nogrids)%lat_min .OR. part(nij)%y_lat >= domains(nogrids)%lat_max) THEN
      part(nij)%state = st_out
      part(nij)%drift_state = drst_stop
    END IF
  END IF
ENDDO

RETURN

END SUBROUTINE out_of_domain

!========================================================================

SUBROUTINE update_distance(dplx, dply, nij)
!************************************************************************
!
! *out_of_domain*
!
! Author - L. LEPERS
!
! Last update -
!
! Description - compute the distance travelled by each particle
!
! Reference -
!
! Calling program -
!
! External calls -
!
! Module calls -
!
!************************************************************************

USE partpars
USE grid_module
USE modstate

IMPLICIT NONE

INTEGER, INTENT(IN) :: nij
REAL, INTENT(IN) :: dplx, dply
INTEGER :: state

IF (part(nij)%drift_state /= drst_not_released)THEN !particle have been released
  state = part(nij)%state
  part_drift(nij)%dist_state(state+1) = part_drift(nij)%dist_state(state+1) + &
                                            & SQRT(dplx * dplx + dply * dply)
END IF

RETURN

END SUBROUTINE update_distance

SUBROUTINE resuspension
  !resuspend the particle if some conditions are met

  USE get_vars
  USE parampars
  USE partpars
  USE modstate
  USE wave_routines

  IMPLICIT NONE

  INTEGER :: nij

  REAL :: threshold_sq
  REAL :: u_speed_cur, v_speed_cur, u_speed_wave, v_speed_wave, speed_sq

  threshold_sq = oserit_param%resus_min_speed * oserit_param%resus_min_speed !square so no needs to make sqrt

  DO nij=1,nopart
    IF(part(nij)%state == st_seabed .AND. part(nij)%drift_state /= drst_not_released)THEN
      u_speed_cur = 0
      v_speed_cur = 0
      u_speed_wave = 0
      v_speed_wave = 0
      IF(oserit_param%iopt_cur_drift == 1)THEN
        u_speed_cur = get_u_cur_loc(part(nij)%interp_loc)
        v_speed_cur = get_v_cur_loc(part(nij)%interp_loc)
      END IF

      IF(oserit_param%iopt_wave == 1)THEN
        CALL wave_velocity(part(nij)%interp_loc,1,u_speed_wave,v_speed_wave)
      END IF

      speed_sq = (u_speed_cur-u_speed_wave)**2 + (v_speed_cur-v_speed_wave)**2

      IF(speed_sq > threshold_sq)THEN
        part(nij)%state = st_colmn
        part(nij)%drift_state = drst_drift
        part(nij)%z_depth = MIN(-get_depth_tot_loc(part(nij)%interp_loc) + oserit_param%resus_height,0.0)
      END IF

    END IF
  ENDDO


END SUBROUTINE


SUBROUTINE grid_velocity
  !compute the velocity at the location loc of the vertical grid
  USE get_vars
  USE timepars_ose
  USE partpars
  USE modstate
  IMPLICIT NONE
  INTEGER :: nij
  REAL :: depth, depth_prev, coefficient
  DO nij = 1,nopart
       part_drift(nij)%grid_velocity = 0
       IF(part(nij)%drift_state .EQ. drst_drift)THEN ! particle not released/stopped shouldn't move
           !d hauteur/d timestep
          if (part(nij)%state /= st_atm)then
            depth = get_depth_tot_loc(part(nij)%interp_loc)
            depth_prev = get_depth_tot_loc_prev(part(nij)%interp_loc)
            part_drift(nij)%grid_velocity = (part(nij)%interp_loc%z_depth / depth) * ((depth - depth_prev) &
                                              & /(time_since_epoch - time_since_epoch_prev))
          end if
     END IF
  ENDDO



END SUBROUTINE
