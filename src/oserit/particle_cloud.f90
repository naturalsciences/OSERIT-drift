MODULE particle_cloud
!************************************************************************
!
! *Particle_cloud*
!
! Author - Valerie Duliere
!
! $Revision$
!
! $LastChangedDate$
!
! Description -
!
! Reference -
!
! Routines - cld_generator, read_cld
!
!************************************************************************
!

IMPLICIT NONE

CONTAINS

SUBROUTINE cld_generator
!************************************************************************
!
! *cld_generator* Velocity arrays as a circular current
!
! Author - Valerie Duliere
!
! Last update -
!
! Description - Generate random positions and StartTime of particles
!
!************************************************************************
!

USE get_vars
USE datatypes_ose
USE partpars
USE syspars
USE physpars
USE iopars
USE time_routines, ONLY: convert_date, error_lbound_var_date
USE time_routines_ose
USE timepars_ose
USE rng_library
USE modstate
USE oseswitches
USE parampars

IMPLICIT NONE

!
!*Local variables
!

REAL :: delxdat, delydat
REAL :: dist_x, dist_y, dist_x_rot, dist_y_rot
REAL :: x_pos, y_pos, r_pos, phi_pos, phi_depth, z_pos
REAL :: sig_i, sig_j, sig_z
REAL :: osin, ocos

INTEGER :: i, j, n, ij, nij, iunit

CHARACTER (LEN=lentime) :: CStartTime
INTEGER, DIMENSION(7) :: IStartDateTime_ose, IEndDateTime_ose, IDTimeRelease_ose
REAL :: DiffTime_ose, daynum1, daynum2, DTime
INTEGER :: iyear, iyear1, iyear2
INTEGER :: numsteps

REAL :: depth_tot_start, depth_tot_end
INTEGER :: part_state

TYPE(interp_loc) :: start_loc, end_loc, part_loc

REAL :: orientation
CHARACTER (LEN=1) :: coma

REAL :: lat_center, lon_center, depth_center
REAL :: x_lon, y_lat
INTEGER :: MovingSource

REAL :: xcoord, ycoord, depth_tot

REAL :: eps = 1.0E-12
REAL :: temp1, temp2
INTEGER :: n_iter

REAL, DIMENSION(16) :: BP_temp, BP_fract, Dif, V_mol, M_mol
INTEGER :: n_fraction

INTEGER, ALLOCATABLE, DIMENSION(:) :: nopart_per_rel

REAL :: temp


!
! Name               Type    Purpose
!*******************************************************************************
!*x_pos*             REAL    random X-distance from the center of the slick [m]
!*y_pos*             REAL    random Y-distance from the center of the slick [m]
!*dist_x_rot*        REAL    random X-distance from the center of the slick
!                              after rotation [m]
!*dist_y_rot*        REAL    random Y-distance from the center of the slick
!                              after rotation [m]
!*dist_x*            REAL    random X-distance from the center of the slick [°]
!*dist_y*            REAL    random Y-distance from the center of the slick [°]
!*delxdat*           REAL    Grid spacing in X-direction in case of regular data grid
!*delydat*           REAL    Grid spacing in Y-direction in case of regular data grid
!*CStartTime*        CHAR    Time of release of the particle
!*oslick_i*          INTEGER i-coordinate of the slick center
!*oslick_j*          INTEGER j-coordinate of the slick center
!*length*            REAL    length of oil slick ellipse (m)
!*width*             REAL    width of oil slick ellipse (m)
!*orientation*       REAL    ellipse orientation (0-North; 90-East; 180-South)
!*opart_i*           INTEGER i-coordinate of the particle
!*opart_j*           INTEGER j-coordinate of the particle
!*sig_i*             REAL    Standard deviation
!*sig_j*             REAL    Standard deviation
!*sig_z*             REAL    Standard deviation
!*BP_temp*           REAL    Boiling point temperatures
!*BP_fract*          REAL    Volume fractions corresponding to
!                            boiling point temperatures
!*Dif*               REAL     Mass diffusivity of the component [m2/s]
!*V_mol*             REAL     Molar volume
!*M_mol*             REAL     Molar weight [kg/mole]
!*n_fraction*        INTEGER Number of fractions for evaporation
!********************************************************************************

coma=','

start_loc%y_lat   = oserit_param%y_lat_start
start_loc%x_lon   = oserit_param%x_lon_start
start_loc%z_depth = oserit_param%depth_start

end_loc%y_lat     = oserit_param%y_lat_end
end_loc%x_lon     = oserit_param%x_lon_end
end_loc%z_depth    = oserit_param%depth_end

orientation = oserit_param%orientation
ocos= COS(orientation*degtorad_d)
osin= SIN(orientation*degtorad_d)

sig_i = oserit_param%width/2.
sig_j = oserit_param%length/2.
sig_z = oserit_param%ellipse_thick/2.

!CALL open_file(iunit,TRIM(runtitle)//'_cld_ics.dat','OUT','A')

!
! CHECK for continuous release
!------------------------------
IStartDateTime_ose = convert_date(CStartDateTime_ose)
IEndDateTime_ose   = convert_date(CEndDateTime_ose)
IDTimeRelease_ose  = convert_date(CDTimeRelease_ose)

CALL date_to_minute(IStartDateTime_ose, daynum1)
CALL date_to_minute(IEndDateTime_ose, daynum2)
CALL date_to_minute(IDTimeRelease_ose, DTime)


!---compute the difference between start and end times of release [min]
iyear1 = IStartDateTime_ose(1)
iyear2 = IEndDateTime_ose(1)
IF (iyear1.EQ.iyear2) THEN
   DiffTime_ose = daynum2 - daynum1
ELSE
   DiffTime_ose = (365 + leap_year(iyear1))*24.0*60.0 - daynum1
   DO iyear=iyear1+1,iyear2-1
       DiffTime_ose = DiffTime_ose + (365 + leap_year(iyear))*24.0*60.0
   ENDDO
   DiffTime_ose = DiffTime_ose + daynum2
ENDIF

! if source is not continuous, DTime parameters are set to 0
IF (DiffTime_ose.EQ.0) THEN
   CDTimeRelease_ose="0000/00/00;00:00:00"
   DTime = 0.00
ELSE IF (DiffTime_ose.LE.7.) THEN
   CDTimeRelease_ose="0000/00/00;00:00:06"
   DTime = 0.10
ELSE IF (DiffTime_ose.LE.30.) THEN
   CDTimeRelease_ose="0000/00/00;00:01:00"
   DTime = 1.00
ELSE
   CDTimeRelease_ose="0000/00/00;00:10:00"
   DTime = 10.00
ENDIF



!
! Check for moving source
!------------------------
!
MovingSource = 0
IF (start_loc%y_lat.NE.end_loc%y_lat) MovingSource = 1
IF (start_loc%x_lon.NE.end_loc%x_lon) MovingSource = 1
IF (start_loc%z_depth.NE.end_loc%z_depth) MovingSource = 1

!
! Number of releases
!--------------------
!
IF (oserit_param%iopt_backtrack.EQ.0) THEN
  IF (DiffTime_ose.GT.0..AND.DTime.GT.0) THEN
    norelease = INT(DiffTime_ose/ DTime)+1
  ELSE IF (MovingSource.GT.0.) THEN
    norelease = norelease_moving
  ELSE
    norelease = 1
  ENDIF
ELSE IF (oserit_param%iopt_backtrack.EQ.1) THEN
  IF (DiffTime_ose.LT.0..AND.DTime.GT.0) THEN
    norelease = INT(ABS(DiffTime_ose)/ DTime)+1
  ELSE IF (MovingSource.GT.0.) THEN
    norelease = norelease_moving
  ELSE
    norelease = 1
  ENDIF
ENDIF
!
! Allocate
!--------------------
!
ALLOCATE(nopart_per_rel(norelease),STAT=errstat)

nopart_per_rel = 0.0

!
! Number of particles released at each release
!----------------------------------------------
!
temp = 1.*nopart/norelease
nopart_per_rel(:)=INT(temp)
IF (norelease.GT.1) nopart_per_rel(norelease)=nopart-(norelease-1)*nopart_per_rel(1)

!
! ALLOCATE ARRAYS
!----------------
!
ALLOCATE(part_drift(nopart), STAT=errstat)
ALLOCATE(part(nopart), STAT=errstat)
ALLOCATE(part_old(nopart), STAT=errstat)

!
! Release depth
!---------------------------------------------
!

depth_tot_start = get_depth_tot_loc(start_loc)

!---check if dry point
IF (.NOT.get_mask_loc(start_loc)) GOTO 1002

start_loc%z_depth = MAX(start_loc%z_depth,-1.*depth_tot_start)


depth_tot_end = get_depth_tot_loc(end_loc)

!---check if dry point
IF (.NOT.get_mask_loc(end_loc)) GOTO 1002

end_loc%z_depth = MAX(end_loc%z_depth,-1.*depth_tot_end)

!
! Compute particles location at release and time of release
!----------------------------------------------------------
!
nij=0
DO ij=1,norelease
   numsteps = (ij-1) * (1-2*oserit_param%iopt_backtrack)

   CALL add_mins_to_date(CStartDateTime_ose,CStartTime,numsteps,CDTimeRelease_ose)
   temp1        = MAX(1.,norelease-1.)
   temp2        = (ij-1.)/temp1
   lon_center   = (1.-temp2)*start_loc%x_lon+(temp2)*end_loc%x_lon
   lat_center   = (1.-temp2)*start_loc%y_lat+(temp2)*start_loc%y_lat
   depth_center = (1.-temp2)*start_loc%z_depth+(temp2)*start_loc%z_depth
   IF ((1.-temp2)*start_loc%z_depth+(temp2)*start_loc%z_depth == 0)THEN
     part_state = st_surfc
   ELSE IF ((1.-temp2)*start_loc%z_depth+(temp2)*start_loc%z_depth < 0)THEN
     part_state = st_colmn
   ELSE
     STOP 'the particle is not starting under the water or at the surface'
   END IF

   DO n=1,nopart_per_rel(ij)
      n_iter = 0
 10   CONTINUE
      part_loc%x_lon=lon_center
      part_loc%y_lat=lat_center
      part_loc%z_depth = depth_center
      IF (n.GT.1) THEN
        CALL rng_uniform_var(r_pos,main_numgen,0.,1.)
        CALL rng_uniform_var(phi_depth,main_numgen,0.,2.*pi)
        CALL rng_uniform_var(phi_pos,main_numgen,0.,2.*pi)
        x_pos = r_pos * sig_i * cos(phi_pos) * sin(phi_depth) ! along minor axis
        y_pos = r_pos * sig_j * sin(phi_pos) * sin(phi_depth)! along major axis
        z_pos = r_pos * sig_z * cos(phi_depth)
      !---rotation
        dist_x_rot=x_pos*ocos+y_pos*osin   ![m]
        dist_y_rot=y_pos*ocos-x_pos*osin   ![m]
      !---conversion from meters to degrees
        dist_x=dist_x_rot/degtorad_d/Rearth/COS(start_loc%y_lat*degtorad_d) ![°] /delxdat
        dist_y=dist_y_rot/degtorad_d/Rearth ![°] /delydat
        part_loc%x_lon=lon_center+dist_x
        part_loc%y_lat=lat_center+dist_y
        part_loc%z_depth=depth_center+z_pos
      ENDIF

      !---check if on a dry point
      IF(n_iter.GE.100) GOTO 1002
      IF (.NOT.get_mask_loc(part_loc)) GOTO 10

      nij = nij + 1

      part_drift(nij)%CStartDateTime = CStartTime
      part(nij)%x_lon   = part_loc%x_lon
      part(nij)%y_lat   = part_loc%y_lat
      part(nij)%z_depth = part_loc%z_depth
      part(nij)%state = part_state


   ENDDO
ENDDO

DEALLOCATE(nopart_per_rel)

RETURN

1001 FORMAT(1(I6,A1))
1003 FORMAT(1(I6,A1),A23,A1,2(F11.7,A1),2(F11.2,A1)1(I6,A1))

1002 nerrs = 1
IF (errchk.AND.nerrs.LE.maxerrors) THEN
   WRITE (ioerr,'(A)') 'Particles on dry points or out of the domain '
ENDIF

END SUBROUTINE cld_generator

!========================================================================

SUBROUTINE read_cld
!************************************************************************
!
! *read_cld*
!
! Author - V Duliere
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
USE iopars
USE oseswitches
USE parampars
USE json_module

IMPLICIT NONE

!
! Local Variables
!

INTEGER :: nij, n_t, n_p, nbr_times, nbr_parts, state
CHARACTER :: coma = ','
REAL :: eps = 1.0E-12
LOGICAL :: found
character(len=:),allocatable :: num_time, num_part, string, CStartTime

type(json_file), TARGET :: json
type(json_file), POINTER :: pjson

!
! Name          Type    Purpose
!*******************************************************************************
!*CStartTime*   CHAR    Time of release of the particle
!*oslick_i*     INTEGER i-coordinate of the slick center
!*oslick_j*     INTEGER j-coordinate of the slick center
!*opart_i*      INTEGER i-coordinate of the particle
!*opart_j*      INTEGER j-coordinate of the particle
!*BP_temp*      REAL    Boiling point temperatures
!*BP_fract*     REAL    Volume fractions corresponding to
!                   boiling point temperatures
!*Dif*          REAL     Mass diffusivity of the component [m2/s]
!*V_mol*        REAL     Molar volume
!*M_mol*        REAL     Molar weight
!*n_fraction*   INTEGER Number of fractions for evaporation
!
!-------------------------------------------------------------------------------

PRINT*, "json initial cloud should be at :" ,TRIM(input_dir)//'/'//TRIM(runtitle)//'_initial_location.json'

pjson=>json
call json%initialize()
call json%load_file(filename=TRIM(input_dir)//'/'//TRIM(runtitle)//'_initial_location.json')

ALLOCATE(part_drift(nopart), STAT=errstat)
ALLOCATE(part(nopart), STAT=errstat)
ALLOCATE(part_old(nopart), STAT=errstat)

nij = 1
CALL json%info('features',n_children=nbr_times)
num_time = '00000'

DO n_t=1,nbr_times
    write(num_time,'(i5.5)') n_t
    call json%get('features('//num_time//').properties.time',CStartTime,found)
    if (.not. found) stop 'initial_location.json: time undefined'

    call json%get('features('//num_time//').properties.state',state,found)
    if (.not. found) state = -1

    CALL json%info('features('//num_time//').geometry.coordinates',n_children=nbr_parts)
    num_part = '00000'

    DO n_p=1,nbr_parts
      ! check that there are not too many particles defined
      if(nij > nopart) stop 'there are more particle in initial_location.json than nopart'

      num_part = '0000000000'
      write(num_part,'(i10.10)') n_p

      call json%get('features('//num_time//').geometry.coordinates('//num_part//').(1)',string,found)
      if (.not. found) stop 'initial_location.json: lon undefined'
      read(string,*) part(nij)%x_lon

      call json%get('features('//num_time//').geometry.coordinates('//num_part//').(2)',string,found)
      if (.not. found) stop 'initial_location.json: lat undefined'
      read(string,*) part(nij)%y_lat

      call json%get('features('//num_time//').geometry.coordinates('//num_part//').(3)',string,found)
      if (.not. found) stop 'initial_location.json: z undefined'
      read(string,*) part(nij)%z_depth

      !if the state is defined in the file, it is readed from there 
      if (state == -1)then
        part(nij)%state = 0
        if (part(nij)%z_depth < 0) part(nij)%state = 1
      else
        part(nij)%state = state
      end if

      part_drift(nij)%CStartDateTime = CStartTime

      nij = nij + 1
    ENDDO
ENDDO

if(nij-1 /= nopart)then
  PRINT*, "expected:",nopart,"amount:",nij-1
  stop "not the good amount of particle in initial_location.json or file not found"
end if
!
!---Close files
!

call json%destroy()

RETURN

1001 FORMAT(1(I6,A1))
1003 FORMAT(1(I6,A1),A23,A1,2(F11.7,A1),2(F11.2,A1)1(I6,A1))

END SUBROUTINE read_cld

!========================================================================

SUBROUTINE particle_init
!************************************************************************
!
! *particle_init*
!
! Author - V Duliere
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
USE iopars
USE oseswitches
USE parampars
USE get_vars
USE modstate

IMPLICIT NONE

!
! Local Variables
!

INTEGER :: i, j, n, ij, nij, iunit
REAL :: lat, lon, depth
CHARACTER (LEN=lentime) :: CStartTime
CHARACTER :: coma = ','
REAL :: eps = 1.0E-12
REAL, DIMENSION(16) :: BP_temp, BP_fract, Dif, V_mol, M_mol
INTEGER :: n_fraction

!
! Name          Type    Purpose
!*******************************************************************************
!*CStartTime*   CHAR    Time of release of the particle
!*lat*          REAL    latitude
!*lon*          REAL    longidute
!*depth*        REAL    depth
!*BP_temp*      REAL    Boiling point temperatures
!*BP_fract*     REAL    Volume fractions corresponding to
!                       boiling point temperatures
!*Dif*          REAL    Mass diffusivity of the component [m2/s]
!*V_mol*        REAL    Molar volume
!*M_mol*        REAL    Molar weight
!*n_fraction*   INTEGER Number of fractions for evaporation
!
!--------------------------------------------------------------------------------

DO nij=1,nopart

     part(nij)%drift_state       = drst_not_released

     part(nij)%depth_tot = get_depth_tot_loc(part(nij)%interp_loc)
     part(nij)%interp_loc%z_depth = MAX(part(nij)%interp_loc%z_depth,-part(nij)%depth_tot)


     part_drift(nij)%cdw         = 0.
     part_drift(nij)%ccw         = 0.
     part_drift(nij)%K_z         = 0.
     part_drift(nij)%K_z_prev    = 0.
     part_drift(nij)%driftv_wave = 0.
     part_drift(nij)%driftv_curh = 0.
     part_drift(nij)%driftv_curv = 0.
     part_drift(nij)%driftv_wo   = 0.
     part_drift(nij)%driftv_wind = 0.
     part_drift(nij)%uvel_adv    = 0.
     part_drift(nij)%vvel_adv    = 0.
     part_drift(nij)%wvel_adv    = 0.

    part_drift(nij)%added_airdrift = 1.

     part_drift(nij)%uvel_dif    = 0.
     part_drift(nij)%vvel_dif    = 0.
     part_drift(nij)%wvel_dif    = 0.


     ALLOCATE(part_drift(nij)%dist_state(nbr_state))

     part_drift(nij)%dist_state = 0.0

     part_drift(nij)%intrus_depth= 0.






ENDDO

part_old = part

RETURN

END SUBROUTINE particle_init


function continue_drifting() result(continue_sim)
  ! return false if all particle have stop drifting, otherwise return true
  use partpars
  use modstate

  implicit none
  logical :: continue_sim
  INTEGER, DIMENSION(nopart) :: is_stopped
  is_stopped = 0
  continue_sim = .true.

  WHERE(part(:)%drift_state.EQ.drst_stop) is_stopped(:)=1
  IF(SUM(is_stopped(:)) == nopart) then
    print*,"all particle have stopped to drift"
    continue_sim = .false.
  end if
end function

END MODULE particle_cloud
