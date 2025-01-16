MODULE datatypes_ose
!************************************************************************
!
! *datatypes* Derived type definitions
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

USE syspars

IMPLICIT NONE

!Allows the object to be interpolated on the grid of the forcings
TYPE :: interp_loc
  !INTEGER :: icoord, jcoord, kcoord
  !REAL :: xcoord, ycoord, zcoord
  !INTEGER :: domain
  REAL :: x_lon , y_lat, z_depth !degree decimal and meter, 0 is surface, -100 is 100 meters of depth under water
END TYPE

!---attributes of particles for the Lagrangian model
TYPE, EXTENDS(interp_loc) :: PartMain
   INTEGER :: drift_state, state
   REAL :: depth_tot
END TYPE PartMain

!---attributes of particles related to their drift
TYPE :: PartDrift
   REAL :: cdw, ccw
   REAL :: K_z,K_z_prev
   REAL :: uvel_adv, vvel_adv, wvel_adv
   REAL :: uvel_dif, vvel_dif, wvel_dif
   REAL :: driftv_curh, driftv_curv, driftv_wo, driftv_wind, driftv_wave
   REAL :: intrus_depth
   REAL :: grid_velocity
   REAL :: added_airdrift
   REAL, DIMENSION(:), ALLOCATABLE :: dist_state
   CHARACTER (LEN=lentime) :: CStartDateTime
END TYPE PartDrift


type ForcingVar
  integer :: time, time_old
  real, dimension(:,:,:), allocatable :: var, var_old
  logical :: is_3D
end type

type DepthData
  logical :: depth_set
  real, dimension(:), allocatable :: sigma_layers 
  real, dimension(:,:), allocatable :: depth_mean 
  type(ForcingVar) :: depth_tot
  type(ForcingVar) :: depth
end type

type ForcingType
  integer, dimension(:), allocatable :: times, times_old 
  real, dimension(:), allocatable :: lons, lats 
  type(ForcingVar), dimension(:), allocatable :: vars
  type(DepthData) :: depth_data
end type


type :: Domain
  real :: lon_min, lon_max, lat_min, lat_max
  type(ForcingType), dimension(3) :: forcings
end type

type output_file_data
    character(len=:), allocatable :: filename
    integer :: time_dimid, part_dimid, string_length_dimid, state_dimid
    integer :: time_varid
    integer :: timestep_varid
    integer :: part_varid
    integer :: lon_varid, lat_varid, depth_varid, depth_tot_varid
    integer :: state_varid, part_state_varid, drift_state_varid
    integer :: dist_state_varid
end type


INTEGER, PARAMETER :: max_length_string = 1024

TYPE oserit_parameters

   CHARACTER (LEN=max_length_string) :: run_title
   CHARACTER (LEN=lentime) :: CStartDateTime
   CHARACTER (LEN=lentime) :: CEndDateTime
   INTEGER :: iopt_backtrack

   INTEGER :: iopt_3D
   INTEGER :: iopt_meteo
   INTEGER :: iopt_wind_drift
   REAL :: dwl_min, dwl_max, cwl_min, cwl_max
   INTEGER :: iopt_leeway
   INTEGER :: iopt_cur_drift
   INTEGER :: iopt_wave
   INTEGER :: iopt_wave_drift
   INTEGER :: iopt_stranding
   REAL :: K_dif_x
   REAL :: K_dif_y
   REAL :: K_dif_z
   integer :: iopt_resurfacing
   REAL :: w_imposed
   REAL :: timestep_duration

   INTEGER :: nbr_subtmstp_vertical

   INTEGER :: iopt_gen_cld
   REAL :: length
   REAL :: width
   REAL :: ellipse_thick
   REAL :: orientation
   CHARACTER (LEN=lentime) :: CStartDateTime_ose
   REAL :: y_lat_start
   REAL :: x_lon_start
   REAL :: depth_start
   CHARACTER (LEN=lentime) :: CEndDateTime_ose
   REAL :: y_lat_end
   REAL :: x_lon_end
   REAL :: depth_end
   INTEGER :: nopart
   CHARACTER (LEN=lentime) :: CDTimeRelease_ose


   CHARACTER (LEN=max_length_string), DIMENSION(:), ALLOCATABLE :: io_cur_filename
   CHARACTER (LEN=max_length_string), DIMENSION(:), ALLOCATABLE :: io_win_filename
   CHARACTER (LEN=max_length_string), DIMENSION(:), ALLOCATABLE :: io_wav_filename
   CHARACTER (LEN=max_length_string), DIMENSION(:), ALLOCATABLE :: io_bat_filename
   INTEGER, DIMENSION(:), ALLOCATABLE :: max_lon
   INTEGER, DIMENSION(:), ALLOCATABLE :: max_lat
   INTEGER, DIMENSION(:), ALLOCATABLE :: min_lon
   INTEGER, DIMENSION(:), ALLOCATABLE :: min_lat

   REAL, dimension(:), allocatable :: direction_dirw
   logical, dimension(:), allocatable  :: iopt_clockwise_dirw

   INTEGER, DIMENSION(:,:), ALLOCATABLE :: lookup_cur
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: lookup_win
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: lookup_wav
   INTEGER, DIMENSION(:,:), ALLOCATABLE :: lookup_bat

   INTEGER :: iopt_lag_scheme
   INTEGER :: iopt_dif_coef_h
   INTEGER :: iopt_dif_coef_v
   integer :: iopt_prevent_dif_seabed
   integer :: iopt_prevent_dif_surface
   INTEGER :: norelease_moving
   INTEGER :: iopt_resuspension
   REAL :: resus_height
   REAL :: resus_min_speed

   INTEGER, DIMENSION(:), ALLOCATABLE :: file_duration_wave
   INTEGER, DIMENSION(:), ALLOCATABLE :: file_duration_hydro
   INTEGER, DIMENSION(:), ALLOCATABLE :: file_duration_meteo

   REAL, DIMENSION(:), ALLOCATABLE :: start_time_wave
   REAL, DIMENSION(:), ALLOCATABLE :: start_time_hydro
   REAL, DIMENSION(:), ALLOCATABLE :: start_time_meteo
   INTEGER, DIMENSION(:), ALLOCATABLE ::iopt_uniform_depth

   LOGICAL, DIMENSION(:), ALLOCATABLE :: has_cur, has_wind, has_wave, has_bat !to see which data are in the grid

   INTEGER :: iopt_grid_interp

   real :: max_stockes ! maximum speed of stockes drift if computed on the fly

   integer :: iopt_set_seed

   integer :: seed1 = 123456789
   integer :: seed2 = 123456789

   integer :: iopt_force_last_output = 1
   integer :: tmstp_per_outtmstp = 1

END TYPE oserit_parameters

END MODULE datatypes_ose
