MODULE oserit_run_reader
!************************************************************************
!
! *Oserit_Run_Reader
!
! Author -
!
! Version - @(COHERENS) V2.0
!
! Date:
!
! Revision:
!
! Description -
! read and store the input information to start a run with Oserit
!
!************************************************************************
!
USE syspars
IMPLICIT NONE
INTEGER, PARAMETER :: max_length_string = 1024



CONTAINS

SUBROUTINE parameters_reset(my_parameters)
!*********************************************************************************
!
! *oserit_parameters_reset* set oserit parameters to default values
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
!*********************************************************************************
!
USE datatypes_ose
TYPE( oserit_parameters ), INTENT (INOUT) :: my_parameters

my_parameters%run_title           = 'AAA_1'
my_parameters%CStartDateTime      = '0000/00/00;00:00:00:000'
my_parameters%CEndDateTime        = '0000/00/00;00:00:00:000'
my_parameters%iopt_backtrack  = 0

my_parameters%iopt_3D        = 0
my_parameters%iopt_meteo      = 0
my_parameters%iopt_wind_drift= 0
my_parameters%dwl_min                 = 0.02
my_parameters%dwl_max                 = 0.02
my_parameters%cwl_min                 = 0.00
my_parameters%cwl_max                 = 0.00
my_parameters%iopt_leeway    = 0
my_parameters%iopt_cur_drift       = 0
my_parameters%iopt_wave      = 0
my_parameters%iopt_wave_drift= 0
my_parameters%iopt_stranding = 0
my_parameters%K_dif_x             = 0.25
my_parameters%K_dif_y             = 0.25
my_parameters%K_dif_z             = 0.00043
my_parameters%iopt_resurfacing = 0
my_parameters%w_imposed           = 0.
my_parameters%timestep_duration   = 600

my_parameters%iopt_gen_cld   = 1
my_parameters%length              = 0.
my_parameters%width               = 0.
my_parameters%ellipse_thick       = 0.
my_parameters%orientation         = 0.
my_parameters%CStartDateTime_ose  = '0000/00/00;00:00:00'
my_parameters%y_lat_start         = 51.4
my_parameters%x_lon_start         = 2.6
my_parameters%depth_start         = 10.
my_parameters%CEndDateTime_ose    = '0000/00/00;00:00:00'
my_parameters%y_lat_end           = 51.4
my_parameters%x_lon_end           = 2.6
my_parameters%depth_end           = 10.
my_parameters%nopart              = 1
my_parameters%CDTimeRelease_ose   = '0000/00/00;00:00:00'

my_parameters%iopt_lag_scheme      = 2
my_parameters%iopt_dif_coef_h      = 0
my_parameters%iopt_dif_coef_v      = 0
my_parameters%iopt_prevent_dif_seabed = 0
my_parameters%iopt_prevent_dif_surface = 0
my_parameters%norelease_moving          = 2

my_parameters%iopt_resuspension = 0
my_parameters%resus_min_speed = 0
my_parameters%resus_height = 0.0

my_parameters%nbr_subtmstp_vertical = 1

RETURN

END SUBROUTINE parameters_reset

!=================================================================================

SUBROUTINE oserit_parameters_print(my_parameters)
!*********************************************************************************
!
! *oserit_parameters_print* print out the oserit parameters
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
 USE grid_module
 USE datatypes_ose
 IMPLICIT NONE
 INTEGER :: i
 TYPE( oserit_parameters ), INTENT (INOUT) :: my_parameters

  PRINT *,TRIM(my_parameters%run_title)
  PRINT *,TRIM(my_parameters%CStartDateTime)
  PRINT *,TRIM(my_parameters%CEndDateTime)
  PRINT *,my_parameters%iopt_backtrack

  PRINT *,my_parameters%iopt_3D
  PRINT *,my_parameters%iopt_meteo
  PRINT *,my_parameters%iopt_wind_drift
  PRINT *,my_parameters%dwl_min
  PRINT *,my_parameters%cwl_min
  PRINT *,my_parameters%dwl_max
  PRINT *,my_parameters%cwl_max
  PRINT *,my_parameters%iopt_leeway
  PRINT *,my_parameters%iopt_cur_drift
  PRINT *,my_parameters%iopt_wave
  PRINT *,my_parameters%iopt_wave_drift
  PRINT *,my_parameters%iopt_stranding
  PRINT *,my_parameters%K_dif_x
  PRINT *,my_parameters%K_dif_y
  PRINT *,my_parameters%K_dif_z
  print*, my_parameters%iopt_resurfacing
  PRINT *,my_parameters%w_imposed

  PRINT *,my_parameters%iopt_gen_cld
  PRINT *,my_parameters%length
  PRINT *,my_parameters%width
  PRINT *,my_parameters%ellipse_thick
  PRINT *,my_parameters%orientation
  PRINT *,TRIM(my_parameters%CStartDateTime_ose)
  PRINT *,my_parameters%y_lat_start
  PRINT *,my_parameters%x_lon_start
  PRINT *,my_parameters%depth_start
  PRINT *,TRIM(my_parameters%CEndDateTime_ose)
  PRINT *,my_parameters%y_lat_end
  PRINT *,my_parameters%x_lon_end
  PRINT *,my_parameters%depth_end
  PRINT *,my_parameters%nopart
  PRINT *,TRIM(my_parameters%CDTimeRelease_ose)



  DO i=1, nogrids
    IF (my_parameters%has_cur(i))PRINT *,TRIM(my_parameters%io_cur_filename(i))
    IF (my_parameters%iopt_meteo == 1 .AND. my_parameters%has_wind(i))PRINT *,TRIM(my_parameters%io_win_filename(i))
    IF (my_parameters%iopt_wave == 1 .AND. my_parameters%has_wave(i))PRINT *,TRIM(my_parameters%io_wav_filename(i))
    IF (my_parameters%has_bat(i))PRINT *,TRIM(my_parameters%io_bat_filename(i))

    !PRINT *,TRIM(my_parameters%lookup_cur(i,:))
    !PRINT *,TRIM(my_parameters%lookup_win(i,:))
    !PRINT *,TRIM(my_parameters%lookup_wav(i,:))
    !PRINT *,TRIM(my_parameters%lookup_bat(i,:))
  ENDDO

  PRINT *,my_parameters%iopt_lag_scheme
  PRINT *,my_parameters%iopt_dif_coef_h
  PRINT *,my_parameters%iopt_dif_coef_v
  PRINT *,my_parameters%norelease_moving


  PRINT *,my_parameters%nbr_subtmstp_vertical

RETURN

END SUBROUTINE oserit_parameters_print


!*********************************************************************************
!  PDV
!  This routine must subsistute white space with underscore
!  Dear God, how does fortran have nothing built-in for this??
!
!*********************************************************************************
SUBROUTINE  SubstituteWhiteSp(string,n)
      implicit none
      INTEGER :: i,n
      CHARACTER (LEN=n) :: string, toReturn
      character :: sw,su

      sw=" "
      su="_"
      toReturn=""

      do i = 1, n
         if (index(string(i:i),sw)==0) then ! this is a character
           toReturn=TRIM(toReturn)//string(i:i)
         else
           toReturn=TRIM(toReturn)//su
         endif
      end do
      string = toReturn

   RETURN
END SUBROUTINE SubstituteWhiteSp

!*********************************************************************************

SUBROUTINE read_input_json(oserit_param,infilename)
  !*********************************************************************************
  !
  ! *Usrdef_oserit_read_input* read oserit parameters from json files
  !
  ! Author -
  !
  ! $Revision$  PDV - conversion to read JSON file.
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

  USE iopars
  USE osephyspars
  USE oseswitches
  USE partpars
  USE grid_module
  USE timepars
  USE datatypes_ose
  USE json_module

  TYPE( oserit_parameters ), INTENT(INOUT) :: oserit_param
  CHARACTER (LEN=*), INTENT(IN) :: infilename

  CHARACTER (LEN=max_length_string) :: line, parameter_name, parameter_value
  INTEGER :: start_index, dummy_integer
  INTEGER ::  iunit, ierr
  INTEGER (json_ik) :: n_elements


  type(json_file), TARGET :: json, json_cfg
  type(json_file), POINTER :: pjson, pjson_cfg
  logical :: found, vlog
  integer :: i,j,k,l, tmp
  character(kind=json_CK,len=:),allocatable :: string
  character(len=:),allocatable :: json_key, num_grid, units, num_in_ar, num_in_table

  ! initialize the class
  pjson=>json
  pjson_cfg=>json_cfg
  call json%initialize()
  call json_cfg%initialize()
  print*,""
  print*, "json request file should be: ", infilename//'_request.json'
  print*, "json config file should be: ", infilename//'_cfg.json'
  print*,""

  ! read the file
  call json%load_file(filename=infilename//'_request.json')
  call json_cfg%load_file(filename=infilename//'_cfg.json')


  !Request
  oserit_param%run_title = runtitle

  !time info
  call json%get('time_info.start_time',string,found)
  if (.not. found) stop 'json:no starting date, could also be an invalid parsing of request file'
  oserit_param%CStartDateTime = &
    &string(1:4)//"/"//string(6:7)//"/"//string(9:10)//";"//string(12:13)//":"//string(15:16)//":"//string(18:19)
  call json%get('time_info.end_time',string,found)
  if (.not. found) stop 'json:no end date'
  oserit_param%CEndDateTime = &
    & string(1:4)//"/"//string(6:7)//"/"//string(9:10)//";"//string(12:13)//":"//string(15:16)//":"//string(18:19)
  call json%get('time_info.iopt_backtrack.value',string,found)
  if (.not. found) stop 'json:no backtrack or forecast definition (iopt_backtrack)'
  read(string,*) oserit_param%iopt_backtrack

  !if wind and waves are activated
  call json_cfg%get('iopt_meteo',string,found)
  if (.not. found) stop 'json:iopt_meteo is undefined, could also be an invalid parsing of config file'
  read(string,*) oserit_param%iopt_meteo
  call json_cfg%get('iopt_wave',string,found)
  if (.not. found) stop 'json:iopt_wave is undefined'
  read(string,*) oserit_param%iopt_wave

  call json%get('drifter_parameters.drift_coefficient.dwl.min',string,found)
  if (.not. found) stop 'json:dwl_min is undefined'
  read(string,*) oserit_param%dwl_min
  call json%get('drifter_parameters.drift_coefficient.cwl.min',string,found)
  if (.not. found) stop 'json:cwl_min is undefined'
  read(string,*) oserit_param%cwl_min
  call json%get('drifter_parameters.drift_coefficient.dwl.max',string,found)
  if (.not. found) stop 'json:dwl_max is undefined'
  read(string,*) oserit_param%dwl_max
  call json%get('drifter_parameters.drift_coefficient.cwl.max',string,found)
  if (.not. found) stop 'json:cwl_max is undefined'
  read(string,*) oserit_param%cwl_max


  !release
  call json%get('release.nopart',string,found)
  if (.not. found) stop 'json:nopart is undefined'
  read(string,*) oserit_param%nopart

  call json%get('release.iopt_gen_cld',string,found)
  if (.not. found) stop 'json:iopt_gen_cld is undefined'
  read(string,*) oserit_param%iopt_gen_cld
  IF(oserit_param%iopt_gen_cld == 1)THEN
    call json%get('release.norelease_moving',string,found)
    if (.not. found) stop 'json:norelease_moving is undefined'
    read(string,*) oserit_param%norelease_moving
    call json%get('release.interval_between_releases',string,found)
    if (.not. found) stop 'json:interval_between_releases is undefined'
    oserit_param%CDTimeRelease_ose = string
    call json%get('release.release_start',string,found)
    if (.not. found) stop 'json:release_start is undefined'
    oserit_param%CStartDateTime_ose = string
    call json%get('release.release_end',string,found)
    if (.not. found) stop 'json:release_end is undefined'
    oserit_param%CEndDateTime_ose = string

    call json%get('release.slick.length.value',string,found)
    if (.not. found) stop 'json:length is undefined'
    read(string,*) oserit_param%length
    call json%get('release.slick.width.value',string,found)
    if (.not. found) stop 'json:width is undefined'
    read(string,*) oserit_param%width
    call json%get('release.slick.orientation_from_N.value',string,found)
    if (.not. found) stop 'json:orientation_from_N is undefined'
    read(string,*) oserit_param%orientation
    call json%get('release.slick.ellipse_thick.value',string,found)
    if (.not. found) stop 'json:ellipse_thick is undefined'
    read(string,*) oserit_param%ellipse_thick
    call json%get('release.slick.latitude_start.value',string,found)
    if (.not. found) stop 'json:latitude_start is undefined'
    read(string,*) oserit_param%y_lat_start
    call json%get('release.slick.longitude_start.value',string,found)
    if (.not. found) stop 'json:longitude_start is undefined'
    read(string,*) oserit_param%x_lon_start
    call json%get('release.slick.depth_above_MSL_start.value',string,found)
    if (.not. found) stop 'json:depth_above_MSL_start is undefined'
    read(string,*) oserit_param%depth_start
    call json%get('release.slick.latitude_end.value',string,found)
    if (.not. found) stop 'json:latitude_end is undefined'
    read(string,*) oserit_param%y_lat_end
    call json%get('release.slick.longitude_end.value',string,found)
    if (.not. found) stop 'json:longitude_end is undefined'
    read(string,*) oserit_param%x_lon_end
    call json%get('release.slick.depth_above_MSL_end.value',string,found)
    if (.not. found) stop 'json:depth_above_MSL_end is undefined'
    read(string,*) oserit_param%depth_end
  END IF


  !drift
  call json%get('drift.iopt_3D',string,found)
  if (.not. found) stop 'json:iopt_3D is undefined'
  read(string,*) oserit_param%iopt_3D
  call json%get('drift.iopt_wind_drift',string,found)
  if (.not. found) stop 'json:iopt_wind_drift is undefined'
  read(string,*) oserit_param%iopt_wind_drift
  call json%get('drift.iopt_cur_drift',string,found)
  if (.not. found) stop 'json:iopt_cur_drift is undefined'
  read(string,*) oserit_param%iopt_cur_drift
  call json%get('drift.iopt_wave_drift',string,found)
  if (.not. found) stop 'json:iopt_wave_drift is undefined'
  read(string,*) oserit_param%iopt_wave_drift
  call json%get('drift.iopt_stranding',string,found)
  if (.not. found) stop 'json:iopt_stranding is undefined'
  read(string,*) oserit_param%iopt_stranding
  call json%get('drift.iopt_leeway',string,found)
  if (.not. found) stop 'json:iopt_leeway is undefined'
  read(string,*) oserit_param%iopt_leeway
  call json%get('drift.timestep_duration.value',string,found)
  if (.not. found) stop 'json:timestep_duration is undefined'
  read(string,*) oserit_param%timestep_duration
  call json%get('drift.timestep_duration.units',string,found)
  if (.not. found) stop 'json:drift.timestep_duration units is undefined'
  IF(string /= "s") STOP 'json:drift.timestep_duration units not s'
  call json%get('drift.iopt_force_last_output',string,found)
  if (.not. found) stop 'json:iopt_force_last_output is undefined'
  read(string,*) oserit_param%iopt_force_last_output
  call json%get('drift.tmstp_per_outtmstp.value',string,found)
  if (.not. found) stop 'json:tmstp_per_outtmstp is undefined'
  read(string,*) oserit_param%tmstp_per_outtmstp


  IF( oserit_param%iopt_3D == 1)THEN

    call json%get('drift.iopt_resurfacing',string,found)
    if (.not. found) stop 'json:iopt_resurfacing is undefined'
    read(string,*) oserit_param%iopt_resurfacing

    call json%get('drift.nbr_subtmstp_vertical.value',string,found)
    if (.not. found) stop 'json:nbr_subtmstp_vertical is undefined'
    read(string,*) oserit_param%nbr_subtmstp_vertical
    !resuspension
    call json%get('drift.resuspension.iopt_resuspension',string,found)
    if (.not. found) stop 'json:iopt_resuspension is undefined'
    read(string,*) oserit_param%iopt_resuspension
    IF(oserit_param%iopt_resuspension == 1)THEN
      call json%get('drift.resuspension.min_speed.value',string,found)
      if (.not. found) stop 'json:min_speed is undefined'
      read(string,*) oserit_param%resus_min_speed

      call json%get('drift.resuspension.min_speed.units',string,found)
      if (.not. found) stop 'json:drift.resuspension.min_speed units is undefined'
      IF(string /= "m/s") STOP 'json:drift.resuspension.min_speed units not m/s'

      call json%get('drift.resuspension.height.value',string,found)
      if (.not. found) stop 'json:height is undefined'
      read(string,*) oserit_param%resus_height

      call json%get('drift.resuspension.height.units',string,found)
      if (.not. found) stop 'json:drift.resuspension.height units is undefined'
      IF(string /= "m") STOP 'json:drift.resuspension.height units not m'

    END IF

    call json%get('drift.iopt_dif_coef_v.value',string,found)
    if (.not. found) stop 'json:iopt_dif_coef_v is undefined'
    read(string,*) oserit_param%iopt_dif_coef_v
    IF( oserit_param%iopt_dif_coef_v == 1)THEN
      call json%get('drift.diffusivity_coefficients.K_dif_z.value',string,found)
      if (.not. found) stop 'json:K_dif_z is undefined'
      read(string,*) oserit_param%K_dif_z
    END IF

    IF(oserit_param%iopt_dif_coef_v > 0)THEN
      call json%get('drift.iopt_prevent_dif_seabed',string,found)
      if (.not. found) stop 'json:iopt_prevent_dif_seabed is undefined'
      read(string,*) oserit_param%iopt_prevent_dif_seabed
      !no point to read it if it cannot resurface
      if(oserit_param%iopt_resurfacing==1)THEN
        call json%get('drift.iopt_prevent_dif_surface',string,found)
        if (.not. found) stop 'json:iopt_prevent_dif_surface is undefined'
        read(string,*) oserit_param%iopt_prevent_dif_surface
      end if
    END IF

    call json%get('drift.w_imposed.value',string,found)
    if (.not. found) stop 'json:w_imposed is undefined'
    read(string,*) oserit_param%w_imposed

  END IF

  call json%get('drift.iopt_dif_coef_h.value',string,found)
  if (.not. found) stop 'json:iopt_dif_coef_h is undefined'
  read(string,*) oserit_param%iopt_dif_coef_h
  IF( oserit_param%iopt_dif_coef_h == 1)THEN
    call json%get('drift.diffusivity_coefficients.K_dif_x.value',string,found)
    if (.not. found) stop 'json:K_dif_x is undefined'
    read(string,*) oserit_param%K_dif_x
    call json%get('drift.diffusivity_coefficients.K_dif_y.value',string,found)
    if (.not. found) stop 'json:K_dif_y is undefined'
    read(string,*) oserit_param%K_dif_y
  END IF

  call json%get('drift.iopt_lag_scheme.value',string,found)
  if (.not. found) stop 'json:iopt_lag_scheme is undefined'
  read(string,*) oserit_param%iopt_lag_scheme
  oserit_param%iopt_lag_scheme = oserit_param%iopt_lag_scheme +1

  !rng
  call json%get('rng.iopt_set_seed',string,found)
  if (.not. found) stop 'json:no rng.iopt_set_seed'
  read(string,*) oserit_param%iopt_set_seed
  if (oserit_param%iopt_set_seed == 1)then
      call json%get('rng.seed1',string,found)
      if (.not. found) stop 'json_cfg:no rng.seed1'
      read(string,*) oserit_param%seed1
      call json%get('rng.seed2',string,found)
      if (.not. found) stop 'json_cfg:no rng.seed2'
      read(string,*) oserit_param%seed2
  end if

  call json%destroy()
  if (json%failed()) stop "failed to destroy the request json file reader"



  !forcings

  !find the file needed to import the grids of forcing
  IF(.NOT.ALLOCATED(oserit_param%io_cur_filename))THEN!should not be allocated again
    CALL json_cfg%info('domains',n_children=n_elements)
    nogrids = n_elements

    ALLOCATE(oserit_param%io_cur_filename(nogrids))
    IF (oserit_param%iopt_meteo == 1)ALLOCATE(oserit_param%io_win_filename(nogrids))
    IF (oserit_param%iopt_wave == 1) then
      ALLOCATE(oserit_param%io_wav_filename(nogrids))
      ALLOCATE(oserit_param%direction_dirw(nogrids))
      ALLOCATE(oserit_param%iopt_clockwise_dirw(nogrids))
    end if
    ALLOCATE(oserit_param%io_bat_filename(nogrids))

    ALLOCATE(oserit_param%lookup_cur(nogrids,16))
    oserit_param%lookup_cur = -1
    IF (oserit_param%iopt_meteo == 1)THEN
      ALLOCATE(oserit_param%lookup_win(nogrids,12))
      oserit_param%lookup_win = -1
    END IF
    IF (oserit_param%iopt_wave == 1) THEN
      ALLOCATE(oserit_param%lookup_wav(nogrids,11))
      oserit_param%lookup_wav = -1
    END IF
    ALLOCATE(oserit_param%lookup_bat(nogrids,5))
    oserit_param%lookup_bat = -1

    ALLOCATE(oserit_param%file_duration_hydro(nogrids))
    IF (oserit_param%iopt_meteo == 1)ALLOCATE(oserit_param%file_duration_meteo(nogrids))
    IF (oserit_param%iopt_wave == 1) ALLOCATE(oserit_param%file_duration_wave(nogrids))

    ALLOCATE(oserit_param%start_time_hydro(nogrids))
    IF (oserit_param%iopt_meteo == 1)ALLOCATE(oserit_param%start_time_meteo(nogrids))
    IF (oserit_param%iopt_wave == 1) ALLOCATE(oserit_param%start_time_wave(nogrids))

    ALLOCATE(oserit_param%iopt_uniform_depth(nogrids))

    ALLOCATE(oserit_param%has_cur(nogrids))
    ALLOCATE(oserit_param%has_wind(nogrids))
    ALLOCATE(oserit_param%has_wave(nogrids))
    ALLOCATE(oserit_param%has_bat(nogrids))

    oserit_param%has_cur = .false.
    oserit_param%has_wind = .false.
    oserit_param%has_wave = .false.
    oserit_param%has_bat = .false.

    num_grid = '00000'
    DO i = 1, nogrids
      write(num_grid,'(i5.5)') i

      !looks which grids are present (if the first value of the look_up_table is there, consider the forcing present)
      call json_cfg%get('domains('//num_grid//').hydro.look_up_table.value(1)',string,found)
      if (found) oserit_param%has_cur(i) = .true.
      call json_cfg%get('domains('//num_grid//').meteo.look_up_table.value(1)',string,found)
      if (found .and. oserit_param%iopt_meteo == 1) oserit_param%has_wind(i) = .true.
      call json_cfg%get('domains('//num_grid//').waves.look_up_table.value(1)',string,found)
      if (found .and. oserit_param%iopt_wave == 1) oserit_param%has_wave(i) = .true.
      call json_cfg%get('domains('//num_grid//').bathymetry.look_up_table.value(1)',string,found)
      if (found) oserit_param%has_bat(i) = .true.

      IF(oserit_param%has_cur(i))THEN
        num_in_ar = '00000'
        DO j = 1,SIZE(oserit_param%lookup_cur(i,:))
          write(num_in_ar,'(i5.5)') j
          call json_cfg%get('domains('//num_grid//').hydro.look_up_table.value('//num_in_ar//')',string,found)
          if (.not. found) stop 'json_cfg:error in reading look_up_table current'
          read(string,*) oserit_param%lookup_cur(i,j)
        END DO
      END IF

      IF (oserit_param%iopt_meteo == 1)THEN
        IF(oserit_param%has_wind(i))THEN
          num_in_ar = '00000'
          DO j = 1,SIZE(oserit_param%lookup_win(i,:))
            write(num_in_ar,'(i5.5)') j
            call json_cfg%get('domains('//num_grid//').meteo.look_up_table.value('//num_in_ar//')',string,found)
            if (.not. found) stop 'json_cfg:error in reading look_up_table meteo'
            read(string,*) oserit_param%lookup_win(i,j)
          END DO
        END IF
      END IF

      IF (oserit_param%iopt_wave == 1)THEN
        IF(oserit_param%has_wave(i))THEN
          num_in_ar = '00000'
          DO j = 1,SIZE(oserit_param%lookup_wav(i,:))
            write(num_in_ar,'(i5.5)') j
            call json_cfg%get('domains('//num_grid//').waves.look_up_table.value('//num_in_ar//')',string,found)
            if (.not. found) stop 'json_cfg:error in reading look_up_table waves'
            read(string,*)oserit_param%lookup_wav(i,j)
          END DO
          ! If the wave have a direction, read the direction related parameters
          if (oserit_param%lookup_wav(i,9) > -1)then
            call json_cfg%get('domains('//num_grid//').waves.iopt_clockwise_dirw.value',string,found)
            if (.not. found) stop 'json_cfg:error in iopt_clockwise_dirw waves'
            read(string,*)tmp
            if(tmp == 0)then
              oserit_param%iopt_clockwise_dirw(i) = .false.
            else if(tmp==1)then
              oserit_param%iopt_clockwise_dirw(i) = .true.
            else
              stop 'iopt_clockwise_dirw must be at 0 (false) or 1 (true)'
            end if
            call json_cfg%get('domains('//num_grid//').waves.direction_dirw.value',string,found)
            read(string,*)oserit_param%direction_dirw(i)
            
          end if
        END IF
      END IF

      IF(oserit_param%has_bat(i))THEN
        num_in_ar = '00000'
        DO j = 1,SIZE(oserit_param%lookup_bat(i,:))
          write(num_in_ar,'(i5.5)') j
          call json_cfg%get('domains('//num_grid//').bathymetry.look_up_table.value('//num_in_ar//')',string,found)
          if (.not. found) stop 'json_cfg:error in reading look_up_table bathymetry'
          read(string,*)oserit_param%lookup_bat(i,j)
        END DO
      END IF

      IF (oserit_param%has_cur(i))THEN
        call json_cfg%get('domains('//num_grid//').hydro.filename',string,found)
        if (.not. found) stop 'json_cfg:no io_cur_filename'
        oserit_param%io_cur_filename(i) = TRIM(string)
        call json_cfg%get('domains('//num_grid//').hydro.file_duration.value',string,found)
        if (.not. found) stop 'json_cfg:no file_duration hydro'
        read(string,*)oserit_param%file_duration_hydro(i)
        call json_cfg%get('domains('//num_grid//').hydro.start_time.value',string,found)
        if (.not. found) stop 'json_cfg:no start_time hydro'
        read(string,*)oserit_param%start_time_hydro(i)
        call json_cfg%get('domains('//num_grid//').hydro.iopt_uniform_depth',string,found)
        if (.not. found) stop 'json_cfg:no iopt_uniform_depth hydro'
        read(string,*)oserit_param%iopt_uniform_depth(i)
      END IF


      IF (oserit_param%iopt_meteo == 1 .AND. oserit_param%has_wind(i))THEN
        call json_cfg%get('domains('//num_grid//').meteo.filename',string,found)
        if (.not. found) stop 'json_cfg:no io_win_filename'
        oserit_param%io_win_filename(i) = TRIM(string)
        call json_cfg%get('domains('//num_grid//').meteo.file_duration.value',string,found)
        if (.not. found) stop 'json_cfg:no file_duration meteo'
        read(string,*)oserit_param%file_duration_meteo(i)
        call json_cfg%get('domains('//num_grid//').meteo.start_time.value',string,found)
        if (.not. found) stop 'json_cfg:no start_time meteo'
        read(string,*)oserit_param%start_time_meteo(i)
      END IF
      IF (oserit_param%iopt_wave == 1 .AND. oserit_param%has_wave(i))THEN
        call json_cfg%get('domains('//num_grid//').waves.filename',string,found)
        if (.not. found) stop 'json_cfg:no io_wav_filename'
        oserit_param%io_wav_filename(i) = TRIM(string)
        call json_cfg%get('domains('//num_grid//').waves.file_duration.value',string,found)
        if (.not. found) stop 'json_cfg:no file_duration waves'
        read(string,*)oserit_param%file_duration_wave(i)
        call json_cfg%get('domains('//num_grid//').waves.start_time.value',string,found)
        if (.not. found) stop 'json_cfg:no start_time waves'
        read(string,*)oserit_param%start_time_wave(i)
      END IF

      IF (oserit_param%has_bat(i))THEN
        call json_cfg%get('domains('//num_grid//').bathymetry.filename',string,found)
        if (.not. found) stop 'json_cfg:no io_bat_filename'
        oserit_param%io_bat_filename(i) = string
      END IF
    END DO
  END IF


  call json_cfg%get('iopt_grid_interp.value',string,found)
  if (.not. found) stop 'json_cfg:no iopt_grid_interp.value'
  read(string,*) oserit_param%iopt_grid_interp

  call json_cfg%get('max_stockes',string,found)
  if (.not. found) stop 'json_cfg:no max_stockes'
  read(string,*) oserit_param%max_stockes


  call json_cfg%destroy()
  if ( json_cfg%failed()) stop 1

  !CALL oserit_parameters_print(oserit_param)

  CStartDateTime         = TRIM(oserit_param%CStartDateTime)//':000'
  CEndDateTime           = oserit_param%CEndDateTime//':000'
  iopt_3D           = oserit_param%iopt_3D
  iopt_meteo         = oserit_param%iopt_meteo
  iopt_wind_drift   = oserit_param%iopt_wind_drift
  iopt_leeway       = oserit_param%iopt_leeway
  iopt_cur_drift      = oserit_param%iopt_cur_drift
  iopt_wave         = oserit_param%iopt_wave
  iopt_wave_drift   = oserit_param%iopt_wave_drift
  iopt_stranding    = oserit_param%iopt_stranding
  K_dif_x                = oserit_param%K_dif_x
  K_dif_y                = oserit_param%K_dif_y
  K_dif_z                = oserit_param%K_dif_z
  w_imposed              = oserit_param%w_imposed
  iopt_gen_cld      = oserit_param%iopt_gen_cld

  CStartDateTime_ose = oserit_param%CStartDateTime_ose
  CEndDateTime_ose = oserit_param%CEndDateTime_ose
  nopart = oserit_param%nopart

  CDTimeRelease_ose = oserit_param%CDTimeRelease_ose

  iopt_cur_drift = oserit_param%iopt_cur_drift

  iopt_lag_scheme = oserit_param%iopt_lag_scheme
  iopt_dif_coef_h = oserit_param%iopt_dif_coef_h
  iopt_dif_coef_v = oserit_param%iopt_dif_coef_v
  norelease_moving = oserit_param%norelease_moving

  iopt_grid_interp = oserit_param%iopt_grid_interp


END SUBROUTINE read_input_json

END MODULE oserit_run_reader
