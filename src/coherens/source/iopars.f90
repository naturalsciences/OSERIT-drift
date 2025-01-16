MODULE iopars
!************************************************************************
!
! *iopars* Parameters for input/output
!
! Author - Patrick Luyten
!
! Version - @(COHERENS)iopars.f90  V2.0
!
! $Date: 2011-03-18 14:35:25 +0100 (Fri, 18 Mar 2011) $
!
! $Revision: 188 $
!
! Description -
!
!************************************************************************
!
USE syspars

IMPLICIT NONE


!
!1. General parameters
!---------------------
!

LOGICAL :: cold_start = .FALSE., next_simul = .FALSE.
INTEGER :: isimul = 0, nopenf = 0, nrecunit = 4
CHARACTER (LEN=lentitle) :: intitle, outtitle, runtitle
CHARACTER (LEN=lentitle) :: input_dir, output_dir
CHARACTER (LEN=lenname), DIMENSION(MaxProgLevels) :: procname

!
!2. Model input/output
!---------------------
!
!2.1 Attributes
!--------------

INTEGER, DIMENSION(MaxIOTypes,2) :: maxdatafiles
INTEGER, DIMENSION(MaxCIFTypes) :: ciflinenum

!
!2.2 File descriptor key ids
!---------------------------
!
!---domain decomposition
INTEGER, PARAMETER :: io_mppmod = 1
!--initial conditions
INTEGER, PARAMETER :: io_inicon = 2
!--model grid and data locations
INTEGER, PARAMETER :: io_modgrd = 3
!---external grids
INTEGER, PARAMETER :: io_metgrd = 4, io_sstgrd = 5, io_biogrd = 6, &
                    & io_nstgrd = 7, io_wavgrd = 8, io_inngrd = 9
!---specifiers for biological module
INTEGER, PARAMETER :: io_biospc = 10
!--open (surface) boundaries
INTEGER, PARAMETER :: io_1uvsur = 11, io_2uvobc = 12, io_3uvobc = 13, &
                    & io_salobc = 14, io_tmpobc = 15, io_bioobc = 16, &
                    & io_rlxobc = 17
!--nested output
INTEGER, PARAMETER :: io_nstspc = 18, io_2uvnst = 19, io_3uvnst = 20, &
                    & io_salnst = 21, io_tmpnst = 22, io_bionst = 23
!--surface data
INTEGER, PARAMETER :: io_metsur = 24, io_sstsur = 25, io_biosur = 26

!---key id for Oserit forcing
INTEGER, PARAMETER :: io_cur = 27, io_win = 28, io_dif= 29, &
                    & io_wav = 30

!---key ids for CIF files
INTEGER, PARAMETER :: icif_defruns= 1, icif_model = 2, icif_bio = 3, &
                    & icif_ose = 4

!---key ids for initial conditions
INTEGER, PARAMETER :: ics_phys = 1, ics_bio = 2, ics_ose = 3

!---key ids for surface data grids
INTEGER, PARAMETER :: igrd_model = 1, igrd_meteo = 2, igrd_sst = 3, &
                    & igrd_bio = 4, igrd_wave = 5, igrd_inner= 6

!---file descriptor in string format
CHARACTER (LEN=6), DIMENSION(MaxIOTypes) :: modfiles_desc = &
 & (/'mppmod','inicon','modgrd','metgrd','sstgrd','biogrd','nstgrd','wavgrd',&
 &   'inngrd','biospc',&
 &   '1uvsur','2uvobc','3uvobc','salobc','tmpobc','bioobc','rlxobc','nstspc',&
 &   '2uvnst','3uvnst','salnst','tmpnst','bionst','metsur','sstsur','biosur',&
 &   'osecur','osewin','osedif','osewav','inncur','innwin','inndif','innwav'/)

!
!3. User output
!--------------
!
!---fill and minimum values
LOGICAL :: log_fill = log_undef
INTEGER :: int_fill = int_undef
REAL :: real_fill = real_undef, real_min = real_flag

!---time series
INTEGER :: nosetstsr = 0, nostatstsr = 0,  novarstsr = 0
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ivarstsr, lstatstsr

!---time averages
INTEGER :: nosetsavr = 0, nostatsavr = 0, novarsavr = 0
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ivarsavr, lstatsavr


!---harmonic analysis
INTEGER :: nosetsanal = 0, nofreqsanal = 0, nostatsanal = 0, novarsanal = 0
CHARACTER (LEN=lentime), ALLOCATABLE, DIMENSION(:) :: cdate_time_ref
CHARACTER (LEN=lenfreq), ALLOCATABLE, DIMENSION(:) :: harm_freq_names
INTEGER, ALLOCATABLE, DIMENSION(:) :: index_anal, nofreqsharm
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ifreqsharm, ivarsanal, lstatsanal
REAL, ALLOCATABLE, DIMENSION(:) :: harm_freq

!---elliptic parameters
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: ivarsell, ivecell2d, ivecell3d

!
!4. Monitoring
!-------------
!
!4.1 log files
!-------------
!

LOGICAL :: exitlog
CHARACTER (LEN=*), PARAMETER :: logfmt1 = '(I1,'':'',A)', &
                              & logfmt2 = '(I2,'':'',A)'
CHARACTER (LEN=1), PARAMETER :: logexit = 'R'
CHARACTER (len=leniofile) :: inilog_file, runlog_file
INTEGER :: iolog = 0, loglev1, loglev2, pglev, runlog_count
INTEGER, ALLOCATABLE, DIMENSION(:) :: levprocs_ini, levprocs_run

!
!4.2 Error files
!---------------
!
!---parameters
LOGICAL :: errchk
CHARACTER (len=leniofile) :: errlog_file
INTEGER :: errstat, ioerr = 0, maxerrors, nerrs = 0
INTEGER, ALLOCATABLE, DIMENSION(:) :: levprocs_err

!---key ids
INTEGER, PARAMETER :: ierrno_fopen = 1, ierrno_fclose = 2, ierrno_read = 3,&
                    & ierrno_write = 4, ierrno_fend = 5, ierrno_input = 6,&
                    & ierrno_inival = 7, ierrno_runval = 8, ierrno_alloc = 9,&
                    & ierrno_arg = 10, ierrno_comms = 11, ierrno_MPI = 12, &
                    & ierrno_CDF = 13

!---messages
CHARACTER (LEN=lenerrcode), PARAMETER, DIMENSION(MaxErrCodes) :: error_code = &
      & (/'Not possible to open file                            ',&
      &   'Unable to close file                                 ',&
      &   'Read error                                           ',&
      &   'Write error                                          ',&
      &   'End of file condition                                ',&
      &   'Wrong input values                                   ',&
      &   'Invalid initial values for model parameters or arrays',&
      &   'Invalid values for variables at run time             ',&
      &   'Not possible to allocate arrays                      ',&
      &   'Missing or invalid argument in routine call          ',&
      &   'Communication error                                  ',&
      &   'Error in MPI call                                    ',&
      &   'Error in NetCDF call                                 '/)

!
!4.3 Warning file
!----------------
!

LOGICAL :: warnflag, warning
CHARACTER (len=leniofile) :: warlog_file
INTEGER :: iowarn = 0

!
!4.4 Timer
!---------
!
!---parameters
LOGICAL :: timer = .FALSE.
CHARACTER (len=leniofile) :: timing_file
INTEGER :: levtimer = 0, maxwaitsecs = 3600, nowaitsecs = 0, npcc_max, &
         & npcc_rate, timer_format = 1
INTEGER (KIND=kndilong), DIMENSION(MaxTimers) :: nopcc

!---key-ids
INTEGER, PARAMETER :: &
  & itm_hydro = 1, itm_1dmode = 2, itm_2dmode = 3, itm_3dmode = 4, &
  & itm_dens = 5, itm_temp = 6, itm_sal = 7, itm_init = 8, &
  & itm_trans = 9, itm_adv = 10, itm_hdif = 11, itm_vdif = 12, &
  & itm_phgrad = 13, itm_input = 14, itm_output = 15, itm_inout = 16, &
  & itm_com_coll = 17, itm_com_comb = 18, itm_com_copy = 19, &
  & itm_com_dist = 20, itm_com_exch = 21, itm_com_util = 22, itm_coms = 23, &
  & itm_MPI = 24, itm_CDF = 25, itm_arrint = 26, itm_user = 27, &
  & itm_nest = 28, itm_libs = 29, itm_astro = 30, itm_bconds = 31, &
  & itm_meteo = 32, itm_wait = 33, itm_biolgy = 34

!---timer descriptions
CHARACTER (LEN=20), DIMENSION(MaxTimers) :: desctimer = &
   & (/'Hydrodynamics       ', '1D mode             ', &
     & '2D mode             ', '3D mode             ', &
     & 'Density             ', 'Temperature         ', &
     & 'Salinity            ', 'Initialisation      ', &
     & 'Transport           ', 'Advection           ', &
     & 'Horizontal diffusion', 'Vertical diffusion  ', &
     & 'Baroclinic pressure ', 'Input               ', &
     & 'Output              ', 'Input/output        ', &
     & 'Collect comms       ', 'Combine comms       ', &
     & 'Copy comms          ', 'Distribute comms    ', &
     & 'Exchange comms      ', 'Utility comms       ', &
     & 'Parallel comms      ', 'MPI calls           ', &
     & 'netCDF              ', 'Array interpolation ', &
     & 'User calls          ', 'Nesting procedures  ', &
     & 'Library calls       ', 'Astronomical tide   ', &
     & 'Boundary conditions ', 'Meteo               ', &
     & 'Wait calls          ', 'Biology             ' /)

!
!5. netCDF parameters
!--------------------
!

INTEGER :: char_NF90 = 0, clobber_NF90 = 0, fill_NF90 = 0, global_NF90 = 0, &
         & int_NF90 = 0, noerr_NF90 = 0, nofill_NF90 = 0, nowrite_NF90 = 0, &
         & offset_64bit_NF90 = 0, real_NF90 = 0, share_NF90 = 0, &
         & sizehint_default_NF90 = 0, unlimited_NF90 = 0, write_NF90 = 0

SAVE

!
! Name            Type     Purpose
!------------------------------------------------------------------------------
! general parameters
!*cold_start*     LOGICAL If .TRUE., main program stops after initialisation
!*next_simul*     LOGICAL .TRUE. to start next simulation, .FALSE. to exit
!                         program
!*isimul*         INTEGER Simulation number
!*nopenf*         INTEGER Number of connected files
!*nrecunit*       INTEGER Unit record size in bytes for direct access I/O
!*intitle*        CHAR    Title for model forcing files
!*outtitle*       CHAR    Title for user output files
!*runtitle*       CHAR    Simulation title
!*procname*       CHAR    Name of subprogram at current and higher levels
!
! model I/O
!*maxdatafiles*   INTEGER Largest file index for an active file of given type
!*ciflinenun*     INTEGER Number of the last input line read from a CIF file
!*modfiles*       DERIVED Attributes of model I/O files
!                   first dimension  => file descriptor
!                   second dinebsion => file number
!                   third dimension  => input (1), output (2)
!*surfacegrids*   DERIVED Attributes of surface data grids
!
! key ids and descriptors
! io_?            INTEGER File descriptor key ids
!*io_mppmod*      INTEGER Domain decomposition
!*io_inicon*      INTEGER Initial condition files
!*io_modgrd*      INTEGER Model bathymetry
!*io_metgrd*      INTEGER Meteo surface grid
!*io_sstgrd*      INTEGER Surface SST grid
!*io_biogrd*      INTEGER Surface grid for biological variables
!*io_nstgrd*      INTEGER Locations of nested grid boundaries
!*io_biogrd*      INTEGER Surface grid for wave variables
!*io_biospc*      INTEGER Specifiers arrays for biological model
!*io_1uvsur*      INTEGER Surface boundary conditions/data for 1-D mode
!*io_2uvobc*      INTEGER Open boundary conditions/data for 2-D mode
!*io_3uvobc*      INTEGER Open boundary conditions/data for 3-D mode
!*io_salobc*      INTEGER Open boundary conditions/data for salinity
!*io_tmpobc*      INTEGER Open boundary conditions/data for temperature
!*io_bioobc*      INTEGER Open boundary conditions/data for biological
!                         variables
!*io_rlxobc*      INTEGER Specifiers for relaxation zones
!*io_nstspc*      INTEGER Specifiers for nesting
!*io_2uvnst*      INTEGER 2-D mode output for nesting
!*io_3uvnst*      INTEGER 3-D mode output for nesting
!*io_salnst*      INTEGER Salinity output for nesting
!*io_tmpnst*      INTEGER Temperature output for nesting
!*io_bionst*      INTEGER Biological output for nesting
!*io_metsur*      INTEGER Surface meteo data
!*io_sstsur*      INTEGER Surface SST data
!*io_biosur*      INTEGER 2-D biological data
!
!
! cif_?*          INTEGER CIF files
!*icif_defruns*   INTEGER defruns file
!*icif_model*     INTEGER model setup parameters
!*icif_ose*       INTEGER oserit model setup parameters
!
! ics_?           INTEGER Initial condition files
!*ics_phys*       INTEGER Physical model
!*ics_bio*        INTEGER Biological model
!*ics_ose*        INTEGER Oserit model
!
! igrd_?          INTEGER Surface grid key ids
!*igrd_model*     INTEGER Model grid
!*igrd_meteo*     INTEGER Meteo grid
!*igrd_sst*       INTEGER SST grid
!*igrd_bio*       INTEGER Surface grid for biological data
!*igrd_wave*      INTEGER wave grid
!*igrd_inner*     INTEGER inner domain

!
!*log_fill*       LOGICAL Fill value for logical data
!*int_fill*       INTEGER Fill value for integer data
!*real_fill*      REAL    Fill value for real data
!*real_min*       REAL    Minimum value for unflagged real data
!
! time series output
!*nosetstsr*      INTEGER Number of output file series
!*nostatstsr*     INTEGER Number of stations for irregular output
!*novarstsr*      INTEGER Number of output variables
!*ivarstsr*       INTEGER Output variable indices per file index
!*lstattsr*       INTEGER Station label per file index
!*tsrgrd*         DERIVED Attributes of output grid file
!*tsr0d*          DERIVED Attributes 0-D output files
!*tsr2d*          DERIVED Attributes 2-D output files
!*tsr3d*          DERIVED Attributes 3-D output files
!*tsrgpars*       DERIVED Attributes of output grid
!*tsrstatlocs*    DERIVED Index positions of output stations
!*tsrvars*        DERIVED Output variable attributes
!
! time averaged output
!*nosetsavr*      INTEGER Number of output file series
!*nostatsavr*     INTEGER Number of stations for irregular output
!*novarsavr*      INTEGER Number of output variables
!*ivarsavr*       INTEGER Output variable indices per file index
!*lstatavr*       INTEGER Station label per file index
!*avrgrd*         DERIVED Attributes of output grid file
!*avr0d*          DERIVED Attributes 0-D output files
!*avr2d*          DERIVED Attributes 2-D output files
!*avr3d*          DERIVED Attributes 3-D output files
!*avrgpars*       DERIVED Attributes of output grid
!*avrstatlocs*    DERIVED Index positions of output stations
!*avrvars*        DERIVED Output variable attributes
!
! harmonic analysis
!*nosetsanal*     INTEGER Number of files for each specific harmonic output
!*nofreqsanal*    INTEGER Number of frequencies for harmonic analysis
!*nostatsanal*    INTEGER Number of stations for irregular output
!*novarsanal*     INTEGER Number of output variables
!*cdate_time_ref* CHAR    Reference date for harmonic phases (central time if
!                         undefined)
!*harm_freq_names*CHAR    Names of the frequencies used in harmonic analysis
!*index_anal*     INTEGER Tidal constituent frequency indices (undefined if 0)
!*nofreqsharm*    INTEGER Number of frequencies per file series
!*ifreqsharm*     INTEGER Frequency indices per file index
!*ivarsanal*      INTEGER Output variable indices per file index
!*lstatanal*      INTEGER Station label per file index
!*harm_freq*      REAL    Frequencies of the harmonic components     [radian/s]
!*analgrd*        DERIVED Attributes of output grid file
!*res0d*          DERIVED Attributes 0-D output residual files
!*res2d*          DERIVED Attributes 2-D output residual files
!*res3d*          DERIVED Attributes 3-D output residual files
!*amp0d*          DERIVED Attributes 0-D output amplitude files
!*amp2d*          DERIVED Attributes 2-D output amplitude files
!*amp3d*          DERIVED Attributes 3-D output amplitude files
!*pha0d*          DERIVED Attributes 0-D output phase files
!*pha2d*          DERIVED Attributes 2-D output phase files
!*pha3d*          DERIVED Attributes 3-D output phase files
!*ell2d*          DERIVED Attributes 2-D elliptic output files
!*ell3d*          DERIVED Attributes 3-D elliptic output files
!*analgpars*      DERIVED Attributes of output grid
!*analstatlocs*   DERIVED Index positions of output stations
!*analvars*       DERIVED Attributes of analysed variables
!*ivarsell*       INTEGER Output variable indices for elliptic parameters per
!                         file index
!*ivecell2d*      INTEGER Variable indices of 2-D elliptic vector components
!*ivecell3d*      INTEGER Variable indices of 3-D elliptic vector components
!*ellvars*        DERIVED Attributes of elliptic variables
!
! log files
!*exitlog*        LOGICAL Enables/disables an 'exit' message in log file just
!                         before RETURN statement
!*logfmt1*        CHAR    Format of log message
!*logfmt2*        CHAR    Format of log message
!*logexit*        CHAR    Exit code
!*inilog_file*    CHAR    Name of (generic) 'inilog' file
!*runlog_file*    CHAR    Name of (generic) 'runlog' file
!*loglev_def*     INTEGER Default number of program leveling in log files
!*iolog*          INTEGER File unit of log file
!*loglev1*        INTEGER Program leveling in log files
!*loglev2*        INTEGER Program leveling in log files (exit code)
!*pglev*          INTEGER Current program level
!*runlog_count*   INTEGER Determines the number of time steps after which the
!                         log-file will be re-written
!*levprocs_ini*   INTEGER Program leveling in 'inilog' files for each process
!*levprocs_run*   INTEGER Program leveling in 'runlog' files for each process
!
! error files
!*errchk*         LOGICAL Enables/disables error checking
!*errlog_file*    CHAR    Name of (generic) error file
!*errstat*        INTEGER Error status number as returned by FORTRAN, MPI and
!                         netcdf calls
!*ioerr*          INTEGER File unit of error file
!*maxerrors*      INTEGER Maximum allowed number of error messages
!*nerrs*          INTEGER Number of detected errors
!*levprocs_err*   INTEGER Level of error coding in 'errlog' files for each
!                         process
! ierrno_?        INTEGER Error code key ids
!*ierrno_fopen*   INTEGER File opening error
!*ierrno_fclose*  INTEGER File closing error
!*ierrno_read*    INTEGER Read error
!*ierrno_write*   INTEGER Write error
!*ierrno_fend*    INTEGER End of file condition
!*ierrno_input*   INTEGER Input error
!*ierrno_inival*  INTEGER Invalid parameter or initial value
!*ierrno_runval*  INTEGER Invalid run time value
!*ierrno_alloc*   INTEGER Allocation error
!*ierrno_arg*     INTEGER Missing argument or invalid argument value
!*ierrno_comms*   INTEGER Communication error
!*ierrno_MPI*     INTEGER MPI error
!*ierrno_CDF*     INTEGER Netcdf error
!*error_code*     CHAR    List of error message codes
!
! warning file
!*warning*        LOGICAL Enables issuing of warning messages by master
!*warlog_file*    CHAR    Name of (generic) 'warning' file
!*iowarn*         INTEGER File unit of warning file
!
! timer
!*timer*          LOGICAL .TRUE. if levtimer > 0
!*timing_file*    CHAR    Name of (generic) timing file
!*levtimer*       INTEGER Parameter to control level of info in timing file
!*maxwaitsecs*    INTEGER Maximum allowed time (in seconds) for program
!                         suspension
!*nowaitsecs*     INTEGER Number of seconds to suspend process in wait call
!*npcc_max*       INTEGER Maximum clock count on process clock
!*npcc_rate*      INTEGER Number of process clock counts per second
!*timer_format*   INTEGER Switch to select for time formatting
!*nopcc*          LONGINT Number of process clock counts per timer
! itm_?           INTEGER Key ids of timer processes
!*itm_hydro*      INTEGER hydrodynamics
!*itm_1dmode*     INTEGER 1-D mode calculations
!*itm_2dmode*     INTEGER 2-D mode calculations
!*itm_3dmode*     INTEGER 3-D mode calculations
!*itm_dens*       INTEGER Density calculations
!*itm_temp*       INTEGER Temperature
!*itm_sal*        INTEGER Salinity
!*itm_init*       INTEGER Model initialisation
!*itm_trans*      INTEGER Transport modules
!*itm_adv*        INTEGER Advection
!*itm_hdif*       INTEGER Horizontal diffusion
!*itm_vdif*       INTEGER Vertical diffusion
!*itm_phgrad*     INTEGER Baroclonic pressure gradient
!*itm_input*      INTEGER Reading
!*itm_output*     INTEGER Writing
!*itm_inout*      INTEGER Reading and writing
!*itm_com_coll*   INTEGER Collect communications
!*itm_com_comb*   INTEGER Combine communications
!*itm_com_copy*   INTEGER Copy communications
!*itm_com_dist*   INTEGER Distribute communications
!*itm_com_exch*   INTEGER Exchange communications
!*itm_com_util*   INTEGER Utility communications
!*itm_coms*       INTEGER All comminications
!*itm_MPI*        INTEGER MPI routines
!*itm_CDF*        INTEGER Netcdf routines
!*itm_arrint*     INTEGER Array interpolation
!*itm_user*       INTEGER User output
!*itm_nest*       INTEGER Nesting
!*itm_libs*       INTEGER Libraries
!*itm_astro*      INTEGER Astronomical forcing
!*itm_bconds*     INTEGER Boundary conditions
!*itm_meteo*      INTEGER Meteo
!*itm_wait*       INTEGER Wait calls
!*itm_biolgy*     INTEGER Biology
!*desctimer*      INTEGER Desciptions of timers
!
!************************************************************************
!

END MODULE iopars
