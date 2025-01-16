MODULE syspars
!************************************************************************
!
! *syspars* System parameters
!
! Author - Patrick Luyten
!
! Version - @(COHERENS)syspars.f90  V2.0
!
! $Date: 2011-03-18 14:35:25 +0100 (Fri, 18 Mar 2011) $
!
! $Revision: 188 $
!
! Description - 
!
!************************************************************************
!
IMPLICIT NONE

!--kind parameters
INTEGER, PARAMETER :: kndchar = KIND('A'), kndlog = KIND(.TRUE.), &
                    & kndint = KIND(1), &
                    & kndilong = MAX(4,SELECTED_INT_KIND(10)), &
                    & kndreal = KIND(1.0), kndlong = 8, kndcmplx = 8

!---data types
INTEGER, PARAMETER :: char_type = 1, log_type = 2, int_type = 3, &
                    & longint_type = 4, real_type = 5, long_type = 6, &
                    & cmplx_type = 7 

!---universal parameters
REAL, PARAMETER :: pi = 3.14159265, halfpi = 1.57079633, twopi = 6.28318531
REAL (KIND=kndlong), PARAMETER :: pi_d = 3.1415926535897932_kndlong
REAL (KIND=kndlong), PARAMETER :: halfpi_d = 1.5707963267948966_kndlong
REAL (KIND=kndlong), PARAMETER :: twopi_d = 6.2831853071795865_kndlong
REAL, PARAMETER :: degtorad = pi/180.0, radtodeg = 180.0/pi
REAL, PARAMETER :: degtorad_d = pi_d/180.0_kndlong, &
                 & radtodeg_d = 180.0_kndlong/pi_d

!---tidal parameters
INTEGER, PARAMETER :: MaxAstroTides = 56, MaxConstituents = 77

!---random generators
INTEGER, PARAMETER :: MaxGenerators = 32

!---MPI communications
INTEGER, PARAMETER :: MaxHaloComms = 8

!---model variables
INTEGER, PARAMETER :: MaxBioArids = 500, MaxModArids = 1000, &
                    & MaxTotArids = MaxModArids + MaxBioArids

!---model I/O
INTEGER, PARAMETER :: MaxCIFTypes = 3, MaxCIFVars = 50, MaxGridTypes = 6, &
                    & MaxGridFiles = 2, MaxIOFiles = 30, MaxIOTypes = 34, &
                    & MaxProgLevels  = 20, MaxRestarts = 130

!---monitoring files and error coding
INTEGER, PARAMETER :: MaxErrCodes = 13, MaxErrMesgs = 50, MaxTimers = 34

!---character string lengths
INTEGER, PARAMETER :: lencifline = 300, lencifvar = 120, lendesc = 120, &
                    & lenerrcode = 120, lenformat = 120, lenfreq = 7, &
                    & leniofile = 120, lenname = 31, lennode = 3, &
                    & lentime = 23, lentitle = 100, lenunit = 60, &
                    & lenversion = 10

!---cif file
CHARACTER (LEN=1), PARAMETER :: cifcom = '!', cifend ='#', cifsep =','

!---user output
LOGICAL, PARAMETER :: DegreesOut = .TRUE.
CHARACTER (LEN=lenversion), PARAMETER :: model_version = 'V2.1.1'

!---output formats
CHARACTER (LEN=lenformat), PARAMETER :: IntegerFormat='(50I11)',&
                                      & RealFormat='(50G16.7)'
!---undefined and zero values
LOGICAL, PARAMETER :: log_undef = .FALSE.
INTEGER, PARAMETER :: int_undef = -2147483647, izero = 0
INTEGER (KIND=kndilong), PARAMETER :: izero_d = 0_kndilong, &
                                    & longint_undef = -2147483647_kndilong
REAL, PARAMETER ::  real_undef = -9.9692099683868690E+36, &
                  & real_flag = -9.9692099683868690E+35, rzero = 0.0
REAL (KIND=kndlong), PARAMETER :: rzero_d = 0.0_kndlong
CHARACTER (LEN=lentime), PARAMETER :: cdatetime_undef = &
                                    & 'xxxx/xx/xx;00:00:00:000'

integer, parameter :: LENGTH_STR = 65

!
! Name             Type     Purpose
!------------------------------------------------------------------------------
!*kndchar*         INTEGER Kind parameter for character variables
!*kndlog*          INTEGER Kind parameter for logical variables
!*kndint*          INTEGER Kind parameter for integer variables
!*kndilong*        INTEGER Kind parameter for long integer variables
!*kndreal*         INTEGER Kind parameter for real variables
!*kndlong*         INTEGER Kind parameter for long real variables
!*kndcmplx*        INTEGER Kind parameter for complex variables
!*char_type*       INTEGER Type parameter for character variables
!*log_type*        INTEGER Type parameter for logical variables
!*int_type*        INTEGER Type parameter for integer variables
!*longint_type*    INTEGER Type parameter for long integer variables
!*real_type*       INTEGER Type parameter for real variables
!*long_type*       INTEGER Type parameter for long real variables
!*cmplx_type*      INTEGER Type parameter for complex variables
!*pi*              REAL    Number pi (default kind)
!*halfpi*          REAL    Number pi divided by 2 (default kind)
!*twopi*           REAL    Number pi times 2 (default kind)
!*pi_d*            LONG    Number pi (long kind)
!*halfpi_d*        LONG    Number pi divided by 2 (long kind)
!*twopi_d*         LONG    Number pi times 2 (long kind)
!*degtorad*        REAL    Factor to convert degrees to radians
!*radtodeg*        REAL    Factor to convert radians to degrees
!*degtorad_d*      LONG    Factor to convert degrees to radians (long kind)
!*radtodeg_d*      LONG    Factor to convert radians to degrees (long kind)
!*MaxAstroTides*   INTEGER Maximum number of constituents for astronomical
!                          force
!*MaxConstituents* INTEGER Maximum number of tidal constituents at open
!                          boundaries
!*MaxGenerators*   INTEGER Maximum number of random generators which can be
!                          defined within the program
!*MaxHaloComms*    INTEGER Maximum number of allowed halo communications
!                          (send or receive)
!*MaxBioArids*     INTEGER Maximum number of biological array ids
!*MaxModArids*     INTEGER Maximum number physical model array ids
!*MaxTotArids*     INTEGER Maximum number all model array ids
!*MaxCIFTypes*     INTEGER Maximum number of CIF files
!*MaxCIFVars*      INTEGER Maximum number of data variables on a CIF data line
!*MaxGridTypes*    INTEGER Maximum number of surface grid files per type
!*MaxGridFiles*    INTEGER Maximum number of surface grid types
!*MaxIOFiles*      INTEGER Maximum number of I/O files per type
!*MaxIOTypes*      INTEGER Maximum number of model I/O types
!*MaxProgLevels*   INTEGER Maximum number of subprogram levels
!*MaxRestarts*     INTEGER Maximum number for writing restart conditions
!*MaxErrCodes*     INTEGER Maximum type error messages
!*MaxErrMesgs*     INTEGER Default maximum of error messages
!*MaxTimers*       INTEGER Maximum number of timers
!*DegreesOut*      LOGICAL Determines output unit of phase angles
!                          .TRUE.  => degrees
!                          .FALSE. => radians
!*Releasenum*      CHAR    COHERENS version
!*lencifline*      INTEGER Maximum length of a data line in a CIF file
!*lencifvar*       INTEGER Maximum length of a CIF data variable in string
!                          format 
!*lendesc*         INTEGER Length of "long_name" attribute
!*lenerrcode*      INTEGER Length of error code messages
!*lenformat*       INTEGER Length of a format specification
!*lenfreq*         INTEGER Length of the name of a frequency
!*leniofile*       INTEGER Maximum length for I/O file names
!*lenname*         INTEGER Length of "f90_name" attribute
!*lennode*         INTEGER Length of a nodal type string
!*lentime*         INTEGER Length of date/time string
!*lentitle*        INTEGER Length of simulation title
!*lenunit*         INTEGER Length of "units" attribute
!*lenversion*      INTEGER Length of model_version string
!*cifcom*          CHAR    Data separator on a CIF input line
!*cifend*          CHAR    Marks the end of a block ion a CIF file
!*cifsep*          CHAR    Separates data on a CIF input line
!*maxcifvars*      INTEGER Maximum allowed number of data on a CIF input line
!*maxlencif*       INTEGER Maximum length of a CIF input line string
!*IntegerFormat*   CHAR    Format specification for formatted integer output
!*RealFormat*      CHAR    Format specification for formatted real output
!*log_undef*       LOGICAL Flag for undefined or invalid logical data
!*int_undef*       INTEGER Flag for undefined or invalid integer values
!*izero*           INTEGER Zero (single precision)
!*izero_d*         LONGINT Zero (double precision)
!*longint_undef*   LONGINT Flag for undefined or invalid double precision
!                          integer values
!*real_undef*      REAL    Flag for undefined or invalid real values
!*real_flag*       REAL    Flag below which input data are considered as
!                          invalid (default value)
!*rzero*           REAL    Zero (single precision)
!*rzero_d*         LONG    Zero (double precision)
!*cdatetime_undef* CHAR    Flag for undefined date/times
!
!************************************************************************
! 

END MODULE syspars
