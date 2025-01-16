MODULE timepars
!************************************************************************
!
! *timepars* Time parameters
!
! Author - Patrick Luyten
!
! Version - @(COHERENS)timepars.f90  V2.0
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

LOGICAL :: corrstep, metstepin, predstep
CHARACTER (LEN=lentime) :: CDateTime, CEndDateTime = cdatetime_undef,&
                         & CStartDateTime = cdatetime_undef, ClockTime
INTEGER :: julianday, norestarts = 0, nstep = 0, nt = 0
INTEGER :: ic3d = 1, icnodal = 0
INTEGER (KIND=kndilong) :: nosecsrun = 0
REAL :: delt2d, delt3d, time_zone = 0.0
INTEGER, DIMENSION(6) :: time_convert = (/1,60,3600,86400,2629800,31557600/)
INTEGER, DIMENSION(7) :: IDateTime, IEndDateTime, IStartDateTime
INTEGER, DIMENSION(12) :: days_in_month = &
                        & (/31,28,31,30,31,30,31,31,30,31,30,31/)
INTEGER, DIMENSION(13) :: monthdays = &
                        & (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
INTEGER, DIMENSION(MaxRestarts) :: ntrestart = 0

SAVE

!
! Name           Type     Purpose
!------------------------------------------------------------------------------
!*corrstep*      LOGICAL  .TRUE. at corrector time steps
!*metstepin*     LOGICAL  .TRUE. for meteo input time steps
!*predstep*      LOGICAL  .TRUE. at predictor time steps
!*CDateTime*     CHAR     Current date/time
!*CEndDateTime*  CHAR     End date/time
!*CStartDateTime*CHAR     Start date/time
!*ClockTime*     CHAR     Real date/time at start of simulation
!*julianday*     INTEGER  Julian day (year day between 1 and 365/366)
!*norestarts*    INTEGER  Number of restarts
!*nstep*         INTEGER  Number of 2-D time steps for simulation
!*nt*            INTEGER  (2-D) time step counter
!*ic3d*          INTEGER  Counter for 3-D mode calculations
!                         (mode splitting scheme only)
!*icnodal*       INTEGER  Counter for update of nodal factors and phases
!*nosecsrun*     LONGINT  Number of seconds since start of simulation
!*delt2d*        Real     Time step for 2-D mode                           [s]
!*delt3d*        REAL     Time step for 3-D mode and scalars               [s]
!*time_zone*     REAL     Time difference between local and Greenwich time
!*time_convert*  INTEGER  Time conversion units                            [s]
!*IDateTime*     INTEGER  Current date/time
!*IEndDateTime*  INTEGER  End date/time
!*IStartDateTime*INTEGER  Start date/time
!*days_in_month* INTEGER  Number of days in each month
!*monthdays*     INTEGER  Day number of first day in each month
!*ntrestart*     INTEGER   Time indices for writing to restart file
!                         zone                                         [hours]
!
!************************************************************************
!
!integer Date/Time format : (year,month,day,hour,min,sec,millisec)
!character Date/Time format :yyyy/mm/dd;hh:mm:ss,xxx

END MODULE timepars
