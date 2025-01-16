MODULE time_routines_ose
!************************************************************************
!
! *time_routines* Date/time routines
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
USE iopars
USE timepars
USE time_routines, ONLY: leap_year, convert_date, &
                        & log_timer_in, log_timer_out, &
                        & day_number

IMPLICIT NONE

!
! Interfaces
!

INTERFACE date_to_minute
   MODULE PROCEDURE date_to_minute_char, date_to_minute_int
END INTERFACE

INTERFACE add_mins_to_date
   MODULE PROCEDURE add_mins_to_date_char, add_mins_to_date_int
END INTERFACE

CONTAINS

!========================================================================

SUBROUTINE add_mins_to_date_char(chardate1,chardate2,numsteps,chardt)
!************************************************************************
!
! *add_mins_to_date_char* Add 'numsteps' time steps of 'dmins' mins to
!                         input date/time
!
! Author -
!
! Last update -
!
! Description - returns new date/time in character format
!
! Module calls - add_mins_to_date_int, convert_date
!
!************************************************************************

!
!*Arguments
!

CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardt
CHARACTER (LEN=lentime), INTENT(OUT) :: chardate2
INTEGER, INTENT(IN) :: numsteps

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*chardate1* CHAR    Old date/time
!*chardt*    CHAR    Time between 2 consecutive release of particles
!*chardate2* CHAR    New date/time
!*numsteps*  INTEGER Number of time steps
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER, DIMENSION(7) :: intdate1, intdate2, intdt


procname(pglev+1) = 'add_mins_to_date_char'
CALL log_timer_in()

!
!1. Convert to integer format
!----------------------------
!

intdate1 = convert_date(chardate1)
intdt    = convert_date(chardt)

!
!2. New date/time
!----------------
!

CALL add_mins_to_date_int(intdate1,intdate2,numsteps,intdt)

!
!3. Reconvert to character format
!--------------------------------
!

chardate2 = convert_date(intdate2)

CALL log_timer_out()

RETURN

END SUBROUTINE add_mins_to_date_char

!========================================================================

SUBROUTINE add_mins_to_date_int(intdate1,intdate2,numsteps,intdt)
!************************************************************************
!
! *add_mins_to_date_int* Add 'numsteps' time steps of 'dmins' mins to
!                        input date/time
!
! Author -
!
! Last update -
!
! Description - returns new date/time in integer format
!
! Module calls - day_number, leap_year
!
!************************************************************************

!
!* Arguments
!

INTEGER, INTENT(IN) :: numsteps
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdt
INTEGER, INTENT(OUT), DIMENSION(7) :: intdate2

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*intdate1*  INTEGER  Old date/time
!*intdate2*  INTEGER  New date/time
!*intdt*     INTEGER  Time between 2 consecutive releases of particles
!*numsteps*  INTEGER  Number of time steps
!
!------------------------------------------------------------------------------

!
!* Local variables
!

INTEGER :: ileap, l, m, msecs2, nday2, nhours2, nmins2, nmont2, nodays, &
         & nohours, nomins, nosecs, nsecs2, nyear2, ij
REAL :: daynum, daynumdt
INTEGER, DIMENSION(7) :: intdate3

procname(pglev+1) = 'add_mins_to_date_int'
CALL log_timer_in()

DO ij=1,7
   intdate3(ij)=numsteps*intdt(ij)
ENDDO

!
!1. Zero addition
!----------------
!

IF (numsteps.EQ.0.OR.SUM(intdt).EQ.0) THEN
   intdate2 = intdate1

!
!2. Add minutes
!--------------
!

ELSEIF (numsteps.GT.0) THEN

!  ---milliseconds
   msecs2 = 0

!  ---seconds
   nsecs2 = intdate1(6) + intdate3(6)
   nomins = 0
   IF (nsecs2.GE.60) THEN
      m = MOD(nsecs2,60)
      nomins = (nsecs2-m)/60
      nsecs2 = m
   ENDIF

!  ---minutes
   nmins2 = intdate1(5) + nomins + intdate3(5)
   nohours = 0
   IF (nmins2.GE.60) THEN
      m = MOD(nmins2,60)
      nohours = (nmins2-m)/60
      nmins2 = m
   ENDIF

!  ---hours
   nhours2 = intdate1(4) + nohours + intdate3(4)
   nodays = 0
   IF (nhours2.GE.24) THEN
      m = MOD(nhours2,24)
      nodays = (nhours2-m)/24
      nhours2 = m
   ENDIF

!  ---year
   CALL day_number(intdate1,daynum)

   nodays = nodays + daynum + intdate3(3)
   nyear2 = intdate1(1)
   ileap = leap_year(nyear2)
   DO WHILE (nodays.GE.(365+ileap))
      nodays = nodays - 365 - ileap
      nyear2 = nyear2 + 1
      ileap = leap_year(nyear2)
   ENDDO

!  ---month
   l_210: DO l=2,13
      IF (nodays.LT.(monthdays(l)+ileap)) EXIT l_210
   ENDDO l_210
   nmont2 = l - 1
   IF ((ileap.EQ.1).AND.(nodays.EQ.31)) nmont2 = 2

!  ---day
   IF ((ileap.EQ.1).AND.(nmont2.GT.2)) THEN
      nday2 = nodays + 1 - monthdays(nmont2) - ileap
   ELSE
      nday2 = nodays + 1 - monthdays(nmont2)
   ENDIF

!  ---store in date/time array
   intdate2(1) = nyear2
   intdate2(2:7) = (/nmont2,nday2,nhours2,nmins2,nsecs2,msecs2/)

!
!3. Substract minutes
!--------------------
!

ELSEIF (numsteps.LT.0) THEN

!  ---milliseconds
   msecs2 = 0

!  ---seconds
   nsecs2 = intdate1(6) + intdate3(6)
   nomins = 0
   IF (nsecs2.LT.0) THEN
      m = MOD(nsecs2,60)
      IF (m.EQ.0) THEN
         nomins = nsecs2/60
         nsecs2 = 0
      ELSE
         nomins = (nsecs2-m)/60 - 1
         nsecs2 = 60 + m
      ENDIF
   ENDIF

!  ---minutes
   nmins2 = intdate1(5) + nomins + intdate3(5)
   nohours = 0
   IF (nmins2.LT.0) THEN
      m = MOD(nmins2,60)
      IF (m.EQ.0) THEN
         nohours = nmins2/60
         nmins2 = 0
      ELSE
         nohours = (nmins2-m)/60 - 1
         nmins2 = 60 + m
      ENDIF
   ENDIF

!  ---hours
   nhours2 = intdate1(4) + nohours + intdate3(4)
   nodays = 0
   IF (nhours2.LT.0) THEN
      m = MOD(nhours2,24)
      IF (m.EQ.0) THEN
         nodays = nhours2/24
         nhours2 = 0
      ELSE
         nodays = (nhours2-m)/24 - 1
         nhours2 = 24 + m
      ENDIF
   ENDIF

!  ---year
   CALL day_number(intdate1,daynum)
   nodays = nodays + daynum + intdate3(3)
   nyear2 = intdate1(1)
   ileap = leap_year(nyear2)
   DO WHILE (nodays.LT.0)
      ileap = leap_year(nyear2-1)
      nodays = nodays + 365 + ileap
      nyear2 = nyear2 - 1
   ENDDO

!  ---month
   l_310: DO l=2,13
      IF (nodays.LT.(monthdays(l)+ileap)) EXIT l_310
   ENDDO l_310
   nmont2 = l - 1
   IF ((ileap.EQ.1).AND.(nodays.EQ.31)) nmont2 = 2

!  ---day
   IF ((ileap.EQ.1).AND.(nmont2.GT.2)) THEN
      nday2 = nodays + 1 - monthdays(nmont2) - ileap
   ELSE
      nday2 = nodays + 1 - monthdays(nmont2)
   ENDIF

!  ---store in date/time array
   intdate2(1) = nyear2
   intdate2(2:7) = (/nmont2,nday2,nhours2,nmins2,nsecs2,msecs2/)

ENDIF

CALL log_timer_out()

RETURN

END SUBROUTINE add_mins_to_date_int

!========================================================================

SUBROUTINE add_secs_to_date_int(intdate1,intdate2,numsteps,dsecs)
!************************************************************************
!
! *add_secs_to_date_int* Add 'numsteps' time steps of 'dsecs' secs to
!                        input date/time => modified to use INTEGER as input
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - returns new date/time in integer format
!
! Module calls - day_number, leap_year
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN) :: numsteps
INTEGER, INTENT(IN) :: dsecs
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1
INTEGER, INTENT(OUT), DIMENSION(7) :: intdate2

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*intdate1*  INTEGER  Old date/time
!*intdate2*  INTEGER  New date/time
!*numsteps*  INTEGER  Number of time steps
!*dsecs*     INTEGER  Number of secs in one time step                       [s]
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: ileap, l, m, msecs2, nday2, nhours2, nmins2, nmont2, nodays, &
         & nohours, nomins, nosecs, nsecs2, nyear2
REAL :: daynum


procname(pglev+1) = 'add_secs_to_date_int'
CALL log_timer_in()

nosecs = numsteps*dsecs

!
!1. Zero addition
!----------------
!

IF (numsteps.EQ.0.OR.dsecs.EQ.0) THEN
   intdate2 = intdate1

!
!2. Add seconds
!--------------
!

ELSEIF (numsteps.GT.0) THEN

!  ---milliseconds
   msecs2 = intdate1(7)
   IF (msecs2.GE.1000) THEN
      m = MOD(msecs2,1000)
      nosecs = nosecs + (msecs2-m)/1000
      msecs2 = m
   ENDIF

!  ---seconds
   nsecs2 = intdate1(6) + nosecs
   nomins = 0
   IF (nsecs2.GE.60) THEN
      m = MOD(nsecs2,60)
      nomins = (nsecs2-m)/60
      nsecs2 = m
   ENDIF

!  ---minutes
   nmins2 = intdate1(5) + nomins
   nohours = 0
   IF (nmins2.GE.60) THEN
      m = MOD(nmins2,60)
      nohours = (nmins2-m)/60
      nmins2 = m
   ENDIF

!  ---hours
   nhours2 = intdate1(4) + nohours
   nodays = 0
   IF (nhours2.GE.24) THEN
      m = MOD(nhours2,24)
      nodays = (nhours2-m)/24
      nhours2 = m
   ENDIF

!  ---year
   CALL day_number(intdate1,daynum)
   nodays = nodays + daynum
   nyear2 = intdate1(1)
   ileap = leap_year(nyear2)
   DO WHILE (nodays.GE.(365+ileap))
      nodays = nodays - 365 - ileap
      nyear2 = nyear2 + 1
      ileap = leap_year(nyear2)
   ENDDO

!  ---month
   l_210: DO l=2,13
      IF (nodays.LT.(monthdays(l)+ileap)) EXIT l_210
   ENDDO l_210
   nmont2 = l - 1
   IF ((ileap.EQ.1).AND.(nodays.EQ.31)) nmont2 = 2

!  ---day
   IF ((ileap.EQ.1).AND.(nmont2.GT.2)) THEN
      nday2 = nodays + 1 - monthdays(nmont2) - ileap
   ELSE
      nday2 = nodays + 1 - monthdays(nmont2)
   ENDIF

!  ---store in date/time array
   intdate2(1) = nyear2
   intdate2(2:7) = (/nmont2,nday2,nhours2,nmins2,nsecs2,msecs2/)

!
!3. Substract seconds
!--------------------
!

ELSEIF (numsteps.LT.0) THEN

!  ---milliseconds
   msecs2 = intdate1(7)
   IF (msecs2.LT.0) THEN
      m = MOD(msecs2,1000)
      IF (m.EQ.0) THEN
         nosecs = nosecs + msecs2/1000
         msecs2 = 0
      ELSE
         nosecs = nosecs + (msecs2-m)/1000 - 1
         msecs2 = 1000 + m
      ENDIF
   ENDIF

!  ---seconds
   nsecs2 = intdate1(6) + nosecs
   nomins = 0
   IF (nsecs2.LT.0) THEN
      m = MOD(nsecs2,60)
      IF (m.EQ.0) THEN
         nomins = nsecs2/60
         nsecs2 = 0
      ELSE
         nomins = (nsecs2-m)/60 - 1
         nsecs2 = 60 + m
      ENDIF
   ENDIF

!  ---minutes
   nmins2 = intdate1(5) + nomins
   nohours = 0
   IF (nmins2.LT.0) THEN
      m = MOD(nmins2,60)
      IF (m.EQ.0) THEN
         nohours = nmins2/60
         nmins2 = 0
      ELSE
         nohours = (nmins2-m)/60 - 1
         nmins2 = 60 + m
      ENDIF
   ENDIF

!  ---hours
   nhours2 = intdate1(4) + nohours
   nodays = 0
   IF (nhours2.LT.0) THEN
      m = MOD(nhours2,24)
      IF (m.EQ.0) THEN
         nodays = nhours2/24
         nhours2 = 0
      ELSE
         nodays = (nhours2-m)/24 - 1
         nhours2 = 24 + m
      ENDIF
   ENDIF

!  ---year
   CALL day_number(intdate1,daynum)
   nodays = nodays + daynum
   nyear2 = intdate1(1)
   ileap = leap_year(nyear2)
   DO WHILE (nodays.LT.0)
      ileap = leap_year(nyear2-1)
      nodays = nodays + 365 + ileap
      nyear2 = nyear2 - 1
   ENDDO

!  ---month
   l_310: DO l=2,13
      IF (nodays.LT.(monthdays(l)+ileap)) EXIT l_310
   ENDDO l_310
   nmont2 = l - 1
   IF ((ileap.EQ.1).AND.(nodays.EQ.31)) nmont2 = 2

!  ---day
   IF ((ileap.EQ.1).AND.(nmont2.GT.2)) THEN
      nday2 = nodays + 1 - monthdays(nmont2) - ileap
   ELSE
      nday2 = nodays + 1 - monthdays(nmont2)
   ENDIF

!  ---store in date/time array
   intdate2(1) = nyear2
   intdate2(2:7) = (/nmont2,nday2,nhours2,nmins2,nsecs2,msecs2/)

ENDIF

CALL log_timer_out()


RETURN

END SUBROUTINE add_secs_to_date_int

!========================================================================



SUBROUTINE date_to_minute_char(chardate,minutnum)
!************************************************************************
!
! *date_to_year_char* Convert date/time in minute date
!
! Author -
!
! Last update -
!
! Description -
!
! Module calls - convert_date, leap_year
!
!************************************************************************

!
!* Arguments
!

CHARACTER (LEN=lentime), INTENT(IN) :: chardate
REAL, INTENT(OUT) :: minutnum

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*chardate*  CHAR    Date/time
!*minutnum*   REAL    Time in minute
!
!------------------------------------------------------------------------------

!
!*Local variables
!

INTEGER :: ileap
REAL :: daynum, daynumdt
INTEGER, DIMENSION(7) :: intdate, intdt


procname(pglev+1) = 'date_to_minute_char'
CALL log_timer_in()

intdate = convert_date(chardate)
ileap = leap_year(intdate(1))

minutnum = ((monthdays(intdate(2))+intdate(3) - 1 &
                & + ileap)*24.0+intdate(4))*60.0+intdate(5)+&
                & (intdate(6)+intdate(7)/1000.0)/60.0
CALL log_timer_out()

RETURN

END SUBROUTINE date_to_minute_char

!========================================================================

SUBROUTINE date_to_minute_int(intdate,minutnum)
!************************************************************************
!
! *date_to_year_int* Convert date/time in  minute date
!
! Author -
!
! Last update -
!
! Description - input date is in integer format
!
! Module calls - leap_year
!
!************************************************************************

!
!* Arguments
!

INTEGER, INTENT(IN), DIMENSION(7) :: intdate
REAL, INTENT(OUT) :: minutnum

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*intdate*   INTEGER Date/time
!*yearnum*   REAL    Time in years
!
!------------------------------------------------------------------------------

!
!*Local variables
!

INTEGER :: ileap


procname(pglev+1) = 'date_to_minute_int'
CALL log_timer_in()

ileap = leap_year(intdate(1))

IF(intdate(2).EQ.0) THEN
    minutnum = 0.
ELSE
    minutnum = monthdays(intdate(2))
ENDIF

minutnum = ((minutnum+intdate(3) - 1 &
    & + ileap)*24.0+intdate(4))*60.0+intdate(5)+&
    & (intdate(6)+intdate(7)/1000.0)/60.0

CALL log_timer_out()

RETURN

END SUBROUTINE date_to_minute_int

!========================================================================



FUNCTION date_to_epoch(date_vec) RESULT(date)
  !ignore ms, return time to the 1970-01-01 in second
  USE time_routines
  INTEGER, DIMENSION(:), INTENT(IN) :: date_vec
  INTEGER :: date,n
  REAL :: daynum
  REAL(8) :: time_second_without_leap
  date = julian_date (date_vec(1), date_vec(2), date_vec(3)) * 86400
  date = date + date_vec(6)
  date = date + date_vec(5)*60
  date = date + date_vec(4)*3600

  !TODO, CAN CAUSE IMPRECISION
  !CALL day_number_int(date_vec, daynum)
  !time_second_without_leap = daynum*86400
  !date = date + NINT(time_second_without_leap)
  !leap year
  !DO n =1,(date_vec(1)-1970)
  !  date = date + 31536000 + leap_year(n+1970) * 86400
  !END DO
  !PRINT*,
  
END FUNCTION


FUNCTION epoch_to_date(date) RESULT(date_vec)
  !convert the time since 1970-01-01(date) to a vector of int for the date
  INTEGER, DIMENSION(7) :: date_vec
  INTEGER :: n
  INTEGER :: date
  INTEGER, DIMENSION(7) :: date_1970
  date_1970=(/1970,1,1,0,0,0,0/)
  CALL add_secs_to_date_int(date_1970,date_vec,1,date)

END FUNCTION



FUNCTION julian_date (yyyy, mm, dd) RESULT (julian)
   IMPLICIT NONE
   ! converts calendar date to Julian date
   ! cf Fliegel & Van Flandern, CACM 11(10):657, 1968
   ! example: julian_date(1970,1,1)=2440588
   INTEGER,INTENT(IN) :: yyyy,mm,dd
   INTEGER :: julian
   julian = dd-32075+1461*(yyyy+4800+(mm-14)/12)/4 + &
   367*(mm-2-((mm-14)/12)*12)/12- &
   3*((yyyy + 4900 + (mm - 14)/12)/100)/4 - 2440588
END FUNCTION julian_date

SUBROUTINE update_time
!************************************************************************
!
! *update_time* Update date/time
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Reference -
!
! Calling program - coherens_main
!
! Module calls - add_secs_to_date, convert_date, day_number
!
!************************************************************************
!
!USE switches
use time_routines
!
!*Local variables
!
INTEGER :: icmeteo
REAL :: daynum
INTEGER, DIMENSION(7) :: IDateTimeold


procname(pglev+1) = 'update_time'
CALL log_timer_in()

!---current date/time
IDateTimeold = IDateTime
CALL add_secs_to_date(IDateTimeold,IDateTime,1,delt2d)
CDateTime = convert_date(IDateTime)

!---number of since start
nosecsrun = INT(nt,KIND=kndilong)*INT(delt2d,KIND=kndilong) + &
          & INT(nt,KIND=kndilong)*(delt2d-INT(delt2d))

!---Julian day
CALL day_number(IDateTime,daynum)
julianday = daynum + 1

!---type of time step
predstep = ((nt-1)/ic3d)*ic3d.EQ.(nt-1)
corrstep = (nt/ic3d)*ic3d.EQ.nt

!---meteo input time
!metstepin = .FALSE.
!IF (iopt_meteo.EQ.1) THEN
!   icmeteo = modfiles(io_metsur,1,1)%tlims(3)
!   IF (icmeteo.NE.0) THEN
!      IF ((nt/icmeteo)*icmeteo.EQ.nt) metstepin = .TRUE.
!   ENDIF
!ENDIF

IF (loglev1.GT.0) WRITE (iolog,'(A)') CDateTime

CALL log_timer_out()


RETURN

END SUBROUTINE update_time

!========================================================================

SUBROUTINE update_time_back
!************************************************************************
!
! *update_time_back* Update date/time
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Reference -
!
! Calling program - coherens_main
!
! Module calls - add_secs_to_date, convert_date, day_number
!
!************************************************************************
!
!USE switches
use time_routines
!
!*Local variables
!
INTEGER :: icmeteo
REAL :: daynum
INTEGER, DIMENSION(7) :: IDateTimeold

procname(pglev+1) = 'update_time'
CALL log_timer_in()

!---current date/time
IDateTimeold = IDateTime
CALL add_secs_to_date(IDateTimeold,IDateTime,-1,delt2d)
CDateTime = convert_date(IDateTime)

!---number of since start
nosecsrun = INT(nt,KIND=kndilong)*INT(delt2d,KIND=kndilong) + &
          & INT(nt,KIND=kndilong)*(delt2d-INT(delt2d))

!---Julian day
CALL day_number(IDateTime,daynum)
julianday = daynum + 1

!---type of time step
predstep = ((nt-1)/ic3d)*ic3d.EQ.(nt-1)
corrstep = (nt/ic3d)*ic3d.EQ.nt

!---meteo input time
!metstepin = .FALSE.
!IF (iopt_meteo.EQ.1) THEN
!   icmeteo = modfiles(io_metsur,1,1)%tlims(3)
!   IF (icmeteo.NE.0) THEN
!      IF ((nt/icmeteo)*icmeteo.EQ.nt) metstepin = .TRUE.
!   ENDIF
!ENDIF
IF (loglev1.GT.0) WRITE (iolog,'(A)') CDateTime

CALL log_timer_out()

RETURN

END SUBROUTINE update_time_back


END MODULE time_routines_ose
