MODULE time_routines
!************************************************************************
!
! *time_routines* Date/time routines
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Generic routines - add_secs_to_date, check_date, convert_date, date_to_year,
!                    day_number, diff_dates, num_time_steps, year_to_date
!
! Defined operators - .earlier., .later., .noearlier., .nolater.
!
! Routines - add_secs_to_phase, clock_date_time, diff_clock,
!            error_lbound_var_date, error_ubound_var_date, initialise_time,
!            leap_year, log_timer_in, log_timer_out, read_clock, suspend_proc,
!            update_time
!
!************************************************************************
!
USE iopars
USE syspars
USE timepars

IMPLICIT NONE

!
! Interfaces
!
INTERFACE add_secs_to_date
   MODULE PROCEDURE add_secs_to_date_char, add_secs_to_date_int
END INTERFACE

INTERFACE check_date
   MODULE PROCEDURE check_date_char, check_date_int
END INTERFACE

INTERFACE convert_date
   MODULE PROCEDURE convert_date_to_char, convert_date_to_int
END INTERFACE

INTERFACE date_to_year
   MODULE PROCEDURE date_to_year_char, date_to_year_int
END INTERFACE

INTERFACE day_number
   MODULE PROCEDURE day_number_char, day_number_int
END INTERFACE

INTERFACE diff_dates
   MODULE PROCEDURE diff_dates_char, diff_dates_int
END INTERFACE

INTERFACE num_time_steps
   MODULE PROCEDURE num_time_steps_char, num_time_steps_int
END INTERFACE

INTERFACE year_to_date
   MODULE PROCEDURE year_to_date_char, year_to_date_int
END INTERFACE

INTERFACE OPERATOR (.earlier.)
   MODULE PROCEDURE earlier_char, earlier_int
END INTERFACE

INTERFACE OPERATOR (.later.)
   MODULE PROCEDURE later_char, later_int
END INTERFACE

INTERFACE OPERATOR (.noearlier.)
   MODULE PROCEDURE noearlier_char, noearlier_int
END INTERFACE

INTERFACE OPERATOR (.nolater.)
   MODULE PROCEDURE nolater_char, nolater_int
END INTERFACE

CONTAINS

!========================================================================

SUBROUTINE add_secs_to_date_char(chardate1,chardate2,numsteps,dsecs)
!************************************************************************
!
! *add_secs_to_date_char* Add 'numsteps' time steps of 'dsecs' secs to
!                         input date/time
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - returns new date/time in character format
!
! Module calls - add_secs_to_date_int, convert_date
!
!************************************************************************
!
!*Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1
CHARACTER (LEN=lentime), INTENT(OUT) :: chardate2
INTEGER, INTENT(IN) :: numsteps
REAL, INTENT(IN) :: dsecs

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*chardate1* CHAR    Old date/time
!*chardate2* CHAR    New date/time
!*numsteps*  INTEGER Number of time steps
!*dsecs*     REAL    Number of secs in one time step                        [s]
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER, DIMENSION(7) :: intdate1, intdate2


procname(pglev+1) = 'add_secs_to_date_char'
CALL log_timer_in()

!
!1. Convert to integer format
!----------------------------
!

intdate1 = convert_date(chardate1)

!
!2. New date/time
!----------------
!

CALL add_secs_to_date_int(intdate1,intdate2,numsteps,dsecs)

!
!3. Reconvert to character format
!--------------------------------
!

chardate2 = convert_date(intdate2)

CALL log_timer_out()


RETURN

END SUBROUTINE add_secs_to_date_char

!========================================================================

SUBROUTINE add_secs_to_date_int(intdate1,intdate2,numsteps,dsecs)
!************************************************************************
!
! *add_secs_to_date_int* Add 'numsteps' time steps of 'dsecs' secs to
!                        input date/time
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
REAL, INTENT(IN) :: dsecs
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1
INTEGER, INTENT(OUT), DIMENSION(7) :: intdate2

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*intdate1*  INTEGER  Old date/time
!*intdate2*  INTEGER  New date/time
!*numsteps*  INTEGER  Number of time steps
!*dsecs*     REAL     Number of secs in one time step                       [s]
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

nosecs = numsteps*INT(dsecs)

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
   msecs2 = intdate1(7) + numsteps*INT(1000*(dsecs-INT(dsecs)))
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
   msecs2 = intdate1(7) + numsteps*INT(1000*(dsecs-INT(dsecs)))
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

SUBROUTINE add_secs_to_phase(phasein,phaseout,numsteps,dsecs,freq)
!************************************************************************
!
! *add_secs_to_phase* Calculate phasein+MOD(numsteps*dsecs*freq,twopi)
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - algorithm avoids rounding errors
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN) :: numsteps
REAL, INTENT(IN) :: dsecs, freq, phasein
REAL, INTENT(OUT) :: phaseout

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*phasein*   REAL     Old phase
!*phaseout*  REAL     New phase
!*numsteps*  INTEGER  Number of time steps
!*dsecs*     REAL     Number of secs in one time step                       [s]
!*freq*      REAL     Frequency                                         [rad/s]
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: iperiod, isign, msecs, n, noperiods, nosecs
REAL :: dmsecs


msecs = INT(numsteps)*NINT(1000*dsecs)
nosecs = msecs/1000
dmsecs = 0.001*(msecs-1000*nosecs)
isign = SIGN(1,INT(nosecs))
nosecs = ABS(nosecs)
iperiod = twopi/freq
noperiods = nosecs/iperiod
phaseout = 0.0
n_110: DO n=1,noperiods
   phaseout = phaseout + freq*iperiod
   phaseout = MOD(phaseout,twopi)
ENDDO n_110
nosecs = MOD(nosecs,iperiod)
phaseout = phaseout + freq*(nosecs+dmsecs)
phaseout = MOD(phaseout,twopi)
phaseout = MOD(phasein+isign*phaseout,twopi)
IF (phaseout.LT.0.0) phaseout = phaseout + twopi


RETURN

END SUBROUTINE add_secs_to_phase

!========================================================================

SUBROUTINE check_date_char(chardate,varname)
!************************************************************************
!
! *check_date_char* Check date/time variable
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/time is in character format
!
! Module calls - convert_date, error_lbound_arr, error_limits_arr,
!                leap_year
!
!************************************************************************
!
USE error_routines, ONLY: error_lbound_arr, error_limits_arr

!
!*Arguments
!
CHARACTER (LEN=*), INTENT(IN) :: varname
CHARACTER (LEN=lentime), INTENT(IN) :: chardate

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*chardate*  CHAR     Date/time
!*varname*   CHAR     Name of date/time variable
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ileap, ndays
INTEGER, DIMENSION(7) :: intdate


procname(pglev+1) = 'check_date_char'
CALL log_timer_in()

intdate = convert_date(chardate)
CALL error_lbound_arr(intdate(1),varname,0,.FALSE.,1,indx=(/1/))
CALL error_limits_arr(intdate(2),varname,1,12,1,indx=(/2/))
ileap = leap_year(intdate(1))
ndays = days_in_month(intdate(2))
IF (ileap.EQ.1.AND.intdate(2).GT.1) ndays = ndays + 1
CALL error_limits_arr(intdate(3),varname,1,ndays,1,indx=(/3/))
CALL error_limits_arr(intdate(4),varname,0,23,1,indx=(/4/))
CALL error_limits_arr(intdate(5),varname,0,59,1,indx=(/5/))
CALL error_limits_arr(intdate(6),varname,0,59,1,indx=(/6/))
CALL error_limits_arr(intdate(7),varname,0,999,1,indx=(/7/))

CALL log_timer_out()


RETURN

END SUBROUTINE check_date_char

!========================================================================

SUBROUTINE check_date_int(intdate,varname)
!************************************************************************
!
! *check_date_int* Check date/time variable
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/time is in integer format
!
! Module calls - error_lbound_arr, error_limits_arr, leap_year
!
!************************************************************************
!
USE error_routines, ONLY: error_lbound_arr, error_limits_arr

!
!*Arguments
!
CHARACTER (LEN=*), INTENT(IN) :: varname
INTEGER, INTENT(IN), DIMENSION(7) :: intdate

!
! Name       Type     Purpose
!------------------------------------------------------------------------------
!*intdate*   INTEGER  Date/time
!*varname*   CHAR     Name of date/time variable
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ileap, ndays


procname(pglev+1) = 'check_date_int'
CALL log_timer_in()

CALL error_lbound_arr(intdate(1),varname,0,.FALSE.,1,indx=(/1/))
CALL error_limits_arr(intdate(2),varname,1,12,1,indx=(/2/))
ileap = leap_year(intdate(1))
ndays = days_in_month(intdate(2))
IF (ileap.EQ.1.AND.intdate(2).GT.1) ndays = ndays + 1
CALL error_limits_arr(intdate(3),varname,1,ndays,1,indx=(/3/))
CALL error_limits_arr(intdate(4),varname,0,23,1,indx=(/4/))
CALL error_limits_arr(intdate(5),varname,0,59,1,indx=(/5/))
CALL error_limits_arr(intdate(6),varname,0,59,1,indx=(/6/))
CALL error_limits_arr(intdate(7),varname,0,999,1,indx=(/7/))

CALL log_timer_out()


RETURN

END SUBROUTINE check_date_int

!========================================================================

SUBROUTINE clock_date_time(charclock)
!************************************************************************
!
! *clock_date_time* Returns date/time from systems's real-time clock
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls -
!
!************************************************************************
!
!*Arguments
!
CHARACTER (LEN=lentime), INTENT(OUT) :: charclock

!
! Name       Type   Purpose
!------------------------------------------------------------------------------
!*charclock* CHAR   Date/time from system clock
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=8) :: cdate
CHARACTER (LEN=10) :: ctime


CALL DATE_AND_TIME(DATE=cdate,TIME=ctime)
charclock = cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//';'//ctime(1:2)//&
          & ':'//ctime(3:4)//':'//ctime(5:6)//','//ctime(8:10)


RETURN

END SUBROUTINE clock_date_time

!========================================================================

FUNCTION convert_date_to_char(intdate) RESULT(chardate)
!************************************************************************
!
! *convert_date_to_char* Convert date/time from integer to character format
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls - error_abort
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!* Arguments
!
INTEGER, INTENT(IN), DIMENSION(7) :: intdate
CHARACTER (LEN=lentime) :: chardate

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*intdate*   INTEGER Date/time in integer format
!*chardate*  CHAR    Date/time in character format
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: istat


procname(pglev+1) = 'convert_date_to_char'
CALL log_timer_in()

istat = 0
WRITE (chardate,9001,IOSTAT=istat) intdate
IF (istat.NE.0) THEN
   IF (errchk) WRITE (ioerr,9002) 'Invalid Date/Time: ', intdate
   CALL error_abort('convert_date_to_char',ierrno_write)
ENDIF

CALL log_timer_out()


RETURN

9001 FORMAT(I4.4,'/',I2.2,'/',I2.2,';',I2.2,':',I2.2,':',I2.2,':',I3.3)
9002 FORMAT(A,7(I6,1X))

END FUNCTION convert_date_to_char

!========================================================================

FUNCTION convert_date_to_int(chardate) RESULT(intdate)
!************************************************************************
!
! *convert_date_to_int* Convert date/time from character to integer format
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls - error_abort
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate
INTEGER, DIMENSION(7) :: intdate

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*chardate*  CHAR    Date/time in character format
!*intdate*   INTEGER Date/time in integer format
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: istat


procname(pglev+1) = 'convert_date_to_int'
CALL log_timer_in()

READ (chardate,'(I4,5(TR1,I2),TR1,I3)',IOSTAT=istat) intdate
IF (istat.NE.0) THEN
   IF (errchk) WRITE (ioerr,'(A)') 'Invalid Date/Time: '//chardate
   CALL error_abort('convert_date_to_char',ierrno_read)
ENDIF

CALL log_timer_out()


RETURN

END FUNCTION convert_date_to_int

!========================================================================

SUBROUTINE date_to_year_char(chardate,yearnum)
!************************************************************************
!
! *date_to_year_char* Convert date/time in year date
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date is in character format
!
! Module calls - convert_date, leap_year
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate
REAL, INTENT(OUT) :: yearnum

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*chardate*  CHAR    Date/time
!*yearnum*   REAL    Time in years
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ileap
REAL :: daynum
INTEGER, DIMENSION(7) :: intdate


procname(pglev+1) = 'date_to_year_char'
CALL log_timer_in()

intdate = convert_date(chardate)
ileap = leap_year(intdate(1))
daynum = monthdays(intdate(2))+intdate(3) - 1 +&
                & (intdate(4)+(intdate(5)+(intdate(6)+&
                &  intdate(7)/1000.0)/60.0)/60.0)/24.0 + ileap
yearnum = intdate(1) + daynum/(365+ileap)

CALL log_timer_out()


RETURN

END SUBROUTINE date_to_year_char

!========================================================================

SUBROUTINE date_to_year_int(intdate,yearnum)
!************************************************************************
!
! *date_to_year_int* Convert date/time in year date
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
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
REAL, INTENT(OUT) :: yearnum

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
REAL :: daynum


procname(pglev+1) = 'date_to_year_int'
CALL log_timer_in()

ileap = leap_year(intdate(1))
daynum = monthdays(intdate(2))+intdate(3) - 1 +&
                & (intdate(4)+(intdate(5)+(intdate(6)+&
                &  intdate(7)/1000.0)/60.0)/60.0)/24.0 + ileap
yearnum = intdate(1) + daynum/(365+ileap)

CALL log_timer_out()


RETURN

END SUBROUTINE date_to_year_int

!========================================================================

SUBROUTINE day_number_char(chardate,daynum)
!************************************************************************
!
! *day_number_char* Return day number of the year
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - result is between 0 and 364 or 365 (leap year)
!             - input date/time in character format
!
! Module calls - convert_date, day_number_int
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate
REAL, INTENT(OUT) :: daynum

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*chardate*  CHAR    Date/time
!*daynum*    REAL    Day number
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER, DIMENSION(7) :: intdate


procname(pglev+1) = 'day_number_char'
CALL log_timer_in()

!
!1. Convert date to integer format
!---------------------------------
!

intdate = convert_date(chardate)

!
!2. Day number
!-------------
!

CALL day_number_int(intdate,daynum)

CALL log_timer_out()


RETURN

END SUBROUTINE day_number_char

!========================================================================

SUBROUTINE day_number_int(intdate,daynum)
!************************************************************************
!
! *day_number_int* Return day number of the year
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - result is between 0 and 364 or 365 (leap year)
!             - input date/time in integer format
!
! Module calls - leap_year
!
!************************************************************************
!
!* Arguments
!
REAL, INTENT(OUT) :: daynum
INTEGER, INTENT(IN), DIMENSION(7) :: intdate

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*intdate*   INTEGER Date/time
!*daynum*    REAL    Day number
!
!------------------------------------------------------------------------------
!


procname(pglev+1) = 'day_number_int'
CALL log_timer_in()

daynum = monthdays(intdate(2))+intdate(3) - 1 +&
                & (intdate(4)+(intdate(5)+(intdate(6)+&
                &  intdate(7)/1000.0)/60.0)/60.0)/24.0
IF ((leap_year(intdate(1)).EQ.1).AND.(intdate(2).GT.2)) daynum = daynum+1

CALL log_timer_out()


RETURN

END SUBROUTINE day_number_int

!========================================================================

FUNCTION diff_clock(npccold)
!************************************************************************
!
! *diff_clock* Return number of clock counts since a previous call to
!              process clock
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN) :: npccold
INTEGER :: diff_clock

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*npccold*    INTEGER Old clock count
!*diff_clock* INTEGER New minus old clock count
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: npccnew


CALL SYSTEM_CLOCK(COUNT=npccnew)
IF (npccnew.GE.npccold) THEN
   diff_clock = npccnew - npccold
ELSE
   diff_clock = npcc_max + npccnew - npccold
ENDIF


RETURN

END FUNCTION diff_clock

!========================================================================

SUBROUTINE diff_dates_char(chardate1,chardate2,tunit,nosecs,millisecs,rtime)
!************************************************************************
!
! *diff_dates_char* Return time difference between two character dates
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - if tunit = 0, result is integer and returned in seconds
!                (nosecs) and (optionally) milliseconds
!             - if tunit > 0, result is real and returned in rtime
!
! Module calls - convert_date, diff_dates_int
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardate2
INTEGER, INTENT(IN) :: tunit
INTEGER, INTENT(OUT), OPTIONAL :: millisecs
INTEGER (KIND=kndilong), INTENT(OUT), OPTIONAL :: nosecs
REAL, INTENT(OUT), OPTIONAL :: rtime

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*chardate1* CHAR   Start date
!*chardate2* CHAR   End date
!*tunit*     INTEGER Type of result
!              = 0   => seconds and milliseconds
!              = 1   => seconds
!              = 2   => minutes
!              = 3   => hours
!              = 4   => days
!              = 5   => months
!              = 6   => years
!*nosecs*    LONGINT Result in seconds
!*millisecs* INTEGER Residual milleseconds
!*rtime*     REAL    Result in appropriate units
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER, DIMENSION(7) :: intdate1, intdate2


procname(pglev+1) = 'diff_dates_char'
CALL log_timer_in()

!
!1. Convert dates to integer format
!----------------------------------
!

intdate1 = convert_date(chardate1)
intdate2 = convert_date(chardate2)

!
!2. Number of seconds
!--------------------
!

IF (tunit.EQ.0) THEN
   CALL diff_dates_int(intdate1,intdate2,0,nosecs,millisecs)
ELSE
   CALL diff_dates_int(intdate1,intdate2,tunit,rtime=rtime)
ENDIF

CALL log_timer_out()


RETURN

END SUBROUTINE diff_dates_char

!========================================================================

SUBROUTINE diff_dates_int(intdate1,intdate2,tunit,nosecs,millisecs,rtime)
!************************************************************************
!
! *diff_dates_int* Return time difference between two integer dates
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - if tunit = 0, result is integer and returned in seconds
!               (nosecs) and (optionally) milliseconds
!             - if tunit > 0, result is real and returned in rtime
!
! Module calls - day_number, .earlier., .later., leap_year
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN) :: tunit
INTEGER, INTENT(OUT), OPTIONAL :: millisecs
INTEGER (KIND=kndilong), INTENT(OUT), OPTIONAL :: nosecs
REAL, INTENT(OUT), OPTIONAL :: rtime
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdate2

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*intdate1*  INTEGER Start date
!*intdate2*  INTEGER End date
!*tunit*     INTEGER Type of result
!              = 0   => seconds and milliseconds
!              = 1   => seconds
!              = 2   => minutes
!              = 3   => hours
!              = 4   => days
!              = 5   => months
!              = 6   => years
!*nosecs*    LONGINT Result in seconds
!*millisecs* INTEGER Residual milleseconds
!*rtime*     REAL    Result in appropriate units
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: isign, iyear, iyear1, iyear2, ndays1, ndays2, tconv
INTEGER (KIND=kndilong) :: nodays, numsecs
REAL :: daynum1, daynum2
INTEGER, DIMENSION(7) :: idate1, idate2


procname(pglev+1) = 'diff_dates_int'
CALL log_timer_in()

!
!1. Determine which date comes first
!-----------------------------------
!

IF (intdate1.earlier.intdate2) THEN
   idate1 = intdate1; idate2 = intdate2; isign = 1
ELSEIF (intdate1.later.intdate2) THEN
   idate1 = intdate2; idate2 = intdate1; isign = -1
ELSE
   isign = 0
ENDIF

IF (isign.EQ.0) THEN
   numsecs = 0
   IF (PRESENT(millisecs)) millisecs = 0
   GOTO 1000
ENDIF

!
!2. Number of days between start/end dates
!-----------------------------------------
!

iyear1 = idate1(1)
iyear2 = idate2(1)
CALL day_number(idate1,daynum1)
CALL day_number(idate2,daynum2)

ndays1 = daynum1; ndays2 = daynum2
IF (iyear1.EQ.iyear2) THEN
   nodays = ndays2 - ndays1
ELSE
   nodays = 365 + leap_year(iyear1) - ndays1
   iyear_110: DO iyear=iyear1+1,iyear2-1
      nodays = nodays + 365 + leap_year(iyear)
   ENDDO iyear_110
   nodays = nodays + ndays2
ENDIF

!
!3. Number of seconds
!--------------------
!

numsecs = isign*(nodays*86400_kndilong-((idate1(4)*60+idate1(5))*60+&
               & idate1(6))+(idate2(4)*60+idate2(5))*60+idate2(6))

1000 CONTINUE

!
!4. Return result in appropriate format
!--------------------------------------
!

IF (tunit.EQ.0) THEN
   nosecs = numsecs
   IF (PRESENT(millisecs)) THEN
      millisecs = idate2(7) - idate1(7)
      IF (millisecs.LT.0) THEN
         millisecs = millisecs + 1000
         nosecs = nosecs + isign
      ENDIF
      millisecs = isign*millisecs
   ENDIF
ELSE
   tconv = time_convert(tunit)
   rtime =  numsecs/tconv + MOD(numsecs,INT(tconv,KIND=kndilong))/REAL(tconv)
ENDIF

CALL log_timer_out()


RETURN

END SUBROUTINE diff_dates_int

!========================================================================

FUNCTION earlier_char(chardate1,chardate2)
!************************************************************************
!
! *earlier_char* Checks whether date 'chardate1' is earlier than 'chardate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in character format
!
! Module calls - convert_date
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardate2
LOGICAL :: earlier_char

!
! Name          Type    Purpose
!------------------------------------------------------------------------------
!*chardate1*    CHAR    First date/time
!*chardate2*    CHAR    Second date/time
!*earlier_char* LOGICAL .TRUE. if chardate1 is earlier than chardate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l
INTEGER, DIMENSION(7) :: intdate1, intdate2


intdate1 = convert_date(chardate1)
intdate2 = convert_date(chardate2)

earlier_char = .FALSE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      earlier_char = MERGE(.TRUE.,.FALSE.,intdate1(l).LT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION earlier_char

!========================================================================

FUNCTION earlier_int(intdate1,intdate2)
!************************************************************************
!
! *earlier_int* checks whether date 'intdate1' is earlier than 'intdate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in integer format
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdate2
LOGICAL :: earlier_int

!
! Name         Type    Purpose
!------------------------------------------------------------------------------
!*intdate1*    INTEGER First date/time
!*intdate2*    INTEGER Second date/time
!*earlier_int* LOGICAL .TRUE. if intdate1 is earlier than intdate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l


earlier_int = .FALSE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      earlier_int = MERGE(.TRUE.,.FALSE.,intdate1(l).LT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION earlier_int

!========================================================================

SUBROUTINE error_lbound_var_date(cdate,varname,cdatemin,matchmin)
!************************************************************************
!
! *error_lbound_var_date* Checks whether date/time 'cdate' is not earlier than
!                         'cdatemin' if 'matchmin' is .TRUE. or is later than
!                         'cdatemin' if 'matchmin' is .FALSE.
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - displays error message if needed
!
! Module calls - .earlier., .nolater.
!
!************************************************************************
!
!*Arguments
!
LOGICAL, INTENT(IN) :: matchmin
CHARACTER (LEN=*), INTENT(IN) :: varname
CHARACTER (LEN=lentime), INTENT(IN) :: cdate, cdatemin

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*cdate*    CHAR    Input date/time
!*varname*  CHAR    Name of date/time variable
!*cdatemin* CHAR    Date/time used for comparison
!*matchmin* LOGICAL Allows matching with cdatemin if .TRUE.
!
!------------------------------------------------------------------------------
!

IF (matchmin) THEN
   IF (cdate.earlier.cdatemin) THEN
      nerrs = nerrs + 1
      IF (errchk.AND.nerrs.LE.maxerrors) THEN
         WRITE (ioerr,'(A)') 'Invalid value for date/time parameter '&
                           & //TRIM(varname)//': '//cdate
         WRITE (ioerr,'(A)') 'Must be not earlier than: '//cdatemin
      ENDIF
   ENDIF
ELSEIF (cdate.nolater.cdatemin) THEN
   nerrs = nerrs + 1
   IF (errchk.AND.nerrs.LE.maxerrors) THEN
      WRITE (ioerr,'(A)') 'Invalid value for date/time parameter '&
                        & //TRIM(varname)//': '//cdate
      WRITE (ioerr,'(A)') 'Must be later than: '//cdatemin
   ENDIF
ENDIF


RETURN

END SUBROUTINE error_lbound_var_date

!========================================================================

SUBROUTINE error_ubound_var_date(cdate,varname,cdatemax,matchmax)
!************************************************************************
!
! *error_ubound_var_date* Checks whether date/time cdate is not later than
!                         'cdatemax' if 'matchmax' is .TRUE. or is earlier
!                         than 'cdatemax' if 'matchmax' is .FALSE.
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - displays error message if needed
!
! Module calls - .later., .noearlier.
!
!************************************************************************
!
!*Arguments
!
LOGICAL, INTENT(IN) :: matchmax
CHARACTER (LEN=*), INTENT(IN) :: varname
CHARACTER (LEN=lentime), INTENT(IN) :: cdate, cdatemax

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*cdate*    CHAR    Input date/time
!*varname*  CHAR    Name of date/time variable
!*cdatemax* CHAR    Date/time used for comparison
!*matchmax* LOGICAL Allows matching with cdatemin if .TRUE.
!
!------------------------------------------------------------------------------
!

IF (matchmax) THEN
   IF (cdate.later.cdatemax) THEN
      nerrs = nerrs + 1
      IF (errchk.AND.nerrs.LE.maxerrors) THEN
         WRITE (ioerr,'(A)') 'Invalid value for date/time parameter '&
                           & //TRIM(varname)//': '//cdate
         WRITE (ioerr,'(A)') 'Must be not later than: '//cdatemax
      ENDIF
   ENDIF
ELSEIF (cdate.noearlier.cdatemax) THEN
   nerrs = nerrs + 1
   IF (errchk.AND.nerrs.LE.maxerrors) THEN
      WRITE (ioerr,'(A)') 'Invalid value for date/time parameter '&
                        & //TRIM(varname)//': '//cdate
      WRITE (ioerr,'(A)') 'Must be earlier than: '//cdatemax
   ENDIF
ENDIF


RETURN

END SUBROUTINE error_ubound_var_date

!========================================================================

SUBROUTINE initialise_time
!************************************************************************
!
! *initialise_time* Initialise date/time parameters
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
! Module calls - convert_date, day_number, num_time_steps
!
!************************************************************************
!
!USE switches

!
!*Local variables
!
CHARACTER (LEN=15) :: cval
CHARACTER (LEN=22) :: cstep
REAL :: daynum


procname(pglev+1) = 'initialise_time'
CALL log_timer_in()

!---time step (seconds)
delt2d = 600.0

!---date/time
CDateTime = CStartDateTime
IStartDateTime = convert_date(CStartDateTime)
IDateTime = IStartDateTime
IEndDateTime = convert_date(CEndDateTime)
CALL day_number(IStartDateTime,daynum)
julianday = daynum + 1
nosecsrun = 0

!---time steps
!delt3d = MERGE(delt2d,ic3d*delt2d,iopt_grid_nodim.EQ.1)
delt3d = delt2d
nt = 0
IF (loglev1.GT.0) THEN
   WRITE (cval,'(G15.7)') delt3d; cval = ADJUSTL(cval)
   WRITE (iolog,'(A)') 'delt3d = '//TRIM(cval)
ENDIF
!metstepin = iopt_meteo.EQ.1
metstepin = .FALSE.

!---number of time steps
CALL num_time_steps(IStartDateTime,IEndDateTime,delt2d,nstep)
IF (loglev1.GT.0) THEN
   WRITE (cstep,'(I22)') nstep; cstep = ADJUSTL(cstep)
   WRITE (iolog,'(A)') 'nstep = '//TRIM(cstep)
ENDIF

CALL log_timer_out()


RETURN

END SUBROUTINE initialise_time

!========================================================================

FUNCTION later_char(chardate1,chardate2)
!************************************************************************
!
! *later_char* Checks whether date 'chardate1' is later than 'chardate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in character format
!
! Module calls - convert_date
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardate2
LOGICAL :: later_char

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*chardate1*  CHAR    First date/time
!*chardate2*  CHAR    Second date/time
!*later_char* LOGICAL .TRUE. if chardate1 is later than chardate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l
INTEGER, DIMENSION(7) :: intdate1, intdate2


intdate1 = convert_date(chardate1)
intdate2 = convert_date(chardate2)

later_char = .FALSE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      later_char = MERGE(.TRUE.,.FALSE.,intdate1(l).GT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION later_char

!========================================================================

FUNCTION later_int(intdate1,intdate2)
!************************************************************************
!
! *later_int* Checks whether date 'intdate1' is later than 'intdate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in integer format
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdate2
LOGICAL :: later_int

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*intdate1*   INTEGER First date/time
!*intdate2*   INTEGER Second date/time
!*later_int*  LOGICAL .TRUE. if intdate1 is later than intdate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l


later_int = .FALSE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      later_int = MERGE(.TRUE.,.FALSE.,intdate1(l).GT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION later_int

!========================================================================

FUNCTION leap_year(iyear)
!************************************************************************
!
! *leap_year* Return 1 or 0 if 'iyear' is a leap year or not
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN) :: iyear
INTEGER :: leap_year

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*iyear*      INTEGER Year
!*leap_year*  INTEGER Number of leap days
!
!------------------------------------------------------------------------------
!


IF (MOD(iyear,4).EQ.0.AND.(MOD(iyear,100).NE.0.OR.MOD(iyear,400).EQ.0)) THEN
   leap_year = 1
ELSE
   leap_year = 0
ENDIF


RETURN

END FUNCTION leap_year

!========================================================================

SUBROUTINE log_timer_in(npcc,ivarid,logname,info)
!************************************************************************
!
! *log_timer_in* Write log info and initialise timer on entry of a procedure
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls - inquire_var, read_clock
!
!************************************************************************
!
!USE modvars_routines, ONLY: inquire_var

!
!* Arguments
!
LOGICAL, INTENT(IN), OPTIONAL :: info
CHARACTER (LEN=*), INTENT(IN), OPTIONAL :: logname
INTEGER, INTENT(IN), OPTIONAL :: ivarid
INTEGER, INTENT(OUT), OPTIONAL :: npcc

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*npcc*       INTEGER Clock count
!*ivarid*     INTEGER Variable id
!*logname*    CHAR    String used in log output if present, otherwise name of
!                     calling program is taken
!*info*       LOGICAL No log info is written if present and .FALSE.
!
!------------------------------------------------------------------------------
!
!*Local variables
!
LOGICAL :: infox
CHARACTER (LEN=lenname) :: f90_name
CHARACTER (LEN=200) :: pname
INTEGER :: ivaridx, l


pglev = pglev + 1

!
!1. Optional arguments
!---------------------
!

IF (PRESENT(info)) THEN
   infox = info
ELSE
   infox = .TRUE.
ENDIF

IF (PRESENT(ivarid)) THEN
   ivaridx = ivarid
ELSE
   ivaridx = 0
ENDIF

IF (PRESENT(logname)) THEN
   l = LEN(logname)
   pname = logname(1:l)
ELSE
   pname = procname(pglev)
ENDIF

!
!2. Write info
!-------------
!

IF (infox.AND.loglev1.GE.pglev) THEN
   IF (ivaridx.GT.0) THEN
      !CALL inquire_var(ivaridx,f90_name=f90_name)
      IF (pglev.LT.10) THEN
         WRITE (iolog,logfmt1) pglev, TRIM(pname)//': '//TRIM(f90_name)
      ELSE
         WRITE (iolog,logfmt2) pglev, TRIM(pname)//': '//TRIM(f90_name)
      ENDIF
   ELSE
      IF (pglev.LT.10) THEN
         WRITE (iolog,logfmt1) pglev, TRIM(pname)
      ELSE
         WRITE (iolog,logfmt2) pglev, TRIM(pname)
      ENDIF
   ENDIF
ENDIF

!
!3. Timer
!--------
!

IF (timer) THEN
   IF (PRESENT(npcc)) npcc = read_clock()
ENDIF


RETURN

END SUBROUTINE log_timer_in

!========================================================================

SUBROUTINE log_timer_out(npcc,itm_type,info)
!************************************************************************
!
! *log_timer_out* Write log info and evaluate timer on exit of a procedure
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls - diff_clock
!
!************************************************************************
!
!* Arguments
!
LOGICAL, INTENT(IN), OPTIONAL :: info
INTEGER, INTENT(IN), OPTIONAL :: itm_type, npcc

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*npcc*       INTEGER Old clock count
!*itm_type*   INTEGER Timer type
!*info*       LOGICAL No log info is written if present and .FALSE.
!
!------------------------------------------------------------------------------

IF (timer) THEN
   IF (PRESENT(npcc)) THEN
      nopcc(itm_type) = nopcc(itm_type) + diff_clock(npcc)
   ENDIF
ENDIF

IF (PRESENT(info)) THEN
   IF (.NOT.info) THEN
      pglev = pglev - 1
      RETURN
   ENDIF
ENDIF

IF (loglev2.GE.pglev) THEN
   IF (pglev.LT.10) THEN
      WRITE (iolog,logfmt1) pglev, logexit
   ELSE
      WRITE (iolog,logfmt2) pglev, logexit
   ENDIF
ENDIF

pglev = pglev - 1


RETURN

END SUBROUTINE log_timer_out

!========================================================================

FUNCTION noearlier_char(chardate1,chardate2)
!************************************************************************
!
! *noearlier_char* Checks whether date 'chardate1' is not earlier than
!                  'chardate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in character format
!
! Module calls - convert_date
!
!************************************************************************
!
!*Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardate2
LOGICAL :: noearlier_char

!
! Name            Type    Purpose
!------------------------------------------------------------------------------
!*chardate1*      CHAR    First date/time
!*chardate2*      CHAR    Second date/time
!*noearlier_char* LOGICAL .TRUE. if chardate1 is not earlier than chardate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l
INTEGER, DIMENSION(7) :: intdate1, intdate2


intdate1 = convert_date(chardate1)
intdate2 = convert_date(chardate2)

noearlier_char = .TRUE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      noearlier_char = MERGE(.TRUE.,.FALSE.,intdate1(l).GT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION noearlier_char

!========================================================================

FUNCTION noearlier_int(intdate1,intdate2)
!************************************************************************
!
! *noearlier_int* Checks whether date 'intdate1' is not earlier than 'intdate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in integer format
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdate2
LOGICAL :: noearlier_int

!
! Name            Type    Purpose
!------------------------------------------------------------------------------
!*intdate1*       INTEGER First date/time
!*intdate2*       INTEGER Second date/time
!*noearlier_int*  LOGICAL .TRUE. if intdate1 is not earlier than intdate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l


noearlier_int = .TRUE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      noearlier_int = MERGE(.TRUE.,.FALSE.,intdate1(l).GT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION noearlier_int

!========================================================================

FUNCTION nolater_char(chardate1,chardate2)
!************************************************************************
!
! *nolater_char* Checks whether date 'chardate1' is not later than 'chardate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in character format
!
! Module calls - convert_date
!
!************************************************************************
!
!*Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardate2
LOGICAL :: nolater_char

!
! Name            Type    Purpose
!------------------------------------------------------------------------------
!*chardate1*      CHAR    First date/time
!*chardate2*      CHAR    Second date/time
!*nolater_char*   LOGICAL .TRUE. if chardate1 is not later than chardate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l
INTEGER, DIMENSION(7) :: intdate1, intdate2


intdate1 = convert_date(chardate1)
intdate2 = convert_date(chardate2)

nolater_char = .TRUE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      nolater_char = MERGE(.TRUE.,.FALSE.,intdate1(l).LT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION nolater_char

!========================================================================

FUNCTION nolater_int(intdate1,intdate2)
!************************************************************************
!
! *nolater_int* Checks whether date 'intdate1' is not later than 'intdate2'
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in integer format
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdate2
LOGICAL :: nolater_int

!
! Name            Type    Purpose
!------------------------------------------------------------------------------
!*intdate1*       INTEGER First date/time
!*intdate2*       INTEGER Second date/time
!*nolater_int*    LOGICAL .TRUE. if intdate1 is not later than intdate2
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: l


nolater_int = .TRUE.
l_110: DO l=1,7
   IF (intdate1(l).NE.intdate2(l)) THEN
      nolater_int = MERGE(.TRUE.,.FALSE.,intdate1(l).LT.intdate2(l))
      EXIT l_110
   ENDIF
ENDDO l_110


RETURN

END FUNCTION nolater_int

!========================================================================

SUBROUTINE num_time_steps_char(chardate1,chardate2,dsecs,numsteps)
!************************************************************************
!
! *num_time_steps_char* Return the number of time steps between two dates
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in character format
!
! Module calls - convert_date, num_time_steps_int
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(IN) :: chardate1, chardate2
INTEGER, INTENT(OUT) :: numsteps
REAL, INTENT(IN) :: dsecs

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*chardate1* CHAR    First date/time
!*chardate2* CHAR    Second date/time
!*dsecs*     REAL    Number of secs in one time step                        [s]
!*numsteps*  INTEGER Number of time steps
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER, DIMENSION(7) :: intdate1, intdate2


procname(pglev+1) = 'num_time_steps_char'
CALL log_timer_in()

!
!1. Convert dates to integer format
!----------------------------------
!

intdate1 = convert_date(chardate1)
intdate2 = convert_date(chardate2)
print *, "Start time: ", chardate1
print *, "End time: ", chardate2
!
!2. Number of time steps
!-----------------------
!

CALL num_time_steps_int(intdate1,intdate2,dsecs,numsteps)

CALL log_timer_out()


RETURN

END SUBROUTINE num_time_steps_char

!========================================================================

SUBROUTINE num_time_steps_int(intdate1,intdate2,dsecs,numsteps)
!************************************************************************
!
! *num_time_steps_int* Return the number of time steps between two dates
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description - input date/times in integer format
!
! Module calls - day_number, diff_dates, .earlier., .later., leap_year
!
!************************************************************************
!
!* Arguments
!
INTEGER, INTENT(OUT) :: numsteps
REAL, INTENT(IN) :: dsecs
INTEGER, INTENT(IN), DIMENSION(7) :: intdate1, intdate2

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*intdate1*  INTEGER First date/time
!*intdate2*  INTEGER Second date/time
!*dsecs*     REAL    Number of secs in one time step                        [s]
!*numsteps*  INTEGER Number of time steps
!
!------------------------------------------------------------------------------
!
!* Local variables
!
INTEGER :: isign, iyear, iyear1, iyear2, msecs, ndays1, ndays2,&
         & nodays, nomsecs, nosecs
INTEGER (KIND=kndilong) :: numsecs
REAL :: daynum1, daynum2
INTEGER, DIMENSION(7) :: idate1, idate2


procname(pglev+1) = 'num_time_steps_int'
CALL log_timer_in()


!
!1. Determine which date comes first
!-----------------------------------
!

IF (intdate1.earlier.intdate2) THEN
   idate1 = intdate1; idate2 = intdate2; isign = 1
ELSEIF (intdate1.later.intdate2) THEN
   idate1 = intdate2; idate2 = intdate1; isign = -1
ELSE
   numsteps = 0
   GOTO 1000
ENDIF

!
!2. Number of time steps
!-----------------------
!
nosecs = dsecs
msecs = MERGE(INT((dsecs-nosecs)*1000),0,dsecs.LT.1000.0)

!---without milliseconds
IF (msecs.EQ.0.AND.idate1(7).EQ.0.AND.idate2(7).EQ.0) THEN
   CALL diff_dates(idate1,idate2,0,numsecs)
   !print *, idate1
   !print *, idate2
   !print *, numsecs
   !print *, isign
   !print *, nosecs
   numsteps = isign*numsecs/nosecs
!---with milliseconds
ELSE
!  --number of days between start and end dates
   iyear1 = idate1(1)
   iyear2 = idate2(1)
   CALL day_number(idate1,daynum1)
   CALL day_number(idate2,daynum2)
   ndays1 = daynum1; ndays2 = daynum2
   IF (iyear1.EQ.iyear2) THEN
      nodays = ndays2 - ndays1
   ELSE
      nodays = 365 + leap_year(iyear1) - ndays1
      iyear_110:DO iyear=iyear1+1,iyear2-1
         nodays = nodays + 365 + leap_year(iyear)
      ENDDO iyear_110
      nodays = nodays + ndays2
   ENDIF
!  --number of time steps
   nomsecs = 1000*nosecs+msecs
   numsteps = isign*(1000_kndilong*(86400_kndilong*nodays+&
                   & 3600*(idate2(4)-idate1(4))+&
                   & 60*(idate2(5)-idate1(5))+idate2(6)-idate1(6))+&
                   & idate2(7)-idate1(7))/nomsecs
ENDIF
!VD
numsteps = ABS(numsteps)


1000 CALL log_timer_out()


RETURN

END SUBROUTINE num_time_steps_int

!========================================================================

FUNCTION read_clock()
!************************************************************************
!
! *read_clock* Read clock count from process clock
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
!************************************************************************
!
!* Arguments
!
INTEGER :: read_clock

!
!* Local variables
!
INTEGER :: n


CALL SYSTEM_CLOCK(COUNT=n)
read_clock = n


RETURN

END FUNCTION read_clock

!========================================================================

SUBROUTINE suspend_proc(nosecswait)
!************************************************************************
!
! *suspend_proc* Suspend program execution on calling process by a number of
!                seconds
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
! Module calls - diff_clock, read_clock
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(IN) :: nosecswait

!
! Name        Type    Purpose
!------------------------------------------------------------------------------
!*nosecswait* INTEGER Number of seconds to suspend program execution
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: maxcounts, npcc, numcounts


IF (nosecswait.EQ.0) RETURN
npcc = read_clock()
numcounts = 0; maxcounts = nosecswait*npcc_rate
DO WHILE (numcounts.LE.maxcounts)
   numcounts = diff_clock(npcc)
ENDDO

nopcc(itm_wait) = nopcc(itm_wait) + maxcounts


RETURN

END SUBROUTINE suspend_proc

!========================================================================

!========================================================================

SUBROUTINE year_to_date_char(yearnum,chardate)
!************************************************************************
!
! *year_to_date_char* Convert date in absolute years to date/time in character
!                     format
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls - add_secs_to_date, convert_date, leap_year
!
!************************************************************************
!
!* Arguments
!
CHARACTER (LEN=lentime), INTENT(OUT) :: chardate
REAL, INTENT(IN) :: yearnum

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*yearnum*   REAL    Time in years
!*chardate*  CHAR    Date/time
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ileap, nosecs
INTEGER, DIMENSION(7) :: intdate1, intdate2


intdate1(1) = INT(yearnum); intdate1(2:7) = 0
ileap = leap_year(intdate1(1))
nosecs = (365+ileap)*86400*(yearnum-INT(yearnum))
CALL add_secs_to_date(intdate1,intdate2,nosecs,1.0)
chardate = convert_date(intdate2)


RETURN

END SUBROUTINE year_to_date_char

!========================================================================

SUBROUTINE year_to_date_int(yearnum,intdate)
!************************************************************************
!
! *year_to_date_int* Convert date in absolute years to date/time in integer
!                    format
!
! Author - Patrick Luyten
!
! Last update - 4 Nov 2008  @(COHERENS)time_routines.f90  V2
!
! Description -
!
! Module calls - add_secs_to_date, leap_year
!
!************************************************************************
!
!* Arguments
!
REAL, INTENT(IN) :: yearnum
INTEGER, INTENT(OUT), DIMENSION(7) :: intdate

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*yearnum*   REAL    Time in years
!*intdate*   INTEGER Date/time
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ileap, nosecs
INTEGER, DIMENSION(7) :: intdate1


intdate1(1) = INT(yearnum); intdate1(2:7) = 0
ileap = leap_year(intdate1(1))
nosecs = (365+ileap)*86400*(yearnum-INT(yearnum))
CALL add_secs_to_date(intdate1,intdate,nosecs,1.0)


RETURN

END SUBROUTINE year_to_date_int

END MODULE time_routines
