MODULE rng_library
!************************************************************************
!
! *rng_library* Random generator library
!
! Author - Patrick Luyten
!
! Version - @(COHERENS)rng_library.f90  V2.0
!
! $Date: 2011-03-18 14:35:25 +0100 (Fri, 18 Mar 2011) $
!
! $Revision: 188 $
!
! Description -
!
! Reference - Press W.H., Flannery B.P., Teukolsky S.A. and Vetterling W.T.,
!             1992. Numerical Recipes. The art of scientific computing.
!             Cambridge University Press, Cambridge, 702 pp.
!           - L'Ecuyer P. and Cote S., 1991. Implementing a random number
!             package with splitting facilities. ACM Transactions on
!             Mathematical Software, 17: 98-111.
!
! Subroutines - rng_close, rng_finalize, rng_init, rng_multmod_decompos,
!               rng_normal_arr, rng_normal_var, rng_open, rng_opened,
!               rng_reset, rng_standard_normal, rng_standard_uniform,
!               rng_uniform_arr, rng_uniform_var
!
!************************************************************************
!
USE iopars
USE parampars
USE syspars
USE time_routines, ONLY: log_timer_in, log_timer_out

IMPLICIT NONE

!---parameters for random number generation
LOGICAL :: rng_initialised = .FALSE.
LOGICAL, DIMENSION(0:MaxGenerators) :: generator_set = .FALSE.
INTEGER, PARAMETER :: iec_v = 14, iec_w = 40
INTEGER, PARAMETER :: iec_m1 = 2147483563, iec_m2 = 2147483399
INTEGER, PARAMETER :: iec_a1 = 40014, iec_a2 = 40692
INTEGER, PARAMETER :: iec_q1 = 53668, iec_q2 = 52774
INTEGER, PARAMETER :: iec_r1 = 12211, iec_r2 = 3791
INTEGER, PARAMETER :: default_seed1 = 1234567890, default_seed2 = 123456789
INTEGER :: iec_a1w, iec_a2w, iec_a1vw, iec_a2vw, iranseed1, iranseed2
INTEGER, DIMENSION(MaxGenerators) :: igstate_curr1, igstate_curr2, &
                   & igstate_init1, igstate_init2, igstate_last1, igstate_last2
REAL, PARAMETER :: m1geninv = 1.0/iec_m1

INTEGER :: main_numgen ! to have only one number generator

INTEGER :: iopt_seed = 0

SAVE

CONTAINS

!========================================================================

SUBROUTINE rng_close(numgen)
!************************************************************************
!
! *rng_close* Disable ("close") a random generator
!
! Author - Patrick Luyten
!
! Description -
!
! Module calls - error_abort
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!*Arguments
!
INTEGER, INTENT(IN), OPTIONAL :: numgen

!
! Name     Type    Purpose
!------------------------------------------------------------------------------
!*numgen*  INTEGER Index of generator
!
!------------------------------------------------------------------------------
!
!* Local variables
!
CHARACTER (LEN=12) :: cgen


procname(pglev+1) = 'rng_close'
WRITE (cgen,'(I12)') numgen; cgen = ADJUSTL(cgen)
CALL log_timer_in(logname=TRIM(procname(pglev+1))//': '//cgen)

!
!1. Check generator
!------------------
!

IF (.NOT.generator_set(numgen)) THEN
   nerrs = 1
   IF (errchk) THEN
      WRITE (ioerr,'(A)') 'Attempt to close undefined generator: '//TRIM(cgen)
   ENDIF
   CALL error_abort('rng_close',ierrno_runval)
ENDIF

!
!2. Close
!--------
!

generator_set(numgen) = .FALSE.
igstate_init1(numgen) = int_fill
igstate_init2(numgen) = int_fill
igstate_last1(numgen) = int_fill
igstate_last2(numgen) = int_fill
igstate_curr1(numgen) = int_fill
igstate_curr2(numgen) = int_fill

CALL log_timer_out()


RETURN

END SUBROUTINE rng_close

!========================================================================

SUBROUTINE rng_finalize
!************************************************************************
!
! *rng_finalize* Finalize all random generators
!
! Author - Patrick Luyten
!
! Description - set random seeds for all generators
!
! Module calls - rng_close
!
!************************************************************************
!
!*Local variables
!
INTEGER :: igen


procname(pglev+1) = 'rng_finalize'
CALL log_timer_in()

!
!1. Close all generators
!-----------------------
!

igen_110: DO igen=1,MaxGenerators
   IF (generator_set(igen)) CALL rng_close(igen)
ENDDO igen_110

!
!2. Reset parameters
!-------------------
!

iranseed1 = oserit_param%seed1
iranseed2 = oserit_param%seed2
iec_a1w = iec_a1; iec_a2w = iec_a2
iec_a1vw = iec_a1; iec_a2vw = iec_a2
rng_initialised = .FALSE.

CALL log_timer_out()


RETURN

END SUBROUTINE rng_finalize

!========================================================================

SUBROUTINE rng_init
!************************************************************************
!
! *rng_init* Initialise all random generators
!
! Author - Patrick Luyten
!
! Description - set random seeds for all generators
!
! Module calls - rng_multmod_decompos
!
!************************************************************************
!
!*Local variables
!
CHARACTER (LEN=10) :: ctime
CHARACTER (LEN=86), PARAMETER :: table = 'abcdefghijklmnopqrstuvwxyz' &
          // 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' // '0123456789' // &
          '!@#$%^&*()_+[];:''"<>?,./'
INTEGER, PARAMETER :: twop30 = 1073741824
INTEGER :: ichr, l, n
INTEGER, PARAMETER, DIMENSION(5) :: ishift = (/1,64,4096,262144,16777216/)
INTEGER, DIMENSION(5)  :: ivals


procname(pglev+1) = 'rng_init'
CALL log_timer_in()

!
!1. Default seeds
!----------------
!

iranseed1 = oserit_param%seed1
iranseed2 = oserit_param%seed2

!
!2. Initialise seed from current daytime
!---------------------------------------
!
IF (oserit_param%iopt_set_seed == 0) THEN
   CALL DATE_AND_TIME(TIME=ctime)
   l_210: DO l=1,10
      ichr = MOD(INDEX(table,ctime(l:l)),64)
      IF (ichr.EQ.0) ichr = 63
      n_211: DO n=1,5
         ivals(n) = ichr - n
         IF (ivals(n).GE.1) CYCLE n_211
         ivals(n) = ivals(n) + 63
      END DO n_211
      n_212: DO n=1,5
         iranseed1 = MOD(iranseed1+ishift(n)*ivals(n),twop30)
         iranseed2 = MOD(iranseed2+ishift(n)*ivals(6-n),twop30)
      END DO n_212
   ENDDO l_210
ENDIF
!
!3. Initialise parameters
!------------------------
!

iec_a1w = iec_a1; iec_a2w = iec_a2
n_310: DO n=1,iec_w
   iec_a1w = rng_multmod_decompos(iec_a1w,iec_a1w,iec_m1)
   iec_a2w = rng_multmod_decompos(iec_a2w,iec_a2w,iec_m2)
ENDDO n_310

iec_a1vw = iec_a1; iec_a2vw = iec_a2
n_320: DO n=1,iec_v+iec_w
   iec_a1vw = rng_multmod_decompos(iec_a1vw,iec_a1vw,iec_m1)
   iec_a2vw = rng_multmod_decompos(iec_a2vw,iec_a2vw,iec_m2)
ENDDO n_320

rng_initialised = .TRUE.

CALL rng_open(main_numgen)

CALL log_timer_out()


RETURN

END SUBROUTINE rng_init

!========================================================================

FUNCTION rng_multmod_decompos(a,s,m)
!************************************************************************
!
! *rng_multmod_decompos* Returns (a*s)MOD(m)
!
! Author -
!
! Description - uses decomposition method
!
! Reference - L'Ecuyer and Cote (1991)
!
! Module calls -
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(IN) :: a, m, s
INTEGER :: rng_multmod_decompos

!
! Name                  Type    Purpose
!------------------------------------------------------------------------------
!*a*                    INTEGER Multiplier MLGC generator
!*s*                    INTEGER Input state of generator
!*m*                    INTEGER Modulus of MLGC generator
!*rng_multmod_decompos* INTEGER Result value
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER, PARAMETER :: h = 32768
INTEGER :: a0, a1, k, p, q, qh, rh


IF (a.LT.h) THEN
   a0 = a
   p = 0
ELSE
   a1 = a/h
   a0 = a - h*a1
   qh = m/h
   rh = m - h*qh
   IF (a1.GE.h) THEN
      a1 = a1 - h
      k = s/qh
      p = h*(s-k*qh) - k*rh
      DO WHILE (p.LT.0)
         p = p + m
      ENDDO
   ELSE
      p = 0
   ENDIF

!  ---p = (a2*s*h)MOD m
   IF (a1.NE.0) THEN
      q = m/a1
      k = s/q
      p = p - k*(m-a1*q)
      IF (p.GT.0) p = p - m
      p = p + a1*(s-k*q)
      DO WHILE (p.LT.0)
         p = p + m
      ENDDO
   ENDIF
   k = p/qh

!  ---p = ((a2*h + a1)*s)MOD m
   p = h*(p-k*qh) - k*rh
   DO WHILE (p.LT.0)
      p = p + m
   ENDDO

ENDIF

IF (a0.NE.0) THEN

!  ---p = ((a2*h + a1)*h*s)MOD m
   q = m/a0
   k = s/q
   p = p - k*(m-a0*q)
   IF (p.GT.0) p = p - m
   p = p + a0*(s-k*q)
   DO WHILE (p.LT.0)
      p = p + m
   ENDDO

ENDIF

rng_multmod_decompos = p


RETURN

END FUNCTION rng_multmod_decompos

!========================================================================

SUBROUTINE rng_normal_arr(xrand,ncount,numgen,xmean,xstd,limit)
!************************************************************************
!
! *rng_normal_arr* Generate a vector of random numbers with mean 'xmean' and
!                  standard deviation 'xstd'
!
! Author - Patrick Luyten and Isabel Andreu-Burillo
!
! Description -
!
! Module calls - error_abort, rng_standard_normal
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!*Arguments
!
INTEGER, INTENT(IN) :: ncount, numgen
REAL, INTENT(IN) :: xmean, xstd
REAL, INTENT(OUT), DIMENSION(ncount) :: xrand
REAL, INTENT(IN), OPTIONAL :: limit

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*xrand*    REAL    Vector of random numbers
!*ncount*   INTEGER Size of random vector
!*numgen*   INTEGER Index of generator
!*xmean*    REAL    Mean value
!*xstd*     REAL    Standard deviation
!*limit*    REAL    Maximum deviation from mean value
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=12) :: cgen
INTEGER :: n, ii


procname(pglev+1) = 'rng_normal_arr'
CALL log_timer_in()

!
!1. Check generator
!------------------
!

IF (.NOT.generator_set(numgen)) THEN
   nerrs = 1
   WRITE (cgen,'(I12)') numgen; cgen = ADJUSTL(cgen)
   IF (errchk) THEN
      WRITE (ioerr,'(A)') 'Attempt to use undefined generator: '//TRIM(cgen)
   ENDIF
   CALL error_abort('rng_normal_arr',ierrno_runval)
ENDIF

!
!2. Generate random numbers with zero mean and unit variance
!-----------------------------------------------------------
!

n_210: DO n=1,ncount
   CALL rng_standard_normal(xrand(n),numgen)
ENDDO n_210

!
!3. Check wether the random number remains within the limit
!----------------------------------------------------------
!

IF (PRESENT(limit)) THEN
   DO n=1,ncount
      ii = 0
      DO WHILE (ABS(xrand(n)).GT.ABS(limit).AND.ii.LT.8)
         CALL rng_standard_normal(xrand(n),numgen)
         ii = ii + 1
      ENDDO
      IF (ii.GE.8) THEN
         WRITE (iowarn,'(A)') 'WARNING: in rng_normal_arr, limit value &
            &  might be too low'
      ENDIF
   ENDDO
ENDIF


!
!4. Apply mean and standard deviation
!------------------------------------
!

IF (xstd.NE.1.0) xrand = xstd*xrand
IF (xmean.NE.0.0) xrand = xrand + xmean

CALL log_timer_out()


RETURN

END SUBROUTINE rng_normal_arr

!========================================================================

SUBROUTINE rng_normal_var(xrand,numgen,xmean,xstd,limit)
!************************************************************************
!
! *rng_normal_var* Generate a random number with mean 'xmean' and standard
!                  deviation 'xstd'
!
! Author - Patrick Luyten and Isabel Andreu-Burillo
!
! Description -
!
! Module calls - error_abort, rng_standard_normal
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!*Arguments
!
INTEGER, INTENT(IN) :: numgen
REAL, INTENT(OUT) :: xrand
REAL, INTENT(IN) :: xmean, xstd
REAL, INTENT(IN), OPTIONAL :: limit

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*xrand*    REAL    Random number
!*numgen*   INTEGER Index of generator
!*xmean*    REAL    Mean value
!*xstd*     REAL    Standard deviation
!*limit*    REAL    Maximum deviation from mean value
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=12) :: cgen
INTEGER :: ii

!
!1. Check generator
!------------------
!

IF (.NOT.generator_set(numgen)) THEN
   nerrs = 1
   WRITE (cgen,'(I12)') numgen; cgen = ADJUSTL(cgen)
   IF (errchk) THEN
      WRITE (ioerr,'(A)') 'Attempt to use undefined generator: '//TRIM(cgen)
   ENDIF
   CALL error_abort('rng_normal_var',ierrno_runval)
ENDIF

!
!2. Generate random numbers with zero mean and unit variance
!-----------------------------------------------------------
!

CALL rng_standard_normal(xrand,numgen)

!
!3. Check wether the random number remains within the limit
!----------------------------------------------------------
!

IF (PRESENT(limit)) THEN
   ii = 0
   DO WHILE (ABS(xrand).GT.ABS(limit).AND.ii.LT.8)
      CALL rng_standard_normal(xrand,numgen)
      ii = ii + 1
   ENDDO
   IF (ii.GE.8) THEN
       WRITE (iowarn,'(A)') 'WARNING: in rng_normal_var, limit value &
            &  might be too low'
   ENDIF
ENDIF

!
!4. Apply mean and standard deviation
!------------------------------------
!

IF (xstd.NE.1.0) xrand = xstd*xrand
IF (xmean.NE.0.0) xrand = xrand + xmean


RETURN

END SUBROUTINE rng_normal_var

!========================================================================

SUBROUTINE rng_open(numgen)
!************************************************************************
!
! *rng_open* Initialise "open" a new random generator
!
! Author - Patrick Luyten
!
! Description - returns next available random generator
!
! Module calls - error_abort, error_ubound_var, rng_multmod_decompos
!
!************************************************************************
!
USE error_routines, ONLY: error_abort, error_ubound_var

!
!*Arguments
!
INTEGER, INTENT(OUT) :: numgen

!
! Name     Type    Purpose
!------------------------------------------------------------------------------
!*numgen*  INTEGER Index of new generator
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=12) :: cgen
INTEGER :: igen, istate1, istate2


!
!1. Next available generator number
!-----------------------------------
!

numgen = 1
DO WHILE(generator_set(numgen).AND.(numgen.LE.MaxGenerators))
   numgen = numgen + 1
ENDDO
CALL error_ubound_var(numgen,'numgen',MaxGenerators,.TRUE.)
!CALL error_abort('open_generator',ierrno_runval)

procname(pglev+1) = 'rng_open'
WRITE (cgen,'(I12)') numgen
CALL log_timer_in(logname=TRIM(procname(pglev+1))//': '//cgen)

!
!2. Initialise generator
!------------------------
!

istate1 = iranseed1
istate2 = iranseed2
igen_110: DO igen=2,numgen
   istate1 = rng_multmod_decompos(iec_a1vw,istate1,iec_m1)
   istate2 = rng_multmod_decompos(iec_a2vw,istate1,iec_m2)
ENDDO igen_110

igstate_init1(numgen) = istate1
igstate_init2(numgen) = istate2
igstate_last1(numgen) = istate1
igstate_last2(numgen) = istate2
igstate_curr1(numgen) = istate1
igstate_curr2(numgen) = istate2

generator_set(numgen) = .TRUE.

CALL log_timer_out()


RETURN

END SUBROUTINE rng_open

!========================================================================

SUBROUTINE rng_opened(numgen,flag)
!************************************************************************
!
! *rng_opened* Returns flag = .TRUE. if generator with index 'numgen' has been
!              opened
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*Arguments
!
LOGICAL, INTENT(OUT) :: flag
INTEGER, INTENT(IN) :: numgen

!
! Name     Type    Purpose
!------------------------------------------------------------------------------
!*numgen*  INTEGER Index of new generator
!*flag*    LOGICAL .TRUE. if generator numgen is open
!
!------------------------------------------------------------------------------
!


flag = generator_set(numgen)


RETURN

END SUBROUTINE rng_opened

!========================================================================

SUBROUTINE rng_reset(numgen,seedtype)
!************************************************************************
!
! *rng_reset* Reset generator according to 'seedtype'
!
! Author -
!
! Description -
!
! Reference - L'Ecuyer and Cote (1991)
!
! Module calls - error_abort, error_vals_var, rng_multmod_decompos
!
!************************************************************************
!
USE error_routines, ONLY: error_abort, error_vals_var

!
!*Arguments
!
CHARACTER (LEN=1), INTENT(IN) :: seedtype
INTEGER, INTENT (IN) :: numgen

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*numgen*   INTEGER Index of generator
!*seedtype* CHAR    Type of reset if numgen > 0 ('I', 'L', 'N')
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=12) :: cgen


procname(pglev+1) = 'rng_reset'
CALL log_timer_in()

!
!1. Check arguments
!------------------
!

IF (.NOT.generator_set(numgen)) THEN
   nerrs = 1
   WRITE (cgen,'(I12)') numgen; cgen = ADJUSTL(cgen)
   IF (errchk) THEN
      WRITE (ioerr,'(A)') 'Attempt to reset undefined generator: '//TRIM(cgen)
   ENDIF
   CALL error_abort('rng_reset',ierrno_runval)
ENDIF

!
!2. Reset generator
!------------------
!

SELECT CASE (seedtype)
CASE ('I')
   igstate_last1(numgen) = igstate_init1(numgen)
   igstate_last2(numgen) = igstate_init2(numgen)
CASE ('L')
   CONTINUE
CASE ('N')
   igstate_last1(numgen) = rng_multmod_decompos(iec_a1w,igstate_last1(numgen),&
                                              & iec_m1)
   igstate_last2(numgen) = rng_multmod_decompos(iec_a2w,igstate_last2(numgen),&
                                              & iec_m2)
CASE DEFAULT
   CALL error_vals_var(seedtype,'seedtype','"I" "L" "N"')
   CALL error_abort('rng_reset',ierrno_runval)
END SELECT

igstate_curr1(numgen) = igstate_last1(numgen)
igstate_curr2(numgen) = igstate_last2(numgen)

CALL log_timer_out()


RETURN

END SUBROUTINE rng_reset

!========================================================================

SUBROUTINE rng_standard_normal(xran,numgen)
!************************************************************************
!
! *rng_standard_normal* Generate a random number with zero mean and unit
!                       variance
!
! Author -
!
! Description -
!
! Reference - Numerical Recipes (routine "gasdev")
!
! Module calls - rng_uniform_var
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(IN) :: numgen
REAL, INTENT(OUT) :: xran

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*xran*     REAL    Random number on output
!*numgen*   INTEGER Index of generator
!
!------------------------------------------------------------------------------
!
!*Local variables
!
LOGICAL :: set_vals = .TRUE.
REAL, SAVE :: vsave
REAL :: fac, rsq, u1, u2


IF (set_vals) THEN
   rsq = 0.0
   DO WHILE (rsq.EQ.0.0.OR.rsq.GE.1.0)
      CALL rng_uniform_var(u1,numgen,0.0,1.0)
      u1 = 2.0*u1 - 1.0
      CALL rng_uniform_var(u2,numgen,0.0,1.0)
      u2 = 2.0*u2 - 1.0
      rsq = u1*u1 + u2*u2
   ENDDO
   fac = SQRT(-2.0*LOG(rsq)/rsq)
   vsave = fac*u1
   xran = fac*u2
   set_vals = .FALSE.
ELSE
   xran = vsave
   set_vals = .TRUE.
ENDIF


RETURN

END SUBROUTINE rng_standard_normal

!========================================================================

SUBROUTINE rng_standard_uniform(xran,numgen)
!************************************************************************
!
! *rng_standard_uniform* Returns a random number between 0 and 1
!
! Author -
!
! Description -
!
! Reference - L'Ecuyer and Cote (1991)
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(IN) :: numgen
REAL, INTENT(OUT) :: xran

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*xran*     REAL    Random number on output
!*numgen*   INTEGER Index of generator
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: is1, is2, iz, k


is1 = igstate_curr1(numgen)
is2 = igstate_curr2(numgen)
k = is1/iec_q1
is1 = iec_a1*(is1-k*iec_q1) - k*iec_r1
IF (is1.LT.0) is1 = is1 + iec_m1
k = is2/iec_q2
is2 = iec_a2*(is2-k*iec_q2) - k*iec_r2
IF (is2.LT.0) is2 = is2 + iec_m2
igstate_curr1(numgen) = is1
igstate_curr2(numgen) = is2
iz = is1 - is2
IF (iz.LT.1) iz = iz + iec_m1 - 1
xran = iz*m1geninv


RETURN

END SUBROUTINE rng_standard_uniform

!========================================================================

SUBROUTINE rng_uniform_arr(xrand,nosize,numgen,xlo,xhi)
!************************************************************************
!
! *rng_uniform_arr* Generate a vector of random numbers between 'xlo' and 'xhi'
!
! Author - Patrick Luyten and Isabel Andreu-Burillo
!
! Description -
!
! Module calls - error_abort, rng_standard_uniform
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!*Arguments
!
INTEGER, INTENT(IN) :: numgen, nosize
REAL, INTENT(IN) :: xhi, xlo
REAL, INTENT(OUT), DIMENSION(nosize) :: xrand

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*xrand*    REAL    Vector of random numbers
!*nosize*   INTEGER Size of random vector
!*numgen*   INTEGER Index of generator
!*xlo*      REAL    Lower value of random number
!*xhi*      REAL    Upper value of random number
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=12) :: cgen
INTEGER :: n


procname(pglev+1) = 'rng_uniform_arr'
CALL log_timer_in()

!
!1. Check generator
!------------------
!

IF (.NOT.generator_set(numgen)) THEN
   nerrs = 1
   WRITE (cgen,'(I12)') numgen; cgen = ADJUSTL(cgen)
   IF (errchk) THEN
      WRITE (ioerr,'(A)') 'Attempt to use undefined generator: '//TRIM(cgen)
   ENDIF
   CALL error_abort('rng_uniform_arr',ierrno_runval)
ENDIF

!
!2. Generate random numbers between 0 and 1
!------------------------------------------
!

n_210: DO n=1,nosize
   CALL rng_standard_uniform(xrand(n),numgen)
ENDDO n_210

!
!3. Apply limits
!---------------
!

IF ((xhi-xlo).NE.1.0) xrand = (xhi-xlo)*xrand
IF (xlo.NE.0.0) xrand = xrand + xlo

CALL log_timer_out()


RETURN

END SUBROUTINE rng_uniform_arr

!========================================================================

SUBROUTINE rng_uniform_var(xrand,numgen,xlo,xhi)
!************************************************************************
!
! *rng_uniform_var* Generate a random number between 'xlo' and 'xhi'
!
! Author - Patrick Luyten and Isabel Andreu-Burillo
!
! Description -
!
! Module calls - error_abort, rng_standard_uniform
!
!************************************************************************
!
USE error_routines, ONLY: error_abort

!
!*Arguments
!
INTEGER, INTENT(IN) :: numgen
REAL, INTENT(OUT) :: xrand
REAL, INTENT(IN) :: xhi, xlo

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*xrand*    REAL    Random number
!*numgen*   INTEGER Index of generator
!*xlo*      REAL    Lower value of random number
!*xhi*      REAL    Upper value of random number
!
!------------------------------------------------------------------------------
!
!*Local variables
!
CHARACTER (LEN=12) :: cgen

!
!1. Check generator
!------------------
!

IF (.NOT.generator_set(numgen)) THEN
   nerrs = 1
   WRITE (cgen,'(I12)') numgen; cgen = ADJUSTL(cgen)
   IF (errchk) THEN
      WRITE (ioerr,'(A)') 'Attempt to use undefined generator: '//TRIM(cgen)
   ENDIF
   CALL error_abort('rng_number_var',ierrno_runval)
ENDIF

!
!2. Generate random numbers between 0 and 1
!------------------------------------------
!

CALL rng_standard_uniform(xrand,numgen)

!
!3. Apply limits
!---------------
!

xrand = xlo + (xhi-xlo)*xrand


RETURN

END SUBROUTINE rng_uniform_var


END MODULE rng_library
