MODULE utility_routines
!************************************************************************
!
! *utility_routines* General purpose routines
!
! Author - Patrick Luyten
!
! Version - @(COHERENS)utility_routines.f90  V2.0
!
! $Date: 2011-03-18 14:35:25 +0100 (Fri, 18 Mar 2011) $
!
! $Revision: 188 $
!
! Description -
!
! Generic routines - swap_data
!
! Routines - cfl_orlan, diff_vals, digit_number_int, digit_number_longint,
!            index_position, least_squares_fit, lim_dims, loop_index,
!            mult_index, num_halo, outer_product, prime_factoring, qsort_index,
!            relax_factor, string_replace, tvd_limiter, two_power, upper_case
!
! Internal calls - icomp_xchg
!
!************************************************************************
!

IMPLICIT NONE

INTERFACE swap_data
   MODULE PROCEDURE swap_data_var_int, swap_data_var_real,&
                 &  swap_data_var_cmplx, swap_data_1d_real,&
                 &  swap_data_1d_cmplx, swap_data_2d_real,&
                 &  swap_data_2d_cmplx
END INTERFACE

CONTAINS

!========================================================================

FUNCTION cfl_orlan(x1,x2,x3)
!************************************************************************
!
! *cfl_orlan* Calculate normalised phase velocity for Orlanski radiation
!             condition
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*Arguments
!
REAL, INTENT(IN) :: x1, x2, x3
REAL :: cfl_orlan

!
! Name       Type Purpose
!-----------------------------------------------------------------------------
!*x1*        REAL Value of variable at first/last domain node and time n
!*x2*        REAL Value of variable at first/last domain node and time n-1
!*x3*        REAL Value of variable at second/last but one node and time n-1
!*cfl_orlan* REAL Normalised phase velocity
!
!------------------------------------------------------------------------------
!

IF (x2.EQ.x3) THEN
   cfl_orlan = MERGE(0.0,1.0,x1.LE.x2)
ELSE
   cfl_orlan = MIN(MAX((x1-x2)/(x3-x2),0.0),1.0)
ENDIF


RETURN

END FUNCTION cfl_orlan

!========================================================================

FUNCTION diff_vals(ilist)
!************************************************************************
!
! *diff_vals* Returns .TRUE. if all elements of the vector array 'ilist' are
!             different
!
! Author - Patrick Luyten
!
! Description - arguments are of KIND default
!
!************************************************************************
!
!*  Arguments
!
INTEGER, INTENT(IN), DIMENSION(:) :: ilist
LOGICAL :: diff_vals

!
! Name       Type    Purpose
!------------------------------------------------------------------------------
!*ilist*     INTEGER Input vector list
!*diff_vals* LOGICAL 
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: i, j, nsize


nsize = SIZE(ilist)
diff_vals = .TRUE.
i_110: DO i=1,nsize
j_110: DO j=1,nsize
   IF ((i.NE.j).AND.(ilist(i).EQ.ilist(j))) THEN
      diff_vals = .FALSE.
      EXIT i_110
   ENDIF
ENDDO j_110
ENDDO i_110


RETURN

END FUNCTION diff_vals

!========================================================================

FUNCTION digit_number_int(ival)
!************************************************************************
!
! *digit_number_int* Return the number of significant (decimal) digits within
!                    the integer 'ival'
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*  Arguments
!
INTEGER, INTENT(IN) :: ival
INTEGER :: digit_number_int

!
! Name              Type    Purpose
!------------------------------------------------------------------------------
!*ival*             INTEGER Integer variable
!*digit_number_int* INTEGER Number of digits 
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ix


ix = ival
digit_number_int = 1
DO WHILE (ix/10.GT.0)
   digit_number_int = digit_number_int + 1
   ix = ix/10
END DO


RETURN

END FUNCTION digit_number_int

!========================================================================

FUNCTION digit_number_longint(ival)
!************************************************************************
!
! *digit_number_longint* Return the number of significant (decimal) digits
!                        within the long integer 'ival'
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
USE syspars

!
!*  Arguments
!
INTEGER (KIND=kndilong), INTENT(IN) :: ival
INTEGER :: digit_number_longint

!
! Name                  Type    Purpose
!------------------------------------------------------------------------------
!*ival*                 LONGINT Integer variable
!*digit_number_longint* INTEGER Number of digits 
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER (KIND=kndilong):: ix


ix = ival
digit_number_longint = 1
DO WHILE (ix/10.GT.0)
   digit_number_longint = digit_number_longint + 1
   ix = ix/10
END DO


RETURN

END FUNCTION digit_number_longint

!========================================================================

FUNCTION index_position(ival,ilist)
!************************************************************************
!
! *index_position* Return the index of the first occurrence of 'ival' in the
!                  list 'ilist'
!
! Author - Patrick Luyten
!
! Description - result is 0 if inval is not included in ilist
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(IN) :: ival
INTEGER, INTENT(IN), DIMENSION(:) :: ilist
INTEGER :: index_position

!
! Name            Type    Purpose
!------------------------------------------------------------------------------
!*ival*           INTEGER Integer whose position in ilist is returned (0 if no
!                         matching)
!*ilist*          INTEGER Check list array
!*index_position* INTEGER Index
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: ix, n, nsize


nsize = SIZE(ilist)
ix = 0
n_110: DO n=1,nsize
   IF (ival.EQ.ilist(n)) THEN
      ix = n
      EXIT n_110
   ENDIF
ENDDO n_110
index_position = ix


RETURN

END FUNCTION index_position

!========================================================================

SUBROUTINE least_squares_fit(x,y,nodat,afit,bfit,corrcoef)
!************************************************************************
!
! *least_squares_fit* Linear regression
!
! Author - Patrick Luyten
!
! Description - 
!
!************************************************************************
!
USE iopars

!
!*  Arguments
!
INTEGER, INTENT(IN) :: nodat
REAL, INTENT(OUT) :: afit, bfit, corrcoef
REAL, INTENT(IN), DIMENSION(nodat) :: x, y

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*x*        REAL    X-coordinates of data points
!*y*        REAL    Y-coordinates of data points
!*nodat*    INTEGER Number of data points
!*afit*     REAL    Slope parameter of interpolated straight line
!*bfit*     REAL    Intersection of interpolated line with X-axis
!*corrcoef* REAL    Linear correlation coefficient
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: n
REAL :: chi2, rdat, sigvar, sumt2, sumx, sumy, sxoss, t


!---initialise values
sumt2 = 0.0; bfit = 0.0

!---sum of data values
sumx = SUM(x); sumy = SUM(y)

!---fitting parameters
rdat = REAL(nodat)
sxoss = sumx/rdat
n_110: DO n=1,nodat
   t = x(n) - sxoss
   sumt2 = sumt2 + t*t
   bfit = bfit + t*y(n)
ENDDO n_110
bfit = bfit/sumt2
afit = (sumy-sumx*bfit)/rdat

!---correlation coefficient
chi2 = 0.0; sigvar = 0.0
n_120: DO n=1,nodat
   chi2 = chi2 + (y(n)-afit-bfit*x(n))**2
   sigvar = sigvar + (y(n)-sumy/rdat)**2
ENDDO n_120
corrcoef = 1.0 - chi2/sigvar


RETURN

END SUBROUTINE least_squares_fit

!========================================================================

FUNCTION lim_dims(lims)
!************************************************************************
!
! *lim_dims* Number of iterations within the DO-loop defined by 'lims'
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*  Arguments
!
INTEGER, INTENT(IN), DIMENSION(3) :: lims
INTEGER :: lim_dims

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*lims*     INTEGER Start/end/increment values of loop
!*lim_dims* INTEGER Number of iterations
!
!------------------------------------------------------------------------------
!

IF (lims(3).NE.0) THEN
   lim_dims = (lims(2)-lims(1))/lims(3) + 1
ELSE
   lim_dims = 0
ENDIF


RETURN

END FUNCTION lim_dims

!========================================================================

FUNCTION loop_index(lims,iloop)
!************************************************************************
!
! *loop_index* Return .TRUE. if 'iloop' is within the range of the DO-loop
!              defined by 'lims'
!
! Author - Patrick Luyten
!
! Description - only ascending loops are considered
!             - the sign of lims(3) is irrelevant
!
!************************************************************************
!
!*  Arguments
!
INTEGER, INTENT(IN) :: iloop
INTEGER, INTENT(IN), DIMENSION(3) :: lims
LOGICAL :: loop_index

!
! Name        Type     Purpose
!------------------------------------------------------------------------------
!*lims*       INTEGER  Start/end/increment values of loop
!*iloop*      INTEGER  Loop index
!*loop_index* LOGICAL  Result value
!
!------------------------------------------------------------------------------
!

IF (lims(3).EQ.0) THEN
   loop_index = .FALSE.
ELSEIF ((((iloop-lims(1))/lims(3))*lims(3).EQ.(iloop-lims(1))).AND.&
        & (iloop.GE.lims(1)).AND.(iloop.LE.lims(2))) THEN
   loop_index = .TRUE.
ELSE
   loop_index = .FALSE.
ENDIF


RETURN

END FUNCTION loop_index

!========================================================================

FUNCTION mult_index(ix,iy)
!************************************************************************
!
! *mult_index* Return .TRUE. if 'iy' is non-zero and 'ix' an integer multiple
!              of 'iy'
!
! Author - Patrick Luyten
!
! Description - arguments are KIND default
!
!************************************************************************
!
!*  Arguments
!
INTEGER, INTENT(IN) :: ix, iy
LOGICAL :: mult_index

!
! Name        Type     Purpose
!------------------------------------------------------------------------------
!*ix*         INTEGER  Must be multiple of iy
!*iy*         INTEGER  Must be divisor of ix
!*mult_index* LOGICAL  Result value
!
!------------------------------------------------------------------------------
!

IF (iy.EQ.0) THEN
   mult_index = .FALSE.
ELSEIF ((ix/iy)*iy.EQ.ix) THEN
   mult_index = .TRUE.
ELSE
   mult_index = .FALSE.
ENDIF


RETURN

END FUNCTION mult_index

!========================================================================

FUNCTION num_halo(iopt_adv)
!************************************************************************
!
! *num_halo* Halo size needed for horizontal advection
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*Arguments
!

INTEGER, INTENT(IN) :: iopt_adv
INTEGER :: num_halo

!
! Name        Type     Purpose
!------------------------------------------------------------------------------
!*iopt_avd*   INTEGER  Advection switch
!*num_hal*    INTEGER  Halo size
!
!------------------------------------------------------------------------------
!


SELECT CASE(iopt_adv)
   CASE (0); num_halo = 0
   CASE (1:2); num_halo = 1
   CASE (3); num_halo = 2
END SELECT


RETURN

END FUNCTION num_halo

!=============================================================================

FUNCTION outer_product(a,b)
!*****************************************************************************
!
! *outer_product* Outer product of two vectors
!
! Author -
!
! Description - if a,b are vectors of size m, resp. n, returns the mxn
!               matrix A_ij = a_i*b_j
!
! Reference - Numerical Recipes (routine "outerprod")
!
! *****************************************************************************
!
!* Arguments
!
REAL, DIMENSION(:), INTENT(IN) :: a, b
REAL, DIMENSION(SIZE(a), SIZE(b)) :: outer_product

!
! Name           Type  Purpose
!------------------------------------------------------------------------------
!*a*             REAL  Vector 1
!*b*             REAL  Vector 2
!*outer_product* REAL  Outer product
!
!------------------------------------------------------------------------------
!


outer_product = SPREAD(a,DIM=2,NCOPIES=SIZE(b))*SPREAD(b,DIM=1,NCOPIES=SIZE(a))


RETURN

END FUNCTION outer_product

!========================================================================

SUBROUTINE prime_factoring(nx,nfacs,ifacs,maxfacs,maxprime)
!************************************************************************
!
! *prime_factoring* Factor 'nx' in its prime numbers
!
! Author - Patrick Luyten
!
! Description - prime factors (including multiple values and starting from
!               'maxprime') are stored in 'ifacs'
!             - number of prime factors (including multiple values) are stored
!               in 'nfacs'
!             - number of prime numbers is limited by 'maxfacs'
!
!************************************************************************
!
USE iopars

!
!*Arguments
!
INTEGER, INTENT(IN) :: maxfacs, maxprime, nx
INTEGER, INTENT(OUT) :: nfacs
INTEGER, INTENT(OUT), DIMENSION(maxfacs) :: ifacs

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*nx*       INTEGER Input integer variable
!*nfacs*    INTEGER Number of prime factors
!*ifacs*    INTEGER List of prime factors
!*maxfacs*  INTEGER Maximum number of prime factors
!*maxprime* INTEGER Largest allowed prime factor
!
!------------------------------------------------------------------------------
!
!*Local variables
!
LOGICAL :: flag
INTEGER :: l, m, nn


nn = nx; nfacs = 0
l_110: DO l=maxprime,2,-1
   flag = .TRUE.
   m_111: DO m=2,l-1
      IF (MOD(l,m).EQ.0) flag = .FALSE.
   ENDDO m_111
   IF (flag) THEN
      DO WHILE (MOD(nn,l).EQ.0.AND.nn.GT.1)
         nfacs = nfacs + 1
         ifacs(nfacs) = l
         nn = nn/l
         IF (nfacs.GE.maxfacs) EXIT l_110
      ENDDO
   ENDIF
ENDDO l_110


RETURN

END SUBROUTINE prime_factoring

!========================================================================

SUBROUTINE qsort_index(arr,indx,iorder)
!************************************************************************
!
! *qsort_index* Indexes the array 'arr' such that arr(indx(j)) is in
!               ascending (descending) order if 'iorder' = 1 (-1)
!
! Author - 
!
! Description - Uses "Quicksort" algorithm
!
! Reference - Numerical Recipes (routine "indexx")
!
! Internal calls - icomp_xchg
!
!************************************************************************
!
USE iopars

!
!*Arguments
!
INTEGER, INTENT(IN) :: iorder
REAL, INTENT(IN), DIMENSION(:) :: arr
INTEGER, INTENT(OUT), DIMENSION(SIZE(arr)) :: indx

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*arr*      REAL    Sort array
!*indx*     INTEGER Output index array
!*iorder*   INTEGER Sort order (ascending if = 1, descending if = -1)
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER, PARAMETER :: nn = 15, nstack = 50
INTEGER :: i, ir, indext, j, jstack, k, l, n
REAL :: a
INTEGER, DIMENSION(nstack) :: istack
REAL, DIMENSION(SIZE(arr)) :: x


!---initialise
n = SIZE(arr)
indx = (/(i,i=1,n)/)
jstack = 0
l = 1
ir = n
IF (iorder.EQ.1) THEN
   x = arr
ELSEIF (iorder.EQ.-1) THEN
   x = -arr
ENDIF

DO

!---insertion sort when subarray is small enough
   IF ((ir-l).LT.nn) THEN
      j_110: DO j=l+1,ir
         indext = indx(j)
         a = x(indext)
         i_111: DO i=j-1,l,-1
            IF (x(indx(i)).LE.a) EXIT i_111
            indx(i+1) = indx(i)
         ENDDO i_111
         indx(i+1) = indext
      ENDDO j_110
      IF (jstack.EQ.0) GOTO 1000
      ir = istack(jstack)
      l = istack(jstack-1)
      jstack = jstack - 2

!   ---partitioning
   ELSE
      k = (l+ir)/2
      CALL swap_data(indx(k),indx(l+1))
      CALL icomp_xchg(indx(l),indx(ir))
      CALL icomp_xchg(indx(l+1),indx(ir))
      CALL icomp_xchg(indx(l),indx(l+1))
      i = l + 1
      j = ir
      indext = indx(l+1)
      a = x(indext)
      DO
         DO
            i = i + 1
            IF (x(indx(i)).GE.a) EXIT
         ENDDO
         DO
            j = j - 1
            IF (x(indx(j)).LE.a) EXIT
         ENDDO
         IF (j.LT.i) EXIT
         CALL swap_data(indx(i),indx(j))
      ENDDO
      indx(l+1) = indx(j)
      indx(j) = indext
      jstack = jstack + 2
      IF ((ir-i+1).GE.(j-l)) THEN
         istack(jstack) = ir
         istack(jstack-1) = i
         ir = j - 1
      ELSE
         istack(jstack) = j - 1
         istack(jstack-1) = l
         l = i
      ENDIF
   ENDIF

ENDDO

1000 CONTINUE


RETURN

CONTAINS

!=============================================================================

SUBROUTINE icomp_xchg(i,j)
!*****************************************************************************
!
! *icomp_xchg*
!
! Author -
!
! Description -
!
! Reference - Numerical Recipes
!
!*****************************************************************************
!
!* Arguments
!
INTEGER, INTENT(INOUT) :: i, j

!
!*Local variables
!
INTEGER :: iswap


IF (x(j).LT.x(i)) THEN
   iswap = i
   i = j
   j = iswap
ENDIF


RETURN

END SUBROUTINE icomp_xchg

END SUBROUTINE qsort_index

!========================================================================

FUNCTION relax_factor(idist,ityp,width)
!************************************************************************
!
! *relax_factor* Return relaxation weigth factor
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(IN) :: idist, ityp
REAL, INTENT(IN) :: width
REAL :: relax_factor

!
! Name          Type    Purpose
!------------------------------------------------------------------------------
!*idist*        INTEGER Distance of grid point from open boundary
!*ityp*         INTEGER Type of weight function
!*width*        REAL    Width of relaxation zone
!*relax_factor* REAL    Weight factor
!
!------------------------------------------------------------------------------
!
!*Local variables
!
REAL :: weight


SELECT CASE (ityp)
   CASE (1); weight = 1.0-idist/width
   CASE (2); weight = (1.0-idist/width)**2
   CASE (3); weight = 1.0-TANH(0.5*idist)
END SELECT
relax_factor = weight


RETURN

END FUNCTION relax_factor

!========================================================================

SUBROUTINE string_replace(string,cin,cout)
!************************************************************************
!
! *string_replace* Replaces each occurence of character 'cin' by 'cout' within
!                  a string                  
!
! Author -
!
! Description - in case string consists of'cin' characters only, first
!               character is replaced by 'cout'
!
!************************************************************************
!
!*Arguments
!
CHARACTER (LEN=*), INTENT(INOUT) :: string
CHARACTER (LEN=1), INTENT(IN) :: cin, cout 

!
! Name          Type    Purpose
!------------------------------------------------------------------------------
!*string*       CHAR    Input string
!*cin*          CHAR    Search character
!*cout*         CHAR    Replacement character
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER l, lmax


lmax = LEN_TRIM(string)
IF (lmax.EQ.0) THEN
   string = cout
ELSE
   l_110: DO l=1,lmax
      IF (string(l:l).EQ.cin) string(l:l) = cout
   ENDDO l_110
ENDIF


RETURN

END SUBROUTINE string_replace

!========================================================================

SUBROUTINE swap_data_var_int(ix,iy)
!************************************************************************
!
! *swap_data_var_int* Exchange two integer variables
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
INTEGER, INTENT(INOUT) :: ix, iy

!
!*Local variables
!
INTEGER :: itmp


itmp = ix; ix = iy; iy = itmp


RETURN

END SUBROUTINE swap_data_var_int

!========================================================================

SUBROUTINE swap_data_var_real(x,y)
!************************************************************************
!
! *swap_data_var_real* Exchange two real variables
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
REAL, INTENT(INOUT) :: x, y

!
!*Local variables
!
REAL :: tmp


tmp = x; x = y; y = tmp


RETURN

END SUBROUTINE swap_data_var_real

!========================================================================

SUBROUTINE swap_data_var_cmplx(x,y)
!************************************************************************
!
! *swap_data_var_cmplx* Exchange two complex variables
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
COMPLEX, INTENT(INOUT) :: x,y

!
!*Local variables
!
COMPLEX :: tmp


tmp = x; x = y; y = tmp


RETURN

END SUBROUTINE swap_data_var_cmplx

!========================================================================

SUBROUTINE swap_data_1d_real(x,y)
!************************************************************************
!
! *swap_data_1d_real* Exchange two 1-D real arrays
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
REAL, INTENT(INOUT), DIMENSION(:) :: x,y

!
!*Local variables
!
REAL, DIMENSION(SIZE(x)) :: tmp


tmp = x; x = y; y = tmp


RETURN

END SUBROUTINE swap_data_1d_real

!========================================================================

SUBROUTINE swap_data_1d_cmplx(x,y)
!************************************************************************
!
! *swap_data_1d_cmplx* Exchange two 1-D complex arrays
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
COMPLEX, INTENT(INOUT), DIMENSION(:) :: x,y

!
!*Local variables
!
COMPLEX, DIMENSION(SIZE(x)) :: tmp


tmp = x; x = y; y = tmp


RETURN

END SUBROUTINE swap_data_1d_cmplx

!========================================================================

SUBROUTINE swap_data_2d_real(x,y)
!************************************************************************
!
! *swap_data_2d_real* Exchange two 2-D real arrays
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
REAL, INTENT(INOUT), DIMENSION(:,:) :: x,y

!
!*Local variables
!
REAL, DIMENSION(SIZE(x,1),SIZE(x,2)) :: tmp


tmp = x; x = y; y = tmp


RETURN

END SUBROUTINE swap_data_2d_real

!========================================================================

SUBROUTINE swap_data_2d_cmplx(x,y)
!************************************************************************
!
! *swap_data_2d_cmplx* Exchange two 2-D complex arrays
!
! Author -
!
! Description -
!
!************************************************************************
!
!*Arguments
!
COMPLEX, INTENT(INOUT), DIMENSION(:,:) :: x,y

!
!*Local variables
!
COMPLEX, DIMENSION(SIZE(x,1),SIZE(x,2)) :: tmp


tmp = x; x = y; y = tmp


RETURN

END SUBROUTINE swap_data_2d_cmplx

!========================================================================


!========================================================================

FUNCTION two_power(n)
!************************************************************************
!
! *two_power* Return the smallest integer p for which 2**p >= n
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*  Arguments
!
INTEGER, INTENT(IN) :: n
INTEGER :: two_power

!
!*Local variables
!
INTEGER :: p


p = 0
DO WHILE (2**p.LT.n)
   p = p + 1
ENDDO
two_power = p


RETURN

END FUNCTION two_power

!========================================================================

SUBROUTINE upper_case(string)
!************************************************************************
!
! *upper_case* Convert a string to upper case
!
! Author - Patrick Luyten
!
! Description -
!
!************************************************************************
!
!*  Arguments
!
CHARACTER (LEN=*), INTENT(INOUT) :: string

!
! Name      Type    Purpose
!------------------------------------------------------------------------------
!*string*   CHAR    input/output string
!
!------------------------------------------------------------------------------
!
!*Local variables
!
INTEGER :: idist, l


idist = IACHAR('a') - IACHAR('A')

l_100: DO l=1,LEN_TRIM(string)
   IF (LGE(string(l:l),'a') .AND. LLE(string(l:l),'z')) THEN
      string(l:l) = ACHAR(IACHAR(string(l:l)) - idist)
   ENDIF
ENDDO l_100

RETURN

END SUBROUTINE upper_case

END MODULE utility_routines
