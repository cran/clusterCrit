! ===========================================================================
! File: "matrix.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2010-05-30 07:59:22
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================


MODULE matrix
   
   CONTAINS
   

! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_vector_trace(x,n,r)" --
! 
! x		in		vector
! n		in		size
! r		out		trace of x
! 
! The vector x has length n^2 and represents a square matrix of size nxn read
! in columns. Return its trace.
! 
! ---------------------------------------------------------------------------
SUBROUTINE cluc_vector_trace(x,n,r)
   IMPLICIT NONE

   integer, intent(in) :: n
   double precision, intent(in), dimension(n*n) :: x
   double precision, intent(out) :: r
   integer :: i
   
   r = 0
   DO i = 0,n-1
      r = r + x(i*(n+1)+1)
   END DO
   
END SUBROUTINE  cluc_vector_trace


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_matrix_trace(x,r)" --
! 
! x		in		vector
! r		out		trace of x
! 
! The array x is a square matrix of size nxn. Return its trace.
! 
! ---------------------------------------------------------------------------
SUBROUTINE cluc_matrix_trace(x,r)
   IMPLICIT NONE
   
   double precision, intent(in), dimension(:,:) :: x
   double precision, intent(out) :: r
   integer :: i
   
   r = 0
   DO i=1,size(x,1)
       r = r + x(i,i)  
   END DO
   
END SUBROUTINE  cluc_matrix_trace


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_det(x,r)" --
! 
! x		in		square matrix
! r		out		determinant of x
! 
! The matrix x is supposed to be a square matrix. Use the Lapack subroutine
! DGETRF to obtain an LU decomposition and take the product of the diagonal
! elements of U.
! 
! ---------------------------------------------------------------------------
! *  M       (input) INTEGER
! *          The number of rows of the matrix A.  M >= 0.
! *
! *  N       (input) INTEGER
! *          The number of columns of the matrix A.  N >= 0.
! *
! *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
! *          On entry, the M-by-N matrix to be factored.
! *          On exit, the factors L and U from the factorization
! *          A = P*L*U; the unit diagonal elements of L are not stored.
! *
! *  LDA     (input) INTEGER
! *          The leading dimension of the array A.  LDA >= max(1,M).
! *
! *  IPIV    (output) INTEGER array, dimension (min(M,N))
! *          The pivot indices; for 1 <= i <= min(M,N), row i of the
! *          matrix was interchanged with row IPIV(i).
! *
! *  INFO    (output) INTEGER
! *          = 0:  successful exit
! *          < 0:  if INFO = -i, the i-th argument had an illegal value
! *          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
! *                has been completed, but the factor U is exactly
! *                singular, and division by zero will occur if it is used
! *                to solve a system of equations.

SUBROUTINE cluc_det(x,r)
   IMPLICIT NONE
   
   INTERFACE 
      SUBROUTINE dgetrf( m, n, a, lda, ipiv, info )
         integer, intent(in) :: m, n, lda
         double precision, intent(inout), dimension (lda,n) :: a
         integer, intent(out), dimension (min(m,n)) :: ipiv
         integer, intent(out) :: info
      END SUBROUTINE  dgetrf
   END INTERFACE 
   
   double precision, intent(in), dimension(:,:) :: x
   double precision, intent(out) :: r
   double precision, dimension(:,:), allocatable :: a
   integer, dimension(:), allocatable :: ipiv
   integer :: i, n, info
   
   r = 0.0
   n = size(x,1)
   allocate( a(n,n), ipiv(n) )
   
   a = x
   call dgetrf( n, n, a, n, ipiv, info )
   
   IF (info.lt.0) THEN
      ! there was an error
   ELSE IF (info.eq.0) THEN
      r = 1.0
      DO i=1,n
         r = r * a(i,i)
      END DO
   ELSE
      r = 0.0
   END IF
   
   deallocate(a, ipiv)
   
END SUBROUTINE  cluc_det
   

! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_sym_matrix_inv(x,xi)" --
! 
! x		in		symmetric positive definite square matrix
! xi	out		the inverse of x
! 
! The matrix x is supposed to be a symmetric positive definite square matrix.
! Use the Lapack subroutines DPOTRF which computes the Cholesky factorization and
! DPOTRI which computes the inverse using this Cholesky factorization.
! 
! See in Lapack
! dgetri.f 	 Computes the inverse of a general matrix, using the LU factorization computed by DGETRF.
! dpotri.f 	 Computes the inverse of a symmetric positive definite matrix, using the Cholesky factorization computed by DPOTRF.
! dpotrf.f 	 Computes the Cholesky factorization of a symmetric positive definite matrix. 
! dgesv.f 	 Solves a general system of linear equations AX=B.
! dposv.f 	 Solves a symmetric positive definite system of linear equations AX=B.
! ---------------------------------------------------------------------------
! *  UPLO    (input) CHARACTER*1
! *          = 'U':  Upper triangle of A is stored;
! *          = 'L':  Lower triangle of A is stored.
! *
! *  N       (input) INTEGER
! *          The order of the matrix A.  N >= 0.
! *
! *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
! *          On entry, the triangular factor U or L from the Cholesky
! *          factorization A = U**T*U or A = L*L**T, as computed by
! *          DPOTRF.
! *          On exit, the upper or lower triangle of the (symmetric)
! *          inverse of A, overwriting the input factor U or L.
! *
! *  LDA     (input) INTEGER
! *          The leading dimension of the array A.  LDA >= max(1,N).
! *
! *  INFO    (output) INTEGER
! *          = 0:  successful exit
! *          < 0:  if INFO = -i, the i-th argument had an illegal value
! *          > 0:  if INFO = i, the (i,i) element of the factor U or L is
! *                zero, and the inverse could not be computed.

SUBROUTINE cluc_sym_matrix_inv(x,xi)
   IMPLICIT NONE
   
   INTERFACE 
      SUBROUTINE dpotri( uplo, n, a, lda, info )
         character(1), intent(in) :: uplo
         integer, intent(in) :: n, lda
         double precision, intent(inout), dimension (lda,n) :: a
         integer, intent(out) :: info
      END SUBROUTINE  dpotri
      SUBROUTINE dpotrf( uplo, n, a, lda, info )
         character(1), intent(in) :: uplo
         integer, intent(in) :: n, lda
         double precision, intent(inout), dimension (lda,n) :: a
         integer, intent(out) :: info
      END SUBROUTINE  dpotrf
   END INTERFACE 
   
   double precision, intent(in), dimension(:,:) :: x
   double precision, intent(out), dimension(:,:) :: xi

   double precision, dimension(:,:), allocatable :: a
   integer :: i, j, n, info
   
   n = size(x,1)
   allocate( a(n,n) )
   
   a = x
   call dpotrf( 'U', n, a, n, info )
   call dpotri( 'U', n, a, n, info )
   
   ! dpotri has filled only the upper part of the matrix
   DO i=2,n
      DO j=1,i-1
         a(i,j)=a(j,i)
      END DO
   END DO
   
   IF (info.lt.0) THEN
      ! there was an error
   ELSE IF (info.eq.0) THEN
      xi = a
   ELSE
      ! not invertible
      ! temporarily return null matrix
      xi = 0
   END IF
   
   deallocate(a)
   
END SUBROUTINE  cluc_sym_matrix_inv


END MODULE matrix
