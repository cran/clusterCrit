! ===========================================================================
! File: "critUtils.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2014-12-04 12:41:36
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================


MODULE critUtils

      use norms

      !! Declare static variables
      !! ------------------------
      !! Number of rows, columns, clusters
      INTEGER, SAVE :: sNr, sNc, sNk, sNk1, sNk2
      
      !! Matrix of cluster barycenters
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: sKBar
      !! Total barycenter
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sTBar
      
      !! Matrix of intra-cluster variances for each variable
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: sKVar
      !! Vector of total variances for each variable
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sTVar
      
      !! Pooled within-group dispersion matrix
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: sWTMat
      !! Per cluster within-group dispersion matrix
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, SAVE :: sWKMat
      !! Between-group dispersion matrix
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: sBMat

      !! Within-group sums of distances to barycenter
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sWgPtsBarySum
      !! Within-group sums of squared distances to barycenter
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sWgPtsBarySumPow
      !! Within-group sums of pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sWgPairsSum
      !! Within-group max of pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sWgPairsMax
     
      !! Between-group min of pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sBgPairsMin
      !! Between-group max of pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sBgPairsMax
      !! Between-group sums of pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sBgPairsSum
      !! Between-group distances between barycenters
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sBgPairsBary
      
      !! Within-group pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sWgDist
      !! Between-group pairwise distances
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, SAVE :: sBgDist
      
      !! Distances between points and clusters
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, SAVE :: sPtClDist
      
      !! Number of points in each cluster
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: sKNum
      !! Number of pairs of points
      INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: sPNum
      !! Number of concordances and discordances between pairs of distances
      INTEGER(KIND=8), DIMENSION(:), ALLOCATABLE, SAVE :: sConc
      !! Table of number of pairs of 4 types: belonging/not_belonging to the
      !! same cluster wrt partition P1/P2
      INTEGER, DIMENSION(:,:), ALLOCATABLE, SAVE :: sNTb

      !! Additive value indicating the arrays which have to be computed. It is
      !! tested with the masks below.
      INTEGER, SAVE :: sFlg
      !! Masks
       INTEGER, SAVE :: fWgPtsBarySum       = 1, &
                        fWgPtsBarySumPow    = 2, &
                        fWgPairsSum         = 3, &
                        fWgPairsMax         = 4, &
                        fBgPairsMax         = 5, &
                        fBgPairsMin         = 6, &
                        fBgPairsSum         = 7, &
                        fBgPairsBary        = 8, &
                        fPairsDist          = 9, &
                        fWgKMat             = 10, &
                        fPtClDist           = 11

      !! Pointers
      DOUBLE PRECISION, POINTER :: pWgss
      DOUBLE PRECISION, TARGET :: sWgss

      DOUBLE PRECISION, POINTER :: pBgss
      DOUBLE PRECISION, TARGET :: sBgss

      DOUBLE PRECISION, POINTER :: pDetW
      DOUBLE PRECISION, TARGET :: sDetW

      DOUBLE PRECISION, POINTER :: pDetT
      DOUBLE PRECISION, TARGET :: sDetT

      DOUBLE PRECISION, POINTER :: pScat
      DOUBLE PRECISION, TARGET :: sScat
      DOUBLE PRECISION, TARGET :: sStDev

      CONTAINS


SUBROUTINE cluc_crit_int_init (nr,nc,nk)
    IMPLICIT NONE
    integer, intent(in) :: nr, nc, nk

    nullify(pWgss)
    nullify(pBgss)
    nullify(pDetW)
    nullify(pDetT)
    nullify(pScat)
    sNr = nr
    sNc = nc
    sNk = nk
    sFlg = 0
      
END SUBROUTINE  cluc_crit_int_init
    

SUBROUTINE cluc_crit_int_dispose ()
   IMPLICIT NONE
 
   !! Dynamic arrays
   IF ( allocated(sKBar) ) THEN; deallocate(sKBar); END IF
   IF ( allocated(sTBar) ) THEN; deallocate(sTBar); END IF
   IF ( allocated(sWTMat) ) THEN; deallocate(sWTMat); END IF
   IF ( allocated(sWKMat) ) THEN; deallocate(sWKMat); END IF
   IF ( allocated(sBMat) ) THEN; deallocate(sBMat); END IF
   IF ( allocated(sKNum) ) THEN; deallocate(sKNum); END IF
   IF ( allocated(sPNum) ) THEN; deallocate(sPNum); END IF
   IF ( allocated(sConc) ) THEN; deallocate(sConc); END IF
   IF ( allocated(sWgPtsBarySum) ) THEN; deallocate(sWgPtsBarySum); END IF
   IF ( allocated(sWgPtsBarySumPow) ) THEN; deallocate(sWgPtsBarySumPow); END IF
   IF ( allocated(sWgPairsMax) ) THEN; deallocate(sWgPairsMax); END IF
   IF ( allocated(sWgPairsSum) ) THEN; deallocate(sWgPairsSum); END IF
   IF ( allocated(sBgPairsMin) ) THEN; deallocate(sBgPairsMin); END IF
   IF ( allocated(sBgPairsMax) ) THEN; deallocate(sBgPairsMax); END IF
   IF ( allocated(sBgPairsBary) ) THEN; deallocate(sBgPairsBary); END IF
   IF ( allocated(sBgPairsSum) ) THEN; deallocate(sBgPairsSum); END IF
   IF ( allocated(sWgDist) ) THEN; deallocate(sWgDist); END IF
   IF ( allocated(sBgDist) ) THEN; deallocate(sBgDist); END IF
   IF ( allocated(sPtClDist) ) THEN; deallocate(sPtClDist); END IF
   IF ( allocated(sNTb) ) THEN; deallocate(sNTb); END IF
   IF ( allocated(sKVar) ) THEN; deallocate(sKVar); END IF
   IF ( allocated(sTVar) ) THEN; deallocate(sTVar); END IF
   
   !! Pointers
   nullify(pWgss)
   nullify(pBgss)
       
END SUBROUTINE  cluc_crit_int_dispose


SUBROUTINE cluc_crit_ext_init (nr,nk1,nk2)
    IMPLICIT NONE
    integer, intent(in) :: nr,nk1,nk2

    sNr = nr
    sNk1 = nk1
    sNk2 = nk2
    
END SUBROUTINE  cluc_crit_ext_init
    

SUBROUTINE cluc_crit_ext_dispose ()
   IMPLICIT NONE
 
   IF ( allocated(sNTb) ) THEN; deallocate(sNTb); END IF
       
END SUBROUTINE  cluc_crit_ext_dispose


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_alloc_arrays(p,e)" --
! 
! p		in		partition vector
! e		out		error code (0 for no error)
! 
! Allocate the arrays corresponding to the bits of the sFlg value.
! Min arrays initialized with huge(nrm).
! Other arrays are initialized with 0.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_alloc_arrays(p,e)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      integer, intent(out) :: e
      double precision :: nrm
      integer :: pk
      
      !! sWgPtsBarySum K-vector
      IF ( btest(sFlg, fWgPtsBarySum) ) THEN
         allocate( sWgPtsBarySum(sNk), stat = e)
         IF (e == 0) THEN
            sWgPtsBarySum = 0
         ELSE
            RETURN
         END IF
      END IF
      
      !! sWgPtsBarySumPow K-vector
      IF ( btest(sFlg, fWgPtsBarySumPow) ) THEN
         allocate( sWgPtsBarySumPow(sNk), stat = e)
         IF (e == 0) THEN
            sWgPtsBarySumPow = 0
         ELSE
            RETURN
         END IF
      END IF
             
      !! sWgPairsSum K-vector
      IF ( btest(sFlg, fWgPairsSum) ) THEN
         allocate( sWgPairsSum(sNk), stat = e)
         IF (e == 0) THEN
            sWgPairsSum = 0
         ELSE
            RETURN
         END IF
      END IF
             
      !! sWgPairsMax K-vector
      IF ( btest(sFlg, fWgPairsMax) ) THEN
         allocate( sWgPairsMax(sNk), stat = e)
         IF (e == 0) THEN
            sWgPairsMax = 0
         ELSE
            RETURN
         END IF
      END IF
      
      !! Pairs of clusters
      pk = (sNk*(sNk-1))/2
      
      !! sBgPairsMin K(K-1)/2 vector
      IF ( btest(sFlg, fBgPairsMin) ) THEN
         allocate( sBgPairsMin(pk), stat = e)
         IF (e == 0) THEN
           sBgPairsMin = huge(nrm)
         ELSE
            RETURN
         END IF
      END IF
             
      !! sBgPairsMax K(K-1)/2 vector
      IF ( btest(sFlg, fBgPairsMax) ) THEN
         allocate( sBgPairsMax(pk), stat = e)
         IF (e == 0) THEN
           sBgPairsMax = 0
         ELSE
            RETURN
         END IF
      END IF
             
      !! sBgPairsSum K(K-1)/2 vector
      IF ( btest(sFlg, fBgPairsSum) ) THEN
         allocate( sBgPairsSum(pk), stat = e)
         IF (e == 0) THEN
            sBgPairsSum = 0
         ELSE
            RETURN
         END IF
      END IF
             
      !! sBgPairsBary K(K-1)/2 vector
      IF ( btest(sFlg, fBgPairsBary) ) THEN
         allocate( sBgPairsBary(pk), stat = e)
         IF (e == 0) THEN
            sBgPairsBary = 0
         ELSE
            RETURN
         END IF
      END IF
             
      !! sWgDist, sBgDist: vectors of size sPNum(1) (=N_W) and sPNum(2) (=N_B)
      !! respectively (see vignette for definition of N_W and N_B)
      IF ( btest(sFlg, fPairsDist) ) THEN
         call cluc_pair_counts(p) 
         
         allocate( sWgDist(sPNum(1)), stat = e)
         IF (e == 0) THEN
            sWgDist = 0
         ELSE
            RETURN
         END IF
         
         allocate( sBgDist(sPNum(2)), stat = e)
         IF (e == 0) THEN
            sBgDist = 0
         ELSE
            RETURN
         END IF
      END IF
             
      !! sWKMat: sNc x sNc x sNk array for the K matrices WG{k}
      IF ( btest(sFlg, fWgKMat) ) THEN
         allocate( sWKMat(sNc, sNc, sNk) )
         IF (e /= 0) THEN
            RETURN
         END IF
      END IF
             
             
      !! sPtClDist: sNr x sNk array for point to clusters distances
      IF ( btest(sFlg, fPtClDist) ) THEN
         allocate( sPtClDist(sNr, sNk) )
         IF (e == 0) THEN
            sPtClDist = 0
         ELSE
            RETURN
         END IF
      END IF
             
             
END SUBROUTINE  cluc_alloc_arrays


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_group_counts(p)" --
! 
! p		partition vector
! 
! Return in array 'sKNum' the number of elements in each cluster.
! 
! ---------------------------------------------------------------------------
! More compact instruction is slower
!           sKNum = (/ (count(p==i), i=1,sNk) /)

SUBROUTINE cluc_group_counts(p)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      integer :: i, pn

      IF (.not.allocated(sKNum)) THEN
         ! Initialize the array
         allocate( sKNum(sNk) )
 
         sKNum = 0
         DO i=1,sNr
            pn = p(i)
            sKNum(pn) = sKNum(pn) + 1
         END DO
         
      END IF
             
END SUBROUTINE  cluc_group_counts


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_pair_counts(p)" --
! 
! p		partition vector
! 
! Return in array 'sPNum(0:2)' the total number of pairs of points, the number
! of within-group pairs and the number of between-group pairs. See the formulas
! in the vignette.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_pair_counts(p)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p

      IF (.not.allocated(sPNum)) THEN
         ! Initialize the array
         allocate( sPNum(0:2) )
 
         call cluc_group_counts(p)
         sPNum(0) = sNr*(sNr-1)/2
         sPNum(1) = (sum(sKNum**2) - sNr)/2
         sPNum(2) = sPNum(0) - sPNum(1)
         
      END IF
             
END SUBROUTINE  cluc_pair_counts


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_group_barycenters(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in array 'sKBar' a matrix of size nk x nc : line k is the barycenter
! of the k-th cluster.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_group_barycenters(x,p)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer :: i, pn, pc
      
      IF (.not.allocated(sKBar)) THEN
         ! Initialize the array
         allocate( sKBar(sNk, sNc) )
      
         sKBar = 0
         ! Calc the sum of columns for each group
         DO i=1,sNr
            pn = p(i)
            sKBar(pn,:) = sKBar(pn,:) + x(i,:)
         END DO
         
         ! Divide by the number of elements in the groups
         call cluc_group_counts(p)
         DO i=1,sNk
            pc = sKNum(i)
            IF (pc /= 0) THEN
               sKBar(i,:) = sKBar(i,:) / pc
            END IF
         END DO
      END IF
             
END SUBROUTINE  cluc_group_barycenters


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_main_barycenter(x)" --
! 
! x		data matrix
! 
! Return in vector 'sTBar' a vector containing the barycenter of the data
! (mean value of each column of x).
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_main_barycenter(x)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
          
      IF (.not.allocated(sTBar)) THEN
         allocate( sTBar(sNc) )
         sTBar = sum(x,dim=1)/sNr
      END IF
      
      END SUBROUTINE  cluc_main_barycenter
      
      
! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_group_variances(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in array 'sKVar' a matrix of size nk x nc : line k contains the
! variances of each column for the x belonging to the k-th cluster.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_group_variances(x,p)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer :: i, pn, pc
      
      IF (.not.allocated(sKVar)) THEN
         ! Initialize the array
         allocate( sKVar(sNk, sNc) )
         
         sKVar = 0
        ! Calc the sum of squares of columns for each group
         DO i=1,sNr
            pn = p(i)
            sKVar(pn,:) = sKVar(pn,:) + x(i,:)**2
         END DO
         
         ! Ensure the group barycenters
         call cluc_group_barycenters(x,p)
         
         ! Divide by the number of elements in the groups and substract the
         ! squared mean
         call cluc_group_counts(p)
         DO i=1,sNk
            pc = sKNum(i)
            IF (pc /= 0) THEN
               sKVar(i,:) = sKVar(i,:) / pc - sKBar(i,:)**2
            END IF
         END DO
      END IF
             
END SUBROUTINE  cluc_group_variances


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_main_variances(x)" --
! 
! x		data matrix
! 
! Return in vector 'sTVar' a vector containing the variances of the data
! (variance of each column of x).
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_main_variances(x)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
          
      IF (.not.allocated(sTVar)) THEN
         allocate( sTVar(sNc) )
         
         ! Ensure the barycenter
         call cluc_main_barycenter(x)
         
         sTVar = sum(x**2,dim=1)/sNr - sTBar**2
      END IF
      
END SUBROUTINE  cluc_main_variances


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_wg_matrix(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sWTMat' a matrix of size nc x nc : it is the pooled within-group
! dispersion matrix, i-e the sum of the within-group dispersion
! matrix of each cluster:
!     WG = WG(1) + ... + WG(nk)
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_wg_matrix(x,p)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, dimension(sNc,sNc) :: w
      integer :: k
      
      IF (.not.allocated(sWTMat)) THEN
         ! Initialize the array
         allocate( sWTMat(sNc, sNc) )
         
         sWTMat = 0.0
         
         IF (allocated(sWKMat)) THEN
            DO k=1,sNk
               sWTMat = sWTMat + sWKMat(:,:,k)
            END DO          
         ELSE
            DO k=1,sNk
               call cluc_sub_wg(x,p,k,w)
               sWTMat = sWTMat + w
            END DO          
         END IF
         
      END IF
      
END SUBROUTINE  cluc_wg_matrix


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_group_wg_matrix(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sWKMat' array the sNk within-group
! dispersion matrices WG{k}.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_group_wg_matrix(x,p)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer :: k
      
      IF ( btest(sFlg, fWgKMat) ) THEN
         DO k=1,sNk
            call cluc_sub_wg(x,p,k,sWKMat(:,:,k))
         END DO          
      END IF
      
END SUBROUTINE  cluc_group_wg_matrix


! ---------------------------------------------------------------------------
! 
! "FUNCTION cluc_sub_wg (x,p,k,w)" --
! 
! x		in		data matrix
! p		in		partition vector
! k		in		number of a cluster
! w		out		withing-group dispersion matrix
! 
! Return WG(k), the within-group dispersion matrix of cluster k.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_sub_wg (x,p,k,w)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: k
      double precision, dimension(sNc,sNc), intent(out) :: w
      double precision, dimension(sNr) :: u, v
      integer :: i, j
      
     ! Ensure the cluster barycenters
     call cluc_group_barycenters(x,p)
         
      w = 0.0
      DO i=1,sNC
         DO j=1,i
            ! Calc the dot product of v(k)_i and v(k)_j
            WHERE (p==k)
               u = x(:,i) - sKBar(k,i)
               v = x(:,j) - sKBar(k,j)
            ELSEWHERE
               u = 0.0
               v = 0.0
            END WHERE
            w(i,j) = dot_product(u,v)
            IF (i.ne.j) THEN
               w(j,i) = w(i,j)
            END IF
         END DO
      END DO
      
END SUBROUTINE cluc_sub_wg


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_wgss()" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sWgss' the pooled within-group sum of squares, i-e the sum of
! the within-group sum of squares of each cluster:
!     WGSS = WGSS(1) + ... + WGSS(nk)
! WGSS(k) is the Trace of matrix WG(k). It is also the sum of the squared
! distances between the points of the cluster and their barycenter.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_wgss()
      IMPLICIT NONE
!       double precision, intent(in), dimension(sNr,sNc) :: x
!       integer, intent(in), dimension(sNr) :: p

      IF (.not.associated(pWgss)) THEN
         pWgss => sWgss
         
         ! Calc the sum of squared distances 
         sWgss = sum(sWgPtsBarySumPow)
         
      END IF
      
END SUBROUTINE  cluc_wgss


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_bg_matrix(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sBMat' a matrix of size nc x nc : it is the between-group
! dispersion matrix of the barycenters G_k of all the clusters wrt the
! barycenter G of all the data.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_bg_matrix(x,p)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer :: i, j
      
      IF (.not.allocated(sBMat)) THEN
         ! Initialize the array
         allocate( sBMat(sNc, sNc) )
         
         sBMat = 0.0
         
         ! Ensure the cluster barycenters and the total barycenter
         call cluc_group_barycenters(x,p)
         call cluc_main_barycenter(x)
         
         ! Get the number of points in each cluster
         call cluc_group_counts(p)
         
         DO i=1,sNC
            DO j=1,i
               ! Calc the dot product of (G_i - G) and (G_j - G) weighted by the
               ! number sKNum of elements in each cluster
               sBMat(i,j) = dot_product(sKNum(:) * (sKBar(:,i) - sTBar(i)), sKBar(:,j) - sTBar(j))
               IF (i.ne.j) THEN
                  sBMat(j,i) = sBMat(i,j)
               END IF
            END DO
         END DO
      END IF
END SUBROUTINE  cluc_bg_matrix


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_bgss(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sBgss' the between-group sum of squares, i-e the dispersion of
! the barycenters of all the clusters around the barycenter of the data:
!     BGSS = \sum_{k=1}^K n_k || G_k - G ||^2
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_bgss(x,p)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer :: k

      IF (.not.associated(pBgss)) THEN
         pBgss => sBgss
      
         ! Get the cluster barycenters
         call cluc_group_barycenters(x,p)
         
         ! Get the main barycenter
         call cluc_main_barycenter(x)
         
         ! Calc the sum of squared distances weighted by the number 
         ! of points in each cluster
         call cluc_group_counts(p)
         sBgss = 0.0
         DO k=1,sNk
            sBgss = sBgss + sKNum(k) * sum( ( sKBar(k,:) - sTBar(:) )**2 )
         END DO
      END IF
             
END SUBROUTINE  cluc_bgss


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_det_w(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sDetW' the determinant of the pooled within-group matrix.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_det_w(x,p)
      use matrix

      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p

      IF (.not.associated(pDetW)) THEN
         pDetW => sDetW
         
         call cluc_wg_matrix(x,p)
         call cluc_det(sWTMat, sDetW)  
      END IF
      
END SUBROUTINE  cluc_det_w


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_det_t(x,p)" --
! 
! x		data matrix
! p		partition vector
! 
! Return in 'sDetT' the determinant of the total dispersion matrix T (which is
! the sum of WG and BG).
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_det_t(x,p)
      use matrix

      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p

      IF (.not.associated(pDetT)) THEN
         pDetT => sDetT
         
        call cluc_wg_matrix(x,p)
        call cluc_bg_matrix(x,p)
        call cluc_det(sWTMat+sBMat, sDetT)
         
      END IF
      
END SUBROUTINE  cluc_det_t


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_concordances()" --
! 
! Return in array 'sConc(2)' the total number of concordances and discordances
! between pairs of between-group and within-group distances. They are
! respectively the s+ and s- counts used in the Gamma, G+ and Tau indices.
! 
! See in the vignette the formulas used to calculate s+ and s- :
!     s^{+} = \sum_{(r,s)\in I_{B}}\sum_{(u,v)\in I_{W}} 1_{d_{uv} < d_{rs}}
!     s^{-} = \sum_{(r,s)\in I_{B}}\sum_{(u,v)\in I_{W}} 1_{d_{uv} > d_{rs}}
! 
! The sBgDist and sWgDist arrays must have been calculated and sorted already.
! The sPNum array is also available.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_concordances()
      IMPLICIT NONE
      integer :: ib, iw, ip
      DOUBLE PRECISION :: db

      IF (.not.allocated(sConc)) THEN
         ! Initialize the array
         allocate( sConc(2) )
 
         sConc = 0
         ip = 1
         DO ib=1,sPNum(2)
            db = sBgDist(ib)
            IF (ip>1) THEN
               sConc(1) = sConc(1) + ip - 1
            END IF
            DO iw=ip,sPNum(1)
               IF (db < sWgDist(iw)) THEN
                  sConc(2) = sConc(2) + (sPNum(1) - iw + 1)
                  ip = iw
                  exit 
               ELSE
                  sConc(1) = sConc(1) + 1
               END IF
            END DO 
         END DO 
      END IF
END SUBROUTINE  cluc_concordances


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_pairs_sum_minmax(min, max, e)" --
! 
! The sBgDist and sWgDist arrays must have been calculated and sorted already.
! The sPNum array is also available.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_pairs_sum_minmax(min, max, e)
      IMPLICIT NONE
      DOUBLE PRECISION, intent(out) :: min, max
      integer, intent(out) :: e
      DOUBLE PRECISION, dimension(:), allocatable :: tmp
      integer :: nw, nt

      nw = sPNum(1)
      nt = sPNum(0)
      
      allocate( tmp(nt), stat = e)
      IF (e == 0) THEN
         tmp(1:nw) = sWgDist
         tmp((nw+1):nt) = sBgDist
         
         call cluc_heap_sort(tmp,nt,e)
         min = sum(tmp(1:nw))
         max = sum(tmp((nt-nw+1):nt))
         
         deallocate(tmp)
         
      END IF
      
END SUBROUTINE  cluc_pairs_sum_minmax


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_cross_counts(p1,p2,n)" --
! 
! p1		first partition vector
! p2		second partition vector
! n			length of partition vectors
! 
! Given two partitions, calculate the number of pairs belonging or not belonging
! to the same cluster wrt partition P1 or P2. The values are returned in the
! array 'sNTb':
! 
!           |	1	|	2	|
!       _____________________
!        1	|	Nyy	|	Nyn	|
!        2	|	Nny	|	Nnn	|
!       _____________________

! ---------------------------------------------------------------------------

SUBROUTINE cluc_cross_counts(p1,p2,n)
      IMPLICIT NONE
      integer, intent(in) :: n
      integer, intent(in), dimension(n) :: p1, p2
      integer :: i, j
     
      IF (.not.allocated(sNTb)) THEN
         allocate( sNTb(2,2) )
         sNTb = 0
         
         DO i=1,n-1
            DO j=i+1,n
               IF (p1(i) == p1(j)) THEN
                  IF (p2(i) == p2(j)) THEN
                     sNTb(1,1) = sNTb(1,1) + 1
                  ELSE
                     sNTb(1,2) = sNTb(1,2) + 1
                  END IF
               ELSE
                  IF (p2(i) == p2(j)) THEN
                     sNTb(2,1) = sNTb(2,1) + 1
                  ELSE
                     sNTb(2,2) = sNTb(2,2) + 1
                  END IF
               END IF
            END DO
        END DO
        
      END IF
      
END SUBROUTINE  cluc_cross_counts


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_points_bary_distances(x,p,n,e)" --
! 
! x		in		data matrix
! p		in		partition vector
! n		in		Ln norm
! e		out		error code (0 for no error)
! 
! Return in vector 'sWgPtsBarySum' the sum, for each cluster, of the distances
! between the points and the barycenter.
! 
! Return in vector 'sWgPtsBarySumPow' the sum, for each cluster, of the
! distances at the n power between the points and the barycenter.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_points_bary_distances(x,p,n,e)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: n
      integer, intent(out) :: e
      integer :: i, pn
      logical :: hSum, hSumPow
      double precision :: nrm

      e = 0
      hSum = btest(sFlg, fWgPtsBarySum)
      hSumPow = btest(sFlg, fWgPtsBarySumPow)

      ! Ensure the cluster barycenters
      call cluc_group_barycenters(x,p)
      
      DO i=1,sNr
         pn = p(i)
         nrm = cluc_norm_ln( x(i,:) - sKBar(pn,:), n, .false.)
         IF (hSum) THEN
            sWgPtsBarySum(pn) = sWgPtsBarySum(pn) + cluc_norm_scale(nrm,n)
         END IF
         
         IF (hSumPow) THEN
            sWgPtsBarySumPow(pn) = sWgPtsBarySumPow(pn) + nrm
         END IF
         
      END DO
      
END SUBROUTINE  cluc_points_bary_distances


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_pairwise_distances(x,p,n,e)" --
! 
! x		in		data matrix
! p		in		partition vector
! n		in		Ln norm
! e		out		error code (0 for no error)
! 
! The pairwise distances between N items (points or partitions) are stored in a
! vector of length N(N-1)/2.
! The mapping is:
!     (i,j) ---> i + (j-1)(j-2)/2
! where j > i, i-e 1 <= i <= N-1  and  i+1 <= j <= N
! 
! Fill the following arrays depending on the bits set in the sFlg variable:
!     sBgPairsMin, sBgPairsMax, sBgPairsSum, 
!     sWgPairsMax, sWgPairsSum
!     sPtClDist
!     sBgDist, sWgDist
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_pairwise_distances(x,p,n,e)
    IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: n
      integer, intent(out) :: e
      integer :: i, j, idx, pn, wix, bix, p1, p2
      logical :: hWgSum, hWgMax, &
                 hBgMax, hBgMin, hBgSum, &
                 hDists, hPtCls
      double precision :: nrm

      e = 0
      wix = 1
      bix = 1
      
      hWgSum = btest(sFlg, fWgPairsSum)
      hWgMax = btest(sFlg, fWgPairsMax)
      hBgMax = btest(sFlg, fBgPairsMax)
      hBgMin = btest(sFlg, fBgPairsMin)
      hBgSum = btest(sFlg, fBgPairsSum)
      hDists = btest(sFlg, fPairsDist)
      hPtCls = btest(sFlg, fPtClDist)

      DO i=1,sNr-1
        pn = p(i)
        DO j=i+1,sNr
            nrm = cluc_norm_ln( x(i,:) - x(j,:), n)

            IF (p(j) == pn) THEN
               ! Points belong to the same cluster
               IF (hWgMax) THEN
                  sWgPairsMax(pn) = max( sWgPairsMax(pn), nrm)
               END IF
               IF (hWgSum) THEN
                  sWgPairsSum(pn) = sWgPairsSum(pn) + nrm
               END IF
               IF (hDists) THEN
                  sWgDist(wix) = nrm
                  wix = wix + 1
               END IF
               IF (hPtCls) THEN
                  sPtClDist(i,pn) = sPtClDist(i,pn) + nrm
                  sPtClDist(j,pn) = sPtClDist(j,pn) + nrm
               END IF
            ELSE
               ! Points do not belong to the same cluster
               p1 = min(p(i),p(j))
               p2 = max(p(i),p(j))
               idx = p1 + ((p2-1)*(p2-2))/2
               IF (hBgMin) THEN
                  sBgPairsMin(idx) = min( sBgPairsMin(idx), nrm)
               END IF
               IF (hBgMax) THEN
                  sBgPairsMax(idx) = max( sBgPairsMax(idx), nrm)
               END IF
               IF (hBgSum) THEN
                  sBgPairsSum(idx) = sBgPairsSum(idx) + nrm
               END IF
              IF (hDists) THEN
                  sBgDist(bix) = nrm
                  bix = bix + 1
               END IF
               IF (hPtCls) THEN
                  sPtClDist(i,p(j)) = sPtClDist(i,p(j)) + nrm
                  sPtClDist(j,pn) = sPtClDist(j,pn) + nrm
               END IF
            END IF
                     
         END DO
      END DO
      
      IF (hDists) THEN
          ! Sort the arrays of pair distances
         call cluc_heap_sort(sWgDist,sPNum(1),e)
         call cluc_heap_sort(sBgDist,sPNum(2),e)         
      END IF
      
END SUBROUTINE  cluc_pairwise_distances



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_inter_bary_distances(x,p,n,e)" --
! 
! x		in		data matrix
! p		in		partition vector
! n		in		Ln norm
! e		out		error code (0 for no error)
! 
! Return in vector 'sBgPairsBary' the pairwise distances between the
! barycenters.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_inter_bary_distances(x,p,n,e)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: n
      integer, intent(out) :: e
      integer :: i, j

      e = 0
      IF ( btest(sFlg, fBgPairsBary) ) THEN
         ! Ensure the cluster barycenters
         call cluc_group_barycenters(x,p)
         ! Calculate the distances
         DO i=1,sNk-1
            DO j=i+1,sNk
               sBgPairsBary(i+(j-1)*(j-2)/2) = cluc_norm_ln(sKBar(i,:)-sKBar(j,:),n)
            END DO
         END DO
      END IF

END SUBROUTINE  cluc_inter_bary_distances


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_scat(x,p,n)" --
! 
! x		in		data matrix
! p		in		partition vector
! n		in		Ln norm
! 
! Return in 'sScat' the average scattering among clusters and in 'sStDev' the
! threshold used to calculate densities around barycenters (in S_Dbw index).
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_scat(x,p,n)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: n
      double precision :: sc
      integer :: k
      
      IF (.not.associated(pScat)) THEN
         pScat => sScat
      
         call cluc_group_variances(x,p)
         call cluc_main_variances(x)
         sc = 0.0
         DO k=1,sNk
             sc = sc + cluc_norm_ln(sKVar(k,:),n)
         END DO
         sScat = sc / (sNk * cluc_norm_ln(sTVar,n))
         sStDev = sqrt(sc) / sNk

      END IF
 
END SUBROUTINE cluc_scat



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_bw_density(x,p,n,d)" --
! 
! x		in		data matrix
! p		in		partition vector
! n		in		Ln norm
! d		out		inter-cluster density
! 
! Return in 'd' the inter-cluster density. Density d_kk' at some point P is the
! count of elements of the union of C_k and C_k' which are at distance less than
! sStDev from P. A density ratio R_kk' is then defined as 
!     d_kk'(H) / max(d_kk'(G_k), d_kk'(G_k')) 
! where H is the middle of G_kG_k'. The inter-cluster density is the mean value
! of the R_kk' for all k, k'.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_bw_density(x,p,n,d)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: n
      double precision, intent(out) :: d
      double precision, dimension(sNc) :: h
      integer :: i, k1, k2, c1, c2, c3
      
      ! Ensure the group barycenters
      call cluc_group_barycenters(x,p)
      
      c1 = 0
      c2 = 0
      c3 = 0
      d = 0.0

      DO k1=1,sNk-1
          DO k2=k1+1,sNk
              h = (sKBar(k1,:) + sKBar(k2,:))/2
              DO i=1,sNr
                  IF (p(i) == k1 .or. p(i) == k2) THEN
                      IF ( cluc_norm_ln( (x(i,:) - sKBar(k1,:)), n) < sStDev ) THEN
                          c1 = c1 + 1
                      END IF
                      IF ( cluc_norm_ln( (x(i,:) - sKBar(k2,:)), n) < sStDev ) THEN
                          c2 = c2 + 1
                      END IF
                      IF ( cluc_norm_ln( (x(i,:) - h), n) < sStDev ) THEN
                          c3 = c3 + 1
                      END IF
                  END IF
              END DO
              d = d + real(c3)/max(c1,c2)
          END DO
      END DO
      
END SUBROUTINE cluc_bw_density


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_bary_dist_ratios(x,p,n,r)" --
! 
! x		in		data matrix
! p		in		partition vector
! n		in		Ln norm
! r		out		dist-to-barycenter ratios
! 
! Return in array 'r' the sum of the distance-to-barycenter ratios. For a point
! M in cluster k, calculate the ratio between the distance MG_k and the min of
! all the distances to the other barycenters MG_k' (k'/=k): array 'r' contains
! the sums of these ratios calculated over all the points of each cluster.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_bary_dist_ratios(x,p,n,r)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: n
      double precision, intent(out), dimension(sNk) :: r
      double precision, dimension(sNk) :: dpb
      double precision :: rd
      integer :: i, k, pn
      
      ! Ensure the group barycenters
      call cluc_group_barycenters(x,p)
      
      r = 0.0
      rd = 0.0
      DO i=1,sNr
         pn = p(i)
         DO k=1,sNk
            IF (k == pn) THEN
               rd = cluc_norm_ln( (x(i,:) - sKBar(k,:)), n)
               dpb(k) = huge(rd)
            ELSE
               dpb(k) = cluc_norm_ln( (x(i,:) - sKBar(k,:)), n)
            END IF
         END DO
         r(pn) = r(pn) + rd/minval(dpb)
      END DO
      
END SUBROUTINE cluc_bary_dist_ratios


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_heap_sort(arr,n,e)" --
! 
! arr	in		array to sort
! n		in		size of array
! e		out		error (0 if OK)
! 
! Subroutine to sort an n-array in increasing order using the heapsort
! technique.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_heap_sort(arr,n,e)
   IMPLICIT NONE

   INTEGER, INTENT(IN) :: n
   DOUBLE PRECISION, DIMENSION(n), INTENT(INOUT) :: arr
   INTEGER, INTENT(OUT) :: e
   INTEGER :: i, j, rp, hp
   DOUBLE PRECISION :: tmp

   e = 0
   IF ( n > 0 ) THEN
      hp  = n / 2 + 1
      rp = n
loop: DO WHILE (rp > 1)
         IF ( hp > 1 ) THEN
             hp    = hp - 1
             tmp = arr(hp)
         ELSE
            tmp = arr(rp)
            arr(rp) = arr(1)
            rp = rp - 1
            IF ( rp == 1 ) THEN
               ! Store final value
               arr(1) = tmp
               RETURN
            END IF
         END IF
         i = hp
         j = 2*hp
         ! Move tmp down
         DO WHILE ( j <= rp ) 
                IF ( j < rp ) THEN
                   IF ( arr(j) < arr(j+1) ) j = j + 1
                END IF
                IF ( tmp < arr(j) ) THEN
                   arr(i) = arr(j)
                   i = j
                   j = 2*j
                ELSE
                   j = rp + 1
                END IF
         END DO
         arr(i) = tmp
      END DO loop
   END IF
      
END SUBROUTINE cluc_heap_sort



END MODULE critUtils

