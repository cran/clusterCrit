! ===========================================================================
! File: "indices.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2015-07-17 21:15:05
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================



MODULE INDICES

      use critUtils
! use logFile
! logical, save :: sTiming = .false.
   
      CONTAINS

   
               !!!!!!!!!!!!!!!!!!!!!!!!
               !                      !
               !   Internal indices   !
               !                      !
               !!!!!!!!!!!!!!!!!!!!!!!!

   
! ---------------------------------------------------------------------------
! All the internal criterion subroutines in this module use the same arguments:
!       x	in		data matrix (of size nr x nc)
!       p	in		partition vector (of length nr)
!       v	out		criterion value
!    
! sNr, sNc, sNk are global vars defined in module critUtils
!       sNr			number of rows
!       sNc			number of cols
!       sNk			number of clusters in p (sNk = maxval(p))
! ---------------------------------------------------------------------------
	

! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_ball_hall(p,v)" --
! 
! Needs sWgPtsBarySumPow.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_ball_hall(p,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNk) :: r
    
      ! Calc, for each cluster, the mean of the squared distances between points
      ! and their barycenter
      call cluc_group_counts(p)
      r = sWgPtsBarySumPow / sKNum

      ! Calc the mean of these distances
      v = sum(r)/sNk
    
END SUBROUTINE cluc_crit_ball_hall



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_banfeld_raftery(p,v)" --
! 
! Needs sWKMat.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_banfeld_raftery(p,v)
      use matrix
      
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNk) :: r
      double precision :: t
      integer :: k
         
      call cluc_group_counts(p)
      DO k=1,sNk
          call cluc_matrix_trace(sWKMat(:,:,k),t)
          r(k) = log(t / sKNum(k))
      END DO
      
      v = sum(r * sKNum)
    
END SUBROUTINE cluc_crit_banfeld_raftery



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_c_index(v)" --
! 
! Needs sWgDist and sBgDist.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_c_index(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
      double precision :: smin, smax, stot
      integer :: e
    
      ! Calc the sum of pairwise distances in each cluster
      stot = sum(sWgDist)

      call cluc_pairs_sum_minmax(smin, smax, e)
      
      v = (stot-smin)/(smax-smin)
    
END SUBROUTINE cluc_crit_c_index



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_calinski_harabasz(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_calinski_harabasz(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v

      call cluc_wgss()
      call cluc_bgss(x,p)
      v = ((sNr-sNk)*sBgss) / ((sNk-1)*sWgss)
    
END SUBROUTINE cluc_crit_calinski_harabasz



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_davies_bouldin(p,v)" --
! 
! Needs sWgPtsBarySum and sBgPairsBary.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_davies_bouldin(p,v)

      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNk) :: m, d
      double precision :: mx
      integer :: i, j
        
      mx = 0.0
      d(:) = 0.0
      
      ! Ensure the intra-group mean distances
      call cluc_group_counts(p)
      m = sWgPtsBarySum / sKNum

      DO i=1,sNk
       ! Calculate the max of the D_ij quantities
       DO j=1,sNk
          IF (j == i) THEN
             ! Ignore
          ELSE IF (j > i) THEN
             d(i) = (m(i) + m(j))/sBgPairsBary(i+(j-1)*(j-2)/2)
          ELSE
             ! sBgPairsBary is computed only for i < j
             d(i) = (m(i) + m(j))/sBgPairsBary(j+(i-1)*(i-2)/2)
          END IF
       END DO
       mx = mx + maxval( d(:) ) 
      END DO

      v = mx/sNk
    
END SUBROUTINE cluc_crit_davies_bouldin



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_det_ratio(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_det_ratio(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
        
      call cluc_det_w(x,p)
      call cluc_det_t(x,p)  
      v = sDetT/sDetW
    
END SUBROUTINE cluc_crit_det_ratio



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_dunn(v)" --
! 
! Needs min inter-group distances matrix sBgPairsMin and max intra-group
! distances vector sWgPairsMax.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_dunn(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
        
      v = minval(sBgPairsMin)/maxval(sWgPairsMax)
    
END SUBROUTINE cluc_crit_dunn



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_gamma(v)" --
! 
! Needs sConc (which depends on sWgDist, sBgDist, sPNum).
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_gamma(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
      
      call cluc_concordances()
      
      v = real(sConc(1)-sConc(2))/real(sConc(1)+sConc(2))
    
END SUBROUTINE cluc_crit_gamma



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_g_plus(v)" --
! 
! Needs sConc (which depends on sWgDist, sBgDist, sPNum).
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_g_plus(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
        
      call cluc_concordances()
      
      ! v = real(sConc(2))/(sPNum(0)*(sPNum(0)-1)/2)
      v = real(sConc(2))/sPNum(0)
      v = 2*(v/(sPNum(0)-1))
    
END SUBROUTINE cluc_crit_g_plus



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_gdi(p,c1,c2,v)" --
! 
! Calculate the 15 (5x3) GDI indices: 1 <= c1 <= 5, 1 <= c2 <= 3.
! 
! Depending on the values of c1 and c2, needs sBgPairsBary, sBgPairsMax,
! sBgPairsMin, sWgPairsMax, sWgPairsSum, or sWgPtsBarySum.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_gdi(p,c1,c2,e,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      integer, intent(in) :: c1, c2
      integer, intent(out) :: e
      double precision, intent(out) :: v
      double precision, dimension((sNk*(sNk-1))/2) :: m        
      double precision :: num, den
      integer :: idx, i, j

      e = 0
      num = 0.0
      den = 0.0

      SELECT CASE (c1)
          CASE(1)
             num = minval(sBgPairsMin)
          CASE(2)
             num = minval(sBgPairsMax)
          CASE(3)
             call cluc_group_counts(p)
             m = sBgPairsSum
             DO i=1,sNk-1
                DO j=i+1,sNk
                   idx = i+(j-1)*(j-2)/2
                   m(idx) = m(idx) / (sKNum(i)*sKNum(j))
                END DO
             END DO
             num = minval(m)
          CASE(4)
             num = minval(sBgPairsBary)
          CASE(5)
             DO i=1,sNk-1
                DO j=i+1,sNk
                   m(i+(j-1)*(j-2)/2) = (sWgPtsBarySum(i) + sWgPtsBarySum(j)) / (sKNum(i)+sKNum(j))
                END DO
             END DO
             num = minval(m)
          CASE DEFAULT
             e = 1
             !!stop
       END SELECT
       
       SELECT CASE (c2)
          CASE(1)
             den = maxval(sWgPairsMax)
          CASE(2)
             call cluc_group_counts(p)
             den = maxval( sWgPairsSum/(sKNum*(sKNum-1)) )
          CASE(3)
             call cluc_group_counts(p)
             den = 2*maxval( sWgPtsBarySum/sKNum )
          CASE DEFAULT
             e = 1
             !!stop
       END SELECT

       IF (e == 0) THEN
           v = num/den
       END IF

END SUBROUTINE cluc_crit_gdi



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_ksq_detw(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_ksq_detw(x,p,v)
      use matrix
      
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision :: d
        
      call cluc_wg_matrix(x,p)
      call cluc_det(sWTMat,d)  
      v = sNk**2 * d
    
END SUBROUTINE cluc_crit_ksq_detw



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_log_det_ratio(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_log_det_ratio(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
        
      call cluc_det_w(x,p)
      call cluc_det_t(x,p)  
      v = sNr * log(sDetT/sDetW)
    
END SUBROUTINE cluc_crit_log_det_ratio



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_log_ss_ratio(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_log_ss_ratio(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
            
      call cluc_wgss()
      call cluc_bgss(x,p)
      v = log(sBgss/sWgss)
    
END SUBROUTINE cluc_crit_log_ss_ratio



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_mcclain_rao(p,v)" --
! 
! Needs sBgPairsSum, sWgPairsSum, sPNum
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_mcclain_rao(p,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision :: sbg, swg
        
      call cluc_pair_counts(p)
      sbg = sum(sBgPairsSum)
      swg = sum(sWgPairsSum)

      v = (sPNum(2)*swg)/(sPNum(1)*sbg)
    
END SUBROUTINE cluc_crit_mcclain_rao



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_point_biserial(p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_point_biserial(p,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision :: sbg, swg
      
      call cluc_pair_counts(p)

      sbg = sum(sBgPairsSum)
      swg = sum(sWgPairsSum)
      
      ! Beware of overflows
      !       v = sqrt( real(sPNum(1)*sPNum(2))/ sPNum(0)**2 ) * (swg/sPNum(1) - sbg/sPNum(2)) 
      v = sqrt( (real(sPNum(1))/real(sPNum(0)))   *  (real(sPNum(2))/real(sPNum(0))) ) * (swg/sPNum(1) - sbg/sPNum(2)) 
    
END SUBROUTINE cluc_crit_point_biserial



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_pbm(x,v)" --
! 
! Needs sBgPairsBary and sWgPtsBarySum.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_pbm(x,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      double precision, intent(out) :: v
      double precision :: db, ew, et
      integer :: i
      
      db = maxval(sBgPairsBary)
      ew = sum(sWgPtsBarySum)
      
      ! Get the main barycenter
      call cluc_main_barycenter(x)
         
      et = 0.0
      DO i=1,sNr
         et = et + cluc_norm_ln( x(i,:) - sTBar(:), 2)
      END DO
      
      v = ( (et*db)/(sNk*ew) ) **2
    
END SUBROUTINE cluc_crit_pbm



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_ratkowsky_lance(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_ratkowsky_lance(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNc) :: b, t
      integer :: j
      
      ! Get the cluster barycenters
      call cluc_group_barycenters(x,p)
      
      ! Get the main barycenter
      call cluc_main_barycenter(x)
         
      call cluc_group_counts(p)
      DO j=1,sNc
         b(j) = sum( sKNum * ( sKBar(:,j) - sTBar(j) )**2  )
         t(j) = sum ( (x(:,j) - sTBar(j))**2 )
      END DO
      
      v = sqrt( sum(b/t)/(sNc*sNk) )
    
END SUBROUTINE cluc_crit_ratkowsky_lance



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_ray_turi(v)" --
! 
! Needs sWgss, sBgPairsBary.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_ray_turi(v)
      IMPLICIT NONE
!       double precision, intent(in), dimension(sNr,sNc) :: x
!       integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
        
      call cluc_wgss()
      v = (sWgss / sNr) / minval(sBgPairsBary)**2
      
END SUBROUTINE cluc_crit_ray_turi



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_scott_symons(p,v)" --
! 
! Needs sWKMat.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_scott_symons(p,v)
      use matrix
      
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNk) :: r
      double precision :: d
      integer :: k
         
      call cluc_group_counts(p)
      DO k=1,sNk
         call cluc_det(sWKMat(:,:,k),d)  
         r(k) = log(d) - sNc * log(real(sKNum(k)))
      END DO
      
      v = sum(r * sKNum)
    
END SUBROUTINE cluc_crit_scott_symons



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_sd_dis(v)" --
! 
! Needs sBgPairsBary
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_sd_dis(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
      double precision :: di, sib
      integer :: k1, k2, idx
      
      ! Calc the total separation between clusters
      di = 0.0
      DO k1=1,sNk
          sib = 0
          DO k2=1,sNk
              ! Calc the sum of inter-barycenter distances wrt G_k1
              IF (k2 > k1) THEN
                  idx = k1 + (k2-1)*(k2-2)/2
              ELSE IF (k1 > k2) THEN
                  idx = k2 + (k1-1)*(k1-2)/2
              ELSE
                  cycle
              END IF
              sib = sib + sBgPairsBary(idx)
          END DO
          ! Add the inverse of this sum
          di = di + (1/sib)
      END DO
      v = di * (maxval(sBgPairsBary) / minval(sBgPairsBary))

END SUBROUTINE cluc_crit_sd_dis



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_sd_scat(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_sd_scat(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      
      ! Calc the average scattering among clusters
      call cluc_scat(x,p,2)
      v = sScat

END SUBROUTINE cluc_crit_sd_scat



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_s_dbw(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_s_dbw(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision :: ds
      
      ! Calc the average scattering among clusters and the stdev threshold
      call cluc_scat(x,p,2)
      
      ! Calculate the inter-cluster density
      call cluc_bw_density(x,p,2,ds)
      
      v = sScat + ds
    
END SUBROUTINE cluc_crit_s_dbw



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_silhouette(p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_silhouette(p,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNk) :: dk, sk
      double precision :: a, b, s
      integer :: i, k, pn, numberOfElements
      
      sk = 0
      call cluc_group_counts(p)

      DO i=1,sNr
         pn = p(i)
         
         ! Calc the average intra-cluster distance
         numberOfElements=sKNum(pn)
         IF (numberOfElements == 1) THEN
           a = 0
         ELSE
           a = sPtClDist(i,pn)/(numberOfElements-1)
         END IF

         ! Calc the minimum average distance to other clusters
         DO k=1,sNk
            IF (k == pn) THEN
               dk(k) = huge(a)
            ELSE
               dk(k) = sPtClDist(i,k)/sKNum(k)
            END IF
         END DO
         b = minval(dk)
         
         ! Calc the silhouette width
         s = (b-a)/max(a,b)
         
         ! Add it to the corresponding cluster silhouette
         sk(pn) = sk(pn) + s
         
      END DO
      
      v = sum(sk/sKNum)/sNk
          
END SUBROUTINE cluc_crit_silhouette



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_tau(v)" --
! 
! Needs sConc (which depends on sWgDist, sBgDist, sPNum).
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_tau(v)
      IMPLICIT NONE
      double precision, intent(out) :: v

      call cluc_concordances()

      ! Beware of overflows: better divide in several stages
      !       v = real(sConc(1)-sConc(2))/sqrt(real(sPNum(1)*sPNum(2))) 
      !       v = v/sqrt(real(sPNum(0)*(sPNum(0)-1)/2))
      v = real(sConc(1)-sConc(2))/sqrt(real(sPNum(1))) 
      v = v/sqrt(real(sPNum(2))) 
      v = v/sqrt(real(sPNum(0)/2))       
      v = v/sqrt(real(sPNum(0)-1))       
    
END SUBROUTINE cluc_crit_tau



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_trace_w(v)" --
! 
! Needs sWgss.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_trace_w(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
            
      call cluc_wgss()
      v = sWgss
    
END SUBROUTINE cluc_crit_trace_w



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_trace_wib(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_trace_wib(x,p,v)
      use matrix

      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNc,sNc) :: wi

      v = 0.0
      call cluc_wg_matrix(x,p)
      call cluc_bg_matrix(x,p)
      call cluc_sym_matrix_inv(sWTMat,wi)
      call cluc_matrix_trace(matmul(wi,sBMat),v)
    
END SUBROUTINE cluc_crit_trace_wib

    

! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_wemmert_gancarski(x,p,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_wemmert_gancarski(x,p,v)
      IMPLICIT NONE
      double precision, intent(in), dimension(sNr,sNc) :: x
      integer, intent(in), dimension(sNr) :: p
      double precision, intent(out) :: v
      double precision, dimension(sNk) :: r
        
      call cluc_bary_dist_ratios(x,p,2,r)
      call cluc_group_counts(p)

      r = sKNum - r
      WHERE (r < 0)
         r = 0
      END WHERE
      
      v = sum(r)/sNr
    
END SUBROUTINE cluc_crit_wemmert_gancarski



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_xie_beni(v)" --
! 
! Needs sWgss, sBgPairsMin.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_xie_beni(v)
      IMPLICIT NONE
      double precision, intent(out) :: v
        
      call cluc_wgss()
      v = (sWgss / sNr) / minval(sBgPairsMin)**2

END SUBROUTINE cluc_crit_xie_beni



            !!!!!!!!!!!!!!!!!!!!!!!!
            !                      !
            !   External indices   !
            !                      !
            !!!!!!!!!!!!!!!!!!!!!!!!

! ---------------------------------------------------------------------------
! All the external criterion subroutines in this module use the same arguments:
!       p1	in		first partition vector (of length nr)
!       p2	in		second partition vector (of length nr)
!       v	out		criterion value
!    
! sNr, sNc, sNk are global vars defined in module critUtils
!       sNr			number of rows
!       sNk1		number of clusters in p1 (sNk1 = maxval(p1))
!       sNk2		number of clusters in p2 (sNk2 = maxval(p2))
! ---------------------------------------------------------------------------
	

! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_czekanowski_dice(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_czekanowski_dice(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(2*sNTb(1,1))/(2*sNTb(1,1)+sNTb(1,2)+sNTb(2,1))
    
END SUBROUTINE cluc_crit_czekanowski_dice



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_folkes_mallows(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_folkes_mallows(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = sqrt( real(sNTb(1,1))/(sNTb(1,1) + sNTb(1,2)) ) * sqrt( real(sNTb(1,1))/(sNTb(1,1) + sNTb(2,1)) )
   
END SUBROUTINE cluc_crit_folkes_mallows



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_hubert(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_hubert(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      call cluc_cross_counts(p1,p2,sNr)
      v = real( sum(sNTb) * sNTb(1,1) - (sNTb(1,1) + sNTb(1,2))*(sNTb(1,1) + sNTb(2,1)) ) 
      v = v / ( sqrt(real(sNTb(1,1)+sNTb(1,2))) * sqrt(real(sNTb(1,1)+sNTb(2,1))) )
      v = v / ( sqrt(real(sNTb(1,2)+sNTb(2,2))) * sqrt(real(sNTb(2,1)+sNTb(2,2))) )
    
END SUBROUTINE cluc_crit_hubert



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_jaccard(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_jaccard(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1))/(sNTb(1,1) + sNTb(1,2) + sNTb(2,1))
    
END SUBROUTINE cluc_crit_jaccard



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_kulczynski(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_kulczynski(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1))/(sNTb(1,1)+sNTb(1,2)) + real(sNTb(1,1))/(sNTb(1,1)+sNTb(2,1))
      v=v/2
      
END SUBROUTINE cluc_crit_kulczynski



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_mcnemar(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_mcnemar(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(2,2) - sNTb(2,1))/sqrt(real(sNTb(2,2) + sNTb(2,1)))
    
END SUBROUTINE cluc_crit_mcnemar



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_phi(p1,p2,v)" --
! 
!     v = real(sNTb(1,1)*sNTb(2,2) - sNTb(1,2)*sNTb(2,1)) / &
!           ((sNTb(1,1)+sNTb(1,2))*(sNTb(1,1)+sNTb(2,1))*(sNTb(1,2)+sNTb(2,2))*(sNTb(2,1)+sNTb(2,2)))
! 
!  CAVEAT: the product in the denominator can be very big, so divide in two steps.
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_phi(p1,p2,v)

      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1)*sNTb(2,2) - sNTb(1,2)*sNTb(2,1)) 
      v = v / ( (sNTb(1,1)+sNTb(1,2)) * (sNTb(1,1)+sNTb(2,1)) )
      v = v / ( (sNTb(1,2)+sNTb(2,2)) * (sNTb(2,1)+sNTb(2,2)) )
    
END SUBROUTINE cluc_crit_phi



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_precision(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_precision(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1))/(sNTb(1,1)+sNTb(2,1))
    
END SUBROUTINE cluc_crit_precision



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_rand(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_rand(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1) + sNTb(2,2))/sum(sNTb)
    
END SUBROUTINE cluc_crit_rand



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_recall(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_recall(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1))/(sNTb(1,1)+sNTb(1,2))
    
END SUBROUTINE cluc_crit_recall


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_rogers_tanimoto(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_rogers_tanimoto(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
      double precision :: num, den
        
      call cluc_cross_counts(p1,p2,sNr)
      num = real(sNTb(1,1) + sNTb(2,2))/2
      den = num + sNTb(1,2) + sNTb(2,1)
      v = num/den
    
END SUBROUTINE cluc_crit_rogers_tanimoto



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_russel_rao(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_russel_rao(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
        
      call cluc_cross_counts(p1,p2,sNr)
      v = real(sNTb(1,1))/sum(sNTb)
    
END SUBROUTINE cluc_crit_russel_rao



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_sokal_sneath1(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_sokal_sneath1(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
      double precision :: num, den
        
      call cluc_cross_counts(p1,p2,sNr)
      num = real(sNTb(1,1))/2
      den = num + sNTb(1,2) + sNTb(2,1)
      v = num/den
    
END SUBROUTINE cluc_crit_sokal_sneath1



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_crit_sokal_sneath2(p1,p2,v)" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_crit_sokal_sneath2(p1,p2,v)
      IMPLICIT NONE
      integer, intent(in), dimension(sNr) :: p1, p2
      double precision, intent(out) :: v
      double precision :: num, den
        
      call cluc_cross_counts(p1,p2,sNr)
      num = (sNTb(1,1) + sNTb(2,2))*2
      den = num + sNTb(1,2) + sNTb(2,1)
      v = real(num)/den
    
END SUBROUTINE cluc_crit_sokal_sneath2



END MODULE INDICES

