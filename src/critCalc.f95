! ===========================================================================
! File: "critCalc.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2014-12-04 12:42:04
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================
! Subroutines called from the C code. Not stored in a module.
! 
! ERROR CODES
! -----------
!    cf cluc_errorMsg()


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_int_criterion(x,p,ci,e,v)" --
! 
!       x	in		data matrix (of size nr x nc)
!       p	in		partition vector (of length nr)
!       ci	in		criterion index
!       e	out		error code (0 for no error)
!       v	out		criterion value
! 
! Dispatching subroutine.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_int_criterion(x,p,ci,e,v)
    use indices
! use timer
! use logFile
    IMPLICIT NONE
    double precision, intent(in), dimension(sNr,sNc) :: x
    integer, intent(in), dimension(sNr) :: p
    integer, intent(in) :: ci
    integer, intent(out) :: e
    double precision, intent(out) :: v
    integer :: i, rep

    v = 0.0
    rep = 1
    e = 0

!     IF (sTiming) THEN
!        call startTimer
!        rep = 10000
!     END IF

    !! Compute the criterion
    DO i=1,rep
       IF (ci == 0) THEN
          call cluc_crit_ball_hall(p,v)
       ELSE IF (ci == 1) THEN
          call cluc_crit_banfeld_raftery(p,v)
       ELSE IF (ci == 2) THEN
          call cluc_crit_c_index(v)
       ELSE IF (ci == 3) THEN
          call cluc_crit_calinski_harabasz(x,p,v)
       ELSE IF (ci == 4) THEN
          call cluc_crit_davies_bouldin(p,v)
       ELSE IF (ci == 5) THEN
          call cluc_crit_det_ratio(x,p,v)
       ELSE IF (ci == 6) THEN
          call cluc_crit_dunn(v)
       ELSE IF (ci == 7) THEN
          call cluc_crit_g_plus(v)
       ELSE IF (ci == 8) THEN
          call cluc_crit_gamma(v)
       ELSE IF (ci == 9) THEN
             call cluc_crit_gdi(p,1,1,e,v)
       ELSE IF (ci == 10) THEN
             call cluc_crit_gdi(p,1,2,e,v)
       ELSE IF (ci == 11) THEN
             call cluc_crit_gdi(p,1,3,e,v)
       ELSE IF (ci == 12) THEN
             call cluc_crit_gdi(p,2,1,e,v)
       ELSE IF (ci == 13) THEN
             call cluc_crit_gdi(p,2,2,e,v)
       ELSE IF (ci == 14) THEN
             call cluc_crit_gdi(p,2,3,e,v)
       ELSE IF (ci == 15) THEN
             call cluc_crit_gdi(p,3,1,e,v)
       ELSE IF (ci == 16) THEN
             call cluc_crit_gdi(p,3,2,e,v)
       ELSE IF (ci == 17) THEN
             call cluc_crit_gdi(p,3,3,e,v)
       ELSE IF (ci == 18) THEN
             call cluc_crit_gdi(p,4,1,e,v)
       ELSE IF (ci == 19) THEN
             call cluc_crit_gdi(p,4,2,e,v)
       ELSE IF (ci == 20) THEN
             call cluc_crit_gdi(p,4,3,e,v)
       ELSE IF (ci == 21) THEN
             call cluc_crit_gdi(p,5,1,e,v)
       ELSE IF (ci == 22) THEN
             call cluc_crit_gdi(p,5,2,e,v)
       ELSE IF (ci == 23) THEN
             call cluc_crit_gdi(p,5,3,e,v)
       ELSE IF (ci == 24) THEN
          call cluc_crit_ksq_detw(x,p,v)
       ELSE IF (ci == 25) THEN
          call cluc_crit_log_det_ratio(x,p,v)
       ELSE IF (ci == 26) THEN
          call cluc_crit_log_ss_ratio(x,p,v)
       ELSE IF (ci == 27) THEN
          call cluc_crit_mcclain_rao(p,v)
       ELSE IF (ci == 28) THEN
          call cluc_crit_pbm(x,v)
       ELSE IF (ci == 29) THEN
          call cluc_crit_point_biserial(p,v)
       ELSE IF (ci == 30) THEN
          call cluc_crit_ratkowsky_lance(x,p,v)
       ELSE IF (ci == 31) THEN
          call cluc_crit_ray_turi(v)
       ELSE IF (ci == 33) THEN
          call cluc_crit_scott_symons(p,v)
       ELSE IF (ci == 34) THEN
          call cluc_crit_sd_dis(v)
       ELSE IF (ci == 35) THEN
          call cluc_crit_sd_scat(x,p,v)
       ELSE IF (ci == 32) THEN
          call cluc_crit_s_dbw(x,p,v)
       ELSE IF (ci == 36) THEN
          call cluc_crit_silhouette(p,v)
       ELSE IF (ci == 37) THEN
          call cluc_crit_tau(v)
       ELSE IF (ci == 38) THEN
          call cluc_crit_trace_w(v)
       ELSE IF (ci == 39) THEN
          call cluc_crit_trace_wib(x,p,v)
       ELSE IF (ci == 40) THEN
          call cluc_crit_wemmert_gancarski(x,p,v)
       ELSE IF (ci == 41) THEN
          call cluc_crit_xie_beni(v)
       ELSE
          e = 1
       END IF
    END DO
    
!     IF (sTiming) THEN
!        call stopTimer
!        call logTiming(cn)
!     END IF
    
END SUBROUTINE cluc_calc_int_criterion



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_int_set_flags(ci)" --
! 
!       ci	in		criterion index
! 
! Set the flags to determine which quantities will have to be calculated.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_int_set_flags(ci)
    use critUtils 
! use logFile
    IMPLICIT NONE
    integer, intent(in) :: ci
     
    IF (ci == 0) THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (ci == 1) THEN
       sFlg = ibset(sFlg, fWgKMat)
    ELSE IF (ci == 2) THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF (ci == 3) THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (ci == 4) THEN
       sFlg = ibset(sFlg, fWgPtsBarySum)
       sFlg = ibset(sFlg, fBgPairsBary)
    ELSE IF (ci == 6) THEN
       sFlg = ibset(sFlg, fBgPairsMin)
       sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (ci == 7) THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF (ci == 8) THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF ( ci == 9 ) THEN
             sFlg = ibset(sFlg, fBgPairsMin)
             sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (ci == 10) THEN
             sFlg = ibset(sFlg, fBgPairsMin)
             sFlg = ibset(sFlg, fWgPairsSum)
    ELSE IF (ci == 11) THEN
             sFlg = ibset(sFlg, fBgPairsMin)
             sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (ci == 12) THEN
             sFlg = ibset(sFlg, fBgPairsMax)
             sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (ci == 13) THEN
             sFlg = ibset(sFlg, fBgPairsMax)
             sFlg = ibset(sFlg, fWgPairsSum)
    ELSE IF (ci == 14) THEN
             sFlg = ibset(sFlg, fBgPairsMax)
             sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (ci == 15) THEN
             sFlg = ibset(sFlg, fBgPairsSum)
             sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (ci == 16) THEN
             sFlg = ibset(sFlg, fBgPairsSum)
             sFlg = ibset(sFlg, fWgPairsSum)
    ELSE IF (ci == 17) THEN
             sFlg = ibset(sFlg, fBgPairsSum)
             sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (ci == 18) THEN
             sFlg = ibset(sFlg, fBgPairsBary)
             sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (ci == 19) THEN
             sFlg = ibset(sFlg, fBgPairsBary)
             sFlg = ibset(sFlg, fWgPairsSum)
    ELSE IF (ci == 20) THEN
             sFlg = ibset(sFlg, fBgPairsBary)
             sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (ci == 21) THEN
             sFlg = ibset(sFlg, fWgPtsBarySum)
             sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (ci == 22) THEN
             sFlg = ibset(sFlg, fWgPtsBarySum)
             sFlg = ibset(sFlg, fWgPairsSum)
    ELSE IF (ci == 23) THEN
             sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (ci == 26) THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (ci == 27) THEN
       sFlg = ibset(sFlg, fWgPairsSum)
       sFlg = ibset(sFlg, fBgPairsSum)
   ELSE IF (ci == 28) THEN
       sFlg = ibset(sFlg, fBgPairsBary)
       sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (ci == 29) THEN
       sFlg = ibset(sFlg, fWgPairsSum)
       sFlg = ibset(sFlg, fBgPairsSum)
    ELSE IF (ci == 31) THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
       sFlg = ibset(sFlg, fBgPairsBary)
    ELSE IF (ci == 33) THEN
       sFlg = ibset(sFlg, fWgKMat)
    ELSE IF (ci == 34) THEN
        sFlg = ibset(sFlg, fBgPairsBary)
    ELSE IF (ci == 36) THEN
        sFlg = ibset(sFlg, fPtClDist)
    ELSE IF (ci == 37) THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF (ci == 38) THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (ci == 41) THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
       sFlg = ibset(sFlg, fBgPairsMin)
    END IF
    
END SUBROUTINE cluc_int_set_flags



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_int_precalc(x,p,e)" --
! 
!       x	in		data matrix (of size nr x nc)
!       p	in		partition vector (of length nr)
!       e	out		error code (0 for no error)
! 
! Precalculate the required quantities depending on the flags set during
! the first pass.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_int_precalc(x,p,e)
    use critUtils

    IMPLICIT NONE
    double precision, intent(in), dimension(sNr,sNc) :: x
    integer, intent(in), dimension(sNr) :: p
    integer, intent(out) :: e
    
    e = 0
    call cluc_alloc_arrays(p,e)
    IF (e /= 0) THEN
       e = 3
    ELSE
       call cluc_inter_bary_distances(x,p,2,e)
       IF (e == 0) THEN
          call cluc_points_bary_distances(x,p,2,e)
       END IF
       IF (e == 0) THEN
          call cluc_pairwise_distances(x,p,2,e)
       END IF
       IF (e == 0) THEN
          call cluc_group_wg_matrix(x,p)
       END IF
    END IF
    
END SUBROUTINE cluc_int_precalc



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_ext_criterion(p1,p2,ci,e,v)" --
! 
!       p1	in		first partition vector (of length nr)
!       p2	in		second partition vector (of length nr)
!       cl	in		criterion index
!       e	out		error code (0 for no error)
!       v	out		criterion value
! 
! Dispatching subroutine.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_ext_criterion(p1,p2,ci,e,v)
    use indices
!     use timer

    IMPLICIT NONE
    integer, intent(in), dimension(sNr) :: p1, p2
    integer, intent(in) :: ci
    integer, intent(out) :: e
    double precision, intent(out) :: v
    integer :: i, rep

    v = 0.0
    rep = 1
    e = 0
   
!     IF (sTiming) THEN
!        call startTimer
!        rep = 100000
!     END IF
 
    DO i=1,rep
       IF (ci == 0) THEN
          call cluc_crit_czekanowski_dice(p1,p2,v)
       ELSE IF (ci == 1) THEN
          call cluc_crit_folkes_mallows(p1,p2,v)
       ELSE IF (ci == 2) THEN
          call cluc_crit_hubert(p1,p2,v)
       ELSE IF (ci == 3) THEN
          call cluc_crit_jaccard(p1,p2,v)
       ELSE IF (ci == 4) THEN
          call cluc_crit_kulczynski(p1,p2,v) 
       ELSE IF (ci == 5) THEN
          call cluc_crit_mcnemar(p1,p2,v)
       ELSE IF (ci == 6) THEN
          call cluc_crit_phi(p1,p2,v)
       ELSE IF (ci == 7) THEN
          call cluc_crit_precision(p1,p2,v) 
       ELSE IF (ci == 8) THEN
          call cluc_crit_rand(p1,p2,v)
       ELSE IF (ci == 9) THEN
          call cluc_crit_recall(p1,p2,v) 
       ELSE IF (ci == 10) THEN
          call cluc_crit_rogers_tanimoto(p1,p2,v)
       ELSE IF (ci == 11) THEN
          call cluc_crit_russel_rao(p1,p2,v)
       ELSE IF (ci == 12) THEN
          call cluc_crit_sokal_sneath1(p1,p2,v)
       ELSE IF (ci == 13) THEN
          call cluc_crit_sokal_sneath2(p1,p2,v)
       ELSE
          e = 1
       END IF
    END DO

!    IF (sTiming) THEN
!        call stopTimer
!        call logTiming(cn)
!     END IF
    
END SUBROUTINE cluc_calc_ext_criterion



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_int_start(nr,nc,nk)" --
! 
! Initialization for internal indices.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_int_start (nr,nc,nk)
    use critUtils
   
    IMPLICIT NONE
    integer, intent(in) :: nr, nc, nk
 
    call cluc_crit_int_init(nr,nc,nk)
END SUBROUTINE  cluc_calc_int_start



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_int_end()" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_int_end ()
    use critUtils
    call cluc_crit_int_dispose()
END SUBROUTINE  cluc_calc_int_end



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_ext_start(nr,nk1,nk2)" --
! 
! Initialization for external indices.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_ext_start (nr,nk1,nk2)
    use critUtils
   
    IMPLICIT NONE
    integer, intent(in) :: nr, nk1, nk2
 
    call cluc_crit_ext_init(nr,nk1,nk2)
END SUBROUTINE  cluc_calc_ext_start


! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_ext_end()" --
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_ext_end ()
    use critUtils
    call cluc_crit_ext_dispose()
END SUBROUTINE  cluc_calc_ext_end



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_count_clusters(p,n,r)" --
! 
!       p	in		partition vector
!       r	out		number of clusters
! 
! It is guaranteed (after renumbering in main.R) that p contains all the integer
! values in some interval 1:r, so that the number of values is precisely r.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_count_clusters(p,n,r)
   IMPLICIT NONE
   
   integer, intent(in) :: n
   integer, intent(in), dimension(n) :: p
   integer, intent(out) :: r
   
   r = maxval(p)
   
END SUBROUTINE  cluc_count_clusters



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_concordance(p1,p2,n,m)" --
! 
!       p1	in		first partition vector
!       p2	in		second partition vector
!       n	in		length of vectors p1 and p2
!       m	out		concordance matrix
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_concordance(p1,p2,n,m)
   use critUtils 

   IMPLICIT NONE
   
   integer, intent(in), dimension(sNr) :: p1, p2
   integer, intent(in) :: n
   integer, intent(out), dimension(2,2) :: m
   
   call cluc_cross_counts(p1,p2,n)
   m = sNTb
   
END SUBROUTINE  cluc_calc_concordance


