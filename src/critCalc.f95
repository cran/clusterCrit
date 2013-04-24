! ===========================================================================
! File: "critCalc.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2013-04-24 17:05:36
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================
! Subroutines called from the C code. Not stored in a module.
! 
! ERROR CODES
! -----------
!    cf cluc_errorMsg()

! Note on strings comparison (cf. Fortran 90 Handbook, section 7.3.1.2):
!     << When the operands are both of type character, the shorter one is padded
!     on the right with blank padding characters until the operands are of equal
!     length. Then, the operands are compared one character at a time in order,
!     starting from the leftmost character of each operand until the
!     corresponding characters differ. >>

! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_int_criterion(x,p,cl,cn,e,v)" --
! 
!       x	in		data matrix (of size nr x nc)
!       p	in		partition vector (of length nr)
!       cl	in		criterion length
!       cn	in		criterion name
!       e	out		error code (0 for no error)
!       v	out		criterion value
! 
! Dispatching subroutine.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_int_criterion(x,p,cl,cn,e,v)
    use indices
! use timer
! use logFile
    IMPLICIT NONE
    double precision, intent(in), dimension(sNr,sNc) :: x
    integer, intent(in), dimension(sNr) :: p
    integer, intent(in) :: cl
    character (len=32), intent(in) :: cn
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
       IF (cn == "ball_hall") THEN
          call cluc_crit_ball_hall(x,p,v)
       ELSE IF (cn == "banfeld_raftery") THEN
          call cluc_crit_banfeld_raftery(x,p,v)
       ELSE IF (cn == "c_index") THEN
          call cluc_crit_c_index(x,p,v)
      ELSE IF (cn == "calinski_harabasz") THEN
          call cluc_crit_calinski_harabasz(x,p,v)
       ELSE IF (cn == "davies_bouldin") THEN
          call cluc_crit_davies_bouldin(x,p,v)
       ELSE IF (cn == "det_ratio") THEN
          call cluc_crit_det_ratio(x,p,v)
       ELSE IF (cn == "dunn") THEN
          call cluc_crit_dunn(x,p,v)
       ELSE IF (cn == "gamma") THEN
          call cluc_crit_gamma(x,p,v)
       ELSE IF (cn == "g_plus") THEN
          call cluc_crit_g_plus(x,p,v)
      ELSE IF ( index(cn,"gdi") == 1 ) THEN
          IF (cl == 3) THEN
             call cluc_crit_gdi(x,p,1,1,e,v)
          ELSE IF (cl == 5) THEN
             call cluc_crit_gdi(x,p,iachar(cn(4:4))-48,iachar(cn(5:5))-48,e,v)
          ELSE
             e = 2
          END IF
       ELSE IF (cn == "ksq_detw") THEN
          call cluc_crit_ksq_detw(x,p,v)
       ELSE IF (cn == "log_det_ratio") THEN
          call cluc_crit_log_det_ratio(x,p,v)
       ELSE IF (cn == "log_ss_ratio") THEN
          call cluc_crit_log_ss_ratio(x,p,v)
       ELSE IF (cn == "mcclain_rao") THEN
          call cluc_crit_mcclain_rao(x,p,v)
       ELSE IF (cn == "pbm") THEN
          call cluc_crit_pbm(x,p,v)
       ELSE IF (cn == "point_biserial") THEN
          call cluc_crit_point_biserial(x,p,v)
       ELSE IF (cn == "ratkowsky_lance") THEN
          call cluc_crit_ratkowsky_lance(x,p,v)
       ELSE IF (cn == "ray_turi") THEN
          call cluc_crit_ray_turi(x,p,v)
       ELSE IF (cn == "scott_symons") THEN
          call cluc_crit_scott_symons(x,p,v)
       ELSE IF (cn == "sd_dis") THEN
          call cluc_crit_sd_dis(x,p,v)
       ELSE IF (cn == "sd_scat") THEN
          call cluc_crit_sd_scat(x,p,v)
       ELSE IF (cn == "s_dbw") THEN
          call cluc_crit_s_dbw(x,p,v)
       ELSE IF (cn == "silhouette") THEN
          call cluc_crit_silhouette(x,p,v)
       ELSE IF (cn == "tau") THEN
          call cluc_crit_tau(x,p,v)
       ELSE IF (cn == "trace_w") THEN
          call cluc_crit_trace_w(x,p,v)
       ELSE IF (cn == "trace_wib") THEN
          call cluc_crit_trace_wib(x,p,v)
       ELSE IF (cn == "wemmert_gancarski") THEN
          call cluc_crit_wemmert_gancarski(x,p,v)
       ELSE IF (cn == "xie_beni") THEN
          call cluc_crit_xie_beni(x,p,v)
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
! "SUBROUTINE cluc_int_set_flags(cl,cn)" --
! 
!       cl	in		criterion length
!       cn	in		criterion name
! 
! Set the flags to determine which quantities will have to be calculated.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_int_set_flags(cl,cn)
    use critUtils 
! use logFile
    IMPLICIT NONE
    integer, intent(in) :: cl
    character (len=32), intent(in) :: cn
    integer :: i, c1, c2
     
    IF (cn == "ball_hall") THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (cn == "banfeld_raftery") THEN
       sFlg = ibset(sFlg, fWgKMat)
    ELSE IF (cn == "c_index") THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF (cn == "calinski_harabasz") THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (cn == "davies_bouldin") THEN
       sFlg = ibset(sFlg, fWgPtsBarySum)
       sFlg = ibset(sFlg, fBgPairsBary)
    ELSE IF (cn == "dunn") THEN
       sFlg = ibset(sFlg, fBgPairsMin)
       sFlg = ibset(sFlg, fWgPairsMax)
    ELSE IF (cn == "gamma") THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF (cn == "g_plus") THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF ( index(cn,"gdi") == 1 ) THEN
       IF (cl == 5) THEN
          c1 = iachar(cn(4:4)) - 48
          c2 = iachar(cn(5:5)) - 48
       ELSE
          c1 = 1
          c2 = 1
       END IF

       !! First integer is the between-group distance (1-5)
       SELECT CASE (c1)
          CASE(1)
             sFlg = ibset(sFlg, fBgPairsMin)
          CASE(2)
             sFlg = ibset(sFlg, fBgPairsMax)
          CASE(3)
             sFlg = ibset(sFlg, fBgPairsSum)
          CASE(4)
             sFlg = ibset(sFlg, fBgPairsBary)
          CASE(5)
             sFlg = ibset(sFlg, fWgPtsBarySum)
       END SELECT
       
       !! Second integer is the within-group distance (1-3)
       SELECT CASE (c2)
          CASE(1)
             sFlg = ibset(sFlg, fWgPairsMax)
          CASE(2)
             sFlg = ibset(sFlg, fWgPairsSum)
          CASE(3)
             sFlg = ibset(sFlg, fWgPtsBarySum)
       END SELECT
       
    ELSE IF (cn == "log_ss_ratio") THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (cn == "mcclain_rao") THEN
       sFlg = ibset(sFlg, fWgPairsSum)
       sFlg = ibset(sFlg, fBgPairsSum)
   ELSE IF (cn == "pbm") THEN
       sFlg = ibset(sFlg, fBgPairsBary)
       sFlg = ibset(sFlg, fWgPtsBarySum)
    ELSE IF (cn == "point_biserial") THEN
       sFlg = ibset(sFlg, fWgPairsSum)
       sFlg = ibset(sFlg, fBgPairsSum)
    ELSE IF (cn == "ray_turi") THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
       sFlg = ibset(sFlg, fBgPairsBary)
    ELSE IF (cn == "scott_symons") THEN
       sFlg = ibset(sFlg, fWgKMat)
    ELSE IF (cn == "sd_dis") THEN
        sFlg = ibset(sFlg, fBgPairsBary)
    ELSE IF (cn == "silhouette") THEN
        sFlg = ibset(sFlg, fPtClDist)
    ELSE IF (cn == "tau") THEN
       sFlg = ibset(sFlg, fPairsDist)
    ELSE IF (cn == "trace_w") THEN
       sFlg = ibset(sFlg, fWgPtsBarySumPow)
    ELSE IF (cn == "xie_beni") THEN
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
       call cluc_inter_bary_distances(x,p,2.0,e)
       IF (e == 0) THEN
          call cluc_points_bary_distances(x,p,2.0,e)
       END IF
       IF (e == 0) THEN
          call cluc_pairwise_distances(x,p,2.0,e)
       END IF
       IF (e == 0) THEN
          call cluc_group_wg_matrix(x,p)
       END IF
    END IF
    
END SUBROUTINE cluc_int_precalc



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_calc_ext_criterion(p1,p2,cl,cn,e,v)" --
! 
!       p1	in		first partition vector (of length nr)
!       p2	in		second partition vector (of length nr)
!       cl	in		criterion length
!       cn	in		criterion name
!       e	out		error code (0 for no error)
!       v	out		criterion value
! 
! Dispatching subroutine.
! 
! ---------------------------------------------------------------------------

SUBROUTINE cluc_calc_ext_criterion(p1,p2,cl,cn,e,v)
    use indices
!     use timer

    IMPLICIT NONE
    integer, intent(in), dimension(sNr) :: p1, p2
    integer, intent(in) :: cl
    character (len=32), intent(in) :: cn
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
       IF (cn == "czekanowski_dice") THEN
          call cluc_crit_czekanowski_dice(p1,p2,v)
       ELSE IF (cn == "folkes_mallows") THEN
          call cluc_crit_folkes_mallows(p1,p2,v)
       ELSE IF (cn == "hubert") THEN
          call cluc_crit_hubert(p1,p2,v)
       ELSE IF (cn == "jaccard") THEN
          call cluc_crit_jaccard(p1,p2,v)
       ELSE IF (cn == "kulczynski") THEN
          call cluc_crit_kulczynski(p1,p2,v) 
       ELSE IF (cn == "mcnemar") THEN
          call cluc_crit_mcnemar(p1,p2,v)
       ELSE IF (cn == "phi") THEN
          call cluc_crit_phi(p1,p2,v)
       ELSE IF (cn == "precision") THEN
          call cluc_crit_precision(p1,p2,v) 
       ELSE IF (cn == "rand") THEN
          call cluc_crit_rand(p1,p2,v)
       ELSE IF (cn == "recall") THEN
          call cluc_crit_recall(p1,p2,v) 
       ELSE IF (cn == "rogers_tanimoto") THEN
          call cluc_crit_rogers_tanimoto(p1,p2,v)
       ELSE IF (cn == "russel_rao") THEN
          call cluc_crit_russel_rao(p1,p2,v)
       ELSE IF (cn == "sokal_sneath1") THEN
          call cluc_crit_sokal_sneath1(p1,p2,v)
       ELSE IF (cn == "sokal_sneath2") THEN
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



SUBROUTINE cluc_calc_ext_end ()
    use critUtils
    call cluc_crit_ext_dispose()
END SUBROUTINE  cluc_calc_ext_end



! ---------------------------------------------------------------------------
! 
! "SUBROUTINE cluc_count_clusters(p,n,r)" --
! 
!       p	in		partition vector
!       n	in		length of vector
!       r	out		number of clusters
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


