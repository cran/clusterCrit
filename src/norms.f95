! ===========================================================================
! File: "norms.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2010-04-24 11:27:24
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================


MODULE norms
   
   CONTAINS
   
   FUNCTION cluc_norm_euclid(x)
       IMPLICIT NONE
       double precision, intent(in), dimension(:) :: x
       double precision :: cluc_norm_euclid
       integer :: n
       
       n = size(x)
       cluc_norm_euclid = sqrt( sum( x(1:n)**2 ) )
       
   END FUNCTION  cluc_norm_euclid



   FUNCTION cluc_norm_inf(x)
       IMPLICIT NONE
       double precision, intent(in), dimension(:) :: x
       double precision :: cluc_norm_inf
       integer :: n
           
       n = size(x)
       cluc_norm_inf = maxval( abs(x(1:n)) )
       
   END FUNCTION  cluc_norm_inf



   ! ---------------------------------------------------------------------------
   ! 
   ! "FUNCTION cluc_norm_ln(x,n,s)" --
   ! 
   ! x		in		vector
   ! n		in		Ln norm. Must be > 0.
   ! s		in		optional logical value (default .true.)
   ! 
   ! This function calculates the sum of n powers of the coords. If the 's'
   ! argument is true, the result is scaled, i-e raised to the 1/n power. By
   ! convention, n == huge(n) corresponds to the L-infinite norm. 
   !
   ! The 's' argument is an optional. It is true by default.
   ! 
   ! ---------------------------------------------------------------------------

   FUNCTION cluc_norm_ln(x,n,s)
       IMPLICIT NONE
       double precision, intent(in), dimension(:) :: x
       real, intent(in) :: n
       logical, intent(in), optional :: s
       double precision :: cluc_norm_ln, v
       integer :: ln
       logical :: sc
       
       IF (present(s)) THEN; sc = s; ELSE; sc = .true.; END IF
       
       ln = size(x)
       if ( n == 1.0 ) then
           cluc_norm_ln = sum ( abs (x(1:ln)) )
       else if ( n == 2.0 ) then
            v =  sum ( x(1:ln)**2 ) 
           IF (sc) THEN
              cluc_norm_ln = sqrt(v)
           ELSE
              cluc_norm_ln = v
           END IF
        else if ( n == huge(n) ) then
           cluc_norm_ln = maxval ( abs (x(1:ln)) )
        else 
           v =  sum ( ( abs (x(1:ln)) )**n ) 
           IF (sc) THEN
              cluc_norm_ln = v**( 1.0/n )
           ELSE
              cluc_norm_ln = v
           END IF
        end if
        
   END FUNCTION cluc_norm_ln



   ! ---------------------------------------------------------------------------
   ! 
   ! "FUNCTION cluc_norm_scale(v,n)" --
   ! 
   ! v		in		unscaled norm
   ! n		in		Ln norm
   ! 
   ! Return the n-th root of a sum of n powers of the coords. The argument 'n'
   ! must be > 0 (no check is done here).
   !
   ! ---------------------------------------------------------------------------

   FUNCTION cluc_norm_scale(v,n)
       IMPLICIT NONE
       double precision, intent(in) :: v
       real, intent(in) :: n
       double precision :: cluc_norm_scale
        
       if ( n == 2.0 ) then
          cluc_norm_scale = sqrt(v)
       else if ( n == 1.0 .or. n == huge(n) ) then
           cluc_norm_scale = v
       else 
           cluc_norm_scale = v**( 1.0/n )
       end if
        
   END FUNCTION cluc_norm_scale



   FUNCTION cluc_dist_canberra(x,y)
       IMPLICIT NONE
       double precision, intent(in), dimension(:) :: x, y
       double precision :: cluc_dist_canberra
       integer :: ln
    
       ln = size(x)
       cluc_dist_canberra = sum ( ( abs (x(1:ln)-y(1:ln)) )/ ( abs(x(1:ln)) + abs(y(1:ln)) )  )

   END FUNCTION cluc_dist_canberra
     


   FUNCTION cluc_dist_binary(x,y)
       IMPLICIT NONE
       double precision, intent(in), dimension(:) :: x, y
       double precision :: cluc_dist_binary
       integer :: num, den, ln
    
       ln = size(x)
       num =  count((x(1:ln) /= 0.0) .and. (y(1:ln) /= 0.0))
       den =  count((x(1:ln) /= 0.0) .or. (y(1:ln) /= 0.0))
       cluc_dist_binary = real(num)/real(den)

   END FUNCTION cluc_dist_binary
  

   
END MODULE 

