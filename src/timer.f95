! ===========================================================================
! File: "timer.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2010-05-26 14:46:20
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================



MODULE timer
   IMPLICIT NONE
   
      ! Array for start values
      integer, dimension(8), save :: sv
      ! Array for end values
      integer, dimension(8), save :: ev      
      
   contains
   
   SUBROUTINE startTimer ()      
      call date_and_time(VALUES=sv)
   END SUBROUTINE 
   
   
   SUBROUTINE stopTimer ()
      call date_and_time(VALUES=ev)
   END SUBROUTINE 
   
   
   SUBROUTINE getTiming (v)
      integer, intent(out), dimension(8) :: v
      v = ev-sv      
   END SUBROUTINE    

   
   SUBROUTINE printTiming ()
      integer :: ms

      ms = (ev(6)-sv(6))*60*1000 + (ev(7)-sv(7))*1000 + (ev(8)-sv(8))
      write(*,'(i12,a)') ms, " ms"
      
   END SUBROUTINE    

   
   ! Log the timing with a message 'm'
   SUBROUTINE logTiming (m)
      use logFile
      character (len=*), intent(in) :: m
      character (15) :: str
      integer :: ms

      ms = (ev(6)-sv(6))*60*1000 + (ev(7)-sv(7))*1000 + (ev(8)-sv(8))
      
      call logMsg(m)
      write(str,'(i12,a)') ms, " ms"
      call logMsg(str)
      
   END SUBROUTINE    

END MODULE 
