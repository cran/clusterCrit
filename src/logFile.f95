! ===========================================================================
! File: "logFile.f95"
!                        Created: 2010-04-21 12:11:29
!              Last modification: 2010-05-21 09:25:34
! Author: Bernard Desgraupes
! e-mail: <bernard.desgraupes@u-paris10.fr>
! This is part of the R package 'clusterCrit'.
! ===========================================================================



MODULE logFile
   IMPLICIT NONE
   use, intrinsic :: iso_fortran_env, only : int8
   
      ! Path to logs folder
      character (len=*), parameter :: logDir = "/Users/bernardo/Library/Logs/clusterCrit/"

      ! 'baseName' can be changed with the setLogName() routine
      character (len=32), save :: baseName = "cluscrit"
      
      integer, parameter :: logUnit = 1
      
      ! Declaration of generic procedure
      INTERFACE logValue
         module procedure logBool, logInt, logReal, logDouble
      END INTERFACE 
      
      INTERFACE logArray
         module procedure logBoolArr, logIntArr, logRealArr, logDoubleArr
      END INTERFACE 
      
      INTERFACE logMatrix
         module procedure logBoolMat, logIntMat, logRealMat, logDoubleMat
      END INTERFACE 
      
      
   contains
   
   !! Log a string
   !! ------------
   SUBROUTINE logMsg (m)
      character (len=*), intent(in) :: m

      call openLogFile
      write(logUnit, fmt='(a)') m      
      call closeLogFile
      
   END SUBROUTINE logMsg
   
   
   SUBROUTINE logSep ()
      call openLogFile
      write(logUnit, fmt='(a)') "----------------------------------------"      
      call closeLogFile
      
   END SUBROUTINE logSep
   
   
   !! Print date&time
   !! ---------------
   SUBROUTINE logDate ()
      integer, dimension(8) :: v
      
      call date_and_time(VALUES=v)
      call openLogFile
      write(logUnit, fmt='("--- ",i4,"-",i2.2,"-",i2.2,1x,i2.2,":",i2.2,":",i2.2," ---")') &
            v(1),v(2),v(3),v(5),v(6),v(7)
      call closeLogFile
      
   END SUBROUTINE 
   
   
   !! Log numeric value
   !! -----------------
   SUBROUTINE logBool (x)
      logical, intent(in) :: x

      call openLogFile
      write(logUnit, fmt='(L2)') x
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logInt (x)
      integer, intent(in) :: x

      call openLogFile
      write(logUnit, fmt='(i12)') x
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logLongInt (x)
      integer(kind=int8), intent(in) :: x

      call openLogFile
      write(logUnit, fmt='(i16)') x
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logReal (x)
      real, intent(in) :: x

      call openLogFile
      write(logUnit, fmt='(f12.4)') x
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logDouble (x)
      double precision, intent(in) :: x

      call openLogFile
      write(logUnit, fmt='(d12.4)') x
      call closeLogFile
      
   END SUBROUTINE 
   
   
   !! Log numeric vector
   !! ------------------
   !! The 'lf' argument indicates if a new line should inserted after each value
   !! in order to print them as a column. It is false by default, i-e all the
   !! values of the vector are printed on a single line.
   
   SUBROUTINE logBoolArr (v,lf)
      logical, dimension(:), intent(in) :: v
      logical, intent(in), optional :: lf
      integer :: i, len
      logical :: nl

      IF (present(lf)) THEN; nl = lf; ELSE; nl = .false.; END IF
      len = size(v)
      call openLogFile
      DO i=1,len
         IF (i.lt.len .and. .not.nl) THEN
            write(logUnit, fmt='(L2)', advance='no') v(i)
         ELSE
            write(logUnit, fmt='(L2)') v(i)
         END IF
      END DO
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logIntArr (v,lf)
      integer, dimension(:), intent(in) :: v
      logical, intent(in), optional :: lf
      integer :: i, len
      logical :: nl

      IF (present(lf)) THEN; nl = lf; ELSE; nl = .false.; END IF

      len = size(v)
      call openLogFile
      DO i=1,len
         IF (i.lt.len .and. .not.nl) THEN
            write(logUnit, fmt='(i12)', advance='no') v(i)
         ELSE
            write(logUnit, fmt='(i12)') v(i)
         END IF
      END DO
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logRealArr (v,lf)
      real, dimension(:), intent(in) :: v
      logical, intent(in), optional :: lf
      integer :: i, len
      logical :: nl

      IF (present(lf)) THEN; nl = lf; ELSE; nl = .false.; END IF

      len = size(v)
      call openLogFile
      DO i=1,len
         IF (i.lt.len .and. .not.nl) THEN
            write(logUnit, fmt='(f12.4)', advance='no') v(i)
         ELSE
            write(logUnit, fmt='(f12.4)') v(i)
         END IF
      END DO
      call closeLogFile
      
   END SUBROUTINE 
   
   
   SUBROUTINE logDoubleArr (v,lf)
      double precision, dimension(:), intent(in) :: v
      logical, intent(in), optional :: lf
      integer :: i, len
      logical :: nl

      IF (present(lf)) THEN; nl = lf; ELSE; nl = .false.; END IF

      len = size(v)
      call openLogFile
      DO i=1,len
         IF (i.lt.len .and. .not.nl) THEN
            write(logUnit, fmt='(d12.4)', advance='no') v(i)
         ELSE
            write(logUnit, fmt='(d12.4)') v(i)
         END IF
      END DO
      call closeLogFile
      
   END SUBROUTINE 
   
   
   !! Log numeric matrix
   !! ------------------
   SUBROUTINE logBoolMat (m)
      logical, dimension(:,:), intent(in) :: m
      integer :: i, j, nr, nc

      nr = size(m,1)
      nc = size(m,2)
      call openLogFile
      DO i=1,nr
         DO j=1,nc
            IF (j==nc) THEN
               write(logUnit, fmt='(L2)', advance='yes') m(i,j)
            ELSE
               write(logUnit, fmt='(L2)', advance='no') m(i,j)
            END IF
         END DO
      END DO
      call closeLogFile
      
   END SUBROUTINE 

   
   
   SUBROUTINE logIntMat (m)
      integer, dimension(:,:), intent(in) :: m
      integer :: i, j, nr, nc

      nr = size(m,1)
      nc = size(m,2)
      call openLogFile
      DO i=1,nr
         DO j=1,nc
            IF (j==nc) THEN
               write(logUnit, fmt='(i12)', advance='yes') m(i,j)
            ELSE
               write(logUnit, fmt='(i12)', advance='no') m(i,j)
            END IF
         END DO
      END DO
      call closeLogFile
      
   END SUBROUTINE 

   
   
   SUBROUTINE logRealMat (m)
      real, dimension(:,:), intent(in) :: m
      integer :: i, j, nr, nc

      nr = size(m,1)
      nc = size(m,2)
      call openLogFile
      DO i=1,nr
         DO j=1,nc
            IF (j==nc) THEN
               write(logUnit, fmt='(f12.4)', advance='yes') m(i,j)
            ELSE
               write(logUnit, fmt='(f12.4)', advance='no') m(i,j)
            END IF
         END DO
      END DO
      call closeLogFile
      
   END SUBROUTINE 

   
   
   SUBROUTINE logDoubleMat (m)
      double precision, dimension(:,:), intent(in) :: m
      integer :: i, j, nr, nc

      nr = size(m,1)
      nc = size(m,2)
      call openLogFile
      DO i=1,nr
         DO j=1,nc
            IF (j==nc) THEN
               write(logUnit, fmt='(d12.4)', advance='yes') m(i,j)
            ELSE
               write(logUnit, fmt='(d12.4)', advance='no') m(i,j)
            END IF
         END DO
      END DO
      call closeLogFile
      
   END SUBROUTINE 

   
   
   ! Log file functions
   ! ------------------
   SUBROUTINE openLogFile ()
      character (len=128) :: logPath
      character(3)  :: fstat

      call getLogPath(logPath)
      
      IF ( .not. file_exist(logPath) ) THEN
         fstat = 'new'
      ELSE
         fstat = 'old'
      END IF
      open(unit=logUnit, file=logPath, access='sequential', form='formatted', &
          position='append', status = fstat)

   END SUBROUTINE 
   
   
   SUBROUTINE closeLogFile ()
     close(logUnit)
   END SUBROUTINE 
   
   
   SUBROUTINE editLogFile ()
      character (len=128) :: logPath

      call getLogPath(logPath)
      call system( "aed "//logPath )
!       call system( "open -a /Applications/0xED.app "//logPath )

   END SUBROUTINE 
   
   
   ! Utility functions
   ! -----------------
   FUNCTION file_exist (path)
      implicit none

      character ( len = * ), intent(in) :: path
      logical :: file_exist

      inquire ( file = path, exist = file_exist )
      return
   END FUNCTION file_exist

   
   SUBROUTINE getLogPath (path)
      character (len=*), intent(out) :: path
      character(8)  :: date
      
      IF ( .not. file_exist(logDir) ) THEN
         call system( "mkdir "//logDir )
      END IF
   
      call date_and_time(DATE=date)
      path = logDir//trim(baseName)//"_"//date//".log"
      
   END SUBROUTINE 
   

   SUBROUTINE setLogName (name)
      character (len=*), intent(in) :: name
      
      ! Change the base name
      baseName = name
   END SUBROUTINE 
   

END MODULE 
