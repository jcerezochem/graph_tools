program har_dist


    !==============================================================
    ! This code uses of MOLECULAR_TOOLS (version 1.0/March 2012)
    !==============================================================
    !
    ! Description:
    ! -----------
    ! Program to compute the std. deviation of the density distribution
    ! according to a Bolzmann sampling (varB) or to the quantum density
    ! according to the spatial part of the Wigner distribution (varQ).
    ! The results are the sigmas in dimensionless units
    !
    ! Change log:
    !
    ! TODO:
    ! ------
    !
    ! History
    !
    !============================================================================    

!*****************
!   MODULE LOAD
!*****************
!============================================
!   Generic (structure_types independent)
!============================================
!    use constants

    implicit none

    real(8),parameter :: &
                           HARTtoJ   = 4.3597482d-18,   &
                           boltz   = 1.3806488D-23,     &
                           autown    = 2.1947463068d5    !From FCclasses Freq from AU to cm-1

    real(8) :: KbT, T, g1, varB, varQ, freq
    integer :: Nvib, i

    !===================
    !CPU time 
    real(8) :: ti, tf
    !===================

! (End of variables declaration) 
!==================================================================================

    ! 0. GET COMMAND LINE ARGUMENTS
    read(*,*) T
    read(*,*) Nvib
    
    KbT=boltz*T / HARTtoJ
    do i=1,Nvib
        read(*,*) g1
        freq=g1
        g1 = g1 / autown
        varQ=dsqrt( 1.d0/(2.d0*dtanh(g1/2.d0/KbT)) )
        varB=dsqrt(KbT/g1)
        print*, i, varB, varQ
    enddo



    stop       

end program har_dist

! Try from SI to AU: 
!     do i=1,Nvib
!         read(*,*) g1
!         !Factor to adim (SI)
!         Factor = dsqrt(dabs(g1)*1.d2*clight*2.d0*PI/plankbar)
!         !====
!         g1 = g1 / autown
! !         varQ=1.d0/(dtanh(0.5d0*g1/KbT)*2.d0*g1)
!         varQ=1.d0/(dtanh(0.5d0*g1/KbT)*2.d0*dsqrt(KbT))
!         varB=dsqrt(KbT)/g1
!         !To SI
!         varQ=varQ*BOHRtoM*dsqrt(AUtoKG)
!         varB=varB*BOHRtoM*dsqrt(AUtoKG)
!         varQ=varQ*Factor
!         varB=varB*Factor
!         print*, i, varQ, varB
!     enddo

