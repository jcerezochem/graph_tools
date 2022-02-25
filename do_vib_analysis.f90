program do_vib_analysis

    !Para ser compatible con compute_gauss se lee la amplitud (A), aunque no se usa

    implicit double precision(A-H,O-Z)
    implicit integer(I-N)

    character(len=50) :: arg
    character(len=100) :: line
    character(len=2) :: units
    logical :: argument_retrieved

    double precision,dimension(1:100000) :: freq, rint, rintX, rintX2
    integer,dimension(1:100000) :: iord

    !Default input units
    units="ev"
    !Read binwidth from label
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-units")
                call getarg(i+1, units)
                argument_retrieved=.true.

            case default
                print*, "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo

    !Say the units
    if (units ==  "cm") then
        factor=1.d0
    else
        units="eV"
        factor=1.23981d-4
    endif
    print*, "Assuming spectra with frequency in "//units

    !Read data
    rint=0.d0
    n=1
    do
        read(5,'(A)',iostat=ios) line
        if (trim(line) == "") exit
        if (      index(line,'#') /=0        &
             .or. index(line,'&') /=0        &
!             .or. trim(adjustl(line)) == ""  &
            ) cycle
        if (ios /= 0) exit
        read(line,*) freq(n), rint(n)
        freq(n) = freq(n) / factor
        rintmax = max(rintmax,rint(n))
        if (rint(n) == rintmax) then
            freqmax=freq(n)
            nmax=n
        endif
        n=n+1
    enddo
    n=n-1

    !order in energy
    call sort_vec(freq,iord,n)
    call reorder_vec(rint,iord,n)

    !Locate edges for FWHM
    ! on the left (shuld be an extrapolation...)
    do i=1,n
        rintref=rint(i)
        if (rintref > rintmax/2.d0) then
            freqleft = (rintmax/2.d0-rint(i))/(rint(i-1)-rint(i))*(freq(i-1)-freq(i))+freq(i)
            exit
        endif
    enddo
    ! on the right (shuld be an extrapolation...)
    do i=n,1,-1
        rintref=rint(i)
        if (rintref > rintmax/2.d0) then
            freqright = (rintmax/2.d0-rint(i))/(rint(i+1)-rint(i))*(freq(i+1)-freq(i))+freq(i)
            exit
        endif
    enddo
    fwhm=freqright-freqleft

    !Locate edges for FWHM (grace: start in the max and go down)
    ! on the left (extrapolation between two points were rintmax/2 crosses)
    do i=nmax,1,-1
        rintref=rint(i)
        if (rintref < rintmax/2.d0) then
            freqleft = (rintmax/2.d0-rint(i))/(rint(i+1)-rint(i))*(freq(i+1)-freq(i))+freq(i)
            exit
        endif
    enddo
    ! on the right (extrapolation between two points were rintmax/2 crosses)
    do i=nmax,n
        rintref=rint(i)
        if (rintref < rintmax/2.d0) then
            freqright = (rintmax/2.d0-rint(i))/(rint(i-1)-rint(i))*(freq(i-1)-freq(i))+freq(i)
            exit
        endif
    enddo
    fwhm_gr=freqright-freqleft

!    df=freq(3)-freq(2)
    !Compute area
!    area=rint(1)+rint(n)
    do i=1,n-1
        area = area + (rint(i)+rint(i+1))*(freq(i+1)-freq(i))/2.d0
    enddo
!    area=area/2.d0 * df
    !Normalize
    do i=1,n
        rint(i) = rint(i)/area
    enddo

    !Compute first moment
    do i=1,n
        rintX(i) = rint(i)*freq(i)
    enddo
    ! integrate
    do i=1,n-1
       RM1 = RM1 + (rintX(i)+rintX(i+1))*(freq(i+1)-freq(i))/2.d0
    enddo
!    RM1=rintX(1)+rintX(n)
!    do i=2,n-1
!        RM1 = RM1 + 2.d0*rintX(i)
!    enddo
!    RM1=RM1/2.d0 * df

    !Compute second moment
    do i=1,n
        rintX2(i) = rintX(i)*freq(i)
    enddo
    ! integrate
    do i=1,n-1
       RM2 = RM2 + (rintX2(i)+rintX2(i+1))*(freq(i+1)-freq(i))/2.d0
    enddo
!    RM2=rintX2(1)+rintX2(n)
!    do i=2,n-1
!        RM2 = RM2 + 2.d0*rintX2(i)
!    enddo
!    RM2=RM2/2.d0 * df

    !Compute sigma
    sigma = dsqrt(RM2-RM1**2)

    factor=1.23981d-4
    print*, "DATA IN eV"
    print*, " M1         sigma      wmax      FWHM(grace)  FWHM(alter)"
    print*, "-----------------------------------------------------"
    print'(5(F8.4,3X))', RM1*factor, sigma*factor, freqmax*factor, fwhm_gr*factor, fwhm*factor
    print*, ""

    print*, "DATA IN cm-1"
    print*, " M1         sigma      wmax      FWHM(grace)  FWHM(alter)"
    print*, "-----------------------------------------------------"
    print'(5(F8.1,3X))', RM1, sigma, freqmax, fwhm_gr, fwhm

!    do i=1,n
!        write(0,*) freq(i), rint(i)
!    enddo

    stop

    contains

    subroutine sort_vec(V,IORD,N)

        !Order from min to max and track indices in IORD matrix

        implicit none

        real(8),dimension(:),intent(inout) :: V
        real(8) :: aux
        integer,dimension(:),intent(inout) :: IORD
        integer,intent(in) :: N
        integer :: i,j, iaux

        !Intialize IORD
        do i=1,N
            IORD(i) = i
        enddo

        do i=1,N-1
            do j=i+1,N
                if (V(j)<V(i)) then
                    aux=V(i)
                    V(i) = V(j)
                    V(j) = aux
                    !Track the index permutations in IORD
                    iaux = IORD(i)
                    IORD(i) = IORD(j)
                    IORD(j) = iaux
                endif
            enddo
        enddo

        return

    end subroutine sort_vec   
     
    subroutine reorder_vec(V,IORD,N)

        !Reorder vector V with info in IORD 

        implicit none

        real(8),dimension(:),intent(inout) :: V
        integer,dimension(:),intent(in)    :: IORD
        integer,intent(in) :: N
        real(8),dimension(size(V)) :: aux
        integer :: i,j, iaux

        aux=V
        !Intialize IORD
        do i=1,N
            V(i) = aux(IORD(i))
        enddo

        return

    end subroutine reorder_vec        

end program
