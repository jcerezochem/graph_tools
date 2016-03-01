program average_plots

    ! Avergage spectra with weights

    implicit none

    character(len=200),dimension(100) :: filenames, filenames_bk
    real(8),dimension(100) :: yy,y, x, w
    real(8) :: a
    !Counters
    integer :: i,j,k
    integer :: ispc, iw, ispc_tot
    integer :: N, nadd
    !IO
    integer :: iostatus
    !Input selections stuff
    logical :: argument_retrieved, &
               do_weight=.false.   
    character(len=50) :: arg

    !Initialize
    nadd=0
    !Read data
    ispc=0
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-w")
                do_weight = .true.
                call getarg(i+1, arg)
                argument_retrieved=.true.
                open(10,file=arg,status='old',iostat=iostatus)
                if (iostatus /= 0) then
                    write(0,*) "ERROR: opening "//trim(adjustl(arg))
                    stop
                endif
                iw=0
                do 
                    read(10,*,iostat=iostatus) a
                    if (iostatus/=0) exit
                    iw=iw+1
                    w(iw) = a
                enddo
                close(10)
            case ("-add")
                argument_retrieved=.true.
                call getarg(i+1, arg)
                read(arg,*) nadd
            case ("-h")
                print*, "Program to average spectra"
                print*, "All plots need to be defined in the same time grid."
                print*, "Output spcetrum to stdout" 
                print*, "Options"
                print*, "  -w      File with weights"
                print*, "  -add    Number of additional points, which are"
                print*, "          obtained by linear extrapolation. It is"
                print*, "          assumed that the imput set is evenly spaced"
                print*, "          Weights should include the intermediante points"
                print*, ""
                stop
            case default
                ispc = ispc + 1
                filenames(ispc)=arg
                open(10+ispc,file=filenames(ispc),status='old',iostat=iostatus)
                if (iostatus /= 0) then
                    write(0,*) "ERROR: opening spectrum "//trim(adjustl(arg))
                    stop
                endif
        end select
    enddo

    if (.not.do_weight) then
        do i=1,ispc
            w(i) = 1.d0
        enddo
    endif

    ! Normalize weights
    a = 0.d0
    do i=1,iw
        a = a + w(i)
    enddo 

    ! Prepare for extrapolation
    filenames_bk(1:ispc) = filenames(1:ispc)
    k=0
    do i=1,ispc-1
        k=k+1
        filenames(k) = filenames_bk(i)
        do j=1,nadd
            k=k+1
            filenames(k) = "(Extrapolated)"
        enddo
    enddo
    k=k+1
    filenames(k) = filenames_bk(ispc)
    ispc_tot=k

    if (iw /= ispc_tot) then
        write(0,*) "     Weights     Spectra"
        write(0,*) iw, ispc_tot
        write(0,*) "ERROR: number of weights is not the same as number of spectra + added points"
        stop
    else
        write(0,*) " Spectrum                        Weight    NormWeight"
        write(0,*) "------------------------------------------------------"
        do i=1,iw
            write(0,'(A30,2F10.2)') filenames(i), w(i), w(i)/a
            w(i) = w(i)/a
        enddo
    endif

    do
        do i=1,ispc
            read(10+i,*,iostat=iostatus) x(i), y(i)
            if (iostatus /= 0) stop
        enddo
        ! Check that all x have the same value
        do i=2,ispc
            ! Check x-values (max dev 1%)
            if (abs(x(1)-x(i))/x(1) > 0.01) then
                write(0,*) "ERROR: the spectra have different scales"
                write(0,*) x(1), x(i), i
                stop
            endif
        enddo 

        ! Extrapolate points
        k=0
        do i=1,ispc-1
            k=k+1
            yy(k) = y(i)
!write(100+k,*) x(1), yy(k)
            do j=1,nadd
                k=k+1
                yy(k) = y(i) + (y(i+1)-y(i))*float(j)/float(nadd+1)
!write(100+k,*) x(1), yy(k)
            enddo
        enddo
        k=k+1
        yy(k) = y(ispc)
!write(100+k,*) x(1), yy(k)
        
        ! Compute the average
        a = 0.d0
        do i=1,ispc_tot
            a = a + yy(i)*w(i)
        enddo

        print*, x(1), a
    enddo

    stop

end program average_plots

