program average_plots

    ! Shift the plot to set the minimum y to zero
    ! Labels (#) are identified and kept

    implicit none

    character(len=200),dimension(100) :: filenames
    real(8),dimension(100) :: y, x, w
    real(8) :: a
    !Counters
    integer :: i,j,k
    integer :: ispc, iw
    integer :: N
    !IO
    integer :: iostatus
    !Input selections stuff
    logical :: argument_retrieved, &
               do_weight=.false.   
    character(len=50) :: arg


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
                    print*, "ERROR: opening "//trim(adjustl(arg))
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
            case ("-h")
                print*, "Program to average spectra"
                print*, "All plots need to be defined in the same time grid."
                print*, "Output spcetrum to stdout" 
                print*, "Options"
                print*, "  -weights    File with weights"
                print*, ""
                stop
            case default
                ispc = ispc + 1
                filenames(ispc)=arg
                open(10+ispc,file=filenames(ispc),status='old',iostat=iostatus)
                if (iostatus /= 0) then
                    print*, "ERROR: opening spectrum "//trim(adjustl(arg))
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

    if (iw /= ispc) then
        print*, "ERROR: number of points is not the same as number of spectra"
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
        a = 0.d0
        do i=1,ispc
            read(10+i,*,iostat=iostatus) x(i), y(i)
            if (iostatus /= 0) stop
            a = a + y(i)*w(i)
        enddo
        do i=2,ispc
            ! Check x-values (max dev 1%)
            if (abs(x(1)-x(i))/x(1) > 0.01) then
                print*, "ERROR: the spectra have different scales"
                print*, x(1), x(i), i
                stop
            endif
        enddo 
        print*, x(1), a
    enddo

    stop

end program average_plots

