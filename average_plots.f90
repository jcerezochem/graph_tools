program average_plots

    ! Avergage spectra with weights

    implicit none

    character(len=200),dimension(100) :: filenames, filenames_bk
    real(8),dimension(100) :: yy,y, x, w
    real(8) :: a, da, da0, factor
    !Counters
    integer :: i,j,k, ii
    integer :: ispc, iw, ispc_tot
    integer :: N, nadd
    !IO
    integer :: iostatus
    !Input selections stuff
    logical :: argument_retrieved, &
               do_weight=.false.   
    character(len=200) :: arg, line
    character(len=2)   :: spctype
    character(len=1)   :: cnull
    logical :: print_extra=.true.

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
                    call close_scr_files(ispc)
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
            case ("-type")
                argument_retrieved=.true.
                call getarg(i+1, spctype)
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
                print*, "  -type [xy|y] Type of calculation (default: xy)"
                print*, ""
                call close_scr_files(ispc)
                stop
            case default
                ispc = ispc + 1
                filenames(ispc)=arg
                open(10+ispc,file=filenames(ispc),status='old',iostat=iostatus)
                if (iostatus /= 0) then
                    write(0,*) "ERROR: opening spectrum "//trim(adjustl(arg))
                    call close_scr_files(ispc)
                    stop
                endif
                ! Preprocess the file to get rid out of comment lines
                open(300+ispc,status="scratch")
                do
                    read(10+ispc,'(A)',iostat=iostatus) line
                    if (iostatus/=0) exit
                    call split_line(line,"#",line,cnull) 
                    if ( len_trim(line) == 0 ) cycle
                    write(300+ispc,'(A)') trim(adjustl(line))
                enddo
                close(10+ispc)
                rewind(300+ispc)
        end select
    enddo
    if (adjustl(spctype) == "y") print_extra=.false.

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

    if (.not.do_weight) then
        iw=ispc_tot
        do i=1,iw
            w(i) = 1.d0
        enddo
    endif

    ! Normalize weights
    a = 0.d0
    do i=1,iw
        a = a + w(i)
    enddo 


    if (iw /= ispc_tot) then
        write(0,*) "     Weights     Spectra"
        write(0,*) iw, ispc_tot
        write(0,*) "ERROR: number of weights is not the same as number of spectra + added points"
        call close_scr_files(ispc)
        stop
    else
        write(0,*) " Spectrum                        Weight    NormWeight"
        write(0,*) "------------------------------------------------------"
        do i=1,iw
            write(0,'(A30,2F10.2)') filenames(i), w(i), w(i)/a
            w(i) = w(i)/a
        enddo
        write(0,*) "------------------------------------------------------"
    endif

    a=0.d0
    do i=1,iw
        a=a+w(i)
    enddo
    write(0,'(X,A,F12.4)') "Sum of normalized weights = ", a

    ii=0
    do
        do i=1,ispc
            if (adjustl(spctype) == "y") then
                read(300+i,*,iostat=iostatus) y(i)
            else
                read(300+i,*,iostat=iostatus) x(i), y(i)
            endif
            if (iostatus /= 0) then
                call close_scr_files(ispc)
                stop
            endif
        enddo
        ! Check that all x have the same value
        do i=2,ispc
            ! Check x-values (max dev 1%)
            if (abs(x(1)-x(i))/x(1) > 0.01) then
                write(0,*) "ERROR: the spectra have different scales"
                write(0,*) x(1), x(i), i
                call close_scr_files(ispc)
                stop
            endif
        enddo 

        ! Extrapolate points
        k=0
        do i=1,ispc-1
            k=k+1
            yy(k) = y(i)
            if (print_extra) write(100+k,*) x(1), yy(k)
            do j=1,nadd
                k=k+1
                yy(k) = y(i) + (y(i+1)-y(i))*float(j)/float(nadd+1)
            if (print_extra) write(100+k,*) x(1), yy(k)
            enddo
        enddo
        k=k+1
        yy(k) = y(ispc)
        if (print_extra) write(100+k,*) x(1), yy(k)
        
        ! Compute the average
        a = 0.d0
        factor = 0.d0
        do i=1,ispc_tot
            a = a + yy(i)*w(i)
        enddo
        ! and stddev
        factor = dfloat(ispc_tot - 1)/dfloat(ispc_tot)
        da = 0.d0
        do i=1,ispc_tot
            da = da + (yy(i)-a)**2 *w(i)/factor
        enddo
        da = dsqrt(da)
        !da0 = 0.d0
        !do i=1,ispc_tot
        !    da0 = da0 + (yy(i)-a)**2 
        !enddo
        !da0 = dsqrt(da0/float(ispc_tot-1))

        if (adjustl(spctype) == "y") then
            print*, ii, a, da !, da0
            ii = ii + 1
        else
            print*, x(1), a, da !, da0
        endif
    enddo

    call close_scr_files(ispc)

    stop
 
    contains

    subroutine split_line(line,splitter,line_a,line_b)

        !Split a line from a given marker. If it is not present, it does not
        !split the line (the whole is preserved in line_a

        character(len=*),intent(in):: line,splitter
        character(len=*),intent(out):: line_a,line_b

        !local
        integer :: i,j
        !Auxiliar helps when line(input) is also one 
        !of the outputs, line_a or line_b
        character(len=(len(line_a))) :: aux_line_a

        i=INDEX(line,splitter)
        if ( i == 0 ) then
            line_a=line
            line_b=""
            return
        endif
        j=len_trim(splitter)
        
        aux_line_a=line(1:i-1)
        line_b=line(i+j:)
        line_a=aux_line_a

        return

    end subroutine split_line
 
    subroutine close_scr_files(n)

        integer,intent(in) :: n

        !Local
        integer :: i

        do i=1,n
            close(300+i)
        enddo

        return

    end subroutine close_scr_files


end program average_plots

