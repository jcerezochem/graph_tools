program average_plots

    ! Avergage spectra with weights

    implicit none

    integer,parameter :: MAX_DIM=2000

    character(len=200),dimension(MAX_DIM) :: filenames, filenames_bk
    real(8),dimension(MAX_DIM) :: yy,y, w
    real(8) :: x, x0,y0,xref
    real(8) :: a, da, da0, factor
    !Counters
    integer :: i,j,k, ii
    integer :: ispc, iw, ispc_tot, iread, jread
    integer :: N, nadd
    !IO
    integer :: iostatus
    !Input selections stuff
    logical :: argument_retrieved, &
               do_weight=.false.   
    character(len=200)   :: arg
    character(len=20000) :: line
    character(len=2)     :: spctype
    character(len=1)     :: cnull
    logical :: print_extra=.false.

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
                    !call close_scr_files(ispc)
                    close(3000+iread)
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
                !close(3000+iread)
                !call close_scr_files(ispc)
                stop
            case default
                ispc = ispc + 1
                filenames(ispc)=arg
                open(10+ispc,file=filenames(ispc),status='old',iostat=iostatus)
                if (iostatus /= 0) then
                    write(0,*) "ERROR: opening spectrum "//trim(adjustl(arg))
                    !call close_scr_files(ispc)
                    stop
                endif
                ! Preprocess the file to get rid out of comment lines
                n=0
                iread = mod(ispc,2)
                jread = mod(ispc+1,2)
                open(3000+iread,status='scratch')
                if (ispc==1) open(3000+jread,status='scratch')
                do
                    read(10+ispc,'(A)',iostat=iostatus) line
                    if (iostatus/=0) exit
                    call split_line(line,"#",line,cnull) 
                    if ( len_trim(line) == 0 ) cycle

                    ! Write new file

                    if (adjustl(spctype) == "y") then
                        read(line,*) x0,y0
                        if (ispc==1) then
                            write(3000+iread,*) y0
                        else
                            read(3000+jread,'(A)') line
                            write(3000+iread,*) trim(adjustl(line)), y0
                        endif
                    else
                        read(line,*) x0,y0
                        if (ispc==1) then
                            write(3000+iread,*) x0,y0
                        else
                            read(3000+jread,'(A)') line
                            read(line,*) xref
                            if (abs(x0-xref) > 0.01) then
                                write(0,*) "ERROR: the spectra have different scales"
                                write(0,*) x0, xref, ispc
                                !call close_scr_files(ispc)
                                stop
                            endif
                            write(3000+iread,*) trim(adjustl(line)), y0
                         endif
                    endif

                    n=n+1
                enddo
                rewind(3000+iread)
                close(3000+jread)
                close(10+ispc)
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

    write(0,'(X,A,I0)') "Number of plots to average = ", ispc

    ii=0
    do
        if (adjustl(spctype) == "y") then
            read(3000+iread,*,iostat=iostatus) y(1:ispc)
        else
            read(3000+iread,*,iostat=iostatus) x, y(1:ispc)
        endif
        if (iostatus /= 0) then
            !call close_scr_files(ispc)
            close(3000+iread)
            stop
        endif

        ! Extrapolate points
        k=0
        do i=1,ispc-1
            k=k+1
            yy(k) = y(i)
            if (print_extra) write(100+k,*) x, yy(k)
            do j=1,nadd
                k=k+1
                yy(k) = y(i) + (y(i+1)-y(i))*float(j)/float(nadd+1)
            if (print_extra) write(100+k,*) x, yy(k)
            enddo
        enddo
        k=k+1
        yy(k) = y(ispc)
        if (print_extra) write(100+k,*) x, yy(k)
        
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
        else
            print*, x, a, da !, da0
        endif
        ii = ii + 1
    enddo

    !call close_scr_files(ispc)
    close(3000+iread)

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
            close(1300+i)
        enddo

        return

    end subroutine close_scr_files

        function int20char(i,length) result(c)

        ! Converts an integer into a char of a given length
        ! If length>digits, complete with zeroes

        integer,intent(in)    :: i
        integer,intent(in)    :: length
        character(len=length) :: c
        ! Local
        character(len=10) :: fmt
        integer           :: ilength, j
        character(len=10) :: dummy_char

        !If length<number of digits, rise an error
        if (i == 0) then
            ilength = 1
        elseif (i<0) then
            ilength = int(log10(float(-i)))+2
        else
            ilength = int(log10(float(i)))+1
        endif
        if (ilength>length) then
            write(0,*) "Error in int20char: more digits in number than character size"
            stop
        endif
        ! otherwise fill head with zeroes
        dummy_char = ""
        do j=1,length-ilength
            dummy_char = trim(dummy_char)//"0"
        enddo

        ! Write format
        write(fmt,'(a,i0,a)') '(A,I',ilength,')'
        write(c,fmt) trim(dummy_char), i

        return

    end function int20char


end program average_plots

