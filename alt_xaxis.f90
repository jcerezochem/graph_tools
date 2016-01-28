program alt_axis

    ! Program to build a differnt scale on the alternative axis
    ! on xmgrace

    implicit none

    real(8) :: axis0_min, axis0_max, axis0_minor, axis0_major
    real(8) :: axis1_min, axis1_max, axis1_minor, axis1_major
    real(8) :: scale, shift
    real(8) :: pos, pos_m
    real(8) :: aux
    integer :: i,j,k, nminor, nmajor, npoints
    ! Format label
    integer :: prec, prec_min
    character(len=20) :: labformat
    character(len=10) :: label
    character(len=50) :: title
    !File reading
    character(len=50) :: datfile="NONE"
    integer :: I_DAT = 10
    integer :: IOstatus
    character(len=240) :: line
    character(len=10)  :: cnum
    real(8) :: x,y
    !Input selections stuff
    logical :: argument_retrieved
    character(len=50) :: arg

    !Defaults
    scale       = 1.0
    shift       = 0.0
    nminor      = 1
    title       = "Alternative axis"
    prec        = -1
    ! compulsory:
    axis0_min   = 99999
    axis0_max   =-99999
    axis1_major = 0
    !Read binwidth from label
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-scale")
                call getarg(i+1, arg)
                read(arg,*) scale
                argument_retrieved=.true.
            case ("-shift")
                call getarg(i+1, arg)
                read(arg,*) shift
                argument_retrieved=.true.
            case ("-min0")
                call getarg(i+1, arg)
                read(arg,*) axis0_min
                argument_retrieved=.true.
            case ("-max0")
                call getarg(i+1, arg)
                read(arg,*) axis0_max
                argument_retrieved=.true.
            case ("-major")
                call getarg(i+1, arg)
                read(arg,*) axis1_major
                argument_retrieved=.true.
            case ("-nminor")
                call getarg(i+1, arg)
                read(arg,*) nminor
                argument_retrieved=.true.
            case ("-prec")
                call getarg(i+1, arg)
                read(arg,*) prec
                argument_retrieved=.true.
            case ("-title")
                call getarg(i+1, title)
                argument_retrieved=.true.
            case ("-data")
                call getarg(i+1, datfile)
                argument_retrieved=.true.
            case ("-h")
                print*, "Program to generate the alt axis scale"
                print*, "in agr files. Scale is output to std unit."
                print*, "Options:"
                print*, "  -scale  <value>  (scale wrt main axis, default=1.0)"
                print*, "  -shift  <value>  (shift to main axis,  default=0.0)"
                print*, "  -min0   <value>  (minimum main axis,   no default )"
                print*, "  -max0   <value>  (maximum main axis,   no default )"
                print*, "  -major  <value>  (distance new ticks,  no default )"
                print*, "  -nminor <value>  (number minor ticks,  default=1  )"
                print*, "  -title  <name>   (axis title)"
                print*, "  -data   <name>   (data file to estimate defaults-feuature incomplete)  "
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo


    ! If a data file is given, try to estimate defatuls from there
    if (adjustl(datfile) /= "NONE") then
        open(I_DAT,file=datfile,iostat=IOstatus)
        if (IOstatus /= 0) then
            write(0,*) "ERROR: cannot open ", trim(adjustl(datfile))
            stop
        endif
        do
            read(I_DAT,'(A)',iostat=IOstatus) line
            if (IOstatus /= 0) exit
            if (index(line,"#") == 1 .or. index(line,"@") == 1) cycle
            read(line,*) x,y
            axis0_min=min(axis0_min,x)
            axis0_max=max(axis0_max,x)
        enddo
        axis0_max=max(abs(axis0_min),axis0_max)
        axis0_min=-axis0_max
    endif

    if ( axis0_min == 99999 .or. axis0_max == -99999 ) then
        print*, "ERROR: -min0 and -max0 need to be specified"
        stop
    endif

    ! Write "header"
    print'(A)',  '@    altxaxis  on'
    print'(A)',  '@    altxaxis  label "'//trim(adjustl(title))//'"'
    print'(A)',  '@    altxaxis  label place opposite'
    print'(A)',  '@    altxaxis  tick out'
    print'(A)',  '@    altxaxis  ticklabel place opposite'
    print'(A)',  '@    altxaxis  tick place opposite'
    print'(A)',  '@    altxaxis  tick spec type both'

    ! axis1 = Scale*axis0 + Shift
    if ( axis1_major == 0 ) then
        !Use default number of ticks
        nmajor = 15
        call set_major_dist(axis0_min,axis0_max,axis1_major,prec_min,Scale,shift)
        ! Update precision if needed
        if (prec_min > prec) prec=prec_min
    else
        axis1_max = axis0_max*Scale+shift
        axis1_min = axis0_min*Scale+shift
        ! Update precision if needed
        call get_min_precision(axis1_major,prec_min)
        if (prec_min > prec) prec=prec_min
    endif
    call get_axis_limits(axis0_min, axis0_max,axis1_min,axis1_max,axis1_major,Scale,shift)
    nmajor = (axis1_max-axis1_min)/axis1_major + 1
    write(0,*) "From", axis1_min, " to ", axis1_max, " every ", axis1_major, ". nmajor: ", nmajor
    if (nmajor > 20) then
        write(0,*) "Too many ticks, autoticking."
        !Use default number of ticks
        nmajor = 15
        call set_major_dist(axis0_min,axis0_max,axis1_major,prec_min,Scale,shift)
        ! Update precision if needed
        if (prec_min > prec) prec=prec_min
        call get_axis_limits(axis0_min, axis0_max,axis1_min,axis1_max,axis1_major,Scale,shift)
    endif

    ! Set label format
    if (prec < 0) then
        write(0,*) "ERROR: -prec should be >=0"
        stop
    else 
        write(labformat,'(A,I0,A)') '(F10.',prec,')'
    endif

    ! Compute npoints taking nmajor and nminor into consideration
    ! Start on min
    pos = axis1_min
    k = 0
    do i=1,nmajor
        print'(A,I0,A,F12.6)', "@    altxaxis  tick major ", k,',', pos/Scale
        if (prec == 0) then
            write(label,'(I0)') int(pos)
        else
            write(label,labformat) pos
        endif
        print'(A,I0,A)', "@    altxaxis  ticklabel ", k, ', "'//trim(adjustl(label))//'"'
        ! Print minor
        do j=1,nminor
            k=k+1
            pos_m = pos + axis1_major/float(nminor+1)*j
            print'(A,I0,A,F12.6)', "@    altxaxis  tick minor ", k,',', pos_m/Scale
        enddo
        ! Update pos
        k = k+1
        pos = pos + axis1_major
    enddo
    print'(A,I0)', "@    altxaxis  tick spec ", k
     

    stop

    contains

    subroutine get_axis_limits(axis0_min,axis0_max,axis1_min,axis1_max,axis1_major,Scale,shift)

        real(8),intent(in)  :: axis0_min, axis0_max
        real(8),intent(out) :: axis1_min, axis1_max,axis1_major
        real(8),intent(in)  :: Scale,shift

        ! Determine starting point: 
        !    beyond Axis0 and multiple of axis1_major
        axis1_min = axis0_min*Scale+shift
        axis1_min = int(axis1_min/axis1_major) * axis1_major
        if (axis1_min > axis0_min*Scale+shift) then
            axis1_min = axis1_min - axis1_major
        endif 
        ! Determine ending point: 
        !    beyond Axis0 and multiple of axis1_major
        axis1_max = axis0_max*Scale+shift
        axis1_max = int(axis1_max/axis1_major) * axis1_major
        if (axis1_max < axis0_max*Scale+shift) then
            axis1_max = axis1_max + axis1_major
        endif 

        return

    end subroutine get_axis_limits

    subroutine set_major_dist(axis0_min,axis0_max,axis1_major,prec,Scale,shift)

        real(8),intent(in)  :: axis0_min, axis0_max
        real(8),intent(out) :: axis1_major
        integer,intent(out) :: prec
        real(8),intent(in)  :: Scale,shift
        ! Local
        real(8)             :: axis1_min, axis1_max, aux
        character(len=10)   :: cnum, cdec
        integer             :: i

        ! Initial estimation of limits
        axis1_max = axis0_max*Scale+shift
        axis1_min = axis0_min*Scale+shift
        axis1_major = (axis1_max-axis1_min)/float(nmajor)
        ! Recompute precision
        i = 0
        aux=axis1_major
        prec=-1
        do while (i == 0)
            write(cnum,'(F10.5)') aux
            call split_line(cnum,".",cnum,cdec)
            read(cnum,*) i
            prec = prec+1
            aux = aux*10.d0
        enddo
        ! Check next decimal and increase to prec if needed
        read(cdec,*) i
        if ( i < 5000) then ! take the partition with prec=0
            read(cnum,*) axis1_major
            axis1_major = axis1_major/10.**(prec)
        else ! take first decimal
            prec=prec+1
            read(cnum,*) axis1_major
            axis1_major = axis1_major + float(i/10000)/10.d0
            axis1_major = axis1_major/10.**(prec-1)
        endif

        return

    end subroutine set_major_dist

    subroutine get_min_precision(axis1_major,prec)

        real(8),intent(out) :: axis1_major
        integer,intent(out) :: prec
        ! Local
        real(8)             :: aux
        character(len=10)   :: cnum
        character(len=5)    :: cdec
        integer             :: i

        ! Recompute precision
        i = 0
        aux=axis1_major
        prec=-1
        do while (i == 0)
            write(cnum,'(F10.5)') aux
            call split_line(cnum,".",cnum,cdec)
            read(cnum,*) i
            prec = prec+1
            aux = aux*10.d0
        enddo
        ! Check decimals
        ! Run back till we find something different from 0
        
        prec = 5
        do i=5,1,-1
            if ( cdec(i:i) /= "0") exit
            prec=prec-1
        enddo

        return

    end subroutine get_min_precision

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

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    subroutine split_line_back(line,splitter,line_a,line_b)

        !Split a line from a given marker. If it is not present, it does not
        !split the line (the whole is preserved in line_a >> change: for back
        ! this is stored in line_b (so that if no match, the whole thing is in line_b
        ! the important thing for this SR

        character(len=*),intent(in):: line,splitter
        character(len=*),intent(out):: line_a,line_b

        !local
        integer :: i,j
        !Auxiliar helps when line(input) is also one 
        !of the outputs, line_a or line_b
        character(len=(len(line_a))) :: aux_line_a

        !INDEX with BACK=.true., search match from the end of the string (useful to get file extensions)
        i=INDEX(line,splitter,.true.)
        if ( i == 0 ) then
            line_a=""
            line_b=line
            return
        endif
        j=len_trim(splitter)
        
        aux_line_a=line(1:i-1)
        line_b=line(i+j:)
        line_a=aux_line_a

        return

    end subroutine split_line_back

end program alt_axis

