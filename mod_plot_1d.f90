program mod_plot

    ! Modify a plot by scaling/shifting/inversing the axis
    ! Can be used for plain of strucured data plots (e.g. agr)
    ! Input/Output from standard unit

    implicit double precision (A-H,O-Z)
    implicit integer (I-N)

    character(len=100) :: line, line2
    integer :: unt
    !Input selections stuff
    logical :: argument_retrieved, &
               comments=.true.   , &
               xabs=.false.      , &
               yabs=.false.      , &
               xinv=.false.      , &
               yinv=.false.      , &
               rescale=.false.   
    character(len=50) :: arg

    !
    !Read scale/shift factors. Note that if both scale and shift are required, it is:
    !  Xnew = (Xold+shift)*scale
    !===================================================
    !Defaults
    xshift = 0.d0
    yshift = 0.d0
    xscale = 1.d0    !1.d0/1.23981d-4 ! ev --> cm-1
    yscale = 1.d0 
    xpower = 0.d0
    !Read binwidth from label
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-abs")
                yabs=.true.
            case ("-sc")
                call getarg(i+1, arg)
                read(arg,*) yscale
                argument_retrieved=.true.
            case ("-scale")
                call getarg(i+1, arg)
                read(arg,*) yscale
                argument_retrieved=.true.
            case ("-scale-inv")
                call getarg(i+1, arg)
                read(arg,*) yscale
                yscale=1.d0/yscale
                argument_retrieved=.true.
            case ("-sc-inv")
                call getarg(i+1, arg)
                read(arg,*) yscale
                yscale=1.d0/yscale
                argument_retrieved=.true.
            case ("-rsc")
                call getarg(i+1, arg)
                read(arg,*) yscale
                rescale=.true.
                argument_retrieved=.true.
            case ("-rescale")
                call getarg(i+1, arg)
                read(arg,*) yscale
                rescale=.true.
                argument_retrieved=.true.
            case ("-rescale-inv")
                call getarg(i+1, arg)
                read(arg,*) yscale
                yscale=1.d0/yscale
                rescale=.true.
                argument_retrieved=.true.
            case ("-sh")
                call getarg(i+1, arg)
                read(arg,*) yshift
                argument_retrieved=.true.
            case ("-shift")
                call getarg(i+1, arg)
                read(arg,*) yshift
                argument_retrieved=.true.
            case ("-nocomment")
                comments=.false.
            case ("-inv")
                yinv=.true.
            case ("-h")
                print*, "Program to manipulate data files by"
                print*, "shifting or multiplying the spectra."
                print*, "Original data are fed by stdin and" 
                print*, "and the results output to stdout"
                print*, "For the manipulations use the labels:"
                print*, "  -shift <value>"
                print*, "  -scale <value>"
                print*, "  -scale-inv <value> (applies a scale factor: 1/value"
                print*, "  -rescale <value>"
                print*, "  -rescale-inv <value> (applies a scale factor: 1/value"
                print*, "To modify x/y axes as:"
                print*, " Ynew = (Yold+shift)*scale"
                print*, "when yrescale is used, it is first normalized to max(y)=1"
                print*, "Additonal flags are (without args)"
                print*, " -nocomment   Do not copy comments from the original plot"
                print*, " -abs        Take the absolute of y, before manipulations"
                print*, " -inv        Take the inverse of y, before manipulations"
                print*, "By default all values are set to"
                print*, " 0 (shifts)"
                print*, " 1 (scales)"
                print*, " 1 (yrescale)"
                print*, "Some useful conversion factors:"
                print*, " eV->cm-1:          8068.75"
                print*, " Hartree->kcal/mol: 627.503"
                print*, " Hartree->kJ/mol:   2625.5 "
                print*, " Hartree->eV:       27.2107"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo


    if (rescale) then
       unt=30
       ymax=0.0
       open(30,status='scratch')
       do
           read(5,'(A)',iostat=IOS) line
           if (IOS /= 0) exit
           if (len_trim(line) == 0) exit
           write(30,'(A)') line
           if (index(line,"%")==1) then
               cycle
           elseif (index(line,"#")==1) then
               cycle
           elseif (index(line,"@")==1) then
               cycle
           elseif (index(line,"&")==1) then
               cycle
           endif
           n=n+1
           read(line,*) y
           if (y > ymax) ymax=y
       enddo
       rewind(30)
    else
        unt=5
        ymax=1.d0
    endif
            
    do 
        read(unt,'(A)',iostat=IOstatus) line
        if (IOstatus /= 0) exit
        if (len_trim(line) == 0) exit
        if (index(line,"%")==1) then
            if (comments) print'(A)', trim(adjustl(line))
            cycle
        elseif (index(line,"#")==1) then
            if (comments) print'(A)', trim(adjustl(line))
            cycle
        elseif (index(line,"@")==1) then
            if (comments) print'(A)', trim(adjustl(line))
            cycle
        elseif (index(line,"&")==1) then
            if (comments) print'(A)', trim(adjustl(line))
            cycle
        endif
        read(line,*) y
        y=y/ymax*x**xpower
        if (yabs) y=dabs(y)
        if (yinv) then
            if (y==0.d0) then
                cycle !skip division by zero
            else
                y=1.d0/y
            endif
        endif
        y=(y+yshift)*yscale
        print'(2(X,G15.7))', y
    enddo
    if (rescale) close(30)

    stop

end program



