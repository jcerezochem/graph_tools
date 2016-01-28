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
            case ("-xabs")
                xabs=.true.
            case ("-yabs")
                yabs=.true.
            case ("-xsc")
                call getarg(i+1, arg)
                read(arg,*) xscale
                argument_retrieved=.true.
            case ("-xscale")
                call getarg(i+1, arg)
                read(arg,*) xscale
                argument_retrieved=.true.
            case ("-xscale-inv")
                call getarg(i+1, arg)
                read(arg,*) xscale
                xscale=1.d0/xscale
                argument_retrieved=.true.
            case ("-xsc-inv")
                call getarg(i+1, arg)
                read(arg,*) xscale
                xscale=1.d0/xscale
                argument_retrieved=.true.
            case ("-ysc")
                call getarg(i+1, arg)
                read(arg,*) yscale
                argument_retrieved=.true.
            case ("-yscale")
                call getarg(i+1, arg)
                read(arg,*) yscale
                argument_retrieved=.true.
            case ("-yscale-inv")
                call getarg(i+1, arg)
                read(arg,*) yscale
                yscale=1.d0/yscale
                argument_retrieved=.true.
            case ("-ysc-inv")
                call getarg(i+1, arg)
                read(arg,*) yscale
                yscale=1.d0/yscale
                argument_retrieved=.true.
            case ("-yrsc")
                call getarg(i+1, arg)
                read(arg,*) yscale
                rescale=.true.
                argument_retrieved=.true.
            case ("-yrescale")
                call getarg(i+1, arg)
                read(arg,*) yscale
                rescale=.true.
                argument_retrieved=.true.
            case ("-yrescale-inv")
                call getarg(i+1, arg)
                read(arg,*) yscale
                yscale=1.d0/yscale
                rescale=.true.
                argument_retrieved=.true.
            case ("-divx")
                call getarg(i+1, arg)
                read(arg,*) xpower
                xpower=-xpower
                argument_retrieved=.true.
            case ("-mulx")
                call getarg(i+1, arg)
                read(arg,*) xpower
                argument_retrieved=.true.
            case ("-xsh")
                call getarg(i+1, arg)
                read(arg,*) xshift
                argument_retrieved=.true.
            case ("-xshift")
                call getarg(i+1, arg)
                read(arg,*) xshift
                argument_retrieved=.true.
            case ("-ysh")
                call getarg(i+1, arg)
                read(arg,*) yshift
                argument_retrieved=.true.
            case ("-yshift")
                call getarg(i+1, arg)
                read(arg,*) yshift
                argument_retrieved=.true.
            case ("-nocomment")
                comments=.false.
            case ("-xinv")
                xinv=.true.
            case ("-yinv")
                yinv=.true.
            case ("-h")
                print*, "Program to manipulate data files by"
                print*, "shifting or multiplying the spectra."
                print*, "Original data are fed by stdin and" 
                print*, "and the results output to stdout"
                print*, "For the manipulations use the labels:"
                print*, "  -xshift <value>"
                print*, "  -yshift <value>"
                print*, "  -xscale <value>"
                print*, "  -xscale-inv <value> (applies a scale factor: 1/value"
                print*, "  -yscale <value>"
                print*, "  -yscale-inv <value> (applies a scale factor: 1/value"
                print*, "  -yrescale <value>"
                print*, "  -yrescale-inv <value> (applies a scale factor: 1/value"
                print*, "  -mulx <xpower>"
                print*, "  -divx <xpower>"
                print*, "To modify x/y axes as:"
                print*, " Xnew = (Xold+xshift)*xscale"
                print*, " Ynew = (Yold+yshift)*yscale"
                print*, "when yrescale is used, it is first normalized to max(y)=1"
                print*, "Additonal flags are (without args)"
                print*, " -nocomment   Do not copy comments from the original plot"
                print*, " -xabs        Take the absolute of x, before manipulations"
                print*, " -yabs        Take the absolute of y, before manipulations"
                print*, " -xinv        Take the inverse of x, before manipulations"
                print*, " -yinv        Take the inverse of y, before manipulations"
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
           read(line,*) x,y
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
        read(line,*) x, y
        y=y/ymax*x**xpower
        if (xabs) x=dabs(x)
        if (yabs) y=dabs(y)
        if (xinv) then
            if (x==0.d0) then
                cycle !skip division by zero
            else
                x=1.d0/x
            endif
        endif
        if (yinv) then
            if (y==0.d0) then
                cycle !skip division by zero
            else
                y=1.d0/y
            endif
        endif
        x=(x+xshift)*xscale
        y=(y+yshift)*yscale
        print'(2(X,G15.7))', x,y
    enddo
    if (rescale) close(30)

    stop

end program



