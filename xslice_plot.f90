program filter_distrib

    implicit none

    real(8) :: x,y
    real(8) :: xinf,xsup
    integer :: i
    !IO
    integer :: ioflag
    !Input selections stuff
    logical :: argument_retrieved
    character(len=50) :: arg

    !Get limits to filter
    xinf=-9999.d0
    xsup=9999.d0
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-xinf")
                call getarg(i+1, arg)
                read(arg,*) xinf
                argument_retrieved=.true.
            case ("-xsup")
                call getarg(i+1, arg)
                read(arg,*) xsup
                argument_retrieved=.true.
            case ("-h")
                write(0,*) "Programa para filtrar distribuciones"
                write(0,*) "entre xinf y xsup. Indicado con las"
                write(0,*) "flags:"
                write(0,*) " -xinf <value>"
                write(0,*) " -xsup <value>"
                write(0,*) ""
                stop
        end select
    enddo

    do 
        read(5,*,iostat=ioflag) x,y
        if (ioflag /= 0) exit
        if ( x>xinf .and. x<xsup ) then
            print*, x,y
        endif 
    enddo

    stop

end program filter_distrib

