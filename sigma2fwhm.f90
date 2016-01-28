program sigma2fwhm

    !Input selections stuff
    logical :: argument_retrieved
    character(len=50) :: arg

    real :: sigma=-1., &
            FWHM =-1., &
            HWHM =-1.
    character(len=3) :: units="eV "

    logical :: is_sigma = .false.,&
               is_fwhm  = .false.,&
               is_hwhm  = .false.


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
                call getarg(i+1, arg)
                read(arg,*) units
                argument_retrieved=.true.
            case ("-sig")
                call getarg(i+1, arg)
                read(arg,*) sigma
                argument_retrieved=.true.
            case ("-fwhm")
                call getarg(i+1, arg)
                read(arg,*) FWHM
                argument_retrieved=.true.
            case ("-hwhm")
                call getarg(i+1, arg)
                read(arg,*) HWHM
                argument_retrieved=.true.
                argument_retrieved=.true.
            case ("-h")
                print*, "Program to convert between"
                print*, " SIGMA <-> FWHM <-> HWHM"
                print*, "of normal distributions."
                print*, "Input the property and its units:"
                print*, "  -units <value> (should be either eV[default] or cm1)"
                print*, "  -sig <value>"
                print*, "  -fwhm <value>"
                print*, "  -hwhm <value>"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo

    !Detect what was given
    if (sigma /= -1) is_sigma=.true.
    if (FWHM  /= -1) is_fwhm =.true.
    if (HWHM  /= -1) is_hwhm =.true.

    if ((is_sigma.and.is_fwhm) .or.&
        (is_sigma.and.is_hwhm) .or.&
        (is_hwhm .and.is_fwhm) .or.&
        (.not.is_sigma .and. .not.is_fwhm .and. .not.is_hwhm)) then
        print*, "Provide only ONE of:"
        print*, " -sig -fwhm -hwhm"
        stop
    endif

    if (is_sigma) then
        HWHM = sigma*sqrt(2.*log(2.))
        FWHM = HWHM*2.
    else if (is_fwhm) then
        HWHM  = FWHM/2.
        sigma = HWHM/sqrt(2.*log(2.))
    else if (is_fwhm) then
        FWHM  = HWHM*2.
        sigma = HWHM/sqrt(2.*log(2.))
    endif


    if (adjustl(units) == "cm1") then
        sigma = sigma*1.23981d-4
        FWHM  = FWHM*1.23981d-4
        HWHM  = HWHM*1.23981d-4
    endif

    print*, "UNITS: eV"
    print*, " SIGMA     HWHM     FWHM"
    print*, "------------------------"
    print'(3(F8.3,X))', sigma, HWHM, FWHM
    print*, ""
    sigma = sigma/1.23981d-4 
    FWHM  = FWHM/1.23981d-4
    HWHM  = HWHM/1.23981d-4
    print*, "UNITS: cm-1"
    print*, " SIGMA     HWHM     FWHM"
    print*, "------------------------"
    print'(3(F8.1,X))', sigma, HWHM, FWHM
    print*, ""
     

    stop

end program
