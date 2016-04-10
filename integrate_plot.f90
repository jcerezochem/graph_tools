program rebase_plot

    ! Shift the plot to set the minimum y to zero
    ! Labels (#) are identified and kept

    implicit none

    character(len=200) :: line
    real(8),dimension(20000) :: y, x
    real(8) :: Integral
    !Counters
    integer :: i,j,k
    integer :: N 
    !Input selections stuff
    logical :: argument_retrieved, &
               simpson=.false.   
    character(len=50) :: arg
    ! Cummulative sum stuff
    real(8),dimension(20000) :: cummsum
    real(8) :: intercept, x_icp
    character(len=200) :: intercepts_file="none"
    !IO
    integer :: I_ICP=10
    integer :: iostatus


    !Read options from command line
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-simpson")
                simpson=.true.
            case ("-icp-file")
                call getarg(i+1, intercepts_file)
                argument_retrieved=.true.
            case ("-h")
                print*, "Program to compute the integral of a graph"
                print*, "with trapezoid or Simpson method."
                print*, "Plot (x,y) data are fed by stdin" 
                print*, "Options"
                print*, "  -simpson    Use Simpson method. Needs a equi-spaced grid"
                print*, "              Otherwise, trapezoids is used, which woks"
                print*, "              alos in non-regular grids"
                print*, "  -icp-file   Intercepts file: file with x values to report"
                print*, "              the intercepts of the cummulative sum. Only"
                print*, "              available with the trapezoid method"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo

    ! In order to make it compatible if pipe feeding
    ! while keeping the posibility to save the
    ! comments, we first copy in input to a scratch file

    open(10,status='scratch')

    i = 0
    do 
        read(*,'(A)',iostat=iostatus) line
        ! Stop at the end, of when finding a blank line
        if (len_trim(line) == 0 .or. iostatus /= 0) exit
        write(10,'(A)') line 
        if ( INDEX(line,'#') == 1 ) then
            cycle
        endif

        i=i+1
        read(line,*) x(i), y(i)

    enddo
    N = i

    if (simpson) then
        Integral = y(1)+y(N)
        do i= 2, N-1, 2
            Integral = Integral + 4.d0*y(i)
            Integral = Integral + 2.d0*y(i+1)
        enddo
        Integral = Integral * (x(2)-x(1)) / 3.d0
    else !Trapezios
        ! Allow for non-regular grids
        Integral=0.d0
        cummsum(1)=Integral
        do i=1,N-1
            Integral = Integral + (x(i+1)-x(i))*(y(i+1)+y(i))/2.d0
            cummsum(i+1) = Integral
        enddo

        if (intercepts_file /= "none") then
            print*, "Intercepts of the cummulative sum function"
            open(I_ICP,file=intercepts_file,status='old')
            do 
                read(I_ICP,'(A)',iostat=iostatus) line
                if (len_trim(line) == 0 .or. iostatus /= 0) exit
                read(line,*) x_icp
                do i=2,N
                    if (x(i) > x_icp) then
                        j=i
                        exit
                    endif
                enddo
                ! Compute extrapolated value of cummsum, between (j-1) and j
                intercept = cummsum(j-1) + (cummsum(j)-cummsum(j-1))/(x(j)-x(j-1))*(x_icp-x(j-1))
                print*, x_icp, intercept
            enddo 
            close(I_ICP)
            print*, ""
        endif

    endif

    print*, Integral

    close(10)

    stop

end program rebase_plot

