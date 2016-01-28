program remove_triling_zeroes

    ! Detects blocks of zeroes in the plot
    ! and delete them if needed

    implicit none

    character(len=200) :: line
    real(8),dimension(10000) :: x, y
    integer,dimension(10) :: zero_beg,zero_end
    logical :: zeroes_region, approx
    real(8) :: thr, ymax_abs
    !Counters
    integer :: i,j,k, iz
    integer :: new_range_beg, new_range_end
    integer :: N
    !IO
    integer :: iostatus
    !Input selections stuff
    logical :: argument_retrieved, &
               comments=.true.   , &
               xabs=.false.      , &
               yabs=.false.      , &
               xinv=.false.      , &
               yinv=.false.
    character(len=50) :: arg

    ! Defaults
    approx = .false.
    thr=0.d0

    ! NOTE
    !===================================================
    !Read binwidth from label
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-approx")
                approx=.true.
            case ("-thr")
                call getarg(i+1, arg)
                read(arg,*) thr
                argument_retrieved=.true.
            case ("-h")
                print*, "Remove trailing zeroes from a graph."
                print*, "By default, zero is exactly zero, but"
                print*, "one can tell the program to take an "
                print*, "approximate zero accoding the the max" 
                print*, "or use a threshold"
                print*, "Options"
                print*, "  -approx"
                print*, "  -thr <value>"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo
    

    ymax_abs=0d0
    i = 0
    do 
        read(*,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        if ( INDEX(line,'#') == 1 .or. len_trim(line) == 0 ) then
            cycle
        endif

        i=i+1
        read(line,*) x(i), y(i)
        ymax_abs = max(abs(y(i)),ymax_abs)

    enddo
    N = i

    if (approx) thr=ymax_abs*1.d-3

    ! Detect trailing zeroes
    zeroes_region=.false.
    iz = 0
    do i=1,N
        if ( abs(y(i)) < thr .and. .not.zeroes_region ) then
            iz = iz + 1
            zeroes_region=.true.
            zero_beg(iz) = i
        elseif ( y(i) > thr .and. zeroes_region ) then
            zeroes_region=.false.
            zero_end(iz) = i
        endif
    enddo
!    print*, "Blocks of zeroes:"
!    do i=1,iz
!        print*, "Block", iz, zero_beg(i), "-", zero_end(i)
!    enddo

    ! Only delete blocks at the end or begining
    new_range_beg=1
    new_range_end=N
    do i=1,iz
        if (zero_beg(i) == 1) then
            new_range_beg=zero_end(i)
        elseif (zero_end(i) == N) then
            new_range_end=zero_beg(i)
        endif
    enddo

    do i=new_range_beg,new_range_end
        print*, x(i), y(i)
    enddo
       
    stop

end program remove_triling_zeroes

