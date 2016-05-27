program rebase_plot

    ! Modify x values (assumed angles) to the selected
    ! range: entry_value -to-> entry_value+360

    implicit none

    character(len=200) :: line
    real(8),dimension(1000) :: x, y
    real(8) :: start_range, aux
    !Counters
    integer :: i,j,k
    integer :: N
    !IO
    integer :: iostatus
    !Input selections stuff
    logical :: argument_retrieved
    character(len=50) :: arg
    !other
    logical :: in_range
    logical :: reorder=.true.
    logical :: onecol=.false.
    

    start_range = -180.d0
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-start")
                call getarg(i+1, arg)
                read(arg,*) start_range
                argument_retrieved=.true.
            case ("-noreorder")
                reorder=.false.
            case ("-reorder")
                reorder=.true.
            case ("-onecol")
                onecol=.true.
            case ("-noonecol")
                onecol=.false.
            case ("-h")
                print*, "Program to modify the range to sort"
                print*, "periodic data. Output is also ordered"
                print*, "by default, but this can be avided."
                print*, "Input/output by standard chanels. "
                print*, "Flag options: "
                print*, "  -start <value>  Set the initial for the periodic range:"
                print*, "                  from <value> to <value>+360 [def:-180]"
                print*, "  -[no]reorder    Whether or not the reorder data [def:reorder]"
                print*, "  -[no]onecol     Expect only one column (x-range without y data)"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo


    i = 0
    do 
        read(*,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        if ( INDEX(line,'#') == 1 .or. &
             len_trim(line)  == 0 ) then
            cycle
        endif
        i=i+1
        if (onecol) then
            read(line,*) x(i)
        else
            read(line,*) x(i), y(i)
        endif

    enddo
    N = i

    ! Place in range
    do i=1,N

        in_range = .false.
        do while (.not.in_range)

            if     (x(i) < start_range) then
                x(i) = x(i) + 360.d0
            elseif (x(i) > start_range+360.d0) then
                x(i) = x(i) - 360.d0
            endif

            if (x(i) >= start_range .and. x(i) <= start_range+360.d0) in_range=.true.

        enddo

    enddo

    ! Order
    if (reorder) then
        do i=1,N-1
            do j=i+1,N
                if (x(j)<x(i)) then
                    aux=x(i)
                    x(i) = x(j)
                    x(j) = aux
                    if (.not.onecol) then
                        aux=y(i)
                        y(i) = y(j)
                        y(j) = aux
                    endif
                endif
            enddo
        enddo
    endif

    ! Print
    do i=1,N
        if (onecol) then
            print*, x(i)
        else
            print*, x(i), y(i)
        endif
    enddo


    stop

end program rebase_plot

