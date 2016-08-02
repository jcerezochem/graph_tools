program sort_plot

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
    logical :: reverse=.false.
    logical :: onecol=.false.
    logical :: comments=.true.  
    

    start_range = -180.d0
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-rev")
                reverse=.true.
            case ("-onecol")
                onecol=.true.
            case ("-noonecol")
                onecol=.false.
            case ("-comment")
                comments=.true.
            case ("-nocomment")
                comments=.false.
            case ("-h")
                print*, "Program to sort a graph according to x axis"
                print*, "Input/output by standard chanels. "
                print*, "Flag options: "
                print*, "  -rev            Use reverse order"
                print*, "  -[no]onecol     Expect only one column (x-range without y data) (def:false)"
                print*, "  -[no]comment    Do not copy comments from the original plot (def: true)"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo


    open(30,status='scratch')
    i = 0
    do 
        read(*,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        write(30,'(A)') trim(adjustl(line))
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

    ! Order
    if (reverse) then
        do i=1,N-1
            do j=i+1,N
                if (x(j)>x(i)) then
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
    else
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
    rewind(30)
    i=0
    do 
        read(30,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        if ( INDEX(line,'#') == 1 .and. comments) then
            print*, trim(adjustl(line))
        endif
        if ( INDEX(line,'#') == 1 .or. &
             len_trim(line)  == 0 ) then
            cycle
        endif
        i=i+1
        if (onecol) then
            print*, x(i)
        else
            print*, x(i), y(i)
        endif
    enddo
    close(30)

    !if (reverse) then
    !    do i=N,1,-1
    !    enddo
    !else
    !    do i=1,N
    !        if (onecol) then
    !            print*, x(i)
    !        else
    !            print*, x(i), y(i)
    !        endif
    !    enddo
    !endif


    stop

end program sort_plot

