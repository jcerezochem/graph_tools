program rebase_plot

    ! Shift the plot to set the minimum y to zero
    ! Labels (#) are identified and kept

    implicit none

    character(len=200) :: line
    real(8),dimension(1000) :: y
    real(8) :: ymax, x
    !Counters
    integer :: i,j,k
    integer :: N
    !IO
    integer :: iostatus

    ! In order to make it compatible if pipe feeding
    ! while keeping the posibility to save the
    ! comments, we first copy in input to a scratch file

    open(10,status='scratch')

    i = 0
    do 
        read(*,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        write(10,'(A)') line 
        if ( INDEX(line,'#') == 1 ) then
            cycle
        endif

        i=i+1
        read(line,*) x, y(i)

    enddo
    N = i

    !Get ymax
    ymax = -9999999
    do i=1,N
        ymax = max(y(i),ymax)
    enddo
    
    ! Re-read (to keep labels if were in the middle
    !  rewind(5) <- this is not good with pipes
    rewind(10)
    i = 0
    do
        read(10,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        if ( INDEX(line,'#') == 1 ) then
            print'(A)', line
            cycle
        endif

        i=i+1
        read(line,*) x, y(i)
        print*,      x, y(i)/ymax

    enddo

    close(10)

    stop

end program rebase_plot

