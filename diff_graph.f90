program diff_graph

    ! Numerical differenciation 
    ! Allow to use non-regular grids


    real(8),dimension(10000) :: x,y
    real(8) :: g

    i=0
    do
        i=i+1
        read(5,*,iostat=IOstatus) x(i), y(i)
        if (IOstatus /= 0) exit
    enddo
    N=i-1

    write(0,*) "Read points", N

    do i=2,N-1
        g = 0.5*(y(i)-y(i-1))/(x(i)-x(i-1)) + 0.5*(y(i+1)-y(i))/(x(i+1)-x(i))
        print*, x(i), g
    enddo

    stop

end program
