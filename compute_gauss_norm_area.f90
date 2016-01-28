program compute_gauss

    implicit double precision(A-H,O-Z)
    implicit integer(I-N)

    real(8),parameter :: pi=4.d0*datan(1.d0)

    read(*,*) sigma
    read(*,*) xmin, xmax

    dx = (xmax-xmin)/200.d0

    A = 1/dsqrt(2.0*pi)/sigma

    x=xmin
    do i=1,201
        gau = A*dexp(-0.5*x**2/sigma**2)
        write(30,*) x, gau
        x=x+dx
    enddo

    stop

end program
