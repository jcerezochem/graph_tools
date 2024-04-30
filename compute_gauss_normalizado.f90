program compute_gauss

    !Para ser compatible con compute_gauss se lee la amplitud (A), aunque no se usa

    implicit double precision(A-H,O-Z)
    implicit integer(I-N)

    read(*,*) A
    read(*,*) sigma
    read(*,*) xmin, xmax

    dx = (xmax-xmin)/200.d0

    area=0.d0
    x=xmin
    do i=1,201
        gau = dexp(-0.5*x**2/sigma**2)
        x=x+dx
        !To normalize
        area = area + 2.0*gau
    enddo
    area=area-dexp(-0.5*xmin**2/sigma**2)-dexp(-0.5*xmax**2/sigma**2)
    !Normalization factor:
    area = area*dx/2.d0

    x=xmin
    do i=1,201
        gau = dexp(-0.5*x**2/sigma**2)
        write(30,*) x, gau/area
        x=x+dx
    enddo

    stop

end program
