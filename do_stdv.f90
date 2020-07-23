program do_stdv

    real(8),parameter :: pi=4.d0*datan(1.d0)

    real(8) :: freq, av, stdv, av_l, av_u, stdv_l, stdv_u, delta_stdv, &
               intens,inttot,m1,m2

    n=0
    av=0.d0
    do

        read(5,*,iostat=ios) freq
        if (ios /= 0) exit
        n=n+1   
        av = av + freq

    enddo
    rewind(5)
    av=av/float(n)
    write(0,*) "N data read: ", n
    write(0,'(A,F12.5)') "Average = ", av
    stdv=0.d0
    do i=1,N

        read(5,*,iostat=ios) freq
        stdv = stdv + (freq-av)**2

    enddo
    rewind(5)
    stdv = dsqrt(stdv/float(n))
    write(0,'(50X,A,F10.4)') "Stdv(all)   = ", stdv

!   Lower/Upper set separation
    av_l=0.d0
    av_u=0.d0
    N_l=0
    N_u=0
    do i=1,N

        read(5,*,iostat=ios) freq
        if (i<=N/2) then
            av_l = av_l + freq
            N_l = N_l+1
        else
            av_u = av_u + freq
            N_u=N_U+1
        endif

    enddo
    rewind(5)
    av_l=av_l/float(N_l)
    av_u=av_u/float(N_u)
    write(0,*) "Lower data: ", N_l
    write(0,*) "Upper data: ", N_u

    write(0,*) "Average(lower) = ", av_l
    write(0,*) "Average(upper) = ", av_u
    stdv_l=0.d0
    stdv_u=0.d0
    do i=1,N

        read(5,*,iostat=ios) freq
        if (i<=N/2) then
            stdv_l = stdv_l + (freq-av_l)**2
        else 
            stdv_u = stdv_u + (freq-av_u)**2
        endif

    enddo
    rewind(5)
    stdv_l = dsqrt(stdv_l/float(N_l))
    stdv_u = dsqrt(stdv_u/float(N_u))
    write(0,'(50X,A,F10.4)') "Stdv(lower) = ", stdv_l
    write(0,'(50X,A,F10.4)') "Stdv(upper) = ", stdv_u
    delta_stdv=dabs(stdv_l-stdv_u)

!   Even/Odd separation
    av_l=0.d0
    av_u=0.d0
    N_l=0
    N_u=0
    do i=1,N

        read(5,*,iostat=ios) freq
        if (mod(i,2)==0) then
            av_l = av_l + freq
            N_l = N_l+1
        else
            av_u = av_u + freq
            N_u=N_U+1
        endif

    enddo
    rewind(5)
    av_l=av_l/float(N_l)
    av_u=av_u/float(N_u)
    write(0,*) "Even data: ", N_l
    write(0,*) "Odd  data: ", N_u

    write(0,*) "Average(even) = ", av_l
    write(0,*) "Average(odd ) = ", av_u
    stdv_l=0.d0
    stdv_u=0.d0
    do i=1,N

        read(5,*,iostat=ios) freq
        if (mod(i,2)==0) then
            stdv_l = stdv_l + (freq-av_l)**2
        else
            stdv_u = stdv_u + (freq-av_u)**2
        endif

    enddo
    rewind(5)
    stdv_l = dsqrt(stdv_l/float(N_l))
    stdv_u = dsqrt(stdv_u/float(N_u))
    write(0,'(50X,A,F10.4)') "Stdv(even)  = ", stdv_l
    write(0,'(50X,A,F10.4)') "Stdv(odd )  = ", stdv_u

    if (dabs(stdv_l-stdv_u) > delta_stdv) delta_stdv=dabs(stdv_l-stdv_u)

    write(0,'(A)') "======================================="
    write(0,'(A,F10.4,A,F10.4)') "Stdv(eV)  = ", stdv, "  +/-", delta_stdv/2.d0
    write(0,'(A)') "======================================="

    write(0,'(A)') ""
    write(0,'(A)') "For xmgrace plotting:"
    a=1./stdv**2/2.
    write(0,'(F8.3,A,F8.3,A,F8.3,A)') dsqrt(a/pi),"*exp(-",a,"*(x-",av, ")^2)" 

    write(0,'(A)') ""
    write(0,'(A)') "Further statistical analysis"
    write(0,'(A)') "taking into account the osc. strength"
    write(0,'(A)') ""
    
    av = 0.d0
    m1 = 0.d0
    m2 = 0.d0
    inttot = 0.d0
    do i=1,N

        read(5,*,iostat=ios) freq, intens

        inttot = inttot + intens
        m1 = m1 + intens*freq
        m2 = m2 + intens*freq**2

    enddo
    rewind(5)

    m1 = m1/inttot
    m2 = m2/inttot


    write(0,*) "    M1(eV)     M2(eV²)    sigma(eV)"
    write(0,"(3F11.4)") m1,m2,dsqrt(m2-m1**2)

    write(0,'(/,A,F10.4)') "Corresponding HWHM(eV) = ", stdv*dsqrt(2.d0*dlog(2.d0))
    write(0,*) ""

    stop

end program do_stdv


