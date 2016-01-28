program do_stdv

    !Refined algorithm. If any point exceeds 7sigma, it is eliminated, and sigma recomputed

    real(8) :: freq, av, stdv, av_l, av_u, stdv_l, stdv_u, &
               xlim_max, xlim_min


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
    write(6,*) "N data read: ", n
    write(6,*) "Average = ", av
    stdv=0.d0
    do i=1,N

        read(5,*,iostat=ios) freq
        stdv = stdv + (freq-av)**2

    enddo
    rewind(5)
    stdv = dsqrt(stdv/float(n))
    write(6,'(50X,A,F10.4)') "Stdv(all)   = ", stdv

    xlim_max = av + 3.5d0*stdv
    xlim_min = av - 3.5d0*stdv

    Nref=0
    open(30,status="scratch")
    do i=1,N

        read(5,*,iostat=ios) freq
        if (freq<xlim_min .or. freq>xlim_max) then
            print*, "Warning, a point is eliminated (first round):"
            write(0,*) i
            print*, "x-mu =", freq-av
            cycle
        endif
        Nref=Nref+1
        write(30,*) freq
    enddo
    rewind(30)
    
!==================
    do while (Nref /= N)

        print*, ""
        print*, "Some points are out of 7sigma and are eliminated."
        print*, "Recomputing statistics..."
        print*, ""    
        N=Nref
        av=0.d0
        do i=1,N
  
            read(30,*) freq
            av = av + freq

        enddo
        rewind(30)
        av=av/float(n)
        write(6,*) "N data read: ", n
        write(6,*) "Average = ", av
        stdv=0.d0
        do i=1,N

            read(30,*) freq
            stdv = stdv + (freq-av)**2

        enddo  
        close(30)
        stdv = dsqrt(stdv/float(n))
        write(6,'(50X,A,F10.4)') "Stdv(all)   = ", stdv

        xlim_max = av + 3.5d0*stdv
        xlim_min = av - 3.5d0*stdv

        Nref=0
        open(30,status="scratch")
        do i=1,N

            read(5,*,iostat=ios) freq
            if (freq<xlim_min .or. freq>xlim_max) then
                write(0,*) "Warning, a point is eliminated:", i
                print*, "x-mu =", freq-av
                cycle
            endif
            Nref=Nref+1
            write(30,*) freq
        enddo
        rewind(30)

    enddo
!====================

    print*, ""
    print*, "--------------------------------"
    print'(X,A,F10.4)', "STDV =", stdv
    print*, "--------------------------------"

    stop

end program do_stdv


