program hist2min

    ! Get the global min fromt histieresis cycle
    ! obtain with mm_scan.sh -histieresis

    character(len=200) :: line
    real(8),dimension(200) :: angle, E, angle_unique, E_unique
    logical :: is_done

    i = 0
    do 
        read(*,'(A)',iostat=iostatus) line
        if (iostatus /= 0) exit
        if ( INDEX(line,'#') == 1 ) cycle

        i=i+1
        read(line,*) angle(i), E(i)
        if ( abs(angle(i)) == 180.d0 ) angle(i) = abs(angle(i))

    enddo
    N = i

    k = 0
    Nunique = 0
    do i=1,N
        is_done = .false.
        do kk=1,k
            if ( angle_unique(kk) == angle(i) ) is_done = .true.
        enddo
        if (is_done) cycle

        k = k+1
        angle_unique(k) = angle(i)
        E_unique(k) = E(i)

        do j=i+1,N
            if ( angle(i) == angle(j) ) then
                E_unique(k) = min(E(i),E(j))
            endif
        enddo

       if (angle_unique(k) == 180.d0 ) then
           k=k+1
           angle_unique(k) = -180.d0
           E_unique(k) = E_unique(k-1)
       endif

    enddo
    N = k

    do i=1,N

        print*, angle_unique(i), E_unique(i)

    enddo

        
    stop

end program hist2min

