program get_max

    bmax=0.d0
    do
        read(*,*,iostat=ios) a, b
        if (ios/=0) exit
        if (b>bmax) bmax=b
    enddo
    print*, bmax

    stop

end program

