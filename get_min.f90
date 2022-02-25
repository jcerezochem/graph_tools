program get_min

    bmin=1.e10
    do
        read(*,*,iostat=ios) a, b
        if (ios/=0) exit
        if (b<bmin) bmin=b
    enddo
    print*, bmin

    stop

end program

