program get_range

    read(*,*) a
    do
        read(*,*,iostat=ios) b
        if (ios/=0) exit
    enddo
    print*, a-1, b+1

    stop

end program

