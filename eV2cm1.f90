program eps2cm1

    character(100) :: line

    do  
        read(5,'(A)',iostat=ios) line
        if (ios /= 0) exit
        if ( index(line,'#') /=0 .or. &
             index(line,'&') /=0) then
            write(6,'(A)') trim(line)
        else
            read(line,*) freq, rint
            freq = freq/1.23981d-4
            write(6,*) freq, rint
        endif
    enddo

    
    stop

end program 

