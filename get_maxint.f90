program get_maxint

character(len=100) :: line
real :: Int, MaxInt, E
integer :: IOS

MaxInt=0.0
do 
    read(5,'(A)',iostat=IOS) line
    if (IOS /= 0) exit
    if (index(line,"#") /= 0) then
        cycle
    elseif (index(line,'&') /= 0) then
        cycle
    endif
    read(line,*) E, Int
    if (Int > MaxInt) MaxInt=Int
enddo

print*, MaxInt

stop

end program get_maxint

