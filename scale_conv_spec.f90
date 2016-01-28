program scale_spc

character(len=100) :: line
real :: MaxInt, factor
real,dimension(10000) :: Int,E
integer :: IOS

read(5,*) factor

MaxInt=0.0
n=0
do
    read(5,'(A)',iostat=IOS) line
    if (IOS /= 0) exit
    n=n+1
    read(line,*) E(n), Int(n)
    if (Int(n) > MaxInt) MaxInt=Int(n)
enddo

do i=1,n
    Int(i) = Int(i)/MaxInt*factor
    print*, E(i), Int(i)
enddo

stop

end program scale_spc

