program vector_files_sub

    ! Substraction column vectors written in two files

    real(8) :: x1,x2

    logical :: argument_retrieved
    character(len=50) :: arg

    integer :: I_F1=10, &
               I_F2=11
    character(len=100) :: file1, file2

    integer :: i, IOstatus

    if (iargc() < 1 ) then
        print*, "Usage:"
        print*, " vector_files_add file1 file2"
        stop
    endif
    call getarg(1, file1)
    call getarg(2, file2)

    open(I_F1,file=file1,iostat=IOstatus)
    if (IOstatus /= 0) then
        print*, "Error opening file "//trim(adjustl(file1))
        stop
    endif
    open(I_F2,file=file2,iostat=IOstatus)
    if (IOstatus /= 0) then
        print*, "Error opening file "//trim(adjustl(file2))
        stop
    endif

    do
        read(I_F1,*,iostat=IOstatus) x1
        if (IOstatus /= 0) exit
        read(I_F2,*,iostat=IOstatus) x2
        if (IOstatus /= 0) exit
        print*, x1-x2
    enddo

end program
