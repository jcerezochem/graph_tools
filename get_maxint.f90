program get_maxint

    character(len=100) :: line
    real :: Int, MaxInt, E, IntPrev, MaxInt0
    integer :: unt, IOS
    logical :: get_first_max
    integer :: npoints
    real,dimension(:),allocatable :: y
    !Input selections stuff
    logical :: argument_retrieved
    character(len=50) :: arg


    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-first")
                get_first_max=.true.
            case ("-h")
                print*, "Program to retrive the maximum from a plot"
                print*, "Input/output by standard chanels. "
                print*, "Flag options: "
                print*, "  -first           Get the first max encountered"
                print*, ""
                stop
            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo

    ! If we need to reprocess the data (so, rewind the file) we need to 
    ! copy the original stdin data into a scratch file
    if (get_first_max) then
       unt=30
       open(30,status='scratch')
       do
           read(5,'(A)',iostat=IOS) line
           if (IOS /= 0) exit
           if (len_trim(line) == 0) exit
           write(30,'(A)') line
       enddo
       rewind(30)
    else
        unt=5
    endif

    
    MaxInt=-9999.
    npoints=0
    do 
        read(unt,'(A)',iostat=IOS) line
        if (IOS /= 0) exit
        if (index(line,"#") /= 0) then
            cycle
        elseif (index(line,'&') /= 0) then
            cycle
        endif
        npoints=npoints+1
        read(line,*) E, Int
        if (Int > MaxInt) MaxInt=Int
    enddo

    ! New option: get the first max
    if (get_first_max) then
        rewind(unt)
        allocate(y(npoints))
        i=0
        do 
            read(unt,'(A)',iostat=IOS) line
            if (IOS /= 0) exit
            if (index(line,"#") /= 0) then
                cycle
            elseif (index(line,'&') /= 0) then
                cycle
            endif
            i=i+1
            read(line,*) E, y(i)
        enddo
        close(unt)
        ! Compute max of the running average
        MaxInt0=MaxInt
        MaxInt=-9999.
        IntPrev=-9999.
        do i=1,npoints/5-1
            ii=(i-1)*5+1
            Int = 0.0
            do j=ii,ii+4
                Int = Int + y(j)/5.
            enddo
            ! Get also the region
            if (Int > MaxInt) then
                MaxInt=Int
                ii_max=ii
            endif
            if ( IntPrev/=-9999. .and. Int<IntPrev .and. MaxInt > MaxInt0/100. ) exit
            IntPrev=Int
        enddo
        ! Now get the maximum from the actual set
        ! in the region from the run av max
        MaxInt=-9999.
        do i=ii_max-10,ii_max+10
            if (y(i) > MaxInt) MaxInt=y(i)
        enddo
    endif
    
    print*, MaxInt
    
    stop
    
end program get_maxint

