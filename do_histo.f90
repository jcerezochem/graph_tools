program histo_real 

    real,dimension(1:10000) :: intBin
    real :: intens, Norm
    logical :: correct_LS=.false., &
               weight=.true.,      &
               centers=.false.

    !Read labels stuff
    logical :: argument_retrieved
    character(len=200) :: arg

    !-------------------------

    !Default binwidth
    delta_freq = 0.025 !eV
    !Read binwidth from label
    argument_retrieved=.false.
    do i=1,iargc()
        if (argument_retrieved) then
            argument_retrieved=.false.
            cycle
        endif
        call getarg(i, arg)
        select case (adjustl(arg))
            case ("-bw")
                call getarg(i+1, arg)
                read(arg,*) delta_freq
                argument_retrieved=.true.
            case ("-ls")
                !This option corrects intensities for LS (abs)
                correct_LS=.true.
                write(0,*) "Intensities will be corrected to get LS (abs)"
            case ("-nw")
                !This option skip weigting every value with the intensity (setting all to one)
                weight=.false.
                write(0,*) "No weights will be applied"
            case ("-centers")
                !This option skip weigting every value with the intensity (setting all to one)
                centers=.true.
                write(0,*) "No explicit bars, only centers will be printed"

            case default
                write(0,*) "Unknown label ignored:", trim(adjustl(arg))
        end select
    enddo

    !First read to get the freq range
    freqi = 10000000.
    freqf = -10000000.
    n=0
    do

        
        read(5,*,iostat=ios) freq
        if (ios /= 0) exit
        n=n+1   
        freqi = min(freq,freqi)
        freqf = max(freq,freqf)

    enddo
    write(0,*) "N data read: ", n
    !Enlarge limits 1%
    write(0,*) "Data from ", freqi, " to ", freqf
    delta_tot=freqf-freqi
    delta_enlarge = (delta_tot*1.01-delta_tot)/2.d0
    freqi=freqi-delta_enlarge
    freqf=freqf+delta_enlarge

!    freqi=3.145
!    freqf=3.655
    write(0,*) "Estimated range: ", freqi, " to ", freqf
    write(0,'(X,A,f8.3)') "Requested (hard wired) bin width: ", delta_freq
!    Nbins=20

    !Compute the number of bins
    Nbins = int(aint((freqf-freqi)/delta_freq)) + 1 
    write(0,*) "Will use", Nbins, " bins"

    !Reset the bounds
    range_freq = delta_freq*float(Nbins-1)
    residual = (range_freq - (freqf-freqi))/2.
    freqi = freqi - residual
    freqf = freqf + residual
    write(0,*) "With range: ", freqi, " to ", freqf, "(", freqf-freqi, ")"

    binwdth  = (freqf - freqi)/float(Nbins-1)
    write(0,'(X,A,f8.3)') "Actual bin width: ", binwdth
    !Define bin range to center the bins over the input values
    bini = freqi - binwdth/2.d0
    binf = freqf + binwdth/2.d0


    !Filling bins
    intBin(1:Nbins) = 0.d0    
    rewind(5)
    do

    read(5,*,iostat=ios) freq, intens
    if (ios /= 0) exit

    !Tune the weighting behaviour
    if (.not.weight) then
        intens=1.
    elseif (correct_LS) then
        intens=intens/freq
    endif

    ibin = ((freq-bini)/(binf-bini)) * Nbins + 1
    
    intBin(ibin) = intBin(ibin) + intens

    enddo


    !Normalize histogram
    Norm = 0.d0
    do i=1,Nbins
        Norm = Norm+intBin(i)*binwdth
    enddo


    if (.not.centers) then
        !Print the histogram (explicitely)
        do i=1,Nbins
            freq = freqi + dfloat(i-1)*binwdth
            print*, freq-binwdth/2, 0.d0
            print*, freq-binwdth/2, intBin(i)/Norm
            print*, freq+binwdth/2, intBin(i)/Norm
            print*, freq+binwdth/2, 0.d0
        enddo
    else
        !Print the histogram (only bin centers)
        freq=freqi
        print*, freq-binwdth/2, 0.d0
        do i=1,Nbins
            freq = freqi + dfloat(i-1)*binwdth
            print*, freq, intBin(i)/Norm
        enddo
        print*, freq+binwdth/2, intBin(i)/Norm
    endif



    stop

end program histo_real


