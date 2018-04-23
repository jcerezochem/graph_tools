program histo_real 

    real,dimension(1:10000) :: intBin
    real :: intens, Norm
    logical :: correct_LS=.false., &
               weight=.true., &
               explicit_plot=.false.

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
            case ("-explicit")
                !This option corrects intensities for LS (abs)
                explicit_plot=.true.
                write(0,*) "Histogram will be written explicitily to file"


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
    if (freqi == freqf) then
        freqi=freqi-delta_freq/2.01d0
        freqf=freqf+delta_freq/2.01d0
    else
        delta_tot=freqf-freqi
        delta_enlarge = (delta_tot*1.01-delta_tot)/2.d0
        freqi=freqi-delta_enlarge
        freqf=freqf+delta_enlarge
    endif

!    freqi=3.145
!    freqf=3.655
    write(0,*) "Estimated range: ", freqi, " to ", freqf
    write(0,'(X,A,f8.3)') "Requested (hard wired) bin width: ", delta_freq
!    Nbins=20

    !Compute the number of bins
    Nbins = int(aint((freqf-freqi)/delta_freq)) + 1 
    write(0,*) "Will use", Nbins, " bins"

    if (Nbins>1) then
        !Reset the bounds
        range_freq = delta_freq*float(Nbins-1)
        residual = (range_freq - (freqf-freqi))/2.
        freqi = freqi - residual
        freqf = freqf + residual
        write(0,*) "With range: ", freqi, " to ", freqf, "(", freqf-freqi, ")"
        binwdth  = (freqf - freqi)/float(Nbins-1)
        !Define bin range to center the bins over the input values
        Bini = freqi - binwdth/2.d0
        Binf = freqf + binwdth/2.d0
    else
        binwdth = delta_freq
        write(0,'(X,A,f8.3)') "Only one bin, centered at ", 0.5d0*(freqi+freqf)
        !Define bin range to center the bins over the input values
        Bini = 0.5d0*(freqi+freqf) - binwdth/2.d0
        Binf = 0.5d0*(freqi+freqf) + binwdth/2.d0
    endif
    write(0,'(X,A,f8.3)') "Actual bin width: ", binwdth


    !Filling bins
    intBin(1:Nbins) = 0.d0    
    rewind(5)
    do

        !Tune the weighting behaviour
        if (.not.weight) then
            read(5,*,iostat=ios) freq
            if (ios /= 0) exit
            intens=1.
        elseif (correct_LS) then
            read(5,*,iostat=ios) freq, intens
            if (ios /= 0) exit
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


    !Print the histogram (explicitely)
    if (explicit_plot) then
        do i=1,Nbins
            freq = freqi + dfloat(i-1)*binwdth
            print*, freq-binwdth/2, 0.d0
            print*, freq-binwdth/2, intBin(i)/Norm
            print*, freq+binwdth/2, intBin(i)/Norm
            print*, freq+binwdth/2, 0.d0
        enddo
    else
        do i=1,Nbins
            freq = freqi + dfloat(i-1)*binwdth
            print*, freq, intBin(i)/Norm
        enddo
    endif



    stop

end program histo_real


