program eV2cm1_grace

    character(len=100) :: line, line2

    do 
        read(5,'(A)',iostat=ios) line
        if (ios /= 0) exit
        !===================
        if (index(line,"with string")/=0) then
            print'(A)', line
            read(5,'(A)') line
            print'(A)', line
            read(5,'(A)') line
            if (index(line,"world")/=0) then
                print'(A)', line
                read(5,'(A)') line
                print'(A)', line
                !Now read position
                read(5,*) line, line, x, y
                x=x/1.23981d-4
                print'(A,F15.7,A,F15.7)', "@    string ", x,",",y
            else
                print'(A)', line
            endif
        !=======================
        elseif (index(line,"with line")/=0) then
            print'(A)', line
            read(5,'(A)') line
            print'(A)', line
            read(5,'(A)') line
            if (index(line,"world")/=0) then
                print'(A)', line
                read(5,'(A)') line
                print'(A)', line
                !Now read position
                read(5,*) line, line, x1, y1, x2, y2
                x1=x1/1.23981d-4
                x2=x2/1.23981d-4
                print'(5(A,F15.7))', "@    line ", x1,",",y1,",",x2,",",y2
            else
                print'(A)', line
            endif
        !=======================
        elseif (index(line,"with box")/=0) then
            print'(A)', line
            read(5,'(A)') line
            print'(A)', line
            read(5,'(A)') line
            if (index(line,"world")/=0) then
                print'(A)', line
                read(5,'(A)') line
                print'(A)', line
                !Now read position
                read(5,*) line, line, x1, y1, x2, y2
                x1=x1/1.23981d-4
                x2=x2/1.23981d-4
                print'(5(A,F15.7))', "@    box ", x1,",",y1,",",x2,",",y2
            else
                print'(A)', line
            endif
        !===========================
        elseif (index(line,"Energy (eV)")/=0) then
            print'(A)', '@    xaxis  label "Energy (cm\S-1\N)"'
        !===========================
        elseif (index(line,"@    world")/=0) then
            read(line,*) line2,line2, x1, y1, x2, y2
            x1=x1/1.23981d-4
            x2=x2/1.23981d-4
            print'(5(A,F15.7))', "@    world ", x1,",",y1,",",x2,",",y2
        !===========================
        elseif (index(line,"@target")/=0) then
            print'(A)', line
            read(5,'(A)') line
            print'(A)', line
            do 
                read(5,'(A)') line
                if (index(line,"&")/=0) exit
                read(line,*) x, y
                x=x/1.23981d-4
                print'(2(X,F15.7))', x,y
            enddo
        !===========================
        else
            print'(A)', line
        endif
    enddo

    stop

end program



