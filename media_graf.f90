program media_graf

! Media de dos curvas

real:: t1, t2, msd_e1_1, msd_e2_1, msd_e1_2, msd_e2_2, msd_e1, msd_e2
integer:: I_graf1, I_graf2, O_graf
character(20):: grafico1, grafico2, grafico_out

read(5,*) grafico1
read(5,*) grafico2
read(5,*) grafico_out

!INPUT
I_graf1=10
I_graf2=11
open(I_graf1,file=grafico1,status='old')
open(I_graf2,file=grafico2,status='old')
!OUTPUT
O_graf=20
open(O_graf,file=grafico_out,status='replace')


read(I_graf1,*) t1, msd_e1_1, msd_e2_1
read(I_graf2,*) t2, msd_e1_2, msd_e2_2
do while ( (t1/=-1.0) .and. (t2/=-1.0) )
    if (t1/=t2) then
        write(6,*) 'ERROR: Los tiempos no son iguales'
        write(6,*) 't1=', t1, 't2=', t2
        stop
    endif
    msd_e1 = (msd_e1_1+msd_e1_2)/2.
    msd_e2 = (msd_e2_1+msd_e2_2)/2.
    write(O_graf,'(x,f10.3,2(2x,f10.6))') t1, msd_e1, msd_e2
    read(I_graf1,*) t1, msd_e1_1, msd_e2_1
    read(I_graf2,*) t2, msd_e1_2, msd_e2_2
enddo

end program media_graf