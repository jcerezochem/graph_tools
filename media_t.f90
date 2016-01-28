program media_t

real::t,tini,tfin, data,data_av,stdv
integer::n,ntot
character:: null*1

! ntot : numero total de puntos suministrados
! n    : número de puntos que cumplen con los criterios

    ! Compilation instructions
    ! -------------------------
    !make$ ifort media_t.f90 -o media_t.exe

read(5,*) tini,tfin

! Convert time  limits to ns
tini=tini/1000.
if (tfin < 0) then
    tfin=1000000
else
    tfin=tfin/1000.
endif

data_av=0
n=0
ntot=0
do while (data/=-1.)
    ntot=ntot+1
    read(5,*) t, data 
    if ((t>=tini).and.(t<=tfin)) then
        n=n+1
        data_av=data_av+data
    endif
enddo
ntot = ntot - 1
data_av=data_av/float(n)

rewind(5)
read(5,*) null, null

stdv=0.
do i = 1, ntot
 read(5,*) t, data
 if ((t>=tini).and.(t<=tfin)) then
  stdv = stdv + (data - data_av)**2
 endif
enddo
stdv = sqrt(stdv / float(n))


write(*,'(i10,2X,f5.3,X,f8.5)') ntot, data_av, stdv

end program media_t
