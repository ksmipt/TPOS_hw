	dimension nstm(120)
	dimension rastst(120)
	character*72 from
	character*72 from1
	character*72 tofile
        character*72 tofile1
	character*5 nstchr
	open(99, file='vivod.txt')

        !порядок станций и их расстояние
	nt=120
	do k=1,120
	 nstm(k)=k!в массиве номера всех станций
	enddo

       
        
        open(1,file = '..\..\a10_2011asu.sum')
	
	do k=1,nt
	 read(1,*)Nn, rastst(k), Shir, Dolg
	enddo
	close(1)


!-------------------------------------------------
        !разделить файл с глубиной на промежутки между ст
        tofile='rast_ot_depth\****-****_dep.txt'
	open(2, file = '..\..\a10_2011_SmSan_01km.dat')
	read(2,*)

        k=1
        write(nstchr,'(i5)') 10000+nstm(1)
	tofile(15:18)=nstchr(2:5)
	write(nstchr,'(i5)') 10000+nstm(2)
	tofile(20:23)=nstchr(2:5)
	open(3,file=tofile)
        
	do j=1,4000000
	 read(2,*,end=128)R,Sh,D,depp,sm
	 if (R>=rastst(k).and.R<=rastst(k+1)) then
	   write(3,*)R,depp
	 else 
           close(3)
           k=k+1
           write(nstchr,'(i5)') 10000+nstm(k)
	   tofile(15:18)=nstchr(2:5)
	   write(nstchr,'(i5)') 10000+nstm(k+1)
	   tofile(20:23)=nstchr(2:5)
	   open(3,file=tofile)
           write(3,*)R,depp
         endif
	enddo
128	close(2)
       	close(3)
!----------------------------------------------
        !
        from='rast_ot_depth\****-****_dep.txt' !4
        tofile='rast_ot_depth\dep_stations.txt' !5
        tofile1='rast_ot_depth\****-****_dep1.txt' !6
        open(8,file=tofile)
        do k=1,nt-1 
	 write(nstchr,'(i5)') 10000+nstm(k)
	 from(15:18)=nstchr(2:5)
         tofile1(15:18)=nstchr(2:5)
         write(nstchr,'(i5)') 10000+nstm(k+1)
	 from(20:23)=nstchr(2:5)
         tofile1(20:23)=nstchr(2:5)
	 open(4,file=from)
         open(6,file=tofile1)
	 dmax=0.
	 dmin=10000.

	 read(4,*)r,dep
	 if (dep>dmax) then
	   dmax=dep
         endif
	 if (dep<dmin) then
	   dmin=dep
         endif
         if (r.ne.rastst(k)) then
           write(6,*)rastst(k),dep
         endif
         write(6,*)r,dep
	
	 do i=1,40000 
	  read(4,*,end=199)r,dep
          write(6,*)r,dep
	  if (dep>dmax) then
	   dmax=dep
          endif
	  if (dep<dmin) then
	   dmin=dep
          endif
	 enddo
	 
199      close(4)
         if (r.ne.rastst(k+1)) then
           write(6,*)rastst(k+1),dep
         endif
         close(6)     !find max and min for each station
         write(8,*)nstm(k),nstm(k+1),dmin,dmax,rastst(k),rastst(k+1)
        
         write(99,*)k,rastst(k+1)-rastst(k),dmin,dmax
     

         
	enddo
        close(8)

        close(99)
	stop
        end