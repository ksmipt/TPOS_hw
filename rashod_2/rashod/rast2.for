	dimension nstm(120)
	dimension rastst(120)
        dimension dmin(119)
        dimension dmax(119)
	character*72 from
	character*72 from1
	character*72 tofile
        character*72 tofile1
	character*5 nstchr
	open(99, file='vivod2.txt')

        !порядок станций и их расстояние
	nt=120

        open(1,file='rast_ot_depth\dep_stations.txt')
	
	do k=1,nt-1
	 read(1,*)nstm(k),nstm(k+1),dmin(k),dmax(k),rastst(k),rastst(k+1)
	enddo
	close(1)


!-------------------------------------------------
      
        from='rast_ot_depth\****-****_dep1.txt'
        tofile='rast_ot_depth\****-****_rast_ot_depth.txt' 
    
        do k=1,nt-1
	 write(nstchr,'(i5)') 10000+nstm(k)
	 from(15:18)=nstchr(2:5)
	 tofile(15:18)=nstchr(2:5)
         write(nstchr,'(i5)') 10000+nstm(k+1)
	 from(20:23)=nstchr(2:5)
	 tofile(20:23)=nstchr(2:5)
	 	 
         open(5,file=tofile)

	 do i=int(dmin(k)),int(dmax(k))+1
	  open(7,file=from) !снова открываем созданный файл
	  ras=0.
	  read(7,*)r1,dep1
	
	  do j=1,40000
	   read(7,*,end=200)r2,dep2
	   if (dep1>=i.and.dep2>=i) then
	    ras=ras+(r2-r1)
           endif
           if (dep1>i.and.dep2<i) then
	     ras=ras+(r2-r1)/2   
           endif
           if (dep2>i.and.dep1<i) then
	     ras=ras+(r2-r1)/2   
           endif
	   r1=r2
	   dep1=dep2
	  enddo
200	  write(5,*)i,ras
	  close(7)          
         enddo

         close(5)
	enddo


        close(99)
	stop
        end