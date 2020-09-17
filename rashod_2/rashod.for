        dimension nstm(120)
	dimension rastst(120)
        dimension dmin(119)
        dimension dmax(119)
	character*72 from
	character*72 from1
        character*72 from2
	character*72 tofile1
        character*72 tofile2
	character*5 nstchr
	open(99, file='rashody.txt')

        !порядок станций и их расстояние
	nt=120

        open(1,file='rashod\rast_ot_depth\dep_stations.txt')
	
	do k=1,nt-1
	 read(1,*)nstm(k),nstm(k+1),dmin(k),dmax(k),rastst(k),rastst(k+1)
	enddo
	close(1)
      
        from1='u\****-****_u.txt'
        from2='rashod\rast_ot_depth\****-****_rast_ot_depth.txt' 
        tofile1='rashod\rashod.txt'
        !open(5,file=tofile1)

        sum_pol=0.
        sum_otr=0.
        

!---------------------------------------------------------------------
        do k=1,nt-1
	 write(nstchr,'(i5)') 10000+nstm(k)
	 from1(3:6)=nstchr(2:5)
         from2(22:25)=nstchr(2:5)
         write(nstchr,'(i5)') 10000+nstm(k+1)
         from1(8:11)=nstchr(2:5)
	 from2(27:30)=nstchr(2:5)	 
	 open(2,file=from1)
         open(3,file=from2)
         

         sum_pol_st=0.
         sum_otr_st=0.

         read(3,*)min_d,rmax

         do i=1,min_d 
	  read(2,*)rasst,d,s
          if (s>0) then
	   sum_pol=sum_pol+s*rmax
	   sum_pol_st=sum_pol_st+s*rmax
	  endif
	  if (s<0) then
	   sum_otr=sum_otr+s*rmax
	   sum_otr_st=sum_otr_g+s*rmax
	  endif
         enddo
         !write(*,*)i,min_d,d,rmax
         

         r1=rmax
         do i=1,400000
          read(2,*,end=45)rasst,d,s
          read(3,*,end=45)d2,r2
          !write(*,*)i,d,d2,r2,r1
          if (s>0) then
	   sum_pol=sum_pol+s*(r1+r2)*0.5
	   sum_pol_st=sum_pol_st+s*(r1+r2)*0.5
	  endif
	  if (s<0) then
	   sum_otr=sum_otr+s*(r1+r2)*0.5
	   sum_otr_st=sum_otr_g+s*(r1+r2)*0.5
	  endif
          r1=r2
         enddo


45       close(2)
         close(3)
         write(99,*)nstm(k),nstm(k+1),sum_pol_st,sum_otr_st
        enddo

        write(99,*)sum_pol,sum_otr
        close(99)	
	write(*,*)sum_pol,sum_otr,sum_pol+sum_otr
	stop
	end
	


