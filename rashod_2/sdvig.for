        dimension u_pov(119)
        dimension dlat_st(120)
        dimension hhh(120)
	dimension sdvig(119)
        dimension rastst(120)
	character*72 from
	character*72 tofile
        character*72 tofile1
        character*5 nstchr
        nt = 120

	

	from='..\a10_2011asu.sum'
	open(1,file=from)
	do k=1,nt
	 read(1,*)n,rastst(k),dlat_st(k),dlong
	 !write(*,*)n,rastst(k),dlat_st(k),FFF(dlat_st(k))
	enddo
	close(1)

        from='..\u_11_stm.dat'
	open(2,file=from)
	do k=1,nt
	 read(2,*)rast,w1,hhh(k),w2
	 !write(*,*)rast,hhh(k)
	enddo
	close(2)
	
        do k=2,nt  !!расчет скорости на поверхности
         dl=0.5*(dlat_st(k-1)+dlat_st(k))
         dr = rastst(k)-rastst(k-1)
	 u_pov(k-1)=GGG(dl)*(hhh(k)-hhh(k-1))/fff(dl)/dr
         u_pov(k-1)=u_pov(k-1)*(-1)/1000
	 !write(*,*)k,u_pov(k-1),dr
	enddo

!-------------------------------------------
        from='summau_for1station.txt'
        open(3,file=from)
        do k=1,nt-1
         read(3,*)kk,s
         sdvig(k)=u_pov(k)-s
         !write(*,*),kk,u_pov(k),s,sdvig(k)
        enddo
        close(3) 


!-----------------------------------------------
        from='u_barocl\****-****_u_barocl.txt'
	tofile='u\****-****_u.txt'
        tofile1='u_10m.txt'
        open(6,file=tofile1)	
99      format(f13.6, f8.1, f14.7)
	do k=1,nt-1 !для всех файлов по очереди
	 write(nstchr,'(i5)') 10000+k
	 from(10:13)=nstchr(2:5) !вставляем вместо звёздочек
         tofile(3:6)=nstchr(2:5)
	 write(nstchr,'(i5)') 10000+k+1
	 from(15:18)=nstchr(2:5) !вставляем вместо звёздочек
         tofile(8:11)=nstchr(2:5)
	 open(4,file=from)
         open(5,file=tofile) 
	 
	 do j=1,60000 !для остальных
          read(4,*,end=12)r,Dep,u_b
	  u = u_b+sdvig(k)
	  write(5,99)r,Dep,u
          if (mod(int(Dep),10)==0) then
	    write(6,*)r*(-1),Dep*(-1),u
	  endif	  
	 enddo

12	 close(4);close(5)
         
         if (mod(int(Dep),10).ne.0) then
	   write(6,*)r*(-1),Dep*(-1),u
	 endif         
         !write(*,*)k,r,Dep,u !!скорость у дна	
	enddo 
        close(6)


	stop
	end


       	function GGG(DLAT) !g и коэф Кориолиса
	 XX = SIN(DLAT/57.29578)
         f=2.*(7.27e-5)*XX
         XX=XX*XX
         GGG=9.780318*(1.+(5.2788E-3+2.36E-5*XX)*XX)
	return
	end

	function FFF(DLAT) !g и коэф Кориолиса
	 XX = SIN(DLAT/57.29578)
         FFF=2.*(7.27e-5)*XX
	return
	end