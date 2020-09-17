	dimension nstm(120)	
	real,dimension(120) :: dlat	!! latitude of the stations
	real,dimension(120) :: rastst
        real,dimension(119) :: sum_u
	dimension d_max(119)
        real Dep1,Pres1,Temp1,Sol1,Sv1,Dep2,Pres2,Temp2,Sol2,Sv2
        real ro,lat_mid,r_mid,dr,gg,ff,deltau

	character*72 from
	character*72 from1
	character*72 from2
	character*72 tofile
	character*5 nstchr
	
        nt = 120
	do nst=1,120
	 nstm(nst)=nst !в массиве номера всех станций
	enddo
	

	open(2,file = '..\a10_2011asu.sum')
	do k=1,nt
	 read(2,*)Nn2, rastst(k), dlat(k), Dolg
	enddo
	close(2) 

	open(4, file = 'rashod\rast_ot_depth\dep_stations.txt')
	do k=1,nt-1
	 read(4,*)st1,st2,d_min,d_max(k),r1,r2
         !write(*,*)d_max(k),int(d_max(k))         
        enddo
        close(4)
         !!максимальная глубина между станциями
        
c ----------------------------------------------
        ! посчитать дельта u 
	
        ro = 1035.0 
	!deltaz = 1.
	from1='..\ctd\****_depth.txt'
	from2='..\ctd\****_depth.txt'
	tofile='delta_u\****-****_dep_deltau.txt'

	open(5,file='summau_for1station.txt')
	do k=1,nt-1 !для всех файлов по очереди
 
	 sum_u(k)=0.0
         lat_mid = (dlat(k)+dlat(k+1))*0.5
         gg=GGG(lat_mid)
         ff=FFF(lat_mid)
         r_mid = (rastst(k)+rastst(k+1))*0.5
         dr = rastst(k+1)-rastst(k)

	 write(nstchr,'(i5)') 10000+nstm(k)
	 from1(8:11)=nstchr(2:5) !вставляем вместо звёздочек
	 tofile(9:12)=nstchr(2:5)
	 write(nstchr,'(i5)') 10000+nstm(k+1)
	 from2(8:11)=nstchr(2:5) !вставляем вместо звёздочек
	 tofile(14:17)=nstchr(2:5)
	 !write(*,*)from1,from2,tofile
	 open(1,file=from1)
	 open(2,file=from2)
	 open(3,file=tofile)
	 
	 do j=1,40000 
	  read(1,*,end=123)Dep1,Pres1,Temp1,Sol1,Sv1
	  read(2,*,end=123)Dep2,Pres2,Temp2,Sol2,Sv2
 	  deltau = gg*(Sv2-Sv1)/(ff*ro*dr*1000)
	  sum_u(k)=sum_u(k)+deltau
	  write(3,98)Dep1,deltau,r_mid
	 enddo
98	format(f7.1, f16.12, f13.6)
123	 close(1); close(2)
         do while (Dep1 <= d_max(k)+1)
	  Dep1 = Dep1 + 1
          write(3,98)Dep1,0., r_mid
	 enddo   !заполнить нулями до макс глубины
	 
	 close(3)
	 write(5,77)k,sum_u(k)*100 !сумма в см/с
77      format(i6,f12.7)
	enddo
	close(5)
c ------------------------------------------------------------	
	! бароклинный профиль
99      format(f13.6, f8.1, f12.7)
	from='delta_u\****-****_dep_deltau.txt'
	tofile='u_barocl\****-****_u_barocl.txt'	

	do k=1,nt-1 !для всех файлов по очереди
         r_mid = (rastst(k)+rastst(k+1))*0.5

	 write(nstchr,'(i5)') 10000+nstm(k)
	 from(9:12)=nstchr(2:5) !вставляем вместо звёздочек
         tofile(10:13)=nstchr(2:5)
	 write(nstchr,'(i5)') 10000+nstm(k+1)
	 from(14:17)=nstchr(2:5) !вставляем вместо звёздочек
         tofile(15:18)=nstchr(2:5)
	 open(9,file=from)
         open(11,file=tofile)
	 
	 
	 write(11,99)r_mid,0.0,sum_u(k)*(100)
	 do j=1,60000 !для остальных
          read(9,*,end=12)Dep1,deltau,rr
	  sum_u(k) = sum_u(k)-deltau
	  write(11,99)r_mid,Dep1,sum_u(k)*(100)	  
	 enddo

12	 close(11);close(9)
         !write(*,*)k,sum_u(k)*(100) !!скорость у дна
	enddo
	
	
	stop
	end

	real function GGG(lat_mid) !g и коэф Кориолиса
	 real lat_mid,XX
         XX=SIN(lat_mid/57.29578)
         XX=XX*XX
         GGG=9.780318*(1.+(5.2788E-3+2.36E-5*XX)*XX)
	return
	end

	real function FFF(lat_mid) !g и коэф Кориолиса
	 real lat_mid
         FFF=2.*(7.27e-5)*SIN(lat_mid/57.29578)
	return
	end