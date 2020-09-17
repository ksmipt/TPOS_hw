	!parameter (nstA=2196,nstB=2282)
	dimension nstm(120)
	dimension gg(120)
	dimension Rast(120)
      

	character*72 from
	character*72 from1
	character*72 tofile
	character*5 nstchr
	nt = 120
	from='****.smt'
	tofile='****_depth.txt'
	from1='a10_2011asu.sum'

	
	do nst=1,120
	 nstm(k)=nst !в массиве номера всех станций
	enddo

        open(2,file = from1)
	do k=1,nt
	 read(2,*)Nn, Rast(k), Shir, Dolg	 
	 gg(k) = GGG(Shir)
	enddo
	close(2)

	do k=1,nt !для всех файлов по очереди
	 write(nstchr,'(i5)') 10000+k
	 from(1:4)=nstchr(2:5) !вставляем вместо звёздочек
	 tofile(1:4)=nstchr(2:5)
	 open(1,file=from)
	 open(3,file=tofile)

	 read(1,*)Pres1,Temp1, Sol1, Ox1 ! 
	 Dep1=DEPTH(Pres1,gg(k))

         do j=1,40000 !для остальных
	  read(1,*,end=123)Pres2,Temp2,Sol2,Ox2
	  
	  Dep2=DEPTH(Pres2,gg(k))
          if (int(Dep1)==int(Dep2)) goto 11

	  
	  Temp = priam(Dep1,Dep2,Temp1,Temp2,int(Dep2)*1.0)	  
	  Sol = priam(Dep1,Dep2,Sol1,Sol2,int(Dep2)*1.0)
	  Pres = priam(Dep1,Dep2,Pres1,Pres2,int(Dep2)*1.0)
	  call svanet(Sol,Temp,Pres,Sv) !!!!!!!!! pressure
	  write(3,*)int(Dep2),Pres,Temp,Sol,Sv
	  Dep1 = Dep2; Temp1 = Temp2;Sol1 = Sol2;Pres1 = Pres2
11        continue
	 enddo
         


123	 close(1)
	 close(3)
	enddo

	stop
	end

	function DEPTH(P,G)
         GA=G+1.092E-6*P
	 DEP=(((-1.82E-15*P+2.279E-10)*P-2.2512E-5)*P+9.72659)*P
	 DEPTH=DEP/GA
	RETURN
	END

	function GGG(lat_mid) !g и коэф Кориолиса
         XX=SIN(lat_mid/57.29578)
         XX=XX*XX
         GGG=9.780318*(1.+(5.2788E-3+2.36E-5*XX)*XX)
	return
	end



	function priam(x1,x2,y1,y2,x)
         priam=y1+(x-x1)/(x2-x1)*(y2-y1)
	return
        end


	
      SUBROUTINE SVANET(S,T,P0,SIGMA)
      REAL A,B,C,D,E,A1,B1,AW,BW,K0,KW,K35,
     ,A_T,B_T,C_T,E_T,A1_T,B1_T,AW_T,BW_T,K0_T,KW_T,
     ,A_S,B_S,K0_S,KST
      DATA R3500/1028.1063/,R4/4.8314E-4/,DR350/28.106331/
      P=P0/10.
      SR=SQRT(ABS(S))
C
      R1=((((6.536332E-9*T-1.120083E-6)*T+
     +1.001685E-4)*T-9.095290E-3)*T+
     +6.793952E-2)*T-28.263737

      R2=(((5.3875E-9*T-8.2467E-7)*T+7.6438E-5)*T-
     -4.0899E-3)*T+8.24493E-1
      R3=(-1.6546E-6*T+1.0227E-4)*T-5.72466E-3
C
      R1_T=(((6.536332E-9*5*T-1.120083E-6*4)*T+               !T
     +1.001685E-4*3)*T-9.095290E-3*2)*T+                      !T
     +6.793952E-2                                             !T

      R2_T=((5.3875E-9*4*T-8.2467E-7*3)*T+7.6438E-5*2)*T-     !T
     -4.0899E-3                                               !T

      R3_T=-1.6546E-6*2*T+1.0227E-4                           !T

      SIG=(R4*S+R3*SR+R2)*S+R1
      SIG_T=(R3_T*SR+R2_T)*S+R1_T                             !T
      SIG_S=2*R4*S+1.5*R3*SR+R2                               !S
C
      V350P=1.0/R3500
      SVA=-SIG*V350P/(R3500+SIG)
      SVA_S=-SIG_S/(R3500+SIG)/(R3500+SIG)
      SVA_T=-SIG_T/(R3500+SIG)/(R3500+SIG)

      ALPHA=-SIG_T
      BETA=-SIG_S

      SIGMA=SIG+DR350
      KST=ABS(SIG_S/SIG_T)
C
      SV=SVA
      IF(P.EQ.0.0)RETURN

      E=(9.1697E-10*T+2.0816E-8)*T-9.9348E-7
      E_T=2*9.1697E-10*T+2.0816E-8

      BW=(5.2787E-8*T-6.12293E-6)*T+3.47718E-5
      BW_T=2*5.2787E-8*T-6.12293E-6

      B=BW+E*S
      B_T=BW_T+E_T*S
      B_S=E

      D=1.91075E-4

      C=(-1.6078E-6*T-1.0981E-5)*T+2.2838E-3
      C_T=-1.6078E-6*2.*T-1.0981E-5

      AW=((-5.77905E-7*T+1.16092E-4)*T+1.43713E-3)*T-0.1194975
      AW_T=(-5.77905E-7*3.*T+1.16092E-4*2.)*T+1.43713E-3

      A=(D*SR+C)*S+AW
      A_T=C_T*S+AW_T
      A_S=1.5*D*SR+C

      B1=(-5.3009E-4*T+1.6483E-2)*T+7.944E-2
      B1_T=-5.3009E-4*2.*T+1.6483E-2

      A1=((-6.1670E-5*T+1.09987E-2)*T-0.603459)*T+54.6746
      A1_T=(-6.1670E-5*3.*T+1.09987E-2*2.)*T-0.603459

      KW=(((-5.155288E-5*T+1.360477E-2)*T-2.327105)*T+
     +148.4206)*T-1930.06
      KW_T=((-5.155288E-5*4.*T+1.360477E-2*3.)*T-2.327105*2.)*T+148.4206

      K0=(B1*SR+A1)*S+KW
      K0_T=(B1_T*SR+A1_T)*S+KW_T
      K0_S=1.5*B1*SR+A1

      DK=(B*P+A)*P+K0
      DK_P=2.*B*P+A
      DK_T=(B_T*P+A_T)*P+K0_T
      DK_S=(B_S*P+A_S)*P+K0_S

      K35=(5.03217E-5*P+3.359406)*P+21582.27
      K35_P=2.*5.03217E-5*P+3.359406
      GAM=P/K35
      GAM_P=1./K35-P*K35_P/(K35*K35)
      PK=1.0-GAM
      PK_P=-GAM_P

      SVA_P=SVA*PK_P+
     +((V350P+SVA)*DK+(V350P+SVA)*P*DK_P)/(K35*(K35+DK))-
     -(V350P+SVA)*P*DK*(2*K35*K35_P+K35*DK_P+K35_P*DK)/
     /(K35*K35*(K35+DK)*(K35+DK))
      SVA_S=SVA_S*PK+SVA_S*P*DK/(K35*(K35+DK))+
     +DK_S*(V350P+SVA)*P/(K35+DK)/(K35+DK)
      SVA_T=SVA_T*PK+SVA_T*P*DK/(K35*(K35+DK))+
     +DK_T*(V350P+SVA)*P/(K35+DK)/(K35+DK)
      SVA=SVA*PK+(V350P+SVA)*P*DK/(K35*(K35+DK))
C
      SV=SVA
      V350P_P=V350P*PK_P
      V350P=V350P*PK
      DR35P_P=GAM_P/V350P-GAM*V350P_P/(V350P*V350P)
      DR35P=GAM/V350P

      DVAN_P=SVA_P/(V350P*(V350P+SVA))-
     -SVA*(2.*V350P*V350P_P+V350P*SVA_P+V350P_P*SVA)/
     /(V350P*(V350P+SVA)*V350P*(V350P+SVA))
      DVAN_S=SVA_S/(V350P+SVA)/(V350P+SVA)
      DVAN_T=SVA_T/(V350P+SVA)/(V350P+SVA)
      DVAN=SVA/(V350P*(V350P+SVA))
C
      SIGMA_P=DR35P_P-DVAN_P
      SIGMA_S=-DVAN_S
      SIGMA_T=-DVAN_T
      SIGMA=DR350+DR35P-DVAN

      KST=ABS(SIGMA_S/SIGMA_T)
      AdGrPr=SIGMA_P
      ALPHA=-SIGMA_T
      BETA=-SIGMA_S

      RETURN
      END
