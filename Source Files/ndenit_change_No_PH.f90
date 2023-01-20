      subroutine ndenit_Qi(k,j,cdg,wdn,void,respc)
	use parm
	use parm_subC
	use parm_control
	implicit none
	
	integer :: k, j
	real :: cdg, wdn, void, respc
	real :: fDco2
	
	!!function declairaiton 
	real diffusiv
	real atan
	!! local variables
      real :: fDno3
      real :: fDwfps
      real :: Dtotflux
      real :: fRno3_co2
      real :: fRwfps
      real :: ntotflux
      real :: n2oflux
      real :: grams_soil, sol_cmass
      real :: nitratePPM
      real :: co2_correction
      real :: co2PPM 
      real :: k1, M
      real :: dD0_fc
      real :: x_inflection
      real :: WFPS_threshold
      real :: Dn2oflux
      real :: Dn2flux
      real :: WFPS
      real, dimension(4):: BB
      real :: PI
    
      real :: vof
      real :: tem_no
      real :: krain_no  !! rain impacts on n2o emission
      real :: dDO
      real :: WFPS_dDO
      real :: N2flux
      real :: Fr_NO3, Fr_wfps, Fr_pH
      real :: r0,cw,cn,dn,nc, Fo, Dno, Fn, Ft, Fsw, Fclay, Fdepth, Fr_C
      real :: Fd_pH,Fd_C,Fd_solt, a, b, c, d, Nden_total,Fd_NO3,Fd_wfps
      real :: xx
      real:: aa, bbb, cc,dd
      
      !!sol_cmass |kg c/ha  |amount of carbon stored in the soil layer
    
       n2oflux = 0.
       k1=0.
     
       M=0.

	  Dn2oflux = 0.0
      Dn2flux = 0.0
      PI = 3.1415926536
      tem_no = 0.
      WFPS_dDO = 0.
      
 
      !! calculate nitrate concentration in PPM (mg/kg)
	nitratePPM=0.
	grams_soil=0.
    !grams_soil = sol_bd(k,j) * sol_thick(k,j)*0.1 * 100.0 * 100.0	!!!gram per m2  Qi  
	grams_soil = sol_bd(k,j) * sol_thick(k,j)*0.1 * 100.0 * 100.0  & 
        *(1-sol_rock(k,j)/100.)	!!!gram per m2    
	nitratePPM = sol_no3(k,j)*0.1/grams_soil*1.0E6
	
	if (nitratePPM < min_nitrate(j))  then 
	return
	end if
	
	!! calculate water filled pore space m3/m3
	
      WFPS = 0.
      WFPS = (  (sol_st(k,j)+sol_wpmm(k,j)) / 		&
            (sol_ul(k,j) + sol_wpmm(k,j) ) ) ! *sol_por(k,j) 
    	
	!! calculate CO2 concentration in PPM
	co2PPM=0.
	co2PPM =  respc*0.1 / grams_soil * 1.0E6             
      co2PPM = co2PPM   !+ 380   !! need to double check. 
    ! co2PPM =  sol_cbn(k,j)  *0.1 / grams_soil * 1.0E6    
      !! calculate gas diffusivity at field capacity
      dD0_fc=0.
      dD0_fc = diffusiv((sol_fc(k, j)+sol_wpmm(k,j)) / sol_thick(k,j), sol_bd(k,j),  WFPS)

       
    !calculate soil C mass    
    ! convert grams_soil from g/m2 to kg/ha: grams_soil = 10 kg/ha
      
      sol_cmass = grams_soil * 10. * (sol_cbn(k,j)/100.)  !! by Liang 2022/10
      

      
        ! Denitrification Calculation -------------------------------------------BEGIN----------------------------
      
       ntotflux=0.    !! kg/ha day
       select case (denit_method)
       
        case(0)  ! Original SWAT algorithm
	  vof =0.
	  vof = 1. / (1. + (void/0.04)**5)
        ntotflux= sol_no3(k,j) * 			&
     	(1. - Exp(-cdn(j) * cdg * vof *sol_cbn(k,j) ))      !! kg/ha day
       !  ntotflux=0.1 *ntotflux      !! *0.1 will convert kg/ha day  to g/m2 day
         
        case(1) ! Daycent model based on Qichun Yang   ----Grosso et al 2000
        
        !! calculate co2_correction
        if (dD0_fc >= 0.15) then
           WFPS_threshold = 0.8
        else
           WFPS_threshold = (dD0_fc*250.0 + 43.0)/100.0
        endif  
        if (WFPS .le. WFPS_threshold) then
          co2_correction =  co2PPM
        else  
         if (dD0_fc >= 0.15) then
           aa = 0.004
         else
           aa = -0.1 * dD0_fc + 0.019
         end if  
           co2_correction = co2PPM * (1.0 + aa * &
	   		(WFPS - WFPS_threshold)*100)
        endif
      !! calculate denitrification flux 
        BB(1) = 9.23
        BB(2) = 1.556
        BB(3) = 76.91
        BB(4) = 0.00222
        fDno3 =BB(2) + (BB(3) / PI) *		&
                atan(PI * BB(4) * (nitratePPM - BB(1)))                  !! ugN /g soil day        should be <15 
        fDno3 = max(0.0, fDno3)
        fDco2 = max(0.0, (0.1 * co2_correction**1.3) - min_nitrate(j))       !! ugN /g soil day
        M = min(0.113, dD0_fc) * (-1.25) + 0.145
       x_inflection = (9.0 - M * co2_correction)
        !x_inflection = (0.9 - M * co2_correction)
        x_inflection = x_inflection * wfpsdnitadj(j)              !! Calibration
        fDwfps = (0.45 +(atan(0.6*PI*(10.0*WFPS - &
		x_inflection))) / PI)
        fDwfps = max(0.0, fDwfps) !! change later 
        Dtotflux = min(fDno3 , fDco2)
        if (k<3) then !! in daycent it is k<2, but may start from 0, so change to 3 here
          Dtotflux = max(0.066, Dtotflux)   
        endif
        
        Dtotflux = Dtotflux * fDwfps     
        ntotflux = Dtotflux * grams_soil * 1.0E-6    !!g N /m2 day                    = 0.000001* ugN /g soil day    *     g/soil /m2    
        ntotflux = 10* ntotflux    !! *10  will convert g/m2 to kg/ha 
      ! if (j==14 ) then
      ! print*, i,k,fDno3, fDco2, fDwfps,  ntotflux
    !  end if 
       
        case(2) !! --------replace sol_cmass (soil carbon mass)-of-Wagena et al 2017---with--respir CO2-------------------
         
        
       Fd_NO3 = 0.
       Fd_C= 0.
       Fd_wfps = 0.
       Fd_solt = 0.
       Fd_pH = 0.
       Nden_total = 0.
       a=0.
       b=0.
       c=0.
       d=0.
         
      Fd_NO3=11000. + (40000. + atan(PI*0.002*(nitratePPM-180))) / PI          !gN /ha day                       
      Fd_C= 24000/(1+ ( 200 / exp(0.35*respc) ) ) -100                         !gN /ha day
     
      
      if(sol_tex(k,j)==1 .or. sol_tex(k,j)==2)then     !sand
      a = 1.56
      b = 12.
      c = 16.
      d = 2.01
      else if(sol_tex(k,j)==10.or.sol_tex(k,j)==11  &   ! caly
                 .or. sol_tex(k,j)==12)then
      a = 60.
      b = 18.
      c = 22.
      d = 1.06
      else                                                                   !loam
      a = 4.82
      b = 14.0
      c = 16.0
      d = 1.39
      end if 
     
      Fd_wfps= a / ( b**( c / b**( d * WFPS ) ) )  
               
      Fd_solt= max (  (0.9 *  sol_tmp(k,j) / (sol_tmp(k,j) + exp(9.93-0.312*sol_tmp(k,j) ) ) +0.1),   0.1 )
      
      if(sol_ph(k,j)<= 3.5) then 
        Fd_pH = 0.001
      else if(sol_ph(k,j)> 3.5   .and. sol_ph(k,j)< 6.5 ) then
        Fd_pH = (sol_ph(k,j)-3.5 )/3.
      else 
      Fd_pH = 1.
      end if
      
      Nden_total= min (Fd_NO3, Fd_C)*Fd_wfps*Fd_solt*Fd_pH      !! gN/ha day
      ntotflux=Nden_total/1000          !! kgN/ha day

      case(3)     !! no denitrification
      
      wdn=0.
      
      case(4) !! --Wagena et al 2017---Denitrification ----sol_cmass----------
         
        
       Fd_NO3 = 0.
       Fd_C= 0.
       Fd_wfps = 0.
       Fd_solt = 0.
       Fd_pH = 0.
       Nden_total = 0.
       a=0.
       b=0.
       c=0.
       d=0.
         
      Fd_NO3=11000. + (40000. + atan(PI*0.002*(nitratePPM-180))) / PI          !gN /ha day                       
      !Fd_C= 24000/(1+ ( 200 / exp(0.35*respc) ) ) -100                         !gN /ha day
      Fd_C= 24000/(1+ ( 200 / exp(0.35*sol_cmass) ) ) - 100.                     !gN /ha day   by Liang 2022/10
     
      if(sol_tex(k,j)==1 .or. sol_tex(k,j)==2)then     !sand
      a = 1.56
      b = 12.
      c = 16.
      d = 2.01
      else if(sol_tex(k,j)==10.or.sol_tex(k,j)==11  &   ! caly
                 .or. sol_tex(k,j)==12)then
      a = 60.
      b = 18.
      c = 22.
      d = 1.06
      else                                                                   !loam
      a = 4.82
      b = 14.0
      c = 16.0
      d = 1.39
      end if 
     
      Fd_wfps= a / ( b**( c / b**( d * WFPS ) ) )  
               
      Fd_solt= max (  (0.9 *  sol_tmp(k,j) / (sol_tmp(k,j) + exp(9.93-0.312*sol_tmp(k,j) ) ) +0.1),   0.1 )
      
      if(sol_ph(k,j)<= 3.5) then 
        Fd_pH = 0.001
      else if(sol_ph(k,j)> 3.5   .and. sol_ph(k,j)< 6.5 ) then
        Fd_pH = (sol_ph(k,j)-3.5 )/3.
      else 
      Fd_pH = 1.
      end if
      
      Nden_total= min (Fd_NO3, Fd_C)*Fd_wfps*Fd_solt*Fd_pH      !! gN/ha day
      ntotflux=Nden_total/1000                                  !! kgN/ha day
      
      
      
      case(5) !! ------Century---Parton 1996-------
         
        
       Fd_NO3 = 0.
       Fd_C= 0.
       Fd_wfps = 0.
       Fd_solt = 0.
       Fd_pH = 0.
       Nden_total = 0.
       a=0.
       b=0.
       c=0.
       d=0.
         
      Fd_NO3=11000. + (40000. + atan(PI*0.002*(nitratePPM-180))) / PI          !gN /ha day                       
      !Fd_C= 24000/(1+ ( 200 / exp(0.35*respc) ) ) -100                         !gN /ha day
      Fd_C= 24000/(1+ ( 200 / exp(0.35*sol_cmass) ) ) - 100.                     !gN /ha day   by Liang 2022/10
     
      if(sol_tex(k,j)==1 .or. sol_tex(k,j)==2)then     !sand
      a = 1.56
      b = 12.
      c = 16.
      d = 2.01
      else if(sol_tex(k,j)==10.or.sol_tex(k,j)==11  &   ! caly
                 .or. sol_tex(k,j)==12)then
      a = 60.
      b = 18.
      c = 22.
      d = 1.06
      else                                                                   !loam
      a = 4.82
      b = 14.0
      c = 16.0
      d = 1.39
      end if 
     
      Fd_wfps= a / ( b**( c / b**( d * WFPS ) ) )  
                     
      Nden_total= min(Fd_NO3, Fd_C)*Fd_wfps     !! gN/ha day
      ntotflux=Nden_total/1000                                  !! kgN/ha day
      
      return
      
      end select
    
! Denitrification Calculation -------------------------------------------END----------------------
      
!! N2O calculation based on calculating the ratio of N2 to N2O production: Rn2_n2o----------------BEGIN-----------------------
      
      
      select case (denit_n2o_method)
      
       case (1)            !! Daycent  Qichun Yang--------- 
        if ( sol_st(k,j) > sol_fc(k,j) ) then     
           Rn2_n2o (k,j) = 100.0 
        else
           k1 = max(1.5, 38.4 - 350. * dD0_fc)
           fRno3_co2 = max(0.16 * k1, k1 * exp(-0.8 * nitratePPM/ &
	   	co2PPM))
           fRwfps = max(0.1, 0.015 * WFPS*100. - 0.32)            
           Rn2_n2o(k,j)  = fRno3_co2 * fRwfps
           if (Rn2_n2o(k,j)  < 0.1) Rn2_n2o (k,j) = 0.1          
        endif   
	 n2oflux = ntotflux / (Rn2_n2o(k,j)  + 1.0)     !!  kg/ha day
	 N2flux= ntotflux - n2oflux                            !!  kg/ha day
        
       case(2)    !! Daycent  Del Grosso et al., 2000   Modified based on Fang, et al. 2015 ----------------------
        k1 = max(1.5, 38.4 - 350. * dD0_fc)
        fRno3_co2 = max(0.16 * k1, k1 * exp(-0.8 * nitratePPM/ &
		co2PPM))
        fRwfps = max(0.1, 0.015 * WFPS*100. - 0.32)            
        Rn2_n2o(k,j)  = fRno3_co2 * fRwfps    
    !    WFPS_dDO = ( (sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j) )
    ! &          /sol_por(1,j)
    !    dDO =  diffusiv((sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j), 
    ! &                                           sol_bd(1,j), WFPS_dDO)
     
      !   Rno_n2o(k,j) =4.+(9.*atan((0.75*PI*(10.*dDO-1.86)))/PI)
     
       !n2oflux = ntotflux / (Rn2_n2o(k,j)+Rno_n2o(k,j)  + 1.0)      !!  kgN/ha day
	 n2oflux = ntotflux / (Rn2_n2o(k,j)  + 1.0)                              !! kgN/ha day
	 N2flux = ntotflux - n2oflux                                                     !! kgN/ha day
      

       case(3)   !!NOE model    Bessour et al. 2010 and Khalil et al. 2005-----------------  
        Fo = 0.
        Dno = 0.
        Fn =0.
        r0 = 0.63
        cw = 2.05
        cn = 0.44
        dn = 0.0015
        nc = 3.             !!mg/l
        Fo = 1 - cw * max(0., WFPS-0.62)
        Dno = (cn + dn * nc ) / nc
        Fn = min(Dno*nitratePPM , (cn + dn*nitratePPM), 1.)
        n2oflux =   r0 * Fn * Fo * ntotflux              !!  kgN/ha day
        N2flux = ntotflux - n2oflux                          !!  kgN/ha day
          
       case(4)  !! WNMM model -----------------------------
        if (WFPS >= 1.0) then
         n2oflux =  0.05 * ntotflux                            !!  kgN/ha day
        else 
        Fsw = exp(-23.77+23.77 * WFPS)
        n2oflux =  0.5 * (1 - Fsw) * ntotflux            !!  kgN/ha day
        end if 
        N2flux = ntotflux - n2oflux                         !!  kgN/ha day
       
       case(5) !! FASSET model -------------------------
      
          if (k == 1) then
            xx = 0.
          else
            xx = 0.
            xx = sol_z(k-1,j)
          endif
        Ft = 0.
        Fsw = 0.
        Fclay = 0.
        Fdepth = 0.
        aa=0.
        bbb=0.
        cc=0
        dd=0.
        Ft = 1./ (1.+ exp(-0.64 +0.08*sol_tmp(k,j)) )
        aa= min(1., 0.0116 + 1.36 / (1+exp(- (WFPS-0.815)/0.0896) )  )   
        Fsw = max(0.,  aa)
        bbb= min( 1., 1.26*exp(-0.0116*sol_clay(k,j)-0.249)  )
        Fclay = max(0., bbb)
        cc = ((sol_z(k,j) + xx) / 2.) / 1000.     !!soil depth  m 
        dd=min ( 1., 1.0008-0.0343*cc-3.186*cc**2  )                      
        Fdepth = max(0., dd )
        
        n2oflux =  Ft * Fclay * Fdepth * (1 - Fsw) * ntotflux  !!  kgN/ha day
        N2flux = ntotflux - n2oflux                            !!  kgN/ha day

   
      
       case(6)   !!Wagena et al 2017 -------resp CO2-------------------------

        Fr_NO3 = 0.
        Fr_C = 0.
        Fr_wfps = 0.
        Fr_pH = 0.
        Fr_NO3 = (0.5- (atan(3.1415926*0.01*(nitratePPM-190)) &
		/ 3.1415926))*25
        Fr_C = 13+ ( 30.78*atan(3.1415926*0.07*(respc -13)) &
		/ 3.1415926 ) 
        Fr_wfps = 1.4  /  (13.** ( 17 / 13**(2.2*WFPS)))  
        Fr_pH = 1./ (1470. *exp(-1.1*sol_ph(k,j) ) )
        Rn2_n2o(k,j) = min( Fr_NO3, Fr_C) * Fr_wfps * Fr_pH   
       
  
	  n2oflux = ntotflux / (Rn2_n2o(k,j)  + 1.0)    !!  kgN/ha day
	  N2flux= ntotflux - n2oflux                    !!  kgN/ha day
 
      
       case(7)   !!Wagena et al 2017 ------sol_mass---------------------------
    
        Fr_NO3 = 0.
        Fr_C = 0.
        Fr_wfps = 0.
        Fr_pH = 0.
        Fr_NO3 = (0.5- (atan(3.1415926*0.01*(nitratePPM-190)) &
		/ 3.1415926))*25
        
        Fr_C = 13+ ( 30.78*atan(3.1415926*0.07*(sol_cmass -13)) &           
		/ 3.1415926 )                                    
            if (Fr_C > 0.) then
                Fr_C = Fr_C
            else
               Fr_C = 1.e-6
            end if 
       
        Fr_wfps = 1.4  /  (13.** ( 17 / 13**(2.2*WFPS)))  
        Fr_pH = 1./ (1470. *exp(-1.1*sol_ph(k,j) ) )
        Rn2_n2o(k,j) = min( Fr_NO3, Fr_C) * Fr_wfps * Fr_pH   
       
  
	  n2oflux = ntotflux / (Rn2_n2o(k,j)  + 1.0)    !!  kgN/ha day
	  N2flux= ntotflux - n2oflux                    !!  kgN/ha day
      
      
       case(8)   !!---------Century--------Parton 1996 --------------
              
        Fr_NO3 = 0.
        Fr_C = 0.
        Fr_wfps = 0.
        Fr_NO3 = (0.5- (atan(3.1415926*0.01*(nitratePPM-190)) &
		/ 3.1415926))*25
        Fr_C = 13+ ( 30.78*atan(3.1415926*0.07*(respc -13)) &
		/ 3.1415926 ) 
        Fr_wfps = 1.4  /  (13.** ( 17 / 13**(2.2*WFPS)))  
        Rn2_n2o(k,j) = min( Fr_NO3, Fr_C) * Fr_wfps  
       
  
	  n2oflux = ntotflux / (Rn2_n2o(k,j)  + 1.0)    !!  kgN/ha day
	  N2flux= ntotflux - n2oflux                    !!  kgN/ha day
      
      end select
  
 !! N2O calculation based on calculating the ratio of N2 to N2O production:Rn2_n2o---------------END---------------------------
  
  

    !! NO calculation--------------------------------------------------------------BEGIN-----------------------------
      select case (denit_no_method)
      
      case(0)  !! do not calculate NO----------------
      tem_no  =0.                                                   !!  kgN/ha day
      
      case(1)     !  Qichun Yang------------------      
      
      ! WFPS_dDO = ( (sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j) )
    ! &          /sol_por(1,j)
      WFPS_dDO =(  (sol_fc(1, j)+sol_wpmm(1,j)) / 		&
            (sol_ul(1,j) + sol_wpmm(1,j) ) )   !*sol_por(k,j) 
     
      dDO =  diffusiv((sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j), sol_bd(1,j), WFPS_dDO)
      krain_no = 1.0
      Rno_n2o(k,j) = 8.0 + (18.0*atan(0.75*PI*(10.*dDO-1.86))) /PI
      if (idplt(j) == 4) Rno_n2o(k,j) = Rno_n2o(k,j)* 0.5 
      tem_no = Rno_n2o(k,j) * n2oflux  * krain_no      !!  kgN/ha day
     

      case(2)             !!DAYCENT used by Fang, et al. 2015----------------           
      !WFPS_dDO = ( (sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j) )
   ! &          /sol_por(1,j)
       WFPS_dDO =(  (sol_fc(1, j)+sol_wpmm(1,j)) / 		&
            (sol_ul(1,j) + sol_wpmm(1,j) ) )   !*sol_por(k,j) 
     
      dDO =  diffusiv((sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j), sol_bd(1,j), WFPS_dDO)
      Rno_n2o(k,j) = 4.+(9.*atan((0.75*PI*(10.*dDO-1.86)))/PI)                           !! only different in the coefficent need check the source\
       
      tem_no = Rno_n2o(k,j) * n2oflux                   !!  kgN/ha day
      end select 
      
 !! NO calculation-------------------------------------------------------------END-------------------------------------- 
 
 
 
        !! Adjust N2 and NO with N2O not changed
      if (tem_no .le.   N2flux ) then
               N2flux =  N2flux - tem_no              !!  kgN/ha day
      else 
               tem_no =  N2flux                          !!  kgN/ha day
               N2flux = 0.                                   !!  kgN/ha day
      endif  
       
        !! Update NO, N2O and N2
	if (tem_no .le. 1E-6) tem_no = 0.          !!  kgN/ha day
	  NO(j) = NO(j) + tem_no                    !!  kgN/ha day
	  NO_den(j)=NO_den(j)+ tem_no        !!  kgN/ha day
	  
	if (n2oflux .le. 1E-6) n2oflux = 0.             !!  kgN/ha day
	  N2O(j) = N2O(j) + n2oflux                   !!  kgN/ha day
	  N2O_den(j) = N2O_den(j) + n2oflux    !!  kgN/ha day
    
      if (N2flux.le. 1E-6) N2flux = 0.               !!  kgN/ha day
        N2_den(j)=N2_den(j)+N2flux              !!  kgN/ha day

      wdn = ntotflux 	                                	!!  kgN/ha	
	if(wdn < 0.) wdn = 0. 
	sol_no3(k,j) = sol_no3(k,j) - wdn            !!  kgN/ha day
	no3_denit(j) = no3_denit(j) + wdn           !!  kgN/ha day
	

	return
	end
