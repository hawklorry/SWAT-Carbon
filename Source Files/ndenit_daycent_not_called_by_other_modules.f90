      subroutine ndenit_dct(k,j,cdg,wdn,void,respc)
!!    this subroutine computes denitrification 
	use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
	implicit none
	integer :: k,j
	integer aaa
	real :: cdg, wdn, void, respc, no2prod
	real :: fDco2
	
	!!function declairaiton 
	real diffusiv
	real atan
	!! local variables
	real :: aa
      real :: fDno3
      real :: fDwfps
      real :: Dtotflux
      real :: fRno3_co2
      real :: fRwfps
      !!real, dimension(4) :: A
      real :: ntotflux
      real :: n2oflux
      real :: grams_soil
      real :: nitratePPM
      real :: wfps_fc
      real :: co2_correction
      real :: Rn2n2o
      real :: n2ofrac, n2frac
      real :: excess
      !real :: min_nitrate   
      real :: min_nitrate_end
      real :: fluxout
      real :: co2PPM 
      real :: k1, M
      real :: dD0_fc
      real :: x_inflection
      real :: WFPS_threshold
      real :: ug_per_gram
      real :: grams_per_ug
      real :: CM_per_METER
      real :: Dn2oflux
      real :: Dn2flux
      real :: wfpslyr
      real :: swcfrac
      real :: porespace
      real, dimension(4):: BB
      !!real :: wfpsdnitadj  
      real :: M_PI
      !real :: PARTDENS
      
      real :: vof
	real :: tt1, tt2
	real :: no_n2o_ratio
	real :: tem_no
      real :: krain_no  !! rain impacts on n2o emission
      real :: ttem_no
      real :: dDO
      real :: swcfrac_dDO 
      real :: porespace_dDO
      real :: wfpslyr_dDO
      real :: PARTDENS 
      
	no_n2o_ratio = 0.
	!min_nitrate = 0.1   
      min_nitrate_end = 0.05
	ug_per_gram = 1.0E6
      grams_per_ug = 1.0E-6
	CM_per_METER = 100.0
	Dn2oflux = 0.0
      Dn2flux = 0.0
      wfpslyr = 0.0 
      swcfrac = 0.0
      porespace = 0.0
     !! wfpsdnitadj = 1.1 !! initial is 1.18  
      M_PI = 3.1415926536
      PARTDENS = 2.56
      
      tem_no = 0.
      krain_no  = 0.
      ttem_no = 0.
      swcfrac_dDO = 0. 
      porespace_dDO = 0.
      wfpslyr_dDO = 0.
      
      if (k == 1) then
	  cal_temp(1) = 0
	  cal_temp(3) = 0.
	  cal_temp(4) = 0. 
	end if

	swcfrac = (sol_fc(k, j)+sol_wpmm(k,j)) / sol_thick(k,j)
      porespace = 1.0 - sol_bd(k,j)/PARTDENS !! orginal unit for bulk density is g/cm3 in daycent. check if SWAT variable match this value
      wfpslyr = (sol_st(k, j)+sol_wpmm(k,j)) / sol_thick(k,j)/porespace

	grams_soil = sol_bd(k,j) * sol_thick(k,j)*0.1 * &
		 CM_per_METER * CM_per_METER
	
	nitratePPM = sol_no3(k,j)*0.1/grams_soil*ug_per_gram
	
	if (nitratePPM < min_nitrate(j)) go to 999   !! check if this function correctly
	 rtfr(k) = 1./sol_nly(j) !! change later
	 co2PPM =  respc*0.1 / grams_soil * ug_per_gram             
       co2PPM = co2PPM + 380   !! need to double check. 
       dD0_fc = diffusiv(swcfrac, sol_bd(k,j), &
       		wfpslyr)
     
        
       if (dD0_fc >= 0.15) then
           WFPS_threshold = 0.8
       else
           WFPS_threshold = (dD0_fc*250.0 + 43.0)/100.0
       endif  
         
      
        if (wfpslyr .le. WFPS_threshold) then
          co2_correction =  co2PPM
        else  
         if (dD0_fc >= 0.15) then
           aa = 0.004
         else
           aa = -0.1 * dD0_fc + 0.019
         end if  
           co2_correction = co2PPM * (1.0 + aa * &
	   	(wfpslyr - WFPS_threshold)*100)
        endif


        BB(1) = 9.23
        BB(2) = 1.556
        BB(3) = 76.91
        BB(4) = 0.00222

       
       fDno3 =BB(2) + (BB(3) / M_PI) *		&
                atan(M_PI * BB(4) * (nitratePPM - BB(1)))
        fDno3 = max(0.0, fDno3)
        fDco2 = max(0.0, (0.1 * co2_correction**1.3) - min_nitrate(j))    !! Calibration
        M = min(0.113, dD0_fc) * (-1.25) + 0.145
        x_inflection = (9.0 - M * co2_correction)
        x_inflection = x_inflection * wfpsdnitadj(j)
        fDwfps = (0.45 +(atan(0.6*M_PI*(10.0*wfpslyr - &
		x_inflection))) / M_PI)
        fDwfps = max(0.0, fDwfps) !! change later 
        Dtotflux = min(fDno3 , fDco2)
   
        if (k<3) then !! in daycent it is k<2, but may start from 0, so change to 3 here
          Dtotflux = max(0.066, Dtotflux)   
        endif
        
        Dtotflux = Dtotflux * fDwfps

        if (sol_st(k,j)> sol_fc(k,j)) then     
           Rn2n2o = 100.0 
        else
           k1 = max(1.5, 38.4 - 350. * dD0_fc)
           fRno3_co2 = max(0.16 * k1, k1 * exp(-0.8 * nitratePPM/ &
	   	co2PPM))
           fRwfps = max(0.1, 0.015 * wfpslyr*100. - 0.32)   
           Rn2n2o = fRno3_co2 * fRwfps
         
           if (Rn2n2o < 0.1) Rn2n2o = 0.1
                  
        endif   
        
        ! Denitrification calculation 
        select case (2)
        case(1)  ! Original SWAT algorithem
	 vof = 1. / (1. + (void/0.04)**5)
	 ntotflux= 0.1 * sol_no3(k,j) * 	&
     	(1. - Exp(-cdn (j) * cdg * vof *sol_cbn(k,j) ))  !! convert kg/ha to g/m2
        
        case(2) ! Daycent model
        
       ntotflux = Dtotflux * grams_soil * grams_per_ug
       end select
      
       cal_temp(4) = cal_temp(4) + ntotflux    	
       cal_temp(3) = cal_temp(3) + ntotflux  
	 n2oflux = ntotflux / (Rn2n2o + 1.0)  !!  g N/ m2 d
	
       
  !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !! calculated NO from N2O
      aaa = 4
      if (aaa>3) then
       swcfrac_dDO = (sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j) 
       porespace_dDO = 1.0 - sol_bd(1,j)/2.56    
       wfpslyr_dDO = swcfrac_dDO/porespace_dDO
       dDO =  diffusiv(swcfrac_dDO, sol_bd(1,j), wfpslyr_dDO)
       !!dDO = 0.
       krain_no = 1.0
       no_n2o_ratio = 8.0 + (18.0*atan(0.75*3.1415926*(10.*dDO-1.86))) &
       		/3.1415926   
     
        if (idplt(j) == 4) no_n2o_ratio = no_n2o_ratio* 0.5
        tem_no = no_n2o_ratio * n2oflux  * krain_no 
        !!tem_no = 0.
        if (tem_no .le. sol_nh4(k,j)*0.1) then
            sol_nh4(k,j) = sol_nh4(k,j) - tem_no*10.        
        else   
            ttem_no = tem_no - sol_nh4(k,j)*0.1
          if (ttem_no .le. n2oflux ) then
               n2oflux = n2oflux - ttem_no   
               tem_no = ttem_no +  sol_nh4(k,j) * 0.1
               sol_nh4(k,j) = 0.0 
          else
               tem_no = min (tem_no, sol_nh4(k,j)*0.1 + n2oflux )
               sol_nh4(k,j) = 0.0
               n2oflux = 0.
            
          end if      
         endif    
       endif         
      
      cal_temp(1) = cal_temp(1) + n2oflux
	if (tem_no .le. 1E-30) tem_no = 0.
	  NO(j) = NO(j) + tem_no
	if (n2oflux .le. 1E-30) n2oflux = 0.
	  N2O(j) = N2O(j) + n2oflux
      wdn = ntotflux*10.0 			
	if(wdn < 0.) wdn = 0. 
	sol_no3(k,j) = sol_no3(k,j) - wdn
	
	
999	return
	end
