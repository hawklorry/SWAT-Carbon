        subroutine pweather_YANG
        use parm   
        !! added by qichun based on the Century algorithm
        implicit none  
        integer :: k,j
        !!real :: pweather
        real :: soilw, sfc, swp, wadj, normalizer
        real :: stem, wteff, fwater, ftem, teff, rwater
        real :: parentp, pfine
        real :: clay, silt
        real :: xx, sdth
        real :: parentplyr
        real :: pwr !! phosphorus weathering rate (g P/m2/day)
        
        j = 0
        j = ihru
        k=0
        
        silt = 0.
        clay = 0.
        stem = 0.
        parentp = 84.  !! input parent p (g P/m2)from here, need to change according to Xiajuan Yang's spatial dataset
        wadj = 0.1
        
        xx = 0.
        sdth = 0.
        sdth = sol_z(sol_nly(j),j)

        do k = 1, sol_nly(j)
        silt = sol_silt(k,j)
        clay =  sol_clay(k,j)
        stem = sol_tmp(k,j) 
 
        !! calculate soil thickness 
          if (k == 1) then
	        xx = sol_z(k,j)
	    else	
	        xx = sol_z(k,j) - sol_z(k-1,j)
	    end if
 
        soilw = sol_st(k,j)+sol_wpmm(k,j)
        sfc = sol_fc(k,j)+ sol_wpmm(k,j)
        swp=sol_wpmm(k,j)
        pfine= (silt+clay)/100.0
        normalizer = 11.75+(29.7/3.14)*atan(3.14*0.031*(30-15.4))

       if (sfc>swp)rwater = (soilw-swp)/(sfc-swp)

       if (rwater > 13.)  then
          fwater = 13.
       else
          fwater = 1/(1+10.*exp(-6.*rwater))
       endif   
       
       if (rwater<0.001)rwater=0.001
       if (rwater>1000.)rwater=1000.

       ftem = max((11.75+(29.7/3.14)*atan(3.14*0.031*(stem-15.4))) &
       			/normalizer, 0.01)
     
       teff= (0.1+(0.1/3.14)*atan(6.28*(pfine-0.7)))
       parentplyr = parentp * xx / sdth   !! assume phosphorus distributed evently from soil surface to bottom
       pwr = 0.1*10.*parentplyr*teff*ftem*fwater/365.0  !! convert from g P/m2 to kg P/ha
        
      if (pwr<0) pwr =0.
      
      !!cal_temp(1) = pwr
      sol_solp(k,j) = sol_solp(k,j) +  pwr
      
    !!  pwther(j) = pwther(j) + pwr
       
     
      end do
      
      return
      end
