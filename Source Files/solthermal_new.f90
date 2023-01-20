      subroutine solthermal
      !! This subroutine is used to calculate thermal properties of snow and soil
      use parm
      use parm_subE
      implicit none
      
      integer :: j, k, nly,lu,idp
      real :: aa, bb, cc, d, den, a, b, c, e, pp
      real ::denh2o,denice,om_csol,cpice,cpliq
      real :: tkwat,tkice, om_tkd,om_tkm,fl

       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j)

            
	  !! snow thermal parameters===
	  !! thermal conductivity for snow layer  ( J /  (cm d C)
      if( k_sno(j) >= 864*2 ) then
	        k_sno(j) = 864*2                     
	  else
	      if ( sno_den(j) < 0.156 ) then       
	       k_sno(j) = 0.023+0.234*sno_den(j)    
	      else
	       k_sno(j) = 0.138-1.01*sno_den(j)+3.233*sno_den(j)**2
	      end if
	      ! for calibration
          k_sno(j)=k_sno(j)*864*ks_coe(j)            
	  endif 
	  ! volumetric heat capacity for snow layer J/(cm^3.C)
	  ca_sno(j)=1.9 * sno_den(j)**2/0.917        
      
      
      !! heat capacities of soil layers===
      !! volumetric heat capacity for soil layer J/(cm^3.C)
      do k = 1, nly
        ca_sol(k,j) = 4.1868*( 0.48*sol_minv(k,j) +0.6*sol_orgv(k,j)+1*sol_wcv(k,j)+0.45*sol_icev(k,j))   
	    !! for calibration
	    ca_sol(k,j) = ca_sol(k,j) *c_coe(j)                                                                                           
	  end do
	
      !! thermal conductivities of soil layers (j/(cm C d)
      k = 0
	  lu = 2
      idp = idplt(j)
      pp = 0.0  !!   pp~(-0.2, + 0.2)

       !!----Johansen Method---
       !!  Johansen, O., 1977. Thermal conductivity of soils (No. CRREL-TL-637). Cold Regions Research and Engineering Lab Hanover NH.
       !!Johansen's method is suitable for calculating soil 
       !! thermal conductivity of both coarse- and fine- grained soils in the frozen and unfrozen states. However, 
       !! it is limited to saturations greater than 20%.
	   !! define saturation
       !! sol_ksat is soil thermal conductivity in the saturated state
	   !! sol_kd is soil thermal conductivity in the dry state
	   !! sol_ke is a dimensionless function of soil saturation
	
	  do k = 1, nly
	    sol_satu (k,j) = max((sol_wc(k,j)+sol_ice(k,j))/sol_pormm(k,j),0.0001)             !! soil saturateion degree          
        sol_kd(k,j)=864*((0.135*sol_bd(k,j)*1000+64.7)/ (2700-0.947*sol_bd(k,j)*1000)+ pp)    !! Soil thermal conductivity in the dry state J/(cm d C)           
      end do

      do k = 1, nly
        !if (  (sol_ice(k,j)/sol_wc(k,j)) > 1) then
        if (  sol_ice(k,j) > sol_wc(k,j)) then
	        sol_ke(k,j)= sol_satu(k,j)    !! !! Kersten number 
            sol_ksat(k,j)=(864*2.9)**(1-sol_por(k,j))*(864*2.2)**(sol_por(k,j)-sol_wcv(k,j))*(0.57*864)**sol_wcv(k,j)  !! Soil thermal conductivity in saturated state  J/(cm d C)    
	    else
            if (sol_satu(k,j)>0.1) then
	            sol_ke(k,j)=log10(sol_satu(k,j))+1    !! Kersten number
	        else 
                sol_ke(k,j)=0                                            
	        end if
            sol_ksat(k,j)=(864*0.57)**sol_por(k,j)* (2.9*864)**(1-sol_por(k,j))          !! Soil thermal conductivity in saturated state  J/(cm d C)     
        end if
	  end do
     
      !! soil thermal conduc.
      do k = 1, nly
	   k_sol(k,j)=(sol_ksat(k,j)-sol_kd(k,j))*sol_ke(k,j)+sol_kd(k,j)  
      end do
  
       !!for calibration 
	  do k = 1, nly
       k_sol(k,j)= k_sol(k,j)*k_coe(k,j) 
      end do

 
      return
      end
