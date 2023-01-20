      subroutine soltsurf
      !! This subroutine calculate surface temperature based on energy balance at soil/snow surface
      use parm
      use parm_subE
      implicit none
     
      integer :: j, k, nly, ib,ij,nlyr,idp
      real :: f, tamp, d,t_c,a
      real :: tk, ea, ed, ralb, rbo, rto, rout, rn_pet,snot
      real :: sum, smp, smfac, smleb
      real :: xx, snocov,ee
      real :: cej, eaj
      
      j = 0
      j = ihru
      
      !! ambient air temp===
      if (igro(j) == 1) then     
	   idp = idplt(j)
	  
	   select case (idc(idp))
	   case(1,2,3,4,5,6)
        ambtmp(j)= tmpav(j)
	   case(7)
	   !! air temp under closed canopy
	    t_c = -0.11+0.96*tmpav(j)-0.00008*tmpav(j)**3    
        ambtmp(j)=tmpav(j)+(t_c-tmpav(j))*(log(1+min(blai(idp),laiday(j))) / log(1+ blai(idp)) )      	
	   end select
	  else
        ambtmp(j)= tmpav(j)
	  end if

  
      !! net shortwave radiation===
       !! albedo
      if (igro(j) == 1) then    
	    idp = idplt(j)
        ralb = hru_ra(j)*(1.0 - albday)*Exp(-ext_coef(idp)*laiday(j))
	  else
        if (snoco(j) <= 0.5) then
            albday = sol_alb(j) 
        else
            albday = 0.8
        end if
        ralb = 0.
        ralb = hru_ra(j) *(1.0 - albday)
	  endif

       !! net long-wave radiation===

        !! net emissivity  equation 2.2.20 in SWAT manual
        ea = 0.
        ed = 0.
        rbo = 0.
        ea = Ee(tmpav(j))
        ed = ea * rhd(j)
        rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 2.2.19
         rto = 0.
        if (hru_rmx(j) < 1.e-4) then
	     rto = 0.
        else
         rto = 0.9 * (hru_ra(j) / hru_rmx(j)) + 0.1 !|maximum possible radiation for the day in HRU
        end if

         !! net long-wave radiation equation 2.2.21
         rout = 0.
         tk = 0.
         tk = tmpav(j) + 273.15
         rout = rbo * rto * 4.9e-9 * (tk**4)

		
	   !! calculate net radiation===
         rn_pet = 0.
         rn_pet = ralb + rout        !!MJ/m^2 d
         rn_pet = 100 * rn_pet       !!J/(cm^2 d)
         ralb=100 * ralb             !!J/ (cm^2 d)
	  
	    !!  surface temperature===
	    !! Effective air/ground conductance ratio coefficient
	    idp = idplt(j)
	    eff_conr = 8.1 * ralb * (1 - exp(min(blai(idp),laiday(j))-6.8))/86400.
	    !!  for calibration
	    eff_conr = eff_conr * eff_coe(j)   
       
        !! soil surface temperature 
        !t_bare(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr)*(sol_tmp1(1,j)+rn_pet/(k_sol(1,j)/((sol_thic(1,j)/10)/2)))
        !! snow surface temperature 
        !snosurtmp(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr)*(snotmp1(j)+(rn_pet) /(k_sno(j)/((sno_dep(j)/10)/2)))     
         
        !! surface temperature
	    if (sno_hru(j) <=0.) then
           sur_tmp(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr) *(sol_tmp1(1,j)+rn_pet/(k_sol(1,j)/((sol_thic(1,j)/10.)/2.)))
	    else
           sur_tmp(j)=ambtmp(j)/(1+eff_conr)+eff_conr/(1+eff_conr) *(snotmp1(j)+(rn_pet) /(k_sno(j)/((sno_dep(j)/10.)/2.)))  
	    end if
           


      return
      end
