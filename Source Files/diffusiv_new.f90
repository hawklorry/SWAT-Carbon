      real function diffusiv (A, bulkden, twfps)
      use parm

      implicit none
      
      !!.......Argument declarations
      real A, bulkden, twfps
      
      !!........local variables
      
      real porosity
      real theta_A, ttheta_P, theta_V, my_theta_V
      real tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8
      real vfrac
      real s_wat, sw_p
      real pfc
      real dDO

      integer   debug


      
      porosity = 1.0 - (bulkden)/2.56
      
       if ((porosity - (A)) < 0.0) then
       diffusiv = 1.0
       go to 999
       end if
      
       pfc = (twfps)*100 /(A/porosity)
       
       if (pfc >= 100.0) then
        vfrac = ((twfps)*porosity - A) / (porosity - A)
        else 
        vfrac = 0.0
       endif
      
	
	if (pfc < 100.0) then
        theta_V = (pfc/100.0)*A
        else 
        theta_V = A + (min(vfrac,1.0) * (porosity - A))
      endif

	my_theta_V = twfps * (1.0 - bulkden/2.56)
	 
	 
	if (theta_V < A) then
	  ttheta_P = 0.0
	  else
	  ttheta_P = theta_V - A
	endif  
	
	if (theta_v > A) then
	  theta_A = A
	  else
	  theta_A = theta_V
	endif  
	
	
	s_wat = min(1.0, theta_V/A);
      sw_p = min(1.0, ttheta_P/(porosity-A))

      tp1 = (1.0 - s_wat)** 2.0
      tp2 = (A - theta_A) / (A + (1.0 - porosity))
      tp3 = tp2**(0.5*tp2 + 1.16)
      tp4 = (1.0 - ((porosity-A)**	& 
                 (0.5*((porosity-A)) + 1.16)))
      tp5 = (porosity-A)-ttheta_P
      if (tp5 > 0.0) then
        tp6 = tp5**(0.5*tp5 + 1.16)
        else 
        tp6 = 0.0
      endif
      tp7 = (1.0-sw_p)**2.0
      tp8 = max(0.0, (tp1*tp3*tp4*(tp5-tp6)) /	&
             (1.0E-6 + (tp1*tp3*tp4) + tp5 - tp6) * 1.0E7)

      !!if (debug) printf("From SUB: %f %f %f %f %f %f %f %f\n",
                        !!tp1, tp2, tp3, tp4, tp5, tp6, tp7, tp8);
      dDO = max(0.0, (tp8/1.0E7 + tp7*tp6))
      !!if (debug) {
	    !!printf("In SUB, dDO = %f\n", dDO);
	
      diffusiv = dDO
     
999	return 
	end
	
               
           