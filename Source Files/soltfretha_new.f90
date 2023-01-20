      subroutine soltfretha(k)
      !! This subroutine is used to calculate freeze-thaw cycles in soil layers
      use parm
      use parm_subE
      implicit none
      
      integer, intent (in) :: k
      integer :: j,  nly
	  real :: a, b
	
      j = 0
      j = ihru
      nly = 0
      nly = sol_nly(j)


	  !! to convert 1 g of ice at 0 oC to 1 g of  water at 0 oC,
	  !! 334 J of heat must be absorbed by the water. Conversely,
	  !! when 1 g of water at 0 oC freezes to give 1 g of ice  at 0 oC,
	  !! 334 J of heat will be released to the surroundings.
	  !! water density: 1g/cm^3
	  !! heat of fusion of water: 334J/g
	  !! 1g/cm^3 * 334J/g =334J/cm^3
	  !! ice density: 0.92g/cm^3
	  !! heat of fusion of ice: 334J/g
	  !! 0.92g/cm^3 * 334J/g=307J/cm^3
      
     
      if (k <= sol_nly(j)) then    !! only for default soil layers                    
	   
	   if (sol_tmp1(k,j)<=0) then  !!--- freezing-----
	   
            a = 0.
	        b = 0.
	        a = ca_sol(k,j)*(sol_thic(k,j)/10)*sol_tmp1(k,j)            !! J/cm^2
            b = 334.*sol_wc(k,j)/10                                     !! J/cm^2
	        d_sols(k,j) = a + b                                         !! J/cm^2

		    if ( d_sols(k,j)>= 0. )  then                              !! not all water freezed
                !!d_ice(k,j) = -a/307*10                               !! mm        
               d_ice(k,j) = -a /334.*10.                               !! mm
               sol_wc(k,j) = sol_wc(k,j) - d_ice(k,j)                  !! soil water content inlcuding  mm
               sol_ice(k,j) = sol_ice(k,j)+ d_ice(k,j)                 !! soil ice conteng mm
		       d_sol(k)=0.                                             !! used in soltcal.f   ;  J/(cm^2 d)  source or   sink of letant heat J/cm^2
		    else
               d_sol(k) = (a+b)/1.                                      !! used in soltcal.f   ;  source or sink of letant heat J/cm^2
		        !! d_ice(k,j)=sol_wc(k,j)/0.92                          !! mm
               d_ice(k,j) = sol_wc(k,j)                                 !! change in soil ice content  mm
               sol_ice(k,j) = sol_ice(k,j)+ d_ice(k,j)                  !! update soil ice content mm
		       sol_wc(k,j) = 0.                                         !! update soil water content mm
	        endif

	   else     !!---thawing----

         !!d_sols(k,j)=ca_sol(k,j)*sol_tmp1(k,j)/1-334*sol_icev(k,j)
          a = 0.
	      b = 0. 
          a = sol_thic(k,j)/10*ca_sol(k,j)*sol_tmp1(k,j)           !!J/cm^2
	      !! b=307*sol_ice(k,j)  
	      b = 334*sol_ice(k,j)                                     !!J/cm^2
          d_sols(k,j) = a-b
		 if (d_sols(k,j)<=0)  then
               !!d_ice(k,j)  =-a/307*10 
		       d_ice(k,j)  = -a/334*10                                 !!mm 
               !! sol_wc(k,j) =sol_wc(k,j) - d_ice(k,j)*0.92           !!mm
               sol_wc(k,j) = sol_wc(k,j) - d_ice(k,j)                  !!mm
               sol_ice(k,j) = sol_ice(k,j) + d_ice(k,j)                !!mm
		       d_sol(k) = 0.

	      else
               d_sol(k)=(a-b) /1.0                                                       !!J/(cm^2 d), 1.0 is time step
               !!d_sol(k)=ca_sol(k,j)*sol_tmp1(k,j)/1-334*sol_icev(k,j)
		       d_ice(k,j) = sol_ice(k,j)
		        !!sol_wc(k,j)=sol_wc(k,j)+d_ice(k,j)*0.92
               sol_wc(k,j) = sol_wc(k,j)+d_ice(k,j)
		       sol_ice(k,j) = 0.
	      endif
         
        end if
       
	   
	  else      !! added soil layers without freezing and thaw
		  
	    d_sol(k)=ca_sol(k,j)*(sol_thic(k,j)/10)*sol_tmp1(k,j)/1
	
	  end if
	  
     return
     end
