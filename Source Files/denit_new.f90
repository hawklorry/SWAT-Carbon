        subroutine denit_new 
        use parm 
        use parm_subC 
        use parm_control 
        implicit none 
         
        real ::  void, sut, cdg, wdn , fcgd
        real :: solst,solpor
        integer :: j, k 
        
        j=0
        j=ihru
 
        solst = 0.
        solpor = 0.
          
        do k = 1,sol_nly(j)           
        !! average soil temperature over layers   
        if( k <= sol_nly(j))then
	     if(k==1) then
	     st_ave(j) = sol_tmp(k,j)
	     else
         st_ave(j) = (st_ave(j)*sol_z(k-1,j) + sol_tmp(k,j)*sol_thick(k,j))&
                      /sol_z(k,j)
         end if
        end if
        !! average WFPS over layers   
        solst = solst + sol_st(k,j) + sol_wpmm(k,j)
        solpor = solpor + sol_ul(k,j) + sol_wpmm(k,j)    
        if(k==sol_nly(j) ) then
        WFPS_ave(j) = min(1., solst/solpor)   !!m3/m3
        end if 
        
      
       !! average pH over layers 
         if( k <= sol_nly(j))then
	     if(k==1) then
	      ph_ave(j) = sol_ph(k,j)
	     else
          ph_ave(j) = ( ph_ave(j)*sol_z(k-1,j) + sol_ph(k,j)*sol_thick(k,j))&
                      /sol_z(k,j)
         end if
        end if
            
         if (i_sep(j) /= k .or. isep_opt(j) /= 1) then   !! septic changes 1/28/09 gsm   
           void=0.
           wdn = 0.
           void = sol_por(k,j) * (1. -  (sol_st(k,j) + sol_wpmm(k,j) )  / (sol_ul(k,j) + sol_wpmm(k,j) ) )   ! fraction  
           void =max(void, 0.)       
             !  cdg = sol_tmp(k,j) / (sol_tmp(k,j) + exp(5.058459 - 0.2503591 * sol_tmp(k,j)))
           cdg = fcgd(sol_tmp(k,j))
              
            select case(nswat)
             case(0)     !! Original Denitrification
   	            if (cdg > 0. .and. void <= 0.1) then
                call ndenit(k,j,cdg,wdn,void) 
                end if
             case(1)   !! Daycent 
                if (sol_tmp(k,j) >= -5.0 ) then                       !! Denitrification occurs only at soil temperature above -5     
               call ndenit_Qi(k,j,cdg,wdn,void, sol_RSPC(k,j)) 
               end if  
            end select 
               
               
          

        
          end if    !! septic changes 1/28/09 gsm
          
          end do
 
           return 
           end 
            
   