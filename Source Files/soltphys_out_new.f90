        subroutine soltphys_out  
        !! This subroutine updates soil physical parameters after soil temperature calculation
        use parm
        use parm_subE
        implicit none
        
        integer :: j, k, nly  
        real:: a, b, c
          
          j = 0
          j = ihru
	      nly = 0
          nly = sol_nly(j)
        
         do k = 1,nly
          sol_air(k,j) = sol_pormm(k,j)- sol_wc(k,j)-sol_ice(k,j)   !!mm
          sol_ice(k,j)= max(sol_ice(k,j) , 0.0)  !!mm
          sol_ice(k,j)= min(sol_pormm(k,j) , sol_ice(k,j))
          sol_wc(k,j) = max(sol_wc(k,j) , 0.0)   !!mm
          sol_wc(k,j) = min(sol_pormm(k,j) , sol_wc(k,j)) 
          sol_air(k,j)= max(sol_air(k,j) , 0.0)  !!mm
          sol_air(k,j)= min(sol_pormm(k,j) , sol_air(k,j))  

         if( sol_ice(k,j) <= sol_wpmm(k,j) ) then
	       sol_st(k,j) = sol_wc(k,j)-(sol_wpmm(k,j) - sol_ice(k,j)) 
	     else 
           sol_st(k,j) = sol_wc(k,j)
	     endif
		  
		  sol_st(k,j) = max(sol_st(k,j),0.0)
		  sol_st(k,j)= min(sol_pormm(k,j)-sol_wpmm(k,j) , sol_st(k,j)) 

         end do
         
         
         return
         end