      subroutine soltphys_in
      !! This subroutine is used to determine soil physical parameters used for soil temperature module
      use parm
      use parm_subE
      implicit none
      
      integer :: j, k, nly
     
       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j) 
      
       !! thickness of the current soil layer  mm
       sol_thic(1,j)=sol_z(1,j)                                                                   
	   do k= 2, nly
        sol_thic(k,j)= sol_z(k,j) - sol_z(k-1,j)                                           
       end do    
       !! depth from soil surface to the center of current soil layer mm
       sol_cd(1,j) = sol_z(1,j)*1./2.                                                          
       do k=2,nly
        sol_cd(k,j)=sol_z(k-1,j)+sol_thic(k,j)/2                                      
	   end do

       do k = 1, nly
         sol_pormm(k,j) = sol_por(k,j)*sol_thic(k,j)                    !!soil layer porosity in mm
         if( sol_ice(k,j) <= sol_wpmm(k,j) ) then                       !! when soil ice content in mm <= soil wilting point in mm
	      sol_wc(k,j) = sol_st(k,j)+ sol_wpmm(k,j) - sol_ice(k,j)       !! update soil water content in mm
	     else 
          sol_wc(k,j) = sol_st(k,j)
	     endif
       sol_wcv(k,j) = sol_wc(k,j)/sol_thic(k,j)                          !! ice fraction  in soil layer  mm/mm
       sol_org(k,j) = (sol_rsd(k,j)+sol_hum(k,j))/(0.5*10000)            !! organic matter content mm
       sol_orgv(k,j) = sol_org(k,j)/(sol_thic(k,j))                      !! organic matter fraction in soil layer mm/mm
       sol_minv(k,j) = sol_bd(k,j)/2.65-sol_orgv(k,j)                    !! mineral material fraction in soil layer 
       sol_icev(k,j) = sol_ice(k,j)/sol_thic(k,j)                        !! ice fraction in soil layer mm/mm
       sol_air(k,j) = sol_pormm(k,j)- sol_wc(k,j)-sol_ice(k,j)           !! air content  in soil layer  mm
      end do
  

      return
      end
