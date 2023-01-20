      subroutine soltemp 
      !! This subroutine is used to calculate snow and soil temperature based on heat transfer theory 
      use parm
      use parm_subE
      implicit none
      
      integer :: j, k, nly 
       j = 0
       j = ihru
	   nly = 0
       nly = sol_nly(j)
       
       
       !! ------------------------------------------------Based on :-----------------------------------------------------------------------------------------------------------------------
       !!  Qi, J. et al, 2016. A new soil-temperature module for SWAT application in regions with seasonal snow cover. Journal of Hydrology, 538, pp.863-877.
       !!  Patankar, S., 1980. Numerical heat transfer and fluid flow. CRC press.
       !!  Yin, X. and Arp, P.A., 1993. Predicting forest soil temperatures from monthly air temperature and precipitation records. Canadian journal of forest research, 23(12), pp.2521-2536.
       !!-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       
       !! five parameters to calibrate soil and surface temperature
       !! using pothole parameter to calibrate soil temperatrue in SWAT-CUP 
       
       !! Effective air/ground conductance ratio coefficient
       !! the following is commented out as the parameters values are determined in read_energy.f90
!       if ( pot_tilemm(j) <= 0. ) then
!          eff_coe(j) = 50.
!       else
!          eff_coe(j)  = pot_tilemm(j) !50
!       end if
!         !!  soil heat conduc.
!       if ( pot_volxmm(j) <=0. ) then
!         do k = 1, nly
!         k_coe(k,j) = 10. 
!         end do
!       else 
!         do k = 1, nly
!         k_coe(k,j) = pot_volxmm(j) !10  
!         end do
!       end if
!            !!  snow heat conduc.
!       if ( pot_volmm(j) <=0. ) then
!         ks_coe(j) =1.
!       else
!         ks_coe(j) = pot_volmm(j)                                     
!       end if
!            !! soil heat capacity
!       if ( pot_nsed(j)  <=0. ) then
!          c_coe(j) =1.
!       else
!          c_coe(j) = pot_nsed(j)                                            
!       end if
       
       !! annual average snow depth mm
       ! if(pot_no3l(j)<=0.) then                                        
        ansnodep(j) = 1200     !mm
       !else
      !  ansnodep(j)=pot_no3l(j)
      ! endif
  

      
	   call soltphys_in        !! update soil physical properties before calculation of soil temperature 
       
       call solthermal         !! determine soil thermal properties
       
       call soltly             !! add five new layers under the last soil layers 
      
	   call soltbott           !! calculate soil bottom boundary condition 
       
       call soltsurf           !! calculate soil surface boundary condition 
   
	   call soltcal            !! solve soil heat transfer equaiton
 
	   call soltphys_out       !! update soil physcial properties after calculation of soil temperature
	 	   
       call soltout            !! calculate output soil temperature for specific depth
   


      return
      end
