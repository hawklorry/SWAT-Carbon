      subroutine hru_solwtmp
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: j, k
      real::den_water,surfq_m3, sepbtm_m3
      real, dimension (mlyr) :: latq_m3
      real :: tmpav_mean,temp_equ,delta_wtmp,wtmp_temp
      real :: tmp_dew,pwater,cpwater,beta,tdw,fwind,kt,qsw
      real sub_gwq_m3,sub_wyld_m3
      real:: t,rh,em,e   
       j = 0
       j = ihru
     

        if(gw_q(j)>0.) then
          !wtmp_gwq(j)= 10.5
            wtmp_gwq(j)= bot_tmp(j)
        else
            wtmp_gwq(j)=0.
        end if
      
        !k=0
        !do k=1, sol_nly(j)
         !latq_tot(j)= latq_tot(j)+ flat(k,j)
       ! end do
        
        !if(latq_tot(j)>0.) then
        ! do k=1, sol_nly(j)
        ! wtmp_latq(j)= wtmp_latq(j)+ sol_tmp(k,j)*flat(k,j)/latq_tot(j)
        ! end do
        !else
        ! wtmp_latq(j)= 0.
       ! end if
         
         !!consider all soil years
        ! do k=1, sol_nly(j)
        ! wtmp_latq(j)= wtmp_latq(j)+ sol_tmp(k,j)*sol_thic(k,j)/sol_z(sol_nly(j),j)
        ! end do
        
         !! consider top two layers
         do k=1, 2
            wtmp_latq(j)= wtmp_latq(j)+ sol_tmp(k,j)*sol_thic(k,j)/sol_z(2,j)
         end do
     
     
       if(wtmp_gwq(j)<-1.0) wtmp_gwq(j) = -1.0
       if(wtmp_latq(j) <-1.0) wtmp_latq(j) = -1.0
  
     return
     end 