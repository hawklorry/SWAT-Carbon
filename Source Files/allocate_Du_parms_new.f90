       subroutine allocate_parms_Du
       use parm 
       use carbon_para
       use parm_sedflux
       implicit none
       
       integer :: j


 !-----for oganic carbon module, added by Xinzhong Du-----------------------------------!
      allocate(cbn_sub(msub)) ! carbon transport varibales in subbasin
      allocate(cbn_hru(mhru)) ! carbon variable in HRU scale 
   !   allocate(kd_OC(msub)) !organic carbon partition coefficient
      do j=1,mhru
         allocate(cbn_hru(j)%cbn_ly(mlyr))
      enddo
      allocate(cbn_sp(mhru))  ! carbon parameters for soil processes
      !carbon state variables and parameters in water colunm
      allocate (cbn_rch(mch))    !carbon state variables in the reach
      allocate (cbn_rchpara(mch))     !carbon parameters in the reach/stream
      allocate (cbn_res(mres))   !carbon state variables in the reservoir
      allocate (cbn_respara(mres))    !carbon parameters in the reservoir
      allocate (cbn_wet(mhru))   !carbon state variables in the wetland
      allocate (cbn_wetpara(mhru))    !carbon parameters in the wetland
      allocate (cbn_pnd(mhru))   !carbon state variables in the pond
      allocate (cbn_pndpara(mhru))    !carbon parameters in the pond
      !
      allocate (Ab_rch(mch))     !bottom algae state variables and pathways in the reach
      allocate (Ab_rchpara(mch))      !bottom algae parameters in the reach/stream
      allocate (Ab_res(mres))    !bottom algae state variables and pathways in the reservoir
      allocate (Ab_respara(mres))     !bottom algae parameters in the reservoir
      allocate (Ab_wet(mhru))    !bottom algae state variables and pathways in the wetland
      allocate (Ab_wetpara(mhru))     !bottom algae parameters in the wetland
      allocate (Ab_pnd(mhru))    !bottom algae state variables and pathways in the pond
      allocate (Ab_pndpara(mhru))     !bottom algae parameters in the pond
      !
      allocate (scbn_rch(mch))    !sediment compartment carbon variables in the reach
      allocate (scbn_res(mres))   !sediment compartment carbon variables in the reservoir
      allocate (scbn_wet(mhru))   !sediment compartment carbon variables in the wetland
      allocate (scbn_pnd(mhru))   !sediment compartment carbon variables in the pond
      allocate (scbn_rchpara(mch))    !sediment compartment carbon variables in the reach
      allocate (scbn_respara(mres))   !sediment compartment carbon variables in the reservoir
      allocate (scbn_wetpara(mhru))   !sediment compartment carbon variables in the wetland
      allocate (scbn_pndpara(mhru))   !sediment compartment carbon variables in the pond
 
      allocate (Sed_rch(mch))          !in the reach  
      allocate (Sed_res(mres))         !in the reservoir   
      allocate (Sed_wet(mhru))         !in the wetland    
      allocate (Sed_pnd(mhru))         !in the pond 
      
      allocate (Sed_rchpara(mch))       !in the reach 
      allocate (Sed_respara(mres))      !in the reservoir 
      allocate (Sed_wetpara(mhru))      !in the wetland 
      allocate (Sed_pndpara(mhru))      !in the pond
      !--------------------------------------------------------------------------------------!	  	  
     
       
       end subroutine 