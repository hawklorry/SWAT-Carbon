      subroutine orgncswat2(iwave)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic C/ N removed when using cswat == 2  

      use carbon_para
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      
      implicit none
      
      integer, intent (in) :: iwave
      integer :: j
      real :: xx, er, conc, xx2, wt1, wt2
      real :: sol_mass, QBC, VBC, YBC, YOC, YW, TOT, YEW, X1, PRMT_21, PRMT_44
      real :: DK,  V, X3, CO, CS, perc_clyr, latc_clyr  ,Y1
      real :: ABL,X2,XX1,DOC_mass,VBC1,sat_DIC,cw_DIC,sat_ly,DIC_ly  
      real:: BMC_Fr, con_DIC, swmm, mwmm, QBN, VBN, R_NC , temp_ccp
      real:: KH, K1, K2, AET, T

      integer :: k
      latc_clyr = 0.
      perc_clyr = 0.  
      j = 0
      j = ihru
    
     !!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  Sediment C and N Transport  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    !!====Sediment  Org-C =========================
        xx1 = sol_HPC(1,j)+sol_HSC(1,j)+sol_BMC(1,j)+sol_LSC(1,j)+sol_LMC(1,j)    !!kg/ha  C
        xx2 = sol_HPC(2,j)+sol_HSC(2,j)+sol_BMC(2,j)+sol_LSC(2,j)+sol_LMC(2,j)
        wt1 = (sol_z(1,j) / 1000.) * 10000. * sol_bd(1,j) * 1000. * (1- sol_rock(1,j) / 100.)       !kg/ha soil
        wt2 = ((sol_z(2,j) -sol_z(1,j)) / 1000.) * 10000. * sol_bd(2,j) * 1000. * (1- sol_rock(2,j) / 100.)  

  !    if (xx1/wt1 >= xx2/wt2) then       
    !    ccp(j) = (xx1+xx2) / (wt1 + wt2) * er_POC(j) 
    !  else
        ccp(j) = xx1/wt1 *er_POC(j)
        temp_ccp = ccp(j) 
    !  end if      

      YOC = 0.
      YOC = ccp(j) * sedyld(j) / hru_ha(j) * 1000      !Organic C loss with sediment
      xx = 0.
      xx = sol_HPC(1,j)+sol_HSC(1,j)+sol_BMC(1,j) +sol_LSC(1,j)+sol_LMC(1,j)
      
      if (YOC > 0.9 * xx) then
          YOC = 0.9 * xx
      end if
      
      if (xx > 0.001) then
        X1 = (1. - YOC / xx) 
      else
        X1 = 1.0
        YOC = 0.
      end if


      sol_HSC(1,j)=sol_HSC(1,j)*X1
      sol_HPC(1,j)=sol_HPC(1,j)*X1
      sol_BMC(1,j)=Sol_BMC(1,j)*X1
      sol_LSC(1,j)=sol_LSC(1,j)*X1
      sol_LMC(1,j)=sol_LMC(1,j)*X1
      
      sol_LSLC(1,j)=sol_LSLC(1,j)*X1
      sol_LSLNC(1,j)=sol_LSC(1,j)-sol_LSLC(1,j)
      
      sol_LS(1,j)=sol_LS(1,j)*X1
      sol_LM(1,j)=sol_LM(1,j)*X1
      sol_LSL(1,j)=sol_LSL(1,j)*X1
      
      sol_rsd(1,j) = sol_LS(1,j)+sol_LM(1,j) 
      sedc_d(j) = YOC     
      
      Sed_RPOC(j) = (sol_HSC(1,j)+sol_HPC(1,j))*(1.0- X1)                         !  calculated RPOC transport  
      Sed_LPOC(j) = (sol_LMC(1,j)+sol_LSC(1,j)+sol_BMC(1,j))*(1.0-X1)             !  calculated LPOC transport
      
      Sed_BMC(j) = sol_BMC(1,j)*(1.0-X1)    
      

     !! ================Sediment  Org-N ================================={
      Select Case(2)   
         
       Case(1)     
       xx = 0.
	   wt1 = 0.  !! conversion factor
       er = 0.	!! enrichment ratio
      if (iwave <= 0) then
        !! HRU calculations  
        if (erorgn(j) > .001) then
          er = erorgn(j)
        else
          er = enratio
        end if

      else
        !! subbasin calculations
        xx = sub_orgn(iwave)
        wt1 = sub_bd(iwave) * sol_z(1,j) / 100.        
        er = enratio
      end if
         
        xx1 = sol_HPN(1,j)+sol_HSN(1,j)+sol_BMN(1,j)+sol_LSN(1,j)+sol_LMN(1,j)
        xx2 = sol_HPN(2,j)+sol_HSN(2,j)+sol_BMN(2,j)+sol_LSN(2,j)+sol_LMN(2,j)
        wt1 = (sol_z(1,j) / 1000.) * 10000. * sol_bd(1,j) * 1000. * (1- sol_rock(1,j) / 100.)       
        wt2 = ((sol_z(2,j) -sol_z(1,j)) / 1000.) * 10000. * sol_bd(2,j) * 1000. * (1- sol_rock(2,j) / 100.)
        
      !if (xx1/wt1 >= xx2/wt2) then       
      ! ccp(j) = (xx1+xx2) / (wt1 + wt2) * er 
    !  else
        ccp(j) = xx1/wt1 *er    
    !  end if
  
      if (iwave <= 0) then
        !! HRU calculations
        sedorgn(j) = ccp(j) * sedyld(j) / hru_ha(j) * 1000     ! kg N /ha
      else
        !! subbasin calculations
       conc = 0.
       conc = xx * er / wt1
       sedorgn(j) = .001 * conc * sedyld(j) / (da_ha * sub_fr(iwave))
      end if

      if (sedorgn(j) > 0.9*xx1) then
        sedorgn(j) = 0.9*xx1
      end if

	!! update soil nitrogen pools only for HRU calculations
        xx = sol_HPN(1,j)+sol_HSN(1,j)+sol_BMN(1,j)+sol_LSN(1,j)+sol_LMN(1,j)
      if (iwave <= 0 .and. xx > 1.e-6) then
        xx1=0.
        xx1 = (1. - sedorgn(j) / xx)   
        !!add by zhang to update soil nitrogen pools        
		sol_LSN(1,j) = sol_LSN(1,j) * xx1
		sol_LMN(1,j) = sol_LMN(1,j) * xx1
		sol_HPN(1,j) = sol_HPN(1,j) * xx1
		sol_HSN(1,j) = sol_HSN(1,j) * xx1
		sol_BMN(1,j) = sol_BMN(1,j) * xx1
      end if

     
     Case(2)   
       
      xx1 =0.
      xx1 = sol_HPN(1,j)+sol_HSN(1,j)+sol_BMN(1,j)+sol_LSN(1,j)+sol_LMN(1,j)
      R_NC=0.
      R_NC= xx1   / (  sol_HPC(1,j)+sol_HSC(1,j)+sol_BMC(1,j)+sol_LSC(1,j)+sol_LMC(1,j)  )    !!kg/ha  C
      
      sedorgn(j)=R_NC*YOC
  
       if (sedorgn(j) > 0.9*xx1) then
        sedorgn(j) = 0.9*xx1
      end if

	!! update soil nitrogen pools only for HRU calculations
	   xx=0.
       xx = sol_HPN(1,j)+sol_HSN(1,j)+sol_BMN(1,j)+sol_LSN(1,j)+sol_LMN(1,j)
      if (iwave <= 0 .and. xx > 1.e-6) then
        xx1 = 0.
        xx1 = (1. - sedorgn(j) / xx)
        
        !!add by zhang to update soil nitrogen pools        
		sol_LSN(1,j) = sol_LSN(1,j) * xx1
		sol_LMN(1,j) = sol_LMN(1,j) * xx1
		sol_HPN(1,j) = sol_HPN(1,j) * xx1
		sol_HSN(1,j) = sol_HSN(1,j) * xx1
		sol_BMN(1,j) = sol_BMN(1,j) * xx1
      end if

   End Select
      
       Sed_RPON(j) =(sol_HSN(1,j)+sol_HPN(1,j))*(1.0- xx1)                         ! RPON transport  kg N/ha
       Sed_LPON(j)=(sol_LMN(1,j)+sol_LSN(1,j)+sol_BMN(1,j))*(1.0-xx1)   !LPON transport  kg N/ha
       
       
     !! ================Sediment  Org-N =================================}
  
  
     !!======================Runoff and leached DOC and DON = =================={
  
      sol_mass = 0.  
      sol_mass = (sol_z(1,j) / 1000.) * 10000. * sol_bd(1,j)* 1000. * (1- sol_rock(1,j) / 100.)

      QBC = 0.        !c loss with runoff or lateral flow
      VBC = 0.        !c los with vertical flow
      QBN = 0.        !N loss with runoff or lateral flow
      VBN = 0.        !N los with vertical flow
      R_NC = 0.
       IF(sol_BMC(1,j) > .01) THEN     
        R_NC = sol_BMN(1,j) / sol_BMC(1,j)
        sol_WOC(1,j) = sol_BMC(1,j)+sol_HPC(1,j)+sol_HSC(1,j)       ! soil organic C    
        DK=0.
        
        BMC_Fr = 0.
        BMC_Fr = sol_WOC(1,j)/sol_mass                                     !!kg/kg
        DK = part_DOC(j) * BMC_Fr * sol_bd(1,j) * sol_z(1,j)               !!mm   
         
          X1 =0.
          X1 = sol_st(1,j) + sol_wpmm(1,j)                                 ! mm   
          if (X1 <= 0.) then
            X1 = 0.01
          end  if
          XX = 0.
          XX = X1 + DK          
          V = 0.
          V = surfq(j) + sol_prk(1,j) + flat(1,j)
        
          if(V > 1.E-10) then
              X3=0.
              X3 = sol_BMC(1,j)*(1.-EXP(-V/XX))                                                          !! loss of biomass kgC/ha    
              CO=0.
              CO= X3 / V                                                                                                   !! Biomass C concentration in kgC/ha /mm
              CS=0.
              CS = peroc_DOC(j) *CO                                                                               !! kgC/ha /mm                                
           
             VBC = CO*(  sol_prk(1,j)  )                                                                           !!   kgC/ha            
             QBC = CS*(  surfq(j) + flat(1,j)  )                                                                   !!   kgC/ha   
             sol_BMC(1,j) = sol_BMC(1,j) - VBC- QBC                          
             sol_DOC(1,j)= sol_BMC(1,j)                                                                        !! DOC contration in soil water (kg/ha )

             VBN = R_NC*VBC                                                                                     !!   kgN/ha           |percolation from soil layer on current day
             QBN = R_NC*QBC                                                                                    !!   kgN/ha   
             sol_BMN(1,j)  =  sol_BMN(1,j)  -  VBN -  QBN                                         !!   kgN/ha    
    
      end if
      
      ELSE
         Sol_DOC(1,j)=0.                                ! DOC contration in soil water
       
      END IF
 
      SurQ_DOC(j)=QBC*(surfq(j)/(surfq(j)+flat(1,j)+1.e-6))                                         ! transported by surface runoff from top soil layer
      LatQ_DOC(1,j)=QBC*(flat(1,j)/(surfq(j)+flat(1,j)+1.e-6))                                   !RDOC transported by latral flow from top soil layer
      PerQ_DOC(1,j)=VBC                                                                                      ! perclation into second layer from top soil layer
      LatQT_DOC(j)=  LatQT_DOC(j) +  LatQ_DOC(1,j)
    
    
      SurQ_DON(j)= QBN*(surfq(j)/(surfq(j)+flat(1,j)+1.e-6))                                                              ! transported by surface runoff from top soil layer
      LatQ_DON(1,j) = QBN*(flat(1,j)/(surfq(j)+flat(1,j)+1.e-6))                                                                               ! transported by latral flow from top soil layer
      PerQ_DON(1,j) = VBN                                                                                 ! perclation into second layer from top soil layer
      LatQT_DON(j) = LatQT_DON(j) + LatQ_DON(1,j)                                     !  transported by latral flow of the whole soil profile 



       

      DO k=2,sol_nly(j)
            
          sol_BMC(k,j) = sol_BMC(k,j) +  VBC   
          sol_BMN(k,j) = sol_BMN(k,j) +  VBN     
  
          sol_WOC(k,j) = sol_BMC(k,j)  +sol_HPC(k,j)+sol_HSC(k,j)   !  SOIL ORG C                     
          sol_mass = 0.
          sol_mass = (sol_z(k,j) - sol_z(k-1,j)) / 1000.* 10000. * sol_bd(k,j)* 1000. *(1- sol_rock(k,j) / 100.)
          BMC_Fr = 0.
          BMC_Fr = sol_WOC(k,j)/sol_mass   !!kg/kg
          DK=0.
          DK = part_DOC(j) *BMC_Fr* sol_bd(k,j)* sol_z(k,j)     !!mm    
           R_NC=0.
           CO = 0.
           X3 = 0.
           X1 = 0.
           V = 0.
          IF(sol_BMC(k,j) >=.01)THEN
             R_NC= sol_BMN(k,j) / sol_BMC(k,j)
             V = sol_prk(k,j) + flat(k,j)            
             X1= sol_st(k,j)+sol_wpmm(k,j)                                                                          !   mm       
             if (V>0.) then
             X3=sol_BMC(k,j) *(1.-EXP(-V/(X1+DK)))                                 !loss of biomass kgC / ha
             CO= X3 / V                                       !! Biomass C concentration in kgC/ha /mm
             end if                                                                                     
         END IF
  
          LatQ_DOC(k,j) = CO*flat(k,j)                   ! loss by lateral flow
          PerQ_DOC(k,j) = CO*sol_prk(k,j)               ! leaching to underlying layer
          sol_BMC(k,j) = sol_BMC(k,j) - LatQ_DOC(k,j) - PerQ_DOC(k,j)
          sol_DOC(k,j) = sol_BMC(k,j)                                                                    ! DOC contration in soil water (kg/ha to mg/L)
          
         LatQT_DOC(j) = LatQT_DOC(j)+ LatQ_DOC(k,j)       !  transported by latral flow of the whole soil profile  
        
       
         LatQ_DON(k,j) = R_NC*  LatQ_DOC(k,j)                                ! transported by latral flow from top soil layer
         PerQ_DON(k,j) = R_NC * PerQ_DOC(k,j)                             ! perclation into second layer from top soil layer
         sol_BMN(k,j) = sol_BMN(k,j) -  LatQ_DON(k,j)- PerQ_DON(k,j)    
         LatQT_DON(j) = LatQT_DON(j) +LatQ_DON(k,j)                                              !  transported by latral flow of the whole soil profile 

         
         VBC=0. 
         VBC=PerQ_DOC(k,j)                                                                                         !  DOC percolation to the underlying soil layer

         VBN =0.
         VBN =PerQ_DON(k,j) 

      END DO
      


!$$$$$$$$$$$$$$$$$$$$$$$$$$   Runoff and leached DIC   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        Select case(1)
         
         Case(1)  !! based on Kessler and Harvey 2001-The global flux of carbon dioxide into groundwater

         

        
         T = 0.
         
         if( sol_nly(j)>0 )then
        
             do k = 1, sol_nly(j) 
                T =  T +  sol_tmp(k,j)   
             end do
             T = 273.15 + T / sol_nly(j)
        
         else
         
            T = 273.15 + tmpav(j)
        
         end if
         
      


         !T = 273.15 + sol_tmp(2,j)
         KH = 10** ( 108.3865 + 0.01985076*T - 6919.53/T - 40.45154*log10(T) + 669365. / T**2 )
         K1 = 10** ( -356.3094 - 0.06091964*T + 21834.37/T + 126.8339*log10(T) - 1684915. / T**2 )
         K2 = 10** ( -107.8871 - 0.03252894*T + 5151.79/T - 38.92561*log10(T) + 563713.9 / T**2 ) 
         
         !KH = 2.0E10-3
        ! K1 = 4.45E10-7
        ! K2 = 4.69E10-11       
         AET =  etday *365                                                                                                   !! mm        
       
         !AET = 661.      
                                                                                                                                          !! mm     
         Sol_ApH(j) = 7.
         Sol_pCO2(j) = 10**(8.54*0.0001*AET-2.604)                                                      !! L/L             
         Sol_H(j) =10**(-Sol_ApH(j) )                                                                                 !! mol /l
         con_DIC = 0.
         con_DIC = Sol_pCO2(j) *KH*( 1+K1/Sol_H(j) + K1*K2/Sol_H(j)**2  )             !! mol /l

      
         SurQ_DIC(j) = 12.*0.001*con_DIC* (surfq(j)*0.001*hru_ha(j)*10000.)*1000.  /hru_ha(j)                                                         ! loss by surface runoff  kg/ha
         LatQ_DIC(1,j) = 12.*0.001*con_DIC* (flat(1,j) *0.001*hru_ha(j)*10000.)*1000.   /hru_ha(j)                                                    ! loss by lateral flow  for layer   kg/ha
         Do k = 2, sol_nly(j)  
         LatQ_DIC(k,j) = 12.*0.001*con_DIC* (flat(k,j) *0.001*hru_ha(j)*10000.)*1000.  /hru_ha(j)                                                     ! loss by lateral flow  for layer   kg/ha
         End do
         PerQ_DIC(sol_nly(j) ,j) = 12*0.001*con_DIC* (sol_prk(sol_nly(j) ,j)*0.001*hru_ha(j)*10000.)*1000.  /hru_ha(j)                     ! loss by percolation into groundwaterkg/ha
     
         DO k = 1, sol_nly(j) 
         LatQT_DIC(j)= LatQT_DIC(j) + LatQ_DIC(k,j)        
         End do
       
         xx = 0.
         xx =   SurQ_DIC(j) + LatQT_DIC(j) + PerQ_DIC(sol_nly(j) ,j) 
         if( rspc_d(j) < xx   .and.  xx >0. ) then
         SurQ_DIC(j) =  rspc_d(j)*SurQ_DIC(j) /  xx 
         LatQT_DIC(j) = rspc_d(j)*LatQT_DIC(j) /  xx 
         PerQ_DIC(sol_nly(j) ,j) =  rspc_d(j)*PerQ_DIC(sol_nly(j) ,j) / xx 
         rspc_dnew(j) = 0.
         
         do k=1,sol_nly(j)
            LatQ_DIC(k,j) =LatQT_DIC(j) /real(sol_nly(j))
         end do
       
         else
            rspc_dnew(j) = rspc_d(j) - xx 
         end if

  
       Case(2) 

      !DIC processes in top soil ayer
      swmm=0.
      swmm=sol_st(1,j) + sol_wpmm(1,j)                                                                       !SW-soil water content (mm) simuated by SWAT
     ! as a function of partial pressure of CO2 and PH in soil layers, should be calibrated to generate DIC exports
      sat_DIC=0.
      !sat_DIC=10*swmm*0.01                                                            ! DIC saturation (kg/ha)   10mgC /l* mm * 0.01= (kg/ha) 
      sat_DIC=100.*swmm*0.01      
       xx=0. 
      If (Sol_DIC(1,j) > sat_DIC ) Then 
       xx = Sol_DIC(1,j) - sat_DIC
       Sol_DIC(1,j) = sat_DIC 
       sol_RSPC1(1,j) = sol_RSPC(1,j) + xx
      
      Else
       Sol_DIC(1,j) = Sol_DIC(1,j) + sol_RSPC(1,j)       !! kg/ha     !add resp C from organic carbon to DIC pool (kg/ha)
      if (Sol_DIC(1,j) > sat_DIC ) then
       sol_RSPC1(1,j) = Sol_DIC(1,j) - sat_DIC  
       Sol_DIC(1,j) = sat_DIC                                                         
      else
       sol_RSPC1(1,j) =0.
      end if 
      
      End If
      
      mwmm = 0.
      mwmm = surfq(j) + sol_prk(1,j) + flat(1,j)
     If (Sol_DIC(1,j) > .0 .and. mwmm >0.) Then
          sat_ly=0.
          sat_ly=sol_por(1,j)*sol_z(1,j)                                                                               ! saturate water content of the soil layer (mm)
          con_DIC=0.
        ! con_DIC=Sol_DIC(1,j)*(1-exp(-mwmm/sat_ly))/mwmm             ! DIC concentration in mobile water (kg/ha/mm)
         con_DIC = Sol_DIC(1,j)/swmm                                                      ! DIC concentration in mobile water (kg/ha/mm)
         SurQ_DIC(j) = peroc_DIC(j)*con_DIC*surfq(j)           ! loss by surface runoff 
         LatQ_DIC(1,j) = peroc_DIC(j)*con_DIC*flat(1,j)    ! loss by lateral flow  for layer   
         PerQ_DIC(1,j) = con_DIC*sol_prk(1,j)                                ! loss by percolation into underlying soil layer
      Else
           SurQ_DIC(j)= 0.                                                                        ! loss by surface runoff
          LatQ_DIC(1,j) =0.                                                                  ! loss by lateral flow  for layer   
          PerQ_DIC(1,j) = 0.                                                               ! loss by percolation into underlying soil layer 
     Endif 
        ! update DIC mass blance afer transport loss 
        Sol_DIC(1,j) = Sol_DIC(1,j) - SurQ_DIC(j) - LatQ_DIC(1,j) - PerQ_DIC(1,j) 
       
      DO k=2,sol_nly(j)    
        swmm = 0.
        swmm = sol_st(k,j)+sol_wpmm(k,j)                                            !SW-soil water content (mm) simuated by SWAT
        sat_DIC = 0.
      !  sat_DIC=10*swmm*0.01                                        ! DIC saturation (kg/ha)   10mg C/l
         sat_DIC=100.*swmm*0.01                                        ! DIC saturation (kg/ha)   10mg C/l 
       xx=0. 
      If (Sol_DIC(k,j) > sat_DIC ) Then 
       xx=Sol_DIC(k,j)  - sat_DIC
       Sol_DIC(k,j)  = sat_DIC 
       sol_RSPC1(k,j) = sol_RSPC(k,j)+xx   
      Else
        Sol_DIC(k,j)  = Sol_DIC(k,j)  + PerQ_DIC(k-1,j)  + sol_RSPC(k,j)    !add DIC percolation from upper layer and OC minelization  
       xx = 0.
       if ( Sol_DIC(k,j)  > sat_DIC ) then
        if( Sol_DIC(k,j)  - sol_RSPC(k,j) > sat_DIC ) then      
         xx = Sol_DIC(k,j) -sol_RSPC(k,j)- sat_DIC           !! extra DIC ( perly_DIC)  from percolation, will added to calculated lat and perc
        Sol_DIC(k,j) = sat_DIC 
         sol_RSPC1(k,j) = sol_RSPC(k,j)+xx
        else
         sol_RSPC1(k,j) =Sol_DIC(k,j) -sat_DIC 
         Sol_DIC(k,j)  = sat_DIC       
        end if 
       else
         sol_RSPC1(k,j) =0.
       end if 
      
      End if
            
         mwmm=0.
         mwmm=sol_prk(k,j) + flat(k,j)
        PerQ_DIC(k,j) =0.     !! used for pass perc C to the underlying layer
       if (Sol_DIC(k,j) >=0.  .and.   mwmm>0.)  then
            con_DIC=0.
           ! con_DIC = Sol_DIC(k,j) *(1.-exp(-mwmm/(sol_por(k,j)*( sol_z(k,j)- sol_z(k-1,j) ))))/mwmm        ! DIC concentration in mobile water (kg/ha*mm)
           con_DIC = Sol_DIC(k,j) /swmm                                                                           ! DIC concentration in mobile water (kg/ha*mm)
          
           LatQ_DIC(k,j)=con_DIC*flat(k,j)  ! +  xx* flat(k,j)/mwmm                                                          !loss by lateral flow  for layer   
           PerQ_DIC(k,j) =con_DIC*sol_prk(k,j) !+ xx*sol_prk(k,j)/mwmm                                            ! DIC percolation into underlying layer
              ! update DIC mass by subtracting the transport loss 
            Sol_DIC(k,j) =Sol_DIC(k,j) -LatQ_DIC(k,j)- PerQ_DIC(k,j)     
        else
        LatQ_DIC(k,j)=0.                                                                !loss by lateral flow  for layer     
        PerQ_DIC(k,j) =0.                                               ! DIC percolation into underlying layer
        endif
  
      End Do

      
     
     DO k=1,sol_nly(j)
      !! Update total resp C
      rspc_dnew(j) = rspc_dnew(j) +  sol_RSPC1(k,j) 
     !DIC loss by lateral flow of whole profile   
       LatQT_DIC(j)= LatQT_DIC(j) + LatQ_DIC(k,j)      
     End do

       End select 
!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

 
      DO k=1,sol_nly(j)
           if (k == 1) then
            !10 cm / 1000 = 0.01m; 1 ha = 10000 m2; ton/m3; * 1000 --> final unit is kg/ha; rock fraction is considered
 		    sol_mass = (10) / 1000.* 10000. * sol_bd(k,j)* 1000. *(1- sol_rock(k,j) / 100.)            
          else
		    sol_mass = (sol_z(k,j) - sol_z(k-1,j)) / 1000.* 10000. * sol_bd(k,j)* 1000. *(1- sol_rock(k,j) / 100.)
	      end if
 
              sol_rsd(k,j)= sol_LS(k,j)+sol_LM(k,j)            
              sol_orgn(k,j) = sol_HPN(k,j)
              sol_aorgn(k,j) = sol_HSN(k,j)
              sol_fon(k,j) = sol_LMN(k,j) + sol_LSN(k,j) 
              sol_cbn(k,j) = 100*(sol_HSC(k,j) + sol_HPC(k,j) + sol_BMC(k,j))/sol_mass 
   
     
     
     !! Lateral flow C
   
      
      latc_d(j) = latc_d(j) +   LatQ_DOC(k,j) +  LatQ_DIC(k,j)          
 

      END DO
      
      
      !!no LDOC from HRU or landscape  

       HRU_LDOC(j)=  0.           
       !! Percolation C         
       PerQB_DOC(j)= PerQ_DOC(sol_nly(j),j)                     !Calculate DOC percolation amount from lowest soil layer to shallow aquifer     
       PerQB_DIC(j)= PerQ_DIC(sol_nly(j),j)  
       percc_d(j)= PerQB_DOC(j) + PerQB_DIC(j)
     
      !! Surface runoff  C including DOC and DIC
       surfqc_d(j) = QBC*(surfq(j)/(surfq(j)+flat(1,j)+1.e-6))+  SurQ_DIC(j)     
 
       ccp(j) = temp_ccp 
 
      
      return
      end
