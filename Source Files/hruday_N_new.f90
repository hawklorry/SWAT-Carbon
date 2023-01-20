           subroutine hruday_N

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: j, sb, ii, iflag
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname
      
      integer:: ly, idplant, icl
         j=0
         j=ihru
   
  
      sb = hru_sub(j)
      iflag = 0
      do ii = 1, itoth
        if (ipdhru(ii) == j) iflag = 1
      end do
      if (iflag == 0) return

       pdvas = 0.
       pdvs = 0.

       pdvas(1) = solc_no3_ini(j)             !! initial total NO3 in soil ; kg N/ha  
       pdvas(2) = solc_no3(j)                  !! Total no3 in soil profile; kg N/ha 
       pdvas(3) = solc_nh4_ini(j)             !! initial total NH3 in soil ; kg N/ha  
       pdvas(4) = solc_nh4(j)                  !! Total nh3 in soil profile; kg N/ha 
       pdvas(5) = solc_orgn_ini(j)/1000.      !! Initial orgn in soil profile; ton N/ha                                                                                                                                       !! initial total orgN in soil   kg N/ha  
       pdvas(6) = solc_orgn(j)/1000.          !! Total orgn in soil profile; ton N/ha     
       pdvas(7) = no3_fert(j)+ no3_conf(j)+ no3_grazf(j)    !! kg N/ha 
       pdvas(8) = no3_autof(j)                              !! kg N/ha 
       pdvas(9) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)        !! kg N/ha 
       pdvas(10) = nh4_autof(j)      !! kg N/ha 
       pdvas(11) = no3_rain(j)       !nh3pcp !! kg N/ha 
       pdvas(12) = nh4_rain(j)       !! kg N/ha 
       pdvas(13) = orgn_fert(j)+orgn_grazf(j)      !! including cont., auto, and fer; kg N/ha
       pdvas(14) = OrgN_Plt2Rsd (j)  !! Total residue input N ; kg N/ha                   
       pdvas(15) = no3_immo(j)  !                  
       pdvas(16) = no3_up(j) 
       pdvas(17) = no3_nitr(j)     !! total nitr N kg N/ha 
       pdvas(18) = no3_denit(j)   !! total den N kg N/ha          
       pdvas(19) = absorbed_no3(j) 
       pdvas(20) = nh4_min(j)    !nh4_vol(j)   !OrgN_Plt2Rsd (j)
       pdvas(21) = nh4_vol(j)
       pdvas(22) = absorbed_nh3(j)
       pdvas(23) = N2O_nit(j) 
       pdvas(24) = NO_nit(j)   !*1000  
       pdvas(25) = N2O_den(j) !*1000  !! g /ha    !!   30
       pdvas(26) = NO_den(j) !*1000  !! g /ha     !!    32
       pdvas(27) = N2_den(j)    
       pdvas(28) = N2O(j)   !*1000                       !*1000 N2O_den(j)                         !! gN/ha     !!SWATCUP =87
       pdvas(29) = NO(j)    !*1000                            !NO_den(j) *1000                                         !NO(j) *1000 
   
       pdvas(30) = surqno3_0(j)      !gw_qdeep(j)
       pdvas(31) = tileno3(j)
       pdvas(32) = latno3_0(j)               !      latno3(j) 
       pdvas(33) = percn(j)               !! kg N/ha 
       pdvas(34) = sedorgn_0(j)               !cmtot_kgh(j)        
       pdvas(35) = Sed_RPON(j)                               ! RPON from hru to stream(kg/ha)                                     
       pdvas(36) = Sed_LPON(j)                               ! LPON from hru to stream (kg/ha)
       pdvas(37) = SurQ_DON(j)                              ! Surface RDON to stream  (kg/ha)
       pdvas(38) = LatQT_DON(j)                            ! Lateral RDON to stream  (kg/ha)
       pdvas(39) = PerQ_DON (sol_nly(j),j)              ! RDON percolation amount from lowest soil layer to shallow aquifer
       pdvas(40) = rchrg_don (j)                                 ! RDON recharge to aquifer  (kg/ha) 
       pdvas(41) = revap_don(j)                                  ! revap DON to soils             (kg/ha)
       pdvas(42) = GwQ_DON (j)                             ! GW RDON to stream (kg/ha) 
       pdvas(43) = shallst_don_decay(j)                      ! DON decay in shallow aquifer (kg/ha) 
       pdvas(44) = gwseep_don(j)                                !! DON seep to deeper aquifer  (kg/ha) 
       pdvas(45) = shallst_don(j)                                  !! state     DON in shallow aquifer  (kg/ha) 
 
       !!  Balance for Soil NH4 
       pdvas(46) = nh4_autof(j) + nh4_fert(j) + nh4_conf(j) + nh3_grazf(j) + nh4_min(j) + nh4_rain(j)  - ( (solc_nh4(j)+ solc_urea(j)- solc_urea_ini(j)-solc_nh4_ini(j)) + nh4_vol(j) + no3_nitr(j) + absorbed_nh3(j) )  
       !!  Balance for Soil NO3 
       pdvas(47) = no3_fert(j) + no3_autof(j)+ no3_conf(j)+ no3_grazf(j) + no3_nitr(j) + no3_rain(j) - ( (solc_no3(j) -  solc_no3_ini(j) )  +  no3_up(j)  + surqno3_0(j) + latno3_0(j) + percn(j) + no3_denit(j) + no3_immo(j) + absorbed_no3(j)+ N2O_nit(j) + NO_nit(j) )                                                               !!  Balance for Soil NO3           ! soltmp_500(j) 
       !!  Balance for Soil orgN including residue n 
       pdvas(48) = OrgN_Plt2Rsd (j) + orgn_fert(j) + orgn_grazf(j)+ no3_immo(j) + absorbed_no3(j)+ absorbed_nh3(j) - ( (solc_orgn(j) - solc_orgn_ini(j)) + sedorgn_0(j) + PerQ_DON (sol_nly(j),j)+ SurQ_DON_0(j)+LatQT_DON_0(j) + nh4_min(j)    )    
       !! Balance for total soil N
      ! if(j==1) then
       !print*,i,j,solc_orgn(j), solc_orgn_ini(j),OrgN_Plt2Rsd (j) + orgn_fert(j) + no3_immo(j) + absorbed_no3(j)+ absorbed_nh3(j), sedorgn_0(j) + PerQ_DON (sol_nly(j),j)+ SurQ_DON_0(j)+LatQT_DON_0(j) + nh4_min(j)   
       !end if
       pdvas(49) = (OrgN_Plt2Rsd(j)+no3_fert(j)+no3_autof(j)+ no3_conf(j)+ no3_grazf(j) +nh4_fert(j)+nh4_autof(j)+nh4_conf(j) + nh3_grazf(j) + no3_rain(j) + nh4_rain(j) + orgn_fert(j)+ orgn_grazf(j)       &                                           
             -  no3_up(j) - sedorgn_0(j) - surqno3_0(j) - latno3_0(j) - percn(j) - N2O(j) - NO(j)- N2_den(j) - nh4_vol(j) - PerQ_DON (sol_nly(j),j)- SurQ_DON_0(j)- LatQT_DON_0(j) )           &                             !bactrolp + bactsedlp
             -  (   (solc_no3(j)-  solc_no3_ini(j) )  + (solc_nh4(j)-solc_nh4_ini(j))   +   (solc_orgn(j)-  solc_orgn_ini(j))   )  
    
      pdvas(50) = plantn(j)
      pdvas(51) = grainN(j) 
    

       pdvas(52) = (  Min_ML_MBN(j)   &                ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         + Min_SL_MBN(j)         &               !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         + Min_SL_SHN(j)   &              ! mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         + Min_MB_SHN(j)   &              ! mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         + Min_MB_PHN(j)   &              ! mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         + Min_SH_MBN(j)   &              ! mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         + Min_SH_PHN(j)   &              ! mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         + Min_PH_MBN(j)   &              ! mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
         - IMM_ML_MBN(j)   &              ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         - IMM_SL_MBN(j)   &              ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         - IMM_SL_SHN(j)   &              ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         - IMM_MB_SHN(j)   &              ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         - IMM_MB_PHN(j)   &              ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         - IMM_SH_MBN(j)   &              ! immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         - IMM_SH_PHN(j)   &              ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         - IMM_PH_MBN(j)    )             ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                                     !! kg C/ha
       
       pdvas(53) = IMMO_ERR(j)     !
                    
   
      call xmon 
 
      idplant = idplt(j)
      if (idplant > 0) then
        cropname = cpnm(idplant)
      else
        cropname = "NOCR"
      endif

         write (output_N_hru_num,1002) cropname, j, subnum(j),        &
          hruno(j), sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),     &
          (pdvas(ii), ii = 1, 53)
1002  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,       &
      53f10.3)   

 
     

      return

1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,8e10.3,2f10.3,1x,i4,45f10.3)  
1001  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,66f10.3,e10.3,e10.3,8f10.3,3f10.3,45f10.3)    
      end
