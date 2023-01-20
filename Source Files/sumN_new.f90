       subroutine sumN
       !! this subroutine performs summary calculations for HRU based on land uses, soils, and slopes
       use parm
       use parm_subC
       use parm_output
       implicit none 
       
       integer :: j, k
       real :: cnv 
       integer:: ly 
       j = 0
       j = ihru

       cnv = 0.
       cnv = 10. * hru_ha(j)

      if (curyr > nyskip) then
        
       !! watershed summations for N
       if (ffcst == 0 .and. iscen == 1) then
      
         wshddayN(1) = wshddayN(1) + iniorgn(j) * hru_dafr(j) !/1000.  ! ton N/ha  !! initial orgN               !! KG 
         wshddayN(2) = wshddayN(2) + solc_orgn_fnl(j) * hru_dafr(j) !/1000.  ! ton N/ha  !! final orgN                 !! KG 
	     wshddayN(3) = wshddayN(3) + inino3(j) * hru_dafr(j)   !! intial NO3                 !! KG 
	     wshddayN(4) = wshddayN(4) + solc_no3_fnl(j) * hru_dafr(j)    !! final  NO3                 !! KG 
         wshddayN(5) = wshddayN(5) + ininh3(j) * hru_dafr(j)   !! inital nh3                 !! KG    
         wshddayN(6) = wshddayN(6) + solc_nh4_fnl(j)  * hru_dafr(j)   !! final  nh3                 !! KG 
         wshddayN(7) = wshddayN(7) + (no3_fert(j)+ no3_conf(j) + no3_grazf(j)+ no3_autof(j)) * hru_dafr(j)              !!  NO3 fertilization KG 
         wshddayN(8) = wshddayN(8) + (nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)) * hru_dafr(j)              !! NH4 fertilizaiton KG
         wshddayN(9) = wshddayN(9) + (orgn_fert(j)+ orgn_grazf(j)) * hru_dafr(j)             !! OrgN fertilization KG
         wshddayN(10) = wshddayN(10) + fixn * hru_dafr(j)                                      !! KG
         wshddayN(11) = wshddayN(11) + (no3_rain(j)+nh4_rain(j))* hru_dafr(j)                  !! KG                                                        
         wshddayN(12) = wshddayN(12) + sedorgn(j) * hru_dafr(j)                             !! KG
         wshddayN(13) = wshddayN(13) + surqno3(j) * hru_dafr(j)                           !! KG
         wshddayN(14) = wshddayN(14) + latno3(j) * hru_dafr(j)                                !! KG
         wshddayN(15) = wshddayN(15) + percn(j) * hru_dafr(j)                               !! KG
         wshddayN(16) = wshddayN(16) + no3gw(j) * hru_dafr(j)                               !! KG
         wshddayN(17) = wshddayN(17) + tileno3(j) * hru_dafr(j)                                       !! KG
         wshddayN(18) = wshddayN(18) + grainN(j) * hru_dafr(j)                                    !! KG
         wshddayN(19) = wshddayN(19) + no3_up(j) * hru_dafr(j)                                   !! KG
         wshddayN(20) = wshddayN(20) + no3_denit(j) * hru_dafr(j)                          !! KG
         wshddayN(21) = wshddayN(21) + nh4_vol(j) * hru_dafr(j)                             !! KG
         wshddayN(22) = wshddayN(22) + no3_nitr(j) * hru_dafr(j)                               !! KG
         wshddayN(23) = wshddayN(23) + SurQ_DON(j) * hru_dafr(j)                        !! KG
         wshddayN(24) = wshddayN(24) + LatQT_DON(j) * hru_dafr(j)                      !! KG
         wshddayN(25) = wshddayN(25) + rchrg_don(j) * hru_dafr(j)                           !! KG
         wshddayN(26) = wshddayN(26) + GwQ_DON(j) * hru_dafr(j)                      !! KG
         wshddayN(27) = wshddayN(27) + N2O(j) * hru_dafr(j)                                 !! KG
         wshddayN(28) = wshddayN(28) + NO(j) * hru_dafr(j)                                   !! KG
         wshddayN(29) = wshddayN(29) + N2_den(j) * hru_dafr(j)                             !! KG
         wshddayN(30) = wshddayN(30) + N2O_nit(j) * hru_dafr(j)                            !! KG
         wshddayN(31) = wshddayN(31) + NO_nit(j) * hru_dafr(j)                               !! KG
         wshddayN(32) = wshddayN(32) + gw_no3loss(j) * hru_dafr(j)                        !! KG
         wshddayN(33) = wshddayN(33) + revapn(j) * hru_dafr(j)                                !! KG
         wshddayN(34) = wshddayN(34) + gwseepn(j) * hru_dafr(j)                             !! KG
         wshddayN(35) = wshddayN(35) + revap_don(j) * hru_dafr(j)                         !! KG
         wshddayN(36) = wshddayN(36) + shallst_don_decay(j) * hru_dafr(j)               !! KG 
         wshddayN(37) = wshddayN(37) + gwseep_don(j) * hru_dafr(j)                       !! KG
         wshddayN(38) = wshddayN(38) + Min_ML_MBN(j) * hru_dafr(j)                    ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         wshddayN(39) = wshddayN(39) + Min_SL_MBN(j) * hru_dafr(j)                        !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(40) = wshddayN(40) + Min_SL_SHN(j) * hru_dafr(j)                    !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(41) = wshddayN(41) + Min_MB_SHN(j) * hru_dafr(j)                          !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         wshddayN(42) = wshddayN(42) + Min_MB_PHN(j) * hru_dafr(j)                          !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         wshddayN(43) = wshddayN(43) + Min_SH_MBN(j) * hru_dafr(j)                        !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(44) = wshddayN(44) + Min_SH_PHN(j) * hru_dafr(j)                       !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         wshddayN(45) = wshddayN(45) + Min_PH_MBN(j) * hru_dafr(j)                              !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
         wshddayN(46) = wshddayN(46) + IMM_ML_MBN(j) * hru_dafr(j)                     ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         wshddayN(47) = wshddayN(47) + IMM_SL_MBN(j) * hru_dafr(j)                         ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(48) = wshddayN(48) + IMM_SL_SHN(j) * hru_dafr(j)                        ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(49) = wshddayN(49) + IMM_MB_SHN(j) * hru_dafr(j)                             ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         wshddayN(50) = wshddayN(50) + IMM_MB_PHN(j) * hru_dafr(j)                         ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         wshddayN(51) = wshddayN(51) + IMM_SH_MBN(j) * hru_dafr(j)                           !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(52) = wshddayN(52) + IMM_SH_PHN(j) * hru_dafr(j)                           ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         wshddayN(53) = wshddayN(53) + IMM_PH_MBN(j) * hru_dafr(j)                          ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
         wshddayN(54) = wshddayN(54) + OrgN_Plt2Rsd(j)* hru_dafr(j)   
         wshddayN(55) = wshddayN(55) + nh4_min(j) * hru_dafr(j)
         wshddayN(56) = wshddayN(56) + no3_immo(j) * hru_dafr(j)
         !!factors
         wshddayN(57) = wshddayN(57) + WFPS_ave(j) * hru_dafr(j)
         wshddayN(58) = wshddayN(58) + st_ave(j) * hru_dafr(j)
         wshddayN(59) = wshddayN(59) + ph_ave(j) * hru_dafr(j)
         wshddayN(60) = wshddayN(60) + rspc_d(j) * hru_dafr(j)
         ! if ( LUC_std == 1) then   !! ouput.std for Agri lands --UMRB--------------------
         
    !      if (idplt(j)/=6 .and. idplt(j)/=7 .and. idplt(j)/=8 &          !! Forest
     !   .and. idplt(j)/=9 .and.  idplt(j)/=10  &                            !! Wetland
    !    .and. idplt(j)/=5 .and. idplt(j)/=12 .and. idplt(j)/=15 &      !! Grass
    !    .and. idplt(j)/=40 .and. idplt(j)/=52   &                        !! Grass
     !   .and. idplt(j)/=18 ) then                                          !! Water
        
     !   hru_dafrAgr1 = hru_haAgr1 + hru_ha(j)
     
     
     
        
 
         !!---------------------------------output.std for different landuse---------------------------------------------------------------------------------{
         
         If(idplt(j)==6 .or. idplt(j)==7 .or. idplt(j)==8) Then       !! Foresty /////////////////////////////    
         hru_haFor = hru_haFor +  hru_ha(j)         !!ha
         
         wshddayN(120) = wshddayN(120) + iniorgn(j) * hru_ha(j) ! /1000.  ! ton N/ha                       !! KG 
         wshddayN(121) = wshddayN(121) + solc_orgn_fnl(j) * hru_ha(j) !/1000.  ! ton N/ha                              !! KG 
	     wshddayN(122) = wshddayN(122) + inino3(j) * hru_ha(j)                               !! KG 
	     wshddayN(123) = wshddayN(123) + solc_no3_fnl(j) * hru_ha(j)                                !! KG 
         wshddayN(124) = wshddayN(124) + ininh3(j) * hru_ha(j)                               !! KG    
         wshddayN(125) = wshddayN(125) + solc_nh4_fnl(j) * hru_ha(j)                               !! KG 
         wshddayN(126) = wshddayN(126) + (no3_fert(j)+ no3_conf(j) + no3_grazf(j)+ no3_autof(j)) * hru_ha(j)              !! KG 
         wshddayN(127) = wshddayN(127) + (nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)) * hru_ha(j)              !! KG
         wshddayN(128) = wshddayN(128) + (orgn_fert(j)+ orgn_grazf(j)) * hru_ha(j)             !! KG
         wshddayN(129) = wshddayN(129) + fixn * hru_ha(j)                                      !! KG
         wshddayN(130) = wshddayN(130) + (no3_rain(j)+nh4_rain(j))* hru_ha(j)                  !! KG                                                        
         wshddayN(131) = wshddayN(131) + sedorgn(j) * hru_ha(j)                             !! KG
         wshddayN(132) = wshddayN(132) + surqno3(j) * hru_ha(j)                           !! KG
         wshddayN(133) = wshddayN(133) + latno3(j) * hru_ha(j)                                !! KG
         wshddayN(134) = wshddayN(134) + percn(j) * hru_ha(j)                               !! KG
         wshddayN(135) = wshddayN(135) + no3gw(j) * hru_ha(j)                               !! KG
         wshddayN(136) = wshddayN(136) + tileno3(j) * hru_ha(j)                                       !! KG
         wshddayN(137) = wshddayN(137) + grainN(j) * hru_ha(j)                                    !! KG
         wshddayN(138) = wshddayN(138) + no3_up(j) * hru_ha(j)                                   !! KG
         wshddayN(139) = wshddayN(139) + no3_denit(j)  * hru_ha(j)                          !! KG
         wshddayN(140) = wshddayN(140) + nh4_vol(j) * hru_ha(j)                             !! KG
         wshddayN(141) = wshddayN(141) + no3_nitr(j) * hru_ha(j)                               !! KG
         wshddayN(142) = wshddayN(142) + SurQ_DON(j) * hru_ha(j)                        !! KG
         wshddayN(143) = wshddayN(143) + LatQT_DON(j) * hru_ha(j)                      !! KG
         wshddayN(144) = wshddayN(144) + rchrg_don(j) * hru_ha(j)                           !! KG
         wshddayN(145) = wshddayN(145) + GwQ_DON(j) * hru_ha(j)                      !! KG
         wshddayN(146) = wshddayN(146) + N2O(j) * hru_ha(j)                                 !! KG
         wshddayN(147) = wshddayN(147) + NO(j) * hru_ha(j)                                   !! KG
         wshddayN(148) = wshddayN(148) + N2_den(j) * hru_ha(j)                             !! KG
         wshddayN(149) = wshddayN(149) + N2O_nit(j) * hru_ha(j)                            !! KG
         wshddayN(150) = wshddayN(150) + NO_nit(j) * hru_ha(j)                               !! KG
         wshddayN(151) = wshddayN(151) + gw_no3loss(j) * hru_ha(j)                        !! KG
         wshddayN(152) = wshddayN(152) + revapn(j) * hru_ha(j)                                !! KG
         wshddayN(153) = wshddayN(153) + gwseepn(j) * hru_ha(j)                             !! KG
         wshddayN(154) = wshddayN(154) + revap_don(j)   * hru_ha(j)                         !! KG
         wshddayN(155) = wshddayN(155) + shallst_don_decay(j)*hru_ha(j)               !! KG 
         wshddayN(156) = wshddayN(156) + gwseep_don(j)  * hru_ha(j)                       !! KG
         wshddayN(157) = wshddayN(157) + Min_ML_MBN(j) * hru_ha(j)                    ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         wshddayN(158) = wshddayN(158) + Min_SL_MBN(j)  * hru_ha(j)                        !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(159) = wshddayN(159) + Min_SL_SHN(j) * hru_ha(j)                    !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(160) = wshddayN(160) + Min_MB_SHN(j) * hru_ha(j)                          !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         wshddayN(161) = wshddayN(161) + Min_MB_PHN(j) * hru_ha(j)                          !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         wshddayN(162) = wshddayN(162) + Min_SH_MBN(j) * hru_ha(j)                        !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(163) = wshddayN(163) + Min_SH_PHN(j) * hru_ha(j)                       !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         wshddayN(164) = wshddayN(164) + Min_PH_MBN(j) * hru_ha(j)                              !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
         wshddayN(165) = wshddayN(165) + IMM_ML_MBN(j) * hru_ha(j)                     ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         wshddayN(166) = wshddayN(166) + IMM_SL_MBN(j) * hru_ha(j)                         ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(167) = wshddayN(167) + IMM_SL_SHN(j) * hru_ha(j)                        ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(168) = wshddayN(168) + IMM_MB_SHN(j) * hru_ha(j)                             ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         wshddayN(169) = wshddayN(169) + IMM_MB_PHN(j) * hru_ha(j)                         ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         wshddayN(170) = wshddayN(170) + IMM_SH_MBN(j) * hru_ha(j)                           !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(171) = wshddayN(171) + IMM_SH_PHN(j) * hru_ha(j)                           ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         wshddayN(172) = wshddayN(172) + IMM_PH_MBN(j) * hru_ha(j)                          ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
         wshddayN(173) = wshddayN(173) + OrgN_Plt2Rsd(j) * hru_ha(j)   
         wshddayN(174) = wshddayN(174) + nh4_min(j) * hru_ha(j) 
         wshddayN(175) = wshddayN(175) + no3_immo(j) * hru_ha(j) 
         !!factors
         wshddayN(176) = wshddayN(176) + WFPS_ave(j) * hru_ha(j)
         wshddayN(177) = wshddayN(177) + st_ave(j) * hru_ha(j)
         wshddayN(178) = wshddayN(178) + ph_ave(j) * hru_ha(j)
         wshddayN(179) = wshddayN(179) + rspc_d(j) * hru_ha(j)
         
         Else If (idplt(j)==5 .or. idplt(j)==12 .or. idplt(j)==15   &    !! Grass /////////////////////////////////////
             .or.   idplt(j)==40 .or. idplt(j)==52 ) then  
         hru_haGra = hru_haGra +  hru_ha(j)   !!ha
       
         wshddayN(180) = wshddayN(180) + iniorgn(j) * hru_ha(j)  ! /1000.  ! ton N/ha                             !! KG 
         wshddayN(181) = wshddayN(181) + solc_orgn_fnl(j) * hru_ha(j)  !/1000.  ! ton N/ha                             !! KG 
	     wshddayN(182) = wshddayN(182) + inino3(j)  * hru_ha(j)                               !! KG 
	     wshddayN(183) = wshddayN(183) + solc_no3_fnl(j)  * hru_ha(j)                              !! KG 
         wshddayN(184) = wshddayN(184) + ininh3(j)  * hru_ha(j)                              !! KG    
         wshddayN(185) = wshddayN(185) + solc_nh4_fnl(j)  * hru_ha(j)                               !! KG 
         wshddayN(186) = wshddayN(186) + (no3_fert(j)+ no3_conf(j) + no3_grazf(j)+ no3_autof(j)) * hru_ha(j)                               !! KG 
         wshddayN(187) = wshddayN(187) + (nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)) * hru_ha(j)                                !! KG
         wshddayN(188) = wshddayN(188) + (orgn_fert(j)+ orgn_grazf(j)) * hru_ha(j)                             !! KG
         wshddayN(189) = wshddayN(189) + fixn * hru_ha(j)                                      !! KG
         wshddayN(190) = wshddayN(190) + (no3_rain(j)+nh4_rain(j)) * hru_ha(j)                !! KG                                                                                     !! KG
         wshddayN(191) = wshddayN(191) + sedorgn(j)  * hru_ha(j)                            !! KG
         wshddayN(192) = wshddayN(192) + surqno3(j) * hru_ha(j)                             !! KG
         wshddayN(193) = wshddayN(193) + latno3(j)  * hru_ha(j)                               !! KG
         wshddayN(194) = wshddayN(194) + percn(j)* hru_ha(j)                                !! KG
         wshddayN(195) = wshddayN(195) + no3gw(j)* hru_ha(j)                              !! KG
         wshddayN(196) = wshddayN(196) + tileno3(j) * hru_ha(j)                                     !! KG
         wshddayN(197) = wshddayN(197) + grainN(j) * hru_ha(j)                                   !! KG
         wshddayN(198) = wshddayN(198) + no3_up(j)* hru_ha(j)                                 !! KG
         wshddayN(199) = wshddayN(199) + no3_denit(j) * hru_ha(j)                         !! KG
         wshddayN(200) = wshddayN(200) + nh4_vol(j) * hru_ha(j)                            !! KG
         wshddayN(201) = wshddayN(201) + no3_nitr(j) * hru_ha(j)                              !! KG
         wshddayN(202) = wshddayN(202) + SurQ_DON(j)* hru_ha(j)                       !! KG
         wshddayN(203) = wshddayN(203) + LatQT_DON(j) * hru_ha(j)                    !! KG
         wshddayN(204) = wshddayN(204) + rchrg_don(j) * hru_ha(j)                         !! KG
         wshddayN(205) = wshddayN(205) + GwQ_DON(j) * hru_ha(j)                      !! KG
         wshddayN(206) = wshddayN(206) + N2O(j) * hru_ha(j)                                 !! KG
         wshddayN(207) = wshddayN(207) + NO(j)* hru_ha(j)                                    !! KG
         wshddayN(208) = wshddayN(208) + N2_den(j)* hru_ha(j)                             !! KG
         wshddayN(209) = wshddayN(209) + N2O_nit(j) * hru_ha(j)                         !! KG
         wshddayN(210) = wshddayN(210) + NO_nit(j) * hru_ha(j)                             !! KG
         wshddayN(211) = wshddayN(211) + gw_no3loss(j)* hru_ha(j)                       !! KG
         wshddayN(212) = wshddayN(212) + revapn(j) * hru_ha(j)                               !! KG
         wshddayN(213) = wshddayN(213) + gwseepn(j)* hru_ha(j)                            !! KG
         wshddayN(214) = wshddayN(214) + revap_don(j)* hru_ha(j)                         !! KG
         wshddayN(215) = wshddayN(215) + shallst_don_decay(j)* hru_ha(j)              !! KG 
         wshddayN(216) = wshddayN(216) + gwseep_don(j)* hru_ha(j)                       !! KG
         wshddayN(217) = wshddayN(217) + Min_ML_MBN(j)* hru_ha(j)                   ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         wshddayN(218) = wshddayN(218) + Min_SL_MBN(j)* hru_ha(j)                        !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(219) = wshddayN(219) + Min_SL_SHN(j)* hru_ha(j)                  !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(220) = wshddayN(220) + Min_MB_SHN(j)* hru_ha(j)                         !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         wshddayN(221) = wshddayN(221) + Min_MB_PHN(j)* hru_ha(j)                         !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         wshddayN(222) = wshddayN(222) + Min_SH_MBN(j)* hru_ha(j)                      !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(223) = wshddayN(223) + Min_SH_PHN(j)* hru_ha(j)                      !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         wshddayN(224) = wshddayN(224) + Min_PH_MBN(j)* hru_ha(j)                            !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
         wshddayN(225) = wshddayN(225) + IMM_ML_MBN(j)* hru_ha(j)                   ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         wshddayN(226) = wshddayN(226) + IMM_SL_MBN(j)* hru_ha(j)                          ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(227) = wshddayN(227) + IMM_SL_SHN(j)* hru_ha(j)                       ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(228) = wshddayN(228) + IMM_MB_SHN(j)* hru_ha(j)                           ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         wshddayN(229) = wshddayN(229) + IMM_MB_PHN(j)* hru_ha(j)                         ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         wshddayN(230) = wshddayN(230) + IMM_SH_MBN(j)* hru_ha(j)                            !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(231) = wshddayN(231) + IMM_SH_PHN(j)* hru_ha(j)                          ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         wshddayN(232) = wshddayN(232) + IMM_PH_MBN(j)* hru_ha(j)                         ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
         wshddayN(233) = wshddayN(233) + OrgN_Plt2Rsd(j)* hru_ha(j)   
         wshddayN(234) = wshddayN(234) + nh4_min(j) * hru_ha(j) 
         wshddayN(235) = wshddayN(235) + no3_immo(j) * hru_ha(j) 
         !!factors
         wshddayN(236) = wshddayN(236) + WFPS_ave(j) * hru_ha(j)
         wshddayN(237) = wshddayN(237) + st_ave(j) * hru_ha(j)
         wshddayN(238) = wshddayN(238) + ph_ave(j) * hru_ha(j)
         wshddayN(239) = wshddayN(239) + rspc_d(j) * hru_ha(j)
         
         Else If (idplt(j)==9.or. idplt(j)==10 ) then    !! Wetland ////.or. idplt(j)==18 /////////////////////////////////
          hru_haWat =hru_haWat +  hru_ha(j)   !!ha
         
         wshddayN(240) = wshddayN(240) + iniorgn(j) * hru_ha(j) ! /1000.  ! ton N/ha                         !! KG 
         wshddayN(241) = wshddayN(241) + solc_orgn_fnl(j)* hru_ha(j) !/1000.  ! ton N/ha                             !! KG 
	     wshddayN(242) = wshddayN(242) + inino3(j) * hru_ha(j)                              !! KG 
	     wshddayN(243) = wshddayN(243) + solc_no3_fnl(j) * hru_ha(j)                           !! KG 
         wshddayN(244) = wshddayN(244) + ininh3(j) * hru_ha(j)                             !! KG    
         wshddayN(245) = wshddayN(245) + solc_nh4_fnl(j)* hru_ha(j)                               !! KG 
         wshddayN(246) = wshddayN(246) + (no3_fert(j)+ no3_conf(j) + no3_grazf(j)+ no3_autof(j)) * hru_ha(j)                              !! KG 
         wshddayN(247) = wshddayN(247) + (nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)) * hru_ha(j)                                !! KG
         wshddayN(248) = wshddayN(248) + (orgn_fert(j)+ orgn_grazf(j)) * hru_ha(j)                            !! KG
         wshddayN(249) = wshddayN(249) + fixn * hru_ha(j)                                     !! KG
         wshddayN(250) = wshddayN(250) + (no3_rain(j)+nh4_rain(j))* hru_ha(j)               !! KG                                                                                                 !! KG
         wshddayN(251) = wshddayN(251) + sedorgn(j)* hru_ha(j)                            !! KG
         wshddayN(252) = wshddayN(252) + surqno3(j)* hru_ha(j)                            !! KG
         wshddayN(253) = wshddayN(253) + latno3(j) * hru_ha(j)                              !! KG
         wshddayN(254) = wshddayN(254) + percn(j)* hru_ha(j)                                !! KG
         wshddayN(255) = wshddayN(255) + no3gw(j) * hru_ha(j)                             !! KG
         wshddayN(256) = wshddayN(256) + tileno3(j)* hru_ha(j)                                      !! KG
         wshddayN(257) = wshddayN(257) + grainN(j)* hru_ha(j)                                   !! KG
         wshddayN(258) = wshddayN(258) + no3_up(j) * hru_ha(j)                                !! KG
         wshddayN(259) = wshddayN(259) + no3_denit(j) * hru_ha(j)                         !! KG
         wshddayN(260) = wshddayN(260) + nh4_vol(j)* hru_ha(j)                             !! KG
         wshddayN(261) = wshddayN(261) + no3_nitr(j) * hru_ha(j)                               !! KG
         wshddayN(262) = wshddayN(262) + SurQ_DON(j) * hru_ha(j)                      !! KG
         wshddayN(263) = wshddayN(263) + LatQT_DON(j)* hru_ha(j)                     !! KG
         wshddayN(264) = wshddayN(264) + rchrg_don(j)* hru_ha(j)                          !! KG
         wshddayN(265) = wshddayN(265) + GwQ_DON(j)* hru_ha(j)                       !! KG
         wshddayN(266) = wshddayN(266) + N2O(j) * hru_ha(j)                                 !! KG
         wshddayN(267) = wshddayN(267) + NO(j) * hru_ha(j)                                   !! KG
         wshddayN(268) = wshddayN(268) + N2_den(j)* hru_ha(j)                             !! KG
         wshddayN(269) = wshddayN(269) + N2O_nit(j) * hru_ha(j)                           !! KG
         wshddayN(270) = wshddayN(270) + NO_nit(j)* hru_ha(j)                              !! KG
         wshddayN(271) = wshddayN(271) + gw_no3loss(j)* hru_ha(j)                       !! KG
         wshddayN(272) = wshddayN(272) + revapn(j) * hru_ha(j)                               !! KG
         wshddayN(273) = wshddayN(273) + gwseepn(j)* hru_ha(j)                             !! KG
         wshddayN(274) = wshddayN(274) + revap_don(j) * hru_ha(j)                         !! KG
         wshddayN(275) = wshddayN(275) + shallst_don_decay(j)*hru_ha(j)              !! KG 
         wshddayN(276) = wshddayN(276) + gwseep_don(j) * hru_ha(j)                     !! KG
         wshddayN(277) = wshddayN(277) + Min_ML_MBN(j)* hru_ha(j)                   ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         wshddayN(278) = wshddayN(278) + Min_SL_MBN(j)* hru_ha(j)                        !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(279) = wshddayN(279) + Min_SL_SHN(j) * hru_ha(j)                 !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(280) = wshddayN(280) + Min_MB_SHN(j) * hru_ha(j)                        !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         wshddayN(281) = wshddayN(281) + Min_MB_PHN(j) * hru_ha(j)                         !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         wshddayN(282) = wshddayN(282) + Min_SH_MBN(j)* hru_ha(j)                       !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(283) = wshddayN(283) + Min_SH_PHN(j) * hru_ha(j)                      !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         wshddayN(284) = wshddayN(284) + Min_PH_MBN(j) * hru_ha(j)                             !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
         wshddayN(285) = wshddayN(285) + IMM_ML_MBN(j) * hru_ha(j)                    ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         wshddayN(286) = wshddayN(286) + IMM_SL_MBN(j)  * hru_ha(j)                        ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(287) = wshddayN(287) + IMM_SL_SHN(j)* hru_ha(j)                       ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(288) = wshddayN(288) + IMM_MB_SHN(j)* hru_ha(j)                            ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         wshddayN(289) = wshddayN(289) + IMM_MB_PHN(j) * hru_ha(j)                       ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         wshddayN(290) = wshddayN(290) + IMM_SH_MBN(j) * hru_ha(j)                           !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(291) = wshddayN(291) + IMM_SH_PHN(j) * hru_ha(j)                          ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         wshddayN(292) = wshddayN(292) + IMM_PH_MBN(j) * hru_ha(j)                        ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
         wshddayN(293) = wshddayN(293) + OrgN_Plt2Rsd (j)* hru_ha(j)   
         wshddayN(294) = wshddayN(294) + nh4_min(j) * hru_ha(j) 
         wshddayN(295) = wshddayN(295) + no3_immo(j) * hru_ha(j)
         !!factors
         wshddayN(296) = wshddayN(296) + WFPS_ave(j) * hru_ha(j)
         wshddayN(297) = wshddayN(297) + st_ave(j) * hru_ha(j)
         wshddayN(298) = wshddayN(298) + ph_ave(j) * hru_ha(j)
         wshddayN(299) = wshddayN(299) + rspc_d(j) * hru_ha(j)

         Else  if(    idplt(j) /= 18   ) then         !! Agriculture lands                                                                     !! Agriculture///////////////////////////////////////////
         hru_haAgr = hru_haAgr + hru_ha(j)                        !!ha
        
         wshddayN(300) = wshddayN(300) + iniorgn(j)* hru_ha(j)  !/1000.  ! ton N/ha                            !! KG 
         wshddayN(301) = wshddayN(301) + solc_orgn_fnl(j)* hru_ha(j) ! /1000.  ! ton N/ha                            !! KG 
	     wshddayN(302) = wshddayN(302) + inino3(j)* hru_ha(j)                               !! KG 
	     wshddayN(303) = wshddayN(303) + solc_no3_fnl(j)* hru_ha(j)                               !! KG 
         wshddayN(304) = wshddayN(304) + ininh3(j)* hru_ha(j)                               !! KG    
         wshddayN(305) = wshddayN(305) + solc_nh4_fnl(j)* hru_ha(j)                               !! KG 
         wshddayN(306) = wshddayN(306) + (no3_fert(j)+ no3_conf(j) + no3_grazf(j)+ no3_autof(j)) * hru_ha(j)                                !! KG 
         wshddayN(307) = wshddayN(307) + (nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)) * hru_ha(j)                              !! KG
         wshddayN(308) = wshddayN(308) + (orgn_fert(j)+ orgn_grazf(j)) * hru_ha(j)                           !! KG
         wshddayN(309) = wshddayN(309) + fixn* hru_ha(j)                                    !! KG
         wshddayN(310) = wshddayN(310) + (no3_rain(j)+nh4_rain(j))  * hru_ha(j)             !! KG                                                                                           !! KG
         wshddayN(311) = wshddayN(311) + sedorgn(j)* hru_ha(j)                              !! KG
         wshddayN(312) = wshddayN(312) + surqno3(j) * hru_ha(j)                             !! KG
         wshddayN(313) = wshddayN(313) + latno3(j)* hru_ha(j)                               !! KG
         wshddayN(314) = wshddayN(314) + percn(j) * hru_ha(j)                               !! KG
         wshddayN(315) = wshddayN(315) + no3gw(j)* hru_ha(j)                                !! KG
         wshddayN(316) = wshddayN(316) + tileno3(j)* hru_ha(j)                                   !! KG
         wshddayN(317) = wshddayN(317) + grainN(j)* hru_ha(j)                               !! KG
         wshddayN(318) = wshddayN(318) + no3_up(j) * hru_ha(j)                              !! KG
         wshddayN(319) = wshddayN(319) + no3_denit(j)* hru_ha(j)                            !! KG
         wshddayN(320) = wshddayN(320) + nh4_vol(j)* hru_ha(j)                              !! KG
         wshddayN(321) = wshddayN(321) + no3_nitr(j)* hru_ha(j)                               !! KG
         wshddayN(322) = wshddayN(322) + SurQ_DON(j) * hru_ha(j)                      !! KG
         wshddayN(323) = wshddayN(323) + LatQT_DON(j) * hru_ha(j)                    !! KG
         wshddayN(324) = wshddayN(324) + rchrg_don(j) * hru_ha(j)                         !! KG
         wshddayN(325) = wshddayN(325) + GwQ_DON(j)* hru_ha(j)                       !! KG
         wshddayN(326) = wshddayN(326) + N2O(j) * hru_ha(j)                                 !! KG
         wshddayN(327) = wshddayN(327) + NO(j) * hru_ha(j)                                   !! KG
         wshddayN(328) = wshddayN(328) + N2_den(j) * hru_ha(j)                            !! KG
         wshddayN(329) = wshddayN(329) + N2O_nit(j)* hru_ha(j)                            !! KG
         wshddayN(330) = wshddayN(330) + NO_nit(j) * hru_ha(j)                             !! KG
         wshddayN(331) = wshddayN(331) + gw_no3loss(j) * hru_ha(j)                      !! KG
         wshddayN(332) = wshddayN(332) + revapn(j) * hru_ha(j)                               !! KG
         wshddayN(333) = wshddayN(333) + gwseepn(j) * hru_ha(j)                            !! KG
         wshddayN(334) = wshddayN(334) + revap_don(j)  * hru_ha(j)                        !! KG
         wshddayN(335) = wshddayN(335) + shallst_don_decay(j)*hru_ha(j)            !! KG 
         wshddayN(336) = wshddayN(336) + gwseep_don(j)  * hru_ha(j)                     !! KG
         wshddayN(337) = wshddayN(337) + Min_ML_MBN(j)* hru_ha(j)                   ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         wshddayN(338) = wshddayN(338) + Min_SL_MBN(j)* hru_ha(j)                        !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(339) = wshddayN(339) + Min_SL_SHN(j) * hru_ha(j)                  !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(340) = wshddayN(340) + Min_MB_SHN(j)* hru_ha(j)                         !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         wshddayN(341) = wshddayN(341) + Min_MB_PHN(j) * hru_ha(j)                         !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         wshddayN(342) = wshddayN(342) + Min_SH_MBN(j) * hru_ha(j)                      !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(343) = wshddayN(343) + Min_SH_PHN(j) * hru_ha(j)                      !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         wshddayN(344) = wshddayN(344) + Min_PH_MBN(j) * hru_ha(j)                             !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
         wshddayN(345) = wshddayN(345) + IMM_ML_MBN(j)* hru_ha(j)                     ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         wshddayN(346) = wshddayN(346) + IMM_SL_MBN(j)* hru_ha(j)                          ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         wshddayN(347) = wshddayN(347) + IMM_SL_SHN(j) * hru_ha(j)                     ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         wshddayN(348) = wshddayN(348) + IMM_MB_SHN(j)* hru_ha(j)                            ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         wshddayN(349) = wshddayN(349) + IMM_MB_PHN(j) * hru_ha(j)                        ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         wshddayN(350) = wshddayN(350) + IMM_SH_MBN(j) * hru_ha(j)                           !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         wshddayN(351) = wshddayN(351) + IMM_SH_PHN(j) * hru_ha(j)                          ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         wshddayN(352) = wshddayN(352) + IMM_PH_MBN(j) * hru_ha(j)                        ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
         wshddayN(353) = wshddayN(353) + OrgN_Plt2Rsd(j)* hru_ha(j)   
         wshddayN(354) = wshddayN(354) + nh4_min(j) * hru_ha(j) 
         wshddayN(355) = wshddayN(355) + no3_immo(j) * hru_ha(j)
         !!factors
         wshddayN(356) = wshddayN(356) + WFPS_ave(j) * hru_ha(j)
         wshddayN(357) = wshddayN(357) + st_ave(j) * hru_ha(j)
         wshddayN(358) = wshddayN(358) + ph_ave(j) * hru_ha(j)
         wshddayN(359) = wshddayN(359) + rspc_d(j) * hru_ha(j)
     
         End If 
         
         !!----------------------------------output.std for different landuse--------------------------------------------------------------------------------}
         end if
         
          end if              !! if (curyr > nyskip) then
          
          
          return
          end