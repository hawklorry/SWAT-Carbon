      subroutine header_sqlite

!!    ~ ~ ~ PURPOSE ~ ~ ~                                               
!!    This subroutine defines header titles for the different output files

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~                                    
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hedb(:)     |NA            |column titles in subbasin output file
!!    hedr(:)     |NA            |column titles in reach output file
!!    hedrsv(:)   |NA            |column titles in reservoir output file
!!    heds(:)     |NA            |column titles in HRU output file
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output 
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SQLITE ~ ~ ~
!!    1. To create column in SQLite, replace space and - with underscore _
!!    2. delete (), replace ( with underscore _
!!    3. delete arrays for VB interface output. They are not used any more
!!    4. change m^3 to m3
!!    ~ ~ ~ END SQLITE ~ ~ ~

      use parm

!!    column headers for HRU output file
      heds = (/"  PRECIPmm"," SNOFALLmm"," SNOMELTmm","     IRRmm",    &!4
        !pdvas(1) = subp(j), pdvas(2) = snofall, pdvas(3) = snomlt,pdvas(4) = aird(j)                        
              "     PETmm","      ETmm"," SW_INITmm","  SW_ENDmm",     &!8
        !pdvas(5) = pet_day, pdvas(6) = etday, pdvas(7) = sol_cnsw(j), pdvas(8) = sol_sw(j)   
              "    PERCmm"," GW_RCHGmm"," DA_RCHGmm","   REVAPmm",     &!12
        !pdvas(9) = sepbtm(j), pdvas(10) = rchrg(j), pdvas(11) = gwseep, pdvas(12) = revapday                         
              "  SA_IRRmm","  DA_IRRmm","   SA_STmm","   DA_STmm",     &!16
        !pdvas(13) = shallirr(j), pdvas(14) = deepirr(j),pdvas(15) = shallst(j),pdvas(16) = deepst(j)                
              "SURQ_GENmm","SURQ_CNTmm"," LATQGENmm"," LATQCNTmm",     &!20
        !pdvas(17) = surfq(j),pdvas(18) = qday,pdvas(19) = latq(j),pdvas(20) = latq(j) - lpndloss - lwetloss            
              "   QTILEmm","    GW_Qmm","  GW_Q_Dmm","   TLOSSmm",     &!24
        !pdvas(21) = qtile, pdvas(22) = gw_q(j), pdvas(23) = gw_qdeep(j), pdvas(24) = tloss                
	      "    WYLDmm","   DAILYCN"," TMP_AVdgC"," TMP_MXdgC",     &!28		          
	    !pdvas(25) = qdr(j), pdvas(26) = cnday(j), pdvas(27) = tmpav(j),pdvas(28) = tmx(j)
	      " TMP_MNdgC","SOLARMJ_m2","  SYLDt_ha","  USLEt_ha",     &!32				"SOL_TMPdgC",
	    !pdvas(29) = tmn(j),pdvas(30) = hru_ra(j), pdvas(31) = sedyld(j) / hru_ha(j),pdvas(32) = sedgen(j)/ hru_ha(j)
              " ORGNkg_ha"," ORGPkg_ha"," SEDPkg_ha","N_APPkg_ha",     &!36
        !pdvas(33) = sedorgn(j), pdvas(34) = sedorgp(j), pdvas(35) = sedminpa(j) + sedminps(j),pdvas(36) = fertn  
              "P_APPkg_ha","NAUTOkg_ha","PAUTOkg_ha"," NGRZkg_ha",     &!40
        !pdvas(37) = fertp, pdvas(38) = auton, pdvas(39) = autop, pdvas(40) = grazn             
              " PGRZkg_ha","NCFRTkg_ha","PCFRTkg_ha","NRAINkg_ha",     &!44
        !pdvas(41) = grazp,pdvas(42) = cfertn, pdvas(43) = cfertp, pdvas(44) = no3pcp        
              " NFIXkg_ha","  NUPkg_ha","  PUPkg_ha"," DNITkg_ha",     &!48
        !pdvas(45) = fixn, pdvas(46) = nplnt(j), pdvas(47) = pplnt(j) , pdvas(48) = wdntl
              "  NSQkg_ha","  NLQkg_ha"," TNO3kg_ha","  NGWkg_ha",     &!52
        !pdvas(49) = surqno3(j),pdvas(50) = latno3(j), pdvas(51) = tileno3(j), pdvas(52) = no3gw(j)   
	      " NO3Lkg_ha","  NRgkg_ha"," NReVkg_ha"," NSepkg_ha",     &!56
	    !pdvas(53) = percn(j), pdvas(54) = rchrg_n(j), pdvas(55) = revapn(j), pdvas(56) = gwseepn(j)    
	      " GWNLkg_ha"," SA_Nkg_ha"," SQ_Pkg_ha"," TVAPkg_ha",     &!60
        !pdvas(57) = gw_no3loss(j), pdvas(58) = shallst_n(j), pdvas(59) = surqsolp(j), pdvas(60) = vap_tile 
	      "  GWPkg_ha","    W_STRS","  TMP_STRS","    N_STRS",     &!64
        !pdvas(61) = minpgw(j),pdvas(62) = (1.-strsw(j)), pdvas(63) = (1.-strstmp(j)), pdvas(64) = (1.-strsn(j)) 
	      "    P_STRS","  BIOMt_ha","       LAI","   YLDt_ha",     &!68
        !pdvas(65) = (1.-strsp(j)), pdvas(66) = bio_ms(j) / 1000. ,pdvas(67) = laiday(j), pdvas(68) = yield/1000.
	      "  YLDgt_ha","  YLDbt_ha","  YLDtt_ha","  YLDrt_ha",     &!72
        !pdvas(69) = yieldgrn/1000., pdvas(70) = yieldbms/1000., pdvas(71) = yieldtbr/1000., pdvas(72) = yieldrsd/1000.
	      "   BACTPct","  BACTLPct"," WTAB_CLIm"," WTAB_SOLm",     &!76            !!R682 10/20/21 nbs 
        !pdvas(73) = bactrop + bactsedp, pdvas(74) = bactrolp + bactsedlp, pdvas(75) = wtab(j), pdvas(76) = wat_tbl(j)          
	      "     SNOmm","   RSDt_ha","   COVt_ha",		       &!79
        !pdvas(77) = sno_hru(j), pdvas(78) = sol_rsd(1,j)/1000., pdvas(79) = sol_cov(j)/1000., 
                                         

              "   N2Og_ha","    NOg_ha"," VNH4kg_ha","  DNNkg_ha",	&!83
        !pdvas(80) = N2O(j)*1000., pdvas(81) = NO(j)*1000., pdvas(82) = nh4_vol(j), pdvas(83) = no3_denit(j)                                         
              " NITNkg_ha"," UNO3kg_ha"," FNO3kg_ha"," FNH4kg_ha",	&!87
        !pdvas(84) = no3_nitr(j), pdvas(85) = no3_up(j), pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j),pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)
              " FRONkg_ha"," TNO3kg_ha"," TNH3kg_ha","   TONt_ha",	&!91
        !pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j),pdvas(89) = solc_no3(j),pdvas(90) = solc_nh4(j),pdvas(91) = solc_orgn(j)/1000.  
              "  S_OPt_ha","  S_OCt_ha"," S_OSCt_ha"," SedCkg_ha", 	&!95
        !pdvas(92) = solc_orgp(j)/1000.,pdvas(93) = solc_orgc(j)/1000.,pdvas(94) = solc_orgcs(j)/1000.,pdvas(95) = sedc_d(j) 
              " SrfCkg_ha"," LatCkg_ha"," PerCkg_ha"," NPPCkg_ha", 	&!99
        !pdvas(96) = surfqc_d(j), pdvas(97) = latc_d(j) ,pdvas(98) = percc_d(j),pdvas(99) = NPPC_d(j)                    
              " RspCkg_ha"," FROCkg_ha"," RPOCkg_ha"," LPOCkg_ha",	&!103
        !pdvas(100) = rspc_dnew(j),pdvas(101) = OrgC_Plt2Rsd(j)+ OrgC_Fer(j),pdvas(102) = Sed_RPOC(j),pdvas(103) = Sed_LPOC(j)  
              " RDOCkg_ha","LDOCkg_ha1","  DICkg_ha"," SDOCkg_ha", 	&!107
        !pdvas(104) = HRU_RDOC(j) ,pdvas(105) = HRU_LDOC(j),pdvas(106) = HRU_DIC(j) ,pdvas(107) = SurQ_DOC(j) 
              "LDOCkg_ha2"," PDOCkg_ha"," GDOCkg_ha"," SDICkg_ha",	&!111
        !pdvas(108) = LatQT_DOC(j),pdvas(109) = PerQB_DOC(j), pdvas(110) = GwQ_DOC(j),pdvas(111) = SurQ_DIC(j)
              " LDICkg_ha"," GDICkg_ha"," TDOCkg_ha"," TDICkg_ha",	&!115
        !pdvas(112) = LatQT_DIC(j), pdvas(113) = PerQB_DIC(j),pdvas(114) = solc_doc(j),pdvas(115) = solc_dic(j) 
              "  SurTempC"," SolT50cmC","  ST100cmC","  ST150cmC",  	&!119
        !pdvas(116) = sur_tmp(j), pdvas(117) = soltmp_50(j), pdvas(118) = soltmp_100(j), pdvas(119) = soltmp_150(j) 
              "  ST200cmC","  ST300cmC","  ST350cmC"," ST1000cmC",	&!123
        !pdvas(120) = soltmp_200(j) ,pdvas(121) = soltmp_300(j),pdvas(122) = soltmp_500(j),pdvas(123) = soltmp_1000(j)
              "  FrozeDay"," RPONkg_ha"," LPONkg_ha"," SDONkg_ha",   	&!127    
        !pdvas(124) = sol_frozday(j),pdvas(125) = Sed_RPON(j),pdvas(126) = Sed_LPON(j),pdvas(127) = SurQ_DON(j)            
              " LDONkg_ha"," PDONkg_ha"," rcDONkg_h"," GDONkg_ha", 	&!131
        !pdvas(128) = LatQT_DON(j),pdvas(129) = PerQ_DON (sol_nly(j),j), pdvas(130) = rchrg_don (j),pdvas(131) = GwQ_DON (j)   
              " dDONkg_ha"," rvDONkg_h"," gpDONkg_h"," shaDONkgh", 	&!135
        !pdvas(132) = shallst_don_decay(j) ,pdvas(133) = revap_don(j),pdvas(134) = gwseep_don(j),pdvas(135) = shallst_don(j)       
              " rcDOCkg_h"," dcDOCkg_h"," rvDOCkg_h"," gpDOCkg_h", 	&!139
        !pdvas(136) = rchrg_doc(j),pdvas(137) = shallst_doc_decay(j), pdvas(138) = revap_doc(j),pdvas(139) = gwseep_doc(j)        
              " shaDOCkgh"," GDICkg_ha"," rcDICkg_h"," rvDICkg_h",	&!143
        !pdvas(140) = shallst_doc(j), pdvas(141) = GwQ_DIC(j), pdvas(142) = rchrg_dic(j), pdvas(143) = revap_dic(j)         
              " gpDICkg_h"," shaDICkgh","  SOC1t_ha","  SOC2t_ha",	&!147
        !pdvas(144) = gwseep_dic(j), pdvas(145) = shallst_dic(j), pdvas(146) = sol_soc(1,j), pdvas(147) = sol_soc(2,j)
              "  SOC3t_ha","  SOC4t_ha","  SOC5t_ha","  SOC6t_ha", 	&!151
        !pdvas(148) = sol_soc(3,j), pdvas(149) = sol_soc(4,j), pdvas(150) = sol_soc(5,j), pdvas(151) = sol_soc(6,j)
              "  SOC7t_ha", "  SOCt_ha8" /)     					 !153
        !pdvas(152) = sol_soc(7,j), pdvas(153) = sol_soc(8,j)

!!    column headers for subbasin output file
      hedb = (/"  PRECIPmm"," SNOMELTmm","     PETmm","      ETmm",   &     !4
              "      SWmm","    PERCmm","    SURQmm","    GW_Qmm",    &     !8
              "    WYLDmm","  SYLDt_ha"," ORGNkg_ha"," ORGPkg_ha",     &    !12
              "NSURQkg_ha"," SOLPkg_ha"," SEDPkg_ha"," LAT_Q_mm",   &      !16
              "LATNO3kg_h","GWNO3kg_ha","CHOLAmic_L","CBODU_mg_L",     &    !20
!    &        " DOXQ mg_L","   QTILEmm"," TNO3kg_ha"," TVAPkg_ha"/)
              " DOXQ_mg_L"," TNO3kg_ha","   QTILEmm"," TVAPkg_ha",&         !24
         "   N2Og_ha","    NOg_ha"," VNH4kg_ha","    CH4g_ha",&             !28
           "DNITNkg_ha", " NITNkg_ha","PERCNkg_ha","UPNO3kg_ha",&           !32
          "FRNO3kg_ha","FRNH4kg_ha","FRORNkg_ha","RNNO3kg_ha",  &           !36
         " FIXNkg_ha", "S_NO3kg_ha", "S_NH3kg_ha","  S_ONt_ha",&            !40
          "  S_OPt_ha"," SedCkg_ha", "SurfCkg_ha", " LatCkg_ha",&           !44
          "PercCkg_ha", " NPPCkg_ha", " RspCkg_ha", " SnoFallmm",&          !48
         "SnoDepthmm", "SnoWatermm", "  SurTempC", " SolT50cmC", &          !52
         "SolT100cmC", "SolT150cmC", "SolT200cmC", "SolT300cmC",&           !56
         "SolT350cmC", "SoT1000cmC", "  FrozeDay", "  AirTempC",&           !60
         "       SWC","Sol_TotalC","Sol_TotaSC", " wtmp_SurQ",&             !64
         " wtmp_LatQ",  "  wtmp_GwQ"                  /)

!!  added headers TOTAL N/TOTALP/NO3 Concentration TO HEADING FOR OUTPUT.RCH GSM 10/26/2011
!!    column headers for reach output file
      hedr = (/"  FLOW_INcms"," FLOW_OUTcms","     EVAPcms",         &          !3
              "    TLOSScms","  SED_INtons"," SED_OUTtons",          &          !6
              " SEDCONCmg_L","   ORGN_INkg","  ORGN_OUTkg",          &          !9
              "   ORGP_INkg","  ORGP_OUTkg","    NO3_INkg",          &          !12
              "   NO3_OUTkg","    NH4_INkg","   NH4_OUTkg",          &          !15
              "    NO2_INkg","   NO2_OUTkg","   MINP_INkg",          &          !18
              "  MINP_OUTkg","   CHLA_INkg","  CHLA_OUTkg",          &          !21
              "   CBOD_INkg","  CBOD_OUTkg","  DISOX_INkg",          &          !24
              " DISOX_OUTkg","  SOLPST_Img","  SOLPST_Omg",          &          !27
              "  SORPST_Img","  SORPST_Omg","  REACTPSTmg",          &          !30
              "    VOLPSTmg","  SETTLPSTmg"," RESUS_PSTmg",          &          !33
              "   DIFFPSTmg","    BEDPSTmg","   BURYPSTmg",          &          !36
              "   BED_PSTmg","  BACTP_O_ct"," BACTLP_O_ct",          &          !39
              "  CMETAL_1kg","  CMETAL_2kg","  CMETAL_3kg",          &          !42
              "     TOT_Nkg","     TOT_Pkg"," NO3ConcMg_l",          &          !45
              "    WTMPdegc",						&                           !46
          "   RPOC_INkg","  RPOC_OUTkg", "   LPOC_INkg","  LPOC_OUTkg",&        !50
          "   RDOC_INkg","  RDOC_OUTkg", "   LDOC_INkg","  LDOC_OUTkg",&        !54
          "    DIC_INkg","   DIC_OUTkg", "  TotalPOCkg","  TotalDOCkg", &       !58
          "   TOC_OUTkg"                                                &       !59
  !   &    "    rchstor ","   rcharea  ", "  rchdep    ","  sdti       "        
         /)

!!    column headers for reach sediment output file
!!    previous in readfile.f format 1080
      hedsed=(/ "  SED_INtons"," SED_OUTtons",&
                " SAND_INtons","SAND_OUTtons",&
                " SILT_INtons","SILT_OUTtons",&
                " CLAY_INtons","CLAY_OUTtons",&
                " SMAG_INtons","SMAG_OUTtons",&
                "  LAG_INtons"," LAG_OUTtons",&
                "  GRA_INtons"," GRA_OUTtons",&
                "  CH_BNKtons","  CH_BEDtons",&
                "  CH_DEPtons","  FP_DEPtons",&
               "     TSSmg_L"/)

!!    column headers for reservoir output file
      hedrsv = (/"    VOLUMEm3","  FLOW_INcms"," FLOW_OUTcms",&          
                 "    PRECIPm3","      EVAPm3","   SEEPAGEm3",&          
                 "  SED_INtons"," SED_OUTtons"," SED_CONCppm",&          
                 "   ORGN_INkg","  ORGN_OUTkg"," RES_ORGNppm",&          
                 "   ORGP_INkg","  ORGP_OUTkg"," RES_ORGPppm",&          
                 "    NO3_INkg","   NO3_OUTkg","  RES_NO3ppm",&          
                 "    NO2_INkg","   NO2_OUTkg","  RES_NO2ppm",&          
                 "    NH3_INkg","   NH3_OUTkg","  RES_NH3ppm",&          
                 "   MINP_INkg","  MINP_OUTkg"," RES_MINPppm",&          
                 "   CHLA_INkg","  CHLA_OUTkg","SECCHIDEPTHm",&          
                 "   PEST_INmg","  REACTPSTmg","    VOLPSTmg",&          
                 "  SETTLPSTmg","RESUSP_PSTmg","DIFFUSEPSTmg",&          
                 "REACBEDPSTmg","   BURYPSTmg","  PEST_OUTmg",&          
                 "PSTCNCWmg_m3","PSTCNCBmg_m3"/)

!!    column headers for HRU impoundment output file
      hedwtr = (/"  PNDPCPmm","  PND_INmm","PSED_It_ha","  PNDEVPmm",&
                 "  PNDSEPmm"," PND_OUTmm","PSED_Ot_ha","  PNDVOLm3",&
                 "PNDORGNppm"," PNDNO3ppm","PNDORGPppm","PNDMINPppm",&   
                 "PNDCHLAppm","  PNDSECIm","  WETPCPmm","  WET_INmm",&   
                 "WSED_It_ha","  WETEVPmm","  WETSEPmm"," WET_OUTmm",&
                 "WSED_Ot_ha","  WETVOLm3","WETORGNppm"," WETNO3ppm",&
                 "WETORGPppm","WETMINPppm","WETCHLAppm","  WETSECIm",&   
                 "  POTPCPmm","  POT_INmm","OSED_It_ha","  POTEVPmm",&
                 "  POTSEPmm"," POT_OUTmm","OSED_Ot_ha","  POTVOLm3",&
                 "  POT_SAha","HRU_SURQmm","PLANT_ETmm"," SOIL_ETmm"/)

!!    column headers for HRU potholes output file
!!    previous in readfile.f format 1000
      hedpot = (/" VOL_I","  SA_I","SPILLO","POTSEP",&
                 " POTEV","SOL_SW","TILE_O"," VOL_F",&
                 "  SA_F"/)

!!    column headers for ave annual hru output
!!    previous in stdaa.f format 1800
      hedahu = (/"   AREAkm2","        CN","     AWCmm","   USLE_LS",&
                 "     IRRmm","   AUTONkh","   AUTOPkh","     MIXEF",&
                 "    PRECmm"," SURQGENmm","     GWQmm","      ETmm",&
                 "     SEDth","    NO3kgh","   ORGNkgh","    BIOMth",&
                 "     YLDth","    SURQmm"/)

!!    column headers for monthly basin value
!!    previous in stdaa.f format 2000
      hedamo = (/"   rain_mm","   snow_mm","  surfQ_mm","   latQ_mm",&
                 " watery_mm","     ET_mm","  sedy_tha","    PET_mm"/)

!!    column headers for daily, monthly and yearly watershed summary
!!    the first part of output.std
!!    previous in std3.f format 1300
      hedwshd =(/"   PREC_mm","   SURQ_mm","   LATQ_mm","    GWQ_mm",&
                 "PERCOLA_mm","  TILEQ_mm","     SW_mm","     ET_mm",&
                 "    PET_mm","   WYLD_mm"," SYLD_tons","  NO3_SURQ",&
                 "  NO3_LATQ","  NO3_PERC","  NO3_CROP","     N_ORG",&
                 "     P_SOL","     P_ORG","   TILENO3"/)

!!    column headers for mgt operations, output.mgt which is output only when IMGT = 1
!!    previous in readfile.f format 999
      hedmgt = (/"   PHUBASE","    PHUACC",& !7,8
                 "    SOL_SW","    BIO_MS",& !9,10
                 "   SOL_RSD","SOL_SUMNO3",& !11,12
                 "SOL_SUMSOP","     YIELD",& !13,14
                 "   MIX_EFF","  FERT_AMT",& !15,16
                 "  FERT_NO3","  FERT_NH3",& !17,18
                 " FERT_ORGN"," FERT_SOLP",& !19,20
                 " FERT_ORGP",             & !21
                 "  PEST_AMT","     STRSN",& !22,23
                 "     STRSP","   STRSTMP",& !24,25
                 "     STRSW","     STRSA",& !26,27
                 " YIELD_GRN"," YIELD_BMS",& !28,29
                 " YIELD_TBR"," YIELD_RSD",& !30,31
                 "   YIELD_N","   YIELD_P",& !32,33
                 "MANURE_AMT",             & !34
                 "   IRR_AMT"/)             !35

!!    column headers for soil nutrient, output.snu (previously output.sol) which will be generated when ISOL = 1
!!    previous in readfile.f format 12222
      hedsnu = (/"SU_SOL_RSD","     SOL_P","       NO3","     ORG_N",&
                 "     ORG_P","        CN"/)

!!    column headers for soil water in each layer
!!    output.swr which will be generated when ISTO = 1
!!    previous in readfile.f format 5001
      hedswr = (/"    LAYER1","    LAYER2","    LAYER3","    LAYER4",&
                 "    LAYER5","    LAYER6","    LAYER7","    LAYER8",&
                 "    LAYER9","   LAYER10"/)

!!    column headers for channel dimesion, chan.deg
!!    previous in std1.f format 7000
      heddeg = (/"   DEPTH_M","   WIDTH_M"," SLOPE_M_M"/)


      return
      end                                           
