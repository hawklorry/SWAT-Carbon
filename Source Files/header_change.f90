      subroutine header

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

      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output 
        implicit none
!!    column headers for HRU output file
      !output.hru
      !!problems to address: 55 and 76, the smae latno3
      !!to add sol_rsd(1,j), sol_cov(j),N2O_den(j)
      !!to add yield, yieldgrn, yieldbms,yieldrsd
      !!yieldbms = hi_bms * (1.-rwt(j)) * bio_ms(j)*harveff
      !!clipbms = hi_bms * (1.-rwt(j)) * bio_ms(j) * (1. - harveff)  
      !!yieldtbr: tuber yield
      !!sol_rsd(:,:)   |kg/ha         |amount of organic matter in the soil
      heds = (/"  PRECIPmm",    " SNOFALLmm",   " SNOMELTmm",   "     IRRmm",    &!4
        !pdvas(1) = subp(j), pdvas(2) = snofall, pdvas(3) = snomlt,pdvas(4) = aird(j)                        
              "     PETmm",     "      ETmm",   " SW_INITmm",   "  SW_ENDmm",     &!8
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
	      " TMP_MNdgC","SOLARMJ/m2","  SYLDt/ha","  USLEt/ha",     &!32				"SOL_TMPdgC",
	    !pdvas(29) = tmn(j),pdvas(30) = hru_ra(j), pdvas(31) = sedyld(j) / hru_ha(j),pdvas(32) = sedgen(j)/ hru_ha(j)
              " ORGNkg/ha"," ORGPkg/ha"," SEDPkg/ha","N_APPkg/ha",     &!36
        !pdvas(33) = sedorgn(j), pdvas(34) = sedorgp(j), pdvas(35) = sedminpa(j) + sedminps(j),pdvas(36) = fertn  
              "P_APPkg/ha","NAUTOkg/ha","PAUTOkg/ha"," NGRZkg/ha",     &!40
        !pdvas(37) = fertp, pdvas(38) = auton, pdvas(39) = autop, pdvas(40) = grazn             
              " PGRZkg/ha","NCFRTkg/ha","PCFRTkg/ha","NRAINkg/ha",     &!44
        !pdvas(41) = grazp,pdvas(42) = cfertn, pdvas(43) = cfertp, pdvas(44) = no3pcp        
              " NFIXkg/ha","  NUPkg/ha","  PUPkg/ha"," DNITkg/ha",     &!48
        !pdvas(45) = fixn, pdvas(46) = nplnt(j), pdvas(47) = pplnt(j) , pdvas(48) = wdntl
              "  NSQkg/ha","  NLQkg/ha"," TNO3kg/ha","  NGWkg/ha",     &!52
        !pdvas(49) = surqno3(j),pdvas(50) = latno3(j), pdvas(51) = tileno3(j), pdvas(52) = no3gw(j)   
	      " NO3Lkg/ha","  NRgkg/ha"," NReVkg/ha"," NSepkg/ha",     &!56
	    !pdvas(53) = percn(j), pdvas(54) = rchrg_n(j), pdvas(55) = revapn(j), pdvas(56) = gwseepn(j)    
	      " GWNLkg/ha"," SA_Nkg/ha"," SQ_Pkg/ha"," TVAPkg/ha",     &!60
        !pdvas(57) = gw_no3loss(j), pdvas(58) = shallst_n(j), pdvas(59) = surqsolp(j), pdvas(60) = vap_tile 
	      "  GWPkg/ha","    W_STRS","  TMP_STRS","    N_STRS",     &!64
        !pdvas(61) = minpgw(j),pdvas(62) = (1.-strsw(j)), pdvas(63) = (1.-strstmp(j)), pdvas(64) = (1.-strsn(j)) 
	      "    P_STRS","  BIOMt/ha","       LAI","   YLDt/ha",     &!68
        !pdvas(65) = (1.-strsp(j)), pdvas(66) = bio_ms(j) / 1000. ,pdvas(67) = laiday(j), pdvas(68) = yield/1000.
	      "  YLDgt/ha","  YLDbt/ha","  YLDtt/ha","  YLDrt/ha",     &!72
        !pdvas(69) = yieldgrn/1000., pdvas(70) = yieldbms/1000., pdvas(71) = yieldtbr/1000., pdvas(72) = yieldrsd/1000.
	      "   BACTPct","  BACTLPct"," WTAB_CLIm"," WTAB_SOLm",     &!76            !!R682 10/20/21 nbs 
        !pdvas(73) = bactrop + bactsedp, pdvas(74) = bactrolp + bactsedlp, pdvas(75) = wtab(j), pdvas(76) = wat_tbl(j)          
	      "     SNOmm","   RSDt/ha","   COVt/ha",		       &!79
        !pdvas(77) = sno_hru(j), pdvas(78) = sol_rsd(1,j)/1000., pdvas(79) = sol_cov(j)/1000., 
                                         

              "   N2Og/ha","    NOg/ha"," VNH4kg/ha","  DNNkg/ha",	&!83
        !pdvas(80) = N2O(j)*1000., pdvas(81) = NO(j)*1000., pdvas(82) = nh4_vol(j), pdvas(83) = no3_denit(j)                                         
              " NITNkg/ha"," UNO3kg/ha"," FNO3kg/ha"," FNH4kg/ha",	&!87
        !pdvas(84) = no3_nitr(j), pdvas(85) = no3_up(j), pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j),pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)
              " FRONkg/ha"," TNO3kg/ha"," TNH3kg/ha","   TONt/ha",	&!91
        !pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j),pdvas(89) = solc_no3(j),pdvas(90) = solc_nh4(j),pdvas(91) = solc_orgn(j)/1000.  
              "  S_OPt/ha","  S_OCt/ha"," S_OSCt/ha"," SedCkg/ha", 	&!95
        !pdvas(92) = solc_orgp(j)/1000.,pdvas(93) = solc_orgc(j)/1000.,pdvas(94) = solc_orgcs(j)/1000.,pdvas(95) = sedc_d(j) 
              " SrfCkg/ha"," LatCkg/ha"," PerCkg/ha"," NPPCkg/ha", 	&!99
        !pdvas(96) = surfqc_d(j), pdvas(97) = latc_d(j) ,pdvas(98) = percc_d(j),pdvas(99) = NPPC_d(j)                    
              " RspCkg/ha"," FROCkg/ha"," RPOCkg/ha"," LPOCkg/ha",	&!103
        !pdvas(100) = rspc_dnew(j),pdvas(101) = OrgC_Plt2Rsd(j)+ OrgC_Fer(j),pdvas(102) = Sed_RPOC(j),pdvas(103) = Sed_LPOC(j)  
              " RDOCkg/ha"," LDOCkg/ha","  DICkg/ha"," SDOCkg/ha", 	&!107
        !pdvas(104) = HRU_RDOC(j) ,pdvas(105) = HRU_LDOC(j),pdvas(106) = HRU_DIC(j) ,pdvas(107) = SurQ_DOC(j) 
              " LDOCkg/ha"," PDOCkg/ha"," GDOCkg/ha"," SDICkg/ha",	&!111
        !pdvas(108) = LatQT_DOC(j),pdvas(109) = PerQB_DOC(j), pdvas(110) = GwQ_DOC(j),pdvas(111) = SurQ_DIC(j)
              " LDICkg/ha"," GDICkg/ha"," TDOCkg/ha"," TDICkg/ha",	&!115
        !pdvas(112) = LatQT_DIC(j), pdvas(113) = PerQB_DIC(j),pdvas(114) = solc_doc(j),pdvas(115) = solc_dic(j) 
              "  SurTempC"," SolT50cmC","  ST100cmC","  ST150cmC",  	&!119
        !pdvas(116) = sur_tmp(j), pdvas(117) = soltmp_50(j), pdvas(118) = soltmp_100(j), pdvas(119) = soltmp_150(j) 
              "  ST200cmC","  ST300cmC","  ST350cmC"," ST1000cmC",	&!123
        !pdvas(120) = soltmp_200(j) ,pdvas(121) = soltmp_300(j),pdvas(122) = soltmp_500(j),pdvas(123) = soltmp_1000(j)
              "  FrozeDay"," RPONkg/ha"," LPONkg/ha"," SDONkg/ha",   	&!127    
        !pdvas(124) = sol_frozday(j),pdvas(125) = Sed_RPON(j),pdvas(126) = Sed_LPON(j),pdvas(127) = SurQ_DON(j)            
              " LDONkg/ha"," PDONkg/ha"," rcDONkg/h"," GDONkg/ha", 	&!131
        !pdvas(128) = LatQT_DON(j),pdvas(129) = PerQ_DON (sol_nly(j),j), pdvas(130) = rchrg_don (j),pdvas(131) = GwQ_DON (j)   
              " dDONkg/ha"," rvDONkg/h"," gpDONkg/h"," shaDONkgh", 	&!135
        !pdvas(132) = shallst_don_decay(j) ,pdvas(133) = revap_don(j),pdvas(134) = gwseep_don(j),pdvas(135) = shallst_don(j)       
              " rcDOCkg/h"," dcDOCkg/h"," rvDOCkg/h"," gpDOCkg/h", 	&!139
        !pdvas(136) = rchrg_doc(j),pdvas(137) = shallst_doc_decay(j), pdvas(138) = revap_doc(j),pdvas(139) = gwseep_doc(j)        
              " shaDOCkgh"," GDICkg/ha"," rcDICkg/h"," rvDICkg/h",	&!143
        !pdvas(140) = shallst_doc(j), pdvas(141) = GwQ_DIC(j), pdvas(142) = rchrg_dic(j), pdvas(143) = revap_dic(j)         
              " gpDICkg/h"," shaDICkgh","  SOC1t/ha","  SOC2t/ha",	&!147
        !pdvas(144) = gwseep_dic(j), pdvas(145) = shallst_dic(j), pdvas(146) = sol_soc(1,j), pdvas(147) = sol_soc(2,j)
              "  SOC3t/ha","  SOC4t/ha","  SOC5t/ha","  SOC6t/ha", 	&!151
        !pdvas(148) = sol_soc(3,j), pdvas(149) = sol_soc(4,j), pdvas(150) = sol_soc(5,j), pdvas(151) = sol_soc(6,j)
              "  SOC7t/ha", "  SOCt/ha8" /)     					 !153
        !pdvas(152) = sol_soc(7,j), pdvas(153) = sol_soc(8,j)
           


!!    numbers printed to VB interface HRU output file 
      icols = (/43,53,63,73,83,93,103,113,123,133,143,153,  &            
     163,173,183,193,203,213,223,233,243,253,263,273,283,   &           
     293,303,313,323,333,343,353,363,373,383,393,403,413,   &           
     423,433,443,453,463,473,483,493,503,513,523,533,543,   &           
     553,563,573,583,593,603,613,623,633,643,653,663,673,   &           
     683,693,703,713,723,733,743,753,763,773,783,793,803,   &           
     813,823/)

!!    column headers for subbasin output file
      hedb = (/"  PRECIPmm"," SNOMELTmm","     PETmm","      ETmm",   &     !4
              "      SWmm","    PERCmm","    SURQmm","    GW_Qmm",    &     !8
              "    WYLDmm","  SYLDt/ha"," ORGNkg/ha"," ORGPkg/ha",     &    !12
              "NSURQkg/ha"," SOLPkg/ha"," SEDPkg/ha"," LAT Q(mm)",   &      !16
              "LATNO3kg/h","GWNO3kg/ha","CHOLAmic/L","CBODU mg/L",     &    !20
!    &        " DOXQ mg/L","   QTILEmm"," TNO3kg/ha"," TVAPkg/ha"/)
              " DOXQ mg/L"," TNO3kg/ha","   QTILEmm"," TVAPkg/ha",&         !24
         "   N2Og/ha","    NOg/ha"," VNH4kg/ha","    CH4g/ha",&             !28
           "DNITNkg/ha", " NITNkg/ha","PERCNkg/ha","UPNO3kg/ha",&           !32
          "FRNO3kg/ha","FRNH4kg/ha","FRORNkg/ha","RNNO3kg/ha",  &           !36
         " FIXNkg/ha", "S_NO3kg/ha", "S_NH3kg/ha","  S_ONt/ha",&            !40
          "  S_OPt/ha"," SedCkg/ha", "SurfCkg/ha", " LatCkg/ha",&           !44
          "PercCkg/ha", " NPPCkg/ha", " RspCkg/ha", " SnoFallmm",&          !48
         "SnoDepthmm", "SnoWatermm", "  SurTempC", " SolT50cmC", &          !52
         "SolT100cmC", "SolT150cmC", "SolT200cmC", "SolT300cmC",&           !56
         "SolT350cmC", "SoT1000cmC", "  FrozeDay", "  AirTempC",&           !60
         "       SWC","Sol_TotalC","Sol_TotaSC", "wtmp_SurQ ",&             !64
         "wtmp_LatQ ",  " wtmp_GwQ "                  /)                    !66

!!    numbers printed to VB interface subbasin output file 
      icolb = (/35,45,55,65,75,85,95,105,115,125,135,145,   &            
     155,165,175,185,195,205,215,225,235,245,255,265/)
      
!!  added headers TOTAL N/TOTALP/NO3 Concentration TO HEADING FOR OUTPUT.RCH GSM 10/26/2011
!!    column headers for reach output file
      !output.rch
      hedr = (/"  FLOW_INcms"," FLOW_OUTcms","     EVAPcms",         &          !3
              "    TLOSScms","  SED_INtons"," SED_OUTtons",          &          !6
              " SEDCONCmg/L","   ORGN_INkg","  ORGN_OUTkg",          &          !9
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
              "  CMETAL#1kg","  CMETAL#2kg","  CMETAL#3kg",          &          !42
              "     TOT_Nkg","     TOT_Pkg"," NO3ConcMg/l",          &          !45
              "    WTMPdegc",						&                           !46
          "   RPOC_INkg","  RPOC_OUTkg", "   LPOC_INkg","  LPOC_OUTkg",&        !50
          "   RDOC_INkg","  RDOC_OUTkg", "   LDOC_INkg","  LDOC_OUTkg",&        !54
          "    DIC_INkg","   DIC_OUTkg", "  TotalPOCkg","  TotalDOCkg", &       !58
          "   TOC_OUTkg"                                                &       !59
  !   &    "    rchstor ","   rcharea  ", "  rchdep    ","  sdti       "        
         /)
!!    numbers printed to VB interface reach output file 
      icolr = (/38,50,62,74,86,98,110,122,134,146,158,170,182,194,206,  &
     218,230,242,254,266,278,290,302,314,326,338,350,362,374,386,398,  &
     410,422,434,446,458,470,482,494,506,518,530,542,554,566,578,590,  &
     602,614,626,638,650,662,674,686,698,710,722,734,746,758,770/)

!!    column headers for reservoir output file
      hedrsv = (/"    VOLUMEm3","  FLOW_INcms"," FLOW_OUTcms",         & 
                "    PRECIPm3","      EVAPm3","   SEEPAGEm3",          &
                "  SED_INtons"," SED_OUTtons"," SED_CONCppm",          &
                "   ORGN_INkg","  ORGN_OUTkg"," RES_ORGNppm",          &
                "   ORGP_INkg","  ORGP_OUTkg"," RES_ORGPppm",          &
                "    NO3_INkg","   NO3_OUTkg","  RES_NO3ppm",          &
                "    NO2_INkg","   NO2_OUTkg","  RES_NO2ppm",          &
                "    NH3_INkg","   NH3_OUTkg","  RES_NH3ppm",          &
                "   MINP_INkg","  MINP_OUTkg"," RES_MINPppm",          &
                "   CHLA_INkg","  CHLA_OUTkg","SECCHIDEPTHm",         & 
                "   PEST_INmg","  REACTPSTmg","    VOLPSTmg",         & 
                "  SETTLPSTmg","RESUSP_PSTmg","DIFFUSEPSTmg",         & 
                "REACBEDPSTmg","   BURYPSTmg","  PEST_OUTmg",         & 
                "PSTCNCWmg/m3","PSTCNCBmg/m3"/)

!!    numbers printed to VB interface reservoir output file 
      icolrsv = (/38,50,62,74,86,98,110,122,134,146,158,170,182,194,    &
     206,218,230,242,254,266,278,290,302,314,326,338,350,362,374,386,  &
     398,410,422,434,446,458,470,482,494,506,518/)

!!    column headers for HRU impoundment output file
      hedwtr = (/"  PNDPCPmm","  PND_INmm","PSED_It/ha","  PNDEVPmm",   &
                "  PNDSEPmm"," PND_OUTmm","PSED_Ot/ha"," PNDVOLm^3",   &
                "PNDORGNppm"," PNDNO3ppm","PNDORGPppm","PNDMINPppm",   &
                "PNDCHLAppm","  PNDSECIm","  WETPCPmm","  WET_INmm",   &
                "WSED_It/ha","  WETEVPmm","  WETSEPmm"," WET_OUTmm",   &
                "WSED_Ot/ha"," WETVOLm^3","WETORGNppm"," WETNO3ppm",   &
                "WETORGPppm","WETMINPppm","WETCHLAppm","  WETSECIm",   &
                "  POTPCPmm","  POT_INmm","OSED_It/ha","  POTEVPmm",   &
                "  POTSEPmm"," POT_OUTmm","OSED_Ot/ha"," POTVOLm^3",   &
                "  POT_SAha","HRU_SURQmm","PLANT_ETmm"," SOIL_ETmm"/)
          
  
                  
   
      
      return
      end                                           