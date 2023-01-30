      subroutine hrumon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)     |kg/ha         |land cover/crop biomass (dry weight)
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    hvstiadj(:)   |(kg/ha)/(kg/ha)|optimal harvest index for current time
!!                                 |during growing season
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed (output.hru)
!!    mo_chk        |none          |current month of simulation
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    rwt(:)        |none          |fraction of total plant biomass that is
!!                                 |in roots
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |on any given day
!!    hrumono(1,:)  |mm H2O        |precipitation in HRU during month
!!    hrumono(2,:)  |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow in HRU during month
!!    hrumono(3,:)  |mm H2O        |amount of snow melt in HRU during month
!!    hrumono(4,:)  |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during month (ignores impact of
!!                                 |transmission losses)
!!    hrumono(5,:)  |mm H2O        |amount of lateral flow contribution to main
!!                                 |channel from HRU during month
!!    hrumono(6,:)  |mm H2O        |amount of groundwater flow contribution to
!!                                 |main channel from HRU during month
!!    hrumono(7,:)  |mm H2O        |amount of water moving from shallow aquifer
!!                                 |to plants or soil profile in HRU during month
!!    hrumono(8,:)  |mm H2O        |amount of water recharging deep aquifer in
!!                                 |HRU during month
!!    hrumono(9,:)  |mm H2O        |total amount of water entering both aquifers
!!                                 |from HRU during month
!!    hrumono(10,:) |mm H2O        |water yield (total amount of water entering
!!                                 |main channel) from HRU during month
!!    hrumono(11,:) |mm H2O        |amount of water percolating out of the soil
!!                                 |profile and into the vadose zone in HRU
!!                                 |during month
!!    hrumono(12,:) |mm H2O        |actual evapotranspiration in HRU during month
!!    hrumono(13,:) |mm H2O        |amount of transmission losses from tributary
!!                                 |channels in HRU for month
!!    hrumono(14,:) |metric tons/ha|sediment yield from HRU for month
!!    hrumono(17,:) |kg N/ha       |amount of nitrogen applied in continuous 
!!                                 |fertilizer operation during month in HRU 
!!    hrumono(18,:) |kg P/ha       |amount of phosphorus applied in continuous
!!                                 |fertilizer operation during month in HRU
!!    hrumono(23,:) |mm H2O        |amount of water removed from shallow aquifer
!!                                 |in HRU for irrigation during month
!!    hrumono(24,:) |mm H2O        |amount of water removed from deep aquifer
!!                                 |in HRU for irrigation during month
!!    hrumono(25,:) |mm H2O        |potential evapotranspiration in HRU during
!!                                 |month
!!    hrumono(26,:) |kg N/ha       |monthly amount of N (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hrumono(27,:) |kg P/ha       |monthly amount of P (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hrumono(28,:) |kg N/ha       |monthly amount of N (organic & mineral)
!!                                 |auto-applied in HRU
!!    hrumono(29,:) |kg P/ha       |monthly amount of P (organic & mineral)
!!                                 |auto-applied in HRU
!!    hrumono(31,:) |stress days   |water stress days in HRU during month
!!    hrumono(32,:) |stress days   |temperature stress days in HRU during month
!!    hrumono(33,:) |stress days   |nitrogen stress days in HRU during month
!!    hrumono(34,:) |stress days   |phosphorus stress days in HRU during month
!!    hrumono(35,:) |kg N/ha       |organic nitrogen in surface runoff in HRU
!!                                 |during month
!!    hrumono(36,:) |kg P/ha       |organic phosphorus in surface runoff in HRU
!!                                 |during month
!!    hrumono(37,:) |kg N/ha       |nitrate in surface runoff in HRU during month
!!    hrumono(38,:) |kg N/ha       |nitrate in lateral flow in HRU during month
!!    hrumono(39,:) |kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                                 |during month
!!    hrumono(40,:) |kg N/ha       |amount of nitrogen removed from soil by plant
!!                                 |uptake in HRU during month
!!    hrumono(41,:) |kg N/ha       |nitrate percolating past bottom of soil
!!                                 |profile in HRU during month
!!    hrumono(42,:) |kg P/ha       |amount of phosphorus removed from soil by
!!                                 |plant uptake in HRU during month
!!    hrumono(43,:) |kg P/ha       |amount of phosphorus moving from labile
!!                                 |mineral to active mineral pool in HRU during
!!                                 |month
!!    hrumono(44,:) |kg P/ha       |amount of phosphorus moving from active
!!                                 |mineral to stable mineral pool in HRU during
!!                                 |month
!!    hrumono(45,:) |kg N/ha       |amount of nitrogen applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |month
!!    hrumono(46,:) |kg P/ha       |amount of phosphorus applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |month
!!    hrumono(47,:) |kg N/ha       |amount of nitrogen added to soil by fixation
!!                                 |in HRU during month
!!    hrumono(48,:) |kg N/ha       |amount of nitrogen lost by denitrification
!!                                 |in HRU during month
!!    hrumono(49,:) |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to nitrate pool in HRU during month
!!    hrumono(50,:) |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to stable organic pool in HRU during month
!!    hrumono(51,:) |kg P/ha       |amount of phosphorus moving from organic to
!!                                 |labile mineral pool in HRU during month
!!    hrumono(52,:) |kg N/ha       |amount of nitrogen moving from fresh organic
!!                                 |to nitrate and active organic pools in HRU
!!                                 |during month
!!    hrumono(53,:) |kg P/ha       |amount of phosphorus moving from fresh
!!                                 |organic to the labile mineral and organic
!!                                 |pools in HRU during month
!!    hrumono(54,:) |kg N/ha       |amount of nitrogen added to soil in rain
!!    hrumono(61,:) |metric tons/ha|daily soil loss predicted with USLE equation
!!    hrumono(63,:) |# bacteria/ha |less persistent bacteria transported to main
!!                                 |channel from HRU during month
!!    hrumono(64,:) |# bacteria/ha |persistent bacteria transported to main
!!                                 |channel from HRU during month
!!    hrumono(65,:) |kg N/ha       |nitrate loading from groundwater in HRU to
!!                                 |main channel during month
!!    hrumono(66,:) |kg P/ha       |soluble P loading from groundwater in HRU to
!!                                 |main channel during month
!!    hrumono(67,:) |kg P/ha       |loading of mineral P attached to sediment
!!                                 |in HRU to main channel during month
!!    laiday(:)     |none          |leaf area index for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    days        |none          |number of days in month
!!    dmt         |metric tons/ha|land cover/crop biomass (dry weight)
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    pdvas(:)    |varies        |array to hold HRU output values
!!    pdvs(:)     |varies        |array to hold selected HRU output values
!!                               |when user doesn't want to print all
!!    sb          |none          |subbasin number
!!    yldt        |metric tons/ha|land cover/crop yield (dry weight)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

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
        
      integer :: j, sb, ii, days, iflag, ix, idplant
      real :: dmt, yldt
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      days = 0

      select case(mo_chk)
        case (9, 4, 6, 11)
          days = 30
        case (2)
          days = 29 - leapyr
        case default
          days = 31
      end select

      do j = 1, nhru
        sb = 0
        sb = hru_sub(j)

        iflag = 0
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do

        if (iflag == 1) then

        pdvas = 0.
        pdvs = 0.

        dmt = 0.
        yldt = 0.
        dmt = bio_ms(j) / 1000.
        yldt = (1. - rwt(j)) * dmt * hvstiadj(j)

        pdvas(1) = hrumono(1,j)                                         !pdvas(1) = subp(j)      
        pdvas(2) = hrumono(2,j)                                         !pdvas(2) = snofall
        pdvas(3) = hrumono(3,j)                                         !pdvas(3) = snomlt
        pdvas(4) = hrumono(4,j)                                         !pdvas(4) = aird(j)
        pdvas(5) = hrumono(5,j)                                         !pdvas(5) = pet_day
        pdvas(6) = hrumono(6,j)                                         !pdvas(6) = etday
        pdvas(7) = hrumono(7,j) / Real(days)                            !pdvas(7) = sol_cnsw(j)  
        pdvas(8) = hrumono(8,j) / Real(days)                            !pdvas(8) = sol_sw(j)    
        pdvas(9) = hrumono(9,j)                                         !pdvas(9) = sepbtm(j)    
        pdvas(10) = hrumono(10,j)                                       !pdvas(10) = rchrg(j)    
        pdvas(11) = hrumono(11,j)                                       !pdvas(11) = gwseep      
        pdvas(12) = hrumono(12,j)                                       !pdvas(12) = revapday    
        pdvas(13) = hrumono(13,j)                                       !pdvas(13) = shallirr(j)
        
        pdvas(14) = hrumono(14,j)                                       !pdvas(14) = deepirr(j)
        pdvas(15) = hrumono(15,j) / Real(days)   !shallst(j)            !pdvas(15) = shallst(j)                    
        pdvas(16) = hrumono(16,j) / Real(days)   !deepst(j)             !pdvas(16) = deepst(j)                     
        pdvas(17) = hrumono(17,j)                                       !pdvas(17) = surfq(j)
        pdvas(18) = hrumono(18,j)                                       !pdvas(18) = qday                          
        pdvas(19) = hrumono(19,j)                                       !pdvas(19) = latq(j)                       
        pdvas(20) = hrumono(20,j)                                       !pdvas(20) = latq(j) - lpndloss - lwetloss 
        pdvas(21) = hrumono(21,j)                                       !pdvas(21) = qtile 
        pdvas(22) = hrumono(22,j)                                       !pdvas(22) = gw_q(j)                       
                                                                        
        pdvas(23) = hrumono(23,j)                                       !pdvas(23) = gw_qdeep(j)                  
        pdvas(24) = hrumono(24,j)                                       !pdvas(24) = tloss                         
        pdvas(25) = hrumono(25,j)                                       !pdvas(25) = qdr(j)      
        
        pdvas(26) = hrumono(26,j) / Real(days)                          !pdvas(26) = cnday(j)
        pdvas(27) = hrumono(27,j) / Real(days)                          !pdvas(27) = tmpav(j)
        pdvas(28) = hrumono(28,j) / Real(days)                          !pdvas(28) = tmx(j)
        pdvas(29) = hrumono(29,j) / Real(days)                          !pdvas(29) = tmn(j)
        pdvas(30) = hrumono(30,j) / Real(days)                          !pdvas(30) = hru_ra(j) 
        
        pdvas(31) = hrumono(31,j)                                       !pdvas(31) = sedyld(j) / hru_ha(j)
        pdvas(32) = hrumono(32,j)                                       !pdvas(32) = sedgen(j)/ hru_ha(j)
        pdvas(33) = hrumono(33,j)                                       !pdvas(33) = sedorgn(j)                   
        pdvas(34) = hrumono(34,j)                                       !pdvas(34) = sedorgp(j)                   
        pdvas(35) = hrumono(35,j)                                       !pdvas(35) = sedminpa(j) + sedminps(j)    
        pdvas(36) = hrumono(36,j)                                       !pdvas(36) = fertn
        pdvas(37) = hrumono(37,j)                                       !pdvas(37) = fertp
        pdvas(38) = hrumono(38,j)                                       !pdvas(38) = auton
        pdvas(39) = hrumono(39,j)                                       !pdvas(39) = autop
        pdvas(40) = hrumono(40,j)                                       !pdvas(40) = grazn
                                                                        
        pdvas(41) = hrumono(41,j)                                       !pdvas(41) = grazp
        pdvas(42) = hrumono(42,j)                                       !pdvas(42) = cfertn
        pdvas(43) = hrumono(43,j)                                       !pdvas(43) = cfertp
        pdvas(44) = hrumono(44,j)                                       !pdvas(44) = no3pcp
        pdvas(45) = hrumono(45,j)                                       !pdvas(45) = fixn
        pdvas(46) = hrumono(46,j)                                       !pdvas(46) = nplnt(j) 
        pdvas(47) = hrumono(47,j)                                       !pdvas(47) = pplnt(j) 
        pdvas(48) = hrumono(48,j)                                       !pdvas(48) = wdntl
        
        pdvas(49) = hrumono(49,j)                                       !pdvas(49) = surqno3(j)   
        pdvas(50) = hrumono(50,j)                                       !pdvas(50) = latno3(j)    
        pdvas(51) = hrumono(51,j)                                       !pdvas(51) = tileno3(j)   
        pdvas(52) = hrumono(52,j)                                       !pdvas(52) = no3gw(j)     
        pdvas(53) = hrumono(53,j)                                       !pdvas(53) = percn(j)     
        pdvas(54) = hrumono(54,j)                                       !pdvas(54) = rchrg_n(j)    
        pdvas(55) = hrumono(55,j)                                       !pdvas(55) = revapn(j)     
        pdvas(56) = hrumono(56,j)                                       !pdvas(56) = gwseepn(j)    
        pdvas(57) = hrumono(57,j)                                       !pdvas(57) = gw_no3loss(j) 
        pdvas(58) = hrumono(58,j) / Real(days)                          !pdvas(58) = shallst_n(j)  
        pdvas(59) = hrumono(59,j)                                       !pdvas(59) = surqsolp(j)   
                                                                              
        pdvas(60) = hrumono(60,j)                                       !pdvas(60) = vap_tile      
        pdvas(61) = hrumono(61,j)                                       !pdvas(61) = minpgw(j)
        pdvas(62) = hrumono(62,j)                                       !pdvas(62) = (1.-strsw(j))     
        pdvas(63) = hrumono(63,j)                                       !pdvas(63) = (1.-strstmp(j))   
                                                                          
        pdvas(64) = hrumono(64,j)                                      !pdvas(64) = (1.-strsn(j))     
        pdvas(65) = hrumono(65,j)                                      !pdvas(65) = (1.-strsp(j))   . 
        pdvas(66) = hrumono(66,j)                                       !pdvas(66) = bio_ms(j) / 1000  
        pdvas(67) = hrumono(67,j)/ Real(days)                           !pdvas(67) = laiday(j)         
        pdvas(68) = hrumono(68,j)                                       !pdvas(68) = yield/1000.     
        pdvas(69) = hrumono(69,j)                                       !pdvas(69) = yieldgrn/1000.
        pdvas(70) = hrumono(70,j)                                       !pdvas(70) = yieldbms/1000.
        pdvas(71) = hrumono(71,j)                                       !pdvas(71) = yieldtbr/1000.
        pdvas(72) = hrumono(72,j)                                       !pdvas(72) = yieldrsd/1000.                                                                

        pdvas(73) = hrumono(73,j)                                       !pdvas(73) = bactrop + bactsedp     
        pdvas(74) = hrumono(74,j)                                       !pdvas(74) = bactrolp + bactsedlp   
        pdvas(75) = hrumono(75,j)/ Real(days)                                       !pdvas(75) = wtab(j)                
        pdvas(76) = hrumono(76,j)/ Real(days)                                       !pdvas(76) = wat_tbl(j)             
        pdvas(77) = hrumono(77,j)/ Real(days)                                       !pdvas(77) = sno_hru(j)             
        pdvas(78) = hrumono(78,j)/ Real(days)                                       !pdvas(78) = sol_rsd(1,j)/1000.     
        pdvas(79) = hrumono(79,j)/ Real(days)                                       !pdvas(79) = sol_cov(j)/1000.       
        

        pdvas(80) = hrumono(80,j)               !pdvas(80) = N2O(j)*1000.                                           
        pdvas(81) = hrumono(81,j)               !pdvas(81) = NO(j)*1000.                                          
        pdvas(82) = hrumono(82,j)               !pdvas(82) = nh4_vol(j)                                                
        pdvas(83) = hrumono(83,j)               !pdvas(83) = no3_denit(j)                                         
        pdvas(84) = hrumono(84,j)               !pdvas(84) = no3_nitr(j)                                            
        pdvas(85) = hrumono(85,j)               !pdvas(85) = no3_up(j)                                            
        pdvas(86) = hrumono(86,j)               !pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j) 
        pdvas(87) = hrumono(87,j)               !pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)
        pdvas(88) = hrumono(88,j)               !pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j)        
        

        pdvas(89) = hrumono(89,j)/ Real(days)  !pdvas(89) = solc_no3(j)             !solc_no3(j)                                     !! kg N/ha 
        pdvas(90) = hrumono(90,j)/ Real(days)  !pdvas(90) = solc_nh4(j)             !solc_nh4(j)                                      !! kg N/ha 
        pdvas(91) = hrumono(91,j)/ Real(days)  !pdvas(91) = solc_orgn(j)/1000.      !solc_orgn(j)/1000.                           !! t N/ha 
        pdvas(92) = hrumono(92,j)/ Real(days)  !pdvas(92) = solc_orgp(j)/1000.      !solc_orgp(j)/1000                            !! t P/ha 
        pdvas(93) = hrumono(93,j)/ Real(days)  !pdvas(93) = solc_orgc(j)/1000.      !solc_orgc(j)                                     !! t C/ha 
        pdvas(94) = hrumono(94,j)/ Real(days)  !pdvas(94) = solc_orgcs(j)/1000.     !solc_orgcs(j)                                   !! t C/ha
        
        pdvas(95) = hrumono(95,j)       !pdvas(95) = sedc_d(j)                           !! kg C/ha
        pdvas(96) = hrumono(96,j)       !pdvas(96) = surfqc_d(j)                         !! kg C/ha
        pdvas(97) = hrumono(97,j)       !pdvas(97) = latc_d(j)                             !! kg C/ha
        pdvas(98) = hrumono(98,j)       !pdvas(98) = percc_d(j)                          !! kg C/ha
        pdvas(99) = hrumono(99,j)       !pdvas(99) = NPPC_d(j)                        !! kg C/ha
        pdvas(100) = hrumono(100,j)     !pdvas(100) = rspc_dnew(j)                        !! kg C/ha  
        pdvas(101) = hrumono(101,j)     !pdvas(101) = OrgC_Plt2Rsd(j)+ OrgC_Fer(j) 
       
        pdvas(102)= hrumono(102,j)           !pdvas(102) = Sed_RPOC(j)      ! RPOC from hru to stream(kg/ha/day) !sol_BMC(1,j)/1000.            !            
        pdvas(103) = hrumono(103,j)          !pdvas(103) = Sed_LPOC(j)      ! LPOC from hru to stream (kg/ha/day)    !sol_BMC(2,j) /1000.           !           
        pdvas(104) = hrumono(104,j)          !pdvas(104) = HRU_RDOC(j)      ! RDOC from hru to stream (kg/ha/day)    !sol_BMC(3,j) /1000.           !           
        pdvas(105) = hrumono(105,j)          !pdvas(105) = HRU_LDOC(j)      ! LDOC from hru to stream (kg/ha/day)    !sol_HSC(1,j) /1000.           !           
        pdvas(106) = hrumono(106,j)          !pdvas(106) = HRU_DIC(j)       ! DIC from hru to stream(kg/ha/day)  !sol_HSC(2,j)/1000.            !              
        pdvas(107) = hrumono(107,j)          !pdvas(107) = SurQ_DOC(j)      ! Surface RDOC to stream  (kg/ha/day)    !sol_HSC(3,j) /1000.           !           
        pdvas(108) = hrumono(108,j)          !pdvas(108) = LatQT_DOC(j)     ! Lateral RDOCto stream  (kg/ha/day) !sol_HPC(1,j) /1000.           !           
        pdvas(109) = hrumono(109,j)          !pdvas(109) = PerQB_DOC(j)     ! RDOC percolation amount from lowest soil layer to shallow aquifer  !sol_HPC(2,j)/1000.            
        pdvas(110) = hrumono(110,j)          !pdvas(110) = GwQ_DOC(j)       ! GW RDOC to stream (kg/ha)  !sol_HPC(3,j) /1000.           !           
        
        pdvas(111) = hrumono(111,j)                     !pdvas(111) = SurQ_DIC(j)                              ! Surface DIC from HRUto stream (kg/ha)                   
        pdvas(112) = hrumono(112,j)                     !pdvas(112) = LatQT_DIC(j)                            ! Lateral DIC from HRU to stream(kg/ha)                 
        pdvas(113) = hrumono(113,j)                     !pdvas(113) = PerQB_DIC(j)                            ! DIC percolation amount from lowest soil layer to shallow aquifer           
        pdvas(114) = hrumono(114,j)/ Real(days)         !pdvas(114) = solc_doc(j)                             !  Total DOC in soil profile (kg/ha)                                        
        pdvas(115) = hrumono(115,j)/ Real(days)         !pdvas(115) = solc_dic(j)                            !  Total DIC in soil profile (kg/ha)    
                       
 
        pdvas(116) = hrumono(116,j)/ Real(days)     !pdvas(116) = sur_tmp(j)     
        pdvas(117) = hrumono(117,j)/ Real(days)     !pdvas(117) = soltmp_50(j)   
        pdvas(118) = hrumono(118,j)/ Real(days)     !pdvas(118) = soltmp_100(j)  
        pdvas(119) = hrumono(119,j)/ Real(days)     !pdvas(119) = soltmp_150(j)  
        pdvas(120) = hrumono(120,j)/ Real(days)     !pdvas(120) = soltmp_200(j)  
        
        
        pdvas(121) = hrumono(121,j)/ Real(days)                 !pdvas(121) = soltmp_300(j)  
        pdvas(122) = hrumono(122,j)/ Real(days)                 !pdvas(122) = soltmp_500(j)  
        pdvas(123) = hrumono(123,j)/ Real(days)                 !pdvas(123) = soltmp_1000(j) 
        pdvas(124) = hrumono(124,j)                             !pdvas(124) = sol_frozday(j) 

        pdvas(125) = hrumono(125,j)                 !pdvas(125) = Sed_RPON(j)                      
        pdvas(126) = hrumono(126,j)                 !pdvas(126) = Sed_LPON(j)             
        pdvas(127) = hrumono(127,j)                 !pdvas(127) = SurQ_DON(j)             
        pdvas(128) = hrumono(128,j)                 !pdvas(128) = LatQT_DON(j)            
        pdvas(129) = hrumono(129,j)                 !pdvas(129) = PerQ_DON (sol_nly(j),j) 
        pdvas(130) = hrumono(130,j)                 !pdvas(130) = rchrg_don (j)           
        
        
        
        pdvas(131) = hrumono(131,j)                 !pdvas(131) = GwQ_DON (j)            
        pdvas(132) = hrumono(132,j)                 !pdvas(132) = shallst_don_decay(j)   
        pdvas(133) = hrumono(133,j)                 !pdvas(133) = revap_don(j)           
        pdvas(134) = hrumono(134,j)                 !pdvas(134) = gwseep_don(j)          
        pdvas(135) = hrumono(135,j)/ Real(days)     !pdvas(135) = shallst_don(j)             
         
        pdvas(136) = hrumono(136,j)                 !pdvas(136) = rchrg_doc(j)            
        pdvas(137) = hrumono(137,j)                 !pdvas(137) = shallst_doc_decay(j)    
        pdvas(138) = hrumono(138,j)                 !pdvas(138) = revap_doc(j)            
        pdvas(139) = hrumono(139,j)                 !pdvas(139) = gwseep_doc(j)         
        pdvas(140) = hrumono(140,j)/ Real(days)     !pdvas(140) = shallst_doc(j)   
        pdvas(141) = hrumono(141,j)                 !pdvas(141) = GwQ_DIC(j)     
        pdvas(142) = hrumono(142,j)                 !pdvas(142) = rchrg_dic(j)   
        pdvas(143) = hrumono(143,j)                 !pdvas(143) = revap_dic(j)   
        pdvas(144) = hrumono(144,j)                 !pdvas(144) = gwseep_dic(j)  
                                                    
        pdvas(145) = hrumono(145,j)/ Real(days)     !pdvas(145) = shallst_dic(j) 


      pdvas(146) = hrumono(146,j)/ Real(days)                   !pdvas(146) = sol_soc(1,j)   
      pdvas(147) = hrumono(147,j)/ Real(days)                   !pdvas(147) = sol_soc(2,j)   
      pdvas(148) = hrumono(148,j)/ Real(days)                   !pdvas(148) = sol_soc(3,j)   
      pdvas(149) = hrumono(149,j)/ Real(days)                   !pdvas(149) = sol_soc(4,j)   
      pdvas(150) = hrumono(150,j)/ Real(days)                   !pdvas(150) = sol_soc(5,j)   
      pdvas(151) = hrumono(151,j)/ Real(days)                   !pdvas(151) = sol_soc(6,j)   
      pdvas(152) = hrumono(152,j)/ Real(days)                   !pdvas(152) = sol_soc(7,j)   
      pdvas(153) = hrumono(153,j)/ Real(days)                   !pdvas(153) = sol_soc(8,j)   
        

      if (itots > 0) then 
	   ix = itots
	else
         ix = mhruo
	endif


        if (ipdvas(1) > 0) then
          do ii = 1, ix
            pdvs(ii) = pdvas(ipdvas(ii))
          end do
 
          idplant = idplt(j)
          if (idplant > 0) then
            cropname = cpnm(idplant)
          else
            cropname = "NOCR"
          endif

          if (iscen == 1) then                  !main.f90(163):      do iscen = 1, scenario                             
            select case (isproj)                !zeroini.f90(82):      isproj = 0
            case (0)
                if(ioutput == 1) then
                  !!output basic information
                  call sqlite3_set_column( colhru(1), j )
                  call sqlite3_set_column( colhru(2), cropname )
                  call sqlite3_set_column( colhru(3), nmgt(j) )
                  call sqlite3_set_column( colhru(4), iyr )
                  call sqlite3_set_column( colhru(5), mo_chk )
                  do ii = 1, ix
                    call sqlite3_set_column( colhru(5 + ii), pdvs(ii))
                  end do
                  call sqlite3_insert_stmt( db, stmthru, colhru )
                else 
            write (output_hru_num,1000) cropname, j, subnum(j), hruno(j), sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
 1000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,66f10.3,e10.3,e10.3,8f10.3,3f10.3,66f10.3,8f10.3)  
                end if
      !      case (1)
        !    write (21,1000) cropname, j, subnum(j), hruno(j),           
    ! &         sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
    !        case (2)
      !       write (output_hru_num,2000) cropname, j, subnum(j), hruno(j), sb,       
    ! &         nmgt(j), mo_chk, hru_km(j),(pdvs(ii), ii = 1, ix), iyr
           end select
          end if
      else
!! write with different format for hrus greater than 9999
      select case (isproj)
           case (0)
            write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
  1001 format (a4,i7,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,3e10.3,3f10.3,1x,i4,66f10.3)   

    !        case (1) 
!            write (21,1001) cropname, j, subnum(j), hruno(j),           &
!     &         sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
!            case(2) 
!            write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,       &
!     &         nmgt(j), mo_chk, hru_km(j),(pdvs(ii), ii = 1, ix), iyr
        end select
          end if

        end if
      end do

      return
 2000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
      end