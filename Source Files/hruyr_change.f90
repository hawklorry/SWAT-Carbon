      subroutine hruyr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_yrms(:)   |metric tons/ha|annual biomass (dry weight) in the HRU
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    hruyro(1,:)   |mm H2O        |precipitation in HRU during year
!!    hruyro(2,:)   |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow in HRU during year
!!    hruyro(3,:)   |mm H2O        |amount of snow melt in HRU during year
!!    hruyro(4,:)   |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during year (ignores impact of
!!                                 |transmission losses)
!!    hruyro(5,:)   |mm H2O        |amount of lateral flow contribution to main
!!                                 |channel from HRU during year
!!    hruyro(6,:)   |mm H2O        |amount of groundwater flow contribution to
!!                                 |main channel from HRU during year
!!    hruyro(7,:)   |mm H2O        |amount of water moving from shallow aquifer
!!                                 |to plants or soil profile in HRU during year
!!    hruyro(8,:)   |mm H2O        |amount of water recharging deep aquifer in
!!                                 |HRU during year
!!    hruyro(9,:)   |mm H2O        |total amount of water entering both aquifers
!!                                 |from HRU during year
!!    hruyro(10,:)  |mm H2O        |water yield (total amount of water entering
!!                                 |main channel) from HRU during year
!!    hruyro(11,:)  |mm H2O        |amount of water percolating out of the soil
!!                                 |profile and into the vadose zone in HRU
!!                                 |during year
!!    hruyro(12,:)  |mm H2O        |actual evapotranspiration in HRU during year
!!    hruyro(13,:)  |mm H2O        |amount of transmission losses from tributary
!!                                 |channels in HRU for year
!!    hruyro(14,:)  |metric tons/ha|sediment yield from HRU for year
!!    hruyro(17,:)  |kg N/ha       |amount of nitrogen applied in continuous 
!!                                 |fertilizer operation during year in HRU 
!!    hruyro(18,:)  |kg P/ha       |amount of phosphorus applied in continuous
!!                                 |fertilizer operation during year in HRU
!!    hruyro(23,:)  |mm H2O        |amount of water removed from shallow aquifer
!!                                 |in HRU for irrigation during year
!!    hruyro(24,:)  |mm H2O        |amount of water removed from deep aquifer
!!                                 |in HRU for irrigation during year
!!    hruyro(25,:)  |mm H2O        |potential evapotranspiration in HRU during
!!                                 |year
!!    hruyro(26,:)  |kg N/ha       |annual amount of N (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruyro(27,:)  |kg P/ha       |annual amount of P (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruyro(28,:)  |kg N/ha       |annual amount of N (organic & mineral)
!!                                 |auto-applied in HRU
!!    hruyro(29,:)  |kg P/ha       |annual amount of P (organic & mineral)
!!                                 |auto-applied in HRU
!!    hruyro(31,:)  |stress days   |water stress days in HRU during year
!!    hruyro(32,:)  |stress days   |temperature stress days in HRU during year
!!    hruyro(33,:)  |stress days   |nitrogen stress days in HRU during year
!!    hruyro(34,:)  |stress days   |phosphorus stress days in HRU during year
!!    hruyro(35,:)  |kg N/ha       |organic nitrogen in surface runoff in HRU
!!                                 |during year
!!    hruyro(36,:)  |kg P/ha       |organic phosphorus in surface runoff in HRU
!!                                 |during year
!!    hruyro(37,:)  |kg N/ha       |nitrate in surface runoff in HRU during year
!!    hruyro(38,:)  |kg N/ha       |nitrate in lateral flow in HRU during year
!!    hruyro(39,:)  |kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                                 |during year
!!    hruyro(40,:)  |kg N/ha       |amount of nitrogen removed from soil by plant
!!                                 |uptake in HRU during year
!!    hruyro(41,:)  |kg N/ha       |nitrate percolating past bottom of soil
!!                                 |profile in HRU during year
!!    hruyro(42,:)  |kg P/ha       |amount of phosphorus removed from soil by
!!                                 |plant uptake in HRU during year
!!    hruyro(43,:)  |kg P/ha       |amount of phosphorus moving from labile
!!                                 |mineral to active mineral pool in HRU during
!!                                 |year
!!    hruyro(44,:)  |kg P/ha       |amount of phosphorus moving from active
!!                                 |mineral to stable mineral pool in HRU during
!!                                 |year
!!    hruyro(45,:)  |kg N/ha       |amount of nitrogen applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |year
!!    hruyro(46,:)  |kg P/ha       |amount of phosphorus applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |year
!!    hruyro(47,:)  |kg N/ha       |amount of nitrogen added to soil by fixation
!!                                 |in HRU during year
!!    hruyro(48,:)  |kg N/ha       |amount of nitrogen lost by denitrification
!!                                 |in HRU during year
!!    hruyro(49,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to nitrate pool in HRU during year
!!    hruyro(50,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to stable organic pool in HRU during year
!!    hruyro(51,:)  |kg P/ha       |amount of phosphorus moving from organic to
!!                                 |labile mineral pool in HRU during year
!!    hruyro(52,:)  |kg N/ha       |amount of nitrogen moving from fresh organic
!!                                 |to nitrate and active organic pools in HRU
!!                                 |during year
!!    hruyro(53,:)  |kg P/ha       |amount of phosphorus moving from fresh
!!                                 |organic to the labile mineral and organic
!!                                 |pools in HRU during year
!!    hruyro(54,:)  |kg N/ha       |amount of nitrogen added to soil in rain
!!                                 |during year
!!    hruyro(61,:)  |metric tons/ha|daily soil loss predicted with USLE equation
!!    hruyro(63,:)  |# bacteria/ha |less persistent bacteria transported to main
!!                                 |channel from HRU during year
!!    hruyro(64,:)  |# bacteria/ha |persistent bacteria transported to main
!!                                 |channel from HRU during year
!!    hruyro(65,:)  |kg N/ha       |nitrate loading from groundwater in HRU to
!!                                 |main channel during year
!!    hruyro(66,:)  |kg P/ha       |soluble P loading from groundwater in HRU to
!!                                 |main channel during year
!!    hruyro(67,:)  |kg P/ha       |loading of mineral P attached to sediment
!!                                 |in HRU to main channel during year
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed (output.hru)
!!    iyr           |year          |current year of simulation (eg 1980)
!!    lai_yrmx(:)   |none          |maximum leaf area index for the year in the
!!                                 |HRU
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |on any given day
!!    yldanu(:)     |metric tons/ha|annual yield (dry weight) in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    pdvas(:)    |varies        |array to hold HRU output values
!!    pdvs(:)     |varies        |array to hold selected HRU output values
!!                               |when user doesn't want to print all
!!    sb          |none          |subbasin number
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
        
      integer :: j, sb, ii, iflag, idplant
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      do j = 1, nhru
        iflag = 0
	    sb = hru_sub(j)
	  
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do

        if (iflag == 1) then

        pdvas = 0.
        pdvs = 0.

        pdvas(1) = hruyro(1,j)                                              !pdvas(1) = subp(j)      
        pdvas(2) = hruyro(2,j)                                              !pdvas(2) = snofall
        pdvas(3) = hruyro(3,j)                                              !pdvas(3) = snomlt
        pdvas(4) = hruyro(4,j)                                              !pdvas(4) = aird(j)
        pdvas(5) = hruyro(5,j)                                              !pdvas(5) = pet_day
        pdvas(6) = hruyro(6,j)                                              !pdvas(6) = etday
        pdvas(7) = hruyro(7,j) / Real(366 - leapyr)                         !pdvas(7) = sol_cnsw(j)  
        pdvas(8) = hruyro(8,j)                                              !pdvas(8) = sol_sw(j)    
        pdvas(9) = hruyro(9,j)                                              !pdvas(9) = sepbtm(j)    
        pdvas(10) = hruyro(10,j)                                            !pdvas(10) = rchrg(j)    
        pdvas(11) = hruyro(11,j)                                            !pdvas(11) = gwseep      
        pdvas(12) = hruyro(12,j)                                            !pdvas(12) = revapday    
        pdvas(13) = hruyro(13,j)                                            !pdvas(13) = shallirr(j)
        
        pdvas(14) = hruyro(14,j)                                            !pdvas(14) = deepirr(j)
        pdvas(15) = hruyro(15,j)/ Real(366 - leapyr)                                            !pdvas(15) = shallst(j)                    
        pdvas(16) = hruyro(16,j)/ Real(366 - leapyr)                                            !pdvas(16) = deepst(j)                     
        pdvas(17) = hruyro(17,j)                                            !pdvas(17) = surfq(j)
        pdvas(18) = hruyro(18,j)                                            !pdvas(18) = qday                          
        pdvas(19) = hruyro(19,j)                                            !pdvas(19) = latq(j)                       
        pdvas(20) = hruyro(20,j)                                            !pdvas(20) = latq(j) - lpndloss - lwetloss 
        pdvas(21) = hruyro(21,j)                                            !pdvas(21) = qtile 
        pdvas(22) = hruyro(22,j)                                            !pdvas(22) = gw_q(j)                       
        
        pdvas(23) = hruyro(23,j)                                            !pdvas(23) = gw_qdeep(j) 
        pdvas(24) = hruyro(24,j)                                            !pdvas(24) = tloss       
        pdvas(25) = hruyro(25,j)                                            !pdvas(25) = qdr(j)      
        
        pdvas(26) = hruyro(26,j)/ Real(366 - leapyr)                                             !pdvas(26) = cnday(j)
        pdvas(27) = hruyro(27,j)/ Real(366 - leapyr)                                             !pdvas(27) = tmpav(j)
        pdvas(28) = hruyro(28,j)/ Real(366 - leapyr)                                             !pdvas(28) = tmx(j)
        pdvas(29) = hruyro(29,j)/ Real(366 - leapyr)                                             !pdvas(29) = tmn(j)
        pdvas(30) = hruyro(30,j)/ Real(366 - leapyr)                                             !pdvas(30) = hru_ra(j) 
        
        pdvas(31) = hruyro(31,j)                                            !pdvas(31) = sedyld(j) / hru_ha(j)
        pdvas(32) = hruyro(32,j)                                            !pdvas(32) = sedgen(j)/ hru_ha(j)
        pdvas(33) = hruyro(33,j)                                            !pdvas(33) = sedorgn(j)                 
        pdvas(34) = hruyro(34,j)                                            !pdvas(34) = sedorgp(j)                 
        pdvas(35) = hruyro(35,j)                                            !pdvas(35) = sedminpa(j) + sedminps(j)  
        pdvas(36) = hruyro(26,j)                                            !pdvas(36) = fertn
        pdvas(37) = hruyro(37,j)                                            !pdvas(37) = fertp
        pdvas(38) = hruyro(38,j)                                            !pdvas(38) = auton
        pdvas(39) = hruyro(39,j)                                            !pdvas(39) = autop
        pdvas(40) = hruyro(40,j)                                            !pdvas(40) = grazn
        
        pdvas(41) = hruyro(41,j)                                            !pdvas(41) = grazp
        pdvas(42) = hruyro(42,j)                                            !pdvas(42) = cfertn
        pdvas(43) = hruyro(43,j)                                            !pdvas(43) = cfertp
        pdvas(44) = hruyro(44,j)                                            !pdvas(44) = no3pcp
        pdvas(45) = hruyro(45,j)                                            !pdvas(45) = fixn
        pdvas(46) = hruyro(46,j)                                            !pdvas(46) = nplnt(j) 
        pdvas(47) = hruyro(47,j)                                            !pdvas(47) = pplnt(j) 
        pdvas(48) = hruyro(48,j)                                            !pdvas(48) = wdntl
        
        pdvas(49) = hruyro(49,j)                                            !pdvas(49) = surqno3(j)   
        pdvas(50) = hruyro(50,j)                                            !pdvas(50) = latno3(j)    
        pdvas(51) = hruyro(51,j)                                            !pdvas(51) = tileno3(j)   
        pdvas(52) = hruyro(52,j)                                            !pdvas(52) = no3gw(j)     
        pdvas(53) = hruyro(53,j)                                            !pdvas(53) = percn(j)     
        pdvas(54) = hruyro(54,j)                                            !pdvas(54) = rchrg_n(j)    
        pdvas(55) = hruyro(55,j)                                            !pdvas(55) = revapn(j)     
        pdvas(56) = hruyro(56,j)                                            !pdvas(56) = gwseepn(j)    
        pdvas(57) = hruyro(57,j)                                            !pdvas(57) = gw_no3loss(j) 
        pdvas(58) = hruyro(58,j)/ Real(366 - leapyr)                                             !pdvas(58) = shallst_n(j)  
        pdvas(59) = hruyro(59,j)                                            !pdvas(59) = surqsolp(j)   
        
        pdvas(60) = hruyro(60,j)                                            !pdvas(60) = vap_tile      
        pdvas(61) = hruyro(61,j)                                            !pdvas(61) = minpgw(j)
        pdvas(62) = hruyro(62,j)                                            !pdvas(62) = (1.-strsw(j))  
        pdvas(63) = hruyro(63,j)                                            !pdvas(63) = (1.-strstmp(j))
        
        pdvas(64) = hruyro(64,j)                                            !pdvas(64) = (1.-strsn(j))     
        pdvas(65) = hruyro(65,j)                                            !pdvas(65) = (1.-strsp(j))   . 
        pdvas(66) = hruyro(66,j)                                             !pdvas(66) = bio_ms(j) / 1000  
        pdvas(67) = hruyro(67,j)/ Real(366 - leapyr)                                              !pdvas(67) = laiday(j)         
        pdvas(68) = hruyro(68,j)                                             !pdvas(68) = yield/1000.  
        !pdvas(68) = yldanu(j)
          
        pdvas(69) = hruyro(69,j)                                             !pdvas(69) = yieldgrn/1000.
        pdvas(70) = hruyro(70,j)                                             !pdvas(70) = yieldbms/1000.
        pdvas(71) = hruyro(71,j)                                             !pdvas(71) = yieldtbr/1000.
        pdvas(72) = hruyro(72,j)                                             !pdvas(72) = yieldrsd/1000.     
           
        pdvas(73) = hruyro(73,j)                                            !pdvas(73) = bactrop + bactsedp     
        pdvas(74) = hruyro(74,j)                                            !pdvas(74) = bactrolp + bactsedlp   
        pdvas(75) = hruyro(75,j)/ Real(366 - leapyr)                                !pdvas(75) = wtab(j)            
        pdvas(76) = hruyro(76,j)/ Real(366 - leapyr)                                !pdvas(76) = wat_tbl(j)         
        pdvas(77) = hruyro(77,j)/ Real(366 - leapyr)                                !pdvas(77) = sno_hru(j)         
        pdvas(78) = hruyro(78,j)/ Real(366 - leapyr)                                !pdvas(78) = sol_rsd(1,j)/1000. 
        pdvas(79) = hruyro(79,j)/ Real(366 - leapyr)                                !pdvas(79) = sol_cov(j)/1000.   

!!------------------------------------------------------------------------------------
!       hruyro(73,j)=hruyro(73,j) / Real(366 - leapyr)
!       hruyro(74,j)=hruyro(74,j) / Real(366 - leapyr)
!       hruyro(75,j)=hruyro(75,j) / Real(366 - leapyr)
!       hruyro(76,j)=hruyro(76,j) / Real(366 - leapyr)
!       hruyro(77,j)=hruyro(77,j) / Real(366 - leapyr)
!       hruyro(78,j)=hruyro(78,j) / Real(366 - leapyr)
!       hruyro(79,j)=hruyro(79,j) / Real(366 - leapyr)
!       hruyro(80,j)=hruyro(80,j) / Real(366 - leapyr)
!       hruyro(81,j)=hruyro(81,j) / Real(366 - leapyr)       
       pdvas(80) = hruyro(80,j)                                              !pdvas(80) = N2O(j)*1000.                                           
       pdvas(81) = hruyro(81,j)                                              !pdvas(81) = NO(j)*1000.                                          
       pdvas(82) = hruyro(82,j)                                              !pdvas(82) = nh4_vol(j)                                             
       pdvas(83) = hruyro(83,j)                                              !pdvas(83) = no3_denit(j)                                         
       pdvas(84) = hruyro(84,j)                                              !pdvas(84) = no3_nitr(j)                                            
       pdvas(85) = hruyro(85,j)                                              !pdvas(85) = no3_up(j)                                            
       pdvas(86) = hruyro(86,j)                                              !pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j) 
       pdvas(87) = hruyro(87,j)                                              !pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)
       pdvas(88) = hruyro(88,j)                                              !pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j)        

!       hruyro(82,j) = hruyro(82,j) / Real(366 - leapyr) !/12.    !!-------Annual average--
!       hruyro(83,j) = hruyro(83,j) / Real(366 - leapyr) !/12.    !!-------Annual average--
!       hruyro(84,j) = hruyro(84,j) / Real(366 - leapyr) !/12.    !!-------Annual average--
!       hruyro(85,j) = hruyro(85,j) / Real(366 - leapyr) !/12.    !!-------Annual average--
!       hruyro(86,j) = hruyro(86,j) / Real(366 - leapyr) !/12.    !!-------Annual average--
!       hruyro(87,j) = hruyro(87,j) / Real(366 - leapyr) !/12.    !!-------Annual average--
       pdvas(89) = hruyro(89,j)/ Real(366 - leapyr)                             !pdvas(89) = solc_no3(j)        
       pdvas(90) = hruyro(90,j)/ Real(366 - leapyr)                             !pdvas(90) = solc_nh4(j)        
       pdvas(91) = hruyro(91,j)/ Real(366 - leapyr)                             !pdvas(91) = solc_orgn(j)/1000. 
       pdvas(92) = hruyro(92,j)/ Real(366 - leapyr)                             !pdvas(92) = solc_orgp(j)/1000. 
       pdvas(93) = hruyro(93,j)/ Real(366 - leapyr)                             !pdvas(93) = solc_orgc(j)/1000. 
       pdvas(94) = hruyro(94,j)/ Real(366 - leapyr)                             !pdvas(94) = solc_orgcs(j)/1000.
       
       pdvas(95) = hruyro(95,j)                                               !pdvas(95) = sedc_d(j)                           !! kg C/ha
       pdvas(96) = hruyro(96,j)                                               !pdvas(96) = surfqc_d(j)                         !! kg C/ha
       pdvas(97) = hruyro(97,j)                                               !pdvas(97) = latc_d(j)                             !! kg C/ha
       pdvas(98) = hruyro(98,j)                                               !pdvas(98) = percc_d(j)                          !! kg C/ha
       pdvas(99) = hruyro(99,j)                                               !pdvas(99) = NPPC_d(j)                        !! kg C/ha
       pdvas(100) = hruyro(100,j)                                             !pdvas(100) = rspc_dnew(j)                        !! kg C/ha  
       pdvas(101) = hruyro(101,j)                                             !pdvas(101) = OrgC_Plt2Rsd(j)+ OrgC_Fer(j) 
       
       
!       hruyro(95,j)= hruyro(95,j) / Real(366 - leapyr)
!       hruyro(96,j)= hruyro(96,j) / Real(366 - leapyr)
!       hruyro(97,j)= hruyro(97,j) / Real(366 - leapyr)
!       hruyro(98,j)= hruyro(98,j) / Real(366 - leapyr)
!       hruyro(99,j)= hruyro(99,j) / Real(366 - leapyr)
!       hruyro(100,j)= hruyro(100,j) / Real(366 - leapyr)
!       hruyro(101,j)= hruyro(101,j) / Real(366 - leapyr)
!       hruyro(102,j)= hruyro(102,j) / Real(366 - leapyr)
!       hruyro(103,j)= hruyro(103,j) / Real(366 - leapyr)
!       hruyro(104,j)= hruyro(104,j) / Real(366 - leapyr)
!       hruyro(105,j)= hruyro(105,j) / Real(366 - leapyr)
!       hruyro(106,j)= hruyro(106,j) / Real(366 - leapyr)
       pdvas(102) = hruyro(102,j)                                                !pdvas(102) = Sed_RPOC(j)  
       pdvas(103) = hruyro(103,j)                                                !pdvas(103) = Sed_LPOC(j)  
       pdvas(104) = hruyro(104,j)                                                !pdvas(104) = HRU_RDOC(j)  
       pdvas(105) = hruyro(105,j)                                                !pdvas(105) = HRU_LDOC(j)  
       pdvas(106) = hruyro(106,j)                                                !pdvas(106) = HRU_DIC(j)   
       pdvas(107) = hruyro(107,j)                                                !pdvas(107) = SurQ_DOC(j)  
       pdvas(108) = hruyro(108,j)                                                !pdvas(108) = LatQT_DOC(j) 
       pdvas(109) = hruyro(109,j)                                                !pdvas(109) = PerQB_DOC(j) 
       pdvas(110) = hruyro(110,j)                                                !pdvas(110) = GwQ_DOC(j)   
       
       pdvas(111) = hruyro(111,j)                                                !pdvas(111) = SurQ_DIC(j)                              ! Surface DIC from HRUto stream (kg/ha)                   
       pdvas(112) = hruyro(112,j)                                                !pdvas(112) = LatQT_DIC(j)                            ! Lateral DIC from HRU to stream(kg/ha)                 
       pdvas(113) = hruyro(113,j)                                                !pdvas(113) = PerQB_DIC(j)                            ! DIC percolation amount from lowest soil layer to shallow aquifer           
                                                                                 
                                                                                 
!       hruyro(107,j) = hruyro(107,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(108,j) = hruyro(108,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
       pdvas(114) = hruyro(114,j)/ Real(366 - leapyr)                                                !pdvas(114) = solc_doc(j)                             !  Total DOC in soil profile (kg/ha)                                        
       pdvas(115) = hruyro(115,j)/ Real(366 - leapyr)                                                !pdvas(115) = solc_dic(j)                            !  Total DIC in soil profile (kg/ha)    
       
    
!       hruyro(109,j) = hruyro(109,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(110,j) = hruyro(110,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(111,j) = hruyro(111,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(112,j) = hruyro(112,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(113,j) = hruyro(113,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(114,j) = hruyro(114,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(115,j) = hruyro(115,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average--
!       hruyro(116,j) = hruyro(116,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average-- 
!       hruyro(117,j) = hruyro(117,j) / Real(366 - leapyr) !/ 12.      !!-------Annual average-- 
       pdvas(116) = hruyro(116,j)/ Real(366 - leapyr)                                               !pdvas(116) = sur_tmp(j)     
       pdvas(117) = hruyro(117,j)/ Real(366 - leapyr)                                               !pdvas(117) = soltmp_50(j)   
       pdvas(118) = hruyro(118,j)/ Real(366 - leapyr)                                               !pdvas(118) = soltmp_100(j)  
       pdvas(119) = hruyro(119,j)/ Real(366 - leapyr)                                               !pdvas(119) = soltmp_150(j)  
       pdvas(120) = hruyro(120,j)/ Real(366 - leapyr)                                               !pdvas(120) = soltmp_200(j)  
       pdvas(121) = hruyro(121,j)/ Real(366 - leapyr)                                               !pdvas(121) = soltmp_300(j)  
       pdvas(122) = hruyro(122,j)/ Real(366 - leapyr)                                               !pdvas(122) = soltmp_500(j)  
       pdvas(123) = hruyro(123,j)/ Real(366 - leapyr)                                               !pdvas(123) = soltmp_1000(j) 
       pdvas(124) = hruyro(124,j)                                                               !pdvas(124) = sol_frozday(j) 
       
!       hruyro(118,j)= hruyro(118,j) / Real(366 - leapyr)
!       hruyro(119,j)= hruyro(119,j) / Real(366 - leapyr)
!       hruyro(120,j)= hruyro(120,j) / Real(366 - leapyr)
!       hruyro(121,j)= hruyro(121,j) / Real(366 - leapyr)
!       hruyro(122,j)= hruyro(122,j) / Real(366 - leapyr)
!       hruyro(123,j)= hruyro(123,j) / Real(366 - leapyr)
!       hruyro(124,j)= hruyro(124,j) / Real(366 - leapyr)
       pdvas(125) = hruyro(125,j)                                                           !pdvas(125) = Sed_RPON(j)                
       pdvas(126) = hruyro(126,j)                                                           !pdvas(126) = Sed_LPON(j)             
       pdvas(127) = hruyro(127,j)                                                           !pdvas(127) = SurQ_DON(j)             
       pdvas(128) = hruyro(128,j)                                                           !pdvas(128) = LatQT_DON(j)            
       pdvas(129) = hruyro(129,j)                                                           !pdvas(129) = PerQ_DON (sol_nly(j),j) 
       pdvas(130) = hruyro(130,j)                                                           !pdvas(130) = rchrg_don (j)           
       
       
       
!       hruyro(125,j) = hruyro(125,j) / Real(366 - leapyr)
!       hruyro(126,j)= hruyro(126,j) / Real(366 - leapyr)
!       hruyro(127,j)= hruyro(127,j) / Real(366 - leapyr)
       pdvas(131) = hruyro(131,j)                                                           !pdvas(131) = GwQ_DON (j)            
       pdvas(132) = hruyro(132,j)                                                           !pdvas(132) = shallst_don_decay(j)   
       pdvas(133) = hruyro(133,j)                                                           !pdvas(133) = revap_don(j)           
       pdvas(134) = hruyro(134,j)                                                           !pdvas(134) = gwseep_don(j)          
       pdvas(135) = hruyro(135,j)/ Real(366 - leapyr)                                                           !pdvas(135) = shallst_don(j)          
      
       
       pdvas(136) = hruyro(136,j)                                               !pdvas(136) = rchrg_doc(j)            
       pdvas(137) = hruyro(137,j)                                               !pdvas(137) = shallst_doc_decay(j)    
       pdvas(138) = hruyro(138,j)                                               !pdvas(138) = revap_doc(j)            
       pdvas(139) = hruyro(139,j)                                               !pdvas(139) = gwseep_doc(j)         
       pdvas(140) = hruyro(140,j)/ Real(366 - leapyr)                                               !pdvas(140) = shallst_doc(j)   
       pdvas(141) = hruyro(141,j)                                               !pdvas(141) = GwQ_DIC(j)     
       pdvas(142) = hruyro(142,j)                                               !pdvas(142) = rchrg_dic(j)   
       pdvas(143) = hruyro(143,j)                                               !pdvas(143) = revap_dic(j)   
       pdvas(144) = hruyro(144,j)                                               !pdvas(144) = gwseep_dic(j)  
       
!       hruyro(138,j)=hruyro(138,j) / Real(366 - leapyr) !/ 12.
       pdvas(145) = hruyro(145,j)/ Real(366 - leapyr)                                             !pdvas(145) = shallst_dic(j) 
       
       pdvas(146) = hruyro(146,j)/ Real(366 - leapyr)                                               !pdvas(146) = sol_soc(1,j) 
       pdvas(147) = hruyro(147,j)/ Real(366 - leapyr)                                               !pdvas(147) = sol_soc(2,j) 
       pdvas(148) = hruyro(148,j)/ Real(366 - leapyr)                                               !pdvas(148) = sol_soc(3,j) 
       pdvas(149) = hruyro(149,j)/ Real(366 - leapyr)                                               !pdvas(149) = sol_soc(4,j) 
       pdvas(150) = hruyro(150,j)/ Real(366 - leapyr)                                               !pdvas(150) = sol_soc(5,j) 
       pdvas(151) = hruyro(151,j)/ Real(366 - leapyr)                                               !pdvas(151) = sol_soc(6,j) 
       pdvas(152) = hruyro(152,j)/ Real(366 - leapyr)                                               !pdvas(152) = sol_soc(7,j) 
       pdvas(153) = hruyro(153,j)/ Real(366 - leapyr)                                               !pdvas(153) = sol_soc(8,j) 
   !!-------------------------------------------------------------------------------------------------------
        
        if (ipdvas(1) > 0) then
          do ii = 1, itots
            pdvs(ii) = pdvas(ipdvas(ii))
          end do
 
        idplant = idplt(j)
        if (idplant > 0) then
          cropname = cpnm(idplant)
        else
          cropname = "NOCR"
        endif
        
        if (ioutput == 1) then
          !!output basic information
          call sqlite3_set_column( colhru(1), j )
          call sqlite3_set_column( colhru(2), cropname )
          call sqlite3_set_column( colhru(3), nmgt(j) )
          call sqlite3_set_column( colhru(4), iyr )
        end if

          if (iscen == 1 .and. isproj == 0) then       
              if(ioutput == 1) then
                  do ii = 1, itots
                    call sqlite3_set_column(colhru(tblhru_num + ii), pdvs(ii))
                  end do
              else
          write (output_hru_num,1000) cropname, j, subnum(j), hruno(j), sb, nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots)
 1000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,66f10.3,e10.3,e10.3,8f10.3,6f10.3,66f10.3,8f10.3)   
             end if
      !     else if (isproj == 1) then
    !      write (21,1000) cropname, j, subnum(j), hruno(j),             
    ! &            sb, nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots)
    !      else if (iscen == 1 .and. isproj == 2) then
   !       write (output_hru_num,2000) cropname, j, subnum(j), hruno(j), sb,         
   !  &    nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
          endif
        else
         if (iscen == 1 .and. isproj == 0) then
            if(ioutput == 1) then
                do ii = 1, mhruo
                    call sqlite3_set_column(colhru(tblhru_num + ii),pdvas(ii))
                end do                
            else
         write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb, nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo)
 1001 format (a4,i7,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,3e10.3,6f10.3,1x,i4,66f10.3)    
            end if
    !      else if (isproj == 1) then
      !     write (21,1001) cropname, j, subnum(j), hruno(j),             
    ! &           sb, nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo)
      !    else if (iscen == 1 .and. isproj == 2) then
      !    write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,         
      ! &    nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
          endif
        end if
        end if
        
        if(ioutput == 1) then
            call sqlite3_insert_stmt( db, stmthru, colhru )
        end if
      end do

      return

 !2000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
      end