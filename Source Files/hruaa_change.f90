      subroutine hruaa(years)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_aams(:)   |metric tons/ha|average annual biomass (dry weight) in HRU
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hruaao(1,:)   |mm H2O        |precipitation in HRU during simulation
!!    hruaao(2,:)   |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow in HRU during simulation
!!    hruaao(3,:)   |mm H2O        |amount of snow melt in HRU during simulation
!!    hruaao(4,:)   |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during simulation (ignores impact of
!!                                 |transmission losses)
!!    hruaao(5,:)   |mm H2O        |amount of lateral flow contribution to main
!!                                 |channel from HRU during simulation
!!    hruaao(6,:)   |mm H2O        |amount of groundwater flow contribution to
!!                                 |main channel from HRU during simulation
!!    hruaao(7,:)   |mm H2O        |amount of water moving from shallow aquifer
!!                                 |to plants or soil profile in HRU during 
!!                                 |simulation
!!    hruaao(8,:)   |mm H2O        |amount of water recharging deep aquifer in
!!                                 |HRU during simulation
!!    hruaao(9,:)   |mm H2O        |total amount of water entering both aquifers
!!                                 |from HRU during simulation
!!    hruaao(10,:)  |mm H2O        |water yield (total amount of water entering
!!                                 |main channel) from HRU during simulation
!!    hruaao(11,:)  |mm H2O        |amount of water percolating out of the soil
!!                                 |profile and into the vadose zone in HRU
!!                                 |during simulation
!!    hruaao(12,:)  |mm H2O        |actual evapotranspiration in HRU during 
!!                                 |simulation
!!    hruaao(13,:)  |mm H2O        |amount of transmission losses from tributary
!!                                 |channels in HRU for simulation
!!    hruaao(14,:)  |metric tons/ha|sediment yield from HRU for simulation
!!    hruaao(17,:)  |kg N/ha       |amount of nitrogen applied in continuous 
!!                                 |fertilizer operation in HRU for simulation
!!    hruaao(18,:)  |kg P/ha       |amount of phosphorus applied in continuous
!!                                 |fertilizer operation in HRU for simulation
!!    hruaao(23,:)  |mm H2O        |amount of water removed from shallow aquifer
!!                                 |in HRU for irrigation during simulation
!!    hruaao(24,:)  |mm H2O        |amount of water removed from deep aquifer
!!                                 |in HRU for irrigation during simulation
!!    hruaao(25,:)  |mm H2O        |potential evapotranspiration in HRU during
!!                                 |simulation
!!    hruaao(26,:)  |kg N/ha       |annual amount of N (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruaao(27,:)  |kg P/ha       |annual amount of P (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruaao(28,:)  |kg N/ha       |average annual amount of N (organic &
!!                                 |mineral) auto-applied in HRU
!!    hruaao(29,:)  |kg P/ha       |average annual amount of P (organic &
!!                                 |mineral) auto-applied in HRU
!!    hruaao(31,:)  |stress days   |water stress days in HRU during simulation
!!    hruaao(32,:)  |stress days   |temperature stress days in HRU during 
!!                                 |simulation
!!    hruaao(33,:)  |stress days   |nitrogen stress days in HRU during simulation
!!    hruaao(34,:)  |stress days   |phosphorus stress days in HRU during 
!!                                 |simulation
!!    hruaao(35,:)  |kg N/ha       |organic nitrogen in surface runoff in HRU
!!                                 |during simulation
!!    hruaao(36,:)  |kg P/ha       |organic phosphorus in surface runoff in HRU
!!                                 |during simulation
!!    hruaao(37,:)  |kg N/ha       |nitrate in surface runoff in HRU during
!!                                 |simulation
!!    hruaao(38,:)  |kg N/ha       |nitrate in lateral flow in HRU during
!!                                 |simulation
!!    hruaao(39,:)  |kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                                 |during simulation
!!    hruaao(40,:)  |kg N/ha       |amount of nitrogen removed from soil by plant
!!                                 |uptake in HRU during simulation
!!    hruaao(41,:)  |kg N/ha       |nitrate percolating past bottom of soil
!!                                 |profile in HRU during simulation
!!    hruaao(42,:)  |kg P/ha       |amount of phosphorus removed from soil by
!!                                 |plant uptake in HRU during simulation
!!    hruaao(43,:)  |kg P/ha       |amount of phosphorus moving from labile
!!                                 |mineral to active mineral pool in HRU during
!!                                 |simulation
!!    hruaao(44,:)  |kg P/ha       |amount of phosphorus moving from active
!!                                 |mineral to stable mineral pool in HRU during
!!                                 |simulation
!!    hruaao(45,:)  |kg N/ha       |amount of nitrogen applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |simulation
!!    hruaao(46,:)  |kg P/ha       |amount of phosphorus applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |simulation
!!    hruaao(47,:)  |kg N/ha       |amount of nitrogen added to soil by fixation
!!                                 |in HRU during simulation
!!    hruaao(48,:)  |kg N/ha       |amount of nitrogen lost by denitrification
!!                                 |in HRU during simulation
!!    hruaao(49,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to nitrate pool in HRU during simulation
!!    hruaao(50,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to stable organic pool in HRU during 
!!                                 |simulation
!!    hruaao(51,:)  |kg P/ha       |amount of phosphorus moving from organic to
!!                                 |labile mineral pool in HRU during simulation
!!    hruaao(52,:)  |kg N/ha       |amount of nitrogen moving from fresh organic
!!                                 |to nitrate and active organic pools in HRU
!!                                 |during simulation
!!    hruaao(53,:)  |kg P/ha       |amount of phosphorus moving from fresh
!!                                 |organic to the labile mineral and organic
!!                                 |pools in HRU during simulation
!!    hruaao(54,:)  |kg N/ha       |amount of nitrogen added to soil in rain
!!                                 |during simulation
!!    hruaao(61,:)  |metric tons/ha|daily soil loss predicted with USLE equation
!!    hruaao(63,:)  |# bacteria/ha |less persistent bacteria transported to main
!!                                 |channel from HRU during simulation
!!    hruaao(64,:)  |# bacteria/ha |persistent bacteria transported to main
!!                                 |channel from HRU during simulation
!!    hruaao(65,:)  |kg N/ha       |nitrate loading from groundwater in HRU to
!!                                 |main channel during simulation
!!    hruaao(66,:)  |kg P/ha       |soluble P loading from groundwater in HRU to
!!                                 |main channel during simulation
!!    hruaao(67,:)  |kg P/ha       |loading of mineral P attached to sediment
!!                                 |in HRU to main channel during simulation
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed (output.hru)
!!    lai_aamx(:)   |none          |average annual maximum leaf area index in
!!                                 |HRU
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |on any given day
!!    yldaa(:)      |metric tons/ha|average annual yield (dry weight) in HRU
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
!!    years       |years         |length of simulation
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
        
      real, intent (in) :: years
      integer :: j, sb, ii, iflag
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname
      real :: idplant

      do j = 1, nhru
        sb = hru_sub(j)

        iflag = 0
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do


        if (iflag == 1) then
        pdvas = 0.
        pdvs = 0.

        pdvas(1) = hruaao(1,j)                                    ! hrumono(1,j) = hrumono(1,j) + subp(j)   !                               
        pdvas(2) = hruaao(2,j)                                    ! hrumono(2,j) = hrumono(2,j) + snofall   !                               
        pdvas(3) = hruaao(3,j)                                    ! hrumono(3,j) = hrumono(3,j) + snomlt    !                               
        pdvas(4) = hruaao(4,j)                                    !hrumono(4,j) = hrumono(4,j) + aird(j)   !qday                           
        pdvas(5) = hruaao(5,j)                                    !hrumono(5,j) = hrumono(5,j) + pet_day   !latq(j)                        
        pdvas(6) = hruaao(6,j)                                    !hrumono(6,j) = hrumono(6,j) + etday     !gw_q(j)                        
        pdvas(7) = hruaao(7,j)/ Real(355.2)                                    !hrumono(7,j) = hrumono(7,j) + sol_cnsw(j)   !revapday                   
        pdvas(8) = hruaao(8,j) !sol_sw(j)                                      ! hrumono(8,j) = hrumono(8,j) + sol_sw(j) !gwseep                         
        pdvas(9) = hruaao(9,j)                                    !hrumono(9,j) = hrumono(9,j) + sepbtm(j) !rchrg(j)                       
        pdvas(10) = hruaao(10,j)                                  !  hrumono(10,j) = hrumono(10,j) + rchrg(j)    !qdr(j)                     
        pdvas(11) = hruaao(11,j)                                  !  hrumono(11,j) = hrumono(11,j) + gwseep  !sepbtm(j)                      
        pdvas(12) = hruaao(12,j)                                  !  hrumono(12,j) = hrumono(12,j) + revapday    !etday                      
        pdvas(13) = hruaao(13,j)                                  ! hrumono(13,j) = hrumono(13,j) + shallirr(j) !tloss                      
        pdvas(14) = hruaao(14,j)                                  ! hrumono(14,j) = hrumono(14,j) + deepirr(j)  !sedyld(j) / hru_ha(j)      
        pdvas(15) = hruaao(15,j)/ Real(355.2)!shallst(j)                                    ! hrumono(15,j) = hrumono(15,j) + shallst(j)  !ep_day                     
        pdvas(16) = hruaao(16,j)/ Real(355.2)!deepst(j)                                     ! hrumono(16,j) = hrumono(16,j) + deepst(j)   !es_day                     
        pdvas(17) = hruaao(17,j)                                  ! hrumono(17,j) = hrumono(17,j) + surfq(j)    !cfertn                     
        pdvas(18) = hruaao(18,j)                                  !  hrumono(18,j) = hrumono(18,j) + qday    !cfertp                         
        pdvas(19) = hruaao(19,j)                                  ! hrumono(19,j) = hrumono(19,j) + latq(j)   !surfq(j)                     
        pdvas(20) = hruaao(20,j)                                  !  hrumono(20,j) = hrumono(20,j) + latq(j) - lpndloss - lwetloss  !cnday(j)
        pdvas(21) = hruaao(21,j)                                  !  hrumono(21,j) = hrumono(21,j) + qtile   !sol_cnsw(j)                    
        
        pdvas(22) = hruaao(22,j)                                  !hrumono(22,j) = hrumono(22,j) + gw_q(j)    !qdr(:)      |mm H2O        |net water loading from HRU to main channel       
        pdvas(23) = hruaao(23,j)                           !hrumono(23,j) = hrumono(23,j) + gw_qdeep(j)  !shallirr(j)                                                                
        pdvas(24) = hruaao(24,j)                           !hrumono(24,j) = hrumono(24,j) + tloss     !deepirr(j)
        pdvas(25) = hruaao(25,j)                           !hrumono(25,j) = hrumono(25,j) + qdr(j)  !pet_day
        pdvas(26) = hruaao(26,j)/ Real(355.2)                           !hrumono(26,j) = hrumono(26,j) + cnday(j)  !grazn                                        pdvas(26) = cnday(j)
        pdvas(27) = hruaao(27,j)/ Real(355.2)                           !hrumono(27,j) = hrumono(27,j) + tmpav(j)    !grazp                                pdvas(27) = tmpav(j)
        pdvas(28) = hruaao(28,j)/ Real(355.2)                           !hrumono(28,j) = hrumono(28,j) + tmx(j)   !auton                                    pdvas(28) = tmx(j)
        
        pdvas(29) = hruaao(29,j)/ Real(355.2)                                  !hrumono(29,j) = hrumono(29,j) + tmn(j)   !autop                        pdvas(29) = tmn(j)
        pdvas(30) = hruaao(30,j)/ Real(355.2)                                  !hrumono(30,j) = hrumono(30,j) + hru_ra(j)     !sol_tmp(2,j)         pdvas(30) = hru_ra(j) 
        pdvas(31) = hruaao(31,j)                                  !hrumono(31,j) = hrumono(31,j) + sedyld(j) / hru_ha(j)   !(1.-strsw(j))          pdvas(31) = sedyld(j) / hru_ha(j)
        pdvas(32) = hruaao(32,j)                                  !hrumono(32,j) = hrumono(32,j) + sedgen(j) / hru_ha(j)  !(1.-strstmp(j))        pdvas(32) = sedgen(j)/ hru_ha(j)                                                                                      
        pdvas(33) = hruaao(33,j)                                  !hrumono(33,j) = hrumono(33,j) + sedorgn(j)  !(1.-strsn(j))          pdvas(33) = sedorgn(j)               
        pdvas(34) = hruaao(34,j)                                  !hrumono(34,j) = hrumono(34,j) + sedorgp(j)     !(1.-strsp(j))          pdvas(34) = sedorgp(j)               
        pdvas(35) = hruaao(35,j)                                  !hrumono(35,j) = hrumono(35,j) + sedminpa(j) + sedminps(j)  !sedorgn(j)             pdvas(35) = sedminpa(j) + sedminps(j)
        
        pdvas(36) = hruaao(36,j)                                  !hrumono(36,j) = hrumono(36,j) + fertn   !sedorgp(j)             pdvas(36) = fertn
        pdvas(37) = hruaao(37,j)                                  !hrumono(37,j) = hrumono(37,j) + fertp  !surqno3(j)             pdvas(37) = fertp
        pdvas(38) = hruaao(38,j)                                  !hrumono(38,j) = hrumono(38,j) + auton  !latno3(j)              pdvas(38) = auton
        pdvas(39) = hruaao(39,j)                                  !hrumono(39,j) = hrumono(39,j) + autop  !surqsolp(j)            pdvas(39) = autop
        pdvas(40) = hruaao(40,j)                                  !hrumono(40,j) = hrumono(40,j) + grazn   !nplnt(j)               pdvas(40) = grazn
        pdvas(41) = hruaao(41,j)                                  !hrumono(41,j) = hrumono(41,j) + grazp  !percn(j)               pdvas(41) = grazp
        pdvas(42) = hruaao(42,j)                                  !hrumono(42,j) = hrumono(42,j) + cfertn   !pplnt(j)               pdvas(42) = cfertn
        pdvas(43) = hruaao(43,j)                                  !hrumono(43,j) = hrumono(43,j) + cfertp   !rmp1tl                 pdvas(43) = cfertp
        pdvas(44) = hruaao(44,j)                                  !hrumono(44,j) = hrumono(44,j) + no3pcp  !roctl                  pdvas(44) = no3pcp
        pdvas(45) = hruaao(45,j)                                  !hrumono(45,j) = hrumono(45,j) + fixn   !fertn                  pdvas(45) = fixn
        
        pdvas(46) = hruaao(46,j)                                  !hrumono(46,j) = hrumono(46,j) + nplnt(j)  !fertp          pdvas(46) = nplnt(j) 
        pdvas(47) = hruaao(47,j)                                  !hrumono(47,j) = hrumono(47,j) + pplnt(j)   !fixn           pdvas(47) = pplnt(j) 
        pdvas(48) = hruaao(48,j)                                  !hrumono(48,j) = hrumono(48,j) + wdntl   !wdntl          pdvas(48) = wdntl          
        pdvas(49) = hruaao(49,j)                                  !hrumono(49,j) = hrumono(49,j) + surqno3(j)    !hmntl                  pdvas(49) = surqno3(j)
        pdvas(50) = hruaao(50,j)                                  !hrumono(50,j) = hrumono(50,j) + latno3(j)   !rwntl                  pdvas(50) = latno3(j)  
        pdvas(51) = hruaao(51,j)                                  !hrumono(51,j) = hrumono(51,j) + tileno3(j)  !hmptl                  pdvas(51) = tileno3(j) 
        pdvas(52) = hruaao(52,j)                                  !hrumono(52,j) = hrumono(52,j) + no3gw(j)  !rmn2tl                 pdvas(52) = no3gw(j)   
        pdvas(53) = hruaao(53,j)                                  !hrumono(53,j) = hrumono(53,j) + percn(j)  !         pdvas(53) = percn(j)   
        
        pdvas(54) = hruaao(54,j)                                  !hrumono(54,j) = hrumono(54,j) + rchrg_n(j)  !no3pcp             pdvas(54) = rchrg_n(j)   
        pdvas(55) = hruaao(55,j)                                  !hrumono(55,j) = hrumono(55,j) + revapn(j)   !tmx(j)             pdvas(55) = revapn(j)    
        pdvas(56) = hruaao(56,j)                                  !hrumono(56,j) = hrumono(56,j) + gwseepn(j)     !tmn(j)             pdvas(56) = gwseepn(j)   
        pdvas(57) = hruaao(57,j)                                  !hrumono(57,j) = hrumono(57,j) + gw_no3loss(j)    !tmpav(j)           pdvas(57) = gw_no3loss(j)
        pdvas(58) = hruaao(58,j)/ Real(355.2)                                  !hrumono(58,j) = hrumono(58,j) + shallst_n(j)  !hru_ra(j)          pdvas(58) = shallst_n(j) 
        pdvas(59) = hruaao(59,j)                                  !hrumono(59,j) = hrumono(59,j) + surqsolp(j)   !                   pdvas(59) = surqsolp(j)  
        pdvas(60) = hruaao(60,j)                                  !hrumono(60,j) = hrumono(60,j) + vap_tile    !usle           pdvas(60) = vap_tile     
        pdvas(61) = hruaao(61,j)                                  !hrumono(61,j) = hrumono(61,j) + minpgw(j)  !qtile          pdvas(61) = minpgw(j)    
        pdvas(62) = hruaao(62,j)                                  !hrumono(62,j) = hrumono(62,j) + (1.-strsw(j))    !bactrop + bactsedp                     pdvas(62) = (1.-strsw(j))      
        pdvas(63) = hruaao(63,j)                                  !hrumono(63,j) = hrumono(63,j) + (1.-strstmp(j))    !bactrolp + bactsedlp                   pdvas(63) = (1.-strstmp(j))  
        
        pdvas(64) = hruaao(64,j) !bio_aams(j)                                   !hrumono(64,j) = hrumono(64,j) + (1.-strsn(j))   !no3gw(j)                           pdvas(64) = (1.-strsn(j))       
        pdvas(65) = hruaao(65,j)!lai_aamx(j)                                   !hrumono(65,j) = hrumono(65,j) + (1.-strsp(j))    !minpgw(j)                                  pdvas(65) = (1.-strsp(j))       
        pdvas(66) = hruaao(66,j)!yldaa(j)                                      !hrumono(66,j) = hrumono(66,j) + bio_ms(j) / 1000.   !sedminpa(j) + sedminps(j)                pdvas(66) = bio_ms(j) / 1000.   
        pdvas(67) = hruaao(67,j)/ Real(355.2)                                  !hrumono(67,j) = hrumono(67,j) + laiday(j)  !                                  pdvas(67) = laiday(j)           
        pdvas(68) = hruaao(68,j)                                  !hrumono(68,j) = hrumono(68,j) + yield(j)/1000.    !latno3(j)                      pdvas(68) = yield/1000.   
!!      the following two variables are values at the of the year
!!      they are not summed each day
        pdvas(69) = hruaao(69,j) !wtab(j)  !! based on 30 day antecedent climate(mm) (prec,et)     !hrumono(69,j) = hrumono(69,j) + yieldgrn(j)/1000. !gw_qdeep(j)                                    pdvas(69) = yieldgrn/1000.
        pdvas(70) = hruaao(70,j) !wtabelo  !! based on depth from soil surface(mm)                 !hrumono(70,j) = hrumono(70,j) + yieldbms(j)/1000.  !latq(j) - lpndloss - lwetloss              pdvas(70) = yieldbms/1000.
!!      added current snow content in the hru (not summed)                           
        pdvas(71) = hruaao(71,j) !sno_hru(j)                                                       !hrumono(71,j) = hrumono(71,j) + yieldtbr(j)/1000.  !vap_tile                                   pdvas(71) = yieldtbr/1000.
        
!!      added current soil carbon for first layer
        pdvas(72) = cmup_kgh(j)   !! first soil layer only                           !hrumono(72,j) = hrumono(71,j) + yieldrsd(j)/1000.  !vap_tile                                   pdvas(72) = yieldrsd/1000.
!!      added current soil carbon integrated - aggregating all soil layers
        pdvas(73) = cmtot_kgh(j)                                                     !hrumono(73,j) = hrumono(73,j) + bactrop + bactsedp     !sol_LSLC(1,j) /1000.  !N2O(j) *1000                         !! gN/ha
  
!!      adding qtile to output.hru write 3/2/2010 gsm
        pdvas(74) = hruaao(74,j)                                                     !hrumono(74,j) = hrumono(74,j) + bactrolp + bactsedlp    !qtile   !sol_LSLC(2,j) /1000.  !NO(j) *1000                           !! gN/ha 
!!      tileno3 - output.hru                                                         
        pdvas(75) = hruaao(75,j)/ Real(355.2)                                                     !hrumono(75,j) = hrumono(75,j) + wtab(j) !tileno3(j)  !sol_LSLC(3,j) /1000.  !nh4_vol(j)                                !! kg N/ha      
!!      latno3 - output.hru                                                          
        pdvas(76) = hruaao(76,j)/ Real(355.2)                                                     !hrumono(76,j) = hrumono(76,j) + wat_tbl(j)  !latno3(j)   !sol_LSLNC(1,j) /1000.   !no3_denit(j)                             !! kg N/ha 
!!      gw qdeep                                                                     
        pdvas(77) = hruaao(77,j)/ Real(355.2)                                                     !hrumono(77,j) = hrumono(77,j) + sno_hru(j)  !gw_qdeep(j)     !sol_LSLNC(2,j)/1000.   !no3_nitr(j)                                !! kg N/ha  
!!      latq continuous                                                              
        pdvas(78) = hruaao(78,j)/ Real(355.2)                                                     !hrumono(78,j) = hrumono(78,j) + sol_rsd(1,j)/1000.  !latq(j) - lpndloss - lwetloss   !sol_LSLNC(3,j)/1000.  !no3_up(j)                                 !! kg N/ha   different from nplnt(j)
!!      phos due to crack flow (tvap)                                                
        pdvas(79) = hruaao(79,j)/ Real(355.2)                                                     !hrumono(79,j) = hrumono(79,j) + sol_cov(j)/1000.    !vap_tile !sol_LMC(1,j)/1000.    !no3_fert(j) + no3_autof(j)         !! kg N/ha 

        pdvas(80) = hruaao(80,j)                                                     !hrumono(80,j) = hrumono(80,j) + N2O(j)*1000.    !sol_LMC(2,j) /1000.  !nh4_autof(j) + nh4_fert(j)         !! kg N/ha 
        pdvas(81) = hruaao(81,j)                                                     !hrumono(81,j) = hrumono(81,j) + NO(j)*1000.   !pdvas(81) = NO(j)*1000.    !        !                        !! g            !! kg N/ha     
        pdvas(82) = hruaao(82,j)                                                     !hrumono(82,j) = hrumono(82,j) + nh4_vol(j)   !pdvas(82) = nh4_vol(j)    !        !                            !a 
        pdvas(83) = hruaao(83,j)                                                     !hrumono(83,j) = hrumono(83,j) + no3_denit(j)  !pdvas(83) = no3_denit(j)   !         !                           ha 
        pdvas(84) = hruaao(84,j)                                                     !hrumono(84,j) = hrumono(84,j) + no3_nitr(j)  !pdvas(84) = no3_nitr(j)   !         !                            
        pdvas(85) = hruaao(85,j)                                                     !hrumono(85,j) = hrumono(85,j) + no3_up(j)   !pdvas(85) = no3_up(j)     !                               !! kg N
        pdvas(86) = hruaao(86,j)                                                     !hrumono(86,j) = hrumono(86,j) + no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j)    !pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j) 
        pdvas(87) = hruaao(87,j)                                                     !hrumono(87,j) = hrumono(87,j) + nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)    !pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)
        pdvas(88) = hruaao(88,j)                                                     !hrumono(88,j) = hrumono(88,j) + orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd(j)    !pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j)        
        
        pdvas(89) = hruaao(89,j)/ Real(355.2)                                                    !hrumono(89,j) = hrumono(89,j) + solc_no3(j)     ! surfqc_d(j)                                  !! kg C/ha
        pdvas(90) = hruaao(90,j)/ Real(355.2)                                                     !hrumono(90,j) = hrumono(90,j) + solc_nh4(j)     ! latc_d(j)                                      !! kg C/ha
        pdvas(91) = hruaao(91,j)/ Real(355.2)                                                     !hrumono(91,j) = hrumono(91,j) + solc_orgn(j)/1000.  ! percc_d(j)                                   !! kg C/ha
        pdvas(92) = hruaao(92,j)/ Real(355.2)                                                     !hrumono(92,j) = hrumono(92,j) + solc_orgp(j)/1000.  ! NPPC_d(j)                                 !! kg C/ha
        pdvas(93) = hruaao(93,j)/ Real(355.2)                                                     !hrumono(93,j) = hrumono(93,j) + solc_orgc(j)/1000.  ! rspc_dnew(j) ! rspc_d(j)                                     !! kg C/ha
        pdvas(94) = hruaao(94,j)/ Real(355.2)                                                     !hrumono(94,j) = hrumono(94,j) + solc_orgcs(j)/1000.     ! OrgC_Plt2Rsd(j)+OrgC_Fer(j) ! HRU_CH4g (j)      
        pdvas(95) = hruaao(95,j)                                                     !hrumono(95,j) = hrumono(95,j) + sedc_d(j)   !sol_BMC(1,j)/1000. ! HSC1/1000.  ! Sed_RPOC(j)                        !RPOC amount for hru (kg/ha)
        pdvas(96) = hruaao(96,j)                                                     !hrumono(96,j) = hrumono(96,j) + surfqc_d(j)     !sol_BMC(2,j) /1000. !BMC1/1000.  !Sed_LPOC(j)                       !LPOC amount for hru (kg/ha)
        pdvas(97) = hruaao(97,j)                                                     !hrumono(97,j) = hrumono(97,j) + latc_d(j)   !sol_BMC(3,j) /1000. !LMC1/1000.   ! HRU_RDOC(j)                     !RDOC amount for hru (kg/ha)
        pdvas(98) = hruaao(98,j)                                                     !hrumono(98,j) = hrumono(98,j) + percc_d(j)      !sol_HSC(1,j) /1000. !LSC1/1000.    !HRU_LDOC(j)                        !LDOC amount for hru  (kg/ha)
        pdvas(99) = hruaao(99,j)                                                     !hrumono(99,j) = hrumono(99,j) + NPPC_d(j)   !sol_HSC(2,j)/1000.   !HPC2/1000.   !HRU_DIC(j)                            !DIC amount for hru (kg/ha)    
        
        pdvas(100) = hruaao(100,j)                                                   !hrumono(100,j) = hrumono(100,j) + rspc_dnew(j)  !  soil profile respC- DIC loss             !! kg C/ha           
        pdvas(101) = hruaao(101,j)                                                   !hrumono(101,j) = hrumono(101,j) + OrgC_Plt2Rsd(j)+ OrgC_Fer(j)  !sol_HPC(1,j) /1000.   !BMC2/1000.  !LatQT_DOC(j)
        pdvas(102) = hruaao(102,j)                                                   !hrumono(102,j) = hrumono(102,j) + Sed_RPOC(j)    !pdvas(102) = Sed_RPOC(j)
        pdvas(103) = hruaao(103,j)                                                   !hrumono(103,j) = hrumono(103,j) + Sed_LPOC(j)    !pdvas(103) = Sed_LPOC(j)
        pdvas(104) = hruaao(104,j)                                                   !hrumono(104,j) = hrumono(104,j) + HRU_RDOC(j)  !pdvas(104) = HRU_RDOC(j)  
        pdvas(105) = hruaao(105,j)                                                   !hrumono(105,j) = hrumono(105,j) + HRU_LDOC(j)    !pdvas(105) = HRU_LDOC(j)
        pdvas(106) = hruaao(106,j)                                                   !hrumono(106,j) = hrumono(106,j) + HRU_DIC(j)     !pdvas(106) = HRU_DIC(j) 
        
        pdvas(107) = hruaao(107,j)                                                   !hrumono(107,j) = hrumono(107,j) + SurQ_DOC(j)  !pdvas(107) = SurQ_DOC(j)   
        pdvas(108) = hruaao(108,j)                                                   !hrumono(108,j) = hrumono(108,j) + LatQT_DOC(j)   !pdvas(108) = LatQT_DOC(j)
        pdvas(109) = hruaao(109,j)                                                   !hrumono(109,j) = hrumono(109,j) + PerQB_DOC(j)   !pdvas(109) = PerQB_DOC(j)
        pdvas(110) = hruaao(110,j)                                                   !hrumono(110,j) = hrumono(110,j) + GwQ_DOC(j)   !pdvas(110) = GwQ_DOC(j)    
        pdvas(111) = hruaao(111,j)                                                   !hrumono(111,j) = hrumono(111,j) + SurQ_DIC(j)  !pdvas(111) = SurQ_DIC(j)  
        pdvas(112) = hruaao(112,j)                                                   !hrumono(112,j) = hrumono(112,j) + LatQT_DIC(j)  !pdvas(112) = LatQT_DIC(j)
        pdvas(113) = hruaao(113,j)                                                   !hrumono(113,j) = hrumono(113,j) + PerQB_DIC(j) !pdvas(113) = PerQB_DIC(j) 
        pdvas(114) = hruaao(114,j)/ Real(355.2)                                                   !hrumono(114,j) = hrumono(114,j) + solc_doc(j) !pdvas(114) = solc_doc(j)  
        pdvas(115) = hruaao(115,j)/ Real(355.2)                                                   !hrumono(115,j) = hrumono(115,j) + solc_dic(j) !pdvas(115) = solc_dic(j)  
        pdvas(116) = hruaao(116,j)/ Real(355.2)                                                   !hrumono(116,j) = hrumono(116,j) + sur_tmp(j)     !pdvas(116) = sur_tmp(j)   
        
        pdvas(117) = hruaao(117,j)/ Real(355.2)                                                   !hrumono(117,j) = hrumono(117,j) + soltmp_50(j)     !pdvas(117) = soltmp_50(j) 
        pdvas(118) = hruaao(118,j)/ Real(355.2)                                                   !hrumono(118,j) = hrumono(118,j) + OXGF(2,j)     !pdvas(118) = soltmp_100(j)
        pdvas(119) = hruaao(119,j)/ Real(355.2)                                                   !hrumono(119,j) = hrumono(119,j) + soltmp_100(j)   !pdvas(119) = soltmp_150(j)
        pdvas(120) = hruaao(120,j)/ Real(355.2)                                                   !hrumono(120,j) = hrumono(120,j) + soltmp_200(j)    !pdvas(120) = soltmp_200(j)
        
        pdvas(121) = hruaao(121,j)/ Real(355.2)                                                   !hrumono(121,j) = hrumono(121,j) + soltmp_300(j)  !pdvas(121) = soltmp_300(j) 
        pdvas(122) = hruaao(122,j)/ Real(355.2)                                                   !hrumono(122,j) = hrumono(122,j) + soltmp_500(j) !pdvas(122) = soltmp_500(j) 
        pdvas(123) = hruaao(123,j)/ Real(355.2)                                                   !hrumono(123,j) = hrumono(123,j) + soltmp_1000(j) !pdvas(123) = soltmp_1000(j)
        pdvas(124) = hruaao(124,j)                                                   !hrumono(124,j) = hrumono(124,j) + sol_frozday(j) !pdvas(124) = sol_frozday(j)
        
        pdvas(125) = hruaao(125,j)                                                   !hrumono(125,j) = hrumono(125,j) + Sed_RPON(j)     !pdvas(125) = Sed_RPON(j)              
        pdvas(126) = hruaao(126,j)                                                   !hrumono(126,j) = hrumono(126,j) + Sed_LPON(j)     !pdvas(126) = Sed_LPON(j)              
        pdvas(127) = hruaao(127,j)                                                   !hrumono(127,j) = hrumono(127,j) + SurQ_DON(j)     !pdvas(127) = SurQ_DON(j)              
        pdvas(128) = hruaao(128,j)                                                   !hrumono(128,j) = hrumono(128,j) + LatQT_DON(j)      !pdvas(128) = LatQT_DON(j)             
        pdvas(129) = hruaao(129,j)                                                   !hrumono(129,j) = hrumono(129,j) + PerQ_DON (sol_nly(j),j)      !pdvas(129) = PerQ_DON (sol_nly(j),j) 
        pdvas(130) = hruaao(130,j)                                                   !hrumono(130,j) = hrumono(130,j) + rchrg_don (j)    !pdvas(130) = rchrg_don (j)            
        
        pdvas(131) = hruaao(131,j)                                                   !hrumono(131,j) = hrumono(131,j) +  GwQ_DON (j)              !pdvas(131) = GwQ_DON (j)         
        pdvas(132) = hruaao(132,j)                                                   !hrumono(132,j) = hrumono(132,j) +  shallst_don_decay(j)     !pdvas(132) = shallst_don_decay(j)
        pdvas(133) = hruaao(133,j)                                                   !hrumono(133,j) = hrumono(133,j) +  revap_don(j)             !pdvas(133) = revap_don(j)        
        pdvas(134) = hruaao(134,j)                                                   !hrumono(134,j) = hrumono(134,j) +  gwseep_don(j)            !pdvas(134) = gwseep_don(j)       
        pdvas(135) = hruaao(135,j)/ Real(355.2)                                                   !hrumono(135,j) = hrumono(135,j) +  shallst_don(j)           !pdvas(135) = shallst_don(j)      
        
        pdvas(136) = hruaao(136,j)                                                   !hrumono(136,j) = hrumono(136,j) +  rchrg_doc(j)             !pdvas(136) = rchrg_doc(j)         
        pdvas(137) = hruaao(137,j)                                                   !hrumono(137,j) = hrumono(137,j) +  shallst_doc_decay(j)     !pdvas(137) = shallst_doc_decay(j) 
        pdvas(138) = hruaao(138,j)                                                   !hrumono(138,j) = hrumono(138,j) +  revap_doc(j)             !pdvas(138) = revap_doc(j)         
        pdvas(139) = hruaao(139,j)                                                   !hrumono(139,j) = hrumono(139,j) + gwseep_doc(j)             ! pdvas(139) = gwseep_doc(j)       
        pdvas(140) = hruaao(140,j)/ Real(355.2)                                                   !hrumono(140,j) = hrumono(140,j) + shallst_doc(j)            ! pdvas(140) = shallst_doc(j)      
        
        pdvas(141) = hruaao(141,j)                                                   !hrumono(141,j) = hrumono(141,j) + GwQ_DIC(j)                !pdvas(141) = GwQ_DIC(j)    
        pdvas(142) = hruaao(142,j)                                                   !hrumono(142,j) = hrumono(142,j) + rchrg_dic(j)              !pdvas(142) = rchrg_dic(j)  
        pdvas(143) = hruaao(143,j)                                                   !hrumono(143,j) = hrumono(143,j) + revap_dic(j)              !pdvas(143) = revap_dic(j)  
        pdvas(144) = hruaao(144,j)                                                   !hrumono(144,j) = hrumono(144,j) + gwseep_dic(j)             !pdvas(144) = gwseep_dic(j) 
        pdvas(145) = hruaao(145,j)/ Real(355.2)                                                   !hrumono(145,j) = hrumono(145,j) + shallst_dic(j)            !pdvas(145) = shallst_dic(j)
                                                                                     
        pdvas(146) = hruaao(146,j)/ Real(355.2)                                                   !hrumono(146,j) = hrumono(146,j) + sol_soc(1,j)                  !pdvas(146) = sol_soc(1,j)
        pdvas(147) = hruaao(147,j)/ Real(355.2)                                                   !hrumono(147,j) = hrumono(147,j) + sol_soc(2,j)                  !pdvas(147) = sol_soc(2,j)
        pdvas(148) = hruaao(148,j)/ Real(355.2)                                                   !hrumono(148,j) = hrumono(148,j) + sol_soc(3,j)                  !pdvas(148) = sol_soc(3,j)
        pdvas(149) = hruaao(149,j)/ Real(355.2)                                                   !hrumono(149,j) = hrumono(149,j) + sol_soc(4,j)                  !pdvas(149) = sol_soc(4,j)
        pdvas(150) = hruaao(150,j)/ Real(355.2)                                                  !hrumono(150,j) = hrumono(150,j) + sol_soc(5,j)                  !pdvas(150) = sol_soc(5,j)
        pdvas(151) = hruaao(151,j)/ Real(355.2)                                                  !hrumono(151,j) = hrumono(151,j) + sol_soc(6,j)                  !pdvas(151) = sol_soc(6,j)
        pdvas(152) = hruaao(152,j)/ Real(355.2)                                                   !hrumono(152,j) = hrumono(152,j) + sol_soc(7,j)                  !pdvas(152) = sol_soc(7,j)
        pdvas(153) = hruaao(153,j)/ Real(355.2)                                                   !hrumono(153,j) = hrumono(153,j) + sol_soc(8,j)                  !pdvas(153) = sol_soc(8,j)
 
 
 
        

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

            if (iscen == 1 .and. isproj == 0) then
                write (output_hru_num,1000) cropname, j, subnum(j), hruno(j), sb, nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots)
!1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,f4.1,1x,e10.4,66f10.3,	e10.3,e10.3,8f10.3,3f10.3,66f10.3,8f10.3)              
1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,f4.1,1x,e10.4,66f10.3,	e10.3,e10.3,8e10.3,3e10.3,66e10.3,8e10.3)               !!---------used-----------------

      !    else if (isproj == 1) then
      !    write (output_hru_num,1000) cropname, j, subnum(j), hruno(j),             
   !  &    sb, nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots)
   !       else if (iscen == 1 .and. isproj == 2) then
      !    write (output_hru_num,2000) cropname, j, subnum(j), hruno(j), sb,         
    ! &    nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
            endif
        else
            if (iscen == 1 .and. isproj == 0) then
                write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,  nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        !  else if (isproj == 1) then
        !  write (21,1001) cropname, j, subnum(j), hruno(j),             
   !  &         sb, nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo)
   !       else if (iscen == 1 .and. isproj == 2) then
   !       write (28,1001) cropname, j, subnum(j), hruno(j), sb,         
   !  &    nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
            endif
        end if  !!if (ipdvas(1) > 0) then
        
        end if
      end do

      return

1001  format (a4,i7,1x,a5,a4,i5,1x,i4,1x,f4.1,e10.5,66f10.3,1x,e10.5,1x,e10.5,8e10.3,3f10.3,1x,i4,66f10.3)   !!R682 10/20/21 nbs    
      
      end