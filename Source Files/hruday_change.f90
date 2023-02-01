      subroutine hruday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)       |mm H2O        |amount of water applied to HRU on current
!!                                 |day
!!    auton         |kg N/ha       |amount of nitrogen applied in auto-fert
!!                                 |application
!!    autop         |kg P/ha       |amount of phosphorus applied in auto-fert
!!                                 |application
!!    bactrolp      |# colonies/ha |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# colonies/ha |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# colonies/ha |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# colonies/ha |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bio_ms(:)     |kg/ha         |land cover/crop biomass (dry weight)
!!    cfertn        |kg N/ha       |amount of nitrogen added to soil in
!!                                 |continuous fertilizer operation on day
!!    cfertp        |kg P/ha       |amount of phosphorus added to soil in
!!                                 |continuous fertilizer operation on day
!!    cnday(:)      |none          |curve number for current day, HRU and at
!!                                 |current soil moisture
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepirr(:)    |mm H2O        |amount of water removed from deep aquifer
!!                                 |for irrigation
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    fertn         |kg N/ha       |total amount of nitrogen added to soil in
!!                                 |HRU on day in fertilizer application
!!    fertp         |kg P/ha       |total amount of phosphorus added to soil in
!!                                 |HRU on day in fertilizer application
!!    fixn          |kg N/ha       |amount of nitrogen added to plant biomass
!!                                 |via fixation on the day in HRU
!!    grazn         |kg N/ha       |amount of nitrogen added to soil in grazing
!!                                 |on the day in HRU
!!    grazp         |kg P/ha       |amount of phosphorus added to soil in
!!                                 |grazing on the day in HRU
!!    gw_q(:)       |mm H2O        |groundwater contribution to streamflow from
!!                                 |HRU on current day
!!    gwseep        |mm H2O        |amount of water recharging deep aquifer on
!!                                 |current day
!!    hmntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to nitrate pool in soil profile
!!                                 |on current day in HRU
!!    hmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |organic to labile pool in soil profile
!!                                 |on current day in HRU
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_ra(:)     |MJ/m^2        |solar radiation for the day in HRU
!!    hrugis(:)     |none          |GIS code printed to output files
!!                                 |(output.hru, output.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    iida          |julian date   |current day of simulation
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ihru          |none          |HRU number
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed 
!!                                 |(output.hru)
!!    laiday(:)     |none          |leaf area index for HRU
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                                 |the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    no3gw(:)      |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp        |kg N/ha       |nitrate added to the soil in rainfall
!!    nro(:)        |none          |sequence number of year in rotation
!!    nplnt(:)      |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    percn(:)      |kg N/ha       |NO3-N leached from soil profile during the
!!                                 |day
!!    pet_day       |mm H2O        |potential evapotranspiration for day in HRU
!!    pplnt(:)      |kg P/ha       |plant uptake of phosphorus in HRU for the 
!!                                 |day
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    qdr(:)        |mm H2O        |total amount of water entering main channel
!!                                 |for day from HRU
!!    rchrg(:)      |mm H2O        |amount of water recharging both aquifers on
!!                                 |current day in HRU
!!    revapday      |mm H2O        |amount of water moving from the shallow
!!                                 |aquifer into the soil profile or being taken
!!                                 |up by plant roots in the shallow aquifer
!!    rmn2tl        |kg N/ha       |amount of nitrogen moving from the fresh
!!                                 |organic (residue) to the nitrate(80%) and
!!                                 |active organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    rmp1tl        |kg P/ha       |amount of phosphorus moving from the labile
!!                                 |mineral pool to the active mineral pool in
!!                                 |the soil profile on the current day in the
!!                                 |HRU
!!    rmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |fresh organic (residue) to the labile(80%)
!!                                 |and organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    roctl         |kg P/ha       |amount of phosphorus moving from the active
!!                                 |mineral pool to the stable mineral pool
!!                                 |in the soil profile on the current day in
!!                                 |the HRU
!!    rwntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to stable organic pool in soil
!!                                 |profile on current day in HRU
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha       |amount of organic nitrogen in surface runoff
!!                                 |in HRU for the day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    sepbtm(:)     |mm H2O        |seepage leaving the bottom of the soil
!!                                 |profile on day in HRU
!!    shallirr(:)   |mm H2O        |amount of water removed from shallow aquifer
!!                                 |for irrigation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    snofall       |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow on day in HRU
!!    snomlt        |mm H2O        |amount of water in snow melt for the day in
!!                                 |HRU
!!    sol_cnsw(:)   |mm H2O        |soil water content used to calculate daily
!!                                 |CN value (initial soil wter content for day)
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |at end of any given day
!!    sol_tmp(2,:)  |deg C         |daily average temperature of second soil 
!!                                 |layer
!!    strsn(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |nitrogen stress
!!    strsp(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |phosphorus stress
!!    strstmp(:)    |none          |fraction of potential plant growth achieved
!!                                 |on the day in HRU where the reduction is
!!                                 |caused by temperature stress
!!    strsw(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |water stress
!!    subp(:)       |mm H2O        |precipitation for the day in HRU
!!    surfq(:)      |mm H2O        |surface runoff generated on day in HRU
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
!!    tmn(:)        |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)      |deg C         |average temperature for the day in HRU
!!    tmx(:)        |deg C         |maximum temperature for the day in HRU
!!    usle          |metric tons   |daily soil loss predicted with USLE equation
!!    wdntl         |kg N/ha       |amount of nitrogen lost from nitrate pool
!!                                 |by denitrification in soil profile on
!!                                 |current day in HRU
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
      use carbon_para  
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      IMPLICIT  NONE

      integer :: j, sb, ii, iflag, k, idplant, LIG, NONLIG,icl
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname
      real::    sumorgtc, no3_loss
        
      !!by zhang print out soil water
      !!===============================    
      integer :: ly
      real :: sumwater, sumwfsc, sumdepth, sat, wc, dp
      real :: ssoilwater(100), swfsc(100)
      real :: soilwater(11), wfsc(11), sum_depth(11) !10, 100, 200, 300, 400, ..., 1000 mm
      real :: tot_mass, tot_cmass, tot_nmass, tot_LSC, tot_LSLC, tot_LSLNC, tot_LMC, &
          tot_HSC, tot_HPC, tot_BMC, tot_LSN, tot_LMN, tot_HSN, &
          tot_HPN, tot_BMN, tot_pmass,  tot_no3_nh3, T_CFPltSTR, T_CFPltMET,T_CFOrfSTR, &
          T_CFOrfMET, T_CBurntSTR, T_CBurntMET, T_CFMETS1, &
          T_CFSTRS1, T_CFSTRS2, T_CFS1S2, T_CFS1S3, T_CFS2S1, T_CFS2S3, &
          T_CFS3S1, T_EFS1S2, T_EFMETS1, T_EFSTRS1, T_EFSTRS2, T_EFS1S3, &
          T_EFS2S1, T_EFS2S3, T_EFS3S1, T_IMMMETS1, T_IMMSTRS1, T_IMMSTRS2, &
          T_IMMS1S2, T_IMMS1S3, T_IMMS2S1, T_IMMS2S3, T_IMMS3S1, T_MNRMETS1, &
          T_MNRSTRS1, T_MNRSTRS2, T_MNRS1S2, T_MNRS1S3, T_MNRS2S1, T_MNRS2S3, &
          T_MNRS3S1, T_CO2FMET, T_CO2FSTR_L, T_CO2FSTR_N, T_CO2FS1, T_CO2FS2, &
          T_CO2FS3, T_AMINRL, T_DENITRIF, T_MNR, T_IMM, T_DOC, T_DON, &       
          sum_no3_in, sum_no3_out, sum_nh4_in, sum_nh4_out, &          
          sol_mass, sol_cmass, sol_nmass
     
         
      PARAMETER (NONLIG = 1, LIG = 2)     
      !!by zhang print out soil water
      !!===============================
      

      !!by zhang print out soil water
      !!===============================
      if (cswat == 2 ) then
          !fc = sol_fc(kk,j) + sol_wpmm(kk,j)  ! units mm
          !wc = sol_st(kk,j) + sol_wpmm(kk,j)  ! units mm
          !sat = sol_ul(kk,j) + sol_wpmm(kk,j) ! units mm
          !void = sol_por(kk,j) * (1. - wc / sat)   ! fraction

          soilwater(1) = 0.
          wfsc(1) = 0.
          sum_depth(1) = 10.
          do k = 2, 11
            soilwater(k) = 0.
            wfsc(k) = 0.
            sum_depth(k) = 100. * (k -1)
          end do
          
          wc = sol_st(1,ihru) + sol_wpmm(1,ihru)
          sat = sol_ul(1,ihru) + sol_wpmm(1,ihru)
          soilwater(1) = wc      
          wfsc(1) = sol_por(1,ihru) * (wc / sat)   ! fraction
          
          if (sol_nly(ihru) .ge. 2) then
              do k = 2, 11
                sumwater = 0.
                sumwfsc = 0.
                sumdepth = 0.
                do ly = 2, sol_nly(ihru)
                    if (sol_z(ly-1,ihru) .ge. sum_depth(k-1) .and. sol_z(ly,ihru) .le. sum_depth(k)) then

                              dp = sol_z(ly,ihru) - sol_z(ly-1,ihru)
                              if (dp .gt. 0.) then
                                  wc = sol_st(ly,ihru) + sol_wpmm(ly,ihru)*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = sol_ul(ly,ihru) + sol_wpmm(ly,ihru)*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                    
                    elseif ((sol_z(ly-1,ihru) .gt. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) &
                            .or. (sol_z(ly-1,ihru) .ge. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) &
                            .or. (sol_z(ly-1,ihru) .gt. sum_depth(k-1) .and. sol_z(ly,ihru) .ge. sum_depth(k))) &
                             then
                            if (sol_z(ly-1,ihru) .le. sum_depth(k)) then 
                              dp = sum_depth(k) - sol_z(ly-1,ihru)
                              if (dp .gt. 0.) then
                                  wc = (sol_st(ly,ihru) + sol_wpmm(ly,ihru)) *(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = (sol_ul(ly,ihru) + sol_wpmm(ly,ihru)) *(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                            end if
                    elseif ((sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .lt. sum_depth(k)) & 
                            .or. (sol_z(ly-1,ihru) .le. sum_depth(k-1) .and. sol_z(ly,ihru) .lt. sum_depth(k)) & 
                            .or. (sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .le. sum_depth(k))) &
                             then
                            if (sol_z(ly,ihru) .ge. sum_depth(k-1)) then
                              dp = sol_z(ly,ihru) - sum_depth(k-1)
                              if (dp .gt. 0.) then
                                  wc = (sol_st(ly,ihru) + sol_wpmm(ly,ihru))*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = (sol_ul(ly,ihru) + sol_wpmm(ly,ihru)) *(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                            end if
                    
                    elseif ((sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) & 
                             .or. (sol_z(ly-1,ihru) .le. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) & 
                             .or. (sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .ge. sum_depth(k))) &
                              then 
                              dp = sum_depth(k) - sum_depth(k-1)
                              if (dp .gt. 0.) then
                                  wc = (sol_st(ly,ihru) + sol_wpmm(ly,ihru))*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = (sol_ul(ly,ihru) + sol_wpmm(ly,ihru))*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp    
                              end if
                    end if
                end do !!End lyr
                
                if (sumdepth .gt. 0.) then
                      soilwater(k) = sumwater / sumdepth     
                      wfsc(k) = sumwfsc / sumdepth   ! fraction                
                end if
                
              end do !!end k
              
              
          end if
      end if
      !!by zhang print out soil water
      !!===============================
         
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

      pdvas(1) = subp(j)                            !!SWATCUP =8
      pdvas(2) = snofall
      pdvas(3) = snomlt
      pdvas(4) = aird(j)
      pdvas(5) = pet_day
      pdvas(6) = etday
      pdvas(7) = sol_cnsw(j)                        !sol_cnsw(:)   |mm H2O        |soil water content used to calculate daily CN value (initial soil wter content for day)
      pdvas(8) = sol_sw(j)                          !sol_sw(:)      |mm H2O        |amount of water stored in soil profile on any given day
      pdvas(9) = sepbtm(j)                          !sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for the day in HRU
      pdvas(10) = rchrg(j)                          !rchrg(:)    |mm H2O        |amount of water entering shallow aquifer on previous day in HRU
      pdvas(11) = gwseep                            !gwseep      |mm H2O        |amount of water recharging deep aquifer on current day in HRU
      pdvas(12) = revapday                          !revapday    |mm H2O        |amount of water moving from the shallow aquifer into the soil profile or being taken up by plant roots in the shallow aquifer
      pdvas(13) = shallirr(j)
      pdvas(14) = deepirr(j)
      pdvas(15) = shallst(j)                        !shallst(:)     |mm H2O        |depth of water in shallow aquifer
      pdvas(16) = deepst(j)                         !deepst(:)   |mm H2O        |depth of water in deep aquifer
      pdvas(17) = surfq(j)
      pdvas(18) = qday                                  !qday          |mm H2O        |surface runoff loading to main channel for 
      pdvas(19) = latq(j)                               !!SWATCUP =85 
      pdvas(20) = latq(j) - lpndloss - lwetloss                                            
      pdvas(21) = qtile                             !!SWATCUP =81     !qtile       |mm H2O        |drainage tile flow in soil profile for the day
      pdvas(22) = gw_q(j)                                      !!SWATCUP =84     !gw_qdeep(:) |mm H2O        |groundwater contribution to streamflow from deep aquifer from HRU on current day
      pdvas(23) = gw_qdeep(j)   
      pdvas(24) = tloss                         !tloss         |mm H2O        |amount of water removed from surface runoff
      pdvas(25) = qdr(j)                        !!qdr(:)      |mm H2O        |net water loading from HRU to main channel
      
      pdvas(26) = cnday(j)
      pdvas(27) = tmpav(j)
      pdvas(28) = tmx(j)
      pdvas(29) = tmn(j)
      !pdvas(30) = sol_tmp(2,j)      
      pdvas(30) = hru_ra(j)                     !!hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
      
      pdvas(31) = sedyld(j) / hru_ha(j)
      pdvas(32) = usle_sed(j) !sedgen(j)/ hru_ha(j)
      pdvas(33) = sedorgn(j)                                    !sedorgn(:)   |kg N/ha        |amount of organic nitrogen in surface runoff
      pdvas(34) = sedorgp(j)                                    !sedorgp     |kg P/ha       |amount of organic phosphorus in surface runoff
      pdvas(35) = sedminpa(j) + sedminps(j)                     !sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed to sediment in surface runoff in HRU for day
                                                                !sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed to sediment in surface runoff in HRU for day
      pdvas(36) = fertn
      pdvas(37) = fertp
      pdvas(38) = auton
      pdvas(39) = autop
      pdvas(40) = grazn
      pdvas(41) = grazp
      pdvas(42) = cfertn
      pdvas(43) = cfertp
      pdvas(44) = no3pcp
      pdvas(45) = fixn
      
      pdvas(46) = nplnt(j)                                      !nplnt(:)      |kg N/ha       |plant uptake of nitrogen in HRU for the day
      pdvas(47) = pplnt(j)                                      !pplnt(:)      |kg P/ha       |plant uptake of phosphorus in HRU for the day
      pdvas(48) = wdntl
      
      pdvas(49) = surqno3(j)                                    !surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for the day
      pdvas(50) = latno3(j)                                     !latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for the day
      pdvas(51) = tileno3(j)                                    !SWATCUP =82     !sub_tileno3 |kg N/ha       |NO3 in tile flow on day in subbasin
      pdvas(52) = no3gw(j)                                      !no3gw(:)     |kg N/ha        |nitrate loading to reach in groundwater
      pdvas(53) = percn(j)                                      !percn(:)      |kg N/ha       |NO3-N leached from soil profile during the day   !hmptl 
      
      pdvas(54) = rchrg_n(j)    !rmn2tl 
      pdvas(55) = revapn(j)     !rwntl
      pdvas(56) = gwseepn(j)    !hmntl 
      pdvas(57) = gw_no3loss(j) !rmp1tl 
      pdvas(58) = shallst_n(j)  !rmptl
      pdvas(59) = surqsolp(j)                                   !surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface runoff in HRU for the day    !roctl
      pdvas(60) = vap_tile                                        !!!!    phos due to crack flow (tvap)   !estimate soluble p in tiles due to crack flow    |kg P/ha
      pdvas(61) = minpgw(j)                                     !minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater    !wdntl
      

      pdvas(62) = (1.-strsw(j))                                 !strsw(:)       |none          |fraction of potential plant growth achieved on the day where the reduction is caused by water stress
      pdvas(63) = (1.-strstmp(j))                               !strstmp(:)  |none             |fraction of potential plant growth achieved on the day in HRU where the reduction is caused by temperature stress
      pdvas(64) = (1.-strsn(j))                                 !strsn(:)    |none          |fraction of potential plant growth achieved on the day where the reduction is caused by nitrogen stress
      pdvas(65) = (1.-strsp(j))                                     !!SWATCUP =70
      pdvas(66) = bio_ms(j) / 1000.                                 !!SWATCUP =71
      pdvas(67) = laiday(j)                                         !!SWATCUP =72 
      pdvas(68) = yield(j)/1000.                                             !!SWATCUP =73       !yield       |kg             |yield (dry weight)
      !yield = 0.                                    
      !yield was not set to 0 in harvestop or harvkillop. If not set to 0, it will continue to grow.
      
      pdvas(69) = yieldgrn(j)/1000.
      pdvas(70) = yieldbms(j)/1000.
      pdvas(71) = yieldtbr(j)/1000.
      pdvas(72) = yieldrsd(j)/1000.


      pdvas(73) = bactrop + bactsedp                                !!SWATCUP =74 !bactsedp    |# cfu/m^2     |persistent bacteria transported with sediment in surface runoff
      pdvas(74) = bactrolp + bactsedlp                              !!SWATCUP =75   
      pdvas(75) = wtab(j)                                           !! based on 30 day antecedent climate (mm) (prec,et)                !!SWATCUP =76    
      pdvas(76) = wat_tbl(j)                                        !! based on depth from soil surface (mm): Dmoriasi 4/08/2014     !!SWATCUP =77   
      pdvas(77) = sno_hru(j)                                        !!SWATCUP =78   
      pdvas(78) = sol_rsd(1,j)/1000.                                !!sol_rsd(1,:)|kg/ha         |amount of organic matter in the top soil layer
      pdvas(79) = sol_cov(j)/1000.                                  !!sol_cov(:)  |kg/ha         |amount of residue and biomass on soil surface
      !if(ifor==1)then
      !  sol_cov(j) = sol_rsd(1,j) 
      !else
      !  sol_cov(j) = .8 * bio_ms(j) + sol_rsd(1,j)
      !end if 
      !usle_cfac(j)
  
       pdvas(80) = N2O(j)*1000.                                              !! gN/ha                      !!SWATCUP =87
       pdvas(81) = NO(j)*1000.                                              !! gN/ha                      !!SWATCUP =88 !sol_LSLC(2,j) / 1000.    !
       pdvas(82) = nh4_vol(j)                                                !! kg N/ha   !sol_LSLC(3,j) / 1000.    !   
       pdvas(83) = no3_denit(j)                                             !! kg N/ha    !sol_LSLNC(1,j) / 1000.   ! 
       pdvas(84) = no3_nitr(j)                                              !! kg N/ha   !sol_LSLNC(2,j) / 1000.    ! 
       pdvas(85) = no3_up(j)                                                !! kg N/ha   different from nplnt(j)  which includes fixn  !sol_LSLNC(3,j) / 1000.    !
       pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j)        !! kg N/ha   !sol_LMC(1,j) / 1000.      !       ! 
       pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)        !! kg N/ha  !sol_LMC(2,j) / 1000.     
       pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j)             !! kg N/ha  !sol_LMC(3,j) / 1000.     !
       
       pdvas(89) = solc_no3(j)                                      !!Total NO3 in soil in soil profile kg N/ha 
       pdvas(90) = solc_nh4(j)                                      !!Total NH3 in soil profile kg N/ha   
       pdvas(91) = solc_orgn(j)/1000.                         !!Total orgN in soil profile t N/ha !solc_orgn(j) = solc_orgn(j) + sol_LMN(ly,j) + sol_LSN(ly,j) + sol_HPN(ly,j) + sol_BMN(ly,j) + sol_HSN(ly,j)
       pdvas(92) = solc_orgp(j)/1000.                           !! Total orgP in soil profile t P/ha    !solc_orgp = solc_orgp + sol_fop(ly,j) + sol_orgp(ly,j)
       pdvas(93) = solc_orgc(j)/1000.                           !!  total org C in soil profile  t C/ha     !solc_orgc(j) = solc_orgc(j)+ sol_LSC(ly,j)+sol_LMC(ly,j) +sol_HPC(ly,j) +sol_HSC(ly,j)  +sol_BMC(ly,j)
       pdvas(94) = solc_orgcs(j)/1000.                          !! total t C/ha                             !solc_orgcs(j)= solc_orgcs(j)+ sol_HPC(ly,j)+sol_HSC(ly,j) +sol_BMC(ly,j)
       
       pdvas(95) = sedc_d(j)                                    !! POC in sediment load kg C/ha
       pdvas(96) = surfqc_d(j)                                  !! DOC+DIC in surface runoff kg C/ha
       pdvas(97) = latc_d(j)                                      !! DOC+DIC in lateral flow kg C/ha
       pdvas(98) = percc_d(j)                                   !! DOC+DIC in percolation amount from lowest soil layer to shallow aquifer kg C/ha
       pdvas(99) = NPPC_d(j)                                 !! NPPC      kg C/ha
       pdvas(100) = rspc_dnew(j)                             !  soil profile respC- DIC loss             !! kg C/ha                  rspc_d(j)   
       pdvas(101) = OrgC_Plt2Rsd(j)+ OrgC_Fer(j)            !residue and organic manure inputs of carbon               
      
       pdvas(102) = Sed_RPOC(j)                            ! RPOC from hru to stream(kg/ha) !sol_BMC(1,j)/1000.            ! 
       pdvas(103) = Sed_LPOC(j)                            ! LPOC from hru to stream (kg/ha)    !sol_BMC(2,j) /1000.           !
       pdvas(104) = HRU_RDOC(j)                            ! RDOC from hru to stream (kg/ha)    !sol_BMC(3,j) /1000.           !
       pdvas(105) = HRU_LDOC(j)                            ! LDOC from hru to stream (kg/ha)    !sol_HSC(1,j) /1000.           !
       pdvas(106) = HRU_DIC(j)                             ! DIC from hru to stream(kg/ha)  !sol_HSC(2,j)/1000.            !   
       pdvas(107) = SurQ_DOC(j)                            ! Surface RDOC to stream  (kg/ha)    !sol_HSC(3,j) /1000.           !
       pdvas(108) = LatQT_DOC(j)                           ! Lateral RDOCto stream  (kg/ha) !sol_HPC(1,j) /1000.           !
       pdvas(109) = PerQB_DOC(j)                           ! RDOC percolation amount from lowest soil layer to shallow aquifer  !sol_HPC(2,j)/1000.            !
       pdvas(110) = GwQ_DOC(j)                             ! GW RDOC to stream (kg/ha)  !sol_HPC(3,j) /1000.           !
        
       pdvas(111) = SurQ_DIC(j)                              ! Surface DIC from HRUto stream (kg/ha)    !CMF(1,j)   ! 
       pdvas(112) = LatQT_DIC(j)                            ! Lateral DIC from HRU to stream(kg/ha)     !CMF(2,j)   !
       pdvas(113) = PerQB_DIC(j)                            ! DIC percolation amount from lowest soil layer to shallow aquifer      ! shallst_doc_decay(j)      !WATF(1,j)  !
       pdvas(114) = solc_doc(j)                             !  Total DOC in soil profile (kg/ha)       !WATF(2,j) !                                    
       pdvas(115) = solc_dic(j)                            !  Total DIC in soil profile (kg/ha)    !TEMF(1,j) !
       
       pdvas(116) = sur_tmp(j)      !TEMF(2,j) !
       pdvas(117) = soltmp_50(j)    !OXGF(1,j) !
       pdvas(118) = soltmp_100(j)   !OXGF(2,j)  !
       pdvas(119) = soltmp_150(j)   !TILLF(1,j) !
       pdvas(120) = soltmp_200(j)   !TILLF(2,j) !
       
       pdvas(121) = soltmp_300(j)   !R_LSCTP(1,j)*1000  !
       pdvas(122) = soltmp_500(j)   !R_LSCTP(2,j)*1000  !
       pdvas(123) = soltmp_1000(j)  !R_LMCTP(1,j)*1000  !   
       pdvas(124) = sol_frozday(j)  !R_LMCTP(2,j)*1000  !
       
       
        pdvas(125) = Sed_RPON(j)                               ! RPON from hru to stream(kg/ha) !R_BMCTP(1,j)*1000  !                                     
        pdvas(126) = Sed_LPON(j)                               ! LPON from hru to stream (kg/ha)    !R_BMCTP(2,j)*1000  !
        pdvas(127) = SurQ_DON(j)                              ! Surface RDON to stream  (kg/ha) !R_HSCTP(1,j)*1000  !
        pdvas(128) = LatQT_DON(j)                            ! Lateral RDON to stream  (kg/ha)  !R_HSCTP(2,j)*1000  !
        pdvas(129) = PerQ_DON(sol_nly(j),j)              ! RDON percolation amount from lowest soil layer to shallow aquifer   !R_HPCTP(1,j)*1000  !
        pdvas(130) = rchrg_don(j)                                 ! RDON recharge to aquifer  (kg/ha)  !R_HPCTP(2,j)*1000  ! 
       
        pdvas(131) = GwQ_DON(j)                             ! GW RDON to stream (kg/ha) 
        pdvas(132) = shallst_don_decay(j)                      ! DON decay in shallow aquifer (kg/ha) 
        pdvas(133) = revap_don(j)                                  ! revap DON to soils             (kg/ha) 
        pdvas(134) = gwseep_don(j)                                !! DON seep to deeper aquifer  (kg/ha) 
        pdvas(135) = shallst_don(j)                                  !! state     DON in shallow aquifer  (kg/ha) 
        
        pdvas(136) = rchrg_doc(j)                                  !! RDOC recharge to aquifer  (kg/ha) 
        pdvas(137) = shallst_doc_decay(j)                       ! DOC decay in shallow aquifer (kg/ha)
        pdvas(138) = revap_doc(j)                                   ! revap DOC to soils             (kg/ha) 
        pdvas(139) = gwseep_doc(j)                                 !! DOC seep to deeper aquifer  (kg/ha) 
        pdvas(140) = shallst_doc(j)                             !! state     DOC in shallow aquifer  (kg/ha) 
          
        pdvas(141) = GwQ_DIC(j)                                  ! GW DIC to stream (kg/ha)  
        pdvas(142) = rchrg_dic(j)                                    ! DIC recharge to aquifer  (kg/ha) 
        pdvas(143) = revap_dic(j)                                    ! revap DIC to soils             (kg/ha) 
        pdvas(144) = gwseep_dic(j)                                 !DIC seep to deeper aquifer  (kg/ha) 
        pdvas(145) = shallst_dic(j)        !! state                  DIC in shallow aquifer  (kg/ha)  


      pdvas(146) = sol_soc(1,j)/1000.                          !!SWATCUP =74
      pdvas(147) = sol_soc(2,j)/1000.                          !!SWATCUP =74    
      pdvas(148) = sol_soc(3,j)/1000.                                        !!SWATCUP =75
      pdvas(149) = sol_soc(4,j)/1000.                                        !!SWATCUP =76
      pdvas(150) = sol_soc(5,j)/1000.                                        !!SWATCUP =77
      pdvas(151) = sol_soc(6,j)/1000.                                        !!SWATCUP =78
      pdvas(152) = sol_soc(7,j)/1000.                                        !!SWATCUP =79 
      pdvas(153) = sol_soc(8,j)/1000.                                        !!SWATCUP =80 

      call xmon 
          
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
      
      if(ioutput == 1) then
          !!output basic information
          call sqlite3_set_column( colhru(1), j )
          call sqlite3_set_column( colhru(2), cropname )
          call sqlite3_set_column( colhru(3), nmgt(j) )
          if (icalen == 0) then
            call sqlite3_set_column( colhru(4), iyr )
            call sqlite3_set_column( colhru(5), iida )
          else if (icalen == 1) then
            call sqlite3_set_column( colhru(4), iyr )
            call sqlite3_set_column( colhru(5), i_mo )
            call sqlite3_set_column( colhru(6), icl(iida) )
          end if
      end if
      
      if (iscen == 1 .and. isproj == 0) then        
            if(ioutput == 1) then
                do ii = 1, itots
                    call sqlite3_set_column( colhru(tblhru_num + ii), pdvs(ii))
                end do
            else
                if (icalen == 0) then
                write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb, nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
                end if
            end if
!        if (icalen == 0) write (output_hru_num,1001) cropname, j, subnum(j),        &
!     &      hruno(j), sb, nmgt(j), iida, hru_km(j),                     &
!     &       (pdvs(ii), ii = 1, itots)
 !       if (icalen == 1) write (output_hru_num,1002) cropname, j, subnum(j),        &
   !  &      hruno(j), sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),     &
!     &       (pdvs(ii), ii = 1, itots)
1001  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,66f10.3,f10.3,f10.3,8f10.3,3f10.3,66f10.3,8f10.3) 
1002  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5, 66f10.3,1x,e10.5,1x,e10.5,8e10.3,2f10.3,66f10.3)   
!!    added for binary files 3/25/09 gsm line below and write (33333
	!      if (ia_b == 1) then
	!        write (33333) j, hrugis(j), sb,                                  &
    ! &               nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
	  !    endif
       ! else if (isproj == 1) then
     !   write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     !&          nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
     !   else if (iscen == 1 .and. isproj == 2) then
     !   if(icalen == 0)write (output_hru_num,1000) cropname, j, subnum(j), hruno(j),&
  !   &      sb, nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
     !   if(icalen == 1)write (output_hru_num,1003) cropname, j, subnum(j), hruno(j),&
     !&      sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),               &
     !&      (pdvs(ii), ii = 1, itots), iyr
 1003  format(a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,66f10.3,1x,e10.5,1x,e10.5,8e10.3,f10.3,1x,i4, 66f10.3)     
        end if
      else
        if (iscen == 1 .and. isproj == 0) then        
            if(ioutput == 1) then
                do ii = 1, mhruo
                    call sqlite3_set_column( colhru(tblhru_num + ii), pdvas(ii))
                end do
            else
                if(icalen == 0) then
            write (output_hru_num,1000) cropname, j, subnum(j), hruno(j),&
     &        sb,nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
                end if          
        end if
   !     if(icalen == 1)write (output_hru_num,1003) cropname, j, subnum(j), hruno(j),&
    ! &        sb,nmgt(j), i_mo, icl(iida), iyr, hru_km(j),              &
   !  &        (pdvas(ii), ii = 1, mhruo)
!!    added for binary files 3/25/09 gsm line below and write (33333
	  ! if (ia_b == 1) then
      !       write (33333)  j, hrugis(j), sb,                           &
    ! &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
	 !   endif

      !  else if (isproj == 1) then
      !  write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
    ! &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
    !    else if (iscen == 1 .and. isproj == 2) then
     !   if(icalen == 0)write (output_hru_num,1000) cropname, j, subnum(j), hruno(j),& 
    ! &      sb,nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
    !     if(icalen == 1)write(output_hru_num,1000) cropname, j, subnum(j), hruno(j),& 
    ! &      sb,nmgt(j), i_mo, icl(iida), iyr, hru_km(j),                &
    ! &      (pdvas(ii), ii = 1, mhruo), iyr
        end if
    end if
    
    if(ioutput == 1) then
        call sqlite3_insert_stmt( db, stmthru, colhru )
    end if

      !!add by zhang
      !!output carbon related variables
      !!=================================
      if (cswat == 2) then
          if (j  == 1 ) then
          tot_mass = 0.
          tot_cmass = 0.
          tot_nmass = 0.
          tot_LSC = 0.
          tot_LSLC = 0.
          tot_LSLNC = 0.
          tot_LMC = 0.
          tot_HSC = 0.
          tot_HPC = 0.
          tot_BMC = 0.
          tot_LSN = 0.
          tot_LMN = 0.
          tot_HSN = 0.
          tot_HPN = 0.
          tot_BMN = 0.
          tot_pmass = 0. 
          
          tot_no3_nh3 =0.

          T_CFPltSTR =0.
          T_CFPltMET =0.
          T_CFOrfSTR =0.
          T_CFOrfMET =0.
          T_CBurntSTR=0.
          T_CBurntMET=0.          

          T_CFMETS1 =0.
          T_CFSTRS1 =0.
          T_CFSTRS2 =0.
          T_CFS1S2  =0.
          T_CFS1S3  =0.
          T_CFS2S1  =0.
          T_CFS2S3  =0.
          T_CFS3S1  =0.
          T_EFS1S2  =0.
          T_EFMETS1 =0.
          T_EFSTRS1 =0.
          T_EFSTRS2 =0.
          T_EFS1S3  =0.
          T_EFS2S1  =0.
          T_EFS2S3  =0.
          T_EFS3S1  =0.
          T_IMMMETS1    =0.
          T_IMMSTRS1    =0.
          T_IMMSTRS2    =0.
          T_IMMS1S2 =0.
          T_IMMS1S3    =0.
          T_IMMS2S1 =0.
          T_IMMS2S3 =0.
          T_IMMS3S1 =0.
          T_MNRMETS1    =0.
          T_MNRSTRS1    =0.
          T_MNRSTRS2    =0.
          T_MNRS1S2 =0.
          T_MNRS1S3 =0.
          T_MNRS2S1 =0.
          T_MNRS2S3 =0.
          T_MNRS3S1 =0.
          T_CO2FMET =0.
          T_CO2FSTR_L   =0.
          T_CO2FSTR_N   =0.
          T_CO2FS1  =0.
          T_CO2FS2  =0.
          T_CO2FS3  =0.
          
          T_AMINRL  =0.
          T_DENITRIF=0.  
          T_MNR = 0.
          T_IMM = 0.       
          T_DOC = 0.
          T_DON = 0.
          sum_no3_in = 0.
          sum_no3_out = 0. 
          sum_nh4_in = 0.
          sum_nh4_out = 0.
          
          
          do k=1,sol_nly(j) 
              sol_mass = 0.
              if (k == 1) then
 		        sol_mass = (10) / 1000.* 10000. * sol_bd(k,j)* 1000. * &
     	         (1- sol_rock(k,j) / 100.)            
              else
		        sol_mass = (sol_z(k,j) - sol_z(k-1,j)) / 1000.* 10000. &
     	         * sol_bd(k,j)* 1000. *	(1- sol_rock(k,j) / 100.)
	         end if    
	            
              sol_cmass = 0.
              sol_cmass = sol_LSC(k,j)+sol_LMC(k,j)+sol_HPC(k,j)+sol_HSC(k,j) +sol_BMC(k,j)
              sol_nmass = 0. 
              sol_nmass = sol_LSN(k,j)+sol_LMN(k,j)+sol_HPN(k,j)+sol_HSN(k,j) +sol_BMN(k,j)     
              !write (98,9000) iyr, i, k, j, sol_mass,sol_cmass,             &
              !   sol_nmass,sol_LS(k,j),sol_LM(k,j),                         &
              !   sol_LSC(k,j),sol_LMC(k,j),sol_HSC(k,j),sol_HPC(k,j),       &
              !   sol_BMC(k,j),sol_LSN(k,j),sol_LMN(k,j),sol_HPN(k,j),       &
              !   sol_HSN(k,j),sol_BMN(k,j),sol_no3(k,j),sol_fop(k,j),       &
              !   sol_orgp(k,j),sol_solp(k,j)   

              write (cswat_profile_num,9000)     iyr,                 i,                k,               j,           CFMETS1(k,j),   CFSTRS1(k,j),   CFSTRS2(k,j),    CFS1S2(k,j),     CFS1S3(k,j),     CFS2S1(k,j), & !10
                                         CFS2S3(k,j),       CFS3S1(k,j),        EFS1S2(k,1,j),    EFMETS1(k,1,j),   EFSTRS1(k,1,j), EFSTRS2(k,1,j),  EFS1S3(k,1,j),  EFS2S1(k,1,j),   EFS2S3(k,1,j),   EFS3S1(k,1,j), & !20
                                     IMMMETS1(k,1,j),   IMMSTRS1(k,1,j),      IMMSTRS2(k,1,j),    IMMS1S2(k,1,j),   IMMS1S3(k,1,j), IMMS2S1(k,1,j), IMMS2S3(k,1,j), IMMS3S1(k,1,j), MNRMETS1(k,1,j), MNRSTRS1(k,1,j), & !30
                                     MNRSTRS2(k,1,j),    MNRS1S2(k,1,j),       MNRS1S3(k,1,j),    MNRS2S1(k,1,j),   MNRS2S3(k,1,j), MNRS3S1(k,1,j),   CO2FMET(k,j), CO2FSTR(k,2,j),  CO2FSTR(k,1,j),     CO2FS1(k,j), & !40
                                         CO2FS2(k,j),       CO2FS3(k,j),                                                                                                                                               & !42  
                                        sol_LMC(k,j), CFPltMET(k,j) + CFOrfMET(k,j) - CBurntMET(k,j) - (CO2FMET(k,j) + CFMETS1(k,j)),                                                                                 & !44
                                        sol_LSC(k,j), CFPltSTR(k,j) + CFOrfSTR(k,j) - CBurntSTR(k,j) - (CO2FSTR(k,2,j)+ CO2FSTR(k,1,j) + CFSTRS1(k,j) + CFSTRS2(k,j)),                                                & !46
                                        sol_BMC(k,j), (CFMETS1(k,j) + CFSTRS1(k,j) + CFS2S1(k,j) + CFS3S1(k,j))-1.*(CFS1S2(k,j) + CFS1S3(k,j) + CO2FS1(k,j)),                                                         & !48
                                        sol_HSC(k,j), (CFSTRS2(k,j) + CFS1S2(k,j)) - 1.*(CFS2S1(k,j) + CFS2S3(k,j) + CO2FS2(k,j)),                                                                                     & !50
                                        sol_HPC(k,j), (CFS1S3(k,j) + CFS2S3(k,j)) -1.*(CFS3S1(k,j) + CO2FS3(k,j)),                                                                                                     & !52
                                        sol_LMN(k,j), NFPltMET(k,j) + NFOrfMET(k,j) - NBurntMET(k,j) - EFMETS1(k,1,j),                                                                                                & !54
                                        sol_LSN(k,j), NFPltSTR(k,j) + NFOrfSTR(k,j) - NBurntSTR(k,j) -(EFSTRS1(k,1,j) + EFSTRS2(k,1,j)),                                                                              & !56
                                        sol_BMN(k,j), (EFMETS1(k,1,j) + EFSTRS1(k,1,j) + EFS2S1(k,1,j) + EFS3S1(k,1,j)) - (EFS1S2(k,1,j) + EFS1S3(k,1,j)),                                                             & !58
                                        sol_HSN(k,j), (EFSTRS2(k,1,j) + EFS1S2(k,1,j)) - (EFS2S1(k,1,j) + EFS2S3(k,1,j)),                                                                                              & !60                  
                                        sol_HPN(k,j), (EFS1S3(k,1,j) + EFS2S3(k,1,j)) - EFS3S1(k,1,j),                                                                                                                & !62
                                        sol_no3(k,j),  sol_nh4(k,j), sol_no3(k,j)+sol_nh4(k,j), nh4_immo_ly(k,j), no3_immo_ly(k,j), nh4_min_ly(k,j), sol_latc(k,j),  sol_percc(k,j)                                    !70                  
               
               tot_mass = tot_mass + sol_mass
               tot_cmass = tot_cmass + sol_cmass 
               tot_nmass = tot_nmass + sol_nmass
               tot_LSC = tot_LSC + sol_LSC(k,j)
               
               tot_LSLC = tot_LSLC + sol_LSLC(k,j)
               tot_LSLNC = tot_LSLNC + sol_LSLNC(k,j)

               tot_LMC = tot_LMC + sol_LMC(k,j)
               tot_HSC = tot_HSC + sol_HSC(k,j)
               tot_HPC = tot_HPC + sol_HPC(k,j)
               tot_BMC = tot_BMC + sol_BMC(k,j)

               tot_LSN = tot_LSN + sol_LSN(k,j)
               tot_LMN = tot_LMN + sol_LMN(k,j)
               tot_HSN = tot_HSN + sol_HSN(k,j)
               tot_HPN = tot_HPN + sol_HPN(k,j)
               tot_BMN = tot_BMN + sol_BMN(k,j)
        
               tot_pmass =tot_pmass+ sol_orgp(k,j) + sol_fop(k,j) +  sol_solp(k,j)
              
               
               !tot_no3_nh3 = tot_no3_nh3  + sol_nh4(k,j) !sol_no3(k,j) !+ 


              T_CFPltSTR   = T_CFPltSTR + CFPltSTR(k,j)
              T_CFPltMET   = T_CFPltMET + CFPltMET(k,j)              
              T_CFOrfSTR   = T_CFOrfSTR + CFOrfSTR(k,j)
              T_CFOrfMET   = T_CFOrfMET + CFOrfMET(k,j)              
              T_CBurntSTR   = T_CBurntSTR + CBurntSTR(k,j)
              T_CBurntMET   = T_CBurntMET + CBurntMET(k,j)
                            
              
              T_CFMETS1    = T_CFMETS1+CFMETS1(k,j)
              T_CFSTRS1    = T_CFSTRS1+CFSTRS1(k,j)
              T_CFSTRS2    = T_CFSTRS2+CFSTRS2(k,j)
              T_CFS1S2     = T_CFS1S2+CFS1S2(k,j)
              T_CFS1S3     = T_CFS1S3+CFS1S3(k,j)
              T_CFS2S1     = T_CFS2S1+CFS2S1(k,j)
              T_CFS2S3     = T_CFS2S3+CFS2S3(k,j)
              T_CFS3S1     = T_CFS3S1+CFS3S1(k,j)
              
              T_EFS1S2     = T_EFS1S2+EFS1S2(k,1,j)
              T_EFMETS1    = T_EFMETS1+EFMETS1(k,1,j)
              T_EFSTRS1    = T_EFSTRS1+EFSTRS1(k,1,j)
              T_EFSTRS2    = T_EFSTRS2+EFSTRS2(k,1,j)
              T_EFS1S3     = T_EFS1S3+EFS1S3(k,1,j)
              T_EFS2S1     = T_EFS2S1+EFS2S1(k,1,j)
              T_EFS2S3     = T_EFS2S3+EFS2S3(k,1,j)
              T_EFS3S1     = T_EFS3S1+EFS3S1(k,1,j)
              
              T_IMMMETS1   = T_IMMMETS1+IMMMETS1(k,1,j)
              T_IMMSTRS1   = T_IMMSTRS1+IMMSTRS1(k,1,j)
              T_IMMSTRS2   = T_IMMSTRS2+IMMSTRS2(k,1,j)
              T_IMMS1S2    = T_IMMS1S2+IMMS1S2(k,1,j)
              T_IMMS1S3    = T_IMMS1S3+IMMS1S3(k,1,j)
              T_IMMS2S1    = T_IMMS2S1+IMMS2S1(k,1,j)
              T_IMMS2S3    = T_IMMS2S3+IMMS2S3(k,1,j)
              T_IMMS3S1    = T_IMMS3S1+IMMS3S1(k,1,j)
              
              T_MNRMETS1    =T_MNRMETS1+MNRMETS1(k,1,j)
              T_MNRSTRS1    =T_MNRSTRS1+MNRSTRS1(k,1,j)
              T_MNRSTRS2    =T_MNRSTRS2+MNRSTRS2(k,1,j)
              T_MNRS1S2     =T_MNRS1S2+MNRS1S2(k,1,j)
              T_MNRS1S3     =T_MNRS1S3+MNRS1S3(k,1,j)
              T_MNRS2S1     =T_MNRS2S1+MNRS2S1(k,1,j)
              T_MNRS2S3     =T_MNRS2S3+MNRS2S3(k,1,j)
              T_MNRS3S1     =T_MNRS3S1+MNRS3S1(k,1,j)
              
              T_CO2FMET     =T_CO2FMET+CO2FMET(k,j)
              T_CO2FSTR_L   =T_CO2FSTR_L+CO2FSTR(k,2,j)
              T_CO2FSTR_N   =T_CO2FSTR_N+CO2FSTR(k,1,j)
              T_CO2FS1      =T_CO2FS1+CO2FS1(k,j)
              T_CO2FS2      =T_CO2FS2+CO2FS2(k,j)
              T_CO2FS3      =T_CO2FS3+CO2FS3(k,j)
                         

          end do      

              T_MNR = T_MNR + T_MNRMETS1 + T_MNRSTRS1 + T_MNRSTRS2 + T_MNRS1S2 + T_MNRS1S3 + T_MNRS2S1 + T_MNRS2S3 + T_MNRS3S1
              T_IMM = T_IMM + T_IMMMETS1 + T_IMMSTRS1 + T_IMMSTRS2 + T_IMMS1S2 + T_IMMS1S3 + T_IMMS2S1 + T_IMMS2S3 + T_IMMS3S1
              sum_no3_in =  no3_autof(j) + no3_fert(j) + no3_nitr(j)+ no3_rain(j)
              sum_no3_out = no3_loss_pothole(j)+ absorbed_no3(j)+ no3_immo(j)+ no3_surf(j)+no3_lat(j)+ no3_perc(j)+no3_denit(j)+no3_up(j)
              sum_nh4_in = nh4_min(j)+ nh4_fert(j)+nh4_autof(j)+nh4_rain(j)
              sum_nh4_out = absorbed_nh3(j) + nh4_vol(j) + no3_nitr(j)+ nh4_immo(j)


          write (cswat_daily_num,9001) iyr, i, j, cropname, rsdc_d(j), rsdn_d(j),OrgC_Fer(j),orgn_fert(j),sedyld(j), usle_sed(j),                           & !10
                                        enratio, ccp(j), sedc_d(j), percc_d(j),  latc_d(j),emitc_d(j), grainc_d(j), surfqc_d(j), stoverc_d(j), NPPC_d(j),   & !20
                                        GPPC_d(j), T_CFOrfMET+T_CFOrfSTR,rspc_d(j),tot_mass,tot_cmass,tot_nmass, tot_LSC,tot_LSLC, tot_LSLNC, tot_LMC,      & !30
                                        tot_HSC,tot_HPC,tot_BMC, tot_LSN,tot_LMN,tot_HSN,tot_HPN,tot_BMN, bio_ms(j)*CFB, rwt(j), & !40
                                        !etday,tillage_factor(j), (soilwater(ii), ii = 1, 11), (wfsc(ii), ii = 1, 11),      & !64
                                        etday,tillage_factor(j), T_CFMETS1,T_CFSTRS1,T_CFSTRS2,T_CFS1S2,T_CFS1S3,                                           & !47
                                        T_CFS2S1,T_CFS2S3,T_CFS3S1, T_EFS1S2,T_EFMETS1,T_EFSTRS1,T_EFSTRS2,T_EFS1S3,T_EFS2S1,T_EFS2S3,                      & !57
                                        T_EFS3S1, T_IMMMETS1,T_IMMSTRS1,T_IMMSTRS2,T_IMMS1S2,T_IMMS1S3,T_IMMS2S1, T_IMMS2S3,T_IMMS3S1,T_MNRMETS1,           & !67
                                        T_MNRSTRS1, T_MNRSTRS2, T_MNRS1S2, T_MNRS1S3,T_MNRS2S1,T_MNRS2S3,T_MNRS3S1,T_CO2FMET,T_CO2FSTR_L, T_CO2FSTR_N,        & !77
                                        T_CO2FS1, T_CO2FS2, T_CO2FS3,                                                                                         & !80                  
                                        T_CFPltMET + T_CFOrfMET - T_CBurntMET - 1.*(T_CO2FMET+T_CFMETS1),                                                   & !81
                                        T_CFPltSTR + T_CFOrfSTR - T_CBurntSTR - 1.*(T_CO2FSTR_L+T_CO2FSTR_N+T_CFSTRS1+T_CFSTRS2),                           & !82
                                        (T_CFMETS1+T_CFSTRS1+T_CFS2S1+T_CFS3S1)-1*(T_CFS1S2+T_CFS1S3+T_CO2FS1),                                             & !83
                                        (T_CFSTRS2+T_CFS1S2)-1*(T_CFS2S1+T_CFS2S3+T_CO2FS2),                                                                & !84
                                        (T_CFS1S3+T_CFS2S3)-1*(T_CFS3S1+T_CO2FS3),                                                                          & !85
                                        -1*(T_EFMETS1),                                                                                                     & !86
                                        -1*(T_EFSTRS1+T_EFSTRS2),                                                                                           & !87
                                        (T_EFMETS1+T_EFSTRS1+T_EFS2S1+T_EFS3S1)-1*(T_EFS1S2+T_EFS1S3),                                                      & !88
                                        (T_EFSTRS2+T_EFS1S2)-1*(T_EFS2S1+T_EFS2S3),                                                                         & !89
                                        (T_EFS1S3+T_EFS2S3)-1*(T_EFS3S1),                                                                                   & !90            
                                        nplnt(j),wshd_raino3, sol_rsd(1,j),bio_ms(j),sol_cov(j),usle_cfac(j),no3_loss_pothole(j),                           & !91                              &
                                        no3_up(j),absorbed_no3(j),absorbed_nh3(j), no3_immo(j),nh4_min(j),no3_surf(j),no3_lat(j), no3_perc(j),no3_denit(j), no3_rain(j), & ! 107
                                        no3_nitr(j), nh4_immo(j), immo_err1(j),nh4_vol(j),no3_fert(j), nh4_fert(j),no3_autof(j),nh4_autof(j), solc_no3(j),solc_nh4(j),   & ! 117
                                        T_IMM, T_MNR, sum_no3_in, sum_no3_out, sum_nh4_in, sum_nh4_out, CH4(j), N2O(j), N2O_den(j),NO(j),      & !127
                                        NO_den(j)       !128
         
           
          write (cswat_daily1_num,9002) iyr, i, j,     &
           (CMF(ii,j),ii=1,5),        &     ! 1
           (WATF(ii,j),ii=1,5),    &  
           (TEMF(ii,j),ii=1,5),   &
           (OXGF(ii,j),ii=1,5),   &
          ! (TILLF(ii,j),ii=1,5),    &        !5
           (1+ tillagef(ii,j),ii=1,5),  &
           (R_LSCTP(ii,j),ii=1,5),     &
           (R_LMCTP(ii,j),ii=1,5),     & 
           (R_BMCTP(ii,j),ii=1,5),     &
           (R_HSCTP(ii,j),ii=1,5),     &
           (R_HPCTP(ii,j),ii=1,5),     &    !10
           (sol_LSLC(ii,j),ii=1,5),     &
           (sol_LSLNC(ii,j),ii=1,5),     &
           (sol_LMC(ii,j),ii=1,5),     &
           (sol_BMC(ii,j),ii=1,5),     &   
           (sol_HSC(ii,j),ii=1,5),     &    !!15
           (sol_HPC(ii,j),ii=1,5),     &
           (sol_LSN(ii,j),ii=1,5),     &
           (sol_LMN(ii,j),ii=1,5),     &
           (sol_BMN(ii,j),ii=1,5),     &   
           (sol_HSN(ii,j),ii=1,5),     &    !!20
           (sol_HPN(ii,j),ii=1,5),     & 
           (LSCTA (ii,j),ii=1,5),    &
           (LSLCTA (ii,j),ii=1,5),    &
           (LSLnCTA (ii,j),ii=1,5),    &
           (LMCTA (ii,j),ii=1,5),    &    !!25
           (BMCTA (ii,j),ii=1,5),    &
           (HSCTA (ii,j),ii=1,5),    &
           (HPCTA(ii,j),ii=1,5),    &
           (LSNTA (ii,j),ii=1,5),    &
           (LMNTA (ii,j),ii=1,5),    &      !!30
           (BMNTA (ii,j),ii=1,5) ,    &
           (HSNTA(ii,j),ii=1,5) ,    & 
           (HPNTA(ii,j),ii=1,5),    &
           (LSCTP (ii,j),ii=1,5),    &
           (LSLCTP (ii,j),ii=1,5),    &     !!35
           (LSLnCTP (ii,j),ii=1,5),    &
           (LMCTP(ii,j),ii=1,5),    &
           (BMCTP(ii,j),ii=1,5) ,    &   
           (HSCTP(ii,j),ii=1,5),    &
           (HPCTP(ii,j),ii=1,5) ,    &    !!40
           (LSNTP (ii,j),ii=1,5),    &
           (LMNTP(ii,j),ii=1,5),    &
           (BMNTP (ii,j),ii=1,5),    &
           (HSNTP(ii,j),ii=1,5),    &
           (HPNTP(ii,j),ii=1,5),    &     !!45
            etday,          &              
           (sol_tmp(ii,j),ii=1,5),    &   
           (sol_st(ii,j),ii=1,5)           
          end if  
      end if
      
      !!add by zhang
      !!output carbon realted variables
      !!=================================



     


      return

1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,8e10.3,2f10.3,1x,i4,66f10.3)  
!1001  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,66f10.3,f10.3,f10.3,8f10.3,3f10.3,66f10.3)    
9000  format(i4,i4,i4,i8,66(f16.4))
!9001  format(i4,i4,i8,82(f16.6))
9001  format(i4,i4,i8,a8,163(f16.2))
9002  format(i4,i4,i8,25(f16.10),25(f16.10),55(f16.2),120(f16.10),11(f16.2)) 
9003  format(i4,i4,i4,i8,52(f15.3))
9004  format(i4,i4,i4,70(f15.4))


9007  format(i8,i4,i4,i8,264(f20.3))
9008  format(i8,i4,i4,181(f20.3))


 5109 format (i8,i4,i4,i8,80(f20.3))
 5110 format (i8,i4,i4,108(f20.3)) 
      end
