      subroutine wetlan_Du
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates wetlands     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bw1(:)      |none          |1st shape parameter for wetland surface area
!!                               |equation
!!    bw2(:)      |none          |2nd shape parameter for the wetland surface
!!                               |area equation
!!    chlaw(:)    |none          |chlorophyll-a production coefficient for 
!!                               |wetland
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    ipnd1(:)    |none          |beginning month of 2nd "season" of nutrient
!!                               |settling
!!    ipnd2(:)    |none          |ending month of 2nd "season" of nutrient
!!                               |settling
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater
!!    nsetlw(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlw(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    psetlw(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlw(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel
!!    secciw(:)   |none          |water clarity coefficient for wetland
!!    sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in
!!                               |impoundment after settling for one day
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into wetlands
!!    wet_k(:)    |mm/hr         |hydraulic conductivity of bottom of wetlands
!!    wet_mxvol(:)|m^3 H2O       |volume of water required to fill wetlands to
!!                               |maximum water level
!!    wet_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in wetland at beginning of day
!!    wet_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in wetland at beginning of day
!!    wet_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in wetland at beginning of day
!!    wet_nsed(:) |kg/L          |normal sediment concentration in wetland water
!!    wet_nvol(:) |m^3 H2O       |volume of water needed to
!!                               |fill wetlands to normal water level
!!    wet_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in wetland at beginning of day
!!    wet_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in wetland at beginning of day
!!    wet_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in wetland at
!!                               |beginning of day
!!    wet_sed(:)  |kg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in wetland at beginning of day
!!    wet_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in wetland at beginning of day
!!    wet_vol(:)  |m^3 H2O       |volume of water in wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    twlwet      |mm H2O        |water lost through seepage from wetlands on
!!                               |day in HRU
!!    wet_chla(:) |kg chla       |amount of chlorophyll-a in wetland at end
!!                               |of day
!!    wet_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in wetland at end of day
!!    wet_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in wetland at end of day
!!    wet_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in wetland at end of day
!!    wet_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in wetland at end of day
!!    wet_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in wetland at end of day
!!    wet_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in wetland at
!!                               |end of day
!!    wet_seci(:) |m             |secchi-disk depth in wetland at end of day
!!    wet_sed(:)  |kg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in wetland at end of day
!!    wet_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in wetland at end of day
!!    wet_vol(:)  |m^3 H2O       |volume of water in wetlands
!!    wetev       |m^3 H2O       |evaporation from wetland for day
!!    wetflwi     |m^3 H2O       |volume of water flowing in wetland on day
!!    wetflwo     |m^3 H2O       |volume of water flowing out wetland on day
!!    wetpcp      |m^3 H2O       |precipitation on wetland for day
!!    wetsedc     |metric tons   |net change in sediment in wetland on day
!!    wetsedi     |metric tons   |sediment loading to wetland for day
!!    wetsedo     |metric tons   |sediment loading from wetland for day
!!    wetsep      |m^3 H2O       |seepage from wetland bottom for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    iseas       |none          |nutrient settling rate season
!!    j           |none          |HRU number
!!    nitrok      |none          |fraction of nitrogen in wetland removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in wetland removed by
!!                               |settling
!!    sed         |kg/kg         |sediment concentration in wetland at beginning
!!                               |of day
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in pond water
!!                               |on day
!!    vol         |m^3 H2O       |volume of wetland at beginning of day
!!    wetsa       |ha            |surface area of wetland on current day
!!    xx          |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    use carbon_para    !! added by Du
    use parm
    use parm_subC
    use parm_subE
    use parm_subH
    use parm_rchC
    use parm_rchE
    use parm_control
    use parm_output
    implicit none
      
    integer :: j, iseas
    real :: vol, cnv, sed, wetsa, xx, phosk, nitrok, tpco
    real :: wetsani, wetsili, wetclai, wetsagi, wetlagi
    real :: san, sil, cla, sag, lag, inised, finsed,setsed,remsetsed
    real :: wetsano, wetsilo, wetclao, wetsago, wetlago
    real :: qdayi, latqi
    !! added by Du--------------------------------------------
    real :: lpoc_con,rpoc_con,ldoc_con,rdoc_con,dic_con,dic_bed       !local variables for carbon processes 
    real :: wtmp,depth,f_rdb,kac,kh_DIC,CO2_sat,cbn_bur,solpcon       !local variable for carbon processes
    real :: Ab_con,Ab_UN,Ab_UP,DIN_con,qN_Ab,qP_Ab,fnl_Ab,fll_Ab      !local variables for bottom algae
    real :: fnl_dic,I0_Ab,IH_Ab,sedc,ke_Ab,POMcon,fsl_Ab,kph_Ab,wtmpk !local variables for bottom algae
    !! added by Du--------------------------------------------
    real :: fr_cur, CHLACO, ALGCON, yy
    real, external :: THETA
      
    j = 0
    j = ihru
      

    if (wet_fr(j) > 0.) then
        cnv = 0.
        cnv = hru_ha(j) * 10.               !conversion factor
        
        !! store initial values
        vol = 0.
        sed = 0.
        san = 0.
        sil = 0.
        cla = 0.
        sag = 0.
        lag = 0.
        inised = 0.
        finsed = 0.
        setsed = 0.
        remsetsed = 0.
        
        vol = wet_vol(j)
        sed = wet_sed(j)
        san = wet_san(j)
        sil = wet_sil(j)
        cla = wet_cla(j)
        sag = wet_sag(j)
        lag = wet_lag(j)

        !! calculate water balance for day
        wetsa = 0.
        wetsa = bw1(j) * wet_vol(j) ** bw2(j)

        wetev = 10. * evwet(j) * pet_day * wetsa
        wetsep = wet_k(j) * wetsa * 240.
        wetpcp = subp(j) * wetsa * 10.

        !! calculate water flowing into wetland from HRU
        !fr_cur = wet_fr(j) * ((hru_ha(j) - wetsa) / hru_ha(j))     !!R671 4/5/19 nbs 
        !fr_cur = Max(0.,fr_cur)                                    !!R671 4/5/19 nbs 
        wetflwi = qday + latq(j)
        
        !if (iwetile(j) == 1) then                                  !!R671 4/5/19 nbs                              
        !  wetflwi = wetflwi + qtile                                !!R671 4/5/19 nbs 
        !  qtile = qtile * (1. - fr_cur)                            !!R671 4/5/19 nbs 
        !end if                                                     !!R671 4/5/19 nbs 
        !if (iwetgw(j) == 1) then                                   !!R671 4/5/19 nbs 
        !  wetflwi = wetflwi + gw_q(j)                              !!R671 4/5/19 nbs 
        !  gw_q(j) = gw_q(j) * (1. - fr_cur)                        !!R671 4/5/19 nbs 
        !end if                                                     !!R671 4/5/19 nbs 
        
        wetflwi = wetflwi * 10. * (hru_ha(j) * wet_fr(j) - wetsa)
        qdayi = qday
        latqi = latq(j)
        qday = qday * (1. - wet_fr(j))
        latq(j) = latq(j) * (1. - wet_fr(j))
        wetloss = qdayi - qday
        lwetloss = latqi - latq(j)

        qdr(j) = qdr(j) - wetloss - lwetloss
!       qdr(j) = qdr(j) - qdr(j) * wet_fr(j)
       
        !! sediment loading to wetland from HRU
        wetsedi = sedyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))

        wetsani = sanyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
        wetsili = silyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
        wetclai = clayld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
        wetsagi = sagyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))
        wetlagi = lagyld(j) * (wet_fr(j) - (wetsa / hru_ha(j)))

        sedyld(j) = sedyld(j) - sedyld(j) * wet_fr(j)
        sanyld(j) = sanyld(j) - sanyld(j) * wet_fr(j)
        silyld(j) = silyld(j) - silyld(j) * wet_fr(j)
        clayld(j) = clayld(j) - clayld(j) * wet_fr(j)
        sagyld(j) = sagyld(j) - sagyld(j) * wet_fr(j)
        lagyld(j) = lagyld(j) - lagyld(j) * wet_fr(j)

        !! compute nitrogen and phosphorus levels in wetland at beginning
        !! of day
        !! equation 29.1.1 in SWAT manual
        xx = 0.
        xx = wet_fr(j) * hru_ha(j)
        wet_solp(j) = wet_solp(j) + (surqsolp(j) + sedminpa(j)) * xx
        wet_psed(j) = wet_psed(j) + sedminps(j) * xx
        wet_orgp(j) = wet_orgp(j) + sedorgp(j) * xx
        wet_solpg(j) = wet_solpg(j) + minpgw(j) * xx
        wet_orgn(j) = wet_orgn(j) + sedorgn(j) * xx
        wet_no3(j) = wet_no3(j) + surqno3(j) * xx
        wet_no3s(j) = wet_no3s(j) + latno3(j) * xx
        wet_no3g(j) = wet_no3g(j) + no3gw(j) * xx
        !--add carbon amount from HRU to wetland (kg), added by Du--------------!
        if (cswat == 2 ) then
           cbn_wet(j)%LPOC=cbn_wet(j)%LPOC+Sed_LPOC(j)*xx   !(kg C)
           cbn_wet(j)%RPOC=cbn_wet(j)%RPOC+Sed_RPOC(j)*xx   !(kg C)
           cbn_wet(j)%RDOC=cbn_wet(j)%RDOC+HRU_RDOC(j)*xx   !(kg C)
           cbn_wet(j)%DIC=cbn_wet(j)%DIC+HRU_DIC(j)*xx      !(kg C)
        endif
        !--add carbon amount from HRU to wetland, added by Du-------------------! 
        !! remove nutrients entering wetlands from HRU loadings
        xx = 0.
        xx = 1. - wet_fr(j)
        sedorgn(j) = sedorgn(j) * xx
        surqno3(j) = surqno3(j) * xx
        latno3(j) = latno3(j) * xx
        no3gw(j) = no3gw(j) * xx
        sedorgp(j) = sedorgp(j) * xx
        sedminpa(j) = sedminpa(j) * xx
        sedminps(j) = sedminps(j) * xx
        surqsolp(j) = surqsolp(j) * xx
        minpgw(j) = minpgw(j) * xx
        !-------remove carbon amoount entering wetlands from HRU loadings (kg/ha),added by Du!----------
        if (cswat == 2 ) then
           Sed_LPOC(j)=Sed_LPOC(j)*xx   !(kg/ha)
           Sed_RPOC(j)=Sed_RPOC(j)*xx   !(kg/ha)
           HRU_RDOC(j)=HRU_RDOC(j)*xx   !(kg/ha)
           HRU_DIC(j)=HRU_DIC(j)*xx     !(kg/ha)
        endif
        !-----------remove carbon amoount entering wetlands from HRU loadings (kg/ha),added by Du! ------------

        !! new water volume for day 
        wet_vol(j) = wet_vol(j) - wetsep - wetev + wetpcp + wetflwi


        if (wet_vol(j) < 0.001) then

            !! check for volume deficit in wetland
            !! reduce seepage so that the wetland volume is zero
            wetsep = wetsep + wet_vol(j)
            wet_vol(j) = 0.
            !! if seepage is less than the volume deficit, take the 
            !! remainder from evaporation
            if (wetsep < 0.) then
                wetev = wetev + wetsep
                wetsep = 0.
            end if
            wet_sed(j) = 0.

            wet_san(j) = 0.
            wet_sil(j) = 0.
            wet_cla(j) = 0.
            wet_sag(j) = 0.
            wet_lag(j) = 0.

            wet_solp(j) = 0.
            wet_psed(j) = 0.
            wet_orgp(j) = 0.
            wet_solpg(j) = 0.
            wet_orgn(j) = 0.
            wet_no3(j) = 0.
            wet_no3s(j) = 0.
            wet_no3g(j) = 0.
            wet_chla(j) = 0.
            wet_seci(j) = 0.
            !---For carbon module,added by Du-------------------------------------------------!
            if (cswat == 2 ) then
              cbn_wet(j)%LPOC=0.
              cbn_wet(j)%RPOC=0.
              cbn_wet(j)%LDOC=0.
              cbn_wet(j)%RDOC=0.
              cbn_wet(j)%DIC=0.
            endif
            !---For carbon module,added by Du---------------------------------------------------!
        else
 
            !! compute new sediment concentration
            wet_sed(j) = (sed * vol + wetsedi) / wet_vol(j)

            wet_san(j) = (san * vol + wetsani) / wet_vol(j)
            wet_sil(j) = (sil * vol + wetsili) / wet_vol(j)
            wet_cla(j) = (cla * vol + wetclai) / wet_vol(j)
            wet_sag(j) = (sag * vol + wetsagi) / wet_vol(j)
            wet_lag(j) = (lag * vol + wetlagi) / wet_vol(j)

            !! compute outflow if wetland water volume > 0
            if (wet_vol(j) <= wet_nvol(j)) then
                wetflwo = 0.
            else
                if (wet_vol(j) <= wet_mxvol(j)) then
                    wetflwo = (wet_vol(j) - wet_nvol(j)) / 10.
                    wet_vol(j) = wet_vol(j) - wetflwo
                else
                    wetflwo = wet_vol(j) - wet_mxvol(j)
                    wet_vol(j) = wet_mxvol(j)
                end if
            end if
            qday= qday + wetflwo / cnv
            qdr(j) = qdr(j) + wetflwo / cnv

            !! compute sediment settling
	        if (sed_stl(j) < 1.e-6) sed_stl(j) = 0.0
            inised = wet_sed(j)
            if (wet_sed(j) > wet_nsed(j)) then
                wet_sed(j) = (wet_sed(j) - wet_nsed(j)) * sed_stl(j) +  wet_nsed(j)
            end if
            finsed = wet_sed(j)
	        setsed = inised - finsed
            setsed = Max(0.,setsed)

	        if (wet_lag(j) >= setsed) then
	            wet_lag(j) = wet_lag(j) - setsed
	            remsetsed = 0.
	        else
	            remsetsed = setsed - wet_lag(j)
	            wet_lag(j) = 0.
	            if (wet_san(j) >= remsetsed) then
	                wet_san(j) = wet_san(j) - remsetsed
	                remsetsed = 0.
	            else
	                remsetsed = remsetsed - wet_san(j)
	                wet_san(j) = 0.
                    if (wet_sag(j) >= remsetsed) then
	                    wet_sag(j) = wet_sag(j) - remsetsed
	                    remsetsed = 0.
	                else
	                    remsetsed = remsetsed - wet_sag(j)
	                    wet_sag(j) = 0.
                        if (wet_sil(j) >= remsetsed) then
  	                        wet_sil(j) = wet_sil(j) - remsetsed
	                        remsetsed = 0.
	                    else
	                        remsetsed = remsetsed - wet_sil(j)
	                        wet_sil(j) = 0.
                            if (wet_cla(j) >= remsetsed) then
	                            wet_cla(j) = wet_cla(j) - remsetsed
	                            remsetsed = 0.
	                        else
	                            remsetsed = remsetsed - wet_cla(j)
	                            wet_cla(j) = 0.
	                        end if
                        end if
	                end if
	            end if
	        end if

            !! compute sediment leaving wetland
            wetsedo = wet_sed(j) * wetflwo

            wetsano = wet_san(j) * wetflwo
            wetsilo = wet_sil(j) * wetflwo
            wetclao = wet_cla(j) * wetflwo
            wetsago = wet_sag(j) * wetflwo
            wetlago = wet_lag(j) * wetflwo

            sedyld(j) = sedyld(j) + wetsedo

            sanyld(j) = sanyld(j) + wetsano
            silyld(j) = silyld(j) + wetsilo
            clayld(j) = clayld(j) + wetclao
            sagyld(j) = sagyld(j) + wetsago
            lagyld(j) = lagyld(j) + wetlago

            !! net change in amount of sediment in wetland for day
            wetsedc = vol * sed + wetsedi - wetsedo - wet_sed(j) *  wet_vol(j)
            !! determine settling rate for nutrients
            !! part of equation 29.1.3 in SWAT manual
            if (i_mo >= ipnd1(j) .and. i_mo <= ipnd2(j)) then
                iseas = 1
            else
                iseas = 2
            endif
            phosk = 0.
            nitrok = 0.
            phosk = psetlw(iseas,j) * wetsa * 10000. / wet_vol(j)
            phosk = Min(phosk, 1.)
            nitrok = nsetlw(iseas,j) * wetsa * 10000. / wet_vol(j)
            nitrok = Min(nitrok, 1.)

            !! remove nutrients by settling
            !! other part of equation 29.1.3 in SWAT manual
            wet_solp(j) = wet_solp(j) * (1. - phosk)
            wet_psed(j) = wet_psed(j) * (1. - phosk)
            wet_orgp(j) = wet_orgp(j) * (1. - phosk)
            wet_solpg(j) = wet_solpg(j) * (1. - phosk)
            wet_orgn(j) = wet_orgn(j) * (1. - nitrok)
            wet_no3(j) = wet_no3(j) * (1. - nitrok)
            wet_no3s(j) = wet_no3s(j) * (1. - nitrok)
            wet_no3g(j) = wet_no3g(j) * (1. - nitrok)

            if (wet_vol(j) < 1.e-6) wet_vol(j) = 0.0
            if (wet_orgn(j) < 1.e-6) wet_orgn(j) = 0.0
            if (wet_no3(j) < 1.e-6) wet_no3(j) = 0.0
            if (wet_no3s(j) < 1.e-6) wet_no3s(j) = 0.0
            if (wet_no3g(j) < 1.e-6) wet_no3g(j) = 0.0
            if (wet_orgp(j) < 1.e-6) wet_orgp(j) = 0.0
            if (wet_psed(j) < 1.e-6) wet_psed(j) = 0.0
            if (wet_solp(j) < 1.e-6) wet_solp(j) = 0.0
            if (wet_solpg(j) < 1.e-6) wet_solpg(j) = 0.0

            tpco = 0.
            tpco = 1.e+6 * (wet_solp(j) + wet_orgp(j) + wet_psed(j) + wet_solpg(j)) / (wet_vol(j) + wetflwo)
            chlaco = 0.
            wet_chla(j) = 0.
            wet_seci(j) = 0.
            if (tpco > 1.e-4) then
                !! equation 29.1.6 in SWAT manual
                chlaco = chlaw(j) * 0.551 * (tpco**0.76)
                wet_chla(j) = chlaco * (wet_vol(j) + wetflwo) * 1.e-6
            endif
            if (chlaco > 1.e-4) then
                !! equation 29.1.8 in SWAT manual
                wet_seci(j) = secciw(j) * 6.35 * (chlaco**(-0.473))
            endif
            !-----For calculating bottom algea and carbon processes,added by Du----////////////////////////!
            if (cswat == 2 ) then

               if (i == 1 .and. curyr == 1) then
                    Ab_wet(j)%Ab = 100.                    
                    Ab_wet(j)%INb = Ab_wet(j)%Ab * Ab_wetpara(j)%qoN
                    Ab_wet(j)%IPb = Ab_wet(j)%Ab * Ab_wetpara(j)%qoP                    
                end if

                wtmp    = 0.
                wtmp    = 5.0 + 0.75 * tmpav(j) ! calculating water temperature
                if (wtmp <= 0.) wtmp    = 0.1
                depth   = 0.
                depth   = wet_vol(j) / (wetsa * 10000.0) !calculating the depth (m)
                !Caculate initial carbon concentration in the wetland
                rpoc_con    = 0.
                rpoc_con    = cbn_wet(j)%RPOC / wet_vol(j) * 1000.0
                lpoc_con    = 0.
                lpoc_con    = cbn_wet(j)%LPOC / wet_vol(j) * 1000.0
                rdoc_con    = 0.
                rdoc_con    = cbn_wet(j)%RDOC / wet_vol(j) * 1000.0
                ldoc_con    = 0.
                ldoc_con    = cbn_wet(j)%LDOC / wet_vol(j) * 1000.0
                dic_con     = 0.
                dic_con     = cbn_wet(j)%DIC / wet_vol(j) * 1000.0
                !--calculating bottom algae processes in the wetland-----------!
                Ab_con   = 0.
                Ab_con   = Ab_wet(j)%Ab
                !bottom algea death procese
                Ab_wet(j)%death  = 0.
                Ab_wet(j)%death  = Ab_con * Theta((Ab_wetpara(j)%kdb),1.04,wtmp) ! bottom algea death biomass (g/m2/day)
                
                !----bottom algae photosynthesis/growth process
                !For calculating nutrient limitation factor
                qN_Ab    = Ab_wet(j)%INb / Ab_con  !the cell quotas of N (mgN/gD)
                qP_Ab    = Ab_wet(j)%IPb / Ab_con  !the cell quotas of P (mgP/gD)
                fnl_dic  = 0.
                fnl_dic  = Ab_wetpara(j)%fdic*dic_con !H2CO3* and HCO3- concentration (mg/L)
                fnl_Ab   = 0.
                fnl_Ab   = min((1. - Ab_wetpara(j)%qoN / qN_Ab),(1-Ab_wetpara(j)%qoP/qP_Ab),(fnl_dic/(fnl_dic+Ab_wetpara(j)%kscb)))
                if (fnl_Ab < 0.) fnl_Ab  = 0.
                !kinetics for intracellular N and P process in bottom algae
                Ab_UN    = 0.     !uptake rates for N in bottom algae (mgN/m2/day)
                Ab_UP    = 0.     !uptake rates for P in bottom algae (mgP/m2/day)
                DIN_con  = 0.
                DIN_con  = (wet_no3(j) + wet_no3s(j) + wet_no3g(j)) / wet_vol(j) * 1000.0  !calculate DIN concentration (mgN/L)
                Ab_UN    = Ab_wetpara(j)%pmN * (DIN_con / (DIN_con+Ab_wetpara(j)%Ksnb))		&
                            * (Ab_wetpara(j)%KqN / (Ab_wetpara(j)%KqN + (qN_Ab-Ab_wetpara(j)%qoN))) * Ab_con  !calculating N uptakes in bottom algae
                if (Ab_UN < 0.) Ab_UN    = 0.
                solpcon  = 0.
                solpcon  = (wet_solp(j)+wet_solpg(j))/wet_vol(j)*1000.0           !calculate DIP concentration (mgP/L)
                Ab_UP    = Ab_wetpara(j)%pmP * (solpcon / (solpcon + Ab_wetpara(j)%Kspb))		&   
                            * (Ab_wetpara(j)%KqP / (Ab_wetpara(j)%KqP + (qP_Ab-Ab_wetpara(j)%qoP))) * Ab_con !calculating P uptakes in bottom algae
                if (Ab_UP < 0.) Ab_UP    = 0.
                Ab_wet(j)%INb    = Ab_wet(j)%INb + Ab_UN - Ab_wet(j)%death * qN_Ab - qN_Ab * Theta(Ab_wetpara(j)%kexb,1.04,wtmp) * Ab_con    !intracellular N mass balance
                if (Ab_wet(j)%INb<0.) Ab_wet(j)%INb  = 0.
                Ab_wet(j)%IPb    = Ab_wet(j)%IPb + Ab_UP - Ab_wet(j)%death * qP_Ab-qP_Ab*Theta(Ab_wetpara(j)%kexb,1.04,wtmp) * Ab_con    !intracellular P mass balance
                if (Ab_wet(j)%IPb < 0.) Ab_wet(j)%IPb    = 0.
                !For calculating light limation factor
                I0_Ab    = 0.
                I0_Ab    = hru_ra(j)* tfact !0.47    !photosynthetically-active solar radiation reaching water surface
                POMcon   = (lpoc_con + rpoc_con) / 0.58 !Convert POC to POM concentration
                sedc     = 0.
                sedc     = wet_sed(j) * 1.e6   ! TSS concentration (mg/L)
                algcon   = 0.
                algcon   = chlaco/ai0   !!floating algae concentration (mg/L) 
                ke_Ab    = Ab_wetpara(j)%keb + Ab_wetpara(j)%kiss * sedc + Ab_wetpara(j)%kpom             &
                            * POMcon + Ab_wetpara(j)%kap * algcon + Ab_wetpara(j)%kapn                        &
                            * algcon**(.66667) + Ab_wetpara(j)%kAb * Ab_con / depth       !the light extinction coefficient
                IH_Ab   = 0.
                IH_Ab   = I0_Ab * exp(-ke_Ab * depth)          !the quantity of bottom light
                fll_Ab  = 0.
                select case (Ab_wetpara(j)%light)     ! caculating light limitation factor using different light models
                    case (1)
                        !! Half-Saturation Light Model 
                        fll_Ab  = IH_Ab / (Ab_wetpara(j)%klb + IH_Ab)
                    case (2)
                        !! Smith’s Function for light model 
                        fll_Ab  = IH_Ab / (Ab_wetpara(j)%klb**2 + IH_Ab**2)**(0.5)
                    case (3)
                        !! Steele’s Equation for light model
                        fll_Ab  = (IH_Ab / Ab_wetpara(j)%klb) * exp(1. + IH_Ab/Ab_wetpara(j)%klb)
                end select

                kph_Ab     = 0.
                kph_Ab     = Theta(Ab_wetpara(j)%kph,1.04,wtmp)       !temperature correction for photosynthesis rate
                Ab_wet(j)%photo    = 0.
                if (Ab_wetpara(j)%ipho == 0) then !zero order growth model
                    Ab_wet(j)%photo = kph_Ab*fll_Ab*fnl_Ab                     
                else    !first order growth model
                    fsl_Ab  = 0.
                    fsl_Ab  = max(1. - Ab_con / Ab_wetpara(j)%Abmax,0.) ! Space limitation factor
                    Ab_wet(j)%photo = kph_Ab * fll_Ab * fnl_Ab * fsl_Ab * Ab_con
                endif
                !bottom algea respiration process
                Ab_wet(j)%resp = 0.
                Ab_wet(j)%resp = Ab_wetpara(j)%krb1 * Ab_con +	Ab_wetpara(j)%krb2 * Ab_wet(j)%photo
                !Bottom algae biomass mass balance
                Ab_wet(j)%Ab    = Ab_wet(j)%Ab + Ab_wet(j)%photo - Ab_wet(j)%resp - Ab_wet(j)%death
                if (Ab_wet(j)%Ab < 0.) Ab_wet(j)%Ab = 0.000001
                !--Finish calculating bottom algae processes in the wetland-------!
                !---calculating carbon species transformations----!
                cbn_rec%Ab_cbn    = 0.
                cbn_rec%Ab_cbn    = cbn_wetpara(j)%rca * Ab_wet(j)%death / depth    !bottom algae death to total organic carbon
                !LPOC's reaction pathways
                cbn_rec%Ab_LP     = 0.
                cbn_rec%Ab_LP     = cbn_wetpara(j)%f_lpb * cbn_rec%Ab_cbn ! bottom algae to LPOC 
                cbn_rec%LP_set    = 0.
                cbn_rec%LP_set    = cbn_wetpara(j)%sv_lp / depth * lpoc_con                !LPOC settling to bed
                cbn_rec%LP_LD     = 0.
                cbn_rec%LP_LD     = Theta((cbn_wetpara(j)%klp),1.047,wtmp) * lpoc_con     !LPOC dissolution to LDOC
                cbn_rec%LP_DIC    = 0.
                cbn_rec%LP_DIC    = Theta((cbn_wetpara(j)%kd_lp),1.047,wtmp) * lpoc_con  !LPOC decay to DIC
                cbn_rec%LR_POC    = 0.
                cbn_rec%LR_POC    = Theta((cbn_wetpara(j)%klrp),1.047,wtmp) * lpoc_con  ! LPOC decay to RPOC
                xx    = 0.
                xx    = lpoc_con
                lpoc_con  = lpoc_con + cbn_rec%Ab_LP - cbn_rec%LP_set	-cbn_rec%LP_LD - cbn_rec%LP_DIC - cbn_rec%LR_POC  !update LPOC concentration for transformations  
                if (lpoc_con < 0.) then
                    lpoc_con  = 0.
                    !correct LPOC settling amount using mass balance equation if settling is too much
                    cbn_rec%LP_set    = xx + cbn_rec%Ab_LP - cbn_rec%LP_LD -cbn_rec%LP_DIC - cbn_rec%LR_POC
                endif
                !RPOC reaction pathway   
                cbn_rec%Ab_RP     = 0.
                cbn_rec%Ab_RP     = cbn_wetpara(j)%f_rpb * cbn_rec%Ab_cbn    ! bottom algae to RPOC        
                cbn_rec%RP_LD     = 0. 
                cbn_rec%RP_LD     = Theta((cbn_wetpara(j)%krp),1.047,wtmp)*rpoc_con      !RPOC dissolution to LDOC
                cbn_rec%RP_DIC    = 0.
                cbn_rec%RP_DIC    = Theta((cbn_wetpara(j)%kd_rp),1.047,wtmp)*rpoc_con   !RPOC decay to DIC
                cbn_rec%RP_set    = 0.
                cbn_rec%RP_set    = cbn_wetpara(j)%sv_rp/depth*rpoc_con                !RPOC settling to bed
                xx    = 0.
                xx    = rpoc_con
                rpoc_con  = rpoc_con + cbn_rec%Ab_RP - cbn_rec%RP_set	- cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC   !update RPOC concentration for transformations  
                if (rpoc_con < 0.) then
                    rpoc_con  = 0.
                    !correct RPOC settling amount using mass balance equation if settling is too much
                     cbn_rec%RP_set    = xx + cbn_rec%Ab_RP - cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC
                endif
                !LDOC reaction pathways
                cbn_rec%Ab_LD     = 0.
                cbn_rec%Ab_LD     = cbn_wetpara(j)%f_ldb * cbn_rec%Ab_cbn   ! bottom algae death to LDOC
                cbn_rec%LD_DIC    = 0.
                cbn_rec%LD_DIC    = Theta((cbn_wetpara(j)%kld),1.047,wtmp) * ldoc_con  !LDOC decay to DIC
                cbn_rec%LR_DOC    = 0.
                cbn_rec%LR_DOC    = Theta((cbn_wetpara(j)%klrd),1.047,wtmp) * ldoc_con      ! LDOC decay to RDOC
                cbn_rec%LD_NO3    = 0.
                cbn_rec%LD_NO3    = DIN_con * (15.0/14.0) * Theta((cbn_wetpara(j)%kdnit),1.045,wtmp) !LDOC consumbed by NO3 denitrification    
                ldoc_con          = ldoc_con + cbn_rec%Ab_LD + cbn_rec%LP_LD + cbn_rec%RP_LD - cbn_rec%LR_DOC - cbn_rec%LD_DIC - cbn_rec%LD_NO3  !update LDOC concentration for transformations  
                if (ldoc_con < 0.) ldoc_con = 0.      
                !RDOC reation pathways
                f_rdb             = 1. - cbn_wetpara(j)%f_lpb - cbn_wetpara(j)%f_rpb - cbn_wetpara(j)%f_ldb  !fraction of bottom algae death to RDOC
                cbn_rec%Ab_RD     = 0.
                cbn_rec%Ab_RD     = f_rdb*cbn_rec%Ab_cbn ! bottom algae to RPOC
                cbn_rec%RD_DIC    = 0.
                cbn_rec%RD_DIC    = Theta((cbn_wetpara(j)%krd),1.047,wtmp)*rdoc_con      !RDOC decay to DIC
                rdoc_con          = rdoc_con + cbn_rec%Ab_RD + cbn_rec%LR_DOC	- cbn_rec%RD_DIC  !update RDOC concentration for transformations 
                if (rdoc_con < 0.) rdoc_con = 0.
                !DIC reaction pathways
                cbn_rec%Atm_DIC   = 0.
                kac               = Theta(rk2(hru_sub(j)),1.024,wtmp)*0.923           !CO2 reaeration rate
                wtmpk             = wtmp + 273.15  !convet the unit of water temp to Kelvin
                kh_DIC            = 10.0**((2385.73/wtmpk) + 0.0152642 * wtmpk - 14.0184)     !Henry's constant [mol/L/atm] 
                if(ico2==1) cbn_wetpara(j)%p_co2   = co2con(iyr)    
                !CO2_sat=12.0*kh_DIC*cbn_wetpara(j)%p_co2/1000.0           !CO2 saturation (unit converting to mg/L)
                !updated
                CO2_sat           = kh_DIC * (cbn_wetpara(j)%p_co2 / 1000000) * 1000.0 * 12.0     !CO2 saturation (mol/L unit converting to mgC/L)          
          
                cbn_rec%Atm_DIC   = kac * (CO2_sat-cbn_wetpara(j)%f_co2 * dic_con)   !Atmospheric CO2 reaeration 
                cbn_rec%Ab_DIC    = 0.
                cbn_rec%Ab_DIC    = cbn_wetpara(j)%rca * Ab_wet(j)%resp/depth    !bottom algae respiration to DIC  
                cbn_rec%DIC_Ab    = 0.
                cbn_rec%DIC_Ab    = cbn_wetpara(j)%rca * Ab_wet(j)%photo/depth    !DIC consumbed by bottom algae photosynthesis
                !first order sediment diagenesis model-from W2 model
                scbn_wet(j)%scbn  = scbn_wet(j)%scbn + cbn_rec%RP_set	+ cbn_rec%LP_set !add RPOC and LPOC settling from water column     
                dic_bed           = 0.
                dic_bed           = scbn_wet(j)%scbn * scbn_wetpara(j)%ksed   !DIC release from bed sediment compartment 
                cbn_bur           = 0.
                cbn_bur           = scbn_wet(j)%scbn * scbn_wetpara(j)%kbur           !sediment burial amount 
                scbn_wet(j)%scbn  = scbn_wet(j)%scbn - dic_bed - cbn_bur   !update sediment carbon amount after loss
                if (scbn_wet(j)%scbn<0.) scbn_wet(j)%scbn = 0.
                cbn_rec%bed_DIC   = dic_bed           !DIC release from bed sediment         
                dic_con           = dic_con + cbn_rec%Atm_DIC + cbn_rec%LP_DIC + cbn_rec%RP_DIC + cbn_rec%LD_DIC + cbn_rec%RD_DIC             &!transformations from organic carbon pools
                                    + cbn_rec%Ab_DIC - cbn_rec%DIC_Ab + cbn_rec%bed_DIC   !update DIC concentration change after transformation             
                if (dic_con<0.) dic_con   = 0.  !Finish calculating carbon tranformations among different species
                !calculating carbon amount leaving wetland (kg/ha) 
                Sed_LPOC(j)  = Sed_LPOC(j) + (lpoc_con * wetflwo * 1000.0) / hru_ha(j)
                Sed_RPOC(j)  = Sed_RPOC(j) + (rpoc_con * wetflwo * 1000.0) / hru_ha(j)
                HRU_LDOC(j)  = HRU_LDOC(j) + (ldoc_con * wetflwo * 1000.0) / hru_ha(j)
                HRU_RDOC(j)  = HRU_RDOC(j) + (rdoc_con * wetflwo * 1000.0) / hru_ha(j)
                HRU_DIC(j)   = HRU_DIC(j) + (dic_con * wetflwo * 1000.0) / hru_ha(j)
                !update carbon amount (kg) in wetland
                cbn_wet(j)%LPOC      = cbn_wet(j)%LPOC - lpoc_con * wetflwo * 1000.0
                if (cbn_wet(j)%LPOC<0.) cbn_wet(j)%LPOC  = 0.
                cbn_wet(j)%RPOC      = cbn_wet(j)%RPOC - rpoc_con * wetflwo * 1000.0 
                if (cbn_wet(j)%RPOC<0.) cbn_wet(j)%RPOC  = 0.
                cbn_wet(j)%LDOC      = cbn_wet(j)%LDOC - ldoc_con * wetflwo * 1000.0 
                if (cbn_wet(j)%LDOC<0.) cbn_wet(j)%LDOC  = 0.
                cbn_wet(j)%RDOC      = cbn_wet(j)%RDOC - rdoc_con * wetflwo * 1000.0 
                if (cbn_wet(j)%RDOC<0.) cbn_wet(j)%RDOC  = 0.
                cbn_wet(j)%DIC       = cbn_wet(j)%DIC - dic_con * wetflwo * 1000.0
                if (cbn_wet(j)%DIC<0.) cbn_wet(j)%DIC    = 0.
            endif
            !----Finish calculating bottom algea and carbon processes,added by Du----///////////////////////!
            !! compute nutrients leaving wetland
            yy    = 0.
            yy    = wetflwo / (wet_vol(j) + wetflwo)
            sedorgn(j)    = sedorgn(j) + wet_orgn(j) * yy / hru_ha(j)
            surqno3(j)    = surqno3(j) + wet_no3(j) * yy / hru_ha(j)
            latno3(j)     = latno3(j) + wet_no3s(j) * yy / hru_ha(j)
            no3gw(j)      = no3gw(j) + wet_no3g(j) * yy / hru_ha(j)
            sedorgp(j)    = sedorgp(j) + wet_orgp(j) * yy / hru_ha(j)
            sedminps(j)   = sedminps(j) + wet_psed(j) * yy / hru_ha(j)
            surqsolp(j)   = surqsolp(j) + wet_solp(j) * yy / hru_ha(j)
            minpgw(j)     = minpgw(j) + wet_solpg(j) * yy / hru_ha(j)

            !!update nutrient pools in wetlands
            wet_orgn(j) = wet_orgn(j) * (1. - yy)
            wet_no3(j) = wet_no3(j) * (1. - yy)
            wet_no3s(j) = wet_no3s(j) * (1. - yy)
            wet_no3g(j) = wet_no3g(j) * (1. - yy)
            wet_orgp(j) = wet_orgp(j) * (1. - yy)
            wet_psed(j) = wet_psed(j) * (1. - yy)
            wet_solp(j) = wet_solp(j) * (1. - yy)
            wet_solpg(j) = wet_solpg(j) * (1. - yy)
            wet_chla(j) = wet_chla(j) * (1. - yy)


        end if !if (wet_vol(j) < 0.001) then

        !! add impoundment seepage to shallow aquifer 
        !! This will be done in gwmod
!        shallst(j) = shallst(j) + wetsep / cnv

        !! compute seepage depth for HRU water balance
        twlwet = wetsep / cnv

    end if  !if (wet_fr(j) > 0.) then

    if (qdr(j) < 0.) qdr(j) = 0.
    if (sedyld(j) < 0.) then
	    sedyld(j) = 0.0
        sanyld(j) = 0.0
        silyld(j) = 0.0
        clayld(j) = 0.0
        sagyld(j) = 0.0
        lagyld(j) = 0.0
	end if

    return
end