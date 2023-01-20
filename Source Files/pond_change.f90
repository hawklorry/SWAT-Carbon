      subroutine pond_Du(k)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through ponds
!!    and computes evaporation and seepage from the ponds

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bp1(:)      |none          |1st shape parameter for pond surface area
!!                               |equation
!!    bp2(:)      |none          |2nd shape parameter for the pond surface area
!!                               |equation
!!    chlap(:)    |none          |chlorophyll-a production coefficient for pond
!!    hru_sub(:)  |none          |subbasin in which HRU/reach is located
!!    iflod1(:)   |none          |beginning month of non-flood season
!!    iflod2(:)   |none          |ending month of non-flood season
!!    ipnd1(:)    |none          |beginning month of 2nd "season" of nutrient
!!                               |settling
!!    ipnd2(:)    |none          |ending month of 2nd "season" of nutrient
!!                               |settling
!!    i_mo        |none          |current month of simulation
!!    ndtarg(:)   |none          |number of days required to reach target
!!                               |storage from current pond storage
!!    nsetlp(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlp(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    pnd_evol(:) |m^3 H2O       |volume of water required to fill pond
!!                               |to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_k(:)    |mm/hr         |hydraulic conductivity through bottom of
!!                               |ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_no3g(:) |kg N          |amount of nitrate originating from 
!!                               |groundwater in pond at beginning of day
!!    pnd_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in pond at beginning of day
!!    pnd_nsed(:) |kg/L          |normal ratio of sediment to water in pond
!!    pnd_orgn(:) |kg N          |amount of organic N originating from 
!!                               |surface runoff in pond at beginning of day
!!    pnd_orgp(:) |kg P          |amount of organic P originating from 
!!                               |surface runoff in pond at beginning of day
!!    pnd_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in pond at
!!                               |beginning of day
!!    pnd_pvol(:) |m^3 H2O       |volume of water required to fill pond
!!                               |to the principal spillway
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    pnd_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    psetlp(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlp(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    seccip(:)   |none          |water clarity coefficient for pond
!!    sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in
!!                               |impoundment after settling for one day
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on any given day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pnd_chla(:) |kg chl_a      |amount of chlorophyll-a in pond at end of day
!!    pnd_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in pond at end of day
!!    pnd_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in pond at end of day
!!    pnd_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in pond at end of day
!!    pnd_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in pond at end of day
!!    pnd_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in pond at end of day
!!    pnd_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in pond at
!!                               |end of day
!!    pnd_seci(:) |m             |secchi-disk depth of pond
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    pnd_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in pond at end of day
!!    pnd_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in pond at end of day
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlaco      |ppb (ug/L)    |concentration of chlorophyll-a in pond
!!    iseas       |none          |nutrient settling rate season
!!    k           |none          |HRU or reach number
!!    nitrok      |none          |fraction of nitrogen in pond removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in pond removed by
!!                               |settling
!!    pndsa       |ha            |surface area of pond on current day
!!    sed         |kg/L          |sediment concentration in pond at beginning of
!!                               |day
!!    targ        |m^3 H2O       |target storage level in pond
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in pond water
!!                               |on day
!!    vol         |m^3 H2O       |volume of water in pond at beginning of day
!!    xx          |none          |variable to hold intermediate calc result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para  !! added by Du-----------------------------------------------
      use parm
      use parm_subC
      use parm_subE
      use parm_subH
      use parm_rchC
      use parm_rchE
      use parm_control
      use parm_output
      
      implicit none
       
      integer, intent (in) :: k
      real :: vol, sed, pndsa, xx, targ, tpco, phosk, nitrok, chlaco
      integer :: iseas,j
	  real :: san, sil, cla, sag, lag, inised, finsed,setsed,remsetsed
      !! added by Du-----------------------------------------------
      real :: lpoc_con,rpoc_con,ldoc_con,rdoc_con,dic_con,dic_bed       !local variables for carbon processes 
      real :: wtmp,depth,f_rdb,kac,kh_DIC,CO2_sat,cbn_bur,solpcon,yy       !local variable for carbon processes
      real :: Ab_con,Ab_UN,Ab_UP,DIN_con,qN_Ab,qP_Ab,fnl_Ab,fll_Ab      !local variables for bottom algae
      real :: fnl_dic,I0_Ab,IH_Ab,sedc,ke_Ab,POMcon,fsl_Ab,kph_Ab,wtmpk !local variables for bottom algae
      !! added by Du-----------------------------------------------
      real :: algcon
      real :: VELOFL, TRAPPND, SUSP
      real, external :: THETA
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
      vol = pnd_vol(k)
      sed = pnd_sed(k)
      san = pnd_san(k)
      sil = pnd_sil(k)
      cla = pnd_cla(k)
      sag = pnd_sag(k)
      lag = pnd_lag(k)

        !! calculate water balance for day
      pndsa = 0.
      pndsa = hru_fr(k) * bp1(k) * pnd_vol(k) ** bp2(k)
      pndev = 10. * evpnd(k) * pet_day * pndsa
      pndsep = pnd_k(k) * pndsa * 240.
      pndpcp = subp(k) * pndsa * 10.

        !! new water volume for day
      pnd_vol(k) = pnd_vol(k) - pndsep - pndev + pndpcp + pndflwi

      if (pnd_vol(k) < 0.001) then
          !! if volume deficit in pond reduce seepage
          !! so that the pond volume is zero
          pndsep = pndsep + pnd_vol(k)
          pnd_vol(k) = 0.
          !! if seepage is less than the volume deficit, take the remainder
          !! from evaporation
          if (pndsep < 0.) then
            pndev = pndev + pndsep
            pndsep = 0.
          end if
          pnd_sed(k) = 0.
          pnd_san(k) = 0.
          pnd_sil(k) = 0.
          pnd_cla(k) = 0.
          pnd_sag(k) = 0.
          pnd_lag(k) = 0.
          pnd_solp(k) = 0.
          pnd_psed(k) = 0.
          pnd_orgp(k) = 0.
          pnd_solpg(k) = 0.
          pnd_orgn(k) = 0.
          pnd_no3(k) = 0.
          pnd_no3s(k) = 0.
          pnd_no3g(k) = 0.
          pnd_chla(k) = 0.
          pnd_seci(k) = 0.
          !---For carbon module,added by Du-----!
          if (cswat == 2 ) then
              cbn_pnd(k)%LPOC=0.
              cbn_pnd(k)%RPOC=0.
              cbn_pnd(k)%LDOC=0.
              cbn_pnd(k)%RDOC=0.
              cbn_pnd(k)%DIC=0.
          endif
          !---For carbon module,added by Du-----!
      else
        
          !! compute outflow
          if (pnd_evol(k) <= 0.) then
            !! all storage over principle is outflow
            if (pnd_vol(k) <= pnd_pvol(k)) then
              pndflwo = 0.
            else
              pndflwo = pnd_vol(k) - pnd_pvol(k)
            end if
          elseif (pnd_vol(k) > pnd_evol(k)) then
            !! if emergency level is defined, anytime pond volume
            !! exceeds this level, all excess is released
            pndflwo = pnd_vol(k) - pnd_evol(k)
          else
            !! target storage based on flood season and soil water
            xx = 0.
            targ = 0.
            if (iflod2(k) > iflod1(k)) then
              if (i_mo > iflod1(k) .and. i_mo < iflod2(k)) then
                targ = pnd_evol(k)
              else
                xx = Min(sol_sw(k) / sol_sumfc(k),1.)
                targ = pnd_pvol(k) + .5 * (1. - xx) * (pnd_evol(k) -  &
				pnd_pvol(k))
              end if
            else
              if (i_mo > iflod1(k) .or. i_mo < iflod2(k)) then
                targ = pnd_evol(k)
              else
                xx = Min(sol_sw(k) / sol_sumfc(k),1.)
                targ = pnd_pvol(k) + .5 * (1. - xx) * (pnd_evol(k) -  &
				pnd_pvol(k))
              end if
            end if
            if (pnd_vol(k) > targ) then
              pndflwo = (pnd_vol(k) - targ) / ndtarg(k)
            else
              pndflwo = 0.
            end if
          end if
          
          !! compute new sediment concentration
          if (pndsedin < 1.e-6) pndsedin = 0.
          if (pndsa == 0.) pndsa = 0.001    !!MJW added line of code 040811
          velofl = (pndflwo / pndsa) / 10000.
          if (velofl > 1.e-6) then
             trappnd = velsetlp(k) / velofl
             if (trappnd > 1.) trappnd = 1.
             susp = 1. - trappnd
          else
             susp = 1.                      !!R666b 7/19/17 nbs
          endif
               
          pnd_sed(k) = (sed * vol + susp * pndsedin) / pnd_vol(k)
          pnd_san(k) = (san * vol + pndsanin) / pnd_vol(k)
          pnd_sil(k) = (sil * vol + pndsilin) / pnd_vol(k)
          pnd_cla(k) = (cla * vol + pndclain) / pnd_vol(k)
          pnd_sag(k) = (sag * vol + pndsagin) / pnd_vol(k)
          pnd_lag(k) = (lag * vol + pndlagin) / pnd_vol(k)

          !! compute final pond volume
          pnd_vol(k) = pnd_vol(k) - pndflwo
          if (pnd_vol(k) < 0.) then
                pndflwo = pndflwo + pnd_vol(k)
                pnd_vol(k) = 0.
          endif

          !! compute change in sediment concentration due to settling
	      if (sed_stl(k) < 1.e-6) sed_stl(k) = 0.0
          if (pnd_sed(k) > pnd_nsed(k)) then
	          inised = pnd_sed(k)
                pnd_sed(k) = (pnd_sed(k) - pnd_nsed(k)) * sed_stl(k) + pnd_nsed(k)
	          finsed = pnd_sed(k)
	          setsed = inised - finsed

	          if (pnd_lag(k) >= setsed) then
	                pnd_lag(k) = pnd_lag(k) - setsed
	                remsetsed = 0.
              else
	                remsetsed = setsed - pnd_lag(k)
	                pnd_lag(k) = 0.
	                if (pnd_san(k) >= remsetsed) then
	                        pnd_san(k) = pnd_san(k) - remsetsed
	                        remsetsed = 0.
	                else
	                        remsetsed = remsetsed - pnd_san(k)
	                        pnd_san(k) = 0.
                            if (pnd_sag(k) >= remsetsed) then
	                            pnd_sag(k) = pnd_sag(k) - remsetsed
	                            remsetsed = 0.
	                        else
	                            remsetsed = remsetsed - pnd_sag(k)
	                            pnd_sag(k) = 0.
                                if (pnd_sil(k) >= remsetsed) then
  	                                pnd_sil(k) = pnd_sil(k) - remsetsed
	                                remsetsed = 0.
	                            else
	                                remsetsed = remsetsed - pnd_sil(k)
	                                pnd_sil(k) = 0.
                                    if (pnd_cla(k) >= remsetsed) then
	                                    pnd_cla(k) = pnd_cla(k) - remsetsed
	                                    remsetsed = 0.
	                                else
	                                    remsetsed = remsetsed - pnd_cla(k)
	                                    pnd_cla(k) = 0.
	                                end if
                                end if
	                        end if
                    end if
              end if

          end if
          !! compute sediment leaving pond
          pndsedo = pnd_sed(k) * pndflwo
          pndsano = pnd_san(k) * pndflwo
          pndsilo = pnd_sil(k) * pndflwo
          pndclao = pnd_cla(k) * pndflwo
          pndsago = pnd_sag(k) * pndflwo
          pndlago = pnd_lag(k) * pndflwo
 
          !! net change in amount of sediment in pond for day
          pndsedc = vol * sed + pndsedin - pndsedo - pnd_sed(k) * pnd_vol(k)

          !! determine settling rate
          !! part of equation 29.1.3 in SWAT manual
          if (i_mo >= ipnd1(k) .and. i_mo <= ipnd2(k)) then
            iseas = 1
          else
            iseas = 2
          endif
          phosk = 0.
          nitrok = 0.
          phosk = psetlp(iseas,k) * pndsa * 10000. / pnd_vol(k)  !setl/mean depth
          phosk = Min(phosk, 1.)
          nitrok = nsetlp(iseas,k) * pndsa * 10000. / pnd_vol(k) !setl/mean depth
          nitrok = Min(nitrok, 1.)

          !! remove nutrients by settling
          !! other part of equation 29.1.3 in SWAT manual
          pnd_solp(k) = pnd_solp(k) * (1. - phosk)
          pnd_psed(k) = pnd_psed(k) * (1. - phosk)
          pnd_orgp(k) = pnd_orgp(k) * (1. - phosk)
          pnd_solpg(k) = pnd_solpg(k) * (1. - phosk)
          pnd_orgn(k) = pnd_orgn(k) * (1. - nitrok)
          pnd_no3(k) = pnd_no3(k) * (1. - nitrok)
          pnd_no3s(k) = pnd_no3s(k) * (1. - nitrok)
          pnd_no3g(k) = pnd_no3g(k) * (1. - nitrok)

          tpco = 0.
          if (pnd_vol(k) + pndflwo > 0.1) then
            tpco = 1.e+6 * (pnd_solp(k) + pnd_orgp(k) + pnd_psed(k) + pnd_solpg(k)) / (pnd_vol(k) + pndflwo)
          else
            tpco = 0.
          endif
          chlaco = 0.
          pnd_chla(k) = 0.
          pnd_seci(k) = 0.
          if (tpco > 1.e-4) then
              !! equation 29.1.6 in SWAT manual
              chlaco = chlap(k) * 0.551 * (tpco**0.76)
              pnd_chla(k) = chlaco * (pnd_vol(k) + pndflwo) * 1.e-6
          endif
          if (chlaco > 1.e-4) then
            !! equation 29.1.8 in SWAT manual
            pnd_seci(k) = seccip(k) * 6.35 * (chlaco**(-0.473))
          endif
          
          !-----For calculating bottom algea and carbon processes,added by Du-/////////////////////////!
          if (cswat == 2 ) then
                if (i == 1 .and. curyr == 1) then
                    Ab_pnd(k)%Ab = 100.
                    
                    Ab_pnd(k)%INb = Ab_pnd(k)%Ab * Ab_pndpara(k)%qoN
                    Ab_pnd(k)%IPb = Ab_pnd(k)%Ab * Ab_pndpara(k)%qoP
                    
                end if
          
          
                wtmp    = 0.
                wtmp    = 5.0 + 0.75 * tmpav(k) ! calculating water temperature
                if (wtmp <= 0.) wtmp = 0.1
                depth   = 0.
                depth   = pnd_vol(k) / (pndsa * 10000.0) !calculating the depth (m)
                !Caculate initial carbon concentration in the wetland
                rpoc_con    = 0.
                rpoc_con    = cbn_pnd(k)%RPOC / pnd_vol(k) * 1000.0
                lpoc_con    = 0.
                lpoc_con    = cbn_pnd(k)%LPOC / pnd_vol(k) * 1000.0
                rdoc_con    = 0.
                rdoc_con    = cbn_pnd(k)%RDOC / pnd_vol(k) * 1000.0
                ldoc_con    = 0.
                ldoc_con    = cbn_pnd(k)%LDOC / pnd_vol(k) * 1000.0
                dic_con     = 0.
                dic_con     = cbn_pnd(k)%DIC / pnd_vol(k) * 1000.0
                
                !--calculating bottom algae processes in the wetland-----------!
                Ab_con       = 0.
                Ab_con       = Ab_pnd(k)%Ab
                !bottom algea death procese
                Ab_pnd(k)%death  = 0.
                Ab_pnd(k)%death  = Ab_con * Theta((Ab_pndpara(k)%kdb),1.04,wtmp) ! bottom algea death biomass (g/m2/day)
                !----bottom algae photosynthesis/growth process
                !For calculating nutrient limitation factor
                qN_Ab    = Ab_pnd(k)%INb/Ab_con  !the cell quotas of N (mgN/gD)
                qP_Ab    = Ab_pnd(k)%IPb/Ab_con  !the cell quotas of P (mgP/gD)
                fnl_dic  = 0.
                fnl_dic  = Ab_pndpara(k)%fdic*dic_con !H2CO3* and HCO3- concentration (mg/L)
                fnl_Ab   = 0.
                fnl_Ab   = min((1-Ab_pndpara(k)%qoN/qN_Ab),(1-Ab_pndpara(k)%qoP/qP_Ab) ,(fnl_dic/(fnl_dic+Ab_pndpara(k)%kscb)))
                if (fnl_Ab < 0.) fnl_Ab  = 0.
                !kinetics for intracellular N and P process in bottom algae
                Ab_UN    = 0.     !uptake rates for N in bottom algae (mgN/m2/day)
                Ab_UP    = 0.     !uptake rates for P in bottom algae (mgP/m2/day)
                DIN_con  = 0.
                DIN_con  = (pnd_no3(k) + pnd_no3s(k) + pnd_no3g(k)) / pnd_vol(k) * 1000.0  !calculate DIN concentration (mgN/L)
                Ab_UN    = Ab_pndpara(k)%pmN * (DIN_con / (DIN_con + Ab_pndpara(k)%Ksnb)) &
                            * (Ab_pndpara(k)%KqN / (Ab_pndpara(k)%KqN + (qN_Ab - Ab_pndpara(k)%qoN))) * Ab_con  !calculating N uptakes in bottom algae
                if (Ab_UN < 0.) Ab_UN   = 0.
                solpcon  = 0.
                solpcon  = (pnd_solp(k) + pnd_solpg(k)) / pnd_vol(k) * 1000.0           !calculate DIP concentration (mgP/L)
                Ab_UP    = Ab_pndpara(k)%pmP * (solpcon / (solpcon + Ab_pndpara(k)%Kspb))             &
                            * (Ab_pndpara(k)%KqP / (Ab_pndpara(k)%KqP + (qP_Ab - Ab_pndpara(k)%qoP))) * Ab_con !calculating P uptakes in bottom algae
                if (Ab_UP < 0.) Ab_UP   = 0.
                Ab_pnd(k)%INb    = Ab_pnd(k)%INb + Ab_UN - Ab_pnd(k)%death               &
                                     * qN_Ab - qN_Ab * Theta(Ab_pndpara(k)%kexb,1.04,wtmp) * Ab_con    !intracellular N mass balance
                if (Ab_pnd(k)%INb < 0.) Ab_pnd(k)%INb = 0.
                Ab_pnd(k)%IPb    = Ab_pnd(k)%IPb + Ab_UP - Ab_pnd(k)%death               &
                                     * qP_Ab - qP_Ab * Theta(Ab_pndpara(k)%kexb,1.04,wtmp) * Ab_con    !intracellular P mass balance
                if (Ab_pnd(k)%IPb < 0.) Ab_pnd(k)%IPb    = 0.
                
                !For calculating light limation factor
                I0_Ab    = 0.
                I0_Ab    = hru_ra(k) * tfact  !*0.47    !photosynthetically-active solar radiation reaching water surface
                POMcon   = (lpoc_con + rpoc_con)/0.58 !Convert POC to POM concentration
                sedc     = 0.
                sedc     = pnd_sed(k)*1.e6   ! TSS concentration (mg/L)
                algcon   = 0.
                algcon   = chlaco / ai0   !!floating algae concentration (mg/L) 
                ke_Ab    = Ab_pndpara(k)%keb + Ab_pndpara(k)%kiss * sedc + Ab_pndpara(k)%kpom              &
                            * POMcon + Ab_pndpara(k)%kap * algcon + Ab_pndpara(k)%kapn                        &
                            * algcon**(.66667) + Ab_pndpara(k)%kAb * Ab_con/depth       !the light extinction coefficient
                IH_Ab   = 0.
                IH_Ab   = I0_Ab * exp(-ke_Ab * depth)          !the quantity of bottom light
                fll_Ab  = 0.
                select case (Ab_pndpara(k)%light)     ! caculating light limitation factor using different light models
                    case (1)
                        !! Half-Saturation Light Model 
                        fll_Ab  = IH_Ab / (Ab_pndpara(k)%klb+IH_Ab)
                    case (2)
                        !! Smith’s Function for light model 
                        fll_Ab  = IH_Ab / (Ab_pndpara(k)%klb**2 + IH_Ab**2)**(0.5)
                    case (3)
                    !! Steele’s Equation for light model
                    fll_Ab  = (IH_Ab / Ab_pndpara(k)%klb)  *exp(1+IH_Ab / Ab_pndpara(k)%klb)
                end select
                kph_Ab     = 0.
                kph_Ab     = Theta(Ab_pndpara(k)%kph, 1.04, wtmp)       !temperature correction for photosynthesis rate
                Ab_pnd(k)%photo    = 0.
                if (Ab_pndpara(k)%ipho == 0) then !zero order growth model
                    Ab_pnd(k)%photo = kph_Ab * fll_Ab * fnl_Ab                     
                else    !first order growth model
                    fsl_Ab  = 0.
                    fsl_Ab  = max(1. - Ab_con / Ab_pndpara(k)%Abmax, 0.) ! Space limitation factor
                    Ab_pnd(k)%photo = kph_Ab * fll_Ab * fnl_Ab * fsl_Ab * Ab_con
                endif
                !bottom algea respiration process
                Ab_pnd(k)%resp = 0.
                Ab_pnd(k)%resp = Ab_pndpara(k)%krb1 * Ab_con + Ab_pndpara(k)%krb2  *Ab_pnd(k)%photo
                !Bottom algae biomass mass balance
                xx  = 0.
                xx  = Ab_pnd(k)%Ab
                Ab_pnd(k)%Ab    = Ab_pnd(k)%Ab+Ab_pnd(k)%photo - Ab_pnd(k)%resp - Ab_pnd(k)%death
                !--Finish calculating bottom algae processes in the pond-------!
                !---calculating carbon species transformations----!
                cbn_rec%Ab_cbn    = 0.
                cbn_rec%Ab_cbn    = cbn_pndpara(k)%rca * Ab_pnd(k)%death / depth    !bottom algae death to total organic carbon
                !LPOC's reaction pathways
                cbn_rec%Ab_LP     = 0.
                cbn_rec%Ab_LP     = cbn_pndpara(k)%f_lpb * cbn_rec%Ab_cbn ! bottom algae to LPOC 
                cbn_rec%LP_set    = 0.
                cbn_rec%LP_set    = cbn_pndpara(k)%sv_lp / depth * lpoc_con                !LPOC settling to bed
                cbn_rec%LP_LD     = 0.
                cbn_rec%LP_LD     = Theta((cbn_pndpara(k)%klp), 1.047, wtmp) * lpoc_con     !LPOC dissolution to LDOC
                cbn_rec%LP_DIC    = 0.
                cbn_rec%LP_DIC    = Theta((cbn_pndpara(k)%kd_lp), 1.047, wtmp) * lpoc_con  !LPOC decay to DIC
                cbn_rec%LR_POC    = 0.
                cbn_rec%LR_POC    = Theta((cbn_pndpara(k)%klrp), 1.047, wtmp) * lpoc_con  ! LPOC decay to RPOC
                yy    = 0.
                yy    = lpoc_con
                lpoc_con  = lpoc_con + cbn_rec%Ab_LP - cbn_rec%LP_set	&
                            - cbn_rec%LP_LD - cbn_rec%LP_DIC - cbn_rec%LR_POC  !update LPOC concentration for transformations  
                if (lpoc_con  < 0.) then
                    lpoc_con  = 0.
                    !correct LPOC settling amount using mass balance equation if settling is too much
                    cbn_rec%LP_set    = yy + cbn_rec%Ab_LP - cbn_rec%LP_LD - cbn_rec%LP_DIC - cbn_rec%LR_POC
                endif
                !RPOC reaction pathway   
                cbn_rec%Ab_RP     = 0.
                cbn_rec%Ab_RP     = cbn_pndpara(k)%f_rpb * cbn_rec%Ab_cbn    ! bottom algae to RPOC        
                cbn_rec%RP_LD     = 0. 
                cbn_rec%RP_LD     = Theta((cbn_pndpara(k)%krp), 1.047, wtmp) * rpoc_con      !RPOC dissolution to LDOC
                cbn_rec%RP_DIC    = 0.
                cbn_rec%RP_DIC    = Theta((cbn_pndpara(k)%kd_rp), 1.047, wtmp) * rpoc_con   !RPOC decay to DIC
                cbn_rec%RP_set    = 0.
                cbn_rec%RP_set    = cbn_pndpara(k)%sv_rp / depth * rpoc_con                !RPOC settling to bed
                yy    = 0.
                yy    = rpoc_con
                rpoc_con  = rpoc_con+cbn_rec%Ab_RP - cbn_rec%RP_set	& 
                        -cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC   !update RPOC concentration for transformations  
                if (rpoc_con < 0.) then
                    rpoc_con  = 0.
                    !correct RPOC settling amount using mass balance equation if settling is too much
                    cbn_rec%RP_set    = yy + cbn_rec%Ab_RP - cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC
                endif
                !LDOC reaction pathways
                cbn_rec%Ab_LD     = 0.
                cbn_rec%Ab_LD     = cbn_pndpara(k)%f_ldb * cbn_rec%Ab_cbn   ! bottom algae death to LDOC
                cbn_rec%LD_DIC    =   0.
                cbn_rec%LD_DIC    =   Theta((cbn_pndpara(k)%kld), 1.047, wtmp) * ldoc_con  !LDOC decay to DIC
                cbn_rec%LR_DOC    =   0.
                cbn_rec%LR_DOC    =   Theta((cbn_pndpara(k)%klrd), 1.047, wtmp) * ldoc_con      ! LDOC decay to RDOC
                cbn_rec%LD_NO3    =   0.
                cbn_rec%LD_NO3    =   DIN_con * (15.0/14.0) * Theta((cbn_pndpara(k)%kdnit), 1.045, wtmp) !LDOC consumbed by NO3 denitrification    
                ldoc_con          = ldoc_con + cbn_rec%Ab_LD + cbn_rec%LP_LD     &
                                    + cbn_rec%RP_LD - cbn_rec%LR_DOC - cbn_rec%LD_DIC       &
                                    - cbn_rec%LD_NO3  !update LDOC concentration for transformations  
                if (ldoc_con<0.) ldoc_con = 0.      
                !RDOC reation pathways
                f_rdb             = 1. - cbn_pndpara(k)%f_lpb - cbn_pndpara(k)%f_rpb - cbn_pndpara(k)%f_ldb  !fraction of bottom algae death to RDOC
                cbn_rec%Ab_RD     = 0.
                cbn_rec%Ab_RD     = f_rdb * cbn_rec%Ab_cbn ! bottom algae to RPOC
                cbn_rec%RD_DIC    = 0.
                cbn_rec%RD_DIC    = Theta((cbn_pndpara(k)%krd),1.047,wtmp) * rdoc_con      !RDOC decay to DIC
                yy    = 0.
                yy    = rdoc_con
                rdoc_con  = rdoc_con + cbn_rec%Ab_RD + cbn_rec%LR_DOC - cbn_rec%RD_DIC  !update RDOC concentration for transformations 
                if (rdoc_con < 0.) rdoc_con   = 0.
                !DIC reaction pathways
                cbn_rec%Atm_DIC   = 0.
                kac               = Theta(rk2(hru_sub(k)), 1.024, wtmp) * 0.923           !CO2 reaeration rate
                wtmpk             = wtmp + 273.15  !convet the unit of water temp to Kelvin
                kh_DIC            = 10.0**((2385.73 / wtmpk) + 0.0152642 * wtmpk - 14.0184)     !Henry's constant [mol/L/atm] 
                if(ico2==1)   cbn_pndpara(k)%p_co2    = co2con(iyr)                    
          
                !CO2_sat   = 12.0 * kh_DIC * cbn_pndpara(k)%p_co2 / 1000.0           !CO2 saturation (unit converting to mg/L)
                !updated
                CO2_sat           = kh_DIC * (cbn_pndpara(k)%p_co2 / 1000000) * 1000.0 * 12.0     !CO2 saturation (mol/L unit converting to mgC/L) 
          
                cbn_rec%Atm_DIC   = kac * (CO2_sat - cbn_pndpara(k)%f_co2 * dic_con)   !Atmospheric CO2 reaeration 
                cbn_rec%Ab_DIC    = 0.
                cbn_rec%Ab_DIC    = cbn_pndpara(k)%rca * Ab_pnd(k)%resp / depth    !bottom algae respiration to DIC  
                cbn_rec%DIC_Ab    = 0.
                cbn_rec%DIC_Ab    = cbn_pndpara(k)%rca * Ab_pnd(k)%photo / depth    !DIC consumbed by bottom algae photosynthesis
                !first order sediment diagenesis model-from W2 model
                scbn_pnd(k)%scbn  = scbn_pnd(k)%scbn + cbn_rec%RP_set +cbn_rec%LP_set !add RPOC and LPOC settling from water column     
                dic_bed           = 0.
                dic_bed           = scbn_pnd(k)%scbn * scbn_pndpara(k)%ksed   !DIC release from bed sediment compartment 
                cbn_bur           = 0.
                cbn_bur           = scbn_pnd(k)%scbn * scbn_pndpara(k)%kbur           !sediment burial amount 
                scbn_pnd(k)%scbn  = scbn_pnd(k)%scbn - dic_bed - cbn_bur   !update sediment carbon amount after loss
                if (scbn_pnd(k)%scbn<0.) scbn_pnd(k)%scbn = 0.
                cbn_rec%bed_DIC   = dic_bed           !DIC release from bed sediment         
                dic_con           = dic_con + cbn_rec%Atm_DIC + cbn_rec%LP_DIC            &
                                    + cbn_rec%RP_DIC + cbn_rec%LD_DIC + cbn_rec%RD_DIC         &!transformations from organic carbon pools
                                    + cbn_rec%Ab_DIC - cbn_rec%DIC_Ab + cbn_rec%bed_DIC   !update DIC concentration change after transformation             
                if (dic_con<0.) dic_con   = 0.  !Finish calculating carbon tranformations among different species
                !calculating carbon amount leaving wetland (kg/ha) 
                Sed_LPOC(j)   = Sed_LPOC(j) + (lpoc_con * pndflwo / 1000.0) / hru_ha(k)
                Sed_RPOC(j)   = Sed_RPOC(j) + (rpoc_con * pndflwo / 1000.0) / hru_ha(k)
                HRU_LDOC(j)   = HRU_LDOC(j) + (ldoc_con *pndflwo / 1000.0) / hru_ha(k)
                HRU_RDOC(j)   = HRU_RDOC(j) + (rdoc_con *pndflwo / 1000.0) / hru_ha(k)
                HRU_DIC(j)    = HRU_DIC(j) + (dic_con * pndflwo / 1000.0) / hru_ha(k)
                !update carbon amount (kg) in wetland
                cbn_pnd(k)%LPOC  = (pnd_vol(k) * lpoc_con - lpoc_con * pndflwo) / 1000.0
                if (cbn_pnd(k)%LPOC < 0.) cbn_pnd(k)%LPOC    = 0.
                cbn_pnd(k)%RPOC  = (pnd_vol(k) * rpoc_con - rpoc_con * pndflwo) / 1000.0 
                if (cbn_pnd(k)%RPOC < 0.) cbn_pnd(k)%RPOC  = 0.
                cbn_pnd(k)%LDOC  = (pnd_vol(k) * ldoc_con - ldoc_con * pndflwo) / 1000.0 
                if (cbn_pnd(k)%LDOC < 0.) cbn_pnd(k)%LDOC  = 0.
                cbn_pnd(k)%RDOC  = (pnd_vol(k) * rdoc_con - rdoc_con * pndflwo) / 1000.0 
                if (cbn_pnd(k)%RDOC < 0.) cbn_pnd(k)%RDOC    = 0.
                cbn_pnd(k)%DIC   = (pnd_vol(k) * dic_con - dic_con * pndflwo) / 1000.0
                if (cbn_pnd(k)%DIC < 0.) cbn_pnd(k)%DIC  = 0.
          endif !if (cswat == 2 ) then
          !! added by Du//////////////////////////////////////////////////////////////////////////////
      end if ! if (pnd_vol(k) < 0.001) 
        

      return
      end