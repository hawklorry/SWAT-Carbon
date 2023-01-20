      subroutine hhnoqual

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient calculations. No trans-
!!    formations are calculated

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    rch_Alg(:)         |mg alg/L      |algal biomass concentration in reach
!!    rch_NH4(:)      |mg N/L        |ammonia concentration in reach
!!    rch_Chla(:)        |mg chl-a/L    |chlorophyll-a concentration in reach
!!    rch_SolP(:)       |mg P/L        |dissolved P concentration in reach
!!    hhvaroute(2,:,:) |m^3 H2O       |water
!!    hhvaroute(4,:,:) |kg N          |organic nitrogen
!!    hhvaroute(5,:,:) |kg P          |organic posphorus
!!    hhvaroute(6,:,:) |kg N          |nitrate
!!    hhvaroute(7,:,:) |kg P          |soluble phosphorus
!!    hhvaroute(13,:,:)|kg            |chlorophyll-a
!!    hhvaroute(14,:,:)|kg N          |ammonium
!!    hhvaroute(15,:,:)|kg N          |nitrite
!!    hhvaroute(16,:,:)|kg            |carbonaceous biological oxygen demand
!!    hhvaroute(17,:,:)|kg O2         |dissolved oxygen
!!    hrchwtr(ii)      |m^3 H2O       |water stored in reach at beginning of day
!!    hrtwtr(:)        |m^3 H2O       |flow out of reach
!!    inum1            |none          |reach number
!!    inum2            |none          |inflow hydrograph storage location number
!!    rch_NO3(:)      |mg N/L        |nitrate concentration in reach
!!    rch_NO2(:)      |mg N/L        |nitrite concentration in reach
!!    rch_OrgN(:)      |mg N/L        |organic nitrogen concentration in reach
!!    rch_OrgP(:)      |mg P/L        |organic phosphorus concentration in reach
!!    rch_CBOD(:)      |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                    |reach 
!!    rch_DOX(:)       |mg O2/L       |dissolved oxygen concentration in reach
!!    rnum1            |none          |fraction of overland flow
!!    tmpav(:)         |deg C         |average air temperature on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rch_Alg(:)    |mg alg/L      |algal biomass concentration in reach
!!    rch_NH4(:) |mg N/L        |ammonia concentration in reach
!!    rch_Chla(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    rch_SolP(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    rch_NO3(:) |mg N/L        |nitrate concentration in reach
!!    rch_NO2(:) |mg N/L        |nitrite concentration in reach
!!    rch_OrgN(:) |mg N/L        |organic nitrogen concentration in reach
!!    rch_OrgP(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_CBOD(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    rch_DOX(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    jrch        |none          |reach number
!!    nh3con      |mg N/L        |initial ammonia concentration in reach
!!    nitratin    |mg N/L        |nitrate concentration in inflow
!!    nitritin    |mg N/L        |nitrite concentration in inflow
!!    no2con      |mg N/L        |initial nitrite concentration in reach
!!    no3con      |mg N/L        |initial nitrate concentration in reach
!!    o2con       |mg O2/L       |initial dissolved oxygen concentration in 
!!                               |reach
!!    orgncon     |mg N/L        |initial organic N concentration in reach
!!    orgnin      |mg N/L        |organic N concentration in inflow
!!    orgpcon     |mg P/L        |initial organic P concentration in reach
!!    orgpin      |mg P/L        |organic P concentration in inflow
!!    solpcon     |mg P/L        |initial soluble P concentration in reach
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use parm_rchC
      use parm_control
      implicit none
      
      integer :: jrch, ii
      real :: wtrin, chlin, algin, orgnin, ammoin, nitratin, nitritin
      real :: orgpin, dispin, cbodin, disoxin, wtmp
      real :: orgpcon, solpcon, cbodcon, o2con, wtrtot
      real :: algcon, orgncon, nh3con, no2con, no3con

      jrch = 0
      jrch = inum1

!! hourly loop
      do ii = 1, nstep_rch
       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = hhvaroute(2,inum2,ii) * (1. - rnum1)

       if (hrtwtr(ii) / (idt_rch * 60.) > 0.01 .and. wtrin > 0.01) then
!! concentrations
         !! initialize inflow concentrations
         chlin = 0.
         algin = 0.
         orgnin = 0.
         ammoin = 0.
         nitritin = 0.
         nitratin = 0.
         orgpin = 0.
         dispin = 0.
         cbodin = 0.
         disoxin = 0.
         if (wtrin > 0.001) then
         chlin = 1000. * hhvaroute(13,inum2,ii) * (1. - rnum1) / wtrin
         algin = 1000. * chlin / ai0        !! QUAL2E equation III-1
         orgnin = 1000. * hhvaroute(4,inum2,ii) * (1. - rnum1) / wtrin
         ammoin = 1000. * hhvaroute(14,inum2,ii) * (1. - rnum1) / wtrin
         nitritin = 1000. * hhvaroute(15,inum2,ii) * (1. - rnum1) / &
	 	wtrin
         nitratin = 1000. * hhvaroute(6,inum2,ii) * (1. - rnum1) / wtrin
         orgpin = 1000. * hhvaroute(5,inum2,ii) * (1. - rnum1) / wtrin
         dispin = 1000. * hhvaroute(7,inum2,ii) * (1. - rnum1) / wtrin
         cbodin = 1000. * hhvaroute(16,inum2,ii) * (1. - rnum1) / wtrin
         disoxin= 1000. * hhvaroute(17,inum2,ii) * (1. - rnum1) / wtrin
         end if

         if (chlin < 1.e-6) chlin = 0.0
         if (algin < 1.e-6) algin = 0.0
         if (orgnin < 1.e-6) orgnin = 0.0
         if (ammoin < 1.e-6) ammoin = 0.0
         if (nitritin < 1.e-6) nitritin = 0.0
         if (nitratin < 1.e-6) nitratin = 0.0
         if (orgpin < 1.e-6) orgpin = 0.0
         if (dispin < 1.e-6) dispin = 0.0
         if (cbodin < 1.e-6) cbodin = 0.0
         if (disoxin < 1.e-6) disoxin = 0.0

         !! initialize concentration of nutrient in reach
         wtrtot = 0.
         algcon = 0.
         orgncon = 0.
         nh3con = 0.
         no2con = 0.
         no3con = 0.
         orgpcon = 0.
         solpcon = 0.
         cbodcon = 0.
         o2con = 0.
         wtrtot = wtrin + hrchwtr(ii)
         if (wtrtot > 0.01) then           !!R671 4/5/19  nbs
           if (ii == 1) then
             algcon = (algin * wtrin + rch_Alg(jrch) * hrchwtr(ii)) / wtrtot
             orgncon = (orgnin * wtrin + rch_OrgN(jrch) * hrchwtr(ii)) / wtrtot
             nh3con = (ammoin * wtrin + rch_NH4(jrch) * hrchwtr(ii)) / wtrtot
             no2con = (nitritin * wtrin + rch_NO2(jrch) * hrchwtr(ii)) / wtrtot
             no3con = (nitratin * wtrin + rch_NO3(jrch) * hrchwtr(ii)) / wtrtot
             orgpcon = (orgpin * wtrin + rch_OrgP(jrch) * hrchwtr(ii)) / wtrtot
             solpcon = (dispin * wtrin + rch_SolP(jrch) * hrchwtr(ii)) / wtrtot
             cbodcon = (cbodin * wtrin + rch_CBOD(jrch) * hrchwtr(ii)) / wtrtot
             o2con = (disoxin * wtrin + rch_DOX(jrch) * hrchwtr(ii)) / wtrtot
         else
             algcon = (algin * wtrin + rch_hAlg(ii-1) * hrchwtr(ii)) / wtrtot
             orgncon = (orgnin * wtrin + rch_hOrgN(ii-1) * hrchwtr(ii)) / wtrtot
             nh3con = (ammoin * wtrin + rch_hNH4(ii-1) * hrchwtr(ii)) / wtrtot
             no2con = (nitritin * wtrin + rch_hNO2(ii-1) * hrchwtr(ii)) / wtrtot
             no3con = (nitratin * wtrin + rch_hNO3(ii-1) * hrchwtr(ii)) / wtrtot
             orgpcon = (orgpin * wtrin + rch_hOrgP(ii-1) * hrchwtr(ii)) / wtrtot
             solpcon = (dispin * wtrin + rch_hSolP(ii-1) * hrchwtr(ii)) / wtrtot
             cbodcon = (cbodin * wtrin + rch_hCBOD(ii-1) * hrchwtr(ii)) / wtrtot
             o2con = (disoxin * wtrin + rch_hDOX(ii-1) * hrchwtr(ii)) / wtrtot
          end if
         end if                                     !!R671 4/5/19  nbs     

         if (algcon < 1.e-6) algcon = 0.0
         if (orgncon < 1.e-6) orgncon = 0.0
         if (nh3con < 1.e-6) nh3con = 0.0
         if (no2con < 1.e-6) no2con = 0.0
         if (no3con < 1.e-6) no3con = 0.0
         if (orgpcon < 1.e-6) orgpcon = 0.0
         if (solpcon < 1.e-6) solpcon = 0.0
         if (cbodcon < 1.e-6) cbodcon = 0.0
         if (o2con < 1.e-6) o2con = 0.0


         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13
         wtmp = 0.

         wtmp = 5.0 + 0.75 * tmpav(jrch)
         if (wtmp <= 0.) wtmp = 0.1

!! end initialize concentrations

         !! calculate algal biomass concentration at end of hour
         !! (phytoplanktonic algae)
         rch_hAlg(ii) = 0.
         rch_hAlg(ii) = algcon
         if (rch_hAlg(ii) < 0.) rch_hAlg(ii) = 0.

         !! calculate chlorophyll-a concentration at end of hour
         rch_hChla(ii) = 0.
         rch_hChla(ii) = rch_hAlg(ii) * ai0 / 1000.

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end of hour
         rch_hCBOD(ii) = 0.
         rch_hCBOD(ii) = cbodcon
         if (rch_hCBOD(ii) < 0.) rch_hCBOD(ii) = 0.

         !! calculate dissolved oxygen concentration if reach at 
         !! end of hour
         rch_hDOX(ii) = 0.
         rch_hDOX(ii) = o2con 
         if (rch_hDOX(ii) < 0.) rch_hDOX(ii) = 0.
!! end oxygen calculations

!! nitrogen calculations
         !! calculate organic N concentration at end of hour
         rch_hOrgN(ii) = 0.
         rch_hOrgN(ii) = orgncon 
         if (rch_hOrgN(ii) < 0.) rch_hOrgN(ii) = 0.

        !! calculate ammonia nitrogen concentration at end of hour
        rch_hNH4(ii) = 0.
        rch_hNH4(ii) = nh3con 
        if (rch_hNH4(ii) < 0.) rch_hNH4(ii) = 0.

        !! calculate concentration of nitrite at end of hour
        rch_hNO2(ii) = 0.
        rch_hNO2(ii) = no2con 
        if (rch_hNO2(ii) < 0.) rch_hNO2(ii) = 0.

        !! calculate nitrate concentration at end of hour
        rch_hNO3(ii) = 0.
        rch_hNO3(ii) = no3con 
        if (rch_hNO3(ii) < 0.) rch_hNO3(ii) = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of hour
        rch_hOrgP(ii) = 0.
        rch_hOrgP(ii) = orgpcon 
        if (rch_hOrgP(ii) < 0.) rch_hOrgP(ii) = 0.

        !! calculate dissolved phosphorus concentration at end of hour
        rch_hSolP(ii) = 0.
        rch_hSolP(ii) = solpcon 
        if (rch_hSolP(ii) < 0.) rch_hSolP(ii) = 0.
!! end phosphorus calculations

      else
        !! all water quality variables set to zero when no flow
        algin = 0.0
        chlin = 0.0
        orgnin = 0.0
        ammoin = 0.0
        nitritin = 0.0
        nitratin = 0.0
        orgpin = 0.0
        dispin = 0.0
        cbodin = 0.0
        disoxin = 0.0
        rch_hAlg(ii) = 0.0
        rch_hChla(ii) = 0.0
        rch_hOrgN(ii) = 0.0
        rch_hNH4(ii) = 0.0
        rch_hNO2(ii) = 0.0
        rch_hNO3(ii) = 0.0
        rch_hOrgP(ii) = 0.0
        rch_hSolP(ii) = 0.0
        rch_hCBOD(ii) = 0.0
        rch_hDOX(ii) = 0.0
      endif
        if (rch_hAlg(ii) < 1.e-6) rch_hAlg(ii) = 0.0
        if (rch_hChla(ii) < 1.e-6) rch_hChla(ii) = 0.0
        if (rch_hOrgN(ii) < 1.e-6) rch_hOrgN(ii) = 0.0
        if (rch_hNH4(ii) < 1.e-6) rch_hNH4(ii) = 0.0
        if (rch_hNO2(ii) < 1.e-6) rch_hNO2(ii) = 0.0
        if (rch_hNO3(ii) < 1.e-6) rch_hNO3(ii) = 0.0
        if (rch_hOrgP(ii) < 1.e-6) rch_hOrgP(ii) = 0.0
        if (rch_hSolP(ii) < 1.e-6) rch_hSolP(ii) = 0.0
        if (rch_hCBOD(ii) < 1.e-6) rch_hCBOD(ii) = 0.0
        if (rch_hDOX(ii) < 1.e-6) rch_hDOX(ii) = 0.0
      end do
!! end hourly loop

!! set end of day concentrations
      rch_Alg(jrch) = rch_hAlg(nstep_rch)
      rch_Chla(jrch) = rch_hChla(nstep_rch)
      rch_OrgN(jrch) = rch_hOrgN(nstep_rch)
      rch_NH4(jrch) = rch_hNH4(nstep_rch)
      rch_NO2(jrch) = rch_hNO2(nstep_rch)
      rch_NO3(jrch) = rch_hNO3(nstep_rch)
      rch_OrgP(jrch) = rch_hOrgP(nstep_rch)
      rch_SolP(jrch) = rch_hSolP(nstep_rch)
      rch_CBOD(jrch) = rch_hCBOD(nstep_rch)
      rch_DOX(jrch) = rch_hDOX(nstep_rch)

      if (rch_Alg(jrch) < 1.e-6) rch_Alg(jrch) = 0.0
      if (rch_Chla(jrch) < 1.e-6) rch_Chla(jrch) = 0.0
      if (rch_OrgN(jrch) < 1.e-6) rch_OrgN(jrch) = 0.0
      if (rch_NH4(jrch) < 1.e-6) rch_NH4(jrch) = 0.0
      if (rch_NO2(jrch) < 1.e-6) rch_NO2(jrch) = 0.0
      if (rch_NO3(jrch) < 1.e-6) rch_NO3(jrch) = 0.0
      if (rch_OrgP(jrch) < 1.e-6) rch_OrgP(jrch) = 0.0
      if (rch_SolP(jrch) < 1.e-6) rch_SolP(jrch) = 0.0
      if (rch_CBOD(jrch) < 1.e-6) rch_CBOD(jrch) = 0.0
      if (rch_DOX(jrch) < 1.e-6) rch_DOX(jrch) = 0.0

      return
      end