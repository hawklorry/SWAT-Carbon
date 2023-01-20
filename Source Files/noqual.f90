      subroutine noqual

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient calculations. No transformations
!!    are calculated. New concentrations of the nutrients are calculated based
!!    on the loading to the reach from upstream.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0          |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    rch_Alg(:)     |mg alg/L      |algal biomass concentration in reach
!!    rch_NH4(:)  |mg N/L        |ammonia concentration in reach
!!    rch_SolP(:)   |mg P/L        |dissolved phosphorus concentration in reach
!!    inum1        |none          |reach number
!!    inum2        |none          |inflow hydrograph storage location number
!!    rch_NO3(:)  |mg N/L        |nitrate concentration in reach
!!    rch_NO2(:)  |mg N/L        |nitrite concentration in reach
!!    rch_OrgN(:)  |mg N/L        |organic nitrogen concentration in reach
!!    rch_OrgP(:)  |mg P/L        |organic phosphorus concentration in reach
!!    rch_CBOD(:)  |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                |reach 
!!    rch_DOX(:)   |mg O2/L       |dissolved oxygen concentration in reach
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!!    rnum1        |none          |fraction of overland flow
!!    rtwtr        |m^3 H2O       |flow out of reach
!!    varoute(2,:) |m^3 H2O       |water
!!    varoute(4,:) |kg N          |organic nitrogen
!!    varoute(5,:) |kg P          |organic posphorus
!!    varoute(6,:) |kg N          |nitrate
!!    varoute(7,:) |kg P          |soluble phosphorus
!!    varoute(13,:)|kg            |chlorophyll-a
!!    varoute(14,:)|kg N          |ammonium
!!    varoute(15,:)|kg N          |nitrite
!!    varoute(16,:)|kg            |carbonaceous biological oxygen demand
!!    varoute(17,:)|kg O2         |dissolved oxygen
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
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      integer :: jrch
      real :: wtrin, chlin, algin, orgnin, ammoin, nitratin, nitritin
      real :: orgpin, dispin, cbodin, disoxin
      real :: algcon, orgncon, nh3con, no2con, no3con
      real :: orgpcon, solpcon, cbodcon, o2con, wtrtot, cinn

      jrch = 0
      jrch = inum1

       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = varoute(2,inum2) * (1. - rnum1)

       if (rtwtr / 86400. > 0.01 .and. wtrin > 0.01) then
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
         cinn = 0.
         if (varoute(13,inum2) < 1.e-6) varoute(13,inum2) = 0.0
         chlin = 1000. * varoute(13,inum2) * (1. - rnum1) / wtrin
         algin = 1000. * chlin / ai0        !! QUAL2E equation III-1
         orgnin = 1000. * varoute(4,inum2) * (1. - rnum1) / wtrin
         ammoin = 1000. * varoute(14,inum2) * (1. - rnum1) / wtrin
         nitritin = 1000. * varoute(15,inum2) * (1. - rnum1) / wtrin
         nitratin = 1000. * varoute(6,inum2) * (1. - rnum1) / wtrin
         orgpin = 1000. * varoute(5,inum2) * (1. - rnum1) / wtrin
         dispin = 1000. * varoute(7,inum2) * (1. - rnum1) / wtrin
         if (varoute(16,inum2) < 1.e-6) varoute(16,inum2) = 0.0
         cbodin = 1000. * varoute(16,inum2) * (1. - rnum1) / wtrin
         if (varoute(17,inum2) < 1.e-6) varoute(17,inum2) = 0.0
         disoxin= 1000. * varoute(17,inum2) * (1. - rnum1) / wtrin

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
         if (rch_Alg(jrch) < 1.e-6) rch_Alg(jrch) = 0.0
         if (rch_OrgN(jrch) < 1.e-6) rch_OrgN(jrch) = 0.0
         if (rch_NH4(jrch) < 1.e-6) rch_NH4(jrch) = 0.0
         if (rch_NO2(jrch) < 1.e-6) rch_NO2(jrch) = 0.0
         if (rch_NO3(jrch) < 1.e-6) rch_NO3(jrch) = 0.0
         if (rch_OrgP(jrch) < 1.e-6) rch_OrgP(jrch) = 0.0
         if (rch_SolP(jrch) < 1.e-6) rch_SolP(jrch) = 0.0
         if (rch_CBOD(jrch) < 1.e-6) rch_CBOD(jrch) = 0.0
         if (rch_DOX(jrch) < 1.e-6) rch_DOX(jrch) = 0.0
         wtrtot = wtrin + rchwtr
         algcon = (algin * wtrin + rch_Alg(jrch) * rchwtr) / wtrtot
         orgncon = (orgnin * wtrin + rch_OrgN(jrch) * rchwtr) / wtrtot
         nh3con = (ammoin * wtrin + rch_NH4(jrch) * rchwtr) / wtrtot
         no2con = (nitritin * wtrin + rch_NO2(jrch) * rchwtr) / wtrtot
         no3con = (nitratin * wtrin + rch_NO3(jrch) * rchwtr) / wtrtot
         orgpcon = (orgpin * wtrin + rch_OrgP(jrch) * rchwtr) / wtrtot
         solpcon = (dispin * wtrin + rch_SolP(jrch) * rchwtr) / wtrtot
         cbodcon = (cbodin * wtrin + rch_CBOD(jrch) * rchwtr) / wtrtot
         o2con = (disoxin * wtrin + rch_DOX(jrch) * rchwtr) / wtrtot

         !! calculate algal biomass concentration at end of day
         rch_Alg(jrch) = 0.
         rch_Alg(jrch) = algcon
         if (rch_Alg(jrch) < 1.e-6) rch_Alg(jrch) = 0.

         !! calculate chlorophyll-a concentration at end of day
         rch_Chla(jrch) = 0.
         rch_Chla(jrch) = rch_Alg(jrch) * ai0 / 1000.

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day 
         rch_CBOD(jrch) = 0.
         rch_CBOD(jrch) = cbodcon
         if (rch_CBOD(jrch) < 1.e-6) rch_CBOD(jrch) = 0.

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day
         rch_DOX(jrch) = 0.
         rch_DOX(jrch) = o2con
         if (rch_DOX(jrch) < 1.e-6) rch_DOX(jrch) = 0.
!! end oxygen calculations

!! nitrogen calculations
        !! calculate organic N concentration at end of day
        rch_OrgN(jrch) = 0.
        rch_OrgN(jrch) = orgncon
        if (rch_OrgN(jrch) < 1.e-6) rch_OrgN(jrch) = 0.

        !! calculate ammonia nitrogen concentration at end of day
        rch_NH4(jrch) = 0.
        rch_NH4(jrch) = nh3con
        if (rch_NH4(jrch) < 1.e-6) rch_NH4(jrch) = 0.

        !! calculate concentration of nitrite at end of day
        rch_NO2(jrch) = 0.
        rch_NO2(jrch) = no2con
        if (rch_NO2(jrch) < 1.e-6) rch_NO2(jrch) = 0.

        !! calculate nitrate concentration at end of day
        rch_NO3(jrch) = 0.
        rch_NO3(jrch) = no3con
        if (rch_NO3(jrch) < 1.e-6) rch_NO3(jrch) = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day
        rch_OrgP(jrch) = 0.
        rch_OrgP(jrch) = orgpcon
        if (rch_OrgP(jrch) < 1.e-6) rch_OrgP(jrch) = 0.

        !! calculate dissolved phosphorus concentration at end
        !! of day (mineral P)
        rch_SolP(jrch) = 0.
        rch_SolP(jrch) = solpcon
        if (rch_SolP(jrch) < 1.e-6) rch_SolP(jrch) = 0.
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
        rch_Alg(jrch) = 0.0
        rch_Chla(jrch) = 0.0
        rch_OrgN(jrch) = 0.0
        rch_NH4(jrch) = 0.0
        rch_NO2(jrch) = 0.0
        rch_NO3(jrch) = 0.0
        rch_OrgP(jrch) = 0.0
        rch_SolP(jrch) = 0.0
        rch_CBOD(jrch) = 0.0
        rch_DOX(jrch) = 0.0
      endif

      return
      end