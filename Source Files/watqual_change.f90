      subroutine watqual_DQ

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0          |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1          |mg N/mg alg   |fraction of algal biomass that is nitrogen
!!    ai2          |mg P/mg alg   |fraction of algal biomass that is phosphorus
!!    ai3          |mg O2/mg alg  |the rate of oxygen production per unit of
!!                                |algal photosynthesis
!!    ai4          |mg O2/mg alg  |the rate of oxygen uptake per unit of algae
!!                                |respiration
!!    ai5          |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                                |nitrogen oxidation
!!    ai6          |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                                |nitrogen oxidation
!!    algae(:)     |mg alg/L      |algal biomass concentration in reach
!!    rch_NH4(:)  |mg N/L        |ammonia concentration in reach
!!    bc1(:)       |1/day         |rate constant for biological oxidation of NH3
!!                                |to NO2 in reach at 20 deg C
!!    bc2(:)       |1/day         |rate constant for biological oxidation of NO2
!!                                |to NO3 in reach at 20 deg C
!!    bc3(:)       |1/day         |rate constant for hydrolysis of organic N to
!!                                |ammonia in reach at 20 deg C
!!    bc4(:)       |1/day         |rate constant for the decay of organic P to
!!                                |dissolved P in reach at 20 deg C
!!    rch_Chla(:)    |mg chl-a/L    |chlorophyll-a concentration in reach
!!    dayl(:)      |hours         |day length for current day
!!    rch_SolP(:)   |mg P/L        |dissolved phosphorus concentration in reach
!!    hru_ra(:)    |MJ/m^2        |solar radiation for the day in HRU
!!    igropt       |none          |Qual2E option for calculating the local
!!                                |specific growth rate of algae
!!                                |1: multiplicative:
!!                                |   u = mumax * fll * fnn * fpp
!!                                |2: limiting nutrient
!!                                |   u = mumax * fll * Min(fnn, fpp)
!!                                |3: harmonic mean
!!                                |   u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
!!    inum1        |none          |reach number
!!    inum2        |none          |inflow hydrograph storage location number
!!    k_l          |MJ/(m2*hr)    |half saturation coefficient for light
!!    k_n          |mg N/L        |michaelis-menton half-saturation constant
!!                                |for nitrogen
!!    k_p          |mg P/L        |michaelis-menton half saturation constant
!!                                |for phosphorus
!!    lambda0      |1/m           |non-algal portion of the light extinction
!!                                |coefficient
!!    lambda1      |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2      |(1/m)(ug chla/L)**(-2/3)
!!                                |nonlinear algal self-shading coefficient
!!    mumax        |1/day         |maximum specific algal growth rate at 20 deg 
!!                                |C
!!    rch_NO3(:)  |mg N/L        |nitrate concentration in reach
!!    rch_NO2(:)  |mg N/L        |nitrite concentration in reach
!!    rch_OrgN(:)  |mg N/L        |organic nitrogen concentration in reach
!!    rch_OrgP(:)  |mg P/L        |organic phosphorus concentration in reach
!!    p_n          |none          |algal preference factor for ammonia
!!    rch_CBOD(:)  |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                |reach 
!!    rch_DOX(:)   |mg O2/L       |dissolved oxygen concentration in reach
!!    rchdep       |m             |depth of flow on day
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!!    rhoq         |1/day         |algal respiration rate at 20 deg C
!!    rk1(:)       |1/day         |CBOD deoxygenation rate coefficient in reach 
!!                                |at 20 deg C
!!    rk2(:)       |1/day         |reaeration rate in accordance with Fickian
!!                                |diffusion in reach at 20 deg C
!!    rk3(:)       |1/day         |rate of loss of CBOD due to settling in reach
!!                                |at 20 deg C
!!    rk4(:)       |mg O2/        |sediment oxygen demand rate in reach
!!                 |  ((m**2)*day)|at 20 deg C
!!    rnum1        |none          |fraction of overland flow
!!    rs1(:)       |m/day         |local algal settling rate in reach at 20 deg
!!                                |C
!!    rs2(:)       |(mg disP-P)/  |benthos source rate for dissolved phosphorus
!!                 |  ((m**2)*day)|in reach at 20 deg C
!!    rs3(:)       |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in
!!                 |  ((m**2)*day)|reach at 20 deg C
!!    rs4(:)       |1/day         |rate coefficient for organic nitrogen
!!                                |settling in reach at 20 deg C
!!    rs5(:)       |1/day         |organic phosphorus settling rate in reach at
!!                                |20 deg C
!!    rttime       |hr            |reach travel time
!!    rtwtr        |m^3 H2O       |flow out of reach
!!    tfact        |none          |fraction of solar radiation computed in the
!!                                |temperature heat balance that is
!!                                |photosynthetically active
!!    tmpav(:)     |deg C         |average air temperature on current day in HRU
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
!!    algae(:)    |mg alg/L      |algal biomass concentration in reach
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
!!    soxy        |mg O2/L       |saturation concetration of dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algi        |MJ/(m2*hr)    |daylight average, photosynthetically active,
!!                               |light intensity
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                               |to NO2 modified to reflect impact of low 
!!                               |oxygen concentration
!!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                               |to NO3 modified to reflect impact of low
!!                               |oxygen concentration
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    cinn        |mg N/L        |effective available nitrogen concentration
!!    cordo       |none          |nitrification rate correction factor
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    f1          |none          |fraction of algal nitrogen uptake from
!!                               |ammonia pool
!!    fl_1        |none          |growth attenuation factor for light, based on
!!                               |daylight-average light intensity
!!    fll         |none          |growth attenuation factor for light averaged
!!                               |over the diurnal cycle
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphorus
!!    gra         |1/day         |local algal growth rate at 20 deg C
!!    jrch        |none          |reach number
!!    lambda      |1/m           |light extinction coefficient
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
!!    tday        |none          |flow duration (fraction of 24 hr)
!!    thbc1       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NH3 to NO2
!!    thbc2       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NO2 to NO3
!!    thbc3       |none          |temperature adjustment factor for local
!!                               |hydrolysis of organic N to ammonia N
!!    thbc4       |none          |temperature adjustment factor for local
!!                               |decay of organic P to dissolved P
!!    thgra       |none          |temperature adjustment factor for local algal
!!                               |growth rate
!!    thrho       |none          |temperature adjustment factor for local algal
!!                               |respiration rate
!!    thrk1       |none          |temperature adjustment factor for local CBOD
!!                               |deoxygenation
!!    thrk2       |none          |temperature adjustment factor for local oxygen
!!                               |reaeration rate
!!    thrk3       |none          |temperature adjustment factor for loss of
!!                               |CBOD due to settling
!!    thrk4       |none          |temperature adjustment factor for local
!!                               |sediment oxygen demand
!!    thrs1       |none          |temperature adjustment factor for local algal
!!                               |settling rate
!!    thrs2       |none          |temperature adjustment factor for local
!!                               |benthos source rate for dissolved phosphorus
!!    thrs3       |none          |temperature adjustment factor for local
!!                               |benthos source rate for ammonia nitrogen
!!    thrs4       |none          |temperature adjustment factor for local
!!                               |organic N settling rate
!!    thrs5       |none          |temperature adjustment factor for local
!!                               |organic P settling rate
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    uu          |varies        |variable to hold intermediate calculation
!!                               |result
!!    vv          |varies        |variable to hold intermediate calculation
!!                               |result
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ww          |varies        |variable to hold intermediate calculation
!!                               |result
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    yy          |varies        |variable to hold intermediate calculation
!!                               |result
!!    zz          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para   !! added by Du
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: jrch
      real :: wtrin, chlin, algin, orgnin, ammoin, nitratin, nitritin
      real :: orgpin, dispin, cbodin, disoxin, tday, wtmp, fll, gra
      real :: lambda, fnn, fpp, algi, fl_1, xx, yy, zz, ww, cinn
      real :: uu, vv, vv_bottom, cordo, f1, algcon, orgncon, nh3con, no2con, no3con
      real :: orgpcon, solpcon, cbodcon, o2con, wtrtot, bc1mod, bc2mod
      real :: thgra = 1.047, thrho = 1.047, thrs1 = 1.024
      real :: thrs2 = 1.074, thrs3 = 1.074, thrs4 = 1.024, thrs5 = 1.024
      real :: thbc1 = 1.083, thbc2 = 1.047, thbc3 = 1.047, thbc4 = 1.047
      real :: thrk1 = 1.047, thrk2 = 1.024, thrk3 = 1.024, thrk4 = 1.060
      !! -----------------------------------------------------------------------------
      real :: lpocin,rpocin,ldocin,rdocin,dicin                   ! carbon concentration in inflow
      real :: lpoc_con,rpoc_con,ldoc_con,rdoc_con,dic_con,dic_bed,wtmpk !initial carbon concentration in reach
      real :: Foxc,Fxodn,f_rdp,f_rdb,kac,kh_DIC,CO2_sat,cbn_bur  !local variables for carbon processes
      real :: Ab_con,Ab_UN,Ab_UP,DIN_con,qN_Ab,qP_Ab,fnl_Ab,fll_Ab,sedc !local variables for botom algae
      real :: fnl_dic,I0_Ab,IH_Ab,ke_Ab,POMcon,fsl_Ab,kph_Ab,foxb_Ab    !local variables for botom algae
      !! -----------------------------------------------------------------------------
      !! ---------------------------------------------
      real :: RP_set_R,LP_set_R
      real ::  thkdp =1.0
      real :: Ksocf
       !! ---------------------------------------------
!      real :: thrk5 = 1.047, thrk6 = 1.0, thrs6 = 1.024, thrs7 = 1.0
      real :: dcoef, Foxrp, coef, cbodrch, doxrch
      real, external :: rch_wtmp_Ficklin, rch_wtmp_Du, Theta


      jrch  = 0
      jrch  = inum1
      dcoef = 3.
      
      ch_area(jrch)= phi(6,jrch)*ch_l2(jrch)*1000  !!m2             phi(6,:)    |m             |bottom width of main channel
                                                   !!ch_l2(:)    |km            |main channel length in subbasin

      !! initialize water flowing into reach
      wtrin = 0.
      wtrin = varoute(2,inum2) * (1. - rnum1)

      if (jrch>0) then
       ! if (wtrin > 1.e-4) then
      !  if (rtwtr / 86400.> 0.01.and.wtrin>0.001) then
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
       
         chlin = 1000. * varoute(13,inum2) * (1. - rnum1) / wtrin
         algin = 1000. * chlin / ai0        !! QUAL2E equation III-1    readwwq-Qi.f90(136):      if (ai0 <= 0.) ai0 = 50  
         orgnin = 1000. * varoute(4,inum2) * (1. - rnum1) / wtrin
         ammoin = 1000. * varoute(14,inum2) * (1. - rnum1) / wtrin
         nitritin = 1000. * varoute(15,inum2) * (1. - rnum1) / wtrin
         nitratin = 1000. * varoute(6,inum2) * (1. - rnum1) / wtrin
         orgpin = 1000. * varoute(5,inum2) * (1. - rnum1) / wtrin
         dispin = 1000. * varoute(7,inum2) * (1. - rnum1) / wtrin
         cbodin = 1000. * varoute(16,inum2) * (1. - rnum1) / wtrin
         disoxin = 1000. * varoute(17,inum2) * (1. - rnum1) / wtrin
         
          !--------------------------------For carbon process in the stream,added by Du-----------------------!
          
         if (cswat == 2 ) then
               !---C with inflow --------------
              rchin_RPOC(jrch)=varoute(33,inum2) * (1. - rnum1)    !! kg
              rchin_LPOC(jrch)=varoute(34,inum2) * (1. - rnum1)    !! kg
              rchin_RDOC(jrch)=varoute(35,inum2) * (1. - rnum1)    !! kg
              rchin_LDOC(jrch)=varoute(36,inum2) * (1. - rnum1)    !! kg
              rchin_DIC(jrch)=varoute(37,inum2) * (1. - rnum1)     !! kg
              rchin_Alg(jrch)=algin * wtrin /1000                  !! kg             
                 !--C at the beginning of the day -------------
              rchini_RPOC(jrch)= cbn_rch(jrch)%RPOC*rchwtr/1000   !!KG
              rchini_LPOC(jrch)= cbn_rch(jrch)%LPOC*rchwtr/1000   !!KG
              rchini_RDOC(jrch)= cbn_rch(jrch)%RDOC*rchwtr/1000    !!KG
              rchini_LDOC(jrch)= cbn_rch(jrch)%LDOC*rchwtr/1000    !!KG
              rchini_DIC(jrch)= cbn_rch(jrch)%DIC*rchwtr/1000            !!KG
              rchini_Alg(jrch)= algin*rchwtr/1000            !!KG
              
              rpocin=0.
              lpocin=0.
              rdocin=0.
              ldocin=0.
              dicin=0. 
              rpocin = 1000. * rchin_RPOC(jrch) / wtrin  ! RPOC concentration in inflow (mg/L)
              lpocin = 1000. * rchin_LPOC(jrch) / wtrin  ! LPOC concentration in inflow (mg/L)
              rdocin = 1000. *rchin_RDOC(jrch) / wtrin  ! RDOC concentration in inflow (mg/L)
              ldocin= 1000. * rchin_LDOC(jrch)/ wtrin  ! LDOC concentration in inflow (mg/L)
              dicin  = 1000. * rchin_DIC(jrch) / wtrin  ! DIC concentration in inflow (mg/L)         
     
         endif
       !-------------------------------For carbon process in the stream,added by Du-----------------------!
     

         !! initialize concentration of nutrient in reach
         wtrtot1 = 0.
         algcon = 0.
         orgncon = 0.
         nh3con = 0.
         no2con = 0.
         no3con = 0.
         orgpcon = 0.
         solpcon = 0.
         cbodcon = 0.
         o2con = 0.
         
         rch_CBOD(jrch) = amax1(1.e-6, rch_CBOD(jrch))                   !!|mg O2/L       |carbonaceous biochemical oxygen demand in reach 
         
         wtrtot1 = wtrin + rchwtr
         algcon = (algin * wtrin + rch_Alg(jrch) * rchwtr) / wtrtot1
         orgncon = (orgnin * wtrin + rch_OrgN(jrch) * rchwtr) / wtrtot1
         nh3con = (ammoin * wtrin + rch_NH4(jrch) * rchwtr) / wtrtot1 
         no2con = (nitritin * wtrin + rch_NO2(jrch) * rchwtr) / wtrtot1
         no3con = (nitratin * wtrin + rch_NO3(jrch) * rchwtr) / wtrtot1
         orgpcon = (orgpin * wtrin + rch_OrgP(jrch) * rchwtr) / wtrtot1
         solpcon = (dispin * wtrin + rch_SolP(jrch) * rchwtr) / wtrtot1
         cbodcon = (cbodin * wtrin + rch_CBOD(jrch) * rchwtr) / wtrtot1
         o2con = (disoxin * wtrin + rch_DOX(jrch) * rchwtr) / wtrtot1
         !-------------------------------For carbon process in the stream, added by Du----------------------------!
         if (cswat == 2 ) then
     
               
             rpoc_con=0.
             lpoc_con=0.
             rdoc_con=0.
             ldoc_con=0.
             dic_con=0. 
     
            rpoc_con=(rpocin*wtrin+cbn_rch(jrch)%RPOC*rchwtr)/wtrtot1   !RPOC intial concentration 
            lpoc_con=(lpocin*wtrin+cbn_rch(jrch)%LPOC*rchwtr)/wtrtot1   !LPOC intial concentration 
            rdoc_con=(rdocin*wtrin+cbn_rch(jrch)%RDOC*rchwtr)/wtrtot1   !RDOC intial concentration
            ldoc_con=(ldocin*wtrin+cbn_rch(jrch)%LDOC*rchwtr)/wtrtot1   !LDOC intial concentration
            dic_con =(dicin*wtrin+cbn_rch(jrch)%DIC*rchwtr)/wtrtot1     !DIC intial concentration
         
           ! Ab_con= ( Ab_rch(jrch)%Ab/dep_chan(jrch)*wtrtot1 /1000 ) / wtrtot1      ! kg > mg/L       
         
         endif
        !-------------------------------For carbon process in the stream, added by Du----------------------------!
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
        
  !!----stream temperature----------------------------
         if (iwtmp_rch == 1) then
           if(wtmp_option == 1) wtmp    = rch_wtmp_Ficklin() !for Ficklin's model
           if(wtmp_option == 2) wtmp    = rch_wtmp_Du()  !for Du's model calculating heat transfer between stream and air       
           if (wtmp <= 0.) wtmp = 0.1     
         else
           wtmp = 0.
           wtmp = 5.0 + 0.75 * tmpav(hru1(jrch))   
           if (wtmp <= 0.) wtmp = 0.1  
         end if 
         wattemp(jrch)  = wtmp
  !!--------stream temperature----------------------------
         !! calculate effective concentration of available nitrogen
         !! QUAL2E equation III-15
         cinn = nh3con + no3con                      !!    cinn        |mg N/L        |effective available nitrogen concentration
         !! end initialize concentrations
        
         !! calculate saturation concentration for dissolved oxygen
         !! QUAL2E section 3.6.1 equation III-29
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
         xx = 6.642308e07 / ((wtmp + 273.15)**2)
         yy = 1.243800e10 / ((wtmp + 273.15)**3)
         zz = 8.621949e11 / ((wtmp + 273.15)**4)
         soxy = Exp(ww - xx + yy - zz)
         if (soxy < 1.e-6) soxy = 0. 

         !! O2 impact calculations
         !! calculate nitrification rate correction factor for low
         !! oxygen QUAL2E equation III-21
         cordo = 0.                              !!    cordo       |none          |nitrification rate correction factor
	     if (o2con.le.0.001) o2con  = 0.001
	     if (o2con.gt.30.) o2con    = 30.
         cordo = 1.0 - Exp(-0.6 * o2con)
         !! modify ammonia and nitrite oxidation rates to account for
         !! low oxygen
         bc1mod = 0.                             !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                                                                          |to NO2 modified to reflect impact of low 
!!                                                                          |oxygen concentration
         bc2mod = 0.                             !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                                                                          |to NO3 modified to reflect impact of low
!!                                                                          |oxygen concentration
         bc1mod = bc1(jrch) * cordo              !!    bc1(:)           |1/hr          |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
         !!    cordo       |none          |nitrification rate correction factor
         bc2mod = bc2(jrch) * cordo              !!    bc2(:)           |1/hr          |rate constant for biological oxidation of NO2 to NO3 in reach at 20 deg C
         !!    cordo       |none          |nitrification rate correction factor
         !! end O2 impact calculations

         !! calculate flow duration
         tday = 0.
         tday = rttime / 24.0           !!rttime      |hr            |reach travel time
         if (tday > 1.0) tday = 1.0     !!    tday        |none          |flow duration (fraction of 24 hr)
            !!     tday = 1.0

         !! algal growth
         !! calculate light extinction coefficient 
         !! (algal self shading) QUAL2E equation III-12
         if (ai0 * algcon > 1.e-6) then             !!    ai0              |ug chla/mg alg| ratio of chlorophyll-a to algal biomass
                                                    !!   readwwq-Qi.f90(136):      if (ai0 <= 0.) ai0 = 50
            lambda = lambda0 + (lambda1 * ai0 * algcon) + lambda2 *  (ai0 * algcon) ** (.66667)
            !!    lambda      |1/m           |light extinction coefficient
            !!    lambda0          |1/m           |non-algal portion of the light extinction coefficient
            !!    lambda1          |1/(m*ug chla/L)|linear algal self-shading coefficient
            !!    lambda2          |(1/m)(ug chla/L)**(-2/3)
         else
           lambda = lambda0
         endif

	     If (lambda > lambda0) lambda = lambda0

         !! calculate algal growth limitation factors for nitrogen
         !! and phosphorus QUAL2E equations III-13 & III-14
         fnn = 0.                               !!    fnn         |none          |algal growth limitation factor for nitrogen
         fpp = 0.                               !!    fpp         |none          |algal growth limitation factor for phosphorus
         fnn = cinn / (cinn + k_n)              !!    k_n              |mg N/L        |michaelis-menton half-saturation constant for nitrogen
         !    cinn        |mg N/L        |effective available nitrogen concentration
         fpp = solpcon / (solpcon + k_p)        !!    k_p              |mg P/L        |michaelis-menton half saturation constant for phosphorus

         !! calculate daylight average, photosynthetically active,
         !! light intensity QUAL2E equation III-8
         !! Light Averaging Option # 2
         algi = 0.
         if (dayl(hru1(jrch)) > 0.) then
           algi = hru_ra(hru1(jrch)) * tfact / dayl(hru1(jrch))
         else
           algi = 0.
         end if
         !!    algi        |MJ/(m2*hr)    |photosynthetically active light intensity for hour
         !!    tfact       |none          |fraction of solar radiation that is photosynthetically active
         

         !! calculate growth attenuation factor for light, based on
         !! daylight average light intensity QUAL2E equation III-7b
         fl_1 = 0.                  !!    fl_1        |none          |growth attenuation factor for light, based on daylight-average light intensity
         fll = 0.                   !!    fll         |none          |growth attenuation factor for light growth attenuation factor for light averaged over the diurnal cycle
         fl_1 = (1. / (lambda * rchdep)) * Log((k_l + algi) / (k_l + algi * (Exp(-lambda * rchdep))))
         fll = 0.92 * (dayl(hru1(jrch)) / 24.) * fl_1
         !!    k_l         |MJ/(m2*hr)    |half saturation coefficient for light
         !!    algi        |MJ/(m2*hr)    |photosynthetically active light intensity for hour
         
         !! calculcate local algal growth rate
         gra = 0.                       !!    gra         |1/hr          |local alga
         select case (igropt)           !!!!    igropt           |none          |Qua
                                        !  |specific growth rate of algae
                                        !  |1: multiplicative:
                                        !  | u = mumax * fll * fnn * fpp
                                        !  |2: limiting nutrient
                                        !  | u = mumax * fll * Min(fnn, fpp)
                                        !  |3: harmonic mean
                                        !  | u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
           case (1)
             !! multiplicative QUAL2E equation III-3a
             gra = mumax * fll * fnn * fpp              !!    fll         |none          |growth attenuation factor for light
             !!    fnn         |none          |algal growth limitation factor for nitrogen
             !!    fpp         |none          |algal growth limitation factor for phosphorus
             !!    gra         |1/hr          |local algal growth rate at 20 deg C
           case (2)
             !! limiting nutrient QUAL2E equation III-3b
             gra = mumax * fll * Min(fnn, fpp)
           case (3)
             !! harmonic mean QUAL2E equation III-3c
             if (fnn > 1.e-6 .and. fpp > 1.e-6) then
               gra = mumax * fll * 2. / ((1. / fnn) + (1. / fpp))
             else
               gra = 0.
             endif
         end select

         Foxrp  = 0.
         Ksocf  = cbn_rchpara(jrch)%ksdocf                            !allocate (cbn_rchpara(mch))     !carbon parameters in the reach/stream
         !Oxygen inhib parameter CBOD oxidation	0.60	L/mgO2	Ksocf	No	0.60	0.60
         select case(1)
         case(1)
            Foxrp   = o2con / (Ksocf + o2con)
         case(2)
            Foxrp   = (1.-exp(Ksocf * o2con))
         case(3)
           Foxrp    = o2con**2 / (Ksocf + o2con**2)
         end select
         Foxrp  = Max(0., Foxrp)
         Foxrp  = Min(1., Foxrp)
           
         kdp    = 0.5
        
        
         !!  balance term-----------------------------------
         ! rs1(jrch)=0.1
         rch_Alg_Growth = 0.
         rch_Alg_Death  = 0.
         rch_Alg_Resp   = 0.
         rch_Alg_Set    = 0.
         rch_Alg_Growth(jrch)   = Theta(gra,thgra,wtmp) * algcon * tday                      !! mg/L   !!    tday        |none          |flow duration (fraction of 24 hr)
         rch_Alg_Death(jrch)    = Theta(kdp,thkdp,wtmp) * algcon * tday                      !! mg/L    !!    tday        |none          |flow duration (fraction of 24 hr)
         rch_Alg_Resp(jrch) = Foxrp*Theta(rhoq,thrho,wtmp) * algcon * tday             !! mg/L        !!    tday        |none          |flow duration (fraction of 24 hr)
         rch_Alg_Set(jrch)  = Theta(rs1(jrch),thrs1,wtmp) / rchdep*algcon * tday               !! mg/L   !!    tday        |none          |flow duration (fraction of 24 hr)
                                                                

         !! calculate algal biomass concentration at end of day
         !! (phytoplanktonic algae)
         !! QUAL2E equation III-2
         rch_Alg(jrch) = 0.
         rch_Alg(jrch) = algcon + (rch_Alg_Growth(jrch) - rch_Alg_Resp(jrch) - rch_Alg_Set(jrch) - rch_Alg_Death(jrch)) 
         if (rch_Alg(jrch) < 1.e-6) then
            rch_Alg(jrch)   = 0.
            rch_Alg_Set(jrch)   = algcon + (rch_Alg_Growth(jrch) - rch_Alg_Resp(jrch) - rch_Alg_Death(jrch)) 
         end if
	     !! JGA added to set algae limit *****
	     !   if (rch_Alg(jrch) > 5000.) rch_Alg(jrch) = 5000.
         !  if (rch_Alg(jrch) > dcoef * algcon) rch_Alg(jrch) = dcoef * algcon

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1
         rch_Chla(jrch) = 0.
         rch_Chla(jrch) = rch_Alg(jrch) * ai0 / 1000.           !readwwq-Qi.f90(136):      if (ai0 <= 0.) ai0 = 50 !|ug chla/mg alg|ratio of chlorophyll-a to algal biomass
         
         !Dry weight	100	gD	gD	No	100	100
         !Chlorophyll	0.5	gA	gA	No	0.5	2
         !based on qual2k, ai0 is estimated as ca. 500/100 or 5 ug chla / mg alg
         !basins.wwp: 50.000    | AI0 : Ratio of chlorophyll-a to algal biomass [µg-chla/mg algae]

         
         !! end algal growth 


         !! CBOD in the original SWAT is not simulated in this new version, as we explicitly simulate RPOC, LPOC, RDOC, LDOC
         !! Although we did not comment out the following code regarding CBOD, it is not used, and CBOD output should not be used.
         
         !!=========================================================
         !! calculate carbonaceous biological oxygen demand at end
         !! of day QUAL2E section 3.5 equation III-26
         yy = 0.
         zz = 0. 
         yy = Theta(rk1(jrch),thrk1,wtmp) * cbodcon             
         !!    rk1(:)           |1/hr          |CBOD deoxygenation rate coefficient in reach at 20 deg C
         !!    thrk1       |none          |temperature adjustment factor for local CBOD deoxygenation
         zz = Theta(rk3(jrch),thrk3,wtmp) * cbodcon             
         !!    rk3(:)           |1/hr          |rate of loss of CBOD due to settling in reach at 20 deg C
         !!    thrk3       |none          |temperature adjustment factor for loss of CBOD due to settling
         rch_CBOD(jrch) = 0.
         rch_CBOD(jrch) = cbodcon - (yy + zz) * tday            !!    tday        |none          |flow duration (fraction of 24 hr)
         
         coef   = 0.
         !!deoxygenation rate
         coef   = exp(-Theta(rk1(jrch),thrk1,wtmp) * tday)        !!    tday        |none          |flow duration (fraction of 24 hr)
         cbodrch    = coef * cbodcon
         !!cbod rate loss due to settling
         coef   = exp(-Theta(rk3(jrch),thrk3,wtmp) * tday)        !!    tday        |none          |flow duration (fraction of 24 hr)
         cbodrch    = coef * cbodrch
         
         rch_CBOD(jrch) = cbodrch
         if (rch_CBOD(jrch) < 1.e-6) rch_CBOD(jrch) = 0.
	     if (rch_CBOD(jrch) > dcoef * cbodcon) rch_CBOD(jrch) = dcoef * cbodcon
	     !!=========================================================
	     !!Note that the above code should not be used.


!! nitrogen calculations
         !! calculate organic N concentration at end of day
         !! QUAL2E section 3.3.1 equation III-16
         xx = 0.
         yy = 0.
         zz = 0.
         xx = ai1 * Theta(rhoq,thrho,wtmp) * algcon             
         !!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
         !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
         !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
         yy = Theta(bc3(jrch),thbc3,wtmp) * orgncon             
         !!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
         !!    thbc3       |none          |temperature adjustment factor for local hydrolysis of organic N to ammonia N
         zz = Theta(rs4(jrch),thrs4,wtmp) * orgncon             
         !!    rs4(:)           |1/hr          |rate coefficient for organic nitrogen settling in reach at 20 deg C
         !!    thrs4       |none          |temperature adjustment factor for local organic N settling rate
!        red_fac = orgncon / 4.
!        if (red_fac > 0.75) red_fac = 0.75
!        zz = zz + red_fac
         rch_OrgN(jrch) = 0.
         rch_OrgN(jrch) = orgncon + (xx - yy - zz) * tday                   !!    tday        |none          |flow duration (fraction of 24 hr)
         if (rch_OrgN(jrch) < 1.e-6) rch_OrgN(jrch) = 0.
   	     if(rch_OrgN(jrch) > dcoef * orgncon) rch_OrgN(jrch) = dcoef * orgncon

         !! calculate fraction of algal nitrogen uptake from ammonia
         !! pool QUAL2E equation III-18
         f1 = 0.                                                             !!    f1          |none          |fraction of algal nitrogen uptake from ammonia pool
         f1 = p_n * nh3con / (p_n * nh3con + (1. - p_n) * no3con + 1.e-6)

         !! calculate ammonia nitrogen concentration at end of day
         !! QUAL2E section 3.3.2 equation III-17
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ww = Theta(bc3(jrch),thbc3,wtmp) * orgncon                      
         !!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
         !!    thbc3       |none          |temperature adjustment factor for local hydrolysis of organic N to ammonia N
         xx = Theta(bc1mod,thbc1,wtmp) * nh3con                          
         !!    bc1(:)           |1/hr          |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
         !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3 to NO2 modified to reflect impact of low oxygen concentration
         !!    thbc1       |none          |temperature adjustment factor for local biological oxidation of NH3 to NO2
         yy = Theta(rs3(jrch),thrs3,wtmp) / (rchdep * 1000.)             
         !!    rs3(:)           |(mg NH4-N)/   |benthos source rate for ammonia nitrogen ((m**2)*hr) |in reach at 20 deg C
         !!    thrs3       |none          |temperature adjustment factor for local benthos source rate for ammonia nitrogen
         zz = f1 * ai1 * algcon * Theta(gra, thgra, wtmp)                  
         !!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
         !!    f1          |none          |fraction of algal nitrogen uptake from ammonia pool
         !!    gra         |1/hr          |local algal growth rate at 20 deg C
         !!    thgra       |none          |temperature adjustment factor for local algal growth rate
         rch_NH4(jrch) = 0.
         rch_NH4(jrch) = nh3con + (ww - xx + yy - zz) * tday             !!    tday        |none          |flow duration (fraction of 24 hr)
         if (rch_NH4(jrch) < 1.e-6) rch_NH4(jrch) = 0.
         if (rch_NH4(jrch) > dcoef * nh3con .and. nh3con > 0.)  rch_NH4(jrch) = dcoef * nh3con  

         !! calculate concentration of nitrite at end of day
         !! QUAL2E section 3.3.3 equation III-19
         yy = 0.
         zz = 0.
         yy = Theta(bc1mod,thbc1,wtmp) * nh3con               !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3 to NO2 modified to reflect impact of low oxygen concentration
         !!    thbc1       |none          |temperature adjustment factor for local biological oxidation of NH3 to NO2
         zz = Theta(bc2mod,thbc2,wtmp) * no2con               !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2 to NO3 modified to reflect impact of low oxygen concentration
         !!    thbc2       |none          |temperature adjustment factor for local biological oxidation of NO2 to NO3
         rch_NO2(jrch) = 0.
         rch_NO2(jrch) = no2con + (yy - zz) * tday                   !!    tday        |none          |flow duration (fraction of 24 hr)
         if (rch_NO2(jrch) < 1.e-6) rch_NO2(jrch) = 0.
	     if (rch_NO2(jrch) > dcoef * no2con .and. no2con > 0.)   rch_NO2(jrch) = dcoef * no2con

         !! calculate nitrate concentration at end of day
         !! QUAL2E section 3.3.4 equation III-20
         yy = 0.
         zz = 0.
         yy = Theta(bc2mod,thbc2,wtmp) * no2con                      !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2 to NO3 modified to reflect impact of low oxygen concentration       
         !!    thbc2       |none          |temperature adjustment factor for local biological oxidation of NO2 to NO3
         zz = (1. - f1) * ai1 * algcon * Theta(gra,thgra,wtmp)       !!    ai1              |mg N/mg alg   |fraction of algal biomass that is N
         !!    f1          |none          |fraction of algal nitrogen uptake from ammonia pool
         !!    gra         |1/hr          |local algal growth rate at 20 deg C
         !!    thgra       |none          |temperature adjustment factor for local algal growth rate
         rch_NO3(jrch) = 0.
         rch_NO3(jrch) = no3con + (yy - zz) * tday                           !!    tday        |none          |flow duration (fraction of 24 hr)
         if (rch_NO3(jrch) > dcoef * no3con) rch_NO3(jrch) = dcoef *  no3con
	
         if (rch_NO3(jrch) < 1.e-6) rch_NO3(jrch) = 0.
!! end nitrogen calculations

!! phosphorus calculations
         !! calculate organic phosphorus concentration at end of
         !! day QUAL2E section 3.3.6 equation III-24
         xx = 0.
         yy = 0.
         zz = 0.
         xx = ai2 * Theta(rhoq,thrho,wtmp) * algcon          !!    ai2              |mg P/mg alg   |fraction of algal biomass that is P
         !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
         !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
         yy = Theta(bc4(jrch),thbc4,wtmp) * orgpcon          !!    bc4(:)           |1/hr          |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
         !!    thbc4       |none          |temperature adjustment factor for local decay of organic P to dissolved P
         zz = Theta(rs5(jrch),thrs5,wtmp) * orgpcon          !!    rs5(:)           |1/hr          |organic phosphorus settling rate in reach at 20 deg C
         !!    thrs5       |none          |temperature adjustment factor for local organic P settling rate
         rch_OrgP(jrch) = 0.
         rch_OrgP(jrch) = orgpcon + (xx - yy - zz) * tday                        !!    tday        |none          |flow duration (fraction of 24 hr)
         if (rch_OrgP(jrch) < 1.e-6) rch_OrgP(jrch) = 0.
         if (rch_OrgP(jrch) > dcoef * orgpcon) rch_OrgP(jrch) = dcoef * orgpcon

         !! calculate dissolved phosphorus concentration at end
         !! of day QUAL2E section 3.4.2 equation III-25
         xx = 0.
         yy = 0.
         zz = 0.
         xx = Theta(bc4(jrch),thbc4,wtmp) * orgpcon              !!    bc4(:)           |1/hr          |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
         !!    thbc4       |none          |temperature adjustment factor for local decay of organic P to dissolved P
         yy = Theta(rs2(jrch),thrs2,wtmp) / (rchdep * 1000.)     !!    rs2(:)           |(mg disP-P)/  |benthos source rate for dissolved P ((m**2)*hr) |in reach at 20 deg C
         !!    thrs2       |none          |temperature adjustment factor for local benthos source rate for dissolved phosphorus
         zz = ai2 * Theta(gra,thgra,wtmp) * algcon               !!    ai2              |mg P/mg alg   |fraction of algal biomass that is P
         !!    gra         |1/hr          |local algal growth rate at 20 deg C
         !!    thgra       |none          |temperature adjustment factor for local algal growth rate
         rch_SolP(jrch) = 0.
         rch_SolP(jrch) = solpcon + (xx + yy - zz) * tday                            !!    tday        |none          |flow duration (fraction of 24 hr)
         if (rch_SolP(jrch) < 1.e-6) rch_SolP(jrch) = 0.
	     if (rch_SolP(jrch) > dcoef * solpcon) rch_SolP(jrch) = dcoef * solpcon   
!! end phosphorus calculations



!/////////////For calculating carbon transformations and concentration change, added by Du-//////////////////////////////////////////////////////////////////////////////////!
         if (cswat == 2 ) then !calcualte carbon processes in stream
    
                if (i == 1 .and. curyr == 1) then
                    Ab_rch(jrch)%Ab = 100.
                    
                    Ab_rch(jrch)%INb = Ab_rch(jrch)%Ab * Ab_rchpara(jrch)%qoN
                    Ab_rch(jrch)%IPb = Ab_rch(jrch)%Ab * Ab_rchpara(jrch)%qoP
                    
                end if
                
                !For calculating bottom algae processes, added by Du>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
                Ab_con  = 0.
                Ab_con  = Ab_rch(jrch)%Ab
                !bottom algea death procese
                Ab_rch(jrch)%death  = 0.
                Ab_rch(jrch)%death  = Ab_con * Theta(Ab_rchpara(jrch)%kdb,1.04,wtmp) * tday           ! bottom algea death biomass (g/m2/day)    
                !kdb=0.02        !bottom algae death rate (/day)
                !----bottom algae photosynthesis/growth process
                !For calculating nutrient limitation factor
                qN_Ab   = Ab_rch(jrch)%INb / Ab_con         !the cell quotas of N (mgN/gD) !INb=0.01      !state variable, intracellular N concentration (mg N/m2),intial condition
                qP_Ab   = Ab_rch(jrch)%IPb / Ab_con         !the cell quotas of P (mgP/gD) IPb=0.01      !state variable, intracellular P concentration (mg P/m2),intial condition
                !INb and IPb need to be checked. According to QUAL2K, qN_Ab and qP_Ab should be the following.
                !Subsistence quota for nitrogen	4.17401784	mgN/gD	q0N	Yes	7.2	36
                !Subsistence quota for phosphorus	1.92872435	mgP/gD	q0P	Yes	1	5

           
                fnl_dic = 0.
                fnl_dic = Ab_rchpara(jrch)%fdic * cbn_rch(jrch)%DIC  !H2CO3* and HCO3- concentration (mgC/L)           
                !fdic=0.6        !the fraction of DIC in H2CO3* and HCO3-,for calculating nutrient limitation
                fnl_Ab  = 0.
                fnl_Ab  = min((1. - Ab_rchpara(jrch)%qoN / qN_Ab),(1. - Ab_rchpara(jrch)%qoP / qP_Ab), (fnl_dic / (fnl_dic + Ab_rchpara(jrch)%kscb))) !nutrient limitation factor
                !qoN=4.17        !minimum cell quotas of N (mgN/gD/day)
                !qoP=1.93         !minimum cell quotas of P (mgN/gD/day)
                !kscb=0.792      !Inorganic carbon half saturation constant (mg/L,unit converted)
           
                !IF(JRCH==1) THEN
                ! PRINT*,fnl_Ab,fnl_dic,Ab_rchpara(jrch)%fdic
                !END IF
                if (fnl_Ab < 0.) fnl_Ab   = 0. !in case qN_Ab<Ab_rchpara(jrch)%qoN
                !kinetics for intracellular N and P process in bottom algae
                Ab_UN    = 0.     !uptake rates for N in bottom algae (mgN/m2/day)
                Ab_UP    = 0.     !uptake rates for P in bottom algae (mgP/m2/day)
                DIN_con = nh3con + no3con + no2con !calculate DIN concentration in the reach(mgN/L)
                Ab_UN    = Ab_rchpara(jrch)%pmN * (DIN_con/(DIN_con+Ab_rchpara(jrch)%Ksnb)) * Ab_con * (Ab_rchpara(jrch)%KqN / (Ab_rchpara(jrch)%KqN + &
                            (qN_Ab - Ab_rchpara(jrch)%qoN)))  !calculating N uptakes in bottom algae
                if (Ab_UN < 0.) Ab_UN = 0.
                Ab_UP    = Ab_rchpara(jrch)%pmP * (solpcon/(solpcon+Ab_rchpara(jrch)%Kspb)) * Ab_con * (Ab_rchpara(jrch)%KqP / (Ab_rchpara(jrch)%KqP + &
                            (qP_Ab - Ab_rchpara(jrch)%qoP))) !calculating P uptakes in bottom algae
           
                if (Ab_UP < 0.) Ab_UP = 0.
                Ab_rch(jrch)%INb    = Ab_rch(jrch)%INb + Ab_UN - qN_Ab * Ab_rch(jrch)%death - qN_Ab * Theta(Ab_rchpara(jrch)%kexb, 1.04, wtmp) * Ab_con    !intracellular N mass balance
           
                if(Ab_rch(jrch)%INb < 0.) Ab_rch(jrch)%INb    = 0.
                Ab_rch(jrch)%IPb    = Ab_rch(jrch)%IPb+Ab_UP - qP_Ab * Ab_rch(jrch)%death - qP_Ab * Theta(Ab_rchpara(jrch)%kexb,1.04,wtmp) * Ab_con    !intracellular P mass balance
           
                if (Ab_rch(jrch)%IPb < 0.) Ab_rch(jrch)%IPb   = 0.
                !For calculating light limation factor
                I0_Ab   = 0.
                I0_Ab   = hru_ra(hru1(jrch)) * tfact    !photosynthetically-active solar radiation reaching water surface
                POMcon  = (cbn_rch(jrch)%LPOC + cbn_rch(jrch)%RPOC) / 0.58 !Convert POC to POM concentration (mgDM/L)
                sedc=0.
                if (rchwtr>0.) sedc = sedst(jrch) / rchwtr * 1.e6  !TSS concentration (mg/L)
                ke_Ab   = Ab_rchpara(jrch)%keb + Ab_rchpara(jrch)%kiss * sedc + Ab_rchpara(jrch)%kpom *            &
                        POMcon + Ab_rchpara(jrch)%kap * algcon+Ab_rchpara(jrch)%kapn * algcon**(.66667) +         &
                        Ab_rchpara(jrch)%kAb * Ab_con/rchdep               !the light extinction coefficient (/m)
                !keb=0.02        !background light extinction coefficient (/m)
                !kiss=0.052      !light extinction coefficient by inorganic suspended solids (L/mgD/m)
                !kpom=0.174      !light extinction coefficient by particular organic matter (POM) (L/mgD/m)
                !kap=0.0088      !linear light extinction coefficient by algae (L/ugA/m)
                !kapn=0.054      !non-linear light extinction coefficient by algae (L/ugA)2/3/m
                !kAb=0.024       !linear light extinction coefficient by bottom [m3/gD/m]
           
                IH_Ab   = 0.
                IH_Ab   = I0_Ab * exp(-ke_Ab * rchdep)          !the quantity of bottom light
                fll_Ab  = 0.
                select case (Ab_rchpara(jrch)%light)     ! caculating light limitation factor using different light models
                    case (1)
                        !! Half-Saturation Light Model 
                        fll_Ab  = IH_Ab/(Ab_rchpara(jrch)%klb+IH_Ab)   !klb=1.5807      !bottom algae light parameter (MJ/m2/day) 
                        !Light constant	37.7785	langleys/d	KLb	Yes	40	110 
                        !1 langley = 0.04184 MJ/m2
                    case (2)
                        !! Smith¡¯s Function for light model 
                        fll_Ab  = IH_Ab/(Ab_rchpara(jrch)%klb**2+IH_Ab**2)**(0.5)
                    case (3)
                        !! Steele¡¯s Equation for light model
                        fll_Ab  = (IH_Ab/Ab_rchpara(jrch)%klb)*exp(1+IH_Ab/Ab_rchpara(jrch)%klb)
                end select
                kph_Ab  = 0.
                kph_Ab  = Theta(Ab_rchpara(jrch)%kph, 1.04, wtmp)       !temperature correction for photosynthesis rate
                !kph=36.2        ! maximum photosynthesis rate [d-1 or gD/m2/d] 
                
                Ab_rch(jrch)%photo  = 0.               !!pathway,bottom algae photosynthesis biomass (gDM/m2/day)
                if (Ab_rchpara(jrch)%ipho == 0) then !ipho=1       ! photosynthesis model option (0, zero order; 1, first order)
                    Ab_rch(jrch)%photo  = kph_Ab * fll_Ab * fnl_Ab * tday                                        
                else    !first order growth model
                    fsl_Ab  = 0.
                    fsl_Ab  = max(1 - Ab_con / Ab_rchpara(jrch)%Abmax,0.) ! Space limitation factor
                    Ab_rch(jrch)%photo  = kph_Ab * fll_Ab * fnl_Ab * fsl_Ab * Ab_con * tday   
                endif
                
                !bottom algea respiration process
                foxb_Ab = 0.
                foxb_Ab = o2con / (o2con + Ab_rchpara(jrch)%ksob)           !ksob=0.6        !half-saturation oxygen inhibition constant for respiration 
                Ab_rch(jrch)%resp   = 0.                             !resp=0.          !pathway,bottom algae respiration biomass (gDM/m2/day)
                Ab_rch(jrch)%resp   = foxb_Ab*(Ab_rchpara(jrch)%krb1*Ab_con*tday + Ab_rchpara(jrch)%krb2*Ab_rch(jrch)%photo) 
                             !*tday         
                !Bottom algae biomass mass balance
                Ab_rch(jrch)%Ab = Ab_rch(jrch)%Ab + Ab_rch(jrch)%photo - Ab_rch(jrch)%resp - Ab_rch(jrch)%death
                if (Ab_rch(jrch)%Ab<0.) Ab_rch(jrch)%Ab = 0.000001
                !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                
                !----------------For calculating carbon transformations and concentration change---------------------------------------!
                !calculating algal death to organic carbon
                cbn_rec%Alg_cbn = 0.
                cbn_rec%Alg_cbn = cbn_rchpara(jrch)%rca * rch_Alg_Death(jrch)                            !! Floating Algea death to total organic carbon (mg/L)
                !rca=0.4         !algal carbon to biomass ratio
                cbn_rec%Ab_cbn  = 0.
                cbn_rec%Ab_cbn  = cbn_rchpara(jrch)%rca * Ab_rch(jrch)%death *(ch_area(jrch) / wtrtot1)     !! !Bottom Algae death to total organic carbon (mg/L)  g/m2 to mg/L
                !death=0.         !pathway,bottom algae death biomass (g/m2/day)                
      
                f_rdp   =0.
                f_rdp   = 1. - cbn_rchpara(jrch)%f_lpp - cbn_rchpara(jrch)%f_rpp - cbn_rchpara(jrch)%f_ldp      !fraction of algal death to RDOC
                !f_lpp=0.1       !fraction of algal mortality into LPOC       
                !f_ldp=0.05      !fraction of algal mortality into LDOC
                !f_rpp=0.8       !fraction of algal mortality into RPOC
                if(f_rdp < 0. .or. f_rdp > 0.2 ) then 
                    cbn_rchpara(jrch)%f_lpp =0.1
                    cbn_rchpara(jrch)%f_rpp =0.8
                    cbn_rchpara(jrch)%f_ldp =0.05
                    f_rdp=0.05
                end if
                if (cbn_rec%Alg_cbn >0.) then   
                    cbn_rec%Alg_LP  =0.
                    cbn_rec%Alg_LP  =cbn_rchpara(jrch)%f_lpp*cbn_rec%Alg_cbn                     !  Floating  algae to LPOC    mg/L
                    !f_lpp=0.1       !fraction of algal mortality into LPOC
                    cbn_rec%Alg_RP  =0.
                    cbn_rec%Alg_RP  =cbn_rchpara(jrch)%f_rpp*cbn_rec%Alg_cbn                     !  Floating Algal death to RPOC mg/L
                    !f_rpp=0.8       !fraction of algal mortality into RPOC
                    cbn_rec%Alg_LD  =0.
                    cbn_rec%Alg_LD  =cbn_rchpara(jrch)%f_ldp*cbn_rec%Alg_cbn                     !  Floating Algal death to LDOCmg/L
                    !f_lpb=0.1       !fraction of bottom algal mortality into LPOC
                    cbn_rec%Alg_RD  =0.
                    cbn_rec%Alg_RD  =f_rdp*cbn_rec%Alg_cbn                                  !  Floating Algal death to RDOCmg/L
                    !f_rdp::fraction of algal death to RDOC; f_rdp=1.-cbn_rchpara(jrch)%f_lpp-cbn_rchpara(jrch)%f_rpp-cbn_rchpara(jrch)%f_ldp
                end if
                
                f_rdb   =0.
                f_rdb   =1. - cbn_rchpara(jrch)%f_lpb - cbn_rchpara(jrch)%f_rpb - cbn_rchpara(jrch)%f_ldb      !fraction of algal death to RDOC
                !f_lpb=0.1       !fraction of bottom algal mortality into LPOC
                !f_rpb=0.8       !fraction of bottom algal mortality into RPOC
                !f_ldb=0.05      !fraction of bottom algal mortality into LDOC
                if(f_rdb < 0. .or. f_rdb > 0.2 ) then 
                    cbn_rchpara(jrch)%f_lpb =0.1
                    cbn_rchpara(jrch)%f_rpb =0.8
                    cbn_rchpara(jrch)%f_ldb =0.05
                    f_rdb   =0.05
                end if
                if( cbn_rec%Ab_cbn > 0.) then   
                   cbn_rec%Ab_LP    =0.
                   cbn_rec%Ab_LP    =cbn_rchpara(jrch)%f_lpb*cbn_rec%Ab_cbn                       ! bottom algae to LPOC mgC/L
                   !f_lpb=0.1       !fraction of bottom algal mortality into LPOC
                   cbn_rec%Ab_RP    =0.
                   cbn_rec%Ab_RP    =cbn_rchpara(jrch)%f_rpb*cbn_rec%Ab_cbn                      ! bottom algae to RPOC  mgC/L      
                   !f_rpb=0.8       !fraction of bottom algal mortality into RPOC
                   cbn_rec%Ab_LD    =0.
                   cbn_rec%Ab_LD    =cbn_rchpara(jrch)%f_ldb*cbn_rec%Ab_cbn                         ! bottom algae death to LDOC mgC/L
                   !f_ldb=0.05      !fraction of bottom algal mortality into LDOC
                   cbn_rec%Ab_RD    =0.
                   cbn_rec%Ab_RD    =f_rdb*cbn_rec%Ab_cbn                                            ! bottom algae to RDOC mgC/L
                   !f_rdb=1.-cbn_rchpara(jrch)%f_lpb-cbn_rchpara(jrch)%f_rpb-cbn_rchpara(jrch)%f_ldb
                end if

                !LPOC's reaction pathways>>>>>>> 6 >>>>>>>>>>>>>>>>>>>>>>>>
           
                LP_set_R    = 0.
                LP_set_R    = cbn_rchpara(jrch)%sv_lp / rchdep                                       !sv_rp=2.5      !RPOC settling velocity (m/day)
                IF(LP_set_R>1.) LP_set_R    = 1.
                cbn_rec%LP_set  = 0.                                                        !LP_set                  ! LPOC -> bed settling     (mg-C/L/day)
                cbn_rec%LP_set  = LP_set_R * lpoc_con * tday                                              !LPOC settling to bed

                cbn_rec%LP_LD   = 0.                                                         !LP_LD=0.            ! LPOC dissolution to LDOC (mg-C/L/day)
                cbn_rec%LP_LD   = Theta((cbn_rchpara(jrch)%klp), 1.047, wtmp) * lpoc_con * tday         !LPOC dissolution to LDOC
                !klp =0.075      !LPOC dissolution rate (/day) LPOC->LDOC
                cbn_rec%LP_DIC  = 0.
                cbn_rec%LP_DIC  = Theta((cbn_rchpara(jrch)%kd_lp), 1.047, wtmp) * lpoc_con * tday           !LPOC decay to DIC
                !kd_lp=0.08     !LPOC decay rate to DIC (/day) LPOC->DIC                                                                                                             
                cbn_rec%LR_POC  = 0.
                cbn_rec%LR_POC  = Theta((cbn_rchpara(jrch)%klrp), 1.047, wtmp) * lpoc_con * tday     ! LPOC decay to RPOC     
                !klrp=0.01       !decay rate of LPOC to RPOC (/day) LPOC->RPOC                                        
                cbn_rch(jrch)%LPOC  = 0.
                cbn_rch(jrch)%LPOC  = lpoc_con + (cbn_rec%Alg_LP + cbn_rec%Ab_LP        &  
                                    - cbn_rec%LP_set - cbn_rec%LP_LD - cbn_rec%LP_DIC                      &
                                    - cbn_rec%LR_POC) !*tday !update LPOC concentration for transformations  
                if (cbn_rch(jrch)%LPOC < 0.) then
                    cbn_rch(jrch)%LPOC  = 0.
                    !correct LPOC settling amount using mass balance equation if settling is too much
                    cbn_rec%LP_set  = lpoc_con+cbn_rec%Alg_LP + cbn_rec%Ab_LP		&
                                        - cbn_rec%LP_LD - cbn_rec%LP_DIC - cbn_rec%LR_POC
                endif  !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           
                !RPOC reaction pathway>>>>>> 6 >>>>>>>>>>>>>>>>>>>>>>>>>>>
                cbn_rec%RP_LD   = 0. 
                cbn_rec%RP_LD   = Theta((cbn_rchpara(jrch)%krp), 1.047, wtmp) * rpoc_con * tday      !RPOC dissolution to LDOC
                !krp=0.0025      !RPOC dissolution rate (/day) RPOC->LDOC
                cbn_rec%RP_DIC  = 0.
                cbn_rec%RP_DIC  = Theta((cbn_rchpara(jrch)%kd_rp), 1.047, wtmp) * rpoc_con * tday        !RPOC decay to DIC
                !kd_rp=0.001     !RPOC decay rate to DIC (/day) RPOC->DIC
                                                                    
                cbn_rec%RP_set  = 0.

                RP_set_R    = 0.
                RP_set_R    = cbn_rchpara(jrch)%sv_rp / rchdep                   !sv_rp=2.5      !RPOC settling velocity (m/day)
                IF(RP_set_R>1. ) RP_set_R   = 1.
                cbn_rec%RP_set  = RP_set_R * rpoc_con * tday                !RPOC settling to bed

                cbn_rch(jrch)%RPOC  = rpoc_con + (cbn_rec%Alg_RP + cbn_rec%Ab_RP        &
                                    - cbn_rec%RP_set - cbn_rec%RP_LD - cbn_rec%RP_DIC                      &
                                    + cbn_rec%LR_POC) !*tday !update RPOC concentration for transformations  
                if (cbn_rch(jrch)%RPOC < 0.) then
                    cbn_rch(jrch)%RPOC  = 0.
                    !correct RPOC settling amount using mass balance equation if settling is too much
                    cbn_rec%RP_set  = rpoc_con + cbn_rec%Alg_RP + cbn_rec%Ab_RP         &
                                    - cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC
           
                endif !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          
          
                Foxc    = 0.
                Foxc    = o2con / (o2con + cbn_rchpara(jrch)%ksdocf)                                       !ksdocf=1.0      !half-saturation oxygen attenuation for DOC oxidation (mgO2/L)
           
                !LDOC reaction pathways >>>>>> 7 >>>>>>>>>>>>>>>>>>>>>>>>>>
                cbn_rec%LD_DIC  = 0.
                cbn_rec%LD_DIC  = Foxc*Theta((cbn_rchpara(jrch)%kld), 1.047, wtmp) * ldoc_con * tday      !LDOC decay to DIC
                !kld=0.25         !LDOC mineralization rate (/day)    LDOC->DIC !!
                                                         
                cbn_rec%LR_DOC  =0.
                cbn_rec%LR_DOC  = Theta((cbn_rchpara(jrch)%klrd), 1.047, wtmp) * ldoc_con * tday  	    ! LDOC decay to RDOC
                !klrd=0.01       !decay rate of LDOC to RDOC (/day) LDOC->RDOC
                                                     
                cbn_rec%LD_NO3=0.       !!LDOC consumed by NO3- denitrification (mg-C/L/day)
                Fxodn   = 0.
                Fxodn   = o2con / (o2con + cbn_rchpara(jrch)%ksoxdn)                                      !ksoxdn=0.1      !half-saturation oxygen attenuation for denitrification (mgO2/L)
                cbn_rec%LD_NO3  = no3con * (15.0 / 14.0) * (1. - Fxodn) * Theta((cbn_rchpara(jrch)%kdnit), 1.045, wtmp) * tday  !LDOC consumbed by NO3 denitrification    
                !kdnit=0.002     !NO3 denitrification rate (/day) LDOC consumned              
      
                cbn_rch(jrch)%LDOC  = ldoc_con + (cbn_rec%Alg_LD + cbn_rec%Ab_LD                &
                                    + cbn_rec%LP_LD + cbn_rec%RP_LD                        &
                                    - cbn_rec%LR_DOC - cbn_rec%LD_DIC - cbn_rec%LD_NO3) !*tday           !update LDOC concentration for transformations  
                                    
                ! if (cbn_rch(jrch)%LDOC<0.) cbn_rch(jrch)%LDOC=0.    
                if( cbn_rch(jrch)%LDOC < 0.) then   
                    cbn_rch(jrch)%LDOC  = 0.
                    cbn_rec%LD_DIC  = ldoc_con + (cbn_rec%Alg_LD + cbn_rec%Ab_LD                    &
                                     + cbn_rec%LP_LD + cbn_rec%RP_LD                       &
                                     - cbn_rec%LR_DOC - cbn_rec%LD_NO3)
                end if

        
                !RDOC reation pathways
                cbn_rec%RD_DIC  = 0.
                cbn_rec%RD_DIC  = Foxc * Theta((cbn_rchpara(jrch)%krd), 1.047, wtmp) * rdoc_con * tday       !RDOC decay to DIC
                !krd=0.006       ! RDOC mineralization rate (/day) RDOC->DIC  !!                                                

                cbn_rch(jrch)%RDOC  = rdoc_con + (cbn_rec%Alg_RD + cbn_rec%Ab_RD + cbn_rec%LR_DOC - cbn_rec%RD_DIC)   !*tday    !update RDOC concentration for transformations 
                if (cbn_rch(jrch)%RDOC < 0.) then   
                    cbn_rch(jrch)%RDOC  =0.
                    cbn_rec%RD_DIC  = rdoc_con+(cbn_rec%Alg_RD+cbn_rec%Ab_RD +cbn_rec%LR_DOC)
                    !if( cbn_rec%RD_DIC<0.) cbn_rec%RD_DIC=0.
                end if
                !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
           
                !DIC reaction pathways >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                kac = Theta(rk2(jrch),thrk2,wtmp) * 0.923           !CO2 reaeration rate (/day)
                !!    rk2(:)           |1/hr          |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
                wtmpk   = wtmp + 273.15  !convet the unit of water temp to Kelvin
                kh_DIC  = 10.0**((2385.73/wtmpk) + 0.0152642 * wtmpk - 14.0184)    !Henry's constant [mol/L/atm] 
                if(ico2 == 1) cbn_rchpara(jrch)%p_co2   = co2con(iyr)                                                    
                !  CO2_sat=12.0*kh_DIC*cbn_rchpara(jrch)%p_co2/1000.0       !CO2 saturation (unit converting to mg/L)
                CO2_sat =kh_DIC * (cbn_rchpara(jrch)%p_co2 / 1000000) * 1000.0 * 12.0       !CO2 saturation (unit converting to mgC/L)    
                !p_co2=391.0       !partial pressure of CO2 (ppm,parts per million)
                cbn_rec%Atm_DIC = 0.
                cbn_rec%Atm_DIC = kac * (CO2_sat - cbn_rchpara(jrch)%f_co2 * dic_con) * tday    !Atmospheric CO2 reaeration        
          
                cbn_rec%Alg_DIC = 0.   
                cbn_rec%Alg_DIC = cbn_rchpara(jrch)%rca * rch_Alg_Resp(jrch)                              !algae respiration to DIC,  
                !rca=0.4         !algal carbon to biomass ratio
                
                cbn_rec%DIC_Alg = 0.
                cbn_rec%DIC_Alg = cbn_rchpara(jrch)%rca * rch_Alg_Growth(jrch)                               !DIC consumbed by algal photosynthesis    
                
                cbn_rec%Ab_DIC  = 0.    
                cbn_rec%Ab_DIC  = cbn_rchpara(jrch)%rca * Ab_rch(jrch)%resp * (ch_area(jrch) / wtrtot1)  !bottom algae respiration to DIC (ch_area(jrch)/rchwtr)
                !ch_area !!m2
                                             
                cbn_rec%DIC_Ab  = 0.
                cbn_rec%DIC_Ab  = cbn_rchpara(jrch)%rca * Ab_rch(jrch)%photo * (ch_area(jrch) / wtrtot1)  ! mg/L !DIC consumbed by bottom algae photosynthesis  may not balance since may be C_photo-DIC_photo>0 
                                             
        
                !first order sediment diagenesis model-from W2 model
                rch_AlgSet(jrch)    = cbn_rchpara(jrch)%rca * rch_Alg_Set(jrch)             !floating Algae settling to bed sediment  mg/L
                !rca=0.4         !algal carbon to biomass ratio             
          
                !!----sediment C accumulation in KG
                scbn_rch(jrch)%scbn = scbn_rch(jrch)%scbn + (cbn_rec%RP_set + cbn_rec%LP_set + rch_AlgSet(jrch)) * wtrtot1/1000     !!  kg depo !add RPOC and LPOC settling from water column to bed sediment
                !type (sediment_carbon),dimension(:), allocatable:: scbn_rch 
                !real :: scbn=0.01         !sediement carbon comparment state variable (mg-C/L/day),intial condition-- NOW it is in kg/day
                !real :: thick=0.01         !the thickness of sediment layer
                !real :: kbur=0.05        !first-order sediment burial rate (/day)
                !real :: ksed=0.02        !first-order sediment decay rate (/day)
                rch_Bed_DIC(jrch)   = scbn_rch(jrch)%scbn * scbn_rchpara(jrch)%ksed   !DIC release from bed sediment compartment (first-order equation)   !!  kg depo
                rch_Bed_BOC(jrch)   = scbn_rch(jrch)%scbn * scbn_rchpara(jrch)%kbur   !sediment burial amount      !!  kg depo
                scbn_rch(jrch)%scbn = scbn_rch(jrch)%scbn - rch_Bed_DIC(jrch) - rch_Bed_BOC(jrch)  !update sediment carbon amount after loss      !!  kg depo
                                
                if (scbn_rch(jrch)%scbn<0.) then  
                    scbn_rch(jrch)%scbn = 0.
                    rch_Bed_DIC(jrch)   = scbn_rch(jrch)%scbn - rch_Bed_BOC(jrch) 
                end if
                rch_BuryC(jrch) = rch_BuryC(jrch) + rch_Bed_BOC(jrch)

                cbn_rec%bed_DIC      = rch_Bed_DIC(jrch) * 1000 / wtrtot1         !DIC release from bed sediment    mg/L    !wtrtotl |m^3 H2O 
                cbn_rch(jrch)%DIC    = dic_con + (cbn_rec%Atm_DIC + cbn_rec%LP_DIC            &   ! Atm_DIC=0.         ! Atmospheric CO2 reaeration (mg-C/L/day)
                                        + cbn_rec%RP_DIC + cbn_rec%LD_DIC + cbn_rec%RD_DIC            &!transformations from organic carbon pools
                                        + cbn_rec%Alg_DIC + cbn_rec%Ab_DIC - cbn_rec%DIC_Alg - cbn_rec%DIC_Ab      &!from algae
                                        + cbn_rec%bed_DIC)  !*tday             !update DIC concentration change after transformation
                if (cbn_rch(jrch)%DIC<0.) then
                    cbn_rch(jrch)%DIC   = 0.
                    cbn_rec%Atm_DIC     = -dic_con + (-cbn_rec%LP_DIC                         &
                                            -cbn_rec%RP_DIC - cbn_rec%LD_DIC - cbn_rec%RD_DIC            &
                                            -cbn_rec%Alg_DIC - cbn_rec%Ab_DIC + cbn_rec%DIC_Alg + cbn_rec%DIC_Ab      &
                                            -cbn_rec%bed_DIC)  
                 end if 
         
                !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            
        
         endif ! (cswat == 2 ) then
         !--////////////-For calculating carbon transformations and concentration change, added by Du---////////////////////////-----------------------------------!


!! oxygen calculations
         !! calculate dissolved oxygen concentration if reach at 
         !! end of day QUAL2E section 3.6 equation III-28
         uu = 0.
         vv = 0.
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         rhoq = 1.0
         rk2(jrch) = 1.0
         uu = Theta(rk2(jrch),thrk2,wtmp) * (soxy - o2con)      !!    rk2(:)           |1/hr          |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
         !!    thrk2       |none          |temperature adjustment factor for local oxygen reaeration rate
         !vv = (ai3 * Theta(gra,thgra,wtmp) - ai4 * Theta(rhoq,thrho,wtmp)) * algcon * tday
         !!    ai3              |mg O2/mg alg  |the rate of oxygen production per unit ofalgal photosynthesis
         !!    ai4              |mg O2/mg alg  |the rate of oxygen uptake per unit of algae respiration
         !!    rhoq             |1/hr          |algal respiration rate at 20 deg C
         !!    gra         |1/hr          |local algal growth rate at 20 deg C
         !!    thgra       |none          |temperature adjustment factor for local algal growth rate
         !!    thrho       |none          |temperature adjustment factor for local algal respiration rate
         vv = cbn_rchpara(jrch)%rca * rch_Alg_Growth(jrch) * 2.67 - cbn_rchpara(jrch)%rca * rch_Alg_Resp(jrch) * 2.67
         !rocn: ratio of oxygen generated per organic carbon produced by photosynthesis when ammonia is taken up gO2/gC  
         !roca: ratio of oxygen generated per organic carbon produced by photosynthesis when nitrate is taken up gO2/gC 
         !rocn: 3.47
         !roca: 2.67
         !Note that Eq. (58, Ammonium as substrate) is also used for the stoichiometry of the amount of oxygen consumed for plant respiration. 
         
         !!rch_Alg_Growth(jrch)=Theta(gra,thgra,wtmp) * algcon* tday                      !! mg/L   !!    tday        |none          |flow duration (fraction of 24 hr)
         !!rch_Alg_Death(jrch)=Theta(kdp,thkdp,wtmp) * algcon* tday                      !! mg/L    !!    tday        |none          |flow duration (fraction of 24 hr)
         !!rch_Alg_Resp(jrch)=Foxrp*Theta(rhoq,thrho,wtmp) *algcon* tday             !! mg/L        !!    tday        |none          |flow duration (fraction of 24 hr)
         !!rch_Alg_Set(jrch)=Theta(rs1(jrch),thrs1,wtmp) / rchdep*algcon * tday               !! mg/L   !!    tday        |none          |flow duration (fraction of 24 hr)
         vv_bottom = 0.
         vv_bottom = cbn_rchpara(jrch)%rca * Ab_rch(jrch)%photo * 2.67 - cbn_rchpara(jrch)%rca * Ab_rch(jrch)%resp * 2.67


         !ww = Theta(rk1(jrch),thrk1,wtmp) * cbodcon             !!    rk1(:)           |1/hr          |CBOD deoxygenation rate coefficient in reach at 20 deg C
         !!    thrk1       |none          |temperature adjustment factor for local CBOD deoxygenation
         ww =  (cbn_rec%LP_DIC + cbn_rec%RP_DIC + cbn_rec%LD_DIC + cbn_rec%RD_DIC) * 2.67

         
         xx = Theta(rk4(jrch),thrk4,wtmp) / (rchdep * 1000.)        
         !! XX is in mg O2/L
         !!    rk4(:)           |mg O2/((m**2)*day)    |sediment oxygen demand rate in reach at 20 deg C
         !!    thrk4       |none          |temperature adjustment factor for local sediment oxygen demand
         
         yy = ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con           !!    ai5              |mg O2/mg N    |the rate of oxygen uptake per unit of NH3 nitrogen oxidation
         !!    bc1mod      |1/day         |rate constant for biological oxidation of NH3 to NO2 modified to reflect impact of low  oxygen concentration
         !!    thbc1       |none          |temperature adjustment factor for local biological oxidation of NH3 to NO2
         zz = ai6 * Theta(bc2mod,thbc2,wtmp) * no2con           !!    ai6              |mg O2/mg N    |the rate of oxygen uptake per unit of NO2 nitrogen oxidation
         !!    bc2mod      |1/day         |rate constant for biological oxidation of NO2 to NO3 modified to reflect impact of low oxygen concentration
         !!    thbc2       |none          |temperature adjustment factor for local biological oxidation of NO2 to NO3
         rch_DOX(jrch) = o2con + (uu - xx - yy - zz) * tday  + vv + vv_bottom - ww     !!    tday        |none          |flow duration (fraction of 24 hr)
         
         !rch_DOX(jrch) = amin1(0.1, rch_DOX(jrch))
         if (rch_DOX(jrch) < 0.1) rch_DOX(jrch) = 0.1
         if (rch_DOX(jrch) > soxy) rch_DOX(jrch) = soxy       
         
         !option 2 for DOX calculation (not used)
         !==============================
         if (2 == 1) then
             !algea O2 production minus respiration
             !if (vv > 0.) then
               doxrch = soxy
             !else
             !  coef = exp(-0.03 * vv)
             !  doxrch = coef * soxy
             !end if
             
             !cbod deoxygenation
             coef = exp(-0.1 * ww)
             doxrch = coef * doxrch
             
             !benthic sediment oxidation
             coef = 1. - (Theta(rk4(jrch),thrk4,wtmp) / 100.)
             doxrch = coef * doxrch
             
             !ammonia oxydation
             coef = exp(-0.05 * yy)
             doxrch = coef * doxrch
             
             !nitrite oxydation
             coef = exp(-0.05 * zz)
             doxrch = coef * doxrch
             
             !reaeration
             uu = Theta(rk2(jrch),thrk2,wtmp) / 100. * (soxy - doxrch)
             rch_DOX(jrch) = doxrch + uu
             
             if (rch_DOX(jrch) < 1.e-6) rch_DOX(jrch) = 0.
             if (rch_DOX(jrch) > soxy) rch_DOX(jrch) = soxy
         end if
         !==============================
         !option 2 for DOX calculation (not used)
         
                  

         !if (rch_DOX(jrch) > dcoef * o2con) rch_DOX(jrch)= dcoef * o2con
!! end oxygen calculations      
      
      
    

      else   !! if (jrch>0) then
      
     
      
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
         soxy = 0.0
         orgncon = 0.0
         !------For carbon process in the stream- added by Du----------!
         if (cswat == 2 ) then
            cbn_rch(jrch)%RPOC = 0.
            cbn_rch(jrch)%LPOC = 0.
            cbn_rch(jrch)%RDOC = 0.
            cbn_rch(jrch)%LDOC = 0.
            cbn_rch(jrch)%DIC=0.
            !   Ab_rch(jrch)%Ab   =0.      !bottom algae biomass (g/m2/day)->(mg/L)
            !Ab_rch(jrch)%death = 0.        !bottom algae death biomass (g/m2/day)->(mg/L)
            ! Ab_rch(jrch)%photo = 0.      !bottom algae photosynthesis biomass (g/m2/day)->(mg/L)
            ! Ab_rch(jrch)%resp  = 0.       !bottom algae respiration biomass (g/m2/day)->(mg/L)
            cbn_rec%Alg_cbn  = 0.       !Floating Algae death to total organic carbon (mg/L)
            cbn_rec%Ab_cbn  = 0.        !bottom algae death to total organic carbon (mg/L)
            cbn_rec%Alg_LP  = 0.        ! Floating Algae -> LPOC   (mg-C/L/day)
            cbn_rec%Ab_LP   = 0.       ! Bottom Algae -> LPOC     (mg-C/L/day)
            cbn_rec%LP_set   = 0.        ! LPOC -> bed settling     (mg-C/L/day)
            cbn_rec%LP_LD   = 0.      ! LPOC dissolution to LDOC (mg-C/L/day)
            cbn_rec%LP_DIC  = 0.      ! LPOC decay to DIC        (mg-C/L/day)
            cbn_rec%LR_POC = 0.     ! LPOC decay to RPOC       (mg-C/L/day)
            cbn_rec%Alg_RP  = 0.        ! Floating Algae -> RPOC   (mg-C/L/day)
            cbn_rec%Ab_RP  = 0.        ! Bottom Algae -> RPOC     (mg-C/L/day)
            cbn_rec%RP_LD  = 0.        ! RPOC dissolution to LDOC (mg-C/L/day)
            cbn_rec%RP_DIC  = 0.     ! RPOC dissolution to LDOC (mg-C/L/day)
            cbn_rec%RP_set  = 0.        !RPOC settling to bed (mg-C/L/day)
            cbn_rec%Alg_LD  = 0.      ! Algal death to LDOC (mg-C/L/day)
            cbn_rec%Ab_LD  = 0.      ! Bottom Algae -> LDOC  (mg-C/L/day)
            cbn_rec%LD_DIC  = 0.      ! LDOC mineralization to DIC (mg-C/L/day)
            cbn_rec%LR_DOC   = 0.    ! LDOC decay to RDOC  (mg-C/L/day)
            cbn_rec%LD_NO3  = 0.     ! LDOC consumed by NO3- denitrification (mg-C/L/day)
            cbn_rec%Alg_RD  = 0.       ! Floating Algae -> RDOC (mg-C/L/day)
            cbn_rec%Ab_RD   =0.       ! Bottom Algae -> RDOC  (mg-C/L/day)
            cbn_rec%RD_DIC   =0.     ! RDOC mineralization to DIC  (mg-C/L/day)
            cbn_rec%Atm_DIC  =0.     ! Atmospheric CO2 reaeration (mg-C/L/day)
            cbn_rec%Alg_DIC =0.       ! Algal respiration to DIC   (mg-C/L/day)
            cbn_rec%DIC_Alg   =0.     ! DIC consumed by algal photosynthesis (mg-C/L/day)
            cbn_rec%Ab_DIC  =0.       ! Bottom algae respiration to DIC (mg-C/L/day)
            cbn_rec%DIC_Ab   =0.      ! DIC consumed by algae photosynthesis (mg-C/L/day)
            cbn_rec%CBOD_DIC  =0. ! CBOD oxidation (mg-C/L/day)
            cbn_rec%bed_DIC  =0.      ! Sediment release DIC to water column (mg-C/L/day)
       
        endif   !if (cswat == 2 ) then
        !------For carbon process in the stream- added by Du----------!
        
        
      endif        !   if (wtrin > 1.e-4) then
      
      
      
      
      !!---------C remains in reach at the end of day-
          rchre_RPOC(jrch)= cbn_rch(jrch)%RPOC*rchstor(jrch)/1000     !!KG
          rchre_LPOC(jrch)= cbn_rch(jrch)%LPOC*rchstor(jrch)/1000     !!KG
          rchre_RDOC(jrch)= cbn_rch(jrch)%RDOC*rchstor(jrch)/1000    !!KG
          rchre_LDOC(jrch)= cbn_rch(jrch)%LDOC*rchstor(jrch)/1000    !!KG
          rchre_DIC(jrch)= cbn_rch(jrch)%DIC*rchstor(jrch)/1000            !!KG
          rchre_Alg(jrch)= rch_Alg(jrch)*rchstor(jrch)/1000                             !!KG
      
   

!!!! commented following statements per conversation with 
!!!! srini 10/22/08
!    write for srinisan 12/07/2004
!!    write added back 03/02/2010 - per Srin email
      if (ihumus == 1) then
         write (output_wql_num,5000) jrch, i, tmpav(jrch),              &
        chlin, rch_Chla(jrch), orgncon, rch_OrgN(jrch),     &
        ammoin, rch_NH4(jrch), nitritin, rch_NO2(jrch),     &
        nitratin, rch_NO3(jrch), orgpin, rch_OrgP(jrch),    &
        dispin, rch_SolP(jrch), cbodin, rch_CBOD(jrch), soxy,   & 
        disoxin, rch_DOX(jrch), varoute (2,inum2), rttime   
 5000    format ('REACH', i4, i5, 22e12.4)
      end if

      return
      end