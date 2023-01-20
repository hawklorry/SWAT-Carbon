      subroutine sumv

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs summary calculations for HRU

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert
!!                               |application
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with
!!                               |sediment in surface runoff
!!    cfertn      |kg N/ha       |amount of nitrogen added to soil in continuous
!!                               |fertilizer operation on day
!!    cfertp      |kg P/ha       |amount of phosphorus added to soil in continuous
!!                               |fertilizer operation on day
!!    cnday(:)    |none          |curve number for current day, HRU and at
!!                               |current soil moisture
!!    curyr       |none          |current year of simulation
!!    deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
!!                               |for irrigation
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that
!!                               |occurs on day in HRU
!!    fertn       |kg N/ha       |total amount of nitrogen added to soil in HRU
!!                               |on day in fertilizer application
!!    fertp       |kg P/ha       |total amount of phosphorus added to soil in
!!                               |HRU on day in fertilizer application
!!    fixn        |kg N/ha       |amount of nitrogen added to plant biomass via
!!                               |fixation on the day in HRU
!!    grazn       |kg N/ha       |amount of nitrogen added to soil in grazing
!!                               |on the day in HRU
!!    grazp       |kg P/ha       |amount of phosphorus added to soil in grazing
!!                               |on the day in HRU
!!    gw_q(:)     |mm H2O        |groundwater contribution to streamflow from
!!                               |HRU on current day
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day
!!    hmntl       |kg N/ha       |amount of nitrogen moving from active
!!                               |organic to nitrate pool in soil profile
!!                               |on current day in HRU
!!    hmptl       |kg P/ha       |amount of phosphorus moving from the
!!                               |organic to labile pool in soil profile
!!                               |on current day in HRU
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    lat_pst(:)  |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                               |for the day
!!    latno3(:)   |kg N/ha       |amount of NO3-N in lateral flow in HRU for the
!!                               |day
!!    latq(:)     |mm H2O        |amount of water in lateral flow in HRU for the
!!                               |day
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    nplnt(:)    |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    percn(:)    |kg N/ha       |NO3-N leached from soil profile during the day
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    potevmm     |mm H2O        |volume of water evaporated from pothole
!!                               |expressed as depth over HRU
!!    potflwi(:)  |m^3 H2O       |water entering pothole on day
!!    potflwo     |mm H2O        |discharge from pothole expressed as depth
!!                               |over HRU
!!    potpcpmm    |mm H2O        |precipitation falling on pothole water body
!!                               |expressed as depth over HRU
!!    potsedi(:)  |metric tons   |sediment entering pothole on day
!!    potsedo     |metric tons   |sediment leaving pothole on day
!!    potsepmm    |mm H2O        |seepage from pothole expressed as depth over
!!                               |HRU
!!    pplnt(:)    |kg P/ha       |plant uptake of phosphorus in HRU for the day
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface
!!                               |runoff on current day in HRU
!!    pstsol(:)   |kg/ha         |amount of pesticide type leached from soil
!!                               |profile on current day
!!    qday        |mm H2O        |surface runoff loading to main channel for
!!                               |day in HRU
!!    qdr(:)      |mm H2O        |total amount of water entering main channel
!!                               |for day from HRU
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    rmn2tl      |kg N/ha       |amount of nitrogen moving from the fresh
!!                               |organic (residue) to the nitrate(80%) and
!!                               |active organic(20%) pools in soil profile
!!                               |on current day in HRU
!!    rmp1tl      |kg P/ha       |amount of phosphorus moving from the labile
!!                               |mineral pool to the active mineral pool in
!!                               |the soil profile on the current day in the
!!                               |HRU
!!    rmptl       |kg P/ha       |amount of phosphorus moving from the
!!                               |fresh organic (residue) to the labile(80%)
!!                               |and organic(20%) pools in soil profile
!!                               |on current day in HRU
!!    roctl       |kg P/ha       |amount of phosphorus moving from the active
!!                               |mineral pool to the stable mineral pool
!!                               |in the soil profile on the current day in
!!                               |the HRU
!!    rwntl       |kg N/ha       |amount of nitrogen moving from active
!!                               |organic to stable organic pool in soil
!!                               |profile on current day in HRU
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion
!!    sepbtm(:)   |mm H2O        |seepage leaving the bottom of the soil profile
!!                               |on day in HRU
!!    shallirr(:) |mm H2O        |amount of water removed from shallow aquifer
!!                               |for irrigation
!!    snoev       |mm H2O        |amount of water in snow lost through
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in
!!                               |HRU
!!    sol_cnsw(:) |mm H2O        |soil water content used to calculate daily
!!                               |CN value (initial soil wter content for day)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on any given day
!!    sol_tmp(2,:)|deg C         |daily average temperature of second soil layer
!!    strsn(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |nitrogen stress
!!    strsp(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |phosphorus stress
!!    strstmp(:)  |none          |fraction of potential plant growth achieved on
!!                               |the day in HRU where the reduction is caused
!!                               |by temperature stress
!!    strsw(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |water stress
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!                               |via transmission losses on day in HRU
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    usle        |metric tons   |daily soil loss predicted with USLE equation
!!    wdntl       |kg N/ha       |amount of nitrogen lost from nitrate pool
!!                               |by denitrification in soil profile on
!!                               |current day in HRU
!!    wetev       |m^3 H2O       |evaporation from wetland for day
!!    wetflwi     |m^3 H2O       |volume of water flowing in wetland on day
!!    wetflwo     |m^3 H2O       |volume of water flowing out wetland on day
!!    wetpcp      |m^3 H2O       |precipitation on wetland for day
!!    wetsedc     |metric tons   |net change in sediment in wetland during day
!!    wetsedi     |metric tons   |sediment loading to wetland for day
!!    wetsedo     |metric tons   |sediment loading from wetland for day
!!    wetsep      |m^3 H2O       |seepage from wetland bottom for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupstd(:,1,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU on day
!!                               |(in solution)
!!    hrupstd(:,2,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU on day
!!                               |(sorbed to sediment)
!!    hrupstd(:,3,:)|mg pst/ha   |total pesticide loading to stream in surface
!!                               |runoff from HRU
!!    hrupstd(:,4,:)|mg pst      |amount of pesticide type in lateral flow
!!                               |contribution to stream from HRU on day
!!                               |(in solution)
!!    hrumono(1,:)|mm H2O        |precipitation in HRU during month
!!    hrumono(2,:)|mm H2O        |amount of precipitation falling as freezing
!!                               |rain/snow in HRU during month
!!    hrumono(3,:)|mm H2O        |amount of snow melt in HRU during month
!!    hrumono(4,:)|mm H2O        |amount of surface runoff to main channel 
!!                               |from HRU during month (ignores impact of
!!                               |transmission losses)
!!    hrumono(5,:)|mm H2O        |amount of lateral flow contribution to main
!!                               |channel from HRU during month
!!    hrumono(6,:)|mm H2O        |amount of groundwater flow contribution to
!!                               |main channel from HRU during month
!!    hrumono(7,:)|mm H2O        |amount of water moving from shallow aquifer
!!                               |to plants or soil profile in HRU during month
!!    hrumono(8,:)|mm H2O        |amount of water recharging deep aquifer in
!!                               |HRU during month
!!    hrumono(9,:)|mm H2O        |total amount of water entering both aquifers
!!                               |from HRU during month
!!    hrumono(10,:)|mm H2O        |water yield (total amount of water entering
!!                               |main channel) from HRU during month
!!    hrumono(11,:)|mm H2O        |amount of water percolating out of the soil
!!                               |profile and into the vadose zone in HRU
!!                               |during month
!!    hrumono(12,:)|mm H2O        |actual evapotranspiration in HRU during month
!!    hrumono(13,:)|mm H2O        |amount of transmission losses from tributary
!!                               |channels in HRU for month
!!    hrumono(14,:)|metric tons/ha|sediment yield from HRU for month
!!    hrumono(15,:)|mm H2O        |actual amount of transpiration that occurs
!!                               |during month in HRU
!!    hrumono(16,:)|mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs during month in HRU
!!    hrumono(17,:)|kg N/ha       |amount of nitrogen applied in continuous 
!!                               |fertilizer operation during month in HRU 
!!    hrumono(18,:)|kg P/ha       |amount of phosphorus applied in continuous 
!!                               |fertilizer operation during month in HRU 
!!    hrumono(19,:)|mm H2O        |amount of surface runoff generated during month
!!                               |in HRU
!!    hrumono(20,:)|none          |CN values during month in HRU
!!    hrumono(21,:)|mm H2O        |sum of daily soil water values used to calculate
!!                               |the curve number
!!    hrumono(23,:)|mm H2O        |amount of water removed from shallow aquifer
!!                               |in HRU for irrigation during month
!!    hrumono(24,:)|mm H2O        |amount of water removed from deep aquifer
!!                               |in HRU for irrigation during month
!!    hrumono(25,:)|mm H2O        |potential evapotranspiration in HRU during
!!                               |month
!!    hrumono(26,:)|kg N/ha       |monthly amount of N (organic & mineral)
!!                               |applied in HRU during grazing
!!    hrumono(27,:)|kg P/ha       |monthly amount of P (organic & mineral)
!!                               |applied in HRU during grazing
!!    hrumono(28,:)|kg N/ha       |monthly amount of N (organic & mineral)
!!                               |auto-applied in HRU
!!    hrumono(29,:)|kg P/ha       |monthly amount of P (organic & mineral)
!!                               |auto-applied in HRU
!!    hrumono(30,:)|deg C         |sum of daily soil temperature values
!!    hrumono(31,:)|stress days   |water stress days in HRU during month
!!    hrumono(32,:)|stress days   |temperature stress days in HRU during month
!!    hrumono(33,:)|stress days   |nitrogen stress days in HRU during month
!!    hrumono(34,:)|stress days   |phosphorus stress days in HRU during month
!!    hrumono(35,:)|kg N/ha       |organic nitrogen in surface runoff in HRU
!!                               |during month
!!    hrumono(36,:)|kg P/ha       |organic phosphorus in surface runoff in HRU
!!                               |during month
!!    hrumono(37,:)|kg N/ha       |nitrate in surface runoff in HRU during month
!!    hrumono(38,:)|kg N/ha       |nitrate in lateral flow in HRU during month
!!    hrumono(39,:)|kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                               |during month
!!    hrumono(40,:)|kg N/ha       |amount of nitrogen removed from soil by plant
!!                               |uptake in HRU during month
!!    hrumono(41,:)|kg N/ha       |nitrate percolating past bottom of soil 
!!                               |profile in HRU during month
!!    hrumono(42,:)|kg P/ha       |amount of phosphorus removed from soil by 
!!                               |plant uptake in HRU during month
!!    hrumono(43,:)|kg P/ha       |amount of phosphorus moving from labile
!!                               |mineral to active mineral pool in HRU during
!!                               |month
!!    hrumono(44,:)|kg P/ha       |amount of phosphorus moving from active
!!                               |mineral to stable mineral pool in HRU during
!!                               |month
!!    hrumono(45,:)|kg N/ha       |amount of nitrogen applied to HRU in 
!!                               |fertilizer and grazing operations during month
!!    hrumono(46,:)|kg P/ha       |amount of phosphorus applied to HRU in 
!!                               |fertilizer and grazing operations during month
!!    hrumono(47,:)|kg N/ha       |amount of nitrogen added to soil by fixation
!!                               |in HRU during month
!!    hrumono(48,:)|kg N/ha       |amount of nitrogen lost by denitrification
!!                               |in HRU during month
!!    hrumono(49,:)|kg N/ha       |amount of nitrogen moving from active organic
!!                               |to nitrate pool in HRU during month
!!    hrumono(50,:)|kg N/ha       |amount of nitrogen moving from active organic
!!                               |to stable organic pool in HRU during month
!!    hrumono(51,:)|kg P/ha       |amount of phosphorus moving from organic to
!!                               |labile mineral pool in HRU during month
!!    hrumono(52,:)|kg N/ha       |amount of nitrogen moving from fresh organic
!!                               |to nitrate and active organic pools in HRU
!!                               |during month
!!    hrumono(53,:)|kg P/ha       |amount of phosphorus moving from fresh
!!                               |organic to the labile mineral and organic
!!                               |pools in HRU during month
!!    hrumono(54,:)|kg N/ha       |amount of nitrogen added to soil in rain
!!    hrumono(61,:)|metric tons/ha|daily soil loss predicted with USLE equation
!!    hrumono(62,:)|mm H2O        |drainage tile flow contribution to main 
!!                               |channel from HRU in month
!!    hrumono(63,:)|# bacteria/ha |persistent bacteria transported to main 
!!                               |channel from HRU during month
!!    hrumono(64,:)|# bacteria/ha |less persistent bacteria transported to main
!!                               |channel from HRU during month
!!    hrumono(65,:)|kg N/ha       |nitrate loading from groundwater in HRU to
!!                               |main channel during month
!!    hrumono(66,:)|kg P/ha       |soluble P loading from groundwater in HRU to
!!                               |main channel during month
!!    hrumono(67,:)|kg P/ha       |loading of mineral P attached to sediment
!!                               |in HRU to main channel during month
!!    wpstdayo(:,1) |mg pst/ha   |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed on day
!!                               |(in solution)
!!    wpstdayo(:,2) |mg pst/ha   |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed on day
!!                               |(sorbed to sediment)
!!    wpstdayo(:,3) |kg pst/ha   |amount of pesticide type leached from soil
!!                               |profile in watershed on day
!!    wpstdayo(:,4) |kg pst/ha   |amount of pesticide type in lateral flow
!!                               |contribution to stream in watershed on day
!!    wshddayo(1) |mm H2O        |average amount of precipitation in watershed
!!                               |for the day
!!    wshddayo(3) |mm H2O        |surface runoff in watershed for day
!!    wshddayo(4) |mm H2O        |lateral flow contribution to streamflow in 
!!                               |watershed for day
!!    wshddayo(5) |mm H2O        |water percolation past bottom of soil profile
!!                               |in watershed for day
!!    wshddayo(6) |mm H2O        |water yield to streamflow from HRUs in
!!                               |watershed for day
!!    wshddayo(7) |mm H2O        |actual evapotranspiration in watershed
!!                               |for day
!!    wshddayo(8) |deg C         |average maximum temperature in watershed for
!!                               |the day
!!    wshddayo(9) |deg C         |average minimum temperature in watershed for
!!                               |the day
!!    wshddayo(12)|metric tons   |sediment yield from HRUs in watershed for day
!!    wshddayo(13)|metric tons   |sediment loading to ponds in watershed for day
!!    wshddayo(14)|metric tons   |sediment loading from ponds in watershed for
!!                               |day
!!    wshddayo(15)|metric tons   |net change in sediment level in ponds in
!!                               |watershed for day
!!    wshddayo(16)|metric tons   |sediment loading to wetlands for day
!!                               |in watershed
!!    wshddayo(17)|metric tons   |sediment loading to main channels from
!!                               |wetlands for day in watershed
!!    wshddayo(18)|metric tons   |net change in sediment in wetlands for day
!!                               |in watershed
!!    wshddayo(19)|m^3 H2O       |evaporation from ponds in watershed for day
!!    wshddayo(20)|m^3 H2O       |seepage from ponds in watershed for day
!!    wshddayo(21)|m^3 H2O       |precipitation on ponds in watershed for day
!!    wshddayo(22)|m^3 H2O       |volume of water entering ponds in watershed
!!                               |for day
!!    wshddayo(23)|m^3 H2O       |volume of water leaving ponds in watershed
!!                               |for day
!!    wshddayo(24)|m^3 H2O       |evaporation from wetlands for day in watershed
!!    wshddayo(25)|m^3 H2O       |seepage from wetlands for day in watershed
!!    wshddayo(26)|m^3 H2O       |precipitation on wetlands for day in watershed
!!    wshddayo(27)|m^3 H2O       |volume of water entering wetlands on day in
!!                               |watershed
!!    wshddayo(28)|m^3 H2O       |volume of water leaving wetlands on day in
!!                               |watershed
!!    wshddayo(33)|m^3 H2O       |net change in water volume of ponds in
!!                               |watershed for day
!!    wshddayo(35)|mm H2O        |amount of water stored in soil profile in
!!                               |watershed for day
!!    wshddayo(36)|mm H2O        |snow melt in watershed for day
!!    wshddayo(37)|mm H2O        |sublimation in watershed for day
!!    wshddayo(38)|mm H2O        |average amount of tributary channel
!!                               |transmission losses in watershed on day
!!    wshddayo(39)|mm H2O        |freezing rain/snow fall in watershed for day
!!    wshddayo(40)|kg N/ha       |organic N loading to stream in watershed for
!!                               |day
!!    wshddayo(41)|kg P/ha       |organic P loading to stream in watershed for
!!                               |day
!!    wshddayo(42)|kg N/ha       |nitrate loading to stream in surface runoff
!!                               |in watershed for day
!!    wshddayo(43)|kg P/ha       |soluble P loading to stream in watershed for
!!                               |day
!!    wshddayo(44)|kg N/ha       |plant uptake of N in watershed for day
!!    wshddayo(45)|kg N/ha       |nitrate loading to stream in lateral flow
!!                               |in watershed for day
!!    wshddayo(46)|kg N/ha       |nitrate percolation past bottom of soil 
!!                               |profile in watershed for day
!!    wshddayo(104)|mm H2O        |groundwater contribution to stream in
!!                               |watershed on day
!!    wshddayo(105)|mm H2O        |amount of water moving from shallow aquifer
!!                               |to plants/soil profile in watershed on day
!!    wshddayo(106)|mm H2O        |deep aquifer recharge in watershed on day
!!    wshddayo(107)|mm H2O        |total amount of water entering both aquifers
!!                               |in watershed on day
!!    wshddayo(108)|mm H2O        |potential evapotranspiration in watershed
!!                               |on day
!!    wshddayo(109)|mm H2O        |drainage tile flow contribution to stream
!!                               |in watershed on day
!!    wshddayo(110)|kg/ha        |NO3 yield (gwq)                          
!!    wshddayo(111)|mm H2O       |NO3 yield (tile)                          
!!    wtrmon(1,:) |mm H2O        |evaporation from ponds in HRU for month
!!    wtrmon(2,:) |mm H2O        |seepage from ponds in HRU for month
!!    wtrmon(3,:) |mm H2O        |precipitation on ponds in HRU for month
!!    wtrmon(4,:) |mm H2O        |amount of water entering ponds in HRU for
!!                               |month
!!    wtrmon(5,:) |metric tons/ha|sediment entering ponds in HRU for month
!!    wtrmon(6,:) |mm H2O        |amount of water leaving ponds in HRU for
!!                               |month
!!    wtrmon(7,:) |metric tons/ha|sediment leaving ponds in HRU for month
!!    wtrmon(8,:) |mm H2O        |precipitation on wetlands in HRU for month
!!    wtrmon(9,:) |mm H2O        |volume of water entering wetlands from HRU
!!                               |for month
!!    wtrmon(10,:)|metric tons/ha|sediment loading to wetlands for month from
!!                               |HRU
!!    wtrmon(11,:)|mm H2O        |evaporation from wetlands in HRU for month
!!    wtrmon(12,:)|mm H2O        |seeepage from wetlands in HRU for month
!!    wtrmon(13,:)|mm H2O        |volume of water leaving wetlands in HRU
!!                               |for month
!!    wtrmon(14,:)|metric tons/ha|sediment loading from wetlands in HRU to main
!!                               |channel during month
!!    wtrmon(15,:)|mm H2O        |precipitation on potholes in HRU for month
!!    wtrmon(16,:)|mm H2O        |evaporation from potholes in HRU for month
!!    wtrmon(17,:)|mm H2O        |seepage from potholes in HRU for month
!!    wtrmon(18,:)|mm H2O        |water leaving potholes in HRU for month
!!    wtrmon(19,:)|mm H2O        |water entering potholes in HRU for month
!!    wtrmon(20,:)|metric tons/ha|sediment entering potholes in HRU for month
!!    wtrmon(21,:)|metric tons/ha|sediment leaving potholes in HRU for month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    k           |none          |counter (pesticides)
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
      implicit none
      
      integer :: j, k, ii
      real :: cnv  
      integer:: ly
       real :: HPC2           
       real :: HSC2  
       real :: BMC2 
       real :: LMC2 
       real :: LSC2 
       real :: HPN2             
       real :: HSN2  
       real :: BMN2 
       real :: LMN2 
       real :: LSN2 
       real :: HPC1             
       real :: HSC1   
       real :: BMC1 
       real :: LMC1 
       real :: LSC1 
       real :: HPN1             
       real :: HSN1   
       real :: BMN1 
       real :: LMN1 
       real :: LSN1

      j = 0
      j = ihru
    
      cnv = 0.
      cnv = 10. * hru_ha(j)

      if (curyr > nyskip) then

     
     
        do ly = 1, sol_nly(j)
          solc_no3(j)  = solc_no3(j) + sol_no3(ly,j)
          solc_nh4(j)  = solc_nh4(j) + sol_nh4(ly,j)      
          solc_urea(j) = solc_urea(j) + sol_urea(ly,j)
          solc_solp(j) = solc_solp(j) + sol_solp(ly,j) 
          solc_minp(j) = solc_minp(j) + sol_actp(ly,j)+ sol_stap(ly,j)     
	  
  
	    if (cswat == 0) then
          solc_orgn(j) = solc_orgn(j) + sol_aorgn(ly,j) + sol_orgn(ly,j) &
	  		+ sol_fon(ly,j)    
	    solc_orgp = solc_orgp + sol_fop(ly,j) + sol_orgp(ly,j) 
	    end if
	    
	    if (cswat == 1) then
      	  solc_orgn(j) = solc_orgn(j) + sol_orgn(ly,j) + sol_fon(ly,j) + sol_mn(ly,j)
	
          solc_orgp = solc_orgp + sol_fop(ly,j)+sol_orgp(ly,j) + sol_mp(ly,j) 
	    end if
	   
	    if (cswat == 2) then
          solc_orgn(j) = solc_orgn(j) + sol_LMN(ly,j) + sol_LSN(ly,j) + sol_HPN(ly,j) + sol_BMN(ly,j) + sol_HSN(ly,j)
   
	      solc_orgc(j) = solc_orgc(j)+ sol_LSC(ly,j)+sol_LMC(ly,j) +sol_HPC(ly,j) +sol_HSC(ly,j)  +sol_BMC(ly,j)
                      
          solc_orgcs(j)= solc_orgcs(j)+ sol_HPC(ly,j)+sol_HSC(ly,j) +sol_BMC(ly,j)
         
          solc_orgp = solc_orgp + sol_fop(ly,j) + sol_orgp(ly,j)	
          
          solc_doc(j) = solc_doc(j) + Sol_DOC(ly,j) !sol_BMC(ly,j)  
          solc_dic(j) = solc_dic(j) + Sol_DIC(ly,j)  
          
	      Min_ML_MBN(j) =  Min_ML_MBN(j) + MNRMETS1(ly,1,j)                    ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
          Min_SL_MBN(j) =  Min_SL_MBN(j) + MNRSTRS1(ly,1,j)                      !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
          Min_SL_SHN(j) =  Min_SL_SHN(j) + MNRSTRS2(ly,1,j)                     !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
          Min_MB_SHN(j) =  Min_MB_SHN(j) + MNRS1S2(ly,1,j)                         !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
          Min_MB_PHN(j) =  Min_MB_PHN(j) + MNRS1S3(ly,1,j)                          !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
          Min_SH_MBN(j) =  Min_SH_MBN(j) + MNRS2S1(ly,1,j)                          !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
          Min_SH_PHN(j) =  Min_SH_PHN(j) + MNRS2S3(ly,1,j)                          !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
          Min_PH_MBN(j) =  Min_PH_MBN(j) + MNRS3S1(ly,1,j)                            !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                                
          IMM_ML_MBN(j) =  IMM_ML_MBN(j) + IMMMETS1(ly,1,j)                       ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
          IMM_SL_MBN(j) =  IMM_SL_MBN(j) + IMMSTRS1(ly,1,j)                        ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
          IMM_SL_SHN(j) =  IMM_SL_SHN(j) + IMMSTRS2(ly,1,j)                         ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
          IMM_MB_SHN(j) =  IMM_MB_SHN(j) + IMMS1S2(ly,1,j)                           ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
          IMM_MB_PHN(j) =  IMM_MB_PHN(j) + IMMS1S3(ly,1,j)                          ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
          IMM_SH_MBN(j) =  IMM_SH_MBN(j) + IMMS2S1(ly,1,j)                           !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
          IMM_SH_PHN(j) =  IMM_SH_PHN(j) + IMMS2S3(ly,1,j)                            ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
          IMM_PH_MBN(j) =  IMM_PH_MBN(j) + IMMS3S1(ly,1,j)                          ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
	   
	  ! if(i==1 .and. curyr== nyskip +1) then
	  ! iniorgn(j) =  iniorgn(j)  + sol_aorgn(ly,j) + sol_orgn(ly,j) +
    ! &           sol_fon(ly,j) + sol_BMN(ly,j)
	  ! inino3(j)  =  inino3(j)  + sol_no3(ly,j)
        ! ininh3(j) =  ininh3(j)   + sol_nh4(ly,j)              
	  ! end if
	  
	  ! if(i== idal .and. curyr==  nbyr) then
	     solc_orgn_fnl(j) = solc_orgn_fnl(j)+ sol_aorgn(ly,j)+sol_orgn(ly,j) + sol_fon(ly,j) + sol_BMN(ly,j)
	     solc_no3_fnl(j) =  solc_no3_fnl(j) + sol_no3(ly,j)
         solc_nh4_fnl(j) = solc_nh4_fnl(j)+  sol_nh4(ly,j)        
	 
	     sol_soc(ly,j)=sol_HPC(ly,j) + sol_HSC(ly,j)+ sol_BMC(ly,j) 
	 
	 !  end if
	   
	  ! total_BMN(j) = total_BMN(j) + sol_BMN(ly,j)
	   !total_BMC(j) = total_BMC(j) + sol_BMC(ly,j)

	   end if
      
            
         
            
      end do
       
        if (cswat == 2) then
       
       
            HPC2 = 0.            
            HSC2 = 0.  
            BMC2 = 0.
            LMC2 = 0.
            LSC2 = 0.
            HPN2 = 0.            
            HSN2 = 0. 
            BMN2 = 0.
            LMN2 = 0.
            LSN2 = 0.
            HPC1 = 0.             
            HSC1 = 0.  
            BMC1 = 0.
            LMC1 = 0.
            LSC1 = 0.
            HPN1 = 0.            
            HSN1 = 0.  
            BMN1 = 0.
            LMN1 = 0.
            LSN1 = 0.
            do ly = 2, sol_nly(j)
              !	 solc_orgcs(j)=solc_orgcs(j)+sol_HPC(ly,j)+sol_LSC(ly,j)+sol_LMC(ly,j)
              !&                      +sol_HSC(ly,j)  +sol_BMC(ly,j)
                
                HPC2 = HPC2 + sol_HPC(ly,j)             
                HSC2 = HSC2 + sol_HSC(ly,j)  
                BMC2 = BMC2 + sol_BMC(ly,j) 
                LMC2 = LMC2 + sol_LMC(ly,j)
                LSC2 = LSC2 + sol_LSC(ly,j)
                
                HPN2 = HPN2 + sol_HPN(ly,j)             
                HSN2 = HSN2 + sol_HSN(ly,j)  
                BMN2 = BMN2 + sol_BMN(ly,j)
                LMN2 = LMN2 + sol_LMN(ly,j)
                LSN2 = LSN2 + sol_LSN(ly,j)
            
            end do
	     !solc_orgc(j) = solc_orgc(j)+sol_HPC(1,j)+sol_LSC(1,j)+ sol_LMC(1,j) 
       !  &                      +sol_HSC(1,j)  +sol_BMC(1,j)
            
            HPC1 = sol_HPC(1,j)             
            HSC1 = sol_HSC(1,j)  
            BMC1 = sol_BMC(1,j)
            LMC1 = sol_LMC(1,j)
            LSC1 = sol_LSC(1,j)
            
            HPN1 = sol_HPN(1,j)             
            HSN1 = sol_HSN(1,j)  
            BMN1 = sol_BMN(1,j)
            LMN1 = sol_LMN(1,j)
            LSN1 = sol_LSN(1,j)
        
        end if
 
    
      
      
      
      !! HRU summations
        hrumono(1,j) = hrumono(1,j) + subp(j)   !                                                                                pdvas(1) = subp(j)   !!SWATCUP =8
        hrumono(2,j) = hrumono(2,j) + snofall   !                                                                                pdvas(2) = snofall
        hrumono(3,j) = hrumono(3,j) + snomlt    !                                                                               pdvas(3) = snomlt
        hrumono(4,j) = hrumono(4,j) + aird(j)   !qday                                                                           pdvas(4) = aird(j)
        hrumono(5,j) = hrumono(5,j) + pet_day   !latq(j)                                                                        pdvas(5) = pet_day
        hrumono(6,j) = hrumono(6,j) + etday     !gw_q(j)                                                                        pdvas(6) = etday
        hrumono(7,j) = hrumono(7,j) + sol_cnsw(j)   !revapday                                                                   pdvas(7) = sol_cnsw(j)                  
        hrumono(8,j) = hrumono(8,j) + sol_sw(j) !gwseep                                                                         pdvas(8) = sol_sw(j)                    
        hrumono(9,j) = hrumono(9,j) + sepbtm(j) !rchrg(j)                                                                       pdvas(9) = sepbtm(j)                    
        hrumono(10,j) = hrumono(10,j) + rchrg(j)    !qdr(j)                                                                     pdvas(10) = rchrg(j)                    
        hrumono(11,j) = hrumono(11,j) + gwseep  !sepbtm(j)                                                                      pdvas(11) = gwseep                      
        hrumono(12,j) = hrumono(12,j) + revapday    !etday                                                                      pdvas(12) = revapday                    
        hrumono(13,j) = hrumono(13,j) + shallirr(j) !tloss                                                                      pdvas(13) = shallirr(j)
        hrumono(14,j) = hrumono(14,j) + deepirr(j)  !sedyld(j) / hru_ha(j)                                                      pdvas(14) = deepirr(j)
        hrumono(15,j) = hrumono(15,j) + shallst(j)  !ep_day                                                                     pdvas(15) = shallst(j)                  
        hrumono(16,j) = hrumono(16,j) + deepst(j)   !es_day                                                                     pdvas(16) = deepst(j)                   
        hrumono(17,j) = hrumono(17,j) + surfq(j)    !cfertn                                                                     pdvas(17) = surfq(j)
        hrumono(18,j) = hrumono(18,j) + qday    !cfertp                                                                         pdvas(18) = qday                        
        hrumono(19,j) = hrumono(19,j) + latq(j)   !surfq(j)                                                                       pdvas(19) = latq(j)                    !
        hrumono(20,j) = hrumono(20,j) + latq(j) - lpndloss - lwetloss  !cnday(j)                                                                       pdvas(20) = latq(j) - lpndloss - lwetlos
        hrumono(21,j) = hrumono(21,j) + qtile   !sol_cnsw(j)                                                                    pdvas(21) = qtile 
         !hrumono(22,j) calculated in irrigate.f                                                                                pdvas(22) = gw_q(j)                     
        hrumono(22,j) = hrumono(22,j) + gw_q(j)    !qdr(:)      |mm H2O        |net water loading from HRU to main channel         pdvas(23) = gw_qdeep(j)
        hrumono(23,j) = hrumono(23,j) + gw_qdeep(j)  !shallirr(j)                                                                  pdvas(25) = qdr(j)                      
        hrumono(24,j) = hrumono(24,j) + tloss     !deepirr(j)
        hrumono(25,j) = hrumono(25,j) + qdr(j)  !pet_day
        
        hrumono(26,j) = hrumono(26,j) + cnday(j)  !grazn                                        pdvas(26) = cnday(j)
        hrumono(27,j) = hrumono(27,j) + tmpav(j)    !grazp                                pdvas(27) = tmpav(j)
        hrumono(28,j) = hrumono(28,j) + tmx(j)   !auton                                    pdvas(28) = tmx(j)
        hrumono(29,j) = hrumono(29,j) + tmn(j)   !autop                        pdvas(29) = tmn(j)
        hrumono(30,j) = hrumono(30,j) + hru_ra(j)     !sol_tmp(2,j)         pdvas(30) = hru_ra(j) 
                                                                                              
        hrumono(31,j) = hrumono(31,j) + sedyld(j) / hru_ha(j)   !(1.-strsw(j))          pdvas(31) = sedyld(j) / hru_ha(j)
        hrumono(32,j) = hrumono(32,j) + sedgen(j) / hru_ha(j)  !(1.-strstmp(j))        pdvas(32) = sedgen(j)/ hru_ha(j)
        hrumono(33,j) = hrumono(33,j) + sedorgn(j)  !(1.-strsn(j))          pdvas(33) = sedorgn(j)               
        hrumono(34,j) = hrumono(34,j) + sedorgp(j)     !(1.-strsp(j))          pdvas(34) = sedorgp(j)               
        hrumono(35,j) = hrumono(35,j) + sedminpa(j) + sedminps(j)  !sedorgn(j)             pdvas(35) = sedminpa(j) + sedminps(j)
        
        
        hrumono(36,j) = hrumono(36,j) + fertn   !sedorgp(j)             pdvas(36) = fertn
        hrumono(37,j) = hrumono(37,j) + fertp  !surqno3(j)             pdvas(37) = fertp
        hrumono(38,j) = hrumono(38,j) + auton  !latno3(j)              pdvas(38) = auton
        hrumono(39,j) = hrumono(39,j) + autop  !surqsolp(j)            pdvas(39) = autop
        hrumono(40,j) = hrumono(40,j) + grazn   !nplnt(j)               pdvas(40) = grazn
        hrumono(41,j) = hrumono(41,j) + grazp  !percn(j)               pdvas(41) = grazp
        hrumono(42,j) = hrumono(42,j) + cfertn   !pplnt(j)               pdvas(42) = cfertn
        hrumono(43,j) = hrumono(43,j) + cfertp   !rmp1tl                 pdvas(43) = cfertp
        hrumono(44,j) = hrumono(44,j) + no3pcp  !roctl                  pdvas(44) = no3pcp
        hrumono(45,j) = hrumono(45,j) + fixn   !fertn                  pdvas(45) = fixn
        
        
        hrumono(46,j) = hrumono(46,j) + nplnt(j)  !fertp          pdvas(46) = nplnt(j) 
        hrumono(47,j) = hrumono(47,j) + pplnt(j)   !fixn           pdvas(47) = pplnt(j) 
        hrumono(48,j) = hrumono(48,j) + wdntl   !wdntl          pdvas(48) = wdntl
        
        hrumono(49,j) = hrumono(49,j) + surqno3(j)    !hmntl                  pdvas(49) = surqno3(j) 
        hrumono(50,j) = hrumono(50,j) + latno3(j)   !rwntl                  pdvas(50) = latno3(j)  
        hrumono(51,j) = hrumono(51,j) + tileno3(j)  !hmptl                  pdvas(51) = tileno3(j) 
        hrumono(52,j) = hrumono(52,j) + no3gw(j)  !rmn2tl                 pdvas(52) = no3gw(j)   
        hrumono(53,j) = hrumono(53,j) + percn(j)  !         pdvas(53) = percn(j)   
        
        !rmptl !sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed to sediment in surface runoff in HRU for day
                                                                    !sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed to sediment in surface runoff in HRU for day
        hrumono(54,j) = hrumono(54,j) + rchrg_n(j)  !no3pcp             pdvas(54) = rchrg_n(j)   
        hrumono(55,j) = hrumono(55,j) + revapn(j)   !tmx(j)             pdvas(55) = revapn(j)    
        hrumono(56,j) = hrumono(56,j) + gwseepn(j)     !tmn(j)             pdvas(56) = gwseepn(j)   
        hrumono(57,j) = hrumono(57,j) + gw_no3loss(j)    !tmpav(j)           pdvas(57) = gw_no3loss(j)
        hrumono(58,j) = hrumono(58,j) + shallst_n(j)  !hru_ra(j)          pdvas(58) = shallst_n(j) 
        hrumono(59,j) = hrumono(59,j) + surqsolp(j)   !                   pdvas(59) = surqsolp(j)  
        hrumono(60,j) = hrumono(60,j) + vap_tile    !usle           pdvas(60) = vap_tile     
        hrumono(61,j) = hrumono(61,j) + minpgw(j)  !qtile          pdvas(61) = minpgw(j)    
        
        hrumono(62,j) = hrumono(62,j) + (1.-strsw(j))    !bactrop + bactsedp                     pdvas(62) = (1.-strsw(j))       
        hrumono(63,j) = hrumono(63,j) + (1.-strstmp(j))    !bactrolp + bactsedlp                   pdvas(63) = (1.-strstmp(j))     
        hrumono(64,j) = hrumono(64,j) + (1.-strsn(j))   !no3gw(j)                           pdvas(64) = (1.-strsn(j))       
        hrumono(65,j) = hrumono(65,j) + (1.-strsp(j))    !minpgw(j)                                  pdvas(65) = (1.-strsp(j))       
        hrumono(66,j) = hrumono(66,j) + bio_ms(j) / 1000.   !sedminpa(j) + sedminps(j)                pdvas(66) = bio_ms(j) / 1000.   
        hrumono(67,j) = hrumono(67,j) + laiday(j)  !                                  pdvas(67) = laiday(j)           
        !tileno3(j) !bactsedp    |# cfu/m^2     |persistent bacteria transported with sediment in surface runoff
                                                                        !bactrop     |# cfu/m^2     |persistent bacteria transported to main channel with surface runoff
        hrumono(68,j) = hrumono(68,j) + yield(j)/1000.    !latno3(j)                      pdvas(68) = yield/1000.   
        hrumono(69,j) = hrumono(69,j) + yieldgrn(j)/1000. !gw_qdeep(j)                                    pdvas(69) = yieldgrn/1000.
        hrumono(70,j) = hrumono(70,j) + yieldbms(j)/1000.  !latq(j) - lpndloss - lwetloss              pdvas(70) = yieldbms/1000.
        hrumono(71,j) = hrumono(71,j) + yieldtbr(j)/1000.  !vap_tile                                   pdvas(71) = yieldtbr/1000.
        hrumono(72,j) = hrumono(72,j) + yieldrsd(j)/1000.  !vap_tile                                   pdvas(72) = yieldrsd/1000.

    
        
       
        hrumono(73,j) = hrumono(73,j) + bactrop + bactsedp     !sol_LSLC(1,j) /1000.  !N2O(j) *1000                         !! gN/ha
        hrumono(74,j) = hrumono(74,j) + bactrolp + bactsedlp    !qtile   !sol_LSLC(2,j) /1000.  !NO(j) *1000                           !! gN/ha 
        hrumono(75,j) = hrumono(75,j) + wtab(j) !tileno3(j)  !sol_LSLC(3,j) /1000.  !nh4_vol(j)                                !! kg N/ha      
        hrumono(76,j) = hrumono(76,j) + wat_tbl(j)  !latno3(j)   !sol_LSLNC(1,j) /1000.   !no3_denit(j)                             !! kg N/ha 
        hrumono(77,j) = hrumono(77,j) + sno_hru(j)  !gw_qdeep(j)     !sol_LSLNC(2,j)/1000.   !no3_nitr(j)                                !! kg N/ha  
        hrumono(78,j) = hrumono(78,j) + sol_rsd(1,j)/1000.  !latq(j) - lpndloss - lwetloss   !sol_LSLNC(3,j)/1000.  !no3_up(j)                                 !! kg N/ha   different from nplnt(j)
        hrumono(79,j) = hrumono(79,j) + sol_cov(j)/1000.    !vap_tile !sol_LMC(1,j)/1000.    !no3_fert(j) + no3_autof(j)         !! kg N/ha 
       
       
        hrumono(80,j) = hrumono(80,j) + N2O(j)*1000.    !sol_LMC(2,j) /1000.  !nh4_autof(j) + nh4_fert(j)         !! kg N/ha 
        
        hrumono(81,j) = hrumono(81,j) + NO(j)*1000.   !pdvas(81) = NO(j)*1000.    !        !                        !! g            !! kg N/ha     
        hrumono(82,j) = hrumono(82,j) + nh4_vol(j)   !pdvas(82) = nh4_vol(j)    !        !                            !a 
        hrumono(83,j) = hrumono(83,j) + no3_denit(j)  !pdvas(83) = no3_denit(j)   !         !                           ha 
        hrumono(84,j) = hrumono(84,j) + no3_nitr(j)  !pdvas(84) = no3_nitr(j)   !         !                            
        hrumono(85,j) = hrumono(85,j) + no3_up(j)   !pdvas(85) = no3_up(j)     !                               !! kg N
        hrumono(86,j) = hrumono(86,j) + no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j)    !pdvas(86) = no3_fert(j)+ no3_conf(j) +no3_grazf(j)+ no3_autof(j) 
        hrumono(87,j) = hrumono(87,j) + nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)    !pdvas(87) = nh4_fert(j)+ nh4_conf(j) + nh3_grazf(j)+ nh4_autof(j)
        hrumono(88,j) = hrumono(88,j) + orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd(j)    !pdvas(88) = orgn_fert(j)+ orgn_grazf(j)+ OrgN_Plt2Rsd (j)        
        
        hrumono(89,j) = hrumono(89,j) + solc_no3(j)     ! surfqc_d(j)                                  !! kg C/ha
        hrumono(90,j) = hrumono(90,j) + solc_nh4(j)     ! latc_d(j)                                      !! kg C/ha
        hrumono(91,j) = hrumono(91,j) + solc_orgn(j)/1000.  ! percc_d(j)                                   !! kg C/ha
        hrumono(92,j) = hrumono(92,j) + solc_orgp(j)/1000.  ! NPPC_d(j)                                 !! kg C/ha
        hrumono(93,j) = hrumono(93,j) + solc_orgc(j)/1000.  ! rspc_dnew(j) ! rspc_d(j)                                     !! kg C/ha
        hrumono(94,j) = hrumono(94,j) + solc_orgcs(j)/1000.     ! OrgC_Plt2Rsd(j)+OrgC_Fer(j) ! HRU_CH4g (j)      
 
        hrumono(95,j) = hrumono(95,j) + sedc_d(j)   !sol_BMC(1,j)/1000. ! HSC1/1000.  ! Sed_RPOC(j)                        !RPOC amount for hru (kg/ha)
        hrumono(96,j) = hrumono(96,j) + surfqc_d(j)     !sol_BMC(2,j) /1000. !BMC1/1000.  !Sed_LPOC(j)                       !LPOC amount for hru (kg/ha)
        hrumono(97,j) = hrumono(97,j) + latc_d(j)   !sol_BMC(3,j) /1000. !LMC1/1000.   ! HRU_RDOC(j)                     !RDOC amount for hru (kg/ha)
        hrumono(98,j) = hrumono(98,j) + percc_d(j)      !sol_HSC(1,j) /1000. !LSC1/1000.    !HRU_LDOC(j)                        !LDOC amount for hru  (kg/ha)
        hrumono(99,j) = hrumono(99,j) + NPPC_d(j)   !sol_HSC(2,j)/1000.   !HPC2/1000.   !HRU_DIC(j)                            !DIC amount for hru (kg/ha)    
        hrumono(100,j) = hrumono(100,j) + rspc_dnew(j)  !  soil profile respC- DIC loss             !! kg C/ha                  rspc_d(j)   
        hrumono(101,j) = hrumono(101,j) + OrgC_Plt2Rsd(j)+ OrgC_Fer(j)  !sol_HPC(1,j) /1000.   !BMC2/1000.  !LatQT_DOC(j)                                      !.ateral RDOC  (kg/ha)
        
        hrumono(102,j) = hrumono(102,j) + Sed_RPOC(j)    !pdvas(102) = Sed_RPOC(j)    
        hrumono(103,j) = hrumono(103,j) + Sed_LPOC(j)    !pdvas(103) = Sed_LPOC(j)                                        ! gw RDOC  (kg/ha)
        hrumono(104,j) = hrumono(104,j) + HRU_RDOC(j)  !pdvas(104) = HRU_RDOC(j)                              ! surface DIC from HRU (kg/ha) 
        hrumono(105,j) = hrumono(105,j) + HRU_LDOC(j)    !pdvas(105) = HRU_LDOC(j)                              !lateral DIC from HRU (kg/ha) 
        hrumono(106,j) = hrumono(106,j) + HRU_DIC(j)     !pdvas(106) = HRU_DIC(j)                                    !  Total DOC in soil profile (kg/ha£©
        hrumono(107,j) = hrumono(107,j) + SurQ_DOC(j)  !pdvas(107) = SurQ_DOC(j)                                     !  Total DIC in soil profile (kg/ha£©                               
        hrumono(108,j) = hrumono(108,j) + LatQT_DOC(j)   !pdvas(108) = LatQT_DOC(j)   
        hrumono(109,j) = hrumono(109,j) + PerQB_DOC(j)   !pdvas(109) = PerQB_DOC(j)   
        hrumono(110,j) = hrumono(110,j) + GwQ_DOC(j)   !pdvas(110) = GwQ_DOC(j)     
        
        
        hrumono(111,j) = hrumono(111,j) + SurQ_DIC(j)  !pdvas(111) = SurQ_DIC(j)  
        hrumono(112,j) = hrumono(112,j) + LatQT_DIC(j)  !pdvas(112) = LatQT_DIC(j) 
        hrumono(113,j) = hrumono(113,j) + PerQB_DIC(j) !pdvas(113) = PerQB_DIC(j)  
        hrumono(114,j) = hrumono(114,j) + solc_doc(j) !pdvas(114) = solc_doc(j)  
        hrumono(115,j) = hrumono(115,j) + solc_dic(j) !pdvas(115) = solc_dic(j)  
        
        
        hrumono(116,j) = hrumono(116,j) + sur_tmp(j)     !pdvas(116) = sur_tmp(j)   
        hrumono(117,j) = hrumono(117,j) + soltmp_50(j)     !pdvas(117) = soltmp_50(j) 
        hrumono(118,j) = hrumono(118,j) + OXGF(2,j)     !pdvas(118) = soltmp_100(j)
        hrumono(119,j) = hrumono(119,j) + soltmp_100(j)   !pdvas(119) = soltmp_150(j)
        hrumono(120,j) = hrumono(120,j) + soltmp_200(j)    !pdvas(120) = soltmp_200(j)
        
        
        
        hrumono(121,j) = hrumono(121,j) + soltmp_300(j)  !pdvas(121) = soltmp_300(j) 
        hrumono(122,j) = hrumono(122,j) + soltmp_500(j) !pdvas(122) = soltmp_500(j) 
        hrumono(123,j) = hrumono(123,j) + soltmp_1000(j) !pdvas(123) = soltmp_1000(j)
        hrumono(124,j) = hrumono(124,j) + sol_frozday(j) !pdvas(124) = sol_frozday(j)
        
        hrumono(125,j) = hrumono(125,j) + Sed_RPON(j)     !pdvas(125) = Sed_RPON(j)              
        hrumono(126,j) = hrumono(126,j) + Sed_LPON(j)     !pdvas(126) = Sed_LPON(j)              
        hrumono(127,j) = hrumono(127,j) + SurQ_DON(j)     !pdvas(127) = SurQ_DON(j)              
        hrumono(128,j) = hrumono(128,j) + LatQT_DON(j)      !pdvas(128) = LatQT_DON(j)             
        hrumono(129,j) = hrumono(129,j) + PerQ_DON (sol_nly(j),j)      !pdvas(129) = PerQ_DON (sol_nly(j),j)  
        hrumono(130,j) = hrumono(130,j) + rchrg_don (j)    !pdvas(130) = rchrg_don (j)            
        
        
        hrumono(131,j) = hrumono(131,j) +  GwQ_DON (j)              !pdvas(131) = GwQ_DON (j)         
        hrumono(132,j) = hrumono(132,j) +  shallst_don_decay(j)     !pdvas(132) = shallst_don_decay(j)
        hrumono(133,j) = hrumono(133,j) +  revap_don(j)             !pdvas(133) = revap_don(j)        
        hrumono(134,j) = hrumono(134,j) +  gwseep_don(j)            !pdvas(134) = gwseep_don(j)       
        hrumono(135,j) = hrumono(135,j) +  shallst_don(j)           !pdvas(135) = shallst_don(j)      
         
        hrumono(136,j) = hrumono(136,j) +  rchrg_doc(j)             !pdvas(136) = rchrg_doc(j)           
        hrumono(137,j) = hrumono(137,j) +  shallst_doc_decay(j)     !pdvas(137) = shallst_doc_decay(j)   
        hrumono(138,j) = hrumono(138,j) +  revap_doc(j)             !pdvas(138) = revap_doc(j)           
        hrumono(139,j) = hrumono(139,j) + gwseep_doc(j)             ! pdvas(139) = gwseep_doc(j)          
        hrumono(140,j) = hrumono(140,j) + shallst_doc(j)            ! pdvas(140) = shallst_doc(j)         
        
        hrumono(141,j) = hrumono(141,j) + GwQ_DIC(j)                !pdvas(141) = GwQ_DIC(j)    
        hrumono(142,j) = hrumono(142,j) + rchrg_dic(j)              !pdvas(142) = rchrg_dic(j)  
        hrumono(143,j) = hrumono(143,j) + revap_dic(j)              !pdvas(143) = revap_dic(j)  
        hrumono(144,j) = hrumono(144,j) + gwseep_dic(j)             !pdvas(144) = gwseep_dic(j) 
        hrumono(145,j) = hrumono(145,j) + shallst_dic(j)            !pdvas(145) = shallst_dic(j)
        
        hrumono(146,j) = hrumono(146,j) + sol_soc(1,j)                  !pdvas(146) = sol_soc(1,j)
        hrumono(147,j) = hrumono(147,j) + sol_soc(2,j)                  !pdvas(147) = sol_soc(2,j)
        hrumono(148,j) = hrumono(148,j) + sol_soc(3,j)                  !pdvas(148) = sol_soc(3,j)
        hrumono(149,j) = hrumono(149,j) + sol_soc(4,j)                  !pdvas(149) = sol_soc(4,j)
        hrumono(150,j) = hrumono(150,j) + sol_soc(5,j)                  !pdvas(150) = sol_soc(5,j)
        hrumono(151,j) = hrumono(151,j) + sol_soc(6,j)                  !pdvas(151) = sol_soc(6,j)
        hrumono(152,j) = hrumono(152,j) + sol_soc(7,j)                  !pdvas(152) = sol_soc(7,j)
        hrumono(153,j) = hrumono(153,j) + sol_soc(8,j)                  !pdvas(153) = sol_soc(8,j)

        !!----------output_N.hru-------------------------------------
       hrumonN(1,j) = hrumonN(1,j)+ solc_no3_ini(j)        !! State variable; initial total NO3 in soil ; kg N/ha  
       hrumonN(2,j) = hrumonN(2,j)+ solc_no3(j)             !! State variable; Total no3 in soil profile; kg N/ha 
       hrumonN(3,j) = hrumonN(3,j)+ solc_nh4_ini(j)        !! State variable; initial total NH3 in soil ; kg N/ha  
       hrumonN(4,j) = hrumonN(4,j)+ solc_nh4(j)             !! State variable; Total nh3 in soil profile; kg N/ha 
       hrumonN(5,j) = hrumonN(5,j)+ solc_orgn_ini(j)/1000. !! State variable; Initial orgn in soil profile; ton N/ha                                                                                                                                       !! initial total orgN in soil   kg N/ha  
       hrumonN(6,j) = hrumonN(6,j)+ solc_orgn(j)/1000.     !! State variable; Total orgn in soil profile; ton N/ha     
       hrumonN(7,j) = hrumonN(7,j)+no3_fert(j)+no3_conf(j)+no3_grazf(j)    !! kg N/ha 
       hrumonN(8,j) = hrumonN(8,j)+no3_autof(j)                           !! kg N/ha 
       hrumonN(9,j) = hrumonN(9,j)+nh4_fert(j)+nh4_conf(j)+nh3_grazf(j)    !! kg N/ha 
       hrumonN(10,j) = hrumonN(10,j)+ nh4_autof(j)                        !! kg N/ha 
       hrumonN(11,j) = hrumonN(11,j)+ no3_rain(j)                         !! kg N/ha 
       hrumonN(12,j) = hrumonN(12,j)+ nh4_rain(j)                         !! kg N/ha 
       hrumonN(13,j) = hrumonN(13,j)+ orgn_fert(j)+orgn_grazf(j)          !! including cont., auto, and fer; kg N/ha
       hrumonN(14,j) = hrumonN(14,j)+ OrgN_Plt2Rsd (j)                    !! Total residue input N ; kg N/ha                   
       hrumonN(15,j) = hrumonN(15,j)+ no3_immo(j)                   
       hrumonN(16,j) = hrumonN(16,j)+ no3_up(j) 
       hrumonN(17,j) = hrumonN(17,j)+ no3_nitr(j)                         !! total nitr N kg N/ha 
       hrumonN(18,j) = hrumonN(18,j)+ no3_denit(j)                        !! total den N kg N/ha          
       hrumonN(19,j) = hrumonN(19,j)+ absorbed_no3(j) 
       hrumonN(20,j) = hrumonN(20,j)+ nh4_min(j)                          !!OrgN_Plt2Rsd (j)
       hrumonN(21,j) = hrumonN(21,j)+ nh4_vol(j)
       hrumonN(22,j) = hrumonN(22,j)+ absorbed_nh3(j)
       hrumonN(23,j) = hrumonN(23,j)+ N2O_nit(j) 
       hrumonN(24,j) = hrumonN(24,j)+ NO_nit(j)                           !*1000  
       hrumonN(25,j) = hrumonN(25,j)+ N2O_den(j)                          !*1000  !! g /ha    !!    30
       hrumonN(26,j) = hrumonN(26,j)+ NO_den(j)                           !*1000  !! g /ha     !!    32
       hrumonN(27,j) = hrumonN(27,j)+ N2_den(j)    
       hrumonN(28,j) = hrumonN(28,j)+ N2O(j)                              !*1000!*1000 N2O_den(j)  !! gN/ha     !!SWATCUP =87
       hrumonN(29,j) = hrumonN(29,j)+ NO(j)                               !*1000 !NO_den(j) *1000   !NO(j) *1000 
       hrumonN(30,j) = hrumonN(30,j)+ surqno3_0(j)                        !gw_qdeep(j)
       hrumonN(31,j) = hrumonN(31,j)+ tileno3(j)
       hrumonN(32,j) = hrumonN(32,j)+ latno3_0(j)                         !      latno3(j) 
       hrumonN(33,j) = hrumonN(33,j)+ percn(j)                            !! kg N/ha 
       hrumonN(34,j) = hrumonN(34,j)+ sedorgn_0(j)                        !cmtot_kgh(j)        
       hrumonN(35,j) = hrumonN(35,j)+ Sed_RPON(j)                         ! RPON from hru to stream(kg/ha)                                     
       hrumonN(36,j) = hrumonN(36,j)+ Sed_LPON(j)                         ! LPON from hru to stream (kg/ha)
       hrumonN(37,j) = hrumonN(37,j)+ SurQ_DON(j)                         ! Surface RDON to stream  (kg/ha)
       hrumonN(38,j) = hrumonN(38,j)+ LatQT_DON(j)                        ! Lateral RDON to stream  (kg/ha)
       hrumonN(39,j) = hrumonN(39,j)+ PerQ_DON (sol_nly(j),j)             ! RDON percolation amount from lowest soil layer to shallow aquifer
       hrumonN(40,j) = hrumonN(40,j)+ rchrg_don (j)                       ! RDON recharge to aquifer  (kg/ha) 
       hrumonN(41,j) = hrumonN(41,j)+ revap_don(j)                        ! revap DON to soils             (kg/ha)
       hrumonN(42,j) = hrumonN(42,j)+ GwQ_DON (j)                         ! GW RDON to stream (kg/ha) 
       hrumonN(43,j) = hrumonN(43,j)+ shallst_don_decay(j)                ! DON decay in shallow aquifer (kg/ha) 
       hrumonN(44,j) = hrumonN(44,j)+ gwseep_don(j)                       ! DON seep to deeper aquifer  (kg/ha) 
       hrumonN(45,j) = hrumonN(45,j)+ shallst_don(j)                      ! state     DON in shallow aquifer  (kg/ha) 
 
       !!  Balance for Soil NH4 
       hrumonN(46,j) = hrumonN(46,j)+ nh4_autof(j) + nh4_fert(j)                    &
                       + nh4_conf(j) + nh3_grazf(j) + nh4_min(j) + nh4_rain(j)              &
                       - ( (solc_nh4(j)- solc_nh4_ini(j)) + nh4_vol(j) + no3_nitr(j)        &
                       + absorbed_nh3(j) )  
       !!  Balance for Soil NO3 
       hrumonN(47,j) = hrumonN(47,j)+ no3_fert(j) + no3_autof(j)            &
                        + no3_conf(j)+ no3_grazf(j) + no3_nitr(j) + no3_rain(j)              &
                        - ( (solc_no3(j) -  solc_no3_ini(j) )+ no3_up(j) + surqno3_0(j)      &
                        + latno3_0(j) + percn(j) + no3_denit(j) + no3_immo(j)               &
                        + absorbed_no3(j)+ N2O_nit(j) + NO_nit(j) )                                                               !!  Balance for Soil NO3           ! soltmp_500(j) 
       !!  Balance for Soil orgN including residue n 
       hrumonN(48,j) = hrumonN(48,j)+ OrgN_Plt2Rsd (j) + orgn_fert(j)       &
                     + orgn_grazf(j)+ no3_immo(j) + absorbed_no3(j)+ absorbed_nh3(j)      &
                     - ( (solc_orgn(j) - solc_orgn_ini(j)) + sedorgn_0(j)               &
                     + PerQ_DON (sol_nly(j),j)+ SurQ_DON_0(j)+LatQT_DON_0(j)            &
                     + nh4_min(j)    )    
       !!  Balance for total soil N
       hrumonN(49,j) = hrumonN(49,j)+ (OrgN_Plt2Rsd(j)+no3_fert(j)          &
                       + no3_autof(j)+ no3_conf(j)+ no3_grazf(j) +nh4_fert(j)                &
                       + nh4_autof(j)+nh4_conf(j) + nh3_grazf(j) + no3_rain(j)               &
                       + nh4_rain(j) + orgn_fert(j)+ orgn_grazf(j)                          &                        
                       - no3_up(j) - sedorgn_0(j) - surqno3_0(j) - latno3_0(j)             &
                       - percn(j) - N2O(j) - NO(j)- N2_den(j) - nh4_vol(j)                  &
                       - PerQ_DON (sol_nly(j),j)- SurQ_DON_0(j)- LatQT_DON_0(j))           & 
                       - ((solc_no3(j)-  solc_no3_ini(j))                                 &
                       + (solc_nh4(j)-solc_nh4_ini(j))                                   &
                       + (solc_orgn(j)- solc_orgn_ini(j)))  
    
        hrumonN(50,j) = hrumonN(50,j)+ plantn(j)
        hrumonN(51,j) = hrumonN(51,j)+ grainN(j) 
        hrumonN(52,j) = hrumonN(52,j)+ (  Min_ML_MBN(j)       &          ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
                         + Min_SL_MBN(j)                        &!mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
                         + Min_SL_SHN(j)                         &!mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
                         + Min_MB_SHN(j)                        &!mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
                         + Min_MB_PHN(j)                        & !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
                         + Min_SH_MBN(j)                      &!mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
                         + Min_SH_PHN(j)                     &!mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
                         + Min_PH_MBN(j)                     &    !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                            
                         - IMM_ML_MBN(j)                   &! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
                         - IMM_SL_MBN(j)                   &     ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
                         - IMM_SL_SHN(j)                   &     ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
                         - IMM_MB_SHN(j)                   &            ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
                         - IMM_MB_PHN(j)                   &     ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
                         - IMM_SH_MBN(j)                   &        !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
                         - IMM_SH_PHN(j)                   &         ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
                         - IMM_PH_MBN(j)    )                      ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)                                     !! kg C/ha
       
        hrumonN(53,j) = hrumonN(53,j)+IMMO_ERR(j) 
       

        wtrmon(1,j) = wtrmon(1,j) + pndev / cnv
        wtrmon(2,j) = wtrmon(2,j) + pndsep / cnv
        wtrmon(3,j) = wtrmon(3,j) + pndpcp / cnv
        wtrmon(4,j) = wtrmon(4,j) + pndflwi / cnv
        wtrmon(5,j) = wtrmon(5,j) + pndsedin / hru_ha(j)
        wtrmon(6,j) = wtrmon(6,j) + pndflwo / cnv
        wtrmon(7,j) = wtrmon(7,j) + pndsedo / hru_ha(j)
        wtrmon(8,j) = wtrmon(8,j) + wetpcp / cnv
        wtrmon(9,j) = wtrmon(9,j) + wetflwi / cnv
        wtrmon(10,j) = wtrmon(10,j) + wetsedi / hru_ha(j)
        wtrmon(11,j) = wtrmon(11,j) + wetev / cnv
        wtrmon(12,j) = wtrmon(12,j) + wetsep / cnv
        wtrmon(13,j) = wtrmon(13,j) + wetflwo / cnv
        wtrmon(14,j) = wtrmon(14,j) + wetsedo / hru_ha(j)
        wtrmon(15,j) = wtrmon(15,j) + potpcpmm
        wtrmon(16,j) = wtrmon(16,j) + potevmm
        wtrmon(17,j) = wtrmon(17,j) + potsepmm
        wtrmon(18,j) = wtrmon(18,j) + potflwo
        wtrmon(19,j) = wtrmon(19,j) + potflwi(j) / cnv
        wtrmon(20,j) = wtrmon(20,j) + potsedi(j) / hru_ha(j)
        wtrmon(21,j) = wtrmon(21,j) + potsedo / hru_ha(j)

      !! watershed summations
        if (ffcst == 0 .and. iscen == 1) then
            wshddayo(1) = wshddayo(1) + subp(j) * hru_dafr(j)
            wshddayo(3) = wshddayo(3) + surfq(j) * hru_dafr(j)
            wshddayo(4) = wshddayo(4) + latq(j) * hru_dafr(j)
            wshddayo(5) = wshddayo(5) + sepbtm(j) * hru_dafr(j)
            wshddayo(6) = wshddayo(6) + qdr(j) * hru_dafr(j)
            wshddayo(7) = wshddayo(7) + etday * hru_dafr(j)
            wshddayo(8) = wshddayo(8) + tmx(j) * hru_dafr(j)
            wshddayo(9) = wshddayo(9) + tmn(j) * hru_dafr(j)
            wshddayo(12) = wshddayo(12) + sedyld(j)
            wshddayo(13) = wshddayo(13) + pndsedin
            wshddayo(14) = wshddayo(14) + pndsedo
            wshddayo(15) = wshddayo(15) + pndsedc
            wshddayo(16) = wshddayo(16) + wetsedi
            wshddayo(17) = wshddayo(17) + wetsedo
            wshddayo(18) = wshddayo(18) + wetsedc
            wshddayo(19) = wshddayo(19) + pndev
            wshddayo(20) = wshddayo(20) + pndsep
            wshddayo(21) = wshddayo(21) + pndpcp
            wshddayo(22) = wshddayo(22) + pndflwi
            wshddayo(23) = wshddayo(23) + pndflwo
            wshddayo(24) = wshddayo(24) + wetev
            wshddayo(25) = wshddayo(25) + wetsep
            wshddayo(26) = wshddayo(26) + wetpcp
            wshddayo(27) = wshddayo(27) + wetflwi
            wshddayo(28) = wshddayo(28) + wetflwo
            wshddayo(33) = wshddayo(33) + pndflwi - pndflwo
            wshddayo(35) = wshddayo(35) + sol_sw(j) * hru_dafr(j)
            wshddayo(36) = wshddayo(36) + snomlt * hru_dafr(j)
            wshddayo(37) = wshddayo(37) + snoev * hru_dafr(j)
            wshddayo(38) = wshddayo(38) + tloss * hru_dafr(j)
            wshddayo(39) = wshddayo(39) + snofall * hru_dafr(j)
            wshddayo(40) = wshddayo(40) + sedorgn(j) * hru_dafr(j)
            wshddayo(41) = wshddayo(41) + sedorgp(j) * hru_dafr(j)
            wshddayo(42) = wshddayo(42) + surqno3(j) * hru_dafr(j)
            wshddayo(43) = wshddayo(43) + surqsolp(j) * hru_dafr(j)
            wshddayo(44) = wshddayo(44) + nplnt(j) * hru_dafr(j)
            wshddayo(45) = wshddayo(45) + latno3(j) * hru_dafr(j)
            wshddayo(46) = wshddayo(46) + percn(j) * hru_dafr(j)

            !! wshddayo(47) - wshddayo (103) not used

            wshddayo(47) = wshddayo(47) + tmpav(j) * hru_dafr(j) 
            wshddayo(48) = wshddayo(48) + N2O(j)  * hru_dafr(j)    !! kg/ha
            wshddayo(49) = wshddayo(49) + NO(j)  * hru_dafr(j)       !! kg/ha
            wshddayo(50) = wshddayo(50) + nh4_vol(j)  * hru_dafr(j)  !!kg/ha
            wshddayo(51) = wshddayo(51) + HRU_CH4g (j)* hru_dafr(j) 
            wshddayo(52) = wshddayo(52) + rspc_d(j) * hru_dafr(j)    !!kg/ha
            wshddayo(53) = wshddayo(53) + sedc_d(j) * hru_dafr(j)    !!kg/ha
            wshddayo(54) = wshddayo(54) + surfqc_d(j) * hru_dafr(j)  !!kg/ha
            wshddayo(55) = wshddayo(55) + latc_d(j)* hru_dafr(j)     !!kg/ha
            wshddayo(56) = wshddayo(56) + percc_d(j) * hru_dafr(j)   !!kg/ha
            wshddayo(57) = wshddayo(57) + NPPC_d(j) * hru_dafr(j)    !!kg/ha
            wshddayo(58) = wshddayo(58) + snofall* hru_dafr(j)             !!mm
            wshddayo(59) = wshddayo(59) + sno_hru(j)* hru_dafr(j)     !!mm  
            wshddayo(60) = wshddayo(60) + solc_no3(j)*  hru_dafr(j)                               !! kg N/ha  34
            wshddayo(61) = wshddayo(61) + solc_nh4(j)* hru_dafr(j)                               !! kg N/ha   35
            wshddayo(62) = wshddayo(62) + solc_orgn(j)*  hru_dafr(j) /1000                        !!t N/ha  36
            wshddayo(63) = wshddayo(63) + solc_orgp(j)*  hru_dafr(j) /1000                      !!t P/ha   37
            wshddayo(64) = wshddayo(64) + sol_frozday(j)* hru_dafr(j)   
            wshddayo(65) = wshddayo(65) + hru_ra(j)* hru_dafr(j)
            
            wshddayo(66) = wshddayo(66) + GwQ_DOC(j) * hru_dafr(j)               ! (kg/ha)    !!extrepcp_day(j)* hru_dafr(j)                        
            wshddayo(67) = wshddayo(67) + solc_orgc(j)* hru_dafr(j) /1000                ! tC/ha
            wshddayo(68) = wshddayo(68) + solc_orgcs(j)* hru_dafr(j) /1000              ! tC/ha
            wshddayo(69) = wshddayo(69) + Sed_RPOC(j)* hru_dafr(j)                ! RPOC amount for subbasins (kg/ha)                                
            wshddayo(70) = wshddayo(70) + Sed_LPOC(j)* hru_dafr(j)                ! LPOC amount for subbasins (kg/ha)                
            wshddayo(71) = wshddayo(71) + HRU_RDOC(j)* hru_dafr(j)             ! RDOC amount for subbasins  (kg/ha)
            wshddayo(72) = wshddayo(72) + HRU_LDOC(j)* hru_dafr(j)             ! LDOC amount for subbasins  (kg/ha)      
            wshddayo(73) = wshddayo(73) + HRU_DIC(j)* hru_dafr(j)                 ! DIC amount for subbasins (kg/ha)                                           
            wshddayo(74) = wshddayo(74) + 1000.  *(chl_a(j) * (qday * qdfr * cnv) * 1.e-6 )  / ai0  * 0.4 /  (da_km*100) ! Algea C kg/ha
                                                                                    ! 0.4=algal carbon to biomass ratio                     
           
            wshddayo(104) = wshddayo(104) + gw_q(j) * hru_dafr(j)
            wshddayo(105) = wshddayo(105) + revapday * hru_dafr(j)
            wshddayo(106) = wshddayo(106) + gwseep * hru_dafr(j)
            wshddayo(107) = wshddayo(107) + rchrg(j) * hru_dafr(j)
            wshddayo(108) = wshddayo(108) + pet_day * hru_dafr(j)
            wshddayo(109) = wshddayo(109) + qtile * hru_dafr(j)
            wshddayo(110) = wshddayo(110) + no3gw(j) * hru_dafr(j)
            wshddayo(111) = wshddayo(111) + tileno3(j) * hru_dafr(j)
            wshddayo(113) = wshddayo(113) + gw_qdeep(j) * hru_dafr(j)     
          
          
            do ii=1,mstdo
                if(wshddayo(ii).ne.wshddayo(ii)) wshddayo(ii) = 0  !! float error debug, Jaehak Jeong, 2011 Feb
            end do     
        
        else if (ffcst == 1) then
        
              if (j == 1) fcstcnt = fcstcnt + 1
              fcstaao(1) = fcstaao(1) + subp(j) * hru_dafr(j)
              fcstaao(2) = fcstaao(2) + snofall * hru_dafr(j)
              fcstaao(3) = fcstaao(3) + snomlt * hru_dafr(j)
              fcstaao(4) = fcstaao(4) + snoev * hru_dafr(j)
              fcstaao(5) = fcstaao(5) + (qday + tloss) * hru_dafr(j)
              fcstaao(6) = fcstaao(6) + latq(j) * hru_dafr(j)
              fcstaao(7) = fcstaao(7) + qtile * hru_dafr(j)
              fcstaao(8) = fcstaao(8) + gw_q(j) * hru_dafr(j)
              fcstaao(9) = fcstaao(9) + revapday * hru_dafr(j)
              fcstaao(10) = fcstaao(10) + gwseep * hru_dafr(j)
              fcstaao(11) = fcstaao(11) + rchrg(j) * hru_dafr(j)
              fcstaao(12) = fcstaao(12) + qdr(j) * hru_dafr(j)
              fcstaao(13) = fcstaao(13) + sepbtm(j) * hru_dafr(j)
              fcstaao(14) = fcstaao(14) + etday * hru_dafr(j)
              fcstaao(15) = fcstaao(15) + pet_day * hru_dafr(j)
              fcstaao(16) = fcstaao(16) + tloss * hru_dafr(j)
        end if

        !! pesticide summary
        if (hrupest(j) == 1) then
          do k = 1, npmx
            !! HRU summary
            hrupstd(k,1,j) = pst_surq(k,j) * 1.e6 * hru_ha(j)
            hrupstd(k,2,j) = pst_sed(k,j) * 1.e6 * hru_ha(j)
            hrupstd(k,3,j) = (pst_surq(k,j) + pst_sed(k,j)) * 1.e6
            hrupstd(k,4,j) = lat_pst(k) * 1.e6 * hru_ha(j)
            !! watershed summary
            wpstdayo(k,1) = wpstdayo(k,1) + pst_surq(k,j) * hru_dafr(j)  &
	    								* 1.e6
            wpstdayo(k,2) = wpstdayo(k,2) + pst_sed(k,j) * hru_dafr(j) * &
	    								1.e6
            wpstdayo(k,3) = wpstdayo(k,3) + pstsol(k) * hru_dafr(j)
            wpstdayo(k,4) = wpstdayo(k,4) + lat_pst(k) * hru_dafr(j)
          end do
        end if
        
      end if

      return
      end