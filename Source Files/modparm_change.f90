      module parm
       
      character(len=13),dimension (:), allocatable :: hedb     !(63)
     
      integer :: mxsubch    
      integer :: itempa    
     
        
      integer icalen
      real :: prf_bsn       !!prf_bsn     |none          |Basinwide peak rate adjustment factor for sediment
      
      real, dimension (:), allocatable :: alph_e         !in nlch.f90 for tile flow
      real, dimension (:), allocatable :: co_p, surlag, cdn, nperco !co_p in nlch.f90. calculated but not used. !o_p(j) = co_p(j) * (1. - alph_e(j)) + vno3_c * alph_e(j) !tileno3(j) = co * qtile     !Daniel 1/2012
      !cdn              |none          |denitrification exponential rate coefficient
      !nperco      |none          |nitrate percolation coefficient (0-1), nlch.f90
      real, dimension (:), allocatable :: cmn, phoskd, psp, sdnco
      !cmn              |none          |rate factor for humus mineralization on active organic N
      !phoskd      |none          |Phosphorus soil partitioning coefficient; solp.f90
      !psp          |none          |Phosphorus availability index. The fraction of fertilizer P remaining in labile pool after initial rapid phase of P sorption
!!   change per JGA 8/31/2011 gsm for output.mgt 
      real ::  burn_frlb, pst_kg, r2adj_bsn
      !yldkg(:,:,:) |kg/ha         |yield (dry weight) by crop type in the HRU
      !burn_frlb   |none          |fraction of biomass and residue that burn(input in management file) range (0 - 1.0); burnop.f90 
      !pst_kg       |kg/ha            |amount of pesticide applied to HRU
      !r2adj_bsn   |none          |basinwide retention parameter adjustment factor (greater than 1)! D. Moriasi 4/8/2014; not used
      real, dimension (:), allocatable :: yield(:),yieldgrn(:), yieldbms(:), yieldtbr(:), yieldn(:), yieldp(:), yieldrsd(:)
      real :: hi_bms, hi_rsd 
      
!!    arrays for Landscape Transport Capacity 5/28/2009 nadia
      real, dimension (:), allocatable :: l_k1, l_k2, l_lambda, l_beta
      real, dimension (:), allocatable :: l_gama, l_harea, l_vleng
      real, dimension (:), allocatable :: l_vslope, l_ktc
      
!!    arrays for Biofilm variables
      real, dimension (:), allocatable :: biofilm_mumax, biofilm_kinv
      real, dimension (:), allocatable :: biofilm_klw, biofilm_kla
      real, dimension (:), allocatable :: biofilm_cdet, biofilm_bm
       
!!    new arrays for routing units
      real, dimension (:,:), allocatable :: hru_rufr, daru_km, ru_k
      real, dimension (:,:), allocatable :: ru_c, ru_eiq, ru_ovsl, ru_a
      real, dimension (:,:), allocatable :: ru_ovs, ru_ktc
      real, dimension (:), allocatable :: gwq_ru, qdayout
      integer, dimension (:), allocatable :: ils2, ils2flag
      integer :: iru, mru, irch, isub, idum, mhyd_bsn, ipest, ils_nofig
      integer :: mhru1
      integer, dimension (:), allocatable :: mhyd1 , irtun

!! septic variables for output.std
      real :: wshd_sepno3, wshd_sepnh3, wshd_seporgn, wshd_sepfon
      real :: wshd_seporgp, wshd_sepfop, wshd_sepsolp, wshd_sepbod
      real :: wshd_sepmm
      integer, dimension (:), allocatable :: isep_hru                
!! septic variables for output.std

      real :: fixco, nfixmx, rsd_covco, vcrit, res_stlr_co
      !fixco       |none          |nitrogen fixation coefficient; nfix.f90
      !nfixmx      |kg/ha         |maximum daily n-fixation
      !rsd_covco   |              |residue cover factor for computing fraction of cover; cfactor.f90
      !vcrit       |              |Critical velocity; rtsed.f90
      !res_stlr_co |none          |reservoir sediment settling coefficient; readres.f90
      
      real :: wshd_sw, wshd_snob, wshd_pndfr, wshd_pndv, wshd_pndsed
      !wshd_sw       |mm H2O        |average amount of water stored in soil
      !wshd_snob     |mm H20        |average amount of water stored in snow
      !wshd_pndfr  |none          |fraction of watershed area which drains into ponds
      !wshd_pndv   |mm H2O        |mass balance discrepancy for pond water
      !wshd_pndsed |metric tons   |total amount of suspended sediment in ponds
      real :: wshd_wetfr, wshd_resfr, wshd_resha, wshd_pndha, percop
      !wshd_wetfr  |none          |fraction of watershed area which drains into wetlands
      !wshd_resfr  |none          |fraction of watershed area that drains into reservoirs
      !wshd_resha  |ha            |watershed area in hectares which drains into reservoirs
      !wshd_pndha  |ha            |watershed area in hectares which drains into ponds
      !percop       |none          |pesticide percolation coefficient (0-1)
      real :: wshd_fminp, wshd_ftotn, wshd_fnh3, wshd_fno3, wshd_forgn
      !wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
      !wshd_ftotn  |kg N/ha       |average annual amount of N (mineral & organic) applied in watershed
      !wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in watershed
      !wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in watershed
      !wshd_forgn  |kg N/ha       |average annual amount of organic N applied in watershed

      real :: wshd_forgp, wshd_ftotp, wshd_yldn, wshd_yldp, wshd_fixn
      !wshd_forgp  |kg P/ha       |average annual amount of organic P applied in watershed
      !wshd_ftotn  |kg N/ha       |average annual amount of N (mineral & organic) applied in watershed
      !wshd_ftotp  |kg P/ha       |average annual amount of P (mineral & organic) applied in watershed
      !wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in watershed in the yield
      !wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in watershed in the yield
      !wshd_fixn   |kg N/ha       |average annual amount of nitrogen added to plant biomass via fixation
      real :: wshd_pup, wshd_wstrs, wshd_nstrs, wshd_pstrs, wshd_tstrs
      !wshd_pup    |kg P/ha        |average annual amount of plant uptake of phosphorus 
      !wshd_wstrs  |stress units     |average annual number of water stress units
      !wshd_nstrs  |stress units     |average annual number of nitrogen stress
      !wshd_pstrs  |stress units     |average annual number of phosphorus stress
      !wshd_tstrs  |stress units     |average annual number of temperature stress
      real :: wshd_astrs
      
      real :: wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_dnit, ffcb
      !wshd_hmn      |kg N/ha       |average annual amount of nitrogen moving from active organic to nitrate pool in watershed
      !wshd_rwn      |kg N/ha       |average annual amount of nitrogen moving from active organic to stable organic pool in watershed
      !wshd_hmp      |kg P/ha       |average annual amount of phosphorus moving from organic to labile pool in watershed
      !wshd_rmn      |kg N/ha       |average annual amount of nitrogen moving from fresh organic (residue) to nitrate and active organic pools in watershed
      !wshd_dnit     |kg N/ha       |average annual amount of nitrogen lost from nitrate pool due to denitrification in watershed
      !ffcb        |none          |initial soil water content expressed as a fraction of field capacity
      real :: wshd_rmp, wshd_voln, wshd_nitn, wshd_pas, wshd_pal, wdpq
      !wshd_rmp      |kg P/ha       |average annual amount of phosphorus moving from fresh organic (residue) to labile and organic pools in watershed
      !wshd_voln     |kg N/ha       |average annual amount if nitrogen lost by ammonia volatilization in watershed
      !wshd_nitn     |kg N/ha       |average annual amount of nitrogen moving from the NH3 to the NO3 pool by nitrification in the watershed
      !wshd_pas     |kg P/ha       |average annual amount of phosphorus moving from active mineral to stable mineral in watershed
      !wshd_pal     |kg P/ha       |average annual amount of phosphorus moving from labile mineral to active mineral pool in watershed
      !wdpq        |1/day         |Die-off factor for persistent bacteria in soil solution.
      real :: wshd_plch, wshd_raino3, ressedc, basno3f, basorgnf, wof_p
      ! wshd_plch     |kg P/ha      |average annual amount of phosphorus leached into second soil layer
      !wshd_raino3 |kg N/ha       |average annual amount of NO3 added to soil
      !ressedc     |metric tons   |net change in sediment in reservoir during day
      !basno3f     |kg N/ha       |final average amount of nitrogen in the nitrate pool in watershed soil
      real :: wshd_pinlet, wshd_ptile  !!no definitations
      !wshd_pinlet = wshd_pinlet + solp_tileo / hru_ha(j) * hru_dafr(j)
      !wshd_ptile = wshd_ptile + vap_tile * hru_dafr(j)
      real :: basminpf, basorgpf, sftmp, smtmp, smfmx, smfmn, wgpq
      !basminpf    |kg P/ha       |final average amount of phosphorus in the mineral P pool in watershed soil
      !basorgpf    |kg P/ha       |final average amount of phosphorus in the organic P pool in watershed soil
      !sftmp       |deg C         |Snowfall temperature
      !smtmp       |deg C         |Snow melt base temperature 
      !smfmn       |mm/deg C/day  |Minimum melt rate for snow during year (Dec. 21) where deg C refers to the air temperature.
      !smfmx       |mm/deg C/day  |Maximum melt rate for snow during year (June 21) where deg C refers to the air temperature.
!!                               |SMFMX and SMFMN allow the rate of snow melt
!!                               |to vary through the year. These parameters 
!!                               |are accounting for the impact of soil
!!                               |temperature on snow melt.
      !wgpq        |1/day         |Growth factor for persistent bacteria in soil
      real :: wshd_resv, wshd_ressed, basno3i, basorgni, basminpi, wdlpq
      !wshd_resv   |m**3          |total volume of water in all reservoirs in the watershed
      !wshd_ressed  |metric tons   |total amount of suspended sediment in reservoirs in the watershed
      !basno3i       |kg N/ha       |average amount of nitrogen initially in the nitrate pool in watershed soil
      !basorgni      |kg N/ha       |average amount of nitrogen initially in the organic N pool in watershed soil
      !basminpi      |kg P/ha       |average amount of phosphorus initially in the mineral P pool in watershed soil
      !basminpi      |kg P/ha       |average amount of phosphorus initially in the mineral P pool in watershed soil
      !wdlpq       |1/day         |Die-off factor for less persistent bacteria in soil solution.
      

      real :: basorgpi, peakr, pndsedin, sw_excess, albday, wglpq, wdps
      !basorgpi      |kg P/ha       |average amount of phosphorus initially in the organic P pool in watershed soil
      !peakr       |m^3/s         |peak runoff rate
      !pndsedin    |metric tons   |sediment entering pond during day
      !sw_excess   |mm H2O        |amount of water in excess of field capacity
      !albday      |none          |albedo of ground for day
      !wglpq       |1/day         |Growth factor for less persistent bacteria in soil solution.
      !wdps        |1/day         |Die-off factor for persistent bacteria adsorbed to soil particles.
      !real :: wtabelo, timp, tilep, wt_shall                   !!R683 1/13/22 nbs
      real :: wtabelo, timp, wt_shall                            !!R683 1/13/22 nbs
      !wtabelo
      !timp        |none          |Snow pack temperature lag factor (0-1)
      !tilep
      !wt_shall    |mm H2O        |shallow water table depth above the impervious layer
      
      real :: sq_rto
      !sq_rto
      real :: tloss, inflpcp, snomlt, snofall, fixn, qtile, crk, latlyr
      !tloss         |mm H2O        |amount of water removed from surface runoff
      !inflpcp     |mm H2O        |amount of precipitation that infiltrates into soil (enters soil)
      !snomlt      |mm H2O      |amount of snow melt in HRU on current day      
      !snofall       |mm H2O        |amount of precipitation falling as freezing
      !fixn          |kg N/ha       |amount of nitrogen added to plant biomass
      !qtile       |mm H2O        |drainage tile flow in soil profile for the day
      !crk         |mm H2O        |percolation due to crack flow
      !latlyr      |mm H2O        |lateral flow in soil layer for the day
      real :: pndloss, wetloss,potloss, lpndloss, lwetloss
      !pndloss
      !wetloss
      !potloss
      !lpndloss
      !lwetloss
      real :: sedrch, fertn, sol_rd, cfertn, cfertp, sepday, bioday
      !sedrch      |metric tons   |sediment transported out of reach on day
      !fertn         |kg N/ha       |total amount of nitrogen applied to soil
      !sol_rd      |mm            |current rooting depth
      !cfertn      |kg N/ha       |total amount of nitrogen applied to soil
      !cfertp       |kg P/ha       |total amount of phosphorus applied to soil
      !sepday           |mm H2O        |percolation from soil layer
      !bioday      |kg            |biomass generated on current day in HRU
      real :: sepcrk, sepcrktot, fertno3, fertnh4, fertorgn, fertsolp
      !sepcrk      |mm H2O        |water entering cracks in soil
      !sepcrktot
      !fertno3
      !fertnh4
      !fertorgn
      !fertsolp
      real :: fertorgp
      !fertorgp
      real :: fertp, grazn, grazp, soxy, qdfr, sdti, rtwtr, ressa, wgps
      !fertp         |kg P/ha       |total amount of phosphorus applied to soil
      !grazn        |kg N/ha       |total amount of nitrogen applied to soil during grazing in HRU on day
      !grazp        |kg P/ha       |total amount of phosphorus applied to soil during grazing in HRU on day
      !soxy        |mg O2/L       |saturation concetration of dissolved oxygen; hhwatqual.f90
      !qdfr        |none          |fraction of water yield that is surface
      !sdti        |m^3/s         |flow rate in reach for day
      !rtwtr            |m^3 H2O        |water leaving reach on day
      !ressa         |ha            |surface area of reservoir on day
      !wgps        |1/day         |Growth factor for persistent bacteria
      real :: rttime, rchdep, rtevp, rttlc, da_km, resflwi, wdlps, wglps
      !rttime      |hr            |reach travel time
      !rchdep      |m             |depth of flow on day
      !rtevp       |m^3 H2O       |evaporation from reach on day
      !rttlc       |m^3 H2O       |transmission losses from reach on day
      !da_km       |km2           |area of the watershed in square kilometers
      !resflwi      |m^3 H2O       |water entering reservoir on day
      !wdlps       |1/day         |Die-off factor for less persistent bacteria 
      !wglps       |1/day         |Growth factor for less persistent bacteria
      real :: resflwo, respcp, resev, ressep,ressedi,ressedo,dtot,wdprch
      !resflwo       |m^3 H2O       |water leaving reservoir on day
      !respcp      |m^3 H2O       |precipitation on reservoir for day
      !resev       |m^3 H2O       |evaporation from reservoir on day
      !ressep      |m^3 H2O       |seepage from reservoir on day
      !ressedi      |metric tons   |sediment entering reservoir during time step
      !ressedo       |metric tons   |sediment leaving reservoir during time step
      !dtot
      !wdprch      |1/day         |Die-off factor for persistent bacteria in streams
      real :: nperco_bsn,pperco_bsn,rsdco,phoskd_bsn,voltot
      !nperco_bsn
      !pperco_bsn
      !rsdco       |none          |residue decomposition coefficient
      !phoskd_bsn
      !voltot      |mm            |total volume of cracks expressed as depth
      real :: volcrmin, msk_x
      !volcrmin    |mm            |minimum crack volume allowed in any soil layer
      !msk_x       |none          |weighting factor controling relative importance of inflow rate and outflow rate in determining storage on reach
      real :: uno3d, canev, usle, rcn, surlag_bsn,bactkdq,precipday,wdpf
      !uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
      !canev        |mm H2O        |amount of water evaporated from canopy
      !usle        |metric tons/ha|daily soil loss predicted with USLE equation
      !rcn         |mg/L          |Concentration of nitrogen in the rainfall
      !surlag_bsn
      !bactkdq     |none          |Bacteria soil partitioning coefficient.
      !precipday   |mm H2O      |amount of water reaching soil surface in HRU
      !wdpf        |1/day         |Die-off factor for persistent bacteria on
      real :: thbact, wpq20, wlpq20, wps20, wlps20, bactrop, bactsedp
      !thbact      |none          |temperature adjustment factor for bacteria die-off/growth
      !wpq20       |1/day         |Overall rate change for persistent bacteria in soil solution.
      !wlpq20      |1/day         |Overall rate change for less persistent bacteria in soil solution.
      !wps20       |1/day         |Overall rate change for persistent bacteria adsorbed to soil particles.
      !wlps20      |1/day         |Overall rate change for less persistent bacteria adsorbed to soil particles.
      !bactrop     |# cfu/m^2     |persistent bacteria transported to main channel with surface runoff
      !bactsedp    |# cfu/m^2     |persistent bacteria transported with sediment in surface runoff
      real :: bactlchp, bactlchlp, enratio, wetpcp, pndpcp, wetsep, wgpf
      !bactlchp    |# cfu/m^2     |persistent bacteria removed from soil surface layer by percolation
      !bactlchlp   |# cfu/m^2     |less persistent bacteria removed from soil surface layer by percolation
      !enratio     |none          |enrichment ratio calculated for current day
      !wetpcp      |m^3 H2O       |precipitation on wetland for day
      !pndpcp        |m^3 H2O       |precipitation on pond during day
      !wetsep        |m^3 H2O       |seepage from wetland bottom for day
      !wgpf        |1/day         |Growth factor for persistent bacteria on foliage.
      real :: pndsep, wetev, pndev, pndsedo, wetsedo, pndflwi, wetflwi
      !pndsep      |m^3 H2O       |seepage from pond on day
      !wetev         |m^3 H2O       |evaporation from wetland for day
      !pndev         |m^3 H2O       |evaporation from pond on day
      !pndsedo     |metric tons   |sediment leaving pond during day
      !wetsedo       |metric tons   |sediment loading from wetland for day
      !pndflwi     |m^3 H2O       |volume of water flowing into pond on day
      !wetflwi       |m^3 H2O       |volume of water flowing in wetland on day
      real :: pndflwo, wetflwo, wetsedi, da_ha, vpd
      real, dimension (:), allocatable :: twlpnd, twlwet               !!srini pond/wet infiltration to shallow gw storage  !!R??? nbs     
      !pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
      !wetflwo       |m^3 H2O       |volume of water flowing out wetland on day
      !wetsedi       |metric tons   |sediment loading to wetland for day
      !da_ha       |ha            |area of watershed in hectares
      !twlwet      |mm H2O        |water lost through seepage from wetlands on day in HRU
      !twlpnd      |mm H2O        |water lost through seepage from ponds on day in HRU
      
      !vpd         |kPa           |vapor pressure deficit
      real :: bactrolp, bactsedlp, evrch, evlai, pet_day, ep_day, wdlpf
      ! bactrolp    |# cfu/m^2     |less persistent bacteria transported to main channel with surface runoff
      ! bactsedlp   |# cfu/m^2     |less persistent bacteria transported with sediment in surface runoff
      !evrch       |none          |Reach evaporation adjustment factor. 
      !evlai          |none        |leaf area index at which no evaporation occurs from the water surface. This
!!                                  |variable is used in ponded HRUs (eg rice)
!!                                  |where evaporation from the water surface
!!                                  |is restricted by the plant canopy cover.
!!                                  |Evaporation from the water surface equals
!!                                  |potential ET when LAI = 0 an decreases
!!                                  |linearly to O when LAI = EVLAI
      !pet_day      |mm H2O        |potential evapotranspiration on current day
      !ep_day      |mm H2O           |actual amount of transpiration that occurs on day in HRU
      !wdlpf       |1/day         |Die-off factor for less persistent bacteria on foliage.
      real :: snoev, sno3up, adj_pkr, n_updis,p_updis,nactfr,reactw
      !snoev        |mm H2O        |amount of water in snow lost through |sublimation on current day
      !sno3up       |kg N/ha       |amount of nitrate moving upward in the soil profile in watershed
      !adj_pkr     |none          |peak rate adjustment factor in the subbasin.
      !n_updis     |none           |nitrogen uptake distribution parameter
      !p_updis     |none           |phosphorus uptake distribution parameter
      !nactfr        |none          |nitrogen active pool fraction. The fraction of organic nitrogen in the active pool.
      !reactw        |mg pst        |amount of pesticide in lake water lost through reactions
      real :: sdiegropq, sdiegrolpq, sdiegrops, sdiegrolps, es_day
      !sdiegropq   |# cfu/m^2     |average annual change in the number of persistent bacteria colonies in soil solution in watershed
      !sdiegrolpq  |# cfu/m^2     |average annual change in the number of average annual change in the number of less persistent bacteria colonies in soil solution in watershed
      !sdiegrops   |# cfu/m^2     |average annual change in the number of persistent bacteria colonies on soil particles in watershed
      !sdiegrolps  |# cfu/m^2     |average annual change in the number of less persistent bacteria colonies on soil particles in watershed
      !es_day       |mm H2O        |actual amount of evaporation (soil et) that occurs on day in HRU
      real :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max, wof_lp
      !sbactrop    |# colonies/ha |average annual number of persistent bacteria transported to main channel with surface runoff in solution
      !sbactrolp   |# colonies/ha |average annual number of less persistent bacteria transported to main channel with surface runoff in solution
      !sbactsedp   |# colonies/ha |average annual number of persistent bacteria transported with sediment in surface runoff
      !sbactsedlp  |# colonies/ha |average annual number of less persistent bacteria transported with sediment in surface runoff
      !ep_max       |mm H2O        |maximum amount of transpiration (plant et)
      !wof_lp      |none          |fraction of less persistent bacteria on foliage that is washed off by a rainfall event
      real :: sbactlchp, sbactlchlp, psp_bsn, rchwtr, resuspst, setlpst
      !sbactlchp   |# cfu/m^2     |average annual number of persistent bacteria lost from soil surface layer by percolation
      !sbactlchlp  |# cfu/m^2     |average annual number of less persistent bacteria lost from soil surface layer by percolation
      !psp_bsn !not used!
      !rchwtr       |m^3 H2O       |water stored in reach at beginning of day
      !resuspst      |mg pst        |amount of pesticide moving from sediment to lake water due to resuspension
      !setlpst       |mg pst        |amount of pesticide moving from water to sediment due to settling
      real :: bsprev, bssprev, spadyo, spadyev, spadysp, spadyrfv
      !bsprev      |mm H2O        |surface runoff lagged from prior day
      !bssprev       |mm H2O        |lateral flow lagged from prior day of simulation
      !spadyo         |mm H2O        |average annual amount of water released to main channel from potholes in watershed
      !spadyev        |mm H2O        |average annual amount of water removed from potholes by evaporation in watershed
      !spadysp        |mm H2O        |average annual amount of water removed from potholes by seepage in watershed
      !spadyrfv       |mm H2O        |average annual amount of precipitation on potholes in watershed
      real :: spadyosp
      !spadyosp
      real :: qday, usle_ei, al5, pndsedc, no3pcp, rcharea, volatpst
      !qday          |mm H2O        |surface runoff loading to main channel for day in HRU
      !al5         |none        |fraction of total rainfall on day that occurs during 0.5h highest intensity rainfall
      !usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
      !pndsedc     |metric tons   |net change in sediment in pond during day
      !no3pcp        |kg N/ha       |nitrate added to the soil in rainfall
      !rcharea     |m^2           |cross-sectional area of flow
      !volatpst      |mg pst        |amount of pesticide lost from lake water by volatilization
      real :: wetsedc, uobw, ubw, uobn, uobp, respesti, wglpf
      !wetsedc     |metric tons   |net change in sediment in wetland during day
      !uobw        |none          |water uptake normalization parameter
      !ubw         |none          |water uptake distribution parameter
      !uobn        |none          |nitrogen uptake normalization parameter
      !respesti    |mg pst        |pesticide entering reservoir on day
      !wglpf       |1/day         |Growth factor for less persistent bacteria on foliage
      real :: snocovmx, snocov1, snocov2, rexp, rcor, lyrtile, lyrtilex
      !snocovmx    |mm H2O        |Minimum snow water content that corresponds to 100% snow cover. If the snow water content is 
!!                               |less than SNOCOVMX, then a certain percentage 
!!                               |of the ground will be bare.
      !snocov1     |none          |1st shape parameter for snow cover equation
      !snocov2     |none          |2nd shape parameter for snow cover equation
      !rexp        |none          |value of exponent for mixed exponential rainfall distribution (needed only if IDIST=1)
      !rcor        |none          |correction coefficient for generated rainfall
      !lyrtile     |mm H2O        |drainage tile flow in soil layer for day
      !lyrtilex
      real :: ai0, ai1, ai2, ai3, ai4, ai5, ai6, rhoq, tfact, sno50cov
      !ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
      !ai1              |mg N/mg alg   |fraction of algal biomass that is N
      !ai2              |mg P/mg alg   |fraction of algal biomass that is P
      !ai3              |mg O2/mg alg  |the rate of oxygen production per unit of algal photosynthesis
      !ai4              |mg O2/mg alg  |the rate of oxygen uptake per unit of algae respiration
      !ai5              |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
      !ai6              |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
      !rhoq             |1/hr          |algal respiration rate at 20 deg C
      !tfact        |none          |fraction of solar radiation computed in the temperature heat balance that is photosynthetically active
      !sno50cov    |none          |Fraction of SNOCOVMX that corresponds to 50%
      real :: mumax, lambda0, lambda1, lambda2, k_l, k_n, k_p, p_n
      !mumax            |1/hr          |maximum specific algal growth rate at 20 deg C
      !lambda0          |1/m           |non-algal portion of the light extinction
      !lambda1          |1/(m*ug chla/L)|linear algal self-shading coefficient
      !lambda2
      !k_l              |MJ/(m2*hr)    |half saturation coefficient for light
      !k_n              |mg N/L        |michaelis-menton half-saturation constant
      !k_p              |mg P/L        |michaelis-menton half saturation constant
      ! p_n              |none          |algal preference factor for ammonia
      real :: rnum1, autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
      !rnum1       |none          |variable to hold value for rnum1s(:)
      !autop       |kg P/ha       |amount of phosphorus applied in auto-fert
      !auton       |kg N/ha       |amount of nitrogen applied in auto-fert
      !etday         |mm H2O        |actual amount of evapotranspiration that occurs on day in HRU
      ! hmntl         |kg N/ha       |amount of nitrogen moving from active organic to nitrate pool in soil profile on current day in HRU
      !rwntl         |kg N/ha       |amount of nitrogen moving from active organic to stable organic pool in soil profile on current day in HRU
      !hmptl         |kg P/ha       |amount of phosphorus moving from the organic to labile pool in soil profile on current day in HRU
      !rmn2tl        |kg N/ha       |amount of nitrogen moving from the fresh organic (residue) to the nitrate(80%) and
!!                                 |active organic(20%) pools in soil profile
!!                                 |on current day in HRU
      real :: rmptl,wdntl,cmn_bsn,rmp1tl,roctl,gwseep,revapday,reswtr
      !rmptl         |kg P/ha       |amount of phosphorus moving from the fresh organic (residue) to the labile(80%)
!!                                 |and organic(20%) pools in soil profile
!!                                 |on current day in HRU
      !wdntl         |kg N/ha       |amount of nitrogen lost from nitrate pool
      !rmp1tl        |kg P/ha       |amount of phosphorus moving from the labile mineral pool to the active mineral pool in
!!                                 |the soil profile on the current day in the
!!                                 |HRU
      !roctl         |kg P/ha       |amount of phosphorus moving from the active mineral pool to the stable mineral pool
!!                                 |in the soil profile on the current day in
!!                                 |the HRU
      !cmn_bsn
      !gwseep      |mm H2O        |amount of water recharging deep aquifer on current day in HRU
      !revapday    |mm H2O        |amount of water moving from the shallow aquifer into the soil profile or being taken up by plant roots in the shallow aquifer
      !reswtr           |m^3 H2O     |initial reservoir volume
      real :: bury, difus, reactb, solpesto, petmeas, wdlprch, wdpres
      !bury          |mg pst        |loss of pesticide from active sediment layer
      !difus         |mg pst        |diffusion of pesticide from sediment to lake water
      !reactb        |mg pst        |amount of pesticide in sediment that is lost through reactions
      !solpesto      |mg pst        |soluble pesticide in outflow on day
      !petmeas     |mm H2O        |potential ET value read in for day
      !wdlprch     |1/day         |Die-off factor for less persistent bacteria in streams
      !wdpres      |1/day         |Die-off factor for persistent bacteria in reservoirs
      real :: sorpesto, spcon_bsn, spexp_bsn, solpesti, sorpesti,wdlpres
      !sorpesto      |mg pst        |sorbed pesticide in outflow on day
      !spcon_bsn
      !spexp_bsn
      !solpesti      |mg pst        |soluble pesticide entering reservoir
      !sorpesti      |mg pst        |sorbed pesticide entering reservoir
      !wdlpres     |1/day         |Die-off factor for less persistent bacteria in reservoirs
      real :: snoprev, swprev, shallstp, deepstp, msk_co1, msk_co2
      !snoprev     |mm H2O        |amount of water stored as snow on previous day
      !swprev      |mm H2O        |amount of water stored in soil profile in the HRU on the previous day
      !shallstp    |mm H2O        |depth of water in shallow aquifer in HRU on previous day
      !deepstp     |mm H2O        |depth of water in deep aquifer in HRU
      !msk_co1     |none          |calibration coefficient to control impact of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
      !msk_co2     |none          |calibration coefficient to control impact of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
      real :: ressolpo, resorgno, resorgpo, resno3o, reschlao, resno2o
      !ressolpo    |kg P          |soluble P leaving reservoir on day
      !resorgno    |kg N          |organic N leaving reservoir on day
      !resorgpo    |kg P          |orgainc P leaving reservoir on day
      !resno3o     |kg N          |nitrate leaving reservoir on day
      !reschlao    |kg chl-a      |chlorophyll-a leaving reservoir on day
      !resno2o     |kg N          |nitrite leaving reservoir on day
      real :: resnh3o, qdbank, potpcpmm, potevmm, potsepmm, potflwo
      !resnh3o     |kg N          |ammonia leaving reservoir on day
      !qdbank      |m^3 H2O       |streamflow contribution from bank storage
      !potpcpmm      |mm H2O        |precipitation falling on pothole water body expressed as depth over HRU
      !potevmm       |mm H2O        |volume of water evaporated from pothole expressed as depth over HRU
      !potsepmm      |mm H2O        |seepage from pothole expressed as depth over HRU
      !potflwo       |mm H2O        |discharge from pothole expressed as depth over HRU
      real :: potsedo, pest_sol, trnsrch, wp20p_plt, bactminp, bactminlp
      !potsedo       |metric tons   |sediment leaving pothole on day
      !pest_sol
      !trnsrch     |none          |fraction of transmission losses from main channel that enter deep aquifer
      !wp20p_plt   |1/day         |Overall rate change for persistent bacteria on foliage
      !bactminp    |# cfu/m^2     |Threshold detection level for persistent bacteria
!!                               |when bacteria levels drop to this amount the
!!                               |model considers bacterial in the soil to be 
!!                               |insignificant and sets the levels to zero
      !bactminlp   |# cfu/m^2     |Threshold detection level for less persistent bacteria
!!                               |when bacteria levels drop to this amount the 
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levesl to zero
      real :: wp20lp_plt,cncoef,cdn_bsn,sdnco_bsn,bact_swf,bactmx,bactmin
      !wp20lp_plt  |1/day         |Overall rate change for less persistent bacteria on foliage
      !cncoef           |none          |plant ET curve number coefficient
      !cdn_bsn
      !sdnco_bsn
      !bact_swf    |none          |fraction of manure containing active colony forming units (cfu)
      !bactmx      |none          |bacteria percolation coefficient
      !bactminlp   |# cfu/m^2     |Threshold detection level for less persistentbacteria
!!                               |when bacteria levels drop to this amount the 
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levesl to zero
      real :: chla_subco, tb_adj, cn_froz, dorm_hr, smxco
      !chla_subco  |fraction      |regional adjustment on sub chla_a loading
      !tb_adj      |none          |adjustment factor for subdaily unit hydrograph basetime
      !cn_froz
      !dorm_hr     |hours         |time threshold used to define dormant
      !smxco       |              |adjustment factor for max curve number s factor (0-1)
      real :: depimp_bsn, ddrain_bsn, tdrain_bsn, gdrain_bsn
      !depimp_bsn  |mm            |depth to impervious layer. Used to model
      !ddrain_bsn  |mm            |depth to the sub-surface drain
      !tdrain_bsn  |hours         |time to drain soil to field capacity
      !gdrain_bsn
      real :: rch_san, rch_sil, rch_cla, rch_sag, rch_lag, rch_gra
      !rch_san
      !rch_sil
      !rch_cla
      !rch_sag
      !rch_lag
      !rch_gra
!!    declare mike van liew variables
      real :: hlife_ngw_bsn, ch_opco_bsn, ch_onco_bsn
      !hlife_ngw_bsn
      !ch_opco_bsn
      !ch_onco_bsn
      real :: bc1_bsn, bc2_bsn, bc3_bsn, bc4_bsn, rcn_sub_bsn, decr_min    
      !bc1_bsn     
      !bc2_bsn
      !bc3_bsn
      !bc4_bsn
      !rcn_sub_bsn |mg/kg         |Concentration of nitrogen in the rainfall
      !decr_min    |              |Minimum daily residue decay
      real :: anion_excl_bsn
      !anion_excl_bsn
      
      
!!    delcare mike van liew variables

!    Drainmod tile equations  01/2006
      real, dimension (:), allocatable :: wat_tbl,sol_swpwt
      !wat_tbl(:)  |mm            |water table based on depth from soil surface
      !sol_swpwt
      real, dimension (:,:), allocatable :: vwt
      !vwt
	  real :: re_bsn, sdrain_bsn, sstmaxd_bsn
	  !re_bsn      |mm            |Effective radius of drains (range 3.0 - 40.0) 
	  !sdrain_bsn  |mm            |Distance bewtween two drain or tile tubes (range 7600.0 - 30000.0)
	  !sstmaxd_bsn
	  real :: drain_co_bsn, pc_bsn, latksatf_bsn 
	  !drain_co_bsn |mm-day-1     |Drainage coeffcient (range 10.0 - 51.0)
	  !pc_bsn      |mm h-1        |Pump capacity (def val = 1.042 mm h-1 or 25 mm day-1)
	  !latksatf_bsn |             |Multiplication factor to determine lateral ksat from SWAT ksat input value for HRU	  
!    Drainmod tile equations  01/2006


      integer :: i_subhw, imgt, idlast, iwtr, ifrttyp, mo_atmo, mo_atmo1
      !i_subhw = 1    !! perform sediment routing for headwater subbasins 
      !if (imgt == 1) then      !open (143, file="output.mgt", recl=600)
      !idlast      |none          |number of days simulated in year
      !output.pot and output.wtr turned on by same code named IWTR in file.cio
      !ifrttyp      |fertilizer type
      !readatmodep.f90(64):        read (atmofile_num,1001,iostat=eof) mo_atmo1, iyr_atmo1
      integer :: ifirstatmo, iyr_atmo, iyr_atmo1, matmo
      !ifirstatmo
      !iyr_atmo
      ! read (atmofile_num,1001,iostat=eof) mo_atmo1, iyr_atmo1
      !read (atmofile_num,*) matmo  !!maximum number of subbasins
      integer :: mrg, mch, mcr, mpdb, mcrdb, mfdb, mhru, mhyd, mfcst
      !mrg         |none          |max number of rainfall/temp gages
      !mch         |none          |max number of channels
      !mcr         |none          |max number of crops grown per year
      !mpdb        |none          |max number of pesticides in pest.dat
      !mcrdb       |none          |max nunber of crops in crop.dat
      !mfdb        |none          |max number of fertilizers in fert.dat
      !mhru        |none          |max number of HRUs
      !mhyd        |none          |max number of hydrographs
      !mfcst       |none        |maximum number of forecast stations
      integer :: mnr, myr, mcut, mgr, msubo, mrcho, isubwq, ffcst,mrcho1   
      ! mnr         |none          |max number of years of rotation
      !myr         |none          |max number of years of simulation
      !mcut        |none          |max number of cuttings per year
      !mgr         |none          |max number of grazings per year
      !msubo       |none          |max number of variables in output.sub
      !mrcho       |none          |max number of variables in reach file
      !isubwq      |none          |subbasin water quality code |0 do not calculate algae/CBOD |1 calculate algae/CBOD
      !ffcst
      !mrcho1 = 76

      integer :: nhru, isproj, mo, nbyr, immo, nrch, nres, irte, i_mo
      !nhru           |none          |number of HRUs in watershed
      !isproj      |none          |special project code: |1 test rewind (run simulation twice)
      !read (rtefile_num,5100,iostat=eof) (ch_erodmo(irch,mo), mo = 1,12); \readrte.f90(299):      do mo = 1, 12      
      !nbyr        |none        |number of calendar years simulated
      !immo        |none          |current cumulative month of simulation
      !nrch         |none          |number of reaches in watershed
      !nres        |none          |number of reservoirs in watershed
      !irte        |none          |water routing method: |0 variable storage method |1 Muskingum method
      !i_mo        |none        |month being simulated
      integer :: icode, ihout, inum1, inum2, inum3, inum4, wndsim, ihru
      !icodes(:)   |none          |routing command code:
      !ihouts(:)   |none          |For ICODES equal to; command.f90
      !inum1s(:)   |none          |For ICODES equal to; command.f90
      !inum2s(:)   |none          |For ICODES equal to; command.f90
      !inum3s(:)   |none          |For ICODES equal to; command.f90
      !inum4s(:)   |none          |For ICODES equal to; command.f90
      !wndsim      |none          |wind speed input code
      !!ihru          |none         |HRU number
      integer :: inum5, inum6, inum7, inum8, icfac
      !inum5, 
      !inum6, 
      !inum7, 
      !inum8
      !icfac       |              | icfac = 0 for C-factor calculation using  Cmin (as described in manual) = 1 for new C-factor calculation from RUSLE (no minimum needed)
      integer :: nrgage, ntgage, nrgfil, ntgfil, nrtot, nttot, mrech
      !nrgage      |none        |number of raingage files
      !ntgage      |none        |number of temperature gage files
      !nrgfil      |none        |number of rain gages per file
      !ntgfil      |none        |number of temperature gages per file
      !nrtot       |none        |total number of rain gages
      !nttot       |none        |total number of temperature gages
      !mrech       |none          |max number of rechour files
      integer :: lao, igropt, npmx, irtpest, curyr, tmpsim, icrk, iihru
      !lao         |NA            |Qual2E light averaging option. Qual2E defines
      !igropt           |none          |Qual2E option for calculating the local
      !npmx        |none          |number of different pesticides used in the simulation
      !irtpest      |none             |the sequence number of the pesticide type
      !curyr       |none          |current year of simulation
      !tmpsim      |none          |temperature input code
      !icrk        |none          |crack flow code |1 simulate crack flow in watershed
      !do iihru = 1, hrutot(inum1)
      
!    Drainmod tile equations  01/2006
	integer :: ismax, itdrn, iwtdn, iroutunit, ires_nut
	  !ismax       |none          |maximum depressional storage selection flag/code
	  !itdrn       |none          |tile drainage equations flag/code |1 simulate tile flow using subroutine drains(wt_shall); |0 simulate tile flow using subroutine origtile(wt_shall,d) 
      !iwtdn       |none          |water table depth algorithms flag/code |1 simulate wt_shall using subroutine new water table depth routine
!!                               |0 simulate wt_shall using subroutine original water table depth routine  
      !iroutunit   |none          | not being implemented in this version
      !ires_nut = 1 new equations 0 = old equations (Ikenberry)
!    Drainmod tile equations  01/2006

      integer :: mtil, mvaro, mrecd, idist, mudb, mrecm, mrecc, iclb
      !mtil        |none          |max number of tillage types in till.dat
      !mvaro       |none          |max number of variables routed through the reach
      !mrecd       |none          |max number of recday files
      !idist       |none          |rainfall distribution code |  0 for skewed distribution |  1 for mixed exponential distribution
      !mudb        |none          |max number of urban land types in urban.dat
      !mrecm       |none          |max number of recmon files
      !mrecc       |none          |max number of reccnst files
      !iclb        |none        |auto-calibration flag
      
      integer :: mrecy, ipet, nyskip, ideg, ievent, slrsim, iopera
      !mrecy       |none          |max number of recyear files
      !ipet        |none          |code for potential ET method
      !nyskip      |none          |number of years of output summarization and printing to skip
      !ideg        |none          |channel degredation code |1: compute channel degredation (downcutting and widening)
      !ievent      |none          |rainfall/runoff code |0 daily rainfall/curve number technique |1 sub-daily rainfall/Green&Ampt/hourly |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
      !slrsim      |none          |solar radiation input code |1 measured data read for each subbasin
!!                               |2 data simulated for each subbasin
      !iopera = Max(iopera,iopera_sub
      
      integer :: id1, idaf, idal, leapyr, mo_chk, rhsim, mstdo
      !id1         |julian date   |first day of simulation in year
      !idaf        |julian date |beginning day of simulation
      !idal        |julian date |ending day of simulation
      !leapyr      |none          |leap year flag: |0  leap year |1  regular year
      !mo_chk        |none          |current month of simulation
      !rhsim       |none          |relative humidity input code |1 measured data read for each subbasin |2 data simulated for each subbasin
      ! mstdo       |none          |max number of variables summarized in output.std
      integer :: ifirsts, ifirsth, ifirstw, nstot, nhtot, nwtot, icst
      !ifirsts     |none          |solar radiation data search code 0 first day of solar radiation data located in file
!!                               |1 first day of solar radiation data not located in file
      !ifirsth     |none          |relative humidity data search code
      !ifirstw     |none          |wind speed data search code
      !nstot       |none        |number of solar radiation records in file
      !nhtot       |none        |number of relative humidity records in file
      !nwtot       |none        |number of wind speed records in file
      !icst
      integer :: ilog, i, iyr, itotr, iwq, iskip, scenario, ifirstpet
      !ilog         |none         |streamflow print code |0 print streamflow in reach |1 print Log10 streamflow in reach
      !do i = id1, idlst                            !! begin daily loop; if (iyr == 2012 .and. i == 278 .and. j == 16) then
      !iyr         |year          |current year of simulation (actual year)
      ! itotr       |none          |number of output variables printed (.rch)
      !iwq         |none          |stream water quality code |0 do not model stream water quality |1 model stream water quality
      !iskip       |none          |flag for calculations performed only for
      !scenario
      !ifirstpet   |none          |potential ET data search code
      integer :: itotb,itots,iprp,pcpsim,itoth,nd_30,iops,iphr,isto,isol
      !itots       |none          |number of output variables printed (output.hru)
      !itotb       |none          |number of output variables printed (output.sub)
      !iprp        |none        |print code for output.pst file
      !pcpsim      |none          |rainfall input code
      !itoth       |none        |number of HRUs printed (output.hru/output.wtr)
      !nd_30 = nd_30 + 1 |if (nd_30 > 30) nd_30 = 1; !the use of this variable is not clear. 
      !iops = ioper(ihru)      !zero0-Du.f90(344):      ioper = 1      !zero0-Du.f90(345):      iopera = 1
      !iphr        |nond        |print hydrograph ; IPHR = 0 no print; IPHR = 1 print file
      !output.swr ISTO = 0 no print    ISTO = 1 print file
      integer :: iscen, fcstyr, fcstday, fcstcycles, subtot, ogen
      !iscen
      !fcstyr      |year        |beginning year of forecast period
      !fcstday     |julian date |beginning date of forecast period
      !fcstcycles  |none        |number of times forecast period is simulated
      !subtot       |none         |number of subbasins in watershed
      !ogen = igen
      integer :: msub, mhruo, mres, mapp, mpst, mlyr, igen, iprint, iida
      !msub        |none          |max number of subbasins
      !msubo       |none          |max number of variables in output.sub
      !mhruo       |none          |max number of variables in output.hru
      !mres        |none          |max number of reservoirs
      !mapp        |none        |maximum number of applications
      !mpst        |none          |max number of pesticides used in wshed
      !mlyr        |none          |max number of soil layers
      !igen        |none          |random number generator code: 0: use default numbers 1: generate new numbers in every simulation
      !iprint      |none        |print code:0=monthly,1=daily,2=annual
      !iida             |day           |day being simulated
      integer ::  mhruo1                                          !!-------output.hru---------------------------------------------------
      !mhruo1
      integer :: fcstcnt, icn, ised_det, mtran, idtill, motot
      !fcstcnt
      !icn         |none          |CN method flag: !!                               |0 use traditional SWAT method which bases CN on soil moisture
!!                               |1 use alternative method which bases CN on plant ET
!!                               |2 use tradtional SWAT method which bases CN on soil moisture but rention is adjusted for mildly-sloped tiled-drained watersheds
      !ised_det    |none          |max half-hour rainfall fraction calc option: |0 generate max half-hour rainfall fraction from triangular distribution
!!                               |1 use monthly mean max half-hour rainfall fraction
      !mtran
      !idtill = mgt1iop(nop(j),j)
      !allocate_parms.f90(78):      motot = 600             !! (50 years limit); used in rcn_mo(motot,msub), rammo_mo(motot,msub), drydep_no3_mo(motot,msub), drydep_nh4_mo(motot,msub)
      integer, dimension(100) :: ida_lup, iyr_lup
      !if (ida_lup(no_lup) == i .and. iyr_lup(no_lup) == iyr) then
      integer :: no_lup, no_up, nostep
      !no_lup
      !no_up: counter of land use updates
      !read (resfile_num,*,iostat=eof) nostep; res.f90
      
!  routing 5/3/2010 gsm per jga    
! date
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone              !zone        |NA            |time difference with respect to Coordinated Universal Time (ie Greenwich Mean Time)
      character(len=80) :: prog
      character(len=13) :: slrfile, wndfile, rhfile, petfile, calfile
      !calfile     |NA          |name of file containing calibration parameters
      character(len=13) :: atmofile, lucfile
      !read (file_cio2_num,5000,iostat=eof) atmofile
      !lucfile
      character(len=13) :: septdb
      !septdb      !  NA        ! name of septic tank database file 
      character(len=13) :: dpd_file, wpd_file, rib_file, sfb_file, lid_file
      !read (pndfile_num,5101,iostat=eof) dpd_file
      !read (pndfile_num,5101,iostat=eof) wpd_file
      !read (pndfile_num,5101,iostat=eof) rib_file
      !read (pndfile_num,5101,iostat=eof) sfb_file
      !read (pndfile_num,5101,iostat=eof) lid_file
      integer, dimension (:), allocatable :: ifirstr, idg, ifirsthr
      !ifirstr(:)  |none          |measured data search code |0 first day of measured data located in file 1 first day of measured data not located in file
      !idg(:)      |none        |array location of random number seed
      !ifirsthr(:) |none          |measured data search code 0 first day of measured data located in file 1 first day of measured data not located in file
      integer, dimension (:), allocatable :: values, ndays
      !values
      ! ndays(:)    |julian date   |julian date for last day of preceding month (where the array location is the number of the month). The dates are for leap years
      integer, dimension (:), allocatable :: ndays_noleap, ndays_leap
      !ndays_noleap = (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
      !ndays_leap = (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
!     apex/command output files
      integer :: mapex
      !maximum number of APEX command
      real, dimension (:), allocatable :: flodaya, seddaya, orgndaya
      real, dimension (:), allocatable :: orgpdaya, no3daya, minpdaya
      integer, dimension (:), allocatable :: idapa, iypa, ifirsta
      !read (apex_in_num+inum1,*) idapa(inum1), iypa(inum1), flodaya(inum1), seddaya(inum1), orgndaya(inum1), orgpdaya(inum1), no3daya(inum1), minpdaya(inum1)
      real, dimension (:), allocatable :: hi_targ, bio_targ, tnyld
      !hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at planting
      !bio_targ(:,:,:)|kg/ha          |biomass target
      !tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of yield used in autofertilization
      integer, dimension (:), allocatable :: mo_transb, mo_transe
      !read (figfile2_num,5004) mo_transb(inum5s(idum)), mo_transe(inum5s(idum)), ih_tran(inum5s(idum))
      integer, dimension (:), allocatable :: ih_tran
      !read (figfile2_num,5004) mo_transb(inum5s(idum)),mo_transe(inum5s(idum)), ih_tran(inum5s(idum))      
!     apex/command output files

!  septic inputs
!! septic change added iseptic 1/28/09 gsm
      integer :: msdb, iseptic
      real, dimension (:), allocatable :: sptqs,percp
      real, dimension (:), allocatable :: sptbodconcs, spttssconcs
      real, dimension (:), allocatable :: spttnconcs, sptnh4concs
      real, dimension (:), allocatable :: sptno3concs, sptno2concs
      real, dimension (:), allocatable :: sptorgnconcs, spttpconcs
      real, dimension (:), allocatable :: sptminps, sptorgps      
      real, dimension (:), allocatable :: sptfcolis ,failyr,qstemm               
!! septic changes added 1/28/09 gsm

      real, dimension (:), allocatable :: bio_amn, bio_bod, biom,rbiom
      !bio_amn; not used
      !bio_bod(:)       |kg/ha         |BOD concentration in biozone
      !biom(:)          |kg/ha         |biomass of live bacteria in biozone
      ! rbiom(:)         |kg/ha         |daily change in biomass of live bacteria
      real, dimension (:), allocatable :: fcoli, bio_ntr, bz_perc
      !fcoli(:)         |cfu/100ml     |concentration of the fecal coliform in the biozone 
      !bio_ntr
      !bz_perc(j) = sepday
      real, dimension (:), allocatable :: plqm,sep_cap,bz_area
      !plqm             |kg/ha         |plaque in biozone
      !sep_cap(:)      |none          |Number of permanent residents in the hourse  
      !read (septfile_num,*,iostat=eof) bz_area(ihru)
      real, dimension (:), allocatable :: bz_z, bz_thk,  bio_bd
      !bz_z(:)          |mm            |Depth of biozone layer
      !bz_thk(:)        |mm            |thickness of biozone
      !bio_bd(:)        |kg/m^3        |density of biomass
      
!! carbon outputs for .hru file
      real, dimension (:), allocatable :: cmup_kgh, cmtot_kgh
      !cmup_kgh       kg/ha    current soil carbon for first soil layer
      ! cmtot_kgh      kg/ha    current soil carbon integrated - aggregating       
!! carbon outputs for .hru file

      real, dimension (:), allocatable :: coeff_bod_dc, coeff_bod_conv
      !coeff_bod_dc(:)  |m^3/day       |BOD decay rate coefficient
      !coeff_bod_conv(:)|none          |BOD to live bacteria biomass conversion factor
      real, dimension (:), allocatable :: coeff_fc1, coeff_fc2
      !coeff_fc1(:)     |none          |field capacity calibration parameter 1
      !coeff_fc2(:)     |none          |field capacity calibration parameter 2
      real, dimension (:), allocatable :: coeff_fecal, coeff_plq
      !coeff_fecal(:)   |m^3/day       |Fecal coliform bacteria decay rate coefficient
      !coeff_plq(:)     |none          |Conversion factor for plaque from TDS
      real, dimension (:), allocatable :: coeff_mrt, coeff_rsp
      !coeff_mrt(:)     |none          |mortality rate coefficient
      !coeff_rsp(:)     |none          |respiration rate coefficient 
      real, dimension (:), allocatable :: coeff_slg1, coeff_slg2
      !coeff_slg1(:)    |none          |slough-off calibration parameter
      !coeff_slg2(:)    |none          |slough-off calibration parameter
      real, dimension (:), allocatable :: coeff_nitr, coeff_denitr
      !coeff_nitr(:)    |none          |Nitrification rate coefficient
      !coeff_denitr(:)  |none          |Denitrification rate coefficient
      real, dimension (:), allocatable :: coeff_pdistrb,coeff_solpslp 
      !read (septfile_num,*,iostat=eof) coeff_pdistrb(ihru)
      !coeff_solpslp ; not used
      real, dimension (:), allocatable :: coeff_solpintc,coeff_psorpmax
      !read (septfile_num,*,iostat=eof) coeff_solpintc(ihru)
      !read (septfile_num,*,iostat=eof) coeff_psorpmax(ihru)
!! Septic system by Jaehak Jeong
      integer, dimension (:), allocatable :: i_sep,isep_typ
      integer, dimension (:), allocatable :: isep_opt,sep_tsincefail
      integer, dimension (:), allocatable :: isep_tfail,isep_iyr
      integer, dimension (:), allocatable :: sep_strm_dist,sep_den
      
 !!   change per JGA 9/8/2011 gsm for output.mgt 
      real, dimension (:), allocatable :: sol_sumno3, sol_sumsolp
      !sol_sumno3(j) = sol_sumno3(j) + sol_no3(ly,j)
      !sol_sumsolp(j) = sol_sumsolp(j) + sol_solp(ly,j)
      real, dimension (:), allocatable :: strsw_sum, strstmp_sum
      !strsw_sum(j) = strsw_sum(j) + (1. - strsw(j))
      !strstmp_sum(j) = strstmp_sum(j) + (1. - strstmp(j))
      real, dimension (:), allocatable :: strsn_sum, strsp_sum
      !strsn_sum(j) = strsn_sum(j) + (1. - strsn(j))
      !strsp_sum(j) = strsp_sum(j) + (1. - strsp(j))
      real, dimension (:), allocatable :: strsa_sum
      !strsa_sum(j) = strsa_sum(j) + (1. - strsa(j))
      

!! New pothole variables
      real, dimension (:), allocatable :: spill_hru,tile_out,hru_in
      !spill_hru; not used.
      !tile_out(j) = tile_out(j) + tileo
      !hru_in; not used.
      real, dimension (:), allocatable :: spill_precip,pot_seep
      !spill_precip; not used.
      !pot_seep(j) = pot_seep(j) + potsep
      real, dimension (:), allocatable :: pot_evap,pot_sedin
      !pot_evap(j)= pot_evap(j) + potev
      !ot_sedin(j)= pot_sedin(j) + sedyld(j) * pot_fr(j)
      real, dimension (:), allocatable :: pot_solp,pot_solpi
      ! pot_solp(:)    |1/d           | soluble P loss rate in the pothole (.01 - 0.5)
      !pot_solpi(j) = pot_solpi(j) + surqsolp(j) * xx
      real, dimension (:), allocatable :: pot_orgp,pot_orgpi
      !pot_orgp(:)     |             |amount of organic P in pothole water body
      !pot_orgpi(j) = pot_orgpi(j) + sedorgp(j) * xx
      real, dimension (:), allocatable :: pot_orgn,pot_orgni
      !pot_orgn(:)     |kg N         |amount of organic N in pothole water body
      !pot_orgni(j) = pot_orgni(j) + sedorgn(j) * xx
      real, dimension (:), allocatable :: pot_mps,pot_mpsi
      !pot_mps(:)     |kg N          |amount of stable mineral pool P in pothole water body
      !pot_mpsi(j) = pot_mpsi(j) + sedminps(j) * xx
      real, dimension (:), allocatable :: pot_mpa,pot_mpai   
      !pot_mpa(:)     |kg N          |amount of active mineral pool P in pothole water body
      !pot_mpai, not used.
      real, dimension (:), allocatable :: pot_no3i,precip_in
      !pot_no3i(j) = pot_no3i(j) + no3in * xx
      !precip_in, not used
      real, dimension (:), allocatable :: tile_sedo,tile_no3o
      ! tile_sedo(j)= tile_sedo(j)+ sedloss
      !tile_no3o(j)= tile_no3o(j)+ no3loss
      real, dimension (:), allocatable :: tile_solpo,tile_orgno
      !tile_solpo(j)= tile_solpo(j)+ solploss
      !tile_orgno(j)= tile_orgno(j)+ orgnloss
      real, dimension (:), allocatable :: tile_orgpo,tile_minpso   
      !tile_orgpo(j)= tile_orgpo(j)+ orgploss
      !tile_minpso, not used.
      real, dimension (:), allocatable :: tile_minpao  
      !tile_minpao(j)= tile_minpao(j)+ minpaloss                
! output files 

!!  added for binary files 3/25/09 gsm
      integer :: ia_b, ihumus, itemp, isnow
      !ia_b  print ascii or binary files
      !ihumus = 1 (print formerly watqual.out - now output.wql)
      !read (file_cio2_num,*,iostat=eof) itemp
      !read (file_cio2_num,*,iostat=eof) isnow
      integer, dimension (:), allocatable :: icolb,icolr,icolrsv,icols
      !icolb(:)    |none          |space number for beginning of column in subbasin output file
      !icolr(:)    |none          |space number for beginning of column in reach output file
      !icolrsv(:)  |none          |space number for beginning of column in reservoir output file
      !icols(:)    |none          |space number for beginning of column in HRU output file
      integer, dimension (:), allocatable :: ipdvar,ipdvab,ipdvas,ipdhru
      !ipdvar(:)   |none          |output variable codes for .rch file
      !ipdvab(:)   |none          |output variable codes for output.sub file
      !ipdvas(:)   |none          |output variable codes for output.hru file
      !ipdhru(:)   |none        |HRUs whose output information will be printed to the output.hru and output.wtr files
      real, dimension (:), allocatable :: wshddayo,wshdmono,wshdyro
      !wshddayo(:) |varies        |watershed daily output array
      !wshdmono(:) |varies        |watershed monthly output array
      !wshdyro(:)  |varies        |watershed annual output array
      real, dimension (:), allocatable :: wshdaao,fcstaao
      !wshdaao(:)  |varies        |watershed average annual output array
      !fcstaao
      !
       !! ///added by Junyu Qi
      real, dimension (:), allocatable :: wshddayN,wshdmonN,wshdyrN
      real, dimension (:), allocatable :: wshdaaN
       !! ///added by Junyu Qi
       
       
      real, dimension (:,:), allocatable :: wpstdayo,wpstmono,wpstyro
      !wpstdayo(:,:)|varies        |watershed daily pesticide output array
      !wpstmono
      !wpstyro
      real, dimension (:,:), allocatable :: yldkg, bio_hv
      !yldkg(:,:,:) |kg/ha         |yield (dry weight) by crop type in the HRU
      !bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
      real, dimension (:,:), allocatable :: wpstaao,rchmono,rchyro
      !wpstaao(:,1)|mg pst/ha     |amount of pesticide type in surface runoff
      !rchmono(:,:)|varies        |reach monthly output array
      !rchyro
      real, dimension (:,:), allocatable :: rchaao,rchdy,hrumono,hruyro
      !rchaao
      !rchdy(:,:)  |varies        |daily reach output array
      !hrumono(:,:)|varies        |HRU monthly output data array
      !hruyro(:,:) |varies        |HRU annual output array
      
      real, dimension (:,:), allocatable :: hrumonN,hruyrN,hruaaN        
      !hrumonN
      !hruyrN
      !hruaaN 
      real, dimension (:,:), allocatable :: hruaao,submono,subyro,subaao
      !hruaao       |varies        |HRU average annual output array
      ! submono(:,:)|varies        |subbasin monthly output array
      !subyro(:,:) |varies        |subbasin annual output array
      !subaao       |varies        |subbasin average annual output array
      real, dimension (:,:), allocatable :: resoutm,resouty,resouta
      !resoutm(:,:)|varies        |reservoir monthly output array
      !resouty(:,:) |varies        |reservoir annual output array
      !resouta(:,:)|varies        |reservoir average annual output array
      real, dimension (:,:), allocatable :: wshd_aamon
      !wshd_aamon(:,:)|varies        |array of watershed monthly average values
      real, dimension (:,:), allocatable :: wtrmon,wtryr,wtraa 
      !wtrmon(:,:) |varies        |HRU monthly output data array for impoundments
      !wtryr(:,:)  |varies        |HRU impoundment annual output array
      !wtraa(:,:)   |varies        |HRU impoundment average annual output array
      real, dimension (:,:), allocatable :: sub_smfmx, sub_smfmn
      !sub_smfmx    |mm/deg C/day  |Maximum melt rate for snow during year (June |21) where deg C refers to the air temperature
!!                                |SMFMX and SMFMN allow the rate of snow melt to vary through the year. These parameters
!!                                |are accounting for the impact of soil temperature on snow melt.
      !sub_smfmn    |mm/deg C/day  |Minimum melt rate for snow during year (Dec. 21) where deg C refers to the air temperature
      real, dimension (:,:,:), allocatable :: hrupstd,hrupsta,hrupstm
      !hrupstd(:,:,:)|varies      |HRU daily pesticide output array
      !hrupsta(:,:,:)|varies      |HRU average annual pesticide output array
      !hrupstm(:,:,:)|varies      |HRU monthly pesticide output array
      real, dimension (:,:,:), allocatable :: hrupsty
      ! hrupsty(:,:,:)|varies      |HRU annual pesticide output array
      
! mrg = max number of rainfall/temperature gages
      integer, dimension (:), allocatable :: ifirstt,ifirstpcp
      ! ifirstt(:)  |none          |temperature data search code |0 first day of temperature data located in file
!!                               |1 first day of temperature data not located in file
      !ifirstpcp(:)|none          |precipitation data search code |0 first day of precipitation data located in file
!!                               |1 first day of precipitation data not located in file

      integer, dimension (:), allocatable :: elevp,elevt
      !elevp(:)    |m             |elevation of precipitation gage station
      !elevt(:)    |m             |elevation of temperature gage station
      
! mfcst = max number of forecast regions
      real, dimension (:,:), allocatable :: ftmpstdmn,ftmpmn,ftmpmx
      !ftmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
      !ftmpmn(:,:)  |deg C         |avg monthly minimum air temperature
      !ftmpmx(:,:)  |deg C         |avg monthly maximum air temperature
      real, dimension (:,:), allocatable :: ftmpstdmx
      ! ftmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
      real, dimension (:,:,:), allocatable :: fpr_w,fpcp_stat
      !fpr_w(1,:,:) |none          |probability of wet day after dry day in month, allocate (fpr_w(3,12,msub)), readfcst.f90
      !fpcp_stat, allocate (fpcp_stat(12,3,msub)), fpcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
! mch = max number of channels
      real, dimension (:), allocatable :: flwin,flwout,bankst,ch_wi,ch_d
      !flwin(:)    |m^3 H2O       |flow into reach on previous day
      !flwout(:)   |m^3 H2O       |flow out of reach on previous day
      !bankst(:)   |m^3 H2O       |bank storage
      !ch_wi(irch) = ch_w(2,irch)
      !ch_d(:)       |m           |average depth of main channel
      real, dimension (:), allocatable :: ch_onco, ch_opco
      !ch_onco(:)       |ppm           |channel organic n concentration
      !ch_opco(:)       |ppm           |channel organic p concentration
      real, dimension (:), allocatable :: ch_orgn, ch_orgp
      !ch_orgn(jrch) = deg2 * ch_onco(jrch) / 1000.
      !ch_orgp(jrch) = deg2 * ch_opco(jrch) * 1000.
      real, dimension (:), allocatable :: drift,rch_DOX,rch_bactp
      !drift(:)     |kg               |amount of pesticide drifting onto main channel in subbasin
      !rch_DOX(:)       |mg O2/L       |dissolved oxygen concentration in reach
      !rch_bactp(:)     |# cfu/100ml |persistent bacteria stored in reach
      real, dimension (:), allocatable :: alpha_bnk,alpha_bnke
      !alpha_bnk(:)  |days        |alpha factor for bank storage recession curve
      !alpha_bnke(:) |none        |Exp(-alpha_bnk(:))
      real, dimension (:), allocatable :: rch_SolP,rch_Alg,sedst,rchstor
      !rch_SolP(:)  |mg P/L        |dissolved phosphorus concentration in reach
      !rch_Alg(:)         |mg alg/L      |algal biomass concentration in reach
      ! sedst(:)    |metric tons   |amount of sediment stored in reach
      !rchstor(:)  |m^3 H2O       |water stored in reach
      real, dimension (:), allocatable :: rch_OrgN,rch_OrgP,rch_Chla
      !rch_OrgN(:) |mg N/L        |organic nitrogen concentration in reach
      !rch_OrgP(:) |mg P/L        |organic phosphorus concentration in reach
      !rch_Chla(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
      real, dimension (:), allocatable :: rch_NO3,rch_NO2,ch_li,ch_si
      !rch_NO3(:) |mg N/L        |nitrate concentration in reach
      !rch_NO2(:) |mg N/L        |nitrite concentration in reach
      !ch_li(:)    |km            |initial length of main channel
      !ch_si(:)      |m/m         |initial slope of main channel

!      real, dimension (:), allocatable :: ch_cov,ch_di,ch_erod,ch_l2
!      real, dimension (:), allocatable :: ch_san, ch_sil, ch_cla, ch_veg 
!      real, dimension (:), allocatable :: ch_rcur, ch_ss, ch_fpr, ch_eqn
!      real, dimension (:), allocatable :: ch_crht

!     Sediment parameters added by Balaji for the new routines

      real, dimension (:), allocatable :: ch_bnk_san, ch_bnk_sil
      !ch_bnk_san
      !ch_bnk_sil
      real, dimension (:), allocatable :: ch_bnk_cla, ch_bnk_gra
      !ch_bnk_cla
      !ch_bnk_gra
      real, dimension (:), allocatable :: ch_bed_san, ch_bed_sil
      !ch_bed_san
      !ch_bed_sil
      real, dimension (:), allocatable :: ch_bed_cla, ch_bed_gra
      !ch_bed_cla
      !ch_bed_gra
      real, dimension (:), allocatable :: depfp,depsanfp,depsilfp
      !depfp
      !depsanfp, not used.
      !depsilfp
      real, dimension (:), allocatable :: depclafp,depsagfp,deplagfp
      !depclafp
      !depsagfp, not used.
      !deplagfp, not used
      real, dimension (:), allocatable :: depch,depsanch,depsilch
      !depch
      !depsanch
      !depsilch
      real, dimension (:), allocatable :: depclach,depsagch,deplagch
      !depclach
      !depsagch
      !deplagch
      real, dimension (:), allocatable :: depgrach,depgrafp,grast
      !depgrach
      !depgrafp, not used.
      !grast
      real, dimension (:), allocatable :: depprch,depprfp,prf,r2adj
      !depprch
      !depprfp
      !prf(:)      |none          |Reach peak rate adjustment factor for sediment
      !!    r2adj       |dimensionless |curve number retention parameter adjustment factor to 
!!                                 adjust surface runoff for flat slopes (0.5 - 3.0)
      !!    r2adj       |none          |retention parameter adjustment factor (greater than 1)
      real, dimension (:), allocatable :: spcon, spexp
      !spcon       |none          |linear parameter for calculating sediment
      !spexp       |none          |exponent parameter for calculating sediment
      real, dimension (:), allocatable :: sanst,silst,clast,sagst,lagst
      !sanst(jrch) = sanin
      !silst(jrch) = silin
      !clast(jrch) = clain
      !sagst(jrch) = sagin
      !lagst(jrch) = lagin
      real, dimension (:), allocatable :: pot_san,pot_sil,pot_cla
      !pot_san(j) = pot_san(j) + sanyld(j) * pot_fr(j) 
      !pot_sil(j) = pot_sil(j) + silyld(j) * pot_fr(j) 
      !pot_cla(j) = pot_cla(j) + clayld(j) * pot_fr(j)
      real, dimension (:), allocatable :: pot_sag,pot_lag
      !pot_sag(j) = pot_sag(j) + sagyld(j) * pot_fr(j)
      !pot_lag(j) = pot_lag(j) + lagyld(j) * pot_fr(j)
      real, dimension (:), allocatable :: potsani,potsili,potclai
      !potsani(j) = pot_san(j) 
      !potsili(j) = pot_sil(j)
      !potclai(j) = pot_cla(j)
      real, dimension (:), allocatable :: potsagi,potlagi
      !potsagi(j) = pot_sag(j)
      !potlagi(j) = pot_lag(j) 
      real, dimension (:), allocatable :: sanyld,silyld,clayld,sagyld
      !sanyld
      !silyld
      !clayld
      !sagyld
      real, dimension (:), allocatable :: lagyld,grayld
      !lagyld(j) = lagyld(j) * (1. - trapeff(j))
      !grayld, not used.
      real, dimension (:), allocatable :: res_san,res_sil,res_cla
      !res_san
      !res_sil
      !res_cla
      real, dimension (:), allocatable :: res_sag,res_lag,res_gra
      !res_sag
      !res_lag
      !res_gra
      real, dimension (:), allocatable :: pnd_san,pnd_sil,pnd_cla
      !pnd_san
      !pnd_sil
      !pnd_cla
      real, dimension (:), allocatable :: pnd_sag,pnd_lag
      !pnd_sag
      !pnd_lag
      real, dimension (:), allocatable :: wet_san,wet_sil,wet_cla
      !wet_san
      !wet_sil
      !wet_cla
      real, dimension (:), allocatable :: wet_lag, wet_sag
      !wet_lag
      !wet_sag
      real :: ressano,ressilo,resclao,ressago,reslago, resgrao
      !ressano = res_san(jres) * resflwo
      !ressilo = res_sil(jres) * resflwo
      !resclao
      !ressago
      !reslago
      !resgrao
      real :: ressani, ressili, resclai, ressagi, reslagi,resgrai
      !res_san(jres) = (ressani + san * vol) / res_vol(jres)
      !res_sil(jres) = (ressili + sil * vol) / res_vol(jres)
      !resclai
      !ressagi
      !reslagi,
      !resgrai
      real :: potsano,potsilo,potclao,potsago,potlago
      !potsano
      !potsilo
      !potclao
      !potsago
      !potlago
	  real :: pndsanin,pndsilin,pndclain,pndsagin,pndlagin
	  !pndsanin = sanyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
	  !pndsilin
	  !pndclain
	  !pndsagin
	  !pndlagin
	  real :: pndsano,pndsilo,pndclao,pndsago,pndlago
	  !pndsano = pnd_san(k) * pndflwo
	  !pndsilo
	  !pndclao
	  !pndsago
	  !pndlago

      real, dimension (:), allocatable :: ch_di,ch_erod,ch_l2, ch_cov
      !ch_di(:)      |m           |initial depth of main channel
      !ch_erod(:)    |none        |channel erodibility factor (0.0-1.0)
      !ch_l2(:)    |km            |main channel length in subbasin
      !ch_cov
      real, dimension (:), allocatable :: ch_cov1, ch_cov2, ch_bnk_bd
      !ch_cov1(:)    |none        |channel erodibility factor (0.0-1.0)
      !ch_cov2(:)    |none        |channel cover factor (0.0-1.0)
      !ch_bnk_bd(:)  |(g/cc)      |bulk density of channel bank sediment (1.1-1.9)
      real, dimension (:), allocatable :: ch_bed_bd,ch_bnk_kd,ch_bed_kd
      !ch_bed_bd(:)  |(g/cc)      |bulk density of channel bed sediment (1.1-1.9)
      !ch_bnk_kd(:)  |            |erodibility of channel bank sediment by jet test
      !ch_bed_kd(:)  |            |erodibility of channel bed sediment by jet test
      real, dimension (:), allocatable :: ch_bnk_d50, ch_bed_d50     
      !ch_bnk_d50(:) |            |D50(median) particle size diameter of channel
      !ch_bed_d50(:) |            |D50(median) particle size diameter of channel
      real, dimension (:), allocatable :: tc_bed,tc_bnk
      !tc_bed        |N/m2        |critical shear stress of channel bed 
      !tc_bnk        |N/m2        |critical shear stress of channel bank
      integer, dimension (:), allocatable :: ch_eqn                  
      !ch_eqn        |            |sediment routine methods (DAILY): | 0 = original SWAT method
!!                               | 1 = Bagnold's
!!                               | 2 = Kodatie
!!                               | 3 = Molinas WU
!!                               | 4 = Yang      
      real, dimension (:), allocatable :: chpst_conc,chpst_rea,chpst_vol
      !chpst_conc(:) |mg/(m**3)     |initial pesticide concentration in reach
      !chpst_rea(:)  |1/day       |pesticide reaction coefficient in reach
      !chpst_vol(:)  |m/day       |pesticide volatilization coefficient in reach
      real, dimension (:), allocatable :: chpst_koc,chpst_stl,chpst_rsp
      !chpst_koc(:)  |m**3/g      |pesticide partition coefficient between water and sediment in reach
      ! chpst_stl(:)  |m/day       |settling velocity in reach for pesticide
      !chpst_rsp(:)  |m/day       |resuspension velocity in reach for pesticide
      real, dimension (:), allocatable :: chpst_mix,sedpst_conc,ch_wdr
      !chpst_mix(:)  |m/day       |mixing velocity (diffusion/dispersion) for pesticide in reach
      !sedpst_conc(:)|mg/(m**3)   |inital pesticide concentration in river bed
      !ch_wdr(:)   |m/m           |channel width to depth ratio
      real, dimension (:), allocatable :: sedpst_rea,sedpst_bry
      !sedpst_rea(:) |1/day       |pesticide reaction coefficient in river bed
      !sedpst_bry(:) |m/day       |pesticide burial velocity in river bed
      real, dimension (:), allocatable :: sedpst_act,rch_CBOD,rch_bactlp
      !sedpst_act(:)|m            |depth of active sediment layer in reach for pesticide
      !rch_CBOD(:) |mg O2/L       |carbonaceous biochemical oxygen demand in reach
      !rch_bactlp(:)|# cfu/100ml  |less persistent bacteria in reach/outflow
      real, dimension (:), allocatable :: chside,rs1,rs2,rs3,rs4,rs5
      !chside(:)     |            |change in horizontal distance per unit vertical distance (0.0 - 5)
!!                               |0 = for vertical channel bank
!!                               |5 = for channel bank with gentl side slope
      !rs1(:)      |m/day or m/hr |local algal settling rate in reach at 20 deg C
      !rs2(:)      |(mg disP-P)/  |benthos source rate for dissolved phosphorus ((m**2)*day)|in reach at 20 deg C or (mg disP-P)/((m**2)*hr)|
      !rs3(:)      |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in((m**2)*day)|reach at 20 deg C or (mg NH4-N)/((m**2)*hr)|
      !rs4(:)      |1/day or 1/hr |rate coefficient for organic nitrogen settling in reach at 20 deg C
      !rs5(:)      |1/day or 1/hr |organic phosphorus settling rate in reach at 20 deg C

      real, dimension (:), allocatable :: rs6,rs7,rk1,rk2,rk3,rk4,rk5
      !rs6(:)      |1/day         |rate coefficient for settling of arbitrary non-conservative constituent in reach
      !rs7(:)      |(mg ANC)/     |benthal source rate for arbitrary ((m**2)*day)|non-conservative constituent in reach
      !rk1(:)      |1/day or 1/hr |CBOD deoxygenation rate coefficient in reach at 20 deg C
      !rk2(:)      |1/day or 1/hr |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
      !rk3(:)      |1/day or 1/hr |rate of loss of CBOD due to settling in reach at 20 deg C
      !rk4(:)      |mg O2/        |sediment oxygen demand rate in reach ((m**2)*day)|at 20 deg C or mg O2/((m**2)*hr)
      !rk5(:)      |1/day         |coliform die-off rate in reach
      !rk6(:)      |1/day         |decay rate for arbitrary non-conservative constituent in reach  
      
      
      real, dimension (:), allocatable :: rk6,bc1,bc2,bc3,bc4,rch_NH4
      !bc1(:)           |1/hr          |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
      !bc2(:)           |1/hr          |rate constant for biological oxidation of NO2 to NO3 in reach at 20 deg C
      !bc3(:)           |1/hr          |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
      !bc4(:)           |1/hr          |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
      !rch_NH4(:) |mg N/L        |ammonia concentration in reach
      real, dimension (:), allocatable :: orig_sedpstconc
      !orig_sedpstconc, storeinitial.f90 and rewind_init.f90
      real, dimension (:,:), allocatable :: wurch
      !wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach
      integer, dimension (:), allocatable :: icanal
      !read (rtefile_num,*,iostat=eof) icanal(irch)
      integer, dimension (:), allocatable :: itb
      !itb
! msub = max number of subbasins
      real, dimension (:), allocatable :: ch_revap, dep_chan
      !ch_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from bank storage to the root
!!                               |zone as a result of soil moisture depletion
      !dep_chan(:)      |m              |average daily water depth in channel
      real, dimension (:), allocatable :: harg_petco, subfr_nowtr
      !harg_petco(:)              |coefficient related to radiation used in hargreaves eq (range: 0.0019 - 0.0032)
      real, dimension (:), allocatable :: cncoef_sub, dr_sub
      !cncoef_sub  |              |soil water depletion coefficient used | in the new (modified curve number method)
!!                               | same as soil index coeff used in APEX range: 0.5 - 2.0
      !sedyld(j) = sedyld(j) * dr_sub(j)
      real, dimension (:), allocatable :: wcklsp,sub_fr,sub_minp,sub_sw
      !wcklsp(sb) = wcklsp(sb) + cklsp(j) * hru_fr(j); allocate (wcklsp(msub))
      !sub_fr(:)   |km2/km2       |fraction of total watershed area contained
      !sub_minp(:) |kg P/ha       |amount of phosphorus stored in all mineral pools sorbed to sediment
      !sub_sw(:)    |mm H2O        |amount of water in soil profile in subbasin
      real, dimension (:), allocatable :: sub_sumfc,sub_gwno3,sub_gwsolp
      !sub_sumfc(:) |mm H2O        |amount of water in subbasin soil at field capacity
      !sub_gwno3(:)|kg N/ha       |nitrate loading in groundwater from subbasin
      !sub_gwsolp(sb) = sub_gwsolp(sb) + minpgw(j) * hru_fr(j)
      real, dimension (:), allocatable :: sub_km,sub_tc,wlat,sub_pet,co2
      !sub_km(:)   |km^2          |area of subbasin in square kilometers
      !sub_tc(:)   |hr            |time of concentration for subbasin
      !wlat(:)     |degrees       |latitude of weather station used to compile data
      !sub_pet(:)  |mm H2O        |potential evapotranspiration for day in subbasin
      !co2(:)     |ppmv           |CO2 concentration
      real, dimension (:), allocatable :: welev,sub_orgn,sub_orgp,sub_bd
      !welev(:)    |m             |elevation of weather station used to compile weather generator data
      !sub_orgn(:)   |kg N/ha      |amount of nitrogen stored in all organic pools 
      !sub_orgp(:)   |kg P/ha      |amount of phosphorus stored in all organic pools 
      !sub_bd(:)     |Mg/m^3       |bulk density in subbasin first soil layer
      real, dimension (:), allocatable :: sub_wtmp,sub_sedpa,sub_sedps
      !sub_wtmp(sb)             |C, temporary water temperature
      !sub_sedpa(:)  |kg P/ha       |amount of active mineral P attached to sediment removed in surface runoff on day in subbasin
      !sub_sedps(:)  |kg P/ha       |amount of stable mineral P attached to sediment removed in surface runoff on day  in subbasin
      real, dimension (:), allocatable :: sub_minpa,sub_minps,daylmn
      !sub_minpa(:)  |kg P/ha      |amount of phosphorus stored in active mineral pools sorbed to sediment
      !sub_minps(:)  |kg P/ha      |amount of phosphorus stored in stable mineral pools sorbed to sediment
      !daylmn(:)      |hours         |shortest daylength occurring during the year
      real, dimension (:), allocatable :: latcos,latsin,phutot
      !latcos(:)   |none          |Cos(Latitude) for HRU
      !latsin(:)   |none          |Sin(Latitude) for HRU
      !phutot(:)   |heat units    |total potential heat units for year (used when no crop is growing)
      real, dimension (:), allocatable :: tlaps,plaps,tmp_an,sub_precip
      !tlaps(:)    |deg C/km      |temperature lapse rate: temperature increase due to increase in elevation
      !plaps(:)    |mm H2O/km     |precipitation lapse rate: precipitation increase due to increase in elevation
      !tmp_an(:)   |deg C         |average annual air temperature
      ! sub_precip(:)|mm H2O      |amount of water reaching soil surface in subbasin
      real, dimension (:), allocatable :: pcpdays, rcn_sub, rammo_sub
      !rcn_sub_bsn |mg/kg         |Concentration of nitrogen in the rainfall
      !pcpdays(i) = pcpdays(i) + pcpd(mon)
      !rammo_sub   |mg/l          |atmospheric deposition of ammonium values for entire watershed
      real, dimension (:), allocatable :: atmo_day
      !atmo_day, not used.
      real, dimension (:), allocatable :: sub_snom,sub_qd,sub_sedy
      !sub_snom(:) |mm H2O      |amount of snow melt in subbasin on day
      !sub_qd(:)   |mm H2O        |surface runoff loading from subbasin for day
      !sub_sedy(:) |metric tons   |sediment loading on day from subbasin
      real, dimension (:), allocatable :: sub_tran,sub_no3,sub_latno3
      !sub_tran(:)  |mm H2O        |transmission losses on day in subbasin
      !sub_no3(:)  |kg N/ha       |NO3 in surface runoff on day in subbasin
      ! sub_latno3(:)|kg N/ha       |NO3 in lateral flow on day in subbasin
      real, dimension (:,:), allocatable :: sub_smtmp,sub_timp,sub_sftmp
      !sub_smtmp    |deg C         |Snow melt base temperature
      !sub_timp     |none          |snow pack temperature lag factor (0-1)
      !sub_sftmp    |deg C         |Snowfall temperature
      real, dimension (:), allocatable :: sub_tileno3, sub_tilep        !!R683  1/13/22 nbs     
      !sub_tileno3 |kg N/ha       |NO3 in tile flow on day in subbasin
      real, dimension (:), allocatable :: sub_solp,sub_subp,sub_etday
      !sub_solp(:) |kg P/ha       |soluble P in surface runoff on day in subbasin
      !sub_subp(:)  |mm H2O        |precipitation for day in subbasin
      !sub_etday(:)|mm H2O        |actual evapotranspiration on day in subbasin
      real, dimension (:), allocatable :: sub_wyld,sub_surfq,sub_elev
      !sub_wyld(:) |mm H2O        |water yield on day in subbasin
      !sub_surfq(:)|nn H2O        |surface runoff generated on day in subbasin
      !sub_elev(:) |m             |average elevation of subbasin
      real, dimension (:), allocatable :: qird
      !qird(jj) = volmm * sq_rto
      real, dimension (:), allocatable :: sub_gwq,sub_sep,sub_chl
      !sub_gwq(:)  |mm H2O        |groundwater loading on day in subbasin
      !sub_sep(:)  |mm H2O        |percolation out of soil profile on day in subbasin
      !sub_chl(:)  |kg chl-a      |chlorophyll-a loading on day from subbasin
      real, dimension (:), allocatable :: sub_cbod,sub_dox,sub_solpst  !!!! carbonaceous biological oxygen demand on day;kg O2
      !sub_cbod(:) |kg cbod       |carbonaceous biological oxygen demand loading
      !sub_dox(:)  |kg O2         |dissolved oxygen loading on day from subbasin
      !sub_solpst(:)|mg pst        |soluble pesticide loading on day in subbasin
      real, dimension (:), allocatable :: sub_sorpst,sub_yorgn,sub_yorgp
      !sub_sorpst(:)|mg pst        |sorbed pesticide loading on day in subbasin
      ! sub_yorgn(:)|kg N/ha       |organic N loading on day in subbasin
      !sub_yorgp(:)|kg P/ha       |organic P loading on day in subbasin
      real, dimension (:), allocatable :: sub_bactp,sub_bactlp,sub_lat
      !sub_bactp(:)|# bact/ha     |persistent bacteria loading on day from subbasin
      !sub_bactlp(:)|# bact/ha     |less persistent bacteria loading on day from subbasin
      !sub_lat(:)  |degrees       |latitude of subbasin
      real, dimension (:), allocatable :: sub_latq, sub_gwq_d,sub_tileq
      !sub_latq
      !sub_gwq_d
      !sub_tileq
      real, dimension (:), allocatable :: sub_vaptile
      !sub_vaptile(sb) = sub_vaptile(sb) + vap_tile * hru_fr(j) !!vap         |kg P/ha       |amount of P leached from soil layer
      real, dimension (:), allocatable :: sub_dsan, sub_dsil, sub_dcla
      !sub_dsan(sb) = sub_dsan(sb) + sanyld(j)
      !sub_dsil(sb) = sub_dsil(sb) + silyld(j)
      !sub_dcla(sb) = sub_dcla(sb) + clayld(j)
      real, dimension (:), allocatable :: sub_dsag, sub_dlag
      !sub_dsag(sb) = sub_dsag(sb) + sagyld(j)
      !sub_dlag(sb) = sub_dlag(sb) + lagyld(j)
      
!!!!!! drains
      real :: vap_tile
      !pdvas(24) = vap_tile           !!  kg P/ha !vap         |kg P/ha       |amount of P leached from soil layer
      real, dimension (:), allocatable :: wnan
      ! allocate (wnan(mlyr)); wnan(j1) = sol_z(j1,j) - y1  
      real, dimension (:,:), allocatable :: sol_stpwt
      !sol_stpwt(j1,j) = sol_st(j1,j)
      real, dimension (:,:), allocatable :: sub_pst,sub_hhqd,sub_hhwtmp
      !sub_pst(:,:)  |kg/ha        |amount of pesticide in layer in subbasin; allocate (sub_pst(mpst,msub))
      !llocate (sub_hhqd(msub,nstep_rch)); sub_hhqd(sb,1:nstep) = sub_hhqd(sb,1:nstep) / sub_fr(sb)
      !sub_hhwtmp(:,:) |deg C     |water temperature for the time step in subbasin
      real, dimension (:,:), allocatable :: rfinc,tmpinc,radinc,huminc
      !rfinc(:,:)  |%             |monthly rainfall adjustment. Daily rainfall within the month is adjusted to the specified
!!                               |percentage of the original value (used in climate change studies)
      ! tmpinc(:,:) |deg C         |monthly temperature adjustment. Daily maximum and minimum temperatures within the month are
!!                               |raised or lowered by the specified amount (used in climate change studies)
      !radinc(:,:) |MJ/m^2        |monthly solar radiation adjustment. Daily radiation within the month is raised or
!!                               |lowered by the specified amount. (used in climate change studies)
      !huminc(:,:) |none          |monthly humidity adjustment. Daily values for relative humidity within the month are
!!                               |raised or lowered by the specified amount. (used in climate change studies)
      real, dimension (:,:), allocatable :: wndav,ch_k,elevb,elevb_fr
      !wndav(:,:) |m/s            |average wind speed for the month
      !ch_k(2,:)     |mm/hr       |effective hydraulic conductivity of main channel alluvium
      !ch_k(1,:)   |mm/hr         |effective hydraulic conductivity of tributary channel alluvium 
      !elevb(:,:)  |m             |elevation at center of band
      !elevb_fr(:,:)|none         |fraction of subbasin area within elevation band
      real, dimension (:,:), allocatable :: dewpt,ch_w,ch_s,ch_n
      !dewpt(:,:)  |deg C         |average dew point temperature for the month
      !ch_w(2,:)   |m             |average width of main channel
      !ch_w(1,:)   |m             |average width of tributary channels
      !ch_s(1,:)   |m/m           |average slope of tributary channels
      !ch_s(2,:)     |m/m         |average slope of main channel
      !ch_n(1,:)   |none          |Manning's "n" value for the tributary channels
      !ch_n(2,:)     |none        |Manning's "n" value for the main channel
      real, dimension (:,:), allocatable :: amp_r,solarav,tmpstdmx
      !amp_r(:,:)  |none        |alpha factor for rain(mo max 0.5h rain)
      !solarav(:,:)|MJ/m^2/day    |average daily solar radiation for the month
      !tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
      real, dimension (:,:), allocatable :: tmpstdmn,pcf,tmpmn,tmpmx
      !tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
      !pcf(:,:)    |none          |normalization coefficient for precipitation
      !tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
      !tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
      real, dimension (:,:), allocatable :: otmpstdmn,otmpmn,otmpmx !!rewinding
      !otmpstdmn
      !otmpmn
      !otmpmx
      real, dimension (:,:), allocatable :: otmpstdmx, ch_erodmo
      !otmpstdmx
      !ch_erodmo(irch,mo) = ch_cov1(irch)
      real, dimension (:,:), allocatable :: uh, hqdsave, hsdsave
      !allocate (uh(msub,49))
      !hqdsave(sb,ii) = hqd(ii+nstep)  ! save flow after midnight for next day J.Jeong 4/17/2009
      !hsdsave(sb,ii) = hsd(ii+nstep)  ! sediment. J.Jeong 4/22/2009
      real, dimension (:,:,:), allocatable :: pr_w,pcp_stat
      !pr_w(1,:,:) |none          |probability of wet day after dry day in month
      !pr_w(2,:,:) |none          |probability of wet day after wet day in month
      !pcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
      !pcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
      !pcp_stat(:,3,:)|none       |skew coefficient for the average daily
      real, dimension (:,:,:), allocatable :: opr_w,opcp_stat
      !opr_w, rewinding
      !opcp_stat, rewinding
      integer, dimension (:), allocatable :: hrutot,hru1,ireg
      !hrutot(:)   |none          |number of HRUs in subbasin
      !hru1 is the first HRU in a subbasin
      !ireg(:)     |none          |precipitation category: |  1 precipitation <= 508 mm/yr |2 precipitation > 508 and <= 1016 mm/yr |3 precipitation > 1016 mm/yr
      integer, dimension (:), allocatable :: isgage,ihgage,iwgage
      !isgage(:)    |none         |subbasin radiation gage data code (record # for solar radiation used in HRU)
      !ihgage(:)    |none         |subbasin relative humidity data code
      !iwgage(:)    |none         |subbasin wind speed gage data code
      integer, dimension (:), allocatable :: irgage,itgage,subgis 
      ! irgage(:)   |none          |HRU rain gage data code (gage # for rainfall data used in HRU)
      !itgage(:)   |none          |HRU temperature gage data code (gage # for temperature data used in HRU)
      !subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
      integer, dimension (:), allocatable :: fcst_reg, irelh
      !fcst_reg
      !irelh       |none          |irelh = 0 (dewpoint)  = 1 (relative humidity)
!!                               |note:  inputs > 1.0 (dewpoint)
!!                               |       inputs < 1.0 (relative hum)
! mlyr = max number of soil layers
      real, dimension (:,:), allocatable :: sol_aorgn,sol_tmp,sol_fon
      !sol_aorgn(:,:)|kg N/ha     |amount of nitrogen stored in the active organic (humic) nitrogen pool in soil layer
      !sol_tmp(2,:)  |deg C         |daily average temperature of second soil layer
      !sol_tmp(:,:)  |deg C         |daily average temperature of soil layer
      !sol_fon(:,:)     |kg N/ha       |amount of nitrogen stored in the fresh organic (residue) pool
      real, dimension (:,:), allocatable :: sol_awc,sol_prk,volcr
      !sol_awc(:,:)  |mm H20/mm soil|available water capacity of soil layer
      !sol_prk(:,:)|mm H2O        |percolation from soil layer on current day
      !volcr(:,:)  |mm            |crack volume for soil layer
      real, dimension (:,:), allocatable :: pperco_sub
      !pperco_sub
      real, dimension (:,:), allocatable :: sol_actp,sol_stap,conv_wt
      !soL_actp(:,:) |kg P/ha       |amount of phosphorus stored in the active mineraL phosphorus pooL
      !soL_stap(:,:) |kg P/ha       |amount of phosphorus in the soiL Layer stored in the stabLe mineraL phosphorus pooL
      !conv_wt(:,:)  |none          |factor which converts kg/kg soil to kg/ha
      real, dimension (:,:), allocatable :: sol_solp,sol_ul,sol_fc,crdep
      !sol_solp(:,:)|kg P/ha      |amount of phosohorus stored in solution
      !sol_ul(:,:)      |mm H2O        |amount of water held in the soil layer at saturation (sat - wp water)
      !sol_fc(:,:)      |mm H2O        |amount of water available to plants in soil layer at field capacity (fc - wp),Index:(layer,HRU)
      !crdep(:,:)  |mm            |maximum or potential crack volume
      real, dimension (:,:), allocatable :: sol_z,sol_up,sol_bd,sol_st
      !sol_z(:,:)       |mm            |depth to bottom of soil layer,Index:(layer,HRU)
      !sol_up(:,:)   |mm H2O/mm soil|water content of soil at -0.033 MPa (field capacity)
      !sol_bd(:,:) |Mg/m**3       |bulk density of the soil
      !sol_st
      real, dimension (:,:), allocatable :: sol_thick
      !sol_thick(j,ihru) = sol_z(j,ihru) - sol_z(j-1,ihru)
      real, dimension (:,:), allocatable :: flat,sol_nh4,sol_hk,sol_clay
      ! flat(:,:)   |mm H2O        |lateral flow in soil layer on current day
      !sol_nh4(:,:)     |kg N/ha       |amount of nitrogen stored in the ammonium pool in soil layer
      !sol_hk(:,:)   |none          |beta coefficent to calculate hydraulic conductivity
      !sol_clay(1,:)|% 		   |percent clay content in top soil layer in HRU
      real, dimension (:,:), allocatable :: sol_nh3 
      !soL_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!  added 1/27/09 when making septic changes
      real, dimension (:,:), allocatable :: sol_ec 
      !sol_ec, not used.
!  added 1/27/09 when making septic changes
      real, dimension (:,:), allocatable :: sol_orgn,sol_por,sol_wp
      !sol_orgn(:,:)    |kg N/ha       |amount of nitrogen stored in the stable organic N pooL
      !sol_por(:,:)     |none          |total porosity of soil layer expressed as a fraction of the total volume, Index:(layer,HRU)
      !sol_wp(:,:)   |mm H20/mm soil|water content of soil at -1.5 MPa (wilting point)
      real, dimension (:,:), allocatable :: sol_orgp,sol_hum,sol_wpmm
      !sol_orgp(:,:)|kg P/ha      |amount of phosphorus stored in the organic P pool in soil layer
      !sol_hum(:,:)  |kg humus/ha   |amount of organic matter in the soil layer classified as humic substances
      !sol_wpmm(:,:) |mm H20        |water content of soil at -1.5 MPa (wilting point)
      real, dimension (:,:), allocatable :: sol_k,sol_cbn,sol_no3
      !sol_k(:,:)       |mm/hr         |saturated hydraulic conductivity of soil layer. Index:(layer,HRU)
      !sol_cbn(1,:)|%             |percent organic carbon in top soil layer in HRU
      !sol_no3(:,:)     |kg N/ha       |amount of nitrogen stored in the nitrate pool in soil layer
      real, dimension (:,:), allocatable :: sol_urea   
      !sol_urea, not used.
      real, dimension (:,:), allocatable :: sol_rsd,sol_fop
      !sol_rsd(1,:)|kg/ha         |amount of organic matter in the top soil layer
      !sol_fop(:,:)     |kg P/ha       |amount of phosphorus stored in the fresh organic (residue) pool
      real, dimension (:,:), allocatable :: sol_silt, sol_sand, sol_rock
      !sol_silt(:,:)|%             |percent silt content in soil material
      !sol_sand(:,:) |none          |fraction of sand in soil material
      !sol_rock(:,:) |%            |percent of rock fragments in soil layer
      real, dimension (:,:), allocatable :: orig_solno3,orig_solorgn        !rewinding
      real, dimension (:,:), allocatable :: orig_solsolp,orig_solorgp       !rewinding
      real, dimension (:,:), allocatable :: orig_soltmp,orig_solrsd         !rewinding        
      real, dimension (:,:), allocatable :: orig_solfop,orig_solfon         !rewinding
      real, dimension (:,:), allocatable :: orig_solaorgn,orig_solst        !rewinding
      real, dimension (:,:), allocatable :: orig_solactp,orig_solstap       !rewinding
      real, dimension (:,:), allocatable :: orig_volcr                      !rewinding
!    Drainmod tile equations  01/2006 
	  real, dimension (:,:), allocatable :: conk
	  !conk(:,:)   |mm/hr         |lateral saturated hydraulic conductivity for each profile layer in a give HRU. For example (conk(2,1) is conductivity 
!!                               |of layer from sol_z(1,1) to sol_z(2,1) in HRU1
!    Drainmod tile equations  01/2006
      real, dimension (:,:,:), allocatable :: sol_pst,sol_kp
      !sol_pst(:,:,1)|kg/ha           |pesticide in first layer of soil
      !sol_kp(:,:,:)|(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the or m^3/ton|ratio of the concentration in the solid
!!                                |phase to the concentration in solution
      real, dimension (:,:,:), allocatable :: orig_solpst           !rewinding
! mres = max number of reservoirs
      real, dimension (:), allocatable :: velsetlr, velsetlp
      !velsetlr(i) = 24. * 411. * res_d50mm ** 2.
      !velsetlp(ihru) = velsetlpnd
      real, dimension (:), allocatable :: br1,res_k,lkpst_conc, evrsv
      !br1(:)       |none          |1st shape parameter for reservoir surface area equation
      !res_k(:)     |mm/hr         |hydraulic conductivity of the reservoir bottom
      !lkpst_conc(:) |mg/m^3        |pesticide concentration in lake water
      !evrsv(:)     |none          |lake evaporation coefficient
      real, dimension (:), allocatable :: res_evol,res_pvol,res_vol
      !res_evol(:)  |m**3          |volume of water needed to fill the reservoir
      !res_pvol(:)  |m**3          |volume of water needed to fill the reservoir
      !res_vol(:)  |m^3 H2O       |reservoir volume
      real, dimension (:), allocatable :: res_psa,lkpst_rea,lkpst_vol
      !res_psa(:)   |ha            |reservoir surface area when reservoir is filled to principal spillway
      !lkpst_rea(:)  |1/day         |pesticide reaction coefficient in lake water
      !lkpst_vol(:)  |m/day         |pesticide volatilization coefficient in lake
      real, dimension (:), allocatable :: br2,res_rr,res_sed,lkpst_koc
      !br2(:)       |none          |2nd shape parameter for reservoir surface area equation
      !res_rr(:)    |m**3/day      |average daily principal spillway release volume (read in as a release rate in m^3/s and converted to m^3/day)
      !res_sed(:)  |kg/L (ton/m^3)|amount of sediment in reservoir
      !lkpst_koc(:)  |m**3/g        |pesticide partition coefficient between water and sediment in lake water
      real, dimension (:), allocatable :: lkpst_stl,lkpst_rsp,lkpst_mix
      !lkpst_stl(:)  |m/day         |settling velocity in lake water for pesticide sorbed to sediment
      !lkpst_rsp(:)  |m/day         |resuspension velocity in lake water for pesticide sorbed to sediment
      !lkpst_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) in lake water for pesticide
      real, dimension (:), allocatable :: lkspst_conc,lkspst_rea
      !lkspst_conc(:)|mg/m^3        |pesticide concentration in lake bed sediment
      !lkspst_rea(:) |1/day         |pesticide reaction coefficient in lake bed
      real, dimension (:), allocatable :: theta_n, theta_p, con_nirr
      !read (lwqfile_num,*,iostat=eof) theta_n(i)
      !theta_p
      !read (lwqfile_num,*,iostat=eof) con_nirr(i)
      real, dimension (:), allocatable :: con_pirr
      !read (lwqfile_num,*,iostat=eof) con_pirr(i)
      real, dimension (:), allocatable :: lkspst_bry,lkspst_act,sed_stlr
      !lkspst_bry(:) |m/day         |pesticide burial velocity in lake bed
      !lkspst_act(:) |m             |depth of active sediment layer in lake for for pesticide
      !sed_stlr(i) = Exp(-.184 * res_d50)
      real, dimension (:), allocatable :: wurtnf,res_nsed,resdata,chlar
      !wurtnf(:)    |none          |fraction of water removed from the reservoir via WURESN which is returned and becomes flow
!!                                |from the reservoir outlet
      !res_nsed(:)  |kg/L          |normal amount of sediment in reservoir (read in as mg/L and convert to kg/L)
      !resdata
      !chlar(:)      |none          |chlorophyll-a production coefficient for reservoir
      real, dimension (:), allocatable :: res_orgn,res_orgp,res_no3
      ! res_orgn(:)   |kg N          |amount of organic N in reservoir
      !res_orgp(:)   |kg P          |amount of organic P in reservoir
      !res_no3(:)    |kg N          |amount of nitrate in reservoir
      real, dimension (:), allocatable :: res_solp,res_chla,res_seci
      !res_solp(:)   |kg P          |amount of soluble P in reservoir
      !res_chla(:) |kg chl-a      |amount of chlorophyll-a in reservoir
      !res_seci(:) |m             |secchi-disk depth
      real, dimension (:), allocatable :: res_esa,seccir,res_no2,res_nh3
      !res_esa(:)   |ha            |reservoir surface area when reservoir is filled to emergency spillway 
      !seccir(:)     |none          |water clarity coefficient for reservoir
      !res_no2(:)    |kg N          |amount of nitrite in reservoir
      !res_nh3(:)    |kg N          |amount of ammonia in reservoir
      real, dimension (:), allocatable :: res_bactp, res_bactlp
      !res_bactp(:)     |# cfu/100ml |persistent bacteria stored in reservoir
      ! res_bactlp(:)    |# cfu/100ml |less persistent bacteria stored in reservoir
      real, dimension (:), allocatable :: oflowmn_fps, starg_fps
      ! oflowmn_fps  |fraction      |minimum reservoir outflow as a fraction of the principal spillway volume (0-1)
!!                                |m^3/s and converted to m^3/day)
      !starg_fps    |fraction      |target volume as a fraction of the principal spillway volume (.1-5)
      real, dimension (:), allocatable :: weirc, weirk, weirw
      !readres.f90(219):        read (resfile_num,*,iostat=eof) weirc(i)
      !readres.f90(221):        read (resfile_num,*,iostat=eof) weirk(i)
      !readres.f90(223):        read (resfile_num,*,iostat=eof) weirw(i)
      real, dimension (:), allocatable :: acoef, bcoef, ccoef
      !readres.f90(225):        read (resfile_num,*,iostat=eof) acoef(i)
      !readres.f90(227):        read (resfile_num,*,iostat=eof) bcoef(i)
      !readres.f90(229):        read (resfile_num,*,iostat=eof) ccoef(i)
      real, dimension (:), allocatable :: orig_resvol,orig_ressed               !rewinding
      real, dimension (:), allocatable :: orig_lkpstconc,orig_lkspstconc        !!rewinding
      real, dimension (:), allocatable :: orig_ressolp,orig_resorgp             !!rewinding
      real, dimension (:), allocatable :: orig_resno3,orig_resno2               !rewinding
      real, dimension (:), allocatable :: orig_resnh3,orig_resorgn              !rewinding
      real, dimension (:,:), allocatable :: starg,oflowmx,oflowmn
      !starg_fps    |fraction      |target volume as a fraction of the principal spillway volume (.1-5)
      !oflowmx(:,:) |m^3/day       |maximum daily ouflow for the month (read in as
      !oflowmn(:,:) |m^3/day       |minimum daily ouflow for the month (read in as m^3/s and converted to m^3/day)
      real, dimension (:,:), allocatable :: psetlr,nsetlr,wuresn
      !psetlr(1,:)   |m/day         |phosphorus settling rate for mid-year period (read in as m/year and converted to m/day)
      !psetlr(2,:)   |m/day         |phosphorus settling rate for remainder of year (read in as m/year and converted to m/day)
      !nsetlr(1,:)   |m/day         |nitrogen settling rate for mid-year period (read in as m/year and converted to m/day)
      !nsetlr(2,:)   |m/day         |nitrogen settling rate for remainder of year (read in as m/year and converted to m/day)
      !wuresn(:,:)  |m**3          |average amount of water withdrawn from reservoir each month for consumptive water use (read in as 10^4 m^3 and converted to m^3)
      real, dimension (:,:,:), allocatable :: res_out
      ! allocate (res_out(mres,12,myr)) res_out(:,:,:) |m**3/day      |measured average daily outflow from the reservoir for the month (needed if IRESCO=1)
!!                                  |(read in as m**3/s and converted to m**3/day)
      integer, dimension (:), allocatable :: ires1,ires2,res_sub
      !ires1(:)      |none          |beginning of mid-year nutrient settling "season"
      ! ires2(:)      |none          |end of mid-year nutrient settling "season"
      integer, dimension (:), allocatable :: iresco,mores,iyres
      ! iresco(:)    |none          |outflow simulation code: |0 compute outflow for uncontrolled reservoir with average annual release rate
!!                                |1 measured monthly outflow
!!                                |2 simulated controlled outflow-target release
!!                                |3 measured daily outflow
      !mores(:)     |none          |month the reservoir becomes operational
      !iyres(:)     |none          |year of the simulation that the reservoir becomes operational
      integer, dimension (:), allocatable :: iflod1r,iflod2r,ndtargr
      !iflod1r(:)   |none          |beginning month of non-flood season (needed if IRESCO=2)
      !iflod2r(:)   |none          |ending month of non-flood season (needed if IRESCO=2)
      !ndtargr(:)   |days          |number of days to reach target storage from current reservoir storage (needed if IRESCO=2)
! mpdb = max number of pesticides in the database
      real, dimension (:), allocatable :: skoc,ap_ef,decay_f
      !skoc(:)     |(mg/kg)/(mg/L)|soil adsorption coefficient normalized for soil organic carbon content
      !ap_ef(:)     |none             |application efficiency (0-1)
      !decay_f(:)    |none          |exponential of the rate constant for degradation of the pesticide on foliage
      real, dimension (:), allocatable :: hlife_f,hlife_s,decay_s
      ! hlife_f(:)  |days          |half-life of pesticide on foliage
      !hlife_s(:)  |days          |half-life of pesticide in soil
      !decay_s(:)    |none          |exponential of the rate constant for degradation of the pesticide in soil
      real, dimension (:), allocatable :: pst_wsol,pst_wof, irramt
      !pst_wsol(:)  |mg/L (ppm)    |solubility of chemical in water
      !pst_wof(:)  |none          |fraction of pesticide on foliage which is washed-off by a rainfall event
      !irramt(:)       |mm H2O        |depth of irrigation water applied to HRU
      real, dimension (:), allocatable :: phusw, phusw_nocrop
      !phusw(ihru) = husc
      !phusw_nocrop(ihru) = husc
      integer, dimension (:), allocatable :: nope, pstflg, nop
      !nope(:)      |none             |sequence number of pesticide in NPNO(:)
      !pstflg(:)   |none        |flag for types of pesticide used in watershed
      !nop
      integer, dimension (:), allocatable :: yr_skip, isweep
      !yr_skip
      !isweep(:,:,:)|julian date    |date of street sweeping operation
      integer, dimension (:), allocatable :: icrmx, nopmx
      !icrmx
      !nopmx(ihru) = nopmx(ihru) + 1
! new management scehduling variables
      integer, dimension (:,:), allocatable :: mgtop, idop
      !mgtop
      !idop
      integer, dimension (:,:), allocatable :: mgt1iop,mgt2iop,mgt3iop
      !mgt1iop(iop,ihru) = mgt1i
      !mgt2iop(iop,ihru) = mgt2i
      !mgt3iop(iop,ihru) = mgt3i
      real, dimension (:,:), allocatable ::  mgt4op, mgt5op, mgt6op
      !mgt4op(iop,ihru) = mgt4
      !mgt5op(iop,ihru) = mgt5
      !mgt6op(iop,ihru) = mgt6
      real, dimension (:,:), allocatable :: mgt7op, mgt8op, mgt9op
      !mgt7op(iop,ihru) = mgt7
      !mgt8op(iop,ihru) = mgt8
      !mgt9op(iop,ihru) = mgt9
      real, dimension (:,:), allocatable :: mgt10iop, phu_op
      !mgt10iop(iop,ihru) = mgt10i
      !allocate (phu_op(iopera,mhru))
! mcrdb = maximum number of crops in database
      real, dimension (:), allocatable :: wac21,wac22,cnyld,rsdco_pl
      real, dimension (:), allocatable :: wsyf,leaf1,leaf2,alai_min
      real, dimension (:), allocatable :: t_base,t_opt,hvsti,bio_e
      real, dimension (:), allocatable :: vpd2,gsi,chtmx,wavp,cvm
      real, dimension (:), allocatable :: blai,dlai,rdmx,cpyld,bio_leaf
      real, dimension (:), allocatable :: bio_n1,bio_n2,bio_p1,bio_p2
      real, dimension (:), allocatable :: bmx_trees,ext_coef,bm_dieoff
      real, dimension (:), allocatable :: rsr1, rsr2                    
!     real, dimension (:), allocatable :: air_str
      real, dimension (:,:), allocatable :: pltnfr,pltpfr
      integer, dimension (:), allocatable :: idc, mat_yrs
! mfdb = maximum number of fertilizer in database
      real, dimension (:), allocatable :: forgn,forgp,fminn,bactpdb
      real, dimension (:), allocatable :: fminp,fnh3n,bactlpdb,bactkddb
      character(len=8), dimension (200) :: fertnm
! mudb = maximum number of land types in urban database
      real, dimension (:), allocatable :: fimp,curbden,urbcoef,dirtmx
      real, dimension (:), allocatable :: thalf,tnconc,tpconc,tno3conc
      real, dimension (:), allocatable :: fcimp,urbcn2
! mapp = max number of applications
      real :: sweepeff,frt_kg, pst_dep
!! added pst_dep to statement below 3/31/08 gsm
!!   burn 3/5/09       
! mnr = max number years of rotation
!!   burn 3/5/09  
! mtil = max number tillages in database
 !! drainmod tile equations   06/2006

      real, dimension (:), allocatable :: ranrns_hru
      !ranrns_hru(:)|mm           |random roughness for a given HRU
	  integer, dimension (:), allocatable :: itill
	  !tillmix.f90(147):      if(itill(jj) == 1) then
!! drainmod tile equations   06/2006
      real, dimension (:), allocatable :: effmix,deptil, ranrns
      character(len=8), dimension (550) :: tillnm
! mhyd = max number of hydrograph nodes
      real, dimension (:), allocatable :: rnum1s,hyd_dakm
      real, dimension (:,:), allocatable :: varoute,shyd, vartran
      real, dimension (:,:,:), allocatable :: hhvaroute
      integer, dimension (:), allocatable :: icodes,ihouts,inum1s
      integer, dimension (:), allocatable :: inum2s,inum3s,inum4s
      integer, dimension (:), allocatable :: inum5s,inum6s,inum7s,inum8s
      integer, dimension (:), allocatable :: subed
      character(len=10), dimension (:), allocatable :: recmonps
      !recmonps
      character(len=10), dimension (:), allocatable :: reccnstps
      !reccnstps
      character(len=5), dimension (:), allocatable :: subnum
      !main.f90(144):        write (subnum(i),fmt=' (i5.5)') hru_sub(i)
      character(len=4), dimension (:), allocatable :: hruno
      !hruno

! mhru = maximum number of hydrologic response units
      real, dimension (:), allocatable :: grwat_n, grwat_i, grwat_l
      !grwat_n(:)      |none          |Mannings's n for grassed waterway
      !grwat_i(:)      |none          |On/off Flag for waterway simulation
      !grwat_l(:)      |km	           |Length of Grass Waterway
      real, dimension (:), allocatable :: grwat_w, grwat_d
      !grwat_w(:)      |none          |Width of grass waterway
      !grwat_d(:)      |m             |Depth of Grassed waterway
      real, dimension (:), allocatable :: grwat_s, grwat_spcon
      !grwat_s(:)      |m/m           |Slope of grass waterway
      !grwat_spcon(:)  |none          |sediment transport coefficant defined by user
      real, dimension (:), allocatable :: tc_gwat
      !tc_gwat(:)      |none          |Time of concentration for Grassed waterway and its drainage area
      real, dimension (:), allocatable ::pot_volmm,pot_tilemm,pot_volxmm  !!NUBZ
      !readhru-Qi.f90(160):        read (hrufile_num,*,iostat=eof) pot_volmm(ihru
      !readhru-Qi.f90(156):        read (hrufile_num,*,iostat=eof) pot_tilemm(ihru)
      !pot_volxmm , not used.
      real, dimension (:), allocatable :: pot_fr,pot_tile,pot_vol,potsa
      ! pot_fr(:)      |km2/km2       |fraction of HRU area that drains into pothole
      !pot_tile(ihru) = pot_tilemm(ihru)
      !pot_vol, not used
      !potsa(:)      |ha            |surface area of impounded water body
      real, dimension (:), allocatable :: pot_volx,potflwi,potsedi,wfsh
      !pot_volx, not used.
      !potflwi(:)    |m^3 H2O       |water entering pothole on day
      !potsedi(:)    |metric tons   |sediment entering pothole on day
      !wfsh(:)       |mm            |wetting front matric potential
      real, dimension (:), allocatable :: pot_nsed,pot_no3l,newrti,gwno3
      !pot_nsed(:)    |mg/L          |normal sediment concentration in impounded
      !pot_no3l(:)    |1/day         |nitrate decay rate in impounded area
      !newrti(:)   |mm/hr         |infiltration rate for last time step from the previous day
      !gwno3(:)    |mg N/L        |nitrate-N concentration in groundwater
      real, dimension (:), allocatable :: pot_sed,pot_no3,fsred,tmpavp
      !pot_sed(:)     |metric tons   |amount of sediment in pothole water body
      ! pot_no3l(:)    |1/day         |nitrate decay rate in impounded area
      !fsred(:)    |none          |reduction in bacteria loading from filter
      !tmpavp(j) = tmpav(j)
      real, dimension (:), allocatable :: evpot, dis_stream, pot_solpl
      !evpot(:)    |none          |pothole evaporation coefficient
      !dis_stream(:) | m          |average distance to stream
      !read (hrufile_num,*,iostat=eof) pot_solpl(ihru)
      real, dimension (:), allocatable :: sed_con, orgn_con, orgp_con
      !read (hrufile_num,*,iostat=eof) sed_con(ihru)
      ! read (hrufile_num,*,iostat=eof) orgn_con(ihru)
      !read (hrufile_num,*,iostat=eof) orgp_con(ihru)
      real, dimension (:), allocatable :: soln_con, solp_con, pot_k
      !read (hrufile_num,*,iostat=eof) soln_con(ihru)
      !read (hrufile_num,*,iostat=eof) solp_con(ihru)
      !pot_k       |(mm/hr)       |hydraulic conductivity of soil surface of pothole [defaults to condcutivity of upper soil (0.01--10.) layer]
      real, dimension (:), allocatable :: n_reduc, n_lag, n_ln, n_lnco 
      !n_reduc     |              |nitrogen uptake reduction factor (not currently used; defaulted 300.)
      !n_lag       |dimensionless |lag coefficient for calculating nitrate concentration in subsurface
      !n_ln        |dimensionless |power function exponent for calculating nitrate concentration in subsurface drains (1.0 - 3.0)
      !n_lnco      |dimensionless |coefficient for power function for calculating nitrate concentration in subsurface drains (0.5 - 4.0)
      integer, dimension (:), allocatable :: ioper
      !ioper
      integer, dimension (:), allocatable :: ngrwat
      !schedule_ops.f90(122):           ngrwat(ihru) = ngrwat(ihru) + 1
      real, dimension (:), allocatable :: filterw,sumix,usle_ls,phuacc
      !filterw(:)  |m             |filter strip width for bacteria transport
      !sumix(:)      |none          |sum of mixing efficiencies in HRU
      !usle_ls(:)  |none          |USLE equation length slope (LS) factor
      !phuacc(:)   |none          |fraction of plant heat units accumulated
      real, dimension (:), allocatable :: esco,epco,slsubbsn,hru_slp
      !esco(:)      |none          |soil evaporation compensation factor
      !epco(:)     |none          |plant water uptake compensation factor (0-1)
      !slsubbsn(:) |m             |average slope length for subbasin
      !ru_slp(:)	|m/m	       |average slope steepness in HRU
      real, dimension (:), allocatable :: erorgn,erorgp,biomix,pnd_seci
      !erorgn(:)     |none         |organic N enrichment ratio, if left blank
      !erorgp(:)     |none         |organic P enrichment ratio, if left blank
      !biomix(:)  |none             |biological mixing efficiency.
      !pnd_seci(:)   |m             |secchi-disk depth of pond
      real, dimension (:), allocatable :: flowmin,divmax,canmx,usle_p
      !flowmin(:)      |m**3/s        |minimum instream flow for irrigation diversions when IRR=1, irrigation water
!!                                   |will be diverted only when streamflow is at or above FLOWMIN.
      !divmax(:)   |mm H2O or     |maximum daily irrigation diversion from the 10**4 m H2O|reach (when IRR=3): when value is positive,
!!                               |the units are mm H2O; when the value is negative, the units are (10**4 m H2O)
      ! canmxl      |mm H2O        |maximum canopy storage at current day's leaf
      !usle_p(:)       |none           |USLE equation support practice (P) factor
      real, dimension (:), allocatable :: lat_sed,rch_dakm,pnd_no3s,cn1
      !lat_sed(:)  |g/L           |sediment concentration in lateral flow
      !rch_dakm(:)  |km**2        |total drainage area contributing to flow at the outlet (pour point) of the reach in square kilometers
      !pnd_no3s(:) |kg N          |amount of nitrate originating from lateral flow in pond at beginning of day
      !cn1(:)      |none          |SCS runoff curve number for moisture
      real, dimension (:), allocatable :: cn2,lat_ttime,flowfr,sol_zmx
      !cn2(:)          |none           |SCS runoff curve number for moisture
      !lat_ttime(:)|none          |Exponential of the lateral flow travel time
      !flowfr(:)       |none          |fraction of available flow in reach that is allowed to be applied to the HRU
      !sol_zmx(:)    |mm            |maximum rooting depth
      real, dimension (:), allocatable :: tile_ttime
      !tile_ttime(:)|none          |Exponential of the tile flow travel time
      real, dimension (:), allocatable :: slsoil,sed_stl,gwminp,sol_cov
      !slsoil(:)   |m             |slope length for lateral subsurface flow
      !sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in impoundment after settling for one day
      !gwminp(:)   |mg P/L        |soluble P concentration in groundwater loading to reach
      !sol_cov(:)  |kg/ha         |amount of residue on soil surface
      real, dimension (:), allocatable :: yldanu,pnd_solp,pnd_no3,ov_n
      !ldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
      !pnd_solp(:) |kg P          |amount of soluble P originating from surface
      !pnd_no3s(:) |kg N          |amount of nitrate originating from lateral flow in pond at beginning of day
      !ov_n(:)     |none          |Manning's "n" value for overland flow
      real, dimension (:), allocatable :: driftco,pnd_orgp,pnd_orgn,cn3
      !driftco(:)   |none             |coefficient for pesticide drift directly onto stream
      !pnd_orgp(:) |kg P          |amount of organic P originating from surface runoff in pond at beginning of day
      !pnd_orgn(:) |kg N          |amount of organic N originating from surface runoff in pond at beginning of day
      !cn3(:)      |none          |SCS runoff curve number for moisture
      real, dimension (:), allocatable :: sol_sumul,pnd_chla,hru_fr
      !sol_sumul(:)|mm H2O        |amount of water held in soil profile at saturation
      !pnd_chla(:)   |kg chl_a      |amount of chlorophyll-a in pond at end of day
      !hru_fr(:)   |km2/km2       |fraction of subbasin area contained in HRU
      real, dimension (:), allocatable :: bio_ms,sol_alb,strsw,hru_km
      !bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
      !sol_alb(:)  |none          |albedo when soil is moist
      !strsw(:)       |none          |fraction of potential plant growth achieved on the day where the reduction is caused by water stress
      !hru_km(:)    |km**2            |area of HRU in square kilometers
      real, dimension (:), allocatable :: pnd_fr,pnd_psa,pnd_pvol,pnd_k
      !pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains !allocate (pnd_fr(mhru))
      !pnd_psa(:)  |ha            |surface area of ponds when filled to principal spillway
      !pnd_pvol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to fill the ponds to the principal spillway 
      !pnd_k(:)    |mm/hr         |hydraulic conductivity through bottom of ponds
      real, dimension (:), allocatable :: pnd_esa,pnd_evol,pnd_vol,yldaa
      !pnd_esa(:)  |ha            |surface area of ponds when filled to the emergency spillway
      !pnd_evol(:) |m^3 H2O       |runoff volume from catchment area needed to fill the ponds to the emergency spillway (UNIT CHANGE!)
      !pnd_vol(:)  |m^3 H2O       |volume of water in pond
      !yldaa(:)      |metric tons/ha|average annual yield (dry weight) in HRU
      real, dimension (:), allocatable :: pnd_sed,pnd_nsed,strsa,dep_imp
      !pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
      !pnd_nsed(:) |mg/kg         |normal sediment concentration in pond water
      !strsa                        aeration stress
      !dep_imp(:)	|mm            |depth to impervious layer
      real, dimension (:), allocatable :: evpnd, evwet
      !evpnd       |none          |pond evaporation coefficient
      !wetev = 10. * evwet(j) * pet_day * wetsa
      real, dimension (:), allocatable :: wet_fr,wet_nsa,wet_nvol,wet_k
      !wet_fr(:)   |none          |fraction of HRU/subbasin area that drains into wetlands
      !wet_nsa(:)  |ha            |surface area of wetlands at normal water level
      !wet_nvol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to fill wetlands to normal water level
      !wet_k(:)    |mm/hr         |hydraulic conductivity of bottom of wetlands
      integer, dimension (:), allocatable :: iwetgw, iwetile
      !readhru-Qi.f90(238):        read (hrufile_num,*,iostat=eof) iwetgw(ihru)
      !readhru-Qi.f90(236):        read (hrufile_num,*,iostat=eof) iwetile(ihru
      real, dimension (:), allocatable :: wet_mxsa,wet_mxvol,wet_vol
      !wet_mxsa(:) |ha            |surface area of wetlands at maximum water level
      !wet_mxvol(:)|10^4 m^3 H2O  |runoff volume from catchment area needed to fill wetlands to maximum water level
      !wet_vol(:)  |m^3 H2O       |volume of water in wetlands (UNIT CHANGE!)
      real, dimension (:), allocatable :: wet_sed,wet_nsed
      !wet_sed(:)  |mg/L          |sediment concentration in wetland water
      !wet_nsed(:) |mg/kg         |normal sediment concentration in wetland water
      real, dimension (:), allocatable :: smx,sci,bp1,bp2
      !smx(:)      |none          |retention coefficient for cn method based on soil moisture
      !sci(:)      |none          |retention coefficient for cn method based on plant ET
      !bp1(:)      |none          |1st shape parameter for pond surface area equation
      !bp2(:)      |none          |2nd shape parameter for the pond surface area equation
      real, dimension (:), allocatable :: bw1,bw2,bactpq
      !bw1(:)      |none          |1st shape parameter for wetland surface area equation
      !bw2(:)      |none          |2nd shape parameter for the wetland surface area equation
      !bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
      real, dimension (:), allocatable :: bactp_plt,bactlp_plt,cnday
      !bactp_plt(:)|# cfu/m^2     |persistent bacteria on foliage
      !bactlp_plt(:)|# cfu/m^2    |less persistent bacteria on foliage
      !cnday(:)    |none          |curve number for current day, HRU and at current soil moisture
      real, dimension (:), allocatable :: bactlpq,auto_eff,sol_sw,secciw
      !bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
      !auto_eff(:) |none           |fertilizer application efficiency calculated as the amount of N applied divided by the
!!                                |amount of N removed at harvest
      !sol_sw(:)      |mm H2O        |amount of water stored in soil profile on any given day
      !secciw(:)   |none          |water clarity coefficient for wetland
      real, dimension (:), allocatable :: bactps,bactlps,tmpav,chlaw
      !bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
      !bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
      !tmpav(:)    |deg C         |average air temperature on current day in HRU
      !chlaw(:)    |none          |chlorophyll-a production coefficient for wetland
      real, dimension (:), allocatable :: subp,sno_hru,hru_ra,wet_orgn
      !subp(:)     |mm H2O        |precipitation for the day in HRU
      !sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
      !hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
      !wet_orgn(:)   |kg N          |amount of organic N originating from surface runoff in wetland at end of day
      real, dimension (:), allocatable :: tmx,tmn,rsdin,tmp_hi,tmp_lo
      !tmx(:)      |deg C         |maximum temperature for the day in HRU   !(tmx(mhru))
      !tmn(:)      |deg C         |minimum temperature for the day in HRU
      !rsdin(:)    |kg/ha         |initial residue cover
      !tmp_hi(:)   |deg C         |last maximum temperature in HRU
      !tmp_lo(:)   |deg C         |last minimum temperature in HRU
      real, dimension (:), allocatable :: rwt,olai,usle_k,tconc,hru_rmx
      !rwt(:)      |none          |fraction of total plant biomass that is in roots
      !olai
      !usle_k(:)   |              |USLE equation soil erodibility (K) factor
      !tconc(:)    |hr          |time of concentration
      !hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
      real, dimension (:), allocatable :: usle_cfac,usle_eifac
      ! usle_cfac(ihru) = c
      !usle_eifac(j) = usle_ei
      real, dimension (:), allocatable :: anano3,aird,t_ov,sol_sumfc
      !anano3(:)   |kg N/ha       |total amount of nitrogen applied during the year in auto-fertilization
      !aird(:)        |mm H2O        |amount of water applied to HRU on current day
      !t_ov(:)     |hr            |time for flow from farthest point in subbasin
      !sol_sumfc(:)   |mm H2O        |amount of water held in the soil profile
      real, dimension (:), allocatable :: sol_avpor,usle_mult,wet_orgp
      !sol_avpor(:)  |none          |average porosity for entire soil profile
      !usle_mult(:)|none          |product of USLE K,P,LS,exp(rock)
      ! wet_orgp(:)   |kg P          |amount of organic P originating from surface runoff in wetland at end of day
      real, dimension (:), allocatable :: aairr,cht,u10,rhd
      !aairr(:)    |mm H2O        |average annual amount of irrigation water applied to HRU
      !cht(:)     |m              |canopy height
      !u10(:)      |m/s           |wind speed for the day in HRU
      !rhd(:)      |none          |relative humidity for the day in HRU
      real, dimension (:), allocatable :: shallirr,deepirr,lai_aamx
      !shallirr(:) |mm H2O        |amount of water removed from shallow aquifer
      !deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
      !lai_aamx(:)   |none          |average annual maximum leaf area index in HRU
      real, dimension (:), allocatable :: canstor,ovrlnd,ch_l1,wet_no3
      !canstor(:)  |mm H2O        |amount of water held in canopy storage
      !ovrlnd(:)   |mm H2O      |overland flow onto HRU from upstream
      !ch_l1(:)    |km            |longest tributary channel length in subbasin
      !et_no3(:)    |kg N          |amount of nitrate originating from surface runoff in wetland at end of day
      real, dimension (:), allocatable :: irr_mx, auto_wstr
      !irr_mx     |mm               |maximum irrigation amount per auto application
      !auto_wstr(:)   |none or mm    |water stress threshold that triggers irrigation
      real, dimension (:), allocatable :: cfrt_id, cfrt_kg, cpst_id
      !cfrt_id(:)   |none          |manure (fertilizer) identification
      !cfrt_kg(:)   |(kg/ha)/day   |dry weight of fertilizer/manure deposited
      !cpst_id(j) = mgt1iop(nop(j),j)
      real, dimension (:), allocatable :: cpst_kg
      !cpst_kg(j) = mgt4op(nop(j),j)
      real, dimension (:), allocatable :: irr_asq, irr_eff
      !irr_asq    |                 |surface runoff ratio
      !irr_eff(j) = mgt5op(nop(j),j)
      real, dimension (:), allocatable :: irrsq, irrefm, irrsalt
      !irrsq(ihru) = mgt7op(nop(j),j)
      !irrefm(ihru) = mgt6op(nop(j),j)
      !irrsalt(:)  |mg/kg         |concentration of salt in irrigation water
      real, dimension (:), allocatable :: bio_eat, bio_trmp             !!NUBZ
      !bio_trmp(:)  |(kg/ha)/day   |dry weight of biomass removed by trampling daily
      !bio_eat(:)   |(kg/ha)/day   |dry weight of biomass removed by grazing
      integer, dimension (:), allocatable :: ifrt_freq,ipst_freq,irr_noa
      !ifrt_freq(:)|days          |number of days between applications in continuous fertlizer operation
      integer, dimension (:), allocatable :: irr_sc,irr_no
      !irr_sc
      !irr_no
      integer, dimension (:), allocatable :: imp_trig, fert_days,irr_sca
      !imp_trig(:,:,:)|none          |release/impound action code: 0 begin impounding water |1 release impounded water
      !fert_days(:) |none          |number of days continuous fertilization
      !irr_sca(j) = mgt2iop(nop(j),j)
      integer, dimension (:), allocatable :: pest_days, idplt, wstrs_id
      !pest_days(j) = mgt2iop(nop(j),j)
      !idplt(:,:,:)|none          |land cover code from crop.dat
      !wstrs_id(:)    |none          |water stress identifier: |1 plant water demand |2 soil water deficit
      real, dimension (:,:), allocatable :: bio_aahv
      !bio_aahv(:,:,:)|kg/ha         |harvested biomass of plant
!    Drainmod tile equations  08/2006 
	  real, dimension (:), allocatable :: cumei, cumeira
	  !cumei(:)	|Mj*mm/ha*hr   |cumulative USLE rainfall erosion index since last tillage operation
	  !cumeira(j) = cumeira(j) + ei
	  real, dimension (:), allocatable :: cumrt, cumrai
	  !cumrt(:)	|mm H2O		   |cumulative rainfall since last tillage operation
	  !cumrai(j) = cumrai(j) + precipday
!    Drainmod tile equations  08/2006
      real, dimension (:), allocatable :: wet_solp,wet_no3s,wet_chla
      !wet_solp(:)   |kg P          |amount of soluble P originating from surface runoff in wetland at end of day
      !wet_no3s(:)   |kg N          |amount of nitrate originating from lateral flow in wetland at end of day
      !wet_chla(:)   |kg chla       |amount of chlorophyll-a in wetland at end of day
      real, dimension (:), allocatable :: wet_seci,pnd_no3g,pstsol
      !wet_seci(:)   |m             |secchi-disk depth in wetland at end of day
      ! pnd_no3g(:) |kg N          |amount of nitrate originating from groundwater in pond at beginning of day
      !pstsol(:)    |kg/ha         |amount of pesticide type leached from soil profile on current day
      real, dimension (:), allocatable :: gwht,delay,gw_q,pnd_solpg
      !gwht(:)     |m             |groundwater height
      !delay(:)    |days          |groundwater delay: time required for water leaving the bottom of the root zone to reach the shallow aquifer
      !gw_q(:)     |mm H2O        |groundwater contribution to streamflow from HRU on current day
      !pnd_solpg(:)|kg P          |amount of soluble P originating from groundwater in pond at beginning of day
      real, dimension (:), allocatable :: alpha_bf,alpha_bfe,gw_spyld
      !alpha_bf(:) |1/days        |alpha factor for groundwater recession curve
      !alpha_bfe(:)|none          |Exp(-alpha_bf(:))
      !gw_spyld(:) |m**3/m**3     |specific yield for shallow aquifer
      real, dimension (:), allocatable :: alpha_bf_d,alpha_bfe_d
      !alpha_bf_d(:) | 1/days     |alpha factor for groudwater recession curve of the deep aquifer
      !alpha_bfe_d (:) |1/days    |Exp(-alpha_bf_d(:)) for deep aquifer
      real, dimension (:), allocatable :: gw_qdeep
      !gw_qdeep(:) |mm H2O        |groundwater contribution to streamflow from deep aquifer from HRU on current day
      real, dimension (:), allocatable :: gw_delaye,gw_revap,rchrg_dp
      !gw_delaye(:)|none          |Exp(-1./(delay(:)) where delay(:) is the groundwater delay (time required for water
!!                               |leaving the bottom of the root zone to reach the shallow aquifer; units-days)
      !gw_revap(:) |none          |revap coeff: this variable controls the amount of water moving from the shallow aquifer to
!!                               |the root zone as a result of soil moisture depletion
      !rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of root zone percolation that reaches the deep aquifer
      real, dimension (:), allocatable :: revapmn,anion_excl,rchrg
      !revapmn(:)  |mm H2O        |threshold depth of water in shallow aquifer
      !anion_excl(:)|none         |fraction of porosity from which anions are excluded
      !rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of root zone percolation that reaches the deep aquifer
      real, dimension (:), allocatable :: ffc,bio_min,surqsolp
      !ffc(:)      |none          |initial HRU soil water content
      !bio_min(:)   |kg/ha         |minimum plant biomass for grazing
      !surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface runoff in HRU for the day
      real, dimension (:), allocatable :: cklsp,deepst,shallst,wet_solpg
      !cklsp(j) = usle_cfac(j) * usle_mult(j)
      !deepst(:)   |mm H2O        |depth of water in deep aquifer
      !shallst(:)  |mm H2O        |depth of water in shallow aquifer
      !wet_solpg(:)  |kg P          |amount of soluble P originating from groundwater in wetland at end of day
      real, dimension (:), allocatable :: rchrg_src
      !watuse.f90(58):        rchrg_src(j) = -1. * wushal(i_mo,j) * 10000. / cnv !wushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow
      real, dimension (:), allocatable :: wet_no3g,sol_avbd,trapeff
      !wet_no3g(:)   |kg N          |amount of nitrate originating from groundwater in wetland at end of day
      !sol_avbd(:)   |Mg/m^3        |average bulk density for soil profile
      ! trapeff(:)  |none          |filter strip trapping efficiency (used for everything but bacteria)
      real, dimension (:), allocatable :: gwqmn,tdrain,pplnt,snotmp
      ! gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer required before groundwater flow will occur
      !tdrain(:)    |hrs           |time to drain soil to field capacity
      !pplnt(:)      |kg P/ha       |plant uptake of phosphorus in HRU for the day
      !snotmp(:)    |deg C         |temperature of snow pack in HRU
      real, dimension (:), allocatable :: ddrain,gdrain,sol_crk,dayl,brt
      !ddrain(:)   |mm            |depth of drain tube from the soil surface
      !gdrain(:)   |hrs           |drain tile lag time: the amount of time
      !sol_crk(:)    |none          |crack volume potential of soil
      !dayl(:)     |hours         |day length
      !brt(:)      |none          |fraction of surface runoff within the subbasin
!    Drainmod tile equations  01/2006 
	real, dimension (:), allocatable ::ddrain_hru,re,sdrain,sstmaxd
	!ddrain_hru
	!re(:)		|mm			   |effective radius of drains
	!sdrain(:)   |mm            |distance between two drain tubes or tiles
	!sstmaxd(:)  |mm            |static maximum depressional storage; read from .sdr
	real, dimension (:), allocatable :: stmaxd,drain_co,pc,latksatf
	!stmaxd(:)	|mm			   |maximum surface depressional storage for day in a given HRU 
	!drain_co(:) |mm/day        |drainage coefficient 
	! pc(:)       |mm/hr         |pump capacity (default pump capacity = 1.042mm/hr or 25mm/day)
	!latksatf(:) |none          |multiplication factor to determine conk(j1,j) from sol_k(j1,j) for HRU
	
!    Drainmod tile equations  01/2006
      real, dimension (:), allocatable :: twash,rnd2,rnd3,sol_cnsw,doxq
      !twash(:)     |days          |time that solids have built-up on streets
      !rnd2(:)     |none          |random number between 0.0 and 1.0
      !rnd3(:)     |none          |random number between 0.0 and 1.0
      !sol_cnsw(:)   |mm H2O        |soil water content used to calculate daily CN value (initial soil wter content for day)
      !doxq(:)     |mg/L          |dissolved oxygen concentration in the surface runoff on current day in HRU
      real, dimension (:), allocatable :: rnd8,rnd9,percn,sol_sumwp
      !rnd8(:)     |none          |random number between 0.0 and 1.0
      !rnd9(:)     |none          |random number between 0.0 and 1.0
      !percn(:)      |kg N/ha       |NO3-N leached from soil profile during the day
      !sol_sumwp(i) = sol_sumwp(i) + sol_wpmm(j,i)
      real, dimension (:), allocatable :: tauton,tautop,cbodu,chl_a,qdr
      !tauton(:)   |kg N/ha       |amount of N applied in autofert operation in year
      !tautop(:)   |kg P/ha       |amount of P applied in autofert operation in year
      !cbodu(:)      |mg/L          |carbonaceous biological oxygen demand of surface runoff on current day in HRU
      !chl_a(:)    |microgram/L   |chlorophyll-a concentration in water yield on current day in HRU
      !qdr(:)        |mm H2O        |total amount of water entering main channel for day from HRU
      real, dimension (:), allocatable :: tfertn,tfertp,tgrazn,tgrazp
      !tfertn(j) = tfertn(j) + fertn
      !tfertp(j) = tfertp(j) + fertp
      !tgrazn(j) = tgrazn(j) + grazn
      !tgrazp(j) = tgrazp(j) + grazp
      real, dimension (:), allocatable :: latno3,latq,minpgw,no3gw,nplnt
      !latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for the day
      !latq(:)       |mm H2O        |amount of water in lateral flow in HRU for the day
      !minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
      !no3gw(:)     |kg N/ha        |nitrate loading to reach in groundwater
      !nplnt(:)      |kg N/ha       |plant uptake of nitrogen in HRU for the day
      real, dimension (:), allocatable :: tileq, tileno3, tilep         !!R683 1/13/22 nbs
      !tileq(j) = tileq(j) + tileo
      !tileno3(j) = bss(4,j) * tile_ttime(j)
      real, dimension (:), allocatable :: sedminpa,sedminps,sedorgn
      !sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed to sediment in surface runoff in HRU for day
      !sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed to sediment in surface runoff in HRU for day
      !sedorgn(:)   |kg N/ha        |amount of organic nitrogen in surface runoff
      real, dimension (:), allocatable :: sedorgp,sedyld,sepbtm,strsn
      !sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface runoff in HRU for the day
      !sedyld(:)   |metric tons   |daily soil loss caused by water erosion
      !sepbtm(:)     |mm H2O        |seepage leaving the bottom of the soil
      !strsn(:)    |none          |fraction of potential plant growth achieved on the day where the reduction is caused by nitrogen stress
      real, dimension (:), allocatable :: strsp,strstmp,surfq,surqno3
      !strsp(:)    |none          |fraction of potential plant growth achieved on the day where the reduction is caused by phosphorus stress
      !strstmp(:)  |none             |fraction of potential plant growth achieved on the day in HRU where the reduction is caused by temperature stress
      !surfq(:)    |mm H2O        |surface runoff generated on day in HRU
      !surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for the day
      real, dimension (:), allocatable :: tcfrtn,tcfrtp,hru_ha,hru_dafr
      !tcfrtn(j) = tcfrtn(j) + cfertn
      !tcfrtp(j) = tcfrtp(j) + cfertp
      !hru_ha(:)   |ha            |area of HRU in hectares
      ! hru_dafr(:) |km**2/km**2   |fraction of watershed area in HRU
      real, dimension (:), allocatable :: drydep_no3, drydep_nh4
      !drydep_no3  |kg/ha/yr      |atmospheric dry deposition of nitrates !allocate (drydep_no3(msub))
      !drydep_nh4  |kg/ha/yr      |atmospheric dry deposition of ammonia    !allocate (drydep_nh4(msub))
      !
      real, dimension (:), allocatable :: phubase,bio_yrms,hvstiadj
      !phubase(:)  |heat units    |base zero total heat units (used when no land cover is growing
      !bio_yrms(:) |metric tons/ha|annual biomass (dry weight) in the HRU
      !hvstiadj(:) |none          |harvest index adjusted for water stress
      real, dimension (:), allocatable :: laimxfr,laiday,chlap,pnd_psed
      
      !!   f           |none             |fraction of plant's maximum leaf area index corresponding to a given fraction of potential heat units for plant
      !!   ff = f - laimxfr(j)
      !!   laimxfr(j) = f
      !!laiday(:)   |m**2/m**2     |leaf area index
      !chlap(:)    |none          |chlorophyll-a production coefficient for pond
      !pnd_psed(:) |kg P          |amount of mineral P attached to sediment
      real, dimension (:), allocatable :: wet_psed,seccip,plantn,plt_et
      !wet_psed(:)   |kg P          |amount of mineral P attached to sediment
      !seccip(:)   |none          |water clarity coefficient for pond
      !plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
      !plt_et(:)   |mm H2O           |actual ET simulated during life of plant
      real, dimension (:), allocatable :: plt_pet,plantp,bio_aams
      !plt_pet(:)  |mm H2O           |potential ET simulated during life of plant
      !plantp(:)      |kg P/ha       |amount of phosphorus in plant biomass
      !bio_aams(:)   |metric tons/ha|average annual biomass (dry weight) in HRU
      real, dimension (:), allocatable :: bio_aamx,lai_yrmx,dormhr
      !bio_aamx
      !lai_yrmx(:) |none             |maximum leaf area index for the year in the HRU
      !dormhr(:)      |hour          |time threshold used to define dormant period for plant (when daylength is within
!!                                  |the time specified by dormhr from the minimum daylength for the area, the plant will go dormant)
      real, dimension (:), allocatable :: lat_pst
      !lat_pst(:)   |kg pst/ha     |amount of pesticide in lateral flow in HRU
      real, dimension (:), allocatable :: orig_snohru,orig_potvol,fld_fr        !!rewinding
      real, dimension (:), allocatable :: orig_alai,orig_bioms,pltfr_n          !!rewinding
      real, dimension (:), allocatable :: orig_phuacc,orig_sumix,pltfr_p        !!rewinding
      real, dimension (:), allocatable :: orig_phu, phu_plt                     !!rewinding
      real, dimension (:), allocatable :: orig_shallst,orig_deepst              !!rewinding
      real, dimension (:), allocatable :: orig_pndvol,orig_pndsed,rip_fr        !!rewinding
      real, dimension (:), allocatable :: orig_pndno3,orig_pndsolp              !!rewinding
      real, dimension (:), allocatable :: orig_pndorgn,orig_pndorgp             !!rewinding
      real, dimension (:), allocatable :: orig_wetvol,orig_wetsed               !!rewinding
      real, dimension (:), allocatable :: orig_wetno3,orig_wetsolp              !!rewinding
      real, dimension (:), allocatable :: orig_wetorgn,orig_wetorgp             !!rewinding
      real, dimension (:), allocatable :: orig_solcov,orig_solsw                !!rewinding
      real, dimension (:), allocatable :: orig_potno3,orig_potsed               !!rewinding
      real, dimension (:), allocatable :: wtab,wtab_mn,wtab_mx
      !wtab
      !wtab_mn
      !wtab_mx
      real, dimension (:), allocatable :: shallst_n,gw_nloss,rchrg_n
      !shallst_n(:)|ppm NO3-N     |nitrate concentration in shallow aquifer
      !gw_nloss(ihru) = Exp(-.693 / hlife_ngw)
      !!! amount of nitrate getting to the shallow aquifer !rchrg_n(j) = (1.- gw_delaye(j)) * percn(j) + gw_delaye(j) * rchrgn1
      real, dimension (:), allocatable :: det_san, det_sil, det_cla
      !det_san
      !det_sil
      !det_cla
      real, dimension (:), allocatable :: det_sag, det_lag
      !det_sag
      !det_lag
      real, dimension (:), allocatable :: tnylda, afrt_surface
      !tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of yield used in autofertilization
      !afrt_surface(:) |none          |fraction of fertilizer which is applied to top 10 mm of soil (the remaining fraction is applied to first soil layer)
      real :: frt_surface
      !frt_surface   |none          |fraction of fertilizer which is applied to the top 10 mm of soil (the remaining fraction is applied to first soil layer)
      real, dimension (:), allocatable :: auto_nyr, auto_napp
      !auto_nyr(:) |kg NO3-N/ha   |maximum NO3-N content allowed to be applied in one year by auto-fertilization
      !auto_napp(:)|kg NO3-N/ha   |maximum NO3-N content allowed in one fertilizer application
      real, dimension (:), allocatable :: manure_kg, auto_nstrs
      ! manure_kg(:) |(kg/ha)/day   |dry weight of manure deposited on HRU daily
      !auto_nstrs(:)    |none           |nitrogen stress factor which triggers auto fertilization
      real, dimension (:,:), allocatable :: rcn_mo, rammo_mo
      !read (atmofile_num,1002) (rcn_mo(imo,iii), imo = 1,momax)
      !read (atmofile_num,1002) (rammo_mo(imo,iii),imo = 1,momax)  
      real, dimension (:,:), allocatable :: drydep_no3_mo, drydep_nh4_mo
      !read (atmofile_num,1002) (drydep_no3_mo(imo,iii), imo = 1,momax)
      !read (atmofile_num,1002) (drydep_nh4_mo(imo,iii), imo = 1, momax)
      real, dimension (:), allocatable :: rcn_d, rammo_d
      !read (atmofile_num,*,iostat=eof) iyp, idap, (rammo_d(l), rcn_d(l), drydep_nh4_d(l), drydep_no3_d(l),l=1, matmo)
      real, dimension (:), allocatable :: drydep_no3_d, drydep_nh4_d
      !!read (atmofile_num,*,iostat=eof) iyp, idap, (rammo_d(l), rcn_d(l), drydep_nh4_d(l), drydep_no3_d(l),l=1, matmo)
      real, dimension (:,:), allocatable :: yldn
      !yldn(:,:,:) |kg/ha         |average value for yield of crop
      real, dimension (:,:), allocatable :: gwati, gwatn, gwatl
      !gwati(iops,ihru) = mgt1i !mgt1i       |none        |sixth parameter in mgt file operation line
      !gwatn(iops,ihru) = mgt3
      !gwatl(iops,ihru) = mgt7
      real, dimension (:,:), allocatable :: gwatw, gwatd, gwatveg
      !gwatw(iops,ihru) = mgt6
      !gwatd(iops,ihru) = mgt5
      !gwatveg, not used
      real, dimension (:,:), allocatable :: gwata, gwats, gwatspcon
      !gwata, not used
      !gwats(iops,ihru) = mgt8
      !gwatspcon(iops,ihru) = mgt4
      real, dimension (:,:), allocatable :: rfqeo_30d,eo_30d
      !rfqeo_30d(nd_30,j) = precipday - qday - pet_day
      !eo_30d(nd_30,j) = pet_day    !nd_30 = nd_30 + 1
      real, dimension (:,:), allocatable :: wgncur,wgnold,wrt,psetlp
      !wgncur(1,:) |none          |parameter which predicts impact of precip on daily maximum air temperature
      !wgncur(2,:) |none          |parameter which predicts impact of precip on daily minimum air temperature
      !wgnold(:,:) |none          |previous value of wgncur(:)
      !wrt(1,:)    |none          |1st shape parameter for calculation of water retention
      !wrt(2,:)    |none          |2nd shape parameter for calculation of water retention
      !psetlp(1,:) |m/day         |phosphorus settling rate for 1st season  !pond-Du.f90
      !psetlp(2,:) |m/day         |phosphorus settling rate for 2nd season
      real, dimension (:,:), allocatable :: zdb,pst_surq,pst_enr
      !zdb(:,:)      |mm           |division term from net pesticide equation
      !pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface runoff on current day in HRU
      !pst_enr(:,:)  |none         |pesticide enrichment ratio
      real, dimension (:,:), allocatable :: plt_pst,pst_sed,psetlw
      !plt_pst(:,:) |kg/ha            |pesticide on plant foliage
      !pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto sediment
      !psetlw(1,:) |m/day         |phosphorus settling rate for 1st season
      !psetlw(2,:) |m/day         |phosphorus settling rate for 2nd season
      real, dimension (:,:), allocatable :: pcpband,wupnd,tavband,phi
      !pcpband(:,:)|mm H2O        |precipitation for the day in band in HRU
      ! wupnd(:,:)  |10^4 m^3/day  |average daily water removal from the pond
      !tavband(:,:)|deg C         |average temperature for the day in band in HRU
      !phi(1,:)    |m^2           |cross-sectional area of flow in channel at bankfull depth
      !phi(6,:)    |m             |bottom width of main channel
      real, dimension (:,:), allocatable :: wat_phi
      !grass_wway.f90(21):!!    wat_phi(1,:)        |m^2           |cross-sectional area of flow at bankfull
      real, dimension (:,:), allocatable :: wushal,wudeep,tmnband,snoeb
      !ushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow aquifer for the month
      !wudeep(:,:) |10^4 m^3/day  |average daily water removal from the deep aquifer for the month
      ! tmnband(:,:)|deg C         |minimum temperature for the day in band in HRU
      !snoeb(:,:)   |mm H2O        |snow water content in elevation band on current day
      real, dimension (:,:), allocatable :: nsetlw,snotmpeb,bss,surf_bs 
      !nsetlw(1,:) |m/day         |nitrogen settling rate for 1st season 
      ! nsetlw(2,:) |m/day         |nitrogen settling rate for 2nd season
      !snotmpeb(:,:)|deg C         |temperature of snow pack in elevation band
      !snotmpeb(:,:)|deg C         |temperature of snow pack in elevation band
      !bss(1,:)    |mm H2O        |amount of lateral flow lagged
      !surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one day
      real, dimension (:,:), allocatable :: tmxband,nsetlp
      !tmxband(:,:)|deg C         |maximum temperature for the day in band in HRU
      !nsetlp(1,:) |m/day         |nitrogen settling rate for 1st season
      !nsetlp(2,:) |m/day         |nitrogen settling rate for 2nd season
      real, dimension (:,:), allocatable :: rainsub,frad
      !rainsub(:,:)|mm H2O        |precipitation for the time step during the day in HRU
      !frad(:,:)   |none          |fraction of solar radiation occuring during hour in day in HRU
      real, dimension (:),   allocatable ::  rstpbsb
      !rstpbsb(l) = rainsub(k,l)     !allocate (rstpbsb(nstep)
      real, dimension (:,:), allocatable :: orig_snoeb,orig_pltpst          !rewinding
      real, dimension (:,:), allocatable :: terr_p, terr_cn, terr_sl
      !terr_p(iops,ihru) = mgt4
      !terr_cn(iops,ihru) = mgt5
      !terr_sl(iops,ihru) = mgt6
      real, dimension (:,:), allocatable :: drain_d, drain_t, drain_g
      !drain_d(iops,ihru) = mgt4
      !drain_t(iops,ihru) = mgt5
      !drain_g(iops,ihru) = mgt6
      real, dimension (:,:), allocatable :: drain_idep
      !drain_idep(iops,ihru) = mgt7
      real, dimension (:,:), allocatable :: cont_cn, cont_p, filt_w   
      !cont_cn(iops,ihru) = mgt4
      !cont_p(iops,ihru) =  mgt5
      !filt_w, not used.
      real, dimension (:,:), allocatable :: strip_n, strip_cn, strip_c
      !strip_n(iops,ihru) = mgt4
      !strip_cn(iops,ihru) = mgt5
      !strip_cn(iops,ihru) = mgt5
      real, dimension (:,:), allocatable :: strip_p, fire_cn
      !strip_p(iops,ihru) = mgt7
      !fire_cn(iops,ihru) = mgt4
      real, dimension (:,:), allocatable :: cropno_upd,hi_upd,laimx_upd
      !cropno_upd(iops,ihru) = mgt1i
      !hi_upd(iops,ihru) = mgt4
      !laimx_upd(iops,ihru) = mgt5
      real, dimension (:,:,:), allocatable :: pst_lag, phug
      !pst_lag(:,3,:)|kg pst/ha    |amount of pesticide lagged
      !pst_lag(:,1,:)|kg pst/ha    |amount of soluble pesticide in surface runoff
      !pst_lag(:,2,:)|kg pst/ha    |amount of sorbed pesticide in surface runoff
      
 !!     integer, dimension (:), allocatable :: ipot,nrelease,swtrg,hrupest
      integer, dimension (:), allocatable :: nrelease,swtrg,hrupest
      !nrelease(:) |none          |sequence number of impound/release operation
      !swtrg(:)    |none          |rainfall event flag: 0: no rainfall event over midnight 1: rainfall event over midnight
      !hrupest(:)  |none          |pesticide use flag:  | 0: no pesticides used in HRU | 1: pesticides used in HRU
      integer, dimension (:), allocatable :: nro,nrot,nfert
      !nro(:)      |none          |sequence number of year in rotation
      !nrot(:)         |none           |number of years of rotation
      !nfert(:)      |none          |sequence number of fertilizer application
      integer, dimension (:), allocatable :: igro,nair,ipnd1,ipnd2
      !igro(:)    |none           |land cover status code
      !nair(:)         |none          |sequence number of auto-irrigation
      !ipnd1(:)    |none          |beginning month of 2nd "season" of nutrient settling
      !ipnd2(:)    |none          |ending month of 2nd "season" of nutrient settling
      integer, dimension (:), allocatable :: nirr,iflod1,iflod2,ndtarg
      !nirr(:)         |none          |sequence number of irrigation application
      !iflod1(:)   |none          |beginning month of non-flood season
      !iflod2(:)   |none          |ending month of non-flood season
      !ndtarg(:)   |none          |number of days required to reach target storage from current pond storage
      integer, dimension (:), allocatable :: iafrttyp, nstress
      !iafrttyp(j) = mgt1iop(nop(j),j)
      !nstress     |none          |code for approach used to determine amount of nitrogen to HRU
!!                               |0 nitrogen target approach
!!                               |1 annual max approach
      integer, dimension (:), allocatable :: igrotree
      !if (curyr_mat(j) == 0) igrotree(j) = 1
      
      !! burn
      integer, dimension (:), allocatable :: grz_days
      !grz_days(j) = mgt1iop(nop(j),j)
      integer, dimension (:), allocatable :: nmgt,icr,ncut,nsweep,nafert
      !nmgt(:)       |none          |management code (for GIS output only)
      !icr(:)      |none          |sequence number of crop grown within the current year
      !ncut(:)     |none           |sequence number of harvest operation within the current year
      !nsweep(:)   |none          |sequence number of street sweeping operation within the year
      ! nafert(:)   |none          |sequence number of auto-fert application within the year
      integer, dimension (:), allocatable :: irn,irrno,sol_nly,npcp
      !irn(:)      |none          |average annual number of irrigation applications in HRU
      !irrno(:)       |none          |irrigation source location |if IRR=1, IRRNO is the number of the reach
!!                                  |if IRR=2, IRRNO is the number of the reservoir
!!                                  |if IRR=3, IRRNO is the number of the subbasin
!!                                  |if IRR=4, IRRNO is the number of the subbasin
!!                                  |if IRR=5, not used
      !sol_nly(:)  |none          |number of layers in soil profile
      !npcp(:)     |none          |prior day category |1 dry day |2 wet day
      integer, dimension (:), allocatable :: igrz,ndeat,ngr,ncf
      !igrz(:)      |none          |grazing flag for HRU: |0 HRU currently not grazed |1 HRU currently grazed
      !ndeat(:)    |days          |number of days HRU has been grazed
      !ngr(:)       |none          |sequence number of grazing operation
      !ncf(:)       |none          |sequence number of continuous fertilizer
      integer, dimension (:), allocatable :: idorm,urblu,hru_sub,ldrain
      !idorm(:)       |none          |dormancy status code: |0 land cover growing  |1 land cover dormant
      !urblu(:)         |none          |Urban land type identification number from urban.dat
      ! hru_sub(:)     |none          |subbasin in which HRU is located
      !ldrain(:)    |none          |soil layer where drainage tile is located
      integer, dimension (:), allocatable :: hru_seq
      !hru_seq = sequential hru number within the subbasin
      integer, dimension (:), allocatable :: iurban,iday_fert,icfrt
      !iurban(:)  |none             |urban simulation code:  |0  no urban sections in HRU
!!                                 |1  urban sections in HRU, simulate using USGS regression equations
!!                                 |2  urban sections in HRU, simulate using build up/wash off algorithm
      !confert-Qi.f90(350):        iday_fert(j) = iday_fert(j) + 1
      !icfrt(:)     |none          |continuous fert flag for HRU: |0 HRU currently not continuously fertilized
!!                                |1 HRU currently continuously fertilized
      integer, dimension (:), allocatable :: ndcfrt,irip,ifld,hrugis
      !ndcfrt(:)    |days          |number of days HRU has been continuously fertilized
      !irip(:)     |none          |number of HRU (in subbasin) that is a riparian zone
      !ifld(:)     |none          |number of HRU (in subbasin) that is a floodplain
      !hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
      integer, dimension (:), allocatable :: orig_igro,ntil,irrsc
      !orig_igro, rewinding
      !ntiL(:)       |none          |sequence number of tillage operation within current year
      !irrsc(:)       |none          |irrigation source code: 1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside watershed
      integer, dimension (:), allocatable :: iwatable,curyr_mat
      !iwatable     |none          |high water table code: |0 no high water table |1 high water table 
      !sched_mgt.f90(47):            curyr_mat(j) = mgt3iop(nop(j),j)
      integer, dimension (:), allocatable :: ncpest,icpst,ndcpst
      !conapply.f90(130):        ncpest(j) = ncpest(j) + 1
      !icpst                      |icpst = 0 do not apply = 1 application period
      !ndcpst      |day           |current day within the application period
      integer, dimension (:), allocatable :: iday_pest, irr_flag
      !iday_pest   |day           |current day between applications
      !irr_flag
      integer, dimension (:), allocatable :: irra_flag
      !irra_flag, not used.
      integer, dimension (:,:), allocatable :: rndseed, iterr, iyterr
      !iterr, not used
      !iyterr, not used
      !rndseed(:,:)|none        |random number generator seed
      integer, dimension (:,:), allocatable :: itdrain, iydrain, ncrops
      !itdrain, not used
      !iydrain, not used
      !yldn(nicr,j) = yldkg(nicr,j) /  ncrops(nicr,j)
      integer, dimension (:), allocatable :: manure_id
      !manure_id(:)|none               |manure (fertilizer) identification

!!     gsm added for sdr (drainage) 7/24/08
      integer, dimension (:,:), allocatable :: mgt_sdr,idplrot
      !mgt_sdr, not used
      !readmgt-Qi.f90(380):	    idplrot(icrmx(ihru),ihru) = ncrp ; subbasin-Qi.f90(196):      idplrot(icr(j),ihru) = idplt(j)
      integer, dimension (:,:), allocatable :: icont, iycont
      !icont, not used
      !iycont, not used
      integer, dimension (:,:), allocatable :: ifilt, iyfilt
      !ifilt, not used
      !iyfilt, not used
      integer, dimension (:,:), allocatable :: istrip, iystrip
      !istrip, not used
      !iystrip, not used
      integer, dimension (:,:), allocatable :: iopday, iopyr, mgt_ops
      !do while (iida == iopday(iops,ihru).and. iyr == iopyr(iops,ihru))
      !readops.f90(119):          mgt_ops(iops,ihru) = mgt_op  
      real, dimension (:), allocatable :: wshd_pstap, wshd_pstdg
      !wshd_pstap(:)|kg/ha            |total amount of pesticide type applied in watershed during simulation
      !wshd_pstdg(:) |kg pst/ha     |amount of pesticide lost through degradation in watershed
      integer, dimension (:), allocatable :: ndmo,npno,mcrhru
      !ndmo(:)     |days          |cumulative number of days accrued in the month since the simulation began where the
!!                               |array location number is the number of the month
      !npno(:)       |none          |array of unique pesticides used in watershed
      !if (mgt_op == 1) then  !mcrhru(ihru) = mcrhru(ihru) + 1
      character(len=13), dimension (18) :: rfile,tfile
      !rfile(:)    |NA            |rainfall file name (.pcp)
      !tfile(:)    |NA            |temperature file name (.tmp)
!!      character(len=1), dimension (50000) :: hydgrp, kirr  !!for srin's big run

!     character(len=4), dimension (50) :: urbname
      character(len=4), dimension (1000) :: urbname
      !urbname(:)  |NA              |name of urban land use
!!      character(len=16), dimension (50000) :: snam   !! for srin's big runs

      character(len=1), dimension (:), allocatable :: hydgrp, kirr
      !readsol.f90(99):      read (solfile_num,5200) hydgrp(ihru)
      !std2.f90(21):!!    kirr(:)       |NA          |irrigation in HRU
      character(len=16), dimension (:), allocatable :: snam
      !snam(:)       |NA            |soil series name
      character(len=17), dimension (300) :: pname
      !pname(:)    |NA            |name of pesticide/toxin
      
!!    adding qtile to output.hru write 3/2/2010 gsm  increased heds(70) to heds(71)
      character(len=13), allocatable :: heds(:),hedr(:),hedrsv(:) !heds(153),hedr(58),hedrsv(41)        --------------
      !heds(:)     |NA            |column titles in HRU output file
      !hedr(:)     |NA            |column titles in reach output file
      !hedrsv(:)   |NA            |column titles in reservoir output file
      character(len=13), allocatable :: hedsc(:),hedsn(:),hedse(:),hedsp(:)  !hedsc(47),hedsn(53),hedse(9),hedsp(29)
      !hedsc        output_C.hru
      !hedsn        output_N.hru
      !hedse        output_E.hru
      !hedsp        output_P.hru
      character(len=13), allocatable :: hedwtr(:)  !hedwtr(40)
      !hedwtr(:)   |NA            |column titles in HRU impoundment output
      character(len=13), allocatable :: hedbc(:),hedbn(:) !hedbc(56),hedbn(44)  
      !hedbc        output_C.bsn
      !hedbn        output_N.lnd
      character(len=4) :: title(60), cpnm(10000)           !!R683 1/13/22 nbs
      !title       |NA          |description lines in file.cio(1st 3 lines)
      !cpnm(:)       |NA            |four character code to represent crop name
      character(len=17), dimension(100) :: fname            !!R682 10/20/21 nbs
      !resetlu.f90(35):      open (no_lup_num,file=fname(no_lup))
      
! measured input files
      real, dimension (:,:,:), allocatable :: flomon,solpstmon,srbpstmon
      !flomon(:,:,:)   |m**3/d        |average daily water loading for month
      !solpstmon(:,:,:)|mg pst/day    |average daily soluble pesticide loading for month
      !srbpstmon(:,:,:)|mg pst/day    |average daily sorbed pesticide loading for month
      real, dimension (:,:,:), allocatable :: sedmon,orgnmon,orgpmon
      !sedmon(:,:,:)   |metric tons/d |average daily sediment loading for month
      !orgnmon(:,:,:)  |kg N/day      |average daily organic N loading for month
      !orgpmon(:,:,:)  |kg P/day      |average daily organic P loading for month
      real, dimension (:,:,:), allocatable :: no3mon,minpmon,nh3mon
      !no3mon(:,:,:)   |kg N/day      |average daily NO3-N loading for month    !allocate (no3mon(mrecm,myr,12))
      !minpmon(:,:,:)  |kg P/day      |average daily mineral P loading for month
      !nh3mon(:,:,:)   |kg N/day      |average amount of NH3-N loaded to stream on a given day in the month
      real, dimension (:,:,:), allocatable :: no2mon,bactpmon,bactlpmon
      !no2mon(:,:,:)   |kg N/day      |average amount of NO2-N loaded to stream on a given day in the month
      !bactpmon(:,:,:) |# bact/day    |average amount of persistent bacteria loaded to stream on a given day in the month
      !bactlpmon(:,:,:)|# bact/day    |average amount of less persistent bacteria loaded to stream on a given day in the month
      real, dimension (:,:,:), allocatable :: cmtl1mon,cmtl2mon,cmtl3mon
      !cmtl1mon(:,:,:) |# bact/day    |average amount of conservative metal #1
      !cmtl2mon(:,:,:) |# bact/day    |average amount of conservative metal #2
      !cmtl3mon(:,:,:) |# bact/day    |average amount of conservative metal #3
      real, dimension (:,:,:), allocatable :: chlamon,disoxmon,cbodmon
      !chlamon(:,:,:)  |kg/day        |average daily loading of chlorophyll-a in month
      !disoxmon(:,:,:) |kg/day        |average daily loading of dissolved O2 in month
      !cbodmon(:,:,:)  |kg/day        |average daily loading of CBOD in month
      real, dimension (:,:), allocatable :: floyr,sedyr,orgnyr,orgpyr
      !floyr(:,:)   |m**3/d        |average daily water loading for year
      !sedyr(:,:)   |metric tons/d |average daily sediment loading for year
      !orgnyr(:,:)  |kg N/day      |average daily organic N loading for year
      !orgpyr(:,:)  |kg P/day      |average daily organic P loading for year
      real, dimension (:,:), allocatable :: no3yr,minpyr,nh3yr,no2yr
      !no3yr(:,:)   |kg N/day      |average daily NO3-N loading for year
      !minpyr(:,:)  |kg P/day      |average daily mineral P loading for year
      !nh3yr(:,:)   |kg N/day      |average daily NH3-N loading for year
      !no2yr(:,:)   |kg N/day      |average daily NO2-N loading for year
      real, dimension (:,:), allocatable :: bactpyr,bactlpyr,cmtl1yr
      !bactpyr(:,:) |# bact/day    |average daily loading of persistent bacteria for year
      !bactlpyr(:,:)|# bact/day    |average daily loading of less persistent bacteria for year
      !cmtl1yr(:,:) |kg/day        |average daily loading of conservative metal #1 for year
      real, dimension (:,:), allocatable :: cmtl2yr,cmtl3yr,chlayr
      !cmtl2yr(:,:) |kg/day        |average daily loading of conservative metal #2 for year
      !cmtl3yr(:,:) |kg/day        |average daily loading of conservative metal #3 for year
      !chlayr(:,:)  |kg/day        |average daily loading of chlorophyll-a in year 
      real, dimension (:,:), allocatable :: disoxyr,cbodyr,solpstyr
      !disoxyr(:,:) |kg/day        |average daily loading of dissolved O2 in year
      !cbodyr(:,:)  |kg/day        |average daily loading of CBOD in year
      !solpstyr(:,:)|mg pst/day    |average daily soluble pesticide loading for year
      real, dimension (:,:), allocatable :: srbpstyr
      !srbpstyr(:,:)|mg pst/day    |average daily sorbed pesticide loading for year
	real, dimension (:,:), allocatable :: sol_mc,sol_mn,sol_mp
	!anfert-Qi.f90(273):            sol_mc(ly,j) = sol_mc(ly,j) + xx * dwfert * forgn(ifrt)*10.
	!sol_mn(ly,j) = sol_mn(ly,j) + xx * dwfert * forgn(ifrt)
	!anfert-Qi.f90(275):            sol_mp(ly,j) = sol_mp(ly,j) + xx * dwfert * forgp(ifrt)
      real, dimension (:), allocatable :: flocnst,sedcnst,orgncnst
      !flocnst(:)   |m^3 H2O/day   |average daily water loading to reach 
      !sedcnst(:)   |metric tons/d |average daily sediment loading for reach
      !orgncnst(:)  |kg N/day      |average daily organic N loading to reach
      real, dimension (:), allocatable :: orgpcnst,no3cnst,minpcnst
      !orgpcnst(:)  |kg P/day      |average daily organic P loading to reach
      !no3cnst(:)   |kg N/day      |average daily nitrate loading to reach
      !minpcnst(:)  |kg P/day      |average daily soluble P loading to reach
      real, dimension (:), allocatable :: nh3cnst,no2cnst,bactpcnst
      !nh3cnst(:)   |kg N/day      |average daily ammonia loading to reach
      !no2cnst(:)   |kg N/day      |average daily nitrite loading to reach
      !bactpcnst(:) |# bact/day    |average daily persistent bacteria loading to reach
      real, dimension (:), allocatable :: cmtl1cnst,cmtl2cnst,bactlpcnst
      !cmtl1cnst(:) |kg/day        |average daily conservative metal #1 loading
      !cmtl2cnst(:) |kg/day        |average daily conservative metal #2 loading
      !bactlpcnst(:)|# bact/day    |average daily less persistent bacteria loading to reach
      real, dimension (:), allocatable :: cmtl3cnst,chlacnst,disoxcnst
      !cmtl3cnst(:) |kg/day        |average daily conservative metal #3 loading
      !chlacnst(:)  |kg/day        |average daily loading of chlorophyll-a
      !disoxcnst(:) |kg/day        |average daily loading of dissolved O2 
      real, dimension (:), allocatable :: cbodcnst,solpstcnst,srbpstcnst
      !cbodcnst(:)  |kg/day        |average daily loading of CBOD to reach
      !solpstcnst(:)|mg/day        |average daily soluble pesticide loading
      !srbpstcnst(:)|mg/day        |average daily sorbed pesticide loading

! hourly time step (by AVG)
      integer :: idt, nstep
      real, dimension (:), allocatable :: hrtwtr,hhstor,hdepth,hsdti
      !hrtwtr(:)        |m^3 H2O       |flow out of reach
      !hhstor(:)   |m^3 H2O       |water stored in reach at end of hour
      !hdepth(:)   |m             |depth of flow during hour
      !hsdti(:)    |m^3/s         |flow rate in reach for hour
      real, dimension (:), allocatable :: hrchwtr,hsedyld
      !hrchwtr(ii)      |m^3 H2O       |water stored in reach at beginning of day
      !hsedyld(:)    |metric tons|sediment transported out of reach during hour
      real, dimension (:), allocatable :: hsedst
      !hsedst(:)    |metric tons   |amount of sediment stored in reach
      real, dimension (:), allocatable :: hharea,hsolpst,hsorpst
      !hharea(:)   |m^2           |cross-sectional area of flow for time step
      !hsolpst(:)  |mg pst/m^3    |soluble pesticide concentration in outflow
      !hsorpst(:)  |mg pst/m^3    |sorbed pesticide concentration in outflow
 
      real, dimension (:), allocatable :: hhqday,precipdt
      !hhqday(:)   |mm H2O        |surface runoff for the hour in HRUS
      !precipdt(:) |mm H2O      |precipitation in time step for HRU
      real, dimension (:), allocatable :: hhtime,hbactp,hbactlp
      !hhtime(:)        |hr            |flow travel time for hour
      !hbactp(:)    |# cfu/100mL  |persistent bacteria in reach/outflow during hour
      !hbactlp(:)   |# cfu/100mL  |less persistent bacteria in reach/outflow during hour
! store initial values
      integer, dimension (:), allocatable :: ivar_orig          !rewinding
      real, dimension (:), allocatable :: rvar_orig             !rewinding
! Input Uncertainty, added by Ann van Griensven
      integer ::  nauto, nsave, iatmodep
      !nauto, not used
      !nsave       |none        |number of save commands in .fig file
      !IATMODEP: 0 = average annual inputs 1 = monthly inputs  2 = daily
! additional reach variables , added by Ann van Griensven
        real, dimension (:), allocatable :: wattemp
        !watqual2.f90(551):	   wattemp(jrch) =(heatin * wtrin + wtmp * rchwtr) / wtrtot
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
        real, dimension (:), allocatable :: lkpst_mass, lkspst_mass
        !readlwq.f90(183):      lkpst_mass(i) = lkpst_conc(i) * res_vol(i)
        !readlwq.f90(185):      lkspst_mass(i) = lkspst_conc(i) * lkspst_act(i) * lkarea * 10000
        real, dimension (:), allocatable :: vel_chan
        !vel_chan(:)      |m/s            |average flow velocity in channel
!Additional buffer and filter strip variables Mike White
      real, dimension (:), allocatable :: vfscon,vfsratio,vfsch,vfsi
      !vfscon(:)   |none          |Fraction of the total runoff from the entire field
      !vfsratio(:) |none          |Field area/VFS area ratio
      !vfsch(:)    |none          |Fraction of flow entering the most concentrated 10% of the VFS.
      !schedule_ops.f90(86):		vfsi(ihru) = filter_i(iops,ihru) !! on off flag
      real, dimension (:,:), allocatable :: filter_i,filter_ratio
      !readops.f90(142):		     filter_i(iops,ihru) = mgt1i  !! on off flag
      !readops.f90(143):		     filter_ratio(iops,ihru) = mgt3
      real, dimension (:,:), allocatable :: filter_con,filter_ch
      !readops.f90(144):		     filter_con(iops,ihru) = mgt4
      !readops.f90(145):		     filter_ch(iops,ihru) = mgt5
!! sj, june 07 modifications to carbon balance routines
      real, dimension (:,:), allocatable :: sol_n
      !sol_n(k,j) = 100. * sol_nmass / sol_mass
      !sol_n(j,ihru) = sol_cbn(j,ihru) / 11.0
      integer :: cswat
!! sj, june 07 end

!! sj, dec 07 dynamic bulk density
      real, dimension (:,:), allocatable :: sol_bdp
      !sol_bdp, not used
!! sj dec 07 end

!! Armen Jan 08
      real, dimension (:,:), allocatable :: tillagef
      !zero2-Qi.f90(537):       tillagef = 0.5
      real, dimension (:), allocatable :: rtfr
      !rtfr = 0. ! resetting root fraction per layer array to 0
      real, dimension (:), allocatable :: stsol_rd
      !stsol_rd(:) |mm            |storing last soil root depth for use in harvestkillop/killop
!! Armen Jan 08 end
	integer:: urban_flag, dorm_flag
	!urban_flag, not used
	!dorm_flag
	real :: bf_flg, iabstr
	!readbsn-Qi.f90(536):      read (bsnfile_num,*,iostat=eof) bf_flg
	!sub_subbasin.f90(154):           abstinit = min(iabstr,abstinit + pet_day / nstep)
	real, dimension (:), allocatable :: ubnrunoff,ubntss
	!ubnrunoff(:) |mm H2O      |surface runoff from urban impervious cover
	!ubntss(:)    |metric tons    |TSS loading from urban impervious cover
	real, dimension (:,:), allocatable :: sub_ubnrunoff,sub_ubntss, ovrlnd_dt	
    !sub_ubnrunoff(:,:)|mm H2O  |surface runoff from urban impervious cover in subbasin
    !sub_ubntss(:,:)|metric tons  |TSS loading from urban impervious cover in subbasin
    !surface-Qi.f90(88):          precipdt(ii+1) = precipdt(ii+1) + ovrlnd_dt(j,ii)
	real, dimension (:,:,:), allocatable :: hhsurf_bs
	!bsprev = hhsurf_bs(1,j,nstep)		! lag from previous day 

!! subdaily erosion modeling by Jaehak Jeong
	integer:: sed_ch,iuh
	!sed_ch      |              |channel routing for HOURLY; 0=Bagnold;2=Brownlie;3=Yang;
	!iuh         |              |unit hydrograph method: 1=triangular UH; 2=gamma funtion UH;
	real :: eros_spl, rill_mult, eros_expo, sedprev, c_factor
	!eros_spl    |none          |coefficient of splash erosion varing 0.9-3.1
	!rill_mult   |              |Multiplier to USLE_K for soil susceptible to rill erosion, range 0.5-2.0
	!eros_expo   |              |an expoenet in the overland flow erosion equ ranges 1.5-3.0
	!sedprev = hhsurf_bs(2,j,k)
	!readbsn-Qi.f90(566):      read (bsnfile_num,*,iostat=eof) c_factor
	real :: sig_g, ch_d50, uhalpha, abstinit,abstmax
	!sig_g       |none          |geometric standard deviation of particle sizes for the main channel
	!ch_d50           |mm            |median particle diameter of channel bed
	!uhalpha      |             |alpha coefficient for estimating unit hydrograph using a gamma function (*.bsn)
	!abstinit = min(iabstr,abstinit + pet_day / nstep)
	!abstmax, not used
	real, dimension(:,:), allocatable:: hhsedy, sub_subp_dt
	!hhsedy(:,:) |metric tons |sediment yield for the time step
	!sub_subp_dt(:,:)  |mm H2O      |precipitation for time step in subbasin
	real, dimension(:,:), allocatable:: sub_hhsedy,sub_atmp
	!sub_hhsedy(:,:) |metric tons |sediment yield for the time step in subbasin
	!sub_atmp(sb,1:nstep) = sub_atmp(sb,1:nstep) / sub_fr(sb)
	real, dimension(:), allocatable:: rhy,init_abstrc
	! rhy(:)      |m H2O         |main channel hydraulic radius
	!init_abstrc(j) = init_abstrc(j) - etday / nstep
	real, dimension(:), allocatable:: dratio, hrtevp, hrttlc
	!dratio(i) = 0.42 * sub_km(i) ** -0.125; hhsedy(j,k) = dratio(inum1) * (sedspl + sedov)
	!hrtevp(:)   |m^3 H2O       |evaporation from reach during time step
	!hrttlc(:)   |m^3 H2O       |transmission losses from reach during time step
	real, dimension(:,:,:), allocatable:: rchhr
	!rchhr       write (output_rch_num,5001) j, subgis(j), iida, kk, rch_dakm(j),      (rchhr(ii,j,kk), ii = 1, 7)    	
!! subdaily reservoir modeling by Jaehak Jeong
	real, dimension(:), allocatable:: hhresflwi, hhresflwo, hhressedi, hhressedo 
	!hhresflwi(k) = hhvaroute(2,inhyd,k)
	!reshr.f90(165):                  hhresflwo(k) = (res_vol(jres) - res_evol(jres))
	!reshr.f90(122):	  hhressedi(k) = hhvaroute(3,inhyd,k)
	!reshr.f90(254):          hhressedo(k) = res_sed(jres) * hhresflwo(k)


	
!! bmp modeling by jaehak jeong
      character(len=4), dimension(:), allocatable:: lu_nodrain
      !readbsn-Qi.f90(552):       read (bsnfile_num,*) (lu_nodrain(kk), kk=1,numlu)
      integer, dimension(:), allocatable:: bmpdrain
      real, dimension(:), allocatable :: sub_cn2, sub_ha_urb,&
      	 bmp_recharge 
      !sed-fil
      real, dimension(:), allocatable:: sub_ha_imp,subdr_km,subdr_ickm
      real, dimension(:,:), allocatable:: sf_im,sf_iy,sp_sa,&
       sp_pvol,sp_pd,sp_sedi,sp_sede,ft_sa,ft_fsa,&
       ft_dep,ft_h,ft_pd,ft_k,ft_dp,ft_dc,ft_por,&
       tss_den,ft_alp,sf_fr,sp_qi,sp_k,ft_qpnd,sp_dp,&
       ft_qsw,ft_qin,ft_qout,ft_sedpnd,sp_bpw,ft_bpw,&
       ft_sed_cumul,sp_sed_cumul
      integer, dimension(:), allocatable:: num_sf
      integer, dimension(:,:), allocatable:: sf_typ,sf_dim,ft_qfg,&
       sp_qfg,sf_ptp,ft_fc
      real :: sfsedmean,sfsedstdev  !Jaehak effluent probability method for urban bmp 2017  
      
      !detention pond
	integer, dimension(:), allocatable :: dtp_subnum,dtp_imo,&
       dtp_iyr,dtp_numweir,dtp_numstage,dtp_stagdis,&
       dtp_reltype,dtp_onoff
!! sj & armen changes for SWAT-C
	real, dimension (:), allocatable :: cf, cfh, cfdec
!! sj & armen changes for SWAT-C end
! additional nutrient variables by jeong for montana bitterroot
      real, dimension(:), allocatable :: lat_orgn, lat_orgp 

	integer, dimension(:,:), allocatable :: dtp_weirtype,dtp_weirdim
	
	real, dimension(:), allocatable ::dtp_evrsv,&
       dtp_inflvol,dtp_totwrwid,dtp_lwratio,dtp_wdep,dtp_totdep,&
       dtp_watdepact,dtp_outflow,dtp_totrel,dtp_backoff,dtp_seep_sa,&
       dtp_evap_sa,dtp_pet_day,dtp_pcpvol,dtp_seepvol,dtp_evapvol,&
       dtp_flowin,dtp_backup_length,dtp_intcept,dtp_expont,dtp_coef1,&
       dtp_coef2,dtp_coef3,dtp_dummy1,dtp_dummy2,&
       dtp_dummy3,dtp_ivol,dtp_ised

      integer, dimension (:,:),allocatable :: so_res_flag, ro_bmp_flag
      real, dimension (:,:),allocatable :: sol_watp, sol_solp_pre   
	real, dimension (:,:),allocatable :: psp_store, ssp_store, so_res
	real, dimension (:,:),allocatable :: sol_cal, sol_ph,ori_sol_ph  
      integer:: sol_p_model
      integer, dimension (:,:),allocatable :: a_days, b_days
      real, dimension (:), allocatable :: harv_min, fstap, min_res       
      real, dimension (:,:),allocatable :: ro_bmp_flo, ro_bmp_sed
      real, dimension (:,:),allocatable :: ro_bmp_bac
      real, dimension (:,:),allocatable :: ro_bmp_pp, ro_bmp_sp
      real, dimension (:,:),allocatable :: ro_bmp_pn, ro_bmp_sn

      real, dimension (:,:),allocatable :: ro_bmp_flos, ro_bmp_seds
      real, dimension (:,:),allocatable :: ro_bmp_bacs
      real, dimension (:,:),allocatable :: ro_bmp_pps, ro_bmp_sps
      real, dimension (:,:),allocatable :: ro_bmp_pns, ro_bmp_sns

      real, dimension (:,:),allocatable :: ro_bmp_flot, ro_bmp_sedt
      real, dimension (:,:),allocatable :: ro_bmp_bact
      real, dimension (:,:),allocatable :: ro_bmp_ppt, ro_bmp_spt
      real, dimension (:,:),allocatable :: ro_bmp_pnt, ro_bmp_snt

      real, dimension (:),allocatable :: bmp_flo, bmp_sed, bmp_bac
      real, dimension (:),allocatable :: bmp_pp, bmp_sp
      real, dimension (:),allocatable :: bmp_pn, bmp_sn, bmp_flag

      real, dimension (:),allocatable :: bmp_flos, bmp_seds, bmp_bacs
      real, dimension (:),allocatable :: bmp_pps, bmp_sps
      real, dimension (:),allocatable :: bmp_pns, bmp_sns
       
      real, dimension (:),allocatable :: bmp_flot, bmp_sedt, bmp_bact
      real, dimension (:),allocatable :: bmp_ppt, bmp_spt
      real, dimension (:),allocatable :: bmp_pnt, bmp_snt

      real, dimension(:,:), allocatable:: dtp_wdratio,dtp_depweir,&
       dtp_diaweir,dtp_retperd,dtp_pcpret,dtp_cdis,dtp_flowrate,&
       dtp_wrwid,dtp_addon
!!    added for manure Armen Jan 2009
 !!     real, dimension (:,:), allocatable :: sol_mc, sol_mn, sol_mp

      !retention irrigation
      real, dimension(:), allocatable:: ri_subkm,ri_totpvol,&
       irmmdt
      real, dimension(:,:), allocatable:: ri_sed,ri_fr,ri_dim,&
       ri_im,ri_iy,ri_sa,ri_vol,ri_qi,ri_k,ri_dd,ri_evrsv, &
       ri_dep,ri_ndt,ri_pmpvol,ri_sed_cumul,hrnopcp,ri_qloss,&
       ri_pumpv,ri_sedi
      character(len=4), dimension(:,:), allocatable:: ri_nirr
      integer, dimension(:), allocatable:: num_ri,ri_luflg,num_noirr
      
      !wet pond
      integer, dimension(:), allocatable:: wtp_subnum,wtp_onoff,wtp_imo,&
       wtp_iyr,wtp_dim,wtp_stagdis,wtp_sdtype      
      real, dimension(:), allocatable:: wtp_pvol,wtp_pdepth,wtp_sdslope,&
       wtp_lenwdth,wtp_extdepth,wtp_hydeff,wtp_evrsv,wtp_sdintc,&
       wtp_sdexp,wtp_sdc1,wtp_sdc2,wtp_sdc3,wtp_pdia,wtp_plen,&
       wtp_pmann,wtp_ploss,wtp_k,wtp_dp,wtp_sedi,wtp_sede,wtp_qi 
     
      real :: bio_init, lai_init, cnop,hi_ovr,harveff,frac_harvk

      ! van Genuchten equation's coefficients
      real :: lid_vgcl,lid_vgcm,lid_qsurf_total,lid_farea_sum         !!R666 7/19/17 nbs
      
      ! soil water content and amount of accumulated infiltration
      real, dimension(:,:), allocatable :: lid_cuminf_last,&
      lid_sw_last, interval_last,lid_f_last,lid_cumr_last,lid_str_last,&
      lid_farea,lid_qsurf,lid_sw_add,lid_cumqperc_last,lid_cumirr_last,&
      lid_excum_last                                                      !! nbs
      
      ! Green Roof
      integer, dimension(:,:), allocatable:: gr_onoff,gr_imo,gr_iyr
      real, dimension(:,:), allocatable:: gr_farea,gr_solop,gr_etcoef,&
      gr_fc,gr_wp,gr_ksat,gr_por,gr_hydeff,gr_soldpt,gr_dummy1,&
      gr_dummy2,gr_dummy3,gr_dummy4,gr_dummy5
            
      ! Rain Gerden
      integer, dimension(:,:), allocatable:: rg_onoff,rg_imo,rg_iyr
      real, dimension(:,:), allocatable:: rg_farea,rg_solop,rg_etcoef,&
      rg_fc,rg_wp,rg_ksat,rg_por,rg_hydeff,rg_soldpt,rg_dimop,rg_sarea,&
      rg_vol,rg_sth,rg_sdia,rg_bdia,rg_sts,rg_orifice,rg_oheight,&
      rg_odia,rg_dummy1,rg_dummy2,rg_dummy3,rg_dummy4,rg_dummy5
      
      ! CiStern
      integer, dimension(:,:), allocatable:: cs_onoff,cs_imo,cs_iyr,&
      cs_grcon
      real, dimension(:,:), allocatable:: cs_farea,cs_vol,cs_rdepth,&
      cs_dummy1,cs_dummy2,cs_dummy3,cs_dummy4,cs_dummy5
      
      ! Porous paVement
      integer, dimension(:,:), allocatable:: pv_onoff,pv_imo,pv_iyr,&
      pv_solop
      real, dimension(:,:), allocatable:: pv_grvdep,pv_grvpor,pv_farea,&
      pv_drcoef,pv_fc,pv_wp,pv_ksat,pv_por,pv_hydeff,pv_soldpt,&
      pv_dummy1,pv_dummy2,pv_dummy3,pv_dummy4,pv_dummy5
      
      ! LID general
      integer, dimension(:,:), allocatable:: lid_onoff
      
      
      ! modified schedule_mgt by Zhang
      integer, dimension(:), allocatable:: mgtyr_hru
      
      ! modify file number from numeric to string variable
      !input.std = 24
      integer :: input_std_num
      !open (23,file="file.cio")
      integer :: file_cio_num
      !open (103,file=bsnfile)
      integer :: bsnfile_num
      !open (29,file=plantdb)
      integer :: plantdb_num
      
      !open (8,file=urbandb)
      integer :: urbandb_num
      
      !open (171,file=septdb)
      integer :: septdb_num
!      open (7,file=fertdb)
        integer :: fertdb_num
!      open (31,file=pestdb)
        integer :: pestdb_num
!      open (30,file=tilldb)
        integer :: tilldb_num
!      open (27,file=figfile)
        integer :: figfile_num
!      open (25,file=subfile)
        integer :: subfile_num
!      open (12,file=fcstfile)
        integer :: fcstfile_num
        integer :: fcstfile2_num
!      open (9,file=solfile,recl=350)
        integer :: silfile_num
!      open (10,file=mgtfile)
        integer :: mgtfile1_num
!      open (11,file=chmfile)
        integer :: chmfile1_num
!       open (1031,file="basins.cbn")
        integer :: basins_cbn_num
!        open (9999,file='fin.fin')
        integer :: fin_fin_num


        !open (104,file=plantdb)
        integer :: plantdb2_num
        !open (105,file=tilldb)
        integer :: tilldb2_num
        !open (106,file=pestdb)
        integer :: pestdb2_num
        !open (107,file=fertdb)
        integer :: fertdb2_num
        !open (108,file=urbandb)
        integer :: urbandb2_num
        
!        open (100+j,file=rfile(j),recl=1850)
        integer :: rfile_num
!        open (118+j,file=tfile(j),recl=20000)
        integer :: tfile_num
!        open (137,file=slrfile,recl=15000)
        integer :: slrfile_num
!        open (138,file=rhfile,recl=15000)
        integer :: rhfile_num
!        open (139,file=wndfile,recl=15000)
        integer :: wndfile_num
!        open (140,file=petfile)
        integer :: petfile_num
!        open (127,file=atmofile)
        integer :: atmofile_num
!        open (101,file=wwqfile)
        integer :: wwqfile_num
!        open (98,file="cswat_profile.txt")
        integer :: cswat_profile_num
!        open (1001,file="cswat_daily.txt")
        integer :: cswat_daily_num
!        open (1002,file="cswat_daily1.txt")
        integer :: cswat_daily1_num
!      
!        open (101,file=subfile)
        integer :: subfile2_num
!        open (103,file=rtefile)
        integer :: rtefile_num
!        open (104,file=swqfile)
        integer :: swqfile_num
!        open (105,file=resfile)
        integer :: resfile_num
        
!        open (106,file=lwqfile)
        integer :: lwqfile_num
!        open (107,file=month_in,recl=350)
        integer :: month_in_num
!        open (108,file=year_in,recl=350)
        integer :: year_in_num
!        open (109,file=annual_in,recl=350)
        integer :: annual_in_num
!        open (113,file=rufile)
        integer :: rufile_num
!        
!        open (101,file="file.cio")
        integer :: file_cio2_num
!        open (102,file=figfile)
        integer :: figfile2_num
!        
!        open (200+inum1s(idum),file=hour_in,recl=350)
        integer :: hour_in_num
!        open (40+inum1s(idum),file=day_in,recl=350)
        integer :: day_save_num
!        open (555+inum1s(idum),file=day_in,recl=350)
        integer :: day_in_num
!        open (112+inum1s(idum),file=apex_in,recl=350)
        integer :: apex_in_num
!        open (50+inum1s(idum),file=day_in,recl=350)
        !integer :: day_in_num
!        

!        open (121,file='output.snu')
        integer :: output_snu_num
!        open (122,file='lup.dat')
        integer :: lup_dat_num
!        open (115,file='output.snw')
        integer :: output_snw_num
!        open (116,file='ebandtemp.out')
        integer :: ebandtemp_out_num
!        open (24,file="input.std")
        !integer :: input_std_num
!        open (26,file="output.std")
        integer :: output_std_num
!        open (28,file="output.hru",recl=2000)
        integer :: output_hru_num
!        open (33333,file="outputb.hru",form='unformatted')
        integer :: outputb_hru_num
!        open (30,file="output.pst",recl=600)
        integer :: output_pst_num
!        open (31,file="output.sub",recl=1500)
        integer :: output_sub_num
!        open (66666,file = "outputb.sub", form = 'unformatted')
        integer :: outputb_sub_num
!        open (7,file="output.rch",recl=1500)
        integer :: output_rch_num
!        open (8,file="output.rsv",recl=800)
        integer :: output_rsv_num
!        open (77777,file = "outputb.rch", form = 'unformatted')
        integer :: outputb_rch_num
!        open (84,file="output.sed",recl=800)
        integer :: output_sed_num
!        open (77778,file = "bmp-sedfil.out") !jaehak temp urban print out
        integer :: bmp_sedfil_out_num
!        open (77779,file = "bmp-ri.out") !jaehak temp urban print out
        integer :: bmp_ri_out_num
!        open (82,file='output.wql')
        integer :: output_wql_num
!        open (83,file='hourq.out')
        integer :: hourq_out_num
!         
!        open (11,file='rch.dat')
        integer :: rch_dat_num
!        open (12,file='hru.dat')
        integer :: hru_dat_num
!        open (13,file='sub.dat')
        integer :: sub_dat_num
!        open (14,file='rsv.dat')
        integer :: rsv_dat_num
!        open (11123,file='hyd.out')
        integer :: hyd_out_num
!        open (16,file='chan.deg')
        integer :: chan_deg_num
!         
!        open (17,file='wbl.out')
        integer :: wbl_out_num
!        open (18,file='swat.qst')
        integer :: swat_qst_num
!         
!
!        open (129,file='output.swr')
        integer :: output_swr_num
!        open (141,file='output.vel')
        integer :: output_vel_num
!        open (142,file='output.dep')
        integer :: output_dep_num
!        open (143, file="output.mgt", recl=60
        integer :: output_mgt_num
!        open (29,file="output.wtr",recl=800)
        integer :: output_wtr_num
!        open (125,file='output.pot')
        integer :: output_pot_num
!        open (19,file="output2.std")
        integer :: output2_std_num
!        open (20,file="output2.rch",recl=600)
        integer :: output2_rch_num
!        open (21,file="output2.hru",recl=800)
        integer :: output2_hru_num
!        open (22,file="output2.rsv",recl=800)
        integer :: output2_rsv_num
!        open (173,file='septic.out')
        integer :: septic_out_num  
!        open (2222,file='charles.out',recl=800)
        integer :: charles_out_num
!
!        open (1032,file="basins.rwq")   !! Added b
        integer :: basins_rwq_num
!        open (281,file="output_C.hru",recl=2000)  
        integer :: output_C_hru_num
!        open (282,file="output_N.hru",recl=2000)  
        integer :: output_N_hru_num
!        open (283,file="output_E.hru",recl=2000)  
        integer :: output_E_hru_num
!        open (284,file="output_P.hru",recl=2000)  
        integer :: output_P_hru_num
!        open (261,file="output_C.std")
        integer :: output_C_std_num
!        open (262,file="output_N.std")
        integer :: output_N_std_num
!        open (271,file="output_C.lnd")
        integer :: output_C_lnd_num
!        open (272,file="output_N.lnd")
        integer :: output_N_lnd_num
!        open (2721,file="output_Nagr.lnd")
        integer :: output_Nagr_lnd_num
!        open (2722,file="output_Nfor.lnd")
        integer :: output_Nfor_lnd_num
!        open (2723,file="output_Ngra.lnd")
        integer :: output_Ngra_lnd_num
        
!        open (2724,file="output_Nwet.lnd") 
        integer :: output_Nwet_lnd_num
!        open (1986,file="co2.dat")   !!--qichun---
        integer :: co2_dat_num
!        open (1996,file="output2.rch")   !! ------
        !integer :: output2_rch_num
!
!
!        open (104,file=dpd_file)
        integer :: dpd_file_num
!        open (104,file=wpd_file)
        integer :: wpd_file_num
!        open (104,file=rib_file)
        integer :: rib_file_num
!        open (104,file=sfb_file)
        integer :: sfb_file_num
!        open (104,file=lid_file)
        integer :: lid_file_num
!        open daily reservoir outflow file 
!        open (350+i,file=resdayo)
        integer :: resdayo_num
!        open (101,file=resmono)
        integer :: resmono_num
!        open (171,file=septdb)
        !integer :: septdb_num
!        open (114,file=wgnfile)
        integer :: wgnfile_num
!        open (104,file=pndfile)
        integer :: pndfile_num
!        open (105,file=wusfile)
        integer :: wusfile_num
!        open (113,file=snofile)
        integer :: snofile_num
!        open (172,file=septfile, status='old')
        integer :: septfile_num
!        open (112,file=sdrfile)
        integer :: sdrfile_num
!        open (106,file=chmfile)
        integer :: chmfile_num
!        open (107,file=solfile)
        integer :: solfile_num
!        open (108,file=hrufile)
        integer :: hrufile_num
!        open (109,file=mgtfile)
        integer :: mgtfile_num
!        open (110,file=gwfile)
        integer :: gwfile_num
!        open (111,file=opsfile)
        integer :: opsfile_num
!        open (9123,file=fname(no_lup))
        integer :: no_lup_num

        integer :: sw_data_out_num
        integer :: sw_data_in_num
        
        integer :: readCEQUAL_num
        integer :: readBottomAlgae_num
        integer :: readCarbonSoil_num
        integer :: readCarbonSedFlux_num
        integer :: readEnergyPara_num
        
        real :: wtmp_add_para
        
!Flood routing variables by Jaehak Jeong 2017    !!R666 7/19/17 nbs
      real :: dthy                                      !!R666 7/19/17 nbs
      integer, dimension(4) :: IHX                      !!R666 7/19/17 nbs
      integer, dimension(:), allocatable :: NHY         !!R666 7/19/17 nbs
      real, dimension(:), allocatable :: RCHX,RCSS,QCAP,CHXA,CHXP  !!R666 7/19/17 nbs
      real, dimension(:,:,:), allocatable :: QHY                   !!R666 7/19/17 nbs 
          
      end module parm