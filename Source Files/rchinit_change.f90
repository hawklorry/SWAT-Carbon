      subroutine rchinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the
!!    channel routing command loop

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in whic HRU/reach is located
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihout       |none          |outflow hydrograph storage location number
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    sub_pet(:)  |mm H2O        |potential evapotranspiration for day in
!!                               |subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    hdepth(:)   |m             |depth of flow during hour
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of hour
!!    hhtime(:)   |hr            |travel time of flow in reach for hour
!!    hhvaroute(:,:,:)|varies    |hourly routing storage array
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of hour
!!    hrtwtr(:)   |m^3           |water leaving reach in hour
!!    hsdti(:)    |m^3/s         |flow rate in reach for hour
!!    peakr       |m^3/s         |peak rate of flow in channel
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    qdbank      |m^3 H2O       |streamflow contribution from bank storage
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rchwtr      |m^3 H2O       |water stored in reach at beginning of day
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |flow rate in reach for day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    sorpesto    |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    soxy        |mg O2/L       |saturation oxygen concentration in water
!!    varoute(:,:)|varies        |daily routing storage array
!!    volatpst    |mg pst        |amount of pesticide lost from reach by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    kk          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

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
      
      integer :: ii, jrch, kk
   
      jrch = 0
      jrch = inum1
      
!! add transfer amounts
      do ii = 2, mvaro
        varoute(ii,inum2) = varoute(ii,inum2) + vartran(ii,jrch)
        vartran(ii,jrch) = 0.
      end do
      

!! zero flow out variables   for ihout hyd_stor
      do ii = 1, mvaro
        varoute(ii,ihout) = 0.
        if(cswat == 2 ) &
         varoute2(ii,ihout) = 0.                   !!output2.rch-------------
        if (ievent_rch > 0) then
          do kk = 1, nstep_rch                                     ! changed from 24 to nstep for urban modeling by J.Jeong 4/16/2008
            hhvaroute(ii,ihout,kk) = 0.
          end do
        end if  
      end do

!! initialize daily variables
      bury = 0.
      difus = 0.
      rch_hAlg = 0.                 !! Alg concentration in stream at subdaily time step mg/l 
      rch_hCBOD = 0.                !! CBOD concentration in stream at subdaily time step mg/l 
      rch_hChla = 0.                ! Chla  concentration in stream at subdaily time step mg/l 
      hdepth = 0.                   !|m             |depth of flow on day hourly?
      
      rch_hDOX = 0.
      hharea = 0.
    
      hhstor = 0.
      hhtime = 0.
     
      hrchwtr = 0.
      hrtwtr = 0.
      hsdti = 0.
      hsedst = 0.
      hsedyld = 0.
      rch_hSolP = 0.
      hsolpst =0.
      hsorpst = 0.
      peakr = 0.
      pet_day = 0.
      pet_day = sub_pet(jrch)
      qdbank = 0.
      rcharea = 0.
      rchdep = 0.
      rchwtr = 0.
      rchwtr = rchstor(jrch)
      reactb = 0.
      reactw = 0.
      revapday = 0.
      resuspst = 0.
      rtevp = 0.
      rttime = 0.
      rttlc = 0.
      rtwtr = 0.
      sdti = 0.
      sedrch = 0.
      setlpst = 0.
      solpesto = 0.
      sorpesto = 0.
      soxy = 0.
      volatpst = 0.

      vel_chan(jrch) = 0.
      sedrch = 0.
      rch_san = 0.
      rch_sil = 0.
      rch_cla = 0.
      rch_sag = 0.
      rch_lag = 0.
      rch_gra = 0.
!!    Bank erosion
      rchdy(55,jrch) = 0.
!!    Channel Degredation
      rchdy(56,jrch) = 0.
!!    Channel Deposition
      rchdy(57,jrch) = 0.
!!    Floodplain Deposition
      rchdy(58,jrch) = 0.
!!    Total suspended sediments
      rchdy(59,jrch) = 0.
       
      rch_hNH4 = 0.
      rch_hNO2 = 0.
      rch_hNO3 = 0.
      rch_hOrgN = 0.
      rch_hOrgP = 0.
      
     
      rchin_alg=0.                
      rchin_orgn=0.
      rchin_nh4=0.
      rchin_no2 =0.           
      rchin_no3=0.  
      rchin_orgp=0.  
      rchin_solp=0.  
      rchin_cbod=0.  
      rchin_dox=0.  
      rchin_RPOC=0.
      rchin_LPOC=0.
      rchin_RDOC=0.
      rchin_LDOC=0.
      rchin_DIC=0.
      !rchin_CH4s=0.
      rchin_Alg=0.
      
      rchini_RPOC=0.
      rchini_LPOC=0.
      rchini_RDOC=0.
      rchini_LDOC=0.
      rchini_DIC=0.
      !rchini_CH4s=0.
    
      !rchin_hN2O=0.
      !rchin_hN2=0.
      rchini_Alg=0.
      !rchini_Ab=0.
      !rchini_SedC=0.
      rchre_RPOC=0.
      rchre_LPOC=0.
      rchre_RDOC=0.
      rchre_LDOC=0.
      rchre_DIC=0.
      !rchre_CH4s=0.
      rchre_Alg=0.
      !rchre_Ab=0.
      !rchre_SedC=0.
      rch_Bed_BOC=0.
      !rch_CO2=0.
      rch_Bed_DIC=0.
      rch_Bed_CH4=0.
      
        rch_waterarea =0.   
  	  wattemp=0. 
  
     

            rchin_alg =0.       !rchin_alg                               !! daily inflow floating algea mass; kg               
           rchin_orgn=0.
           rchin_nh4=0.
           rchin_no2=0.
            rchin_no3 =0.
           rchin_orgp =0.
           rchin_solp=0.
           rchin_cbod=0.
           rchin_dox=0.
        
            rchini_alg =0.
           rchini_orgn=0.
           rchini_nh4=0.
           rchini_no2=0.
           rchini_no3 =0.
           rchini_orgp =0.
           rchini_solp=0.
           rchini_cbod=0.
           rchini_dox=0.
         ! rchout_chl
           rchout_alg =0.
           rchout_orgn =0.
           rchout_nh4=0.
           rchout_no2=0.
           rchout_no3=0.
           rchout_orgp=0.
           rchout_solp=0.
           rchout_cbod =0.
            rchout_dox =0.
       
          rchre_alg=0.
          rchre_orgn=0.
          rchre_nh4   =0.                                   
          rchre_no2  =0.                                              
          rchre_no3 =0.                                            
          rchre_orgp =0.                                          
          rchre_solp =0.                                                    
          rchre_cbod =0.                                              
          rchre_dox=0.
      
      

       rch_Bed_DIC=0.

      

        rchout_RPOC = 0.          
        rchout_LPOC = 0.          
        rchout_RDOC = 0.         
        rchout_LDOC= 0.          
        rchout_DIC= 0.             
        rchout_CH4s=0.
        rchout_Alg = 0.            
        rchini_Alg = 0. 
        rchre_Alg = 0. 
        rchre_RPOC = 0.      
        rchre_LPOC = 0.         
        rchre_RDOC = 0.        
        rchre_LDOC = 0.       
        rchre_DIC = 0.             
       rch_Alg_Growth  =0.          
       rch_Alg_Death =0.            
       rch_Alg_Resp=0.                   
       rch_Alg_Exc=0.
       rch_Alg_Set=0.                         
    
       Ab_rch(jrch)%death =0.        
       Ab_rch(jrch)%photo =0.      
       Ab_rch(jrch)%resp  =0.       
       cbn_rec%Alg_cbn  =0.              
       cbn_rec%Ab_cbn  =0.               
       cbn_rec%Alg_LP  =0.                    ! Floating Algae -> LPOC   (mg-C/L/day)
       cbn_rec%Ab_LP   =0.                      ! Bottom Algae -> LPOC     (mg-C/L/day)
       cbn_rec%LP_set  =0.                       ! LPOC -> bed settling     (mg-C/L/day)
       cbn_rec%LP_LD   =0.                   ! LPOC dissolution to LDOC (mg-C/L/day)
       cbn_rec%LP_DIC =0.                 ! LPOC decay to DIC        (mg-C/L/day)
       cbn_rec%LR_POC =0.                 ! LPOC decay to RPOC       (mg-C/L/day)
       cbn_rec%Alg_RP  =0.                   ! Floating Algae -> RPOC   (mg-C/L/day)
       cbn_rec%Ab_RP =0.                     ! Bottom Algae -> RPOC     (mg-C/L/day)
       cbn_rec%RP_LD  =0.                    ! RPOC dissolution to LDOC (mg-C/L/day)
       cbn_rec%RP_DIC  =0.                  ! RPOC dissolution to LDOC (mg-C/L/day)
       cbn_rec%RP_set  =0.                   !RPOC settling to bed (mg-C/L/day)
       cbn_rec%Alg_LD  =0.                   ! Algal death to LDOC (mg-C/L/day)
       cbn_rec%Ab_LD  =0.                    ! Bottom Algae -> LDOC  (mg-C/L/day)
       cbn_rec%LD_DIC  =0.                 ! LDOC mineralization to DIC (mg-C/L/day)
       cbn_rec%LR_DOC =0.            ! LDOC decay to RDOC  (mg-C/L/day)
       cbn_rec%LD_NO3 =0.             ! LDOC consumed by NO3- denitrification (mg-C/L/day)
       cbn_rec%Alg_RD =0.                   ! Floating Algae -> RDOC (mg-C/L/day)
       cbn_rec%Ab_RD  =0.                 ! Bottom Algae -> RDOC  (mg-C/L/day)
       cbn_rec%RD_DIC =0.                ! RDOC mineralization to DIC  (mg-C/L/day)
       cbn_rec%Atm_DIC  =0.                  ! Atmospheric CO2 reaeration (mg-C/L/day)
       cbn_rec%Alg_DIC =0.                ! Algal respiration to DIC   (mg-C/L/day)
       cbn_rec%DIC_Alg   =0.              ! DIC consumed by algal photosynthesis (mg-C/L/day)
       cbn_rec%Ab_DIC  =0.                ! Bottom algae respiration to DIC (mg-C/L/day)
       cbn_rec%DIC_Ab   =0.                  ! DIC consumed by algae photosynthesis (mg-C/L/day)
       cbn_rec%bed_DIC =0.                   ! Sediment release DIC to water column (mg-C/L/day)
       !scbn_rch(jrch)%scbn=0           !!kg
                     
    !!   subdaily water qualit y parameters
      
       rch_hwattemp=0.
    
    
    !!------------NEW Water quality parameters------------
       rch_Ab_Death  =0.
       rch_Ab_Photo  =0.
       rch_Ab_Resp   =0.
       rch_Ab_Exc = 0.
       rch_AlgSet = 0.
       rch_Alg_deathC=  0.                   !Floating Algae death to total organic carbon Accumulated KG
       rch_Ab_deathC=  0.                !bottom algae death to total organic carbon Accumulated KG
       rch_Alg_LPOC=   0.                  ! Floating Algae -> LPOC   Accumulated KG
       rch_Ab_LPOC = 0.                           ! Bottom Algae -> LPOC    Accumulated KG
       rch_LPOC_Set  =    0.                          ! LPOC -> bed settling     Accumulated KG
       rch_LPOC_LDOC = 0.                              ! LPOC dissolution to LDOCAccumulated KG
       rch_LPOC_DIC = 0.                           ! LPOC decay to DIC        Accumulated KG
       rch_LPOC_RPOC=   0.                  ! LPOC decay to RPOC    Accumulated KG
       rch_Alg_RPOC=  0.                          ! Floating Algae -> RPOC   Accumulated KG
       rch_Ab_RPOC=    0.                            ! Bottom Algae -> RPOC   Accumulated KG
       rch_RPOC_LDOC=    0.                          ! RPOC dissolution to LDOC Accumulated KG
       rch_RPOC_DIC =  0.              ! RPOC dissolution to LDOC Accumulated KG
       rch_RPOC_Set  =   0.              !RPOC settling to bed    Accumulated KG
       rch_Alg_LDOC =   0.                 ! Algal death to LDOCAccumulated KG
       rch_Ab_LDOC=    0.               ! Bottom Algae -> LDOC Accumulated KG
       rch_LDOC_DIC=  0.           ! LDOC mineralization to DICAccumulated KG
       rch_LDOC_RDOC=  0.           ! LDOC decay to RDOC  Accumulated KG
       rch_LDOC_NO3 =  0.        ! LDOC consumed by NO3- denitrification  Accumulated KG
       rch_Alg_RDOC =  0.                ! Floating Algae -> RDOC Accumulated KG
       rch_Ab_RDOC = 0.                  ! Bottom Algae -> RDOC Accumulated KG
       rch_RDOC_DIC=0.                 ! RDOC mineralization to DIC  Accumulated KG    
       rch_Atm_DIC = 0.              ! Atmospheric CO2 reaeration Accumulated KG
       rch_Alg_DIC= 0.           ! Algal respiration to DIC  Accumulated KG
       rch_DIC_Alg =   0.             ! DIC consumed by algal photosynthesisAccumulated KG
       rch_Ab_DIC =   0.             ! Bottom algae respiration to DICAccumulated KG
       rch_DIC_Ab=   0.               ! DIC consumed by algae photosynthesis Accumulated KG
       rch_Sed_DIC  =  0.            ! Sediment release DIC to water column Accumulated KG   
       rch_LDOC_AbExc =  0.  
       rch_LDOC_AlgExc =  0.  
       rch_Bed_SedR = 0.
       rch_Bed_LPOCR = 0.
       rch_Bed_RPOCR = 0.
       rch_Bed_CH4R = 0.
 
       rch_CH4s_Atm   =  0.   
       rch_CH4g_Atm  =  0.   
       rch_N2g_Atm  =  0.   
       rch_CH4s_CO2  =  0.   
       rch_CH4g_CH4s  =  0.   
       rch_Bed_DenC = 0.
       
       
    
       hrttlc = 0.
       hrtevp = 0.
    
      return
      end