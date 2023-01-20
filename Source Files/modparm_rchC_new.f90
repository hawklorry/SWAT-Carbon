       module parm_rchC
   
   !! -----Parameters--------------
       real:: kdp                                                                               !! Floating Algea death rate / DAY  
       real:: wtrtot1
       real, dimension (:), allocatable :: ch_area   
       real, dimension (:), allocatable :: rch_waterarea 
        
            !-------------Original State Variable---------------------   
          real,dimension(:),allocatable:: rchin_alg                               !! daily inflow floating algea mass; kg               
          real,dimension(:),allocatable:: rchin_orgn                            !! daily inflow orgn; kg   
          real,dimension(:),allocatable:: rchin_nh4                             !! daily inflow nh4; kg  
          real,dimension(:),allocatable:: rchin_no2                             !! daily inflow no2; kg 
          real,dimension(:),allocatable:: rchin_no3                             !! daily inflow no3; kg 
          real,dimension(:),allocatable:: rchin_orgp                             !! daily inflow orgp; kg 
          real,dimension(:),allocatable:: rchin_solp                              !! daily inflow solp; kg 
          real,dimension(:),allocatable:: rchin_cbod                             !! daily inflow cbod; kg 
          real,dimension(:),allocatable:: rchin_dox                               !! daily inflow dox; kg 
        
          real,dimension(:),allocatable:: rchout_alg                               !! daily outflow floating algea; kg  
          real,dimension(:),allocatable:: rchout_orgn                             !! daily outflow orgn; kg  
          real,dimension(:),allocatable:: rchout_nh4                              !! daily outflow nh4; kg  
          real,dimension(:),allocatable:: rchout_no2                              !! daily outflow no2; kg  
          real,dimension(:),allocatable:: rchout_no3                              !! daily outflow no3; kg  
          real,dimension(:),allocatable:: rchout_orgp                             !! daily outflow orgp; kg  
          real,dimension(:),allocatable:: rchout_solp                              !! daily outflow solp; kg  
          real,dimension(:),allocatable:: rchout_cbod                            !! daily outflow cbod ; kg  
          real,dimension(:),allocatable:: rchout_dox                             !! daily outflow dox  ; kg  

          real,dimension(:),allocatable::  rchini_alg                               !! initial floating algea mass at the begining of the day; kg         
          real,dimension(:),allocatable:: rchini_orgn                              !! initial orgn at the begining of the day; kg          
          real,dimension(:),allocatable:: rchini_nh4                               !! initial nh4 at the begining of the day; kg           
          real,dimension(:),allocatable:: rchini_no2                               !! initial no2 at the begining of the day; kg         
          real,dimension(:),allocatable:: rchini_no3                              !! initial no3 at the begining of the day; kg         
          real,dimension(:),allocatable:: rchini_orgp                               !! initial orgp at the begining of the day; kg         
          real,dimension(:),allocatable:: rchini_solp                                !! initial solp at the begining of the day; kg         
          real,dimension(:),allocatable:: rchini_cbod                              !! initial cbod at the begining of the day; kg         
          real,dimension(:),allocatable:: rchini_dox                                !! initial dox at the begining of the day; kg         
          
         real,dimension(:),allocatable:: rchre_alg                                  !! remained floating algea mass at the end of the day; kg  
         real,dimension(:),allocatable:: rchre_orgn                                 !! remained orgn at the end of the day; kg
         real,dimension(:),allocatable:: rchre_nh4                                  !! remained nh4 at the end of the day; kg 
         real,dimension(:),allocatable:: rchre_no2                                  !! remained no2 at the end of the day; kg        
         real,dimension(:),allocatable:: rchre_no3                                  !! remained no3 at the end of the day; kg  
         real,dimension(:),allocatable:: rchre_orgp                                 !! remained orgp at the end of the day; kg  
         real,dimension(:),allocatable:: rchre_solp                                 !! remained solp at the end of the day; kg         
         real,dimension(:),allocatable:: rchre_cbod                                 !! remained cbod at the end of the day; kg       
         real,dimension(:),allocatable:: rchre_dox
      
       !!----Subdaily State varible----------
       real, dimension (:), allocatable :: rch_hAlg                    ! Alg concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hOrgN                ! OrgN concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hNH4                 ! NH4 concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hSolP                  ! SolP concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hCBOD              ! CBOD concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hNO2                 ! NO2  concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hNO3                 ! NO3 concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hOrgP                 ! OrgP concentration in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hDOX                ! DOX concentration in stream at subdaily time step mg/l       
       real, dimension (:), allocatable :: rch_hChla                  ! Chla  concentration in stream at subdaily time step mg/l 
     
       real,dimension (:), allocatable :: rch_hOrgC
       real,dimension (:), allocatable :: rch_hRPOC                     !RPOCconcentration in stream at subdaily time step mg/l 
       real,dimension (:), allocatable :: rch_hLPOC                     !LPOC  concentration  in stream at subdaily time step  mg/l 
       real,dimension (:), allocatable :: rch_hRDOC                    !RDOC concentration in stream at subdaily time step mg/l 
       real,dimension (:), allocatable :: rch_hLDOC                    !LDOC  concentration in stream  at subdaily time step mg/l 
       real,dimension (:), allocatable :: rch_hDIC                        !DIC concentration  in stream at subdaily time step mg/l 
       real, dimension (:), allocatable :: rch_hCH4s
      
       real,dimension (:), allocatable :: rch_hAbM                       ! Bottom algea mass  concentration in stream at subdaily time step  g/m2 
       real,dimension (:), allocatable :: rch_hSedC                       ! Sediment carbon amount at subdaily time step  kg C
       real,dimension (:), allocatable :: rch_hBuryC                      ! Buried carbon amount at subdaily time step  kgC
       
      
         
      
      
        !!------Algea State Variable-----
        real,dimension(:),allocatable:: rch_AbM                           !! Bottom algea biomass concentration ;   g/m2
        real,dimension(:),allocatable:: rch_AbN                            !! Bottom algea N concentration ;   mg/m2
        real,dimension(:),allocatable:: rch_AbP                             !! Bottom algea P concentration ;    mg/m2 

      
       !!------Algea Fluxes------
        real, dimension (:), allocatable ::  rch_Alg_Growth               !! Daily Floating Algea biomass growth;  KG
        real, dimension (:), allocatable ::  rch_Alg_Death                  !! Daily  Floating Algea biomass death ; KG
        real, dimension (:), allocatable ::  rch_Alg_Resp                   !! DailyFloating Algea biomass  resp  ; KG
        real, dimension (:), allocatable ::  rch_Alg_Exc                     !! Daily Floating Algea biomass  exc ; KG
        real, dimension (:), allocatable ::  rch_Alg_Set                      !! Daily Floating Algea biomass settling;  KG
        real,dimension(:),allocatable:: rch_Ab_Death                       !! Daily bottom Algea biomass death ; KG
        real,dimension(:),allocatable:: rch_Ab_Photo                       !! Daily bottom Algea biomass growth;  KG
        real,dimension(:),allocatable:: rch_Ab_Resp                        !! Daily bottom Algea biomass  resp  ; KG
        real,dimension(:),allocatable:: rch_Ab_Exc                          !! Daily bottom Algea biomass  exc ; KG
         
      
        !!-----C Daily State Variables-------
       real,dimension(:),allocatable:: rch_RPOC
       real,dimension(:),allocatable:: rch_LPOC 
       real,dimension(:),allocatable:: rch_RDOC 
       real,dimension(:),allocatable:: rch_LDOC  
       real,dimension(:),allocatable:: rch_DIC 
       real,dimension(:),allocatable:: rch_SedC
       real,dimension(:),allocatable:: rch_BuryC                             !! buried C     KG
       real,dimension(:),allocatable:: rch_CH4s
       real, dimension (:), allocatable :: rch_Bed_BOC                     !!sediment C burial amount      !!  kg

	   real, dimension (:), allocatable :: rchini_RPOC
       real, dimension (:), allocatable :: rchini_LPOC
       real, dimension (:), allocatable :: rchini_RDOC
       real, dimension (:), allocatable :: rchini_LDOC
       real, dimension (:), allocatable :: rchini_DIC
      
       real, dimension (:), allocatable :: rchre_RPOC
       real, dimension (:), allocatable :: rchre_LPOC
       real, dimension (:), allocatable :: rchre_RDOC
       real, dimension (:), allocatable :: rchre_LDOC
       real, dimension (:), allocatable :: rchre_DIC
      
       !!-----C Daily Fluxes-------
       real, dimension (:), allocatable :: rchin_RPOC
	   real, dimension (:), allocatable :: rchin_LPOC
	   real, dimension (:), allocatable :: rchin_RDOC
	   real, dimension (:), allocatable :: rchin_LDOC
	   real, dimension (:), allocatable :: rchin_DIC
	   real,dimension(:),allocatable:: rchout_RPOC         
       real,dimension(:),allocatable:: rchout_LPOC         
       real,dimension(:),allocatable:: rchout_RDOC        
       real,dimension(:),allocatable:: rchout_LDOC       
       real,dimension(:),allocatable:: rchout_DIC
       real,dimension(:),allocatable:: rchout_CH4s
       
	   real, dimension(:),allocatable :: rch_AlgSet                 ! Floating algea C settling to bed sediment  mgC/L  
       real, dimension(:),allocatable :: rch_Alg_deathC              !Floating Algae death to total organic carbon Accumulated KG
       real, dimension(:),allocatable:: rch_Ab_deathC                !bottom algae death to total organic carbon Accumulated KG
       real, dimension(:),allocatable:: rch_Alg_LPOC                 ! Floating Algae -> LPOC   Accumulated KG
       real, dimension(:),allocatable:: rch_Ab_LPOC                  ! Bottom Algae -> LPOC    Accumulated KG
       real, dimension(:),allocatable:: rch_Alg_RPOC                 ! Floating Algae -> RPOC   Accumulated KG
       real, dimension(:),allocatable:: rch_Ab_RPOC                  ! Bottom Algae -> RPOC   Accumulated KG
       real, dimension(:),allocatable:: rch_Alg_LDOC                   ! Algal death to LDOCAccumulated KG
       real, dimension(:),allocatable:: rch_Ab_LDOC                    ! Bottom Algae -> LDOC Accumulated KG 
       real, dimension(:),allocatable:: rch_Alg_RDOC                    ! Floating Algae -> RDOC Accumulated KG
       real, dimension(:),allocatable:: rch_Ab_RDOC                     ! Bottom Algae -> RDOC Accumulated KG
       real, dimension(:),allocatable:: rch_Alg_DIC                         ! Algal respiration to DIC  Accumulated KG
       real, dimension(:),allocatable:: rch_Ab_DIC                         ! Bottom algae respiration to DICAccumulated KG   
       real, dimension(:),allocatable:: rch_LPOC_Set                    ! LPOC -> bed settling     Accumulated KG
       real, dimension(:),allocatable:: rch_LPOC_LDOC               ! LPOC dissolution to LDOCAccumulated KG
       real,dimension(:),allocatable:: rch_LPOC_DIC                    ! LPOC decay to DIC        Accumulated KG
       real,dimension(:),allocatable:: rch_LPOC_RPOC                ! LPOC decay to RPOC    Accumulated KG
       real,dimension(:),allocatable:: rch_RPOC_LDOC                ! RPOC dissolution to LDOC Accumulated KG
       real,dimension(:),allocatable:: rch_RPOC_DIC                  ! RPOC dissolution to LDOC Accumulated KG
       real,dimension(:),allocatable:: rch_RPOC_Set                    !RPOC settling to bed    Accumulated KG
       real,dimension(:),allocatable:: rch_LDOC_DIC                  ! LDOC mineralization to DICAccumulated KG
       real,dimension(:),allocatable:: rch_LDOC_RDOC              ! LDOC decay to RDOC  Accumulated KG
       real,dimension(:),allocatable:: rch_LDOC_NO3                 ! LDOC consumed by NO3- denitrification  Accumulated KG
       real,dimension(:),allocatable:: rch_RDOC_DIC                   ! RDOC mineralization to DIC  Accumulated KG
       real,dimension(:),allocatable:: rch_Atm_DIC                        ! Atmospheric CO2 reaeration Accumulated KG
       real,dimension(:),allocatable:: rch_DIC_Alg                         ! DIC consumed by algal photosynthesisAccumulated KG
       real,dimension(:),allocatable:: rch_DIC_Ab                         ! DIC consumed by algae photosynthesis Accumulated KG
       real,dimension(:),allocatable:: rch_Bed_SedR
       real,dimension(:),allocatable:: rch_LDOC_AbExc
       real,dimension(:),allocatable:: rch_LDOC_AlgExc
       real,dimension(:),allocatable:: rch_Sed_DIC                         ! Sediment release DIC to water column Accumulated KG
       real,dimension(:),allocatable:: rch_Bed_LPOCR
       real,dimension(:),allocatable:: rch_Bed_RPOCR
       real,dimension(:),allocatable:: rch_Bed_CH4R
       real, dimension (:), allocatable :: rch_Bed_DIC                               !  DIC release from bed sediment  mgC/L   
	   real, dimension (:), allocatable :: rch_Bed_CH4                              ! Sediment release CH4 to water column  (hBed_CH4);  kg C/day
       real,dimension(:),allocatable:: rch_CH4s_Atm 
       real,dimension(:),allocatable:: rch_CH4g_Atm
       real,dimension(:),allocatable:: rch_N2g_Atm 
       real,dimension(:),allocatable:: rch_CH4s_CO2 
       real,dimension(:),allocatable:: rch_CH4g_CH4s 
       real,dimension(:),allocatable:: rch_Bed_DenC
       
     
       real,dimension(:),allocatable:: rch_hLPON                     ! LPON concentrationin stream at subdaily time step mg/l 
       real,dimension(:),allocatable:: rch_hRPON                     ! RPON concentration in stream at subdaily time step mg/l 
       real,dimension(:),allocatable:: rch_hLDON                     ! LDON  concentrationin stream at subdaily time step mg/l 
       real,dimension(:),allocatable:: rch_hRDON                     ! RDON concentrationin stream at subdaily time step mg/l 
       real,dimension(:),allocatable:: rch_hDIN      
       real,dimension(:),allocatable:: rch_hN2O                        ! N2O  concentrationin stream at subdaily time step mg/l  
       real,dimension(:),allocatable:: rch_hN2                           ! N2 concentrationin stream at subdaily time step mg/l  

       real,dimension(:),allocatable:: rch_hLPOP                         ! LPON concentrationin stream at subdaily time step mg/l               
       real,dimension(:),allocatable:: rch_hRPOP                         ! LPON concentrationin stream at subdaily time step mg/l         
       real,dimension(:),allocatable:: rch_hLDOP                         ! LPON concentrationin stream at subdaily time step mg/l 
       real,dimension(:),allocatable:: rch_hRDOP                         ! LPON concentrationin stream at subdaily time step mg/l 
       real,dimension(:),allocatable:: rch_hDIP                             ! LPON concentrationin stream at subdaily time step mg/l 
        !!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
   
   
   end module parm_rchC     