      module carbon_para
   !This module defines all the variables used for the carbon module, added by Du 
      type carbon_soilayer
           real :: DOC=0.         !state variable, DOC concentration (mg/L) in soil layer
           real :: DIC=0.         !state variable, DIC concentration (kg/ha) in soil layer
           real :: percly_DOC=0.   !DOC amount (kg/ha) percolating into underlying layers
           real :: percly_DIC=0.   !DIC amount (kg/ha) percolating into underlying layers
           real :: latly_DOC=0.    !DOC amount (kg/ha) transported by lateral flow in each layer
           real :: latly_DIC=0.    !DIC amount (kg/ha) transported by lateral flow in each layer
      end type carbon_soilayer
     
      type carbon_soil_variables
           real :: RPOC=0.        !RPOC amount transported by sediment in HRU or subbasin (kg/ha)
           real :: LPOC=0.        !RPOC amount transported by sediment in HRU or subbasin (kg/ha)
           real :: latq_DOC=0.    !DOC amount transported by lateral flow in HRU or subbasin (kg/ha)
           real :: latq_DIC=0.    !DIC amount transported by lateral flow in HRU or subbasin (kg/ha)
           real :: surfq_DOC=0.   !DOC amount transported by surface runoff in HRU or subbasin (kg/ha)
           real :: surfq_DIC=0.   !DIC amount transported by surface runoff in HRU or subbasin (kg/ha)
           real :: gwq_DOC=0.     !DOC amount transported by baseflow in HRU or subbasin (kg/ha)
           real :: LDOC=0.
           real :: RDOC=0.        !Total RDOC amount from HRU (Kg/ha)
           real :: DIC=0.         !Total DIC amount from HRU (Kg/ha)
      end type carbon_soil_variables
      type (carbon_soil_variables),dimension(:),allocatable:: cbn_sub  !carbon transport vaiables in subbasin scale
      
      type carbon_hru_variable
           type (carbon_soilayer), dimension(:), allocatable:: cbn_ly !carbon variables in soil profile 
           type (carbon_soil_variables) :: trans  ! carbon transport variables in HRU scale   
           real :: perco_DOC=0.   !DOC amonout percolating from lowest soil layer to shallow aquifer
           real :: shallst_doc=0. !state varibale, DOC amount in shallow aquifer
           real :: rchrg_doc=0.   !DOC recharged into shallow aquifer
      end type carbon_hru_variable 
      type (carbon_hru_variable), dimension(:), allocatable:: cbn_hru
      !------parameters for carbon processes in soil layers 
      !real :: kd_OC=1200.0 !organic carbon partition coefficient,basin sacle parameter,1000 to 1200
      real,allocatable ::kd_OC(:)
      
      type carbon_soil_para
           real :: k_eva=0.1        !DIC evasion rate, DIC to air
           real :: DIC_sat=0.01     !DIC saturation constant (kg/m3),DIC to air
           real :: peroc_DIC=0.95   !DIC percolation coefficient
           real :: peroc_DOC=0.7    !DOC percolation coefficient
           real :: enr_POC =1.5     !POC enrichment ratio 
           real :: hlife_docgw=50.0   !DOC half life (days) in groudwater,calculating DOC decay in groundwater 
           real :: kd_OC=500.0  !organic carbon partition coefficient,basin sacle parameter,1000 to 1200
      end type carbon_soil_para
      type (carbon_soil_para),dimension(:), allocatable:: cbn_sp !carbon parameters in soil layers
      
      type carbon_water_variables
           real :: LPOC=0.0    !LPOC concentration in water colunm (mg/L) (kg for reservoir and wetland)
           real :: RPOC=0.0    !RPOC concentration in water colunm (mg/L) (kg for reservoir and wetland)
           real :: LDOC=0.0    !LDOC concentration in water colunm (mg/L) (kg for reservoir and wetland)
           real :: RDOC=0.0    !RDOC concentration in water colunm (mg/L) (kg for reservoir and wetland)
           real :: DIC=0.0     !DIC  concentration in water colunm (mg/L) (kg for reservoir and wetland)
      end type carbon_water_variables
      type (carbon_water_variables),dimension(:),allocatable:: cbn_rch  !carbon vaiables in the reach
      type (carbon_water_variables),dimension(:),allocatable:: cbn_res  !carbon vaiables in the reservoir
      type (carbon_water_variables),dimension(:),allocatable:: cbn_wet  !carbon vaiables in the wetland
      type (carbon_water_variables),dimension(:),allocatable:: cbn_pnd  !carbon vaiables in the pond
      
      real :: resrpoc,reslpoc,resrdoc,resldoc,resdic  !kg C, amount of carbon leaving reservoir on day  
     
      type carbon_water_para
           !LPOC parameters
           real :: f_lpp    !=0.1       !fraction of algal mortality into LPOC
           real :: f_lpb    !=0.1       !fraction of bottom algal mortality into LPOC
           real :: rca      !=0.4         !algal carbon to biomass ratio
           real :: sv_lp        !=2.5       !LPOC settling velocity (m/day)
           real :: klp      ! =0.075      !LPOC dissolution rate (/day) LPOC->LDOC
           real :: kd_lp        !=0.08     !LPOC decay rate to DIC (/day) LPOC->DIC
           real :: klrp     !=0.01       !decay rate of LPOC to RPOC (/day) LPOC->RPOC
           !RPOC parameters
           real :: f_rpp        !=0.8       !fraction of algal mortality into RPOC
           real :: f_rpb        !=0.8       !fraction of bottom algal mortality into RPOC
           real :: sv_rp        !=2.5      !RPOC settling velocity (m/day)
           real :: krp      !=0.0025      !RPOC dissolution rate (/day) RPOC->LDOC
           real :: kd_rp        !=0.001     !RPOC decay rate to DIC (/day) RPOC->DIC
           !LDOC parameters
           real :: f_ldp        !=0.05      !fraction of algal mortality into LDOC
           real :: f_ldb        !=0.05      !fraction of bottom algal mortality into LDOC
           real :: kld      !=0.25         !LDOC mineralization rate (/day)    LDOC->DIC !!
           real :: ksdocf       !=1.0      !half-saturation oxygen attenuation for DOC oxidation (mgO2/L)
           real :: klrd     !=0.01       !decay rate of LDOC to RDOC (/day) LDOC->RDOC
           real :: kdnit        !=0.002     !NO3 denitrification rate (/day) LDOC consumned
           real :: ksoxdn       !=0.1      !half-saturation oxygen attenuation for denitrification (mgO2/L)
           !RDOC parameters
           real :: krd      !=0.006       ! RDOC mineralization rate (/day) RDOC->DIC  !!
           !DIC parameters
           real :: f_co2        !=0.2       !fraction of DIC in CO2
           real :: p_co2        !=391.0       !partial pressure of CO2 (ppm,parts per million)
      end type carbon_water_para
      type (carbon_water_para),dimension(:), allocatable:: cbn_rchpara  !cbn_rp ! river carbon parameters 
      type (carbon_water_para),dimension(:), allocatable:: cbn_respara  !cbn_vp ! reservoir carbon parameters 
      type (carbon_water_para),dimension(:), allocatable:: cbn_wetpara  !cbn_wp ! wetland carbon parameters
      type (carbon_water_para),dimension(:), allocatable:: cbn_pndpara  !cbn_pp ! pond carbon parameters 
      
      type carbon_water_reaction_pathway
           real ::   Alg_cbn=0.        !Floating Algae to total orgaic carbon (mg-C/L/day)
           real ::   Ab_cbn=0.         !bottom algae to total orgaic carbon (mg-C/L/day)
           !LPOC reaction pathways
           real ::   Alg_LP=0.           ! Floating Algae -> LPOC   (mg-C/L/day)
           real ::   Ab_LP=0.            ! Bottom Algae -> LPOC     (mg-C/L/day)
           real ::   LP_set                  ! LPOC -> bed settling     (mg-C/L/day)
           real ::   LP_LD=0.            ! LPOC dissolution to LDOC (mg-C/L/day)
           real ::   LP_DIC=0.          ! LPOC decay to DIC        (mg-C/L/day)
           real ::   LR_POC=0.         ! LPOC decay to RPOC       (mg-C/L/day)
           !RPOC reaction pathways
           real ::   Alg_RP=0.          ! Floating Algae -> RPOC   (mg-C/L/day)
           real ::   Ab_RP=0.          ! Bottom Algae -> RPOC     (mg-C/L/day)
           real ::   RP_Set               ! RPOC -> bed settling     (mg-C/L/day)
           real ::   RP_LD=0.          ! RPOC dissolution to LDOC (mg-C/L/day)
           real ::   RP_DIC=0.         ! RPOC decay to DIC        (mg-C/L/day)
           !LDOC reaction pathways
           real ::   Alg_LD=0.           ! Floating Algae -> RPOC     (mg-C/L/day)
           real ::   Ab_LD=0.            ! Bottom Algae -> LDOC       (mg-C/L/day)
           real ::   LD_DIC=0.          ! LDOC mineralization to DIC (mg-C/L/day)
           real ::   LR_DOC=0.         ! LDOC decay to RDOC         (mg-C/L/day)
           real ::   LD_NO3=0.         ! LDOC consumed by NO3- denitrification (mg-C/L/day)
           !RDOC reaction pathways
           real ::   Alg_RD=0.           ! Floating Algae -> RDOC      (mg-C/L/day)
           real ::   Ab_RD=0.            ! Bottom Algae -> RDOC        (mg-C/L/day)
           real ::   RD_DIC=0.         ! RDOC mineralization to DIC  (mg-C/L/day)
           !DIC reaction pathways
           real ::  Atm_DIC=0.         ! Atmospheric CO2 reaeration (mg-C/L/day)
           real ::  Alg_DIC=0.           ! Algal respiration to DIC   (mg-C/L/day)
           real ::  DIC_Alg=0.           ! DIC consumed by algal photosynthesis (mg-C/L/day)
           real ::  Ab_DIC=0.           ! Bottom algae respiration to DIC   (mg-C/L/day)
           real ::  DIC_Ab=0.            ! DIC consumed by algae photosynthesis (mg-C/L/day)
           real ::  CBOD_DIC           ! CBOD oxidation (mg-C/L/day)
           real ::  bed_DIC                !Sediment release DIC to water column (mg-C/L/day)
      end type carbon_water_reaction_pathway
      type (carbon_water_reaction_pathway) :: cbn_rec ! carbon reaction pathway in water colunm

      type sediment_carbon
           real :: scbn=0.01         !sediement carbon comparment state variable (mg-C/L/day),intial condition-- NOW it is in kg/day
      end type 
      type (sediment_carbon),dimension(:), allocatable:: scbn_rch    !sediment compartment in the reach 
      type (sediment_carbon),dimension(:), allocatable:: scbn_res    !sediment compartment in the reservoir
      type (sediment_carbon),dimension(:), allocatable:: scbn_wet    !sediment compartment in the wetland
      type (sediment_carbon),dimension(:), allocatable:: scbn_pnd    !sediment compartment in the pond
            
      type sediment_carbon_para
           !real :: scbn=0.01         !sediement carbon comparment state variable (mg-C/L/day),intial condition-- NOW it is in kg/day
           !real :: thick=0.01         !the thickness of sediment layer
           real :: kbur=0.05        !first-order sediment burial rate (/day)
           real :: ksed=0.02        !first-order sediment decay rate (/day)
      end type sediment_carbon_para
      type (sediment_carbon_para),dimension(:), allocatable:: scbn_rchpara    !sediment compartment in the reach 
      type (sediment_carbon_para),dimension(:), allocatable:: scbn_respara    !sediment compartment in the reservoir
      type (sediment_carbon_para),dimension(:), allocatable:: scbn_wetpara    !sediment compartment in the wetland
      type (sediment_carbon_para),dimension(:), allocatable:: scbn_pndpara    !sediment compartment in the pond
      
      type bottom_algae
           real :: Ab=0.001      !state variable,bottom algae biomass (g/m2/day),intial condition
           real :: INb=0.01      !state variable, intracellular N concentration (mg N/m2),intial condition
           real :: IPb=0.01      !state variable, intracellular P concentration (mg P/m2),intial condition
           real :: photo=0.         !pathway,bottom algae photosynthesis biomass (g/m2/day)
           real :: resp=0.          !pathway,bottom algae respiration biomass (g/m2/day)
           real :: death=0.         !pathway,bottom algae death biomass (g/m2/day)
      end type bottom_algae
      type (bottom_algae),dimension(:), allocatable:: Ab_rch    !bottom algae in the reach 
      type (bottom_algae),dimension(:), allocatable:: Ab_res    !bottom algae in the reservoir
      type (bottom_algae),dimension(:), allocatable:: Ab_wet    !bottom algae in the wetland  
      type (bottom_algae),dimension(:), allocatable:: Ab_pnd    !bottom algae in the pond
      
      type bottom_algae_parameters
           !
           real :: Ab_ini   !intial bottom algae  (g/m2)
           !parameter for bottom algae photosynthesis
           integer :: ipho      !=1       ! photosynthesis model option (0, zero order; 1, first order)
           real :: kph      !=36.2        ! maximum photosynthesis rate [d-1 or gD/m2/d] 
           !real :: kph=2.        ! maximum photosynthesis rate [d-1 or gD/m2/d]    used by W2
           !For nutrient limitation
           real :: pmN      !=284.7       !maximum uptake rate for N (mgN/gD/day)
           real :: pmP      !=37.85       !maximum uptake rate for P (mgP/gD/day)
           real :: KqN      !=2.54        !intracelluar N half-saturation constant (mgN/gD)
           real :: KqP      !=4.93        !intracelluar P half-saturation constant (mgP/gD)
           real :: Ksnb     !=0.07       !external N half-saturation constant (mg N/L)
           real :: Kspb     !=0.098      !external P half-saturation constant (mg P/L)
           real :: qoN      !=4.17        !minimum cell quotas of N (mgN/gD/day)
           real :: qoP      !=1.93         !minimum cell quotas of P (mgN/gD/day)
           real :: kexb     !=0.21       !bottom algae excretion rate (/day)
           real :: kscb     !=0.792      !Inorganic carbon half saturation constant (mg/L,unit converted)
           real :: fdic     !=0.6        !the fraction of DIC in H2CO3* and HCO3-,for calculating nutrient limitation
           !For light limitation
           real :: keb      !=0.02        !background light extinction coefficient (/m)
           real :: kiss     !=0.052      !light extinction coefficient by inorganic suspended solids (L/mgD/m)
           real :: kpom     !=0.174      !light extinction coefficient by particular organic matter (POM) (L/mgD/m)
           real :: kap      !=0.0088      !linear light extinction coefficient by algae (L/ugA/m)
           real :: kapn     !=0.054      !non-linear light extinction coefficient by algae (L/ugA)2/3/m
           real :: kAb      !=0.024       !linear light extinction coefficient by bottom [m3/gD/m]
           integer ::light      !=1       !light limitation option (1,2,3)
           real :: klb      !=1.5807      !bottom algae light parameter (MJ/m2/day) 
           !For space limitation factor
           real ::Abmax     !=200.0        !First-order carrying capacity of bottom algae [gD/m2/d]
           !parameters for respiration and death process
           real :: kdb      !=0.02        !bottom algae death rate (/day)
           real :: krb1     !=0.042      !bottom algae basal respiration
           real :: krb2     !=0.389      !photo-respiration rate parameter
           ! real :: kdb=0.1        !bottom algae death rate (/day)      !!  Used by W2
           !real :: krb1=0.04     !bottom algae basal respiration    !!  Used by W2
           !real :: krb2=0.04     !photo-respiration rate parameter    !!  Used by   W2
           real :: ksob     !=0.6        !half-saturation oxygen inhibition constant for respiration 
           !
      end type bottom_algae_parameters
     
      type (bottom_algae_parameters),dimension(:), allocatable:: Ab_rchpara !Ab_rp    !paramters for bottom algae in the reach
     
      type (bottom_algae_parameters),dimension(:), allocatable:: Ab_respara !Ab_vp    !paramters for bottom algae in the reservoir
     
      type (bottom_algae_parameters),dimension(:), allocatable:: Ab_wetpara !Ab_wp    !paramters for bottom algae in the wetland
      
      type (bottom_algae_parameters),dimension(:), allocatable:: Ab_pndpara !Ab_pp    !paramters for bottom algae in the pond
      
      integer,allocatable :: para_region(:)                               !currently not being used.
       
      
      
      
 end module carbon_para