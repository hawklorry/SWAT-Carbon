      module parm_sedflux
      !This module defines parameters for sediment flux
      
      type carbon_sedflux
            real :: temp

           !
      end type carbon_sedflux    
      type (carbon_sedflux),dimension(:), allocatable:: Sed_rch !in the reach     
      type (carbon_sedflux),dimension(:), allocatable:: Sed_res !in the reservoir     
      type (carbon_sedflux),dimension(:), allocatable:: Sed_wet !in the wetland      
      type (carbon_sedflux),dimension(:), allocatable:: Sed_pnd !in the pond      
        
       
      type carbon_sedflux_para
         !Sediment nutrient fluxes and sediment oxygen demand 
            real :: m1 = 0.5		!    m1	：solids concentration in aerobic layer 1	kgD/L
            real :: m2 = 0.5		!    m2	：solids concentration in anaerobic layer 2	kgD/L
            real :: Dp = 0.00006	!    Dp	：bioturbation particle mixing coefficient	m^2/d
            real :: Dd = 0.0025	    !    Dd	：pore water diffusion coefficient	m^2/d
            real :: w2 = 0.00000685	!    w2	：deep burial velocity	m/d
            real :: H2 = 0.1		!    H2	：thickness of sediment anaerobic layer 2	m
        !Reaction velocities				
            real :: KappaNH3f = 0.131	!    KappaNH3f	  ：freshwater nitrification velocity	m/d
            real :: KappaNH3s = 0.131	!    KappaNH3s	  ：saltwater nitrification velocity	m/d
            real :: KappaNO3_1f = 0.1	!    KappaNO3_1f  ：freshwater denitrification velocity in layer 1	m/d
            real :: KappaNO3_1s = 0.1	!    KappaNO3_1s  ：saltwater denitrification velocity in layer 1	m/d
            real :: KappaNO3_2 = 0.25	!    KappaNO3_2	  ：denitrfication in the anaerobic layer 2	m/d
            real :: KappaCH4 = 0.7		!    KappaCH4     ：methane oxidation in the aerobic layer 1	m/d
        !Half saturation constants				
            real :: KM_NH3 = 0.728	    !    KM_NH3      :nitrification half saturation for NH4N	mgN/L
            real :: KM_O2_NH3 = 0.37	!   KM_O2_NH3	 :nitrification half saturation for O2	mgO2/L
        !Partitioning coefficients				
            real :: KdNH3 = 1		    !    KdNH3	:partition coefficient for NH4 in layer 1 and 2	L/kgD
            real :: KdPO42 = 20		    !    KdPO42	:partition coefficient for PO4 in layer 2	L/kgD
            real :: dKDPO41f = 20		!    dKDPO41f	:freshwater factor that increases the aerobic layer partition coefficient of inorganic P	unitless
            real :: dKDPO41s = 20		!    dKDPO41s	:saltwater factor that increases the aerobic layer partition coefficient of inorganic P	unitless
            real :: O2critPO4 = 2		!    O2critPO4	:critical O2 concentration in layer 1 for adjustment of partition coefficient for inorganic P	mgO2/L
        !Temperature coefficients				
            real :: ThtaDp = 1.117	    !    ThtaDp	:temperature theta for bioturbation mixing between layers 1 and 2	unitless
            real :: ThtaDd = 1.08	    !    ThtaDd	:temperature theta for pore water diffusion between layers 1 and 2	unitless
            real :: ThtaKmNH3 = 1.125	!    ThtaKmNH3	:temperature theta for nitrification half saturation for NH4N	unitless
            real :: ThtaNH3 = 1.123	    !    ThtaNH3	:temperature theta for nitrification rate	unitless
            real :: ThtaNO3 = 1.08	    !    ThtaNO3	:temperature theta for denitrification rate	unitless
            real :: ThtaCH4 = 1.079	    !    ThtaCH4	:temperature theta for methane oxidation rate	unitless
        !Salinity thresholds				
            real :: SALTSW = 1		    !    SALTSW	:salinity above which sulfide rather than methane is produced from C diagenesis	psu
            real :: SALTND = 1		    !    SALTND	:salinity above which saltwater nitrification/denitrification rates are used for aerobic layer	psu
        !Sulfide constants				
            real :: KappaH2Sd1 = 0.2	!    KappaH2Sd1	:aerobic layer reaction velocity for dissolved sulfide oxidation	m/d
            real :: KappaH2Sp1 = 0.4	!    KappaH2Sp1	:aerobic layer reaction velocity for particulate sulfide oxidation	m/d
            real :: ThtaH2S = 1.079	    !    ThtaH2S	:temperature coefficient for sulfide oxidation	unitless
            real :: KMHSO2 = 4		    !    KMHSO2	:sulfide oxidation normalization constant for O2	mgO2/L
            real :: KdH2S1 = 100		!    KdH2S1	:partition coefficient for sulfide in aerobic layer 1	L/kgD
            real :: KdH2S2 = 100		!    KdH2S2	:partition coefficient for sulfide in anaerobic layer 2	L/kgD
        !Fractions of G classes 1 and 2 for settling PON, POC, and POP				
            real :: frpon1 = 0.65	    !    frpon1	:fraction of class 1 pon	unitless
            real :: frpon2 = 0.25	    !    frpon2	:fraction of class 2 pon	unitless
            real :: frpoc1 = 0.65	    !    frpoc1	:fraction of class 1 poc	unitless
            real :: frpoc2 = 0.2		!    frpoc2	:fraction of class 2 poc	unitless
            real :: frpop1 = 0.65	    !    frpop1	:fraction of class 1 pop	unitless
            real :: frpop2 = 0.2		!    frpop2	:fraction of class 2 pop	unitless
        !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P				
            real :: kpon1 = 0.035	    !    kpon1	:G class 1 pon mineralization	day^-1
            real :: kpon2 = 0.0018	    !    kpon2	:G class 2 pon mineralization	day^-1
            real :: kpon3 = 0		    !    kpon3	:G class 3 pon mineralization	day^-1
            real :: kpoc1 = 0.035	    !    kpoc1	:G class 1 poc mineralization	day^-1
            real :: kpoc2 = 0.0018	    !    kpoc2	:G class 2 poc mineralization	day^-1
            real :: kpoc3 = 0		    !    kpoc3	:G class 3 poc mineralization	day^-1
            real :: kpop1 = 0.035	    !    kpop1	:G class 1 pop mineralization	day^-1
            real :: kpop2 = 0.0018	    !    kpop2	:G class 2 pop mineralization	day^-1
            real :: kpop3 = 0		    !    kpop3	:G class 3 pop mineralization	day^-1
        !Temperature coefficients for G class 1, 2, and 3 mineralization				
            real :: ThtaPON1 = 1.1		!    ThtaPON1	:temperature theta for G class 1 pon	unitless
            real :: ThtaPON2 = 1.15	    !    ThtaPON2	:temperature theta for G class 2 pon	unitless
            real :: ThtaPON3 = 1.17	    !    ThtaPON3	:temperature theta for G class 3 pon	unitless
            real :: ThtaPOC1 = 1.1		!    ThtaPOC1	:temperature theta for G class 1 poc	unitless
            real :: ThtaPOC2 = 1.15	    !    ThtaPOC2	:temperature theta for G class 2 poc	unitless
            real :: ThtaPOC3 = 1.17	    !    ThtaPOC3	:temperature theta for G class 3 poc	unitless
            real :: ThtaPOP1 = 1.1		!    ThtaPOP1	:temperature theta for G class 1 pop	unitless
            real :: ThtaPOP2 = 1.15	    !    ThtaPOP2	:temperature theta for G class 2 pop	unitless
            real :: ThtaPOP3 = 1.17	    !    ThtaPOP3	:temperature theta for G class 3 pop	unitless
        !Parameters for partical mixing and benthic stress				
            real :: POC1R = 0.2667	    !    POC1R	:reference G1 at which w12base = Dp / H2 at 20 degC for DiToro eqn 13.1	mgO2/gD
            real :: kBEN_STR = 0.03	    !    kBEN_STR	:first-order decay rate constant for benthic stress (d^-1) for DiToro eqn 13.3	day^-1
            real :: KM_O2_Dp = 4	    !    KM_O2_Dp	:particle mixing half-saturation constant for O2 (mgO2/L)	mgO2/L
        !Parameters for silica				
            real :: KSI = 0.5		    !    KSI	:reaction rate for particulate biogenic Si 	day^-1
            real :: THTASI = 1.1		!    THTASI	:temperature theta for KSI	unitless
            real :: CSISAT = 40		    !    CSISAT	:saturation concentration for porewater Si	mgSi/L
            real :: THTASISAT = 1.023	!    THTASISAT	:temperature theta for CSISAT	unitless
            real :: DPIE1SI = 10		!    DPIE1SI	:incremental partition coefficient for Si in layer 1	L/kgD
            real :: PIE2SI = 100		!    PIE2SI	:partition coefficient for Si in layer 2	L/kgD
            real :: KMPSI = 50000		!    KMPSI	:particulate biogenic Si half saturation constant for dissolution	mgSi/L
            real :: O2CRITSI = 2		!    O2CRITSI	:critical O2 concentration for layer 1 incremental Si sorption	mgO2/L
        !Particulate organic C, N, and P in layer 2				
            real :: POC2_1 = 115.2384431	!    POC2_1	：G class 1 POC in layer 2	gO2/m^3
            real :: POC2_2 = 1047.173118	!    POC2_2	：G class 2 POC in layer 2	gO2/m^3
            real :: POC2_3 = 2046.350365	!    POC2_3	：G class 3 POC in layer 2	gO2/m^3
            real :: PON2_1 = 63.87748906	!    PON2_1	：G class 1 PON in layer 2	gN/m^3
            real :: PON2_2 = 725.569389	!    PON2_2	：G class 2 PON in layer 2	gN/m^3
            real :: PON2_3 = 756.2043796	!    PON2_3	：G class 3 PON in layer 2	gN/m^3
            real :: POP2_1 = 0.92486712	    !    POP2_1	：G class 1 POP in layer 2	gP/m^3
            real :: POP2_2 = 8.404278637	!    POP2_2	：G class 2 POP in layer 2	gP/m^3
            real :: POP2_3 = 16.42335766	!    POP2_3	：G class 3 POP in layer 2	gP/m^3
            real :: POS2 = 2898.096108	    !    POS2	：Particulate biogenic Si in layer 2	gSi/m^3
        !Dissolved constituents in layer 1 and 2 porewater				
            real :: NH3_1 = 0.562033649	    !    NH3_1	：Dissolved ammonia N in layer 1 porewater	mgN/L
            real :: NH3_2 = 4.376815752	    !    NH3_2	：Dissolved ammonia N in layer 2 porewater	mgN/L
            real :: NO3_1 = 0.228998474	    !    NO3_1	：Dissolved nitrate+nitrite N in layer 1 porewater	mgN/L
            real :: NO3_2 = 0.038186719	    !    NO3_2	：Dissolved nitrate+nitrite N in layer 2 porewater	mgN/L
            real :: PO4_1 = 0.077450401	    !    PO4_1	：Dissolved phosphate P in layer 1 porewater	mgP/L
            real :: PO4_2 = 0.292541657	    !    PO4_2	：Dissolved phosphate P in layer 2 porewater	mgP/L
            real :: Si_1 = 1.483185229	    !    Si_1	：Dissolved Si in layer 1 porewater	mgSi/L
            real :: Si_2 = 8.278129634	    !    Si_2	：Dissolved Si in layer 2 porewater	mgSi/L
            real :: HS_1 = 0		        !    HS_1	：Dissolved sulfide in layer 1 porewater (used if salinity >= 1 psu)	mgO2/L
            real :: HS_2 = 0		        !    HS_2	：Dissolved sulfide in layer 2 porewater (used if salinity >= 1 psu)	mgO2/L
            real :: CH4_1 = 0		        !    CH4_1	：Dissolved CH4 in layer 1 porewater (used if salinity < 1 psu)	mgO2/L
            real :: CH4_2 = 0		        !    CH4_2	：Dissolved CH4 in layer 2 porewater (used if salinity < 1 psu)	mgO2/L

           !
      end type carbon_sedflux_para
     
      type (carbon_sedflux_para),dimension(:), allocatable:: Sed_rchpara !in the reach     
      type (carbon_sedflux_para),dimension(:), allocatable:: Sed_respara !in the reservoir     
      type (carbon_sedflux_para),dimension(:), allocatable:: Sed_wetpara !in the wetland      
      type (carbon_sedflux_para),dimension(:), allocatable:: Sed_pndpara !in the pond

      
 end module parm_sedflux