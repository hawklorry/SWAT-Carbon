      module parm_subC
     
     
     !! By Zhang for C/N cycling
      !!SOM-residue C/N state variables -- currently included
      real :: CFB    !carbon fraction of biomass. This parameter is set to 0.42, but need to be updated
                     !to reflect the different between crop, grass, and forest, as well as the difference
                     !between different tissues of a plant.

                    !Cf      : carbon fraction of organic materials 0.42; from data of Pinck et al., 1950)
                     
      !!===============================================================
      !!Control parameters for carbon2-Zhang-Yang-Qi.f90
      real :: LSR, BMR, XBMT, HSR, HPR, LMR, PRMT_51, PRMT_45
              !XBMT    : control on transformation of microbial biomass by soil texture and structure.
                !Its values: surface litter layer = 1; all other layers = 1-0.75*(SILF + CLAF) (Parton et al., 1993, 1994)
       !HPR     : rate of transformation of passive humus under optimal conditions (subsurface
                !layers = 0.000012 day-1) (Parton et al.,1993, 1994)
       !HSR     : rate of transformation of slow humus under optimal conditions (all layers
                != 0.0005 day-1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
       !KOC     : liquid–solid partition coefficient for microbial biomass (10^3 m^3 Mg-1) 
       !BMR     : rate of transformation of microbial biomass and associated products under optimal
       !            conditions (surface = 0.0164 day-1; all other layers = 0.02 day-1) (Parton et al., 1993, 1994)       
       !LMR     : rate of transformation of metabolic litter under optimal conditions (surface =
                !0.0405 day-1; all other layers = 0.0507 day-1) (Parton et al., 1994)
       !PRMT_51 !COEF ADJUSTS MICROBIAL ACTIVITY FUNCTION IN TOP SOIL LAYER (0.1_1.),     
       !PRMT_45 !COEF IN CENTURY EQ ALLOCATING SLOW TO PASSIVE HUMUS(0.001_0.05) ORIGINAL VALUE = 0.003,

      real :: ALSLCO2, ALSLNCO2, ALMCO2, ABCO2, APCO2, ASCO2, ABP, ASP  !A1CO2,
       !ABCO2   : allocation from biomass to CO2; 0.6 (surface litter), 0.85?.68*(CLAF + SILF) (all other layers) (Parton et al., 1993, 1994)
       !ABL     : carbon allocation from biomass to leaching; ABL = (1-exp(-f/(0.01* SW+ 0.1*(KdBM)*DB)) (Williams, 1995)
       !ABP     : allocation from biomass to passive humus; 0 (surface litter), 0.003 + 0.032*CLAF (all other layers) (Parton et al., 1993, 1994)
       !ALMCO2  : allocation from metabolic litter to CO2; 0.6 (surface litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
       !ALSLCO2 : allocation from lignin of structural litter to CO2; 0.3 (Parton et al., 1993, 1994)
       !ALSLNCO2: allocation from non-lignin of structural litter to CO2; 0.6 (surface litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
       !APCO2   : allocation from passive humus to CO2; 0.55 (Parton et al., 1993, 1994)
       !ASCO2   : allocation from slow humus to CO2; 0.55 (Parton et al., 1993, 1994)
       !ASP     : allocation from slow humus to passive; 0 (surface litter), 0.003-0.009*CLAF (all other layers) (Parton et al., 1993, 1994)
      !!===============================================================
      !!Control parameters for carbon2-Zhang-Yang-Qi.f90
      
      
	 real, dimension(:,:), allocatable :: sol_BMC, sol_BMN, sol_HSC, &
       sol_HSN, sol_HPC, sol_HPN, sol_LM, &
       sol_LMC, sol_LMN, sol_LS, sol_LSL, sol_LSC, sol_LSN	, sol_RNMN, &
       sol_LSLC, sol_LSLNC, sol_RSPC, sol_WOC, sol_WON, sol_HP, sol_HS, & 
       sol_BM	
      !	HSC mass of C present in slow humus (kg ha-1)
      !	HSN mass of N present in slow humus (kg ha-1)
      !	HPC mass of C present in passive humus (kg ha-1)
      !	HPN mass of N present in passive humus (kg ha-1)
      !	LM mass of metabolic litter (kg ha-1)
      !	LMC mass of C in metabolic litter (kg ha-1)
      !	LMN mass of N in metabolic litter (kg ha-1)
      !	LS mass of structural litter (kg ha-1)
      !	LSC mass of C in structural litter (kg ha-1)
      !	LSL mass of lignin in structural litter (kg ha-1)
      !	LSN mass of N in structural litter (kg ha-1)
       
      !!SOM-residue C/N state variables -- may need to be included
	real, dimension(:,:), allocatable :: sol_CAC, sol_CEC  
          
	!!daily updated soil layer associated percolaton and lateral flow Carbon loss
	real, dimension(:,:), allocatable :: sol_percc
	real, dimension(:,:), allocatable :: sol_latc
	
      !!Daily carbon change by different means (entire soil profile for each HRU)
	real, dimension(:), allocatable :: sedc_d, surfqc_d, latc_d, &
     percc_d, NPPC_d, GPPC_d,rsdc_d, grainc_d, stoverc_d, soc_d, &
     rspc_d, emitc_d, GR_d, MR_d, ctowoodc1,ctowoodc2,ctowoodc3,C_PROD, &
     leafdiec, frootjdiec, frootmdiec, wood1_SOM1, wood1_SOM2, &
     wood2_SOM1, wood2_SOM2, wood3_SOM1, wood3_SOM2, rsdn_d,denitrify, &
     no3_autof, nh4_autof, no3_immo, nh4_immo, immo_err1, nh4_min, no3_min, no3_fert, &
     no3_denit,no3_up,   &
     no3_nitr,  no3_perc,  no3_rain,  no3_loss_pothole,  &
     no3_adsortion_parti, no3_lat, no3_surf
     
     real, dimension(:,:), allocatable :: no3_immo_ly, nh4_immo_ly, nh4_min_ly 
     
      real, dimension(:), allocatable :: solp_min   
      real, dimension(:), allocatable :: solp_up     
      real, dimension(:), allocatable :: sol_solp2minp  
      real, dimension(:), allocatable :: solp_fert   
      real, dimension(:), allocatable :: solp_autof  
      real, dimension(:), allocatable :: orgn_autof   
      real, dimension(:), allocatable :: orgp_autof   
      real, dimension(:), allocatable :: orgn_grazf    
      real, dimension(:), allocatable :: nh3_grazf    
      real, dimension(:), allocatable :: no3_grazf     
      real, dimension(:), allocatable :: solp_grazf   
      real, dimension(:), allocatable :: orgp_grazf    
      real, dimension(:), allocatable :: no3_conf
      real, dimension(:), allocatable :: nh4_conf
      real, dimension(:), allocatable :: orgn_conf
      real, dimension(:), allocatable :: orgp_conf
      real, dimension(:), allocatable :: solp_conf
      real, dimension(:,:), allocatable :: eavail
      real, dimension (6):: tree_cfrac !! fraction of different plant tissuse for carbon allocation
      real,dimension (6) :: mfprd

      real :: mrleaf, mrbrch, mrlgwood, mrfrootj, mrfrootm, mrcroot !! maintenance respiration
      real :: grleaf, grbrch, grlgwood, grfrootj, grfrootm, grcroot !! growth respiration

	!!emitc_d include biomass_c eaten by grazing, burnt
      
      !!Daily carbon change by different means (entire soil profile for each Subbasin)
      !!Only defined the variables, but not used them in the code
	real, dimension(:), allocatable :: sub_sedc_d, sub_surfqc_d, &
       sub_latc_d,	sub_percc_d,  sub_NPPC_d, sub_rsdc_d, &
      sub_grainc_d, sub_stoverc_d, sub_emitc_d, sub_soc_d, sub_rspc_d
     

      !!Monthly carbon change by different means (entire soil profile for each HRU)	
	real, dimension(:), allocatable :: sedc_m, surfqc_m, latc_m, percc_m, &
       foc_m, NPPC_m, rsdc_m, grainc_m, stoverc_m, emitc_m, soc_m, &
       rspc_m	

      !!Yearly carbon change by different means (entire soil profile for each HRU)	
	real, dimension(:), allocatable :: sedc_a, surfqc_a, latc_a,  &
      percc_a, foc_a, NPPC_a, rsdc_a, grainc_a, stoverc_a, emitc_a,& 	 
       soc_a, rspc_a	
     

	
      !! The following variables are defined and calculated locally
      !! ==================================================================
      !	HSCTP potential transformation of C in slow humus (kg ha-1 day-1)
      !	HSNTP potential transformation of N in slow humus (kg ha.1 day-1)
      !	HPCTP potential transformation of C in passive humus (kg ha-1 day-1)
      !	HPNTP potential transformation of N in passive humus (kg ha-1 day-1)
      !	HPR rate of transformation of passive humus under optimal conditions (subsurface
      !	layers = 0.000012 day-1) (Parton et al.,1993, 1994)
      !	HSR rate of transformation of slow humus under optimal conditions (all layers
      !	= 0.0005 day.1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
      !	KOC liquid C solid partition coefficient for microbial biomass (10^3 m3 Mg-1)
      !	LMF fraction of the litter that is metabolic
      !	LMNF fraction of metabolic litter that is N (kg kg-1)
      !	LMR rate of transformation of metabolic litter under optimal conditions (surface =
      !	 0.0405 day-1; all other layers = 0.0507 day-1) (Parton et al., 1994)
      !	LMCTP potential transformation of C in metabolic litter (kg ha-1 day-1)
      !	LMNTP potential transformation of N in metabolic litter (kg ha-1 day-1)
      !	LSCTP potential transformation of C in structural litter (kg ha-1 day-1) 
      !	LSF fraction of the litter that is structural
      !	LSLF fraction of structural litter that is lignin (kg kg-1)
      !	LSNF fraction of structural litter that is N (kg kg-1)
      !	LSLCTP potential transformation of C in lignin of structural litter (kg ha-1 day-1)
      !	LSLNCTP potential transformation of C in nonlignin structural litter (kg ha-1 day-1)
      !	LSNTP potential transformation of N in structural litter (kg ha-1 day-1)
      !	LSR rate of potential transformation of structural litter under optimal conditions
      !	(surface = 0.0107 day.1; all other layers = 0.0132 day.1) (Parton et al., 1994)
      !	NCBM N/C ratio of biomass
      !	NCHP N/C ratio passive humus
      !	NCHS N/C ratio of the slow humus
      !	OX oxygen control on biological processes with soil depth
      !	Sf fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter

      !!Tillage factor on SOM decomposition
      integer, dimension(:), allocatable :: tillage_switch
      real, dimension(:), allocatable :: tillage_depth
      integer, dimension(:,:), allocatable :: tillage_days
      real, dimension(:), allocatable :: tillage_factor
      ! tillage_factor: = 1.6 in 30 days after tillage practices occur; otherwise 1.0;
!! By Zhang for C/N cycling
          
      real :: forest_nup, no3_before, nh3_before, no3_after, nh3_after
      real, dimension(:), allocatable    :: erosion, usle_sed, ccp
      real, dimension(:), allocatable   :: absorbed_no3, nh4_fert
      real, dimension(:), allocatable   :: absorbed_nh3, nh4_vol


!!The following variables should be defined in Modparm.f

      integer, parameter :: SRFC = 1, SOIL = 2, NELEM = 1
      
      real, dimension(:,:), allocatable ::   CFS3S1
      real, dimension(:,:), allocatable ::   CFMETS1, CFS1S2
      real, dimension(:,:), allocatable ::   CFS1S3, CFS2S1, CFS2S3
      real, dimension(:,:), allocatable ::   CFSTRS1, CFSTRS2
      
      real, dimension(:,:), allocatable ::   CFPltSTR, CFPltMET
      real, dimension(:,:), allocatable ::   CFOrfSTR, CFOrfMET
      real, dimension(:,:), allocatable ::   CBurntSTR, CBurntMET

      real, dimension(:,:), allocatable ::   NFPltSTR, NFPltMET
      real, dimension(:,:), allocatable ::   NFOrfSTR, NFOrfMET
      real, dimension(:,:), allocatable ::   NBurntSTR, NBurntMET
      
      real, dimension(:,:), allocatable ::   CO2FMET, CO2FS1, CO2FS2 
      real, dimension(:,:), allocatable ::   CO2FS3      
      real, dimension(:,:,:), allocatable ::   CO2FSTR
       
        ! CFMETS1     C flow from the metabolic pool to SOM1 (-)
        ! CFS1S2      C flow from SOM1 to SOM2  (kg[C]? ha)
        ! CFS1S2OLD   C flow from SOM1 to SOM2 before limiting it because of
        !               shortage of E available for immobilization. (kg[C]? ha)
        ! CFS1S3      C flow from SOM1 to SOM3 (kg[C]? ha)
        ! CFS1S3OLD   C flow from SOM1 to SOM3 before limiting it because of
        !               shortage of E available for immobilization. (kg[C]? ha)
        ! CFS2S1      C flow from SOM2 to SOM1 (kg[C]? ha)
        ! CFS2S1OLD   C flow from SOM2 to SOM1 before limiting it because of
        !               shortage of E available for immobilization. (kg[C]? ha)
        ! CFS2S3      C flow from SOM2 to SOM3 (kg[C]? ha)
        ! CFS2S3OLD   C flow from SOM2 to SOM3 before limiting it because of
        !               shortage of E available for immobilization. (kg[C]? ha)
        ! CFSTRS1     C flow from the structural pool to SOM1 (kg[C] / ha)
        ! CFSTRS2     C flow from the structural pool to SOM2 (kg[C] / ha)
        ! CO2FMET     CO2 flow that accompanies the C flow out of the metabolic pool
        !               (kg[C]? ha)
        ! CO2FS1      CO2 flow that accompanies the C flow out of SOM1l (kg[C]? ha)
        ! CO2FS2      CO2 flow that accompanies the C flow out of SOM12 (kg[C]? ha)
        ! CO2FS3      CO2 flow that accompanies the C flow out of SOM3l (kg[C]? ha)
        ! CO2FSTR     CO2 flow that accompanies the C flow out of the structural pool
        !               (kg[C]? ha)      

      real, dimension(:,:,:), allocatable ::  EFMETS1, EFS1S2
      real, dimension(:,:,:), allocatable ::  EFS1S3, EFS2S1, EFS2S3
      real, dimension(:,:,:), allocatable ::  EFS3S1, EFSTRS1, EFSTRS2
        ! EFMETS1     E flow from soil or soil or surface metabolic residue to soil
        !               or surface SOM1 (kg[E]? ha)
        ! EFS1S2      E flow from soil or surface SOM1 to SOM2 (kg[E]? ha)
        ! EFS1S3      E flow from soil SOM1 to SOM3 (kg[E]? ha)
        ! EFS2S1      E flow from SOM2 to SOM1 (kg[E]? ha)
        ! EFS2S3      E flow from SOM2 to SOM3 (kg[E]? ha)
        ! EFS3S1      E flow from SOM3 to SOM1 (kg[E]? ha)
        ! EFSTRS1     E flow from soil or surface structural residue to soil or
        !               surface SOM1 (kg[E]? ha)
        ! EFSTRS2     E flow from soil or soil or surface structural residue to SOM2
        !               (kg[E]? ha)      
      
      real, dimension(:,:,:), allocatable ::  IMMMETS1, IMMS1S2, IMMS1S3
      real, dimension(:,:,:), allocatable ::  IMMS2S1, IMMS2S3, IMMS3S1
      real, dimension(:,:,:), allocatable ::  IMMSTRS1, IMMSTRS2
        ! IMMMETS1    Immobilization of E during the flow from soil or surface metabolic
        !               residue to soil or surface SOM1  (kg[E]? ha)
        ! IMMOB       Immobilization of E  (kg[E]? ha)
        ! IMMS1S2     Immobilization of E during the flow from soil or surface SOM1 to
        !               SOM2 (kg[E]? ha)
        ! IMMS1S3     Immobilization of E during the flow from SOM1 to SOM3 (kg[E]? ha)
        ! IMMS2S1     Immobilization of E during the flow from SOM2 to SOM1 (kg[E]? ha)
        ! IMMS2S3     Immobilization of E during the flow from SOM2 to SOM3 (kg[E]? ha)
        ! IMMS3S1     Immobilization of E during the flow from SOM3 to SOM1 (kg[E]? ha)
        ! IMMSTRS1    Immobilization of E during the flow from soil or surface structural
        !               residue to soil or surface SOM1  (kg[E]? ha)
        ! IMMSTRS2    Immobilization of E during the flow from soil or surface structural
        !               residue to SOM2  (kg[E]? ha)
        ! IMMSUMNET   Net immobilization of E by all flows in a layer (kg[E]? ha)      
      
      real, dimension(:,:,:), allocatable ::  MINERALIZE
      real, dimension(:,:,:), allocatable ::  MNRMETS1, MNRS1S2, MNRS1S3
      real, dimension(:,:,:), allocatable ::  MNRS2S1, MNRS2S3, MNRS3S1
      real, dimension(:,:,:), allocatable ::  MNRSTRS1, MNRSTRS2   
        ! MINERALIZE  Mineralization of E  (kg[E]? ha)
        ! MNRMETS1    Mineralization of E during the flow from soil or surface metabolic
        !               residue to soil or surface SOM1  (kg[E]? ha)
        ! MNRS1S2     Mineralization of E during the flow from SOM1 to SOM2 (kg[E]? ha)
        ! MNRS1S3     Mineralization of E during the flow from SOM1 to SOM3 (kg[E]? ha)
        ! MNRS2S1     Mineralization of E during the flow from SOM2 to SOM1 (kg[E]? ha)
        ! MNRS2S3     Mineralization of E during the flow from SOM2 to SOM3 (kg[E]? ha)
        ! MNRS3S1     Mineralization of E during the flow from SOM3 to SOM1 (kg[E]? ha)
        ! MNRSTRS1    Mineralization of E during the flow from soil or surface structural
        !               to soil or surface SOM1  (kg[E]? ha)
        ! MNRSTRS2    Mineralization of E during the flow from soil or surface structural
        !               residue to SOM2  (kg[E]? ha)   

!! By Zhang for C/N cycling
!!==============================



!!=====================Output============================={

        real, dimension (:,:), allocatable :: sol_soc
       !!----sub--------------
	    real, dimension (:), allocatable :: sub_n2o  
        real, dimension (:), allocatable :: sub_no           
        real, dimension (:), allocatable :: sub_nh4          
        real, dimension (:), allocatable :: sub_ch4
	    real, dimension (:), allocatable :: sub_dnit     
        real, dimension (:), allocatable :: sub_nit 
        real, dimension (:), allocatable :: sub_percno3              
        real, dimension (:), allocatable :: sub_upno3                 
        real, dimension (:), allocatable :: sub_ferno3              
        real, dimension (:), allocatable :: sub_fernh4             
        real, dimension (:), allocatable :: sub_ferorgn 
        real, dimension (:), allocatable :: orgn_fert 
        real, dimension (:), allocatable :: orgp_fert                    
        real, dimension (:), allocatable :: sub_rainno3            
        real, dimension (:), allocatable :: sub_fixn                  
        real, dimension (:), allocatable :: sub_solno3             
        real, dimension (:), allocatable :: sub_solnh3               
        real, dimension (:), allocatable :: sub_solorgn                 
        real, dimension (:), allocatable :: sub_solorgp                 
        real, dimension (:), allocatable :: sub_sedc
        real, dimension (:), allocatable :: sub_surfqc
        real, dimension (:), allocatable :: sub_latc
        real, dimension (:), allocatable :: sub_percc
        real, dimension (:), allocatable :: sub_NPPC
        real, dimension (:), allocatable :: sub_rspc
        real, dimension (:), allocatable :: sub_solorgc
        real, dimension (:), allocatable :: sub_solorgsc


             !!---HRU-----   
        real, dimension (:), allocatable :: rspc_dnew
        real, dimension (:,:), allocatable :: sol_RSPC1 
        real, dimension (:), allocatable :: OrgC_Plt2Rsd
        real, dimension (:), allocatable :: OrgN_Plt2Rsd
        real, dimension (:), allocatable :: OrgP_Plt2Rsd
        real, dimension (:), allocatable :: OrgC_Fer
		!!=====================Output=============================}

!! ===================GWC/NP=============={
      !!---------C---------
       real, dimension (:), allocatable :: shallst_doc_decay
       real, dimension (:), allocatable :: PerQB_DIC
       real, dimension (:), allocatable :: rchrg_dic
       real, dimension (:), allocatable :: shallst_dic
       real, dimension (:), allocatable :: GwQ_DIC 
       real, dimension (:), allocatable :: rchrg_doc
       real, dimension (:), allocatable :: shallst_doc
       real, dimension (:), allocatable :: revap_doc
       real, dimension (:), allocatable :: gwseep_doc
       real, dimension (:), allocatable :: revap_dic
       real, dimension (:), allocatable :: gwseep_dic
       real, dimension (:), allocatable :: hlife_doc
       !!---------N--------
       real, dimension (:), allocatable ::  rchrg_don
       real, dimension (:), allocatable ::  shallst_don
       real, dimension (:), allocatable ::  GwQ_DON
       real, dimension (:), allocatable ::  shallst_don_decay
       real, dimension (:), allocatable ::  revap_don
       real, dimension (:), allocatable ::  gwseep_don
       real, dimension (:), allocatable ::  revapn
       real, dimension (:), allocatable ::  gwseepn
       real, dimension (:), allocatable ::  gw_no3loss
      !! ===================GWC/NP==============}
      !\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
       !!=============Soil OrgC/NP================{
         !! -----------parameters----------
         real, dimension (:), allocatable :: part_DOC 
         real, dimension (:), allocatable :: er_POC
         real, dimension (:), allocatable :: peroc_DOC
         real, dimension (:), allocatable :: peroc_DIC
         !!----state variables------
         real, dimension (:), allocatable :: Sol_ApH
         real, dimension (:), allocatable :: Sol_pCO2  
         real, dimension (:), allocatable :: Sol_H
         real, dimension (:), allocatable :: solc_doc
         real, dimension (:), allocatable :: solc_dic
         real, dimension (:), allocatable :: solc_doc_ini
         real, dimension (:), allocatable :: solc_dic_ini
         real, dimension (:), allocatable :: bio_ms_ini
      !!-------Soil OrgC Fluxes------------    
        real, dimension (:), allocatable ::  Sed_RPOC
        real, dimension (:), allocatable ::  Sed_LPOC
        real, dimension (:,:), allocatable :: PerQ_DOC
        real, dimension (:,:), allocatable :: PerQ_DIC
        real, dimension (:,:), allocatable :: LatQ_DOC
        real, dimension (:,:), allocatable :: LatQ_DIC
        real, dimension (:,:), allocatable :: Sol_DOC
        real, dimension (:,:), allocatable :: Sol_DIC
       
        real, dimension (:), allocatable :: SurQ_DOC  
        real, dimension (:), allocatable :: GwQ_DOC
        real, dimension (:), allocatable :: SurQ_DIC
        real, dimension (:), allocatable :: LatQT_DIC
        real, dimension (:), allocatable :: LatQT_DOC
        real, dimension (:), allocatable :: HRU_LDOC
        real, dimension (:), allocatable :: HRU_RDOC
        real, dimension (:), allocatable :: HRU_DIC
        real, dimension (:), allocatable :: PerQB_DOC   
        real, dimension (:), allocatable :: HRU_CH4s    
        real, dimension (:), allocatable :: HRU_CH4g  
         
        real, dimension (:), allocatable :: SUB_RPOC       
        real, dimension (:), allocatable :: SUB_LPOC        
        real, dimension (:), allocatable :: SUB_RDOC     
        real, dimension (:), allocatable :: SUB_LDOC      
        real, dimension (:), allocatable :: SUB_DIC          
        real, dimension (:), allocatable :: SUB_CH4s
        real, dimension (:), allocatable :: SUB_CH4g    
  
       !!-------Soil OrgN Fluxes------------ 
       real, dimension (:), allocatable :: Sed_RPON 
       real, dimension (:), allocatable :: Sed_LPON 
       real, dimension (:), allocatable :: SurQ_DON 
       real, dimension (:), allocatable :: LatQT_DON 
       real, dimension (:,:), allocatable :: LatQ_DON                          
       real, dimension (:,:), allocatable :: PerQ_DON 
       real, dimension (:), allocatable ::  HRU_RDON
       real, dimension (:), allocatable ::  HRU_LDON
       real, dimension (:), allocatable ::  HRU_DIN
        
        real, dimension (:), allocatable :: SUB_RPON
        real, dimension (:), allocatable :: SUB_LPON
        real, dimension (:), allocatable :: SUB_RDON
        real, dimension (:), allocatable :: SUB_LDON
        real, dimension (:), allocatable :: SUB_DIN
        real, dimension (:), allocatable :: SUB_N2Os
        real, dimension (:), allocatable :: SUB_N2s

       !!-------Soil OrgP Fluxes------------ 
        real, dimension (:), allocatable :: HRU_DIP     
        real, dimension (:), allocatable :: Sed_RPOP 
        real, dimension (:), allocatable :: Sed_LPOP 
        real, dimension (:), allocatable :: HRU_RDOP
        real, dimension (:), allocatable :: HRU_LDOP
   
        real, dimension (:), allocatable :: SUB_RPOP
        real, dimension (:), allocatable :: SUB_LPOP
        real, dimension (:), allocatable :: SUB_RDOP
        real, dimension (:), allocatable :: SUB_LDOP
        real, dimension (:), allocatable :: SUB_DIP
       !!=============Soil OrgC/NP================}
  !!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
       !!=================HRU Output without Lag========={
        real, dimension (:), allocatable :: abs_f
        real:: qtile_0 
        real, dimension (:), allocatable :: sedgen 
        real, dimension (:), allocatable :: latq_0
        real, dimension (:), allocatable :: latno3_0
        real, dimension (:), allocatable :: tileno3_0
        real, dimension (:), allocatable :: sedyld_0
        real, dimension (:), allocatable :: sedorgn_0
        real, dimension (:), allocatable :: sedorgp_0
        real, dimension (:), allocatable :: surqno3_0
        real, dimension (:), allocatable :: surqsolp_0
        real, dimension (:), allocatable :: sedminpa_0
        real, dimension (:), allocatable :: sedminps_0
        real, dimension (:), allocatable :: solc_no3_ini
        real, dimension (:), allocatable :: solc_nh4_ini 
        real, dimension (:), allocatable :: solc_urea_ini
        real, dimension (:), allocatable :: solc_orgn_ini
	    real, dimension (:), allocatable :: solc_orgp_ini
	    real, dimension (:), allocatable :: solc_orgc_ini
        real, dimension (:), allocatable :: solc_orgcs_ini
        real, dimension (:), allocatable :: Sed_RPON_0
        real, dimension (:), allocatable :: Sed_LPON_0
        
        real, dimension (:), allocatable :: Sed_RPOP_0
        real, dimension (:), allocatable :: Sed_LPOP_0
        
        real, dimension (:), allocatable :: SurQ_DON_0
        real, dimension (:), allocatable :: LatQT_DON_0
        real, dimension (:), allocatable :: Sed_RPOC_0
        real, dimension (:), allocatable :: Sed_LPOC_0       
        real, dimension (:), allocatable :: SurQ_DOC_0
        real, dimension (:), allocatable :: LatQT_DOC_0
        real, dimension (:), allocatable :: SurQ_DIC_0 
        real, dimension (:), allocatable :: LatQT_DIC_0 
        real, dimension (:), allocatable :: shallst_doc_0
        real, dimension (:), allocatable :: shallst_dic_0
        real, dimension (:), allocatable :: solc_solp_ini
        real, dimension (:), allocatable :: solc_minp_ini
       !!=================HRU Output without Lag=========}

      real, dimension (:), allocatable :: IMMO_ERR   
      real, dimension (:), allocatable :: sol_BMC_In
      real, dimension (:), allocatable :: Sed_BMC
      real, dimension (:), allocatable :: nh4_rain  
      real, dimension(:), allocatable :: N2O_nit  
      real, dimension(:), allocatable :: NO_nit   
      real, dimension (:), allocatable :: grainN    
      real, dimension (:), allocatable :: solc_no3 
      real, dimension (:), allocatable :: solc_nh4 
      real, dimension (:), allocatable :: solc_urea
      real, dimension (:), allocatable :: solc_orgn 
      real, dimension (:), allocatable :: solc_orgc 
      real, dimension (:), allocatable :: solc_orgcs 
      real, dimension (:), allocatable :: solc_orgp 
      real, dimension (:), allocatable :: solc_solp  
      real, dimension (:), allocatable :: solc_minp 
      
         real, dimension (:), allocatable ::  iniorgn  
	   real, dimension (:), allocatable ::  inino3
         real, dimension (:), allocatable ::  ininh3            
	   real, dimension (:), allocatable ::  solc_orgn_fnl
	   real, dimension (:), allocatable ::  solc_no3_fnl
         real, dimension (:), allocatable ::  solc_nh4_fnl         
        
         real, dimension (:), allocatable ::  Min_ML_MBN           ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         real, dimension (:), allocatable ::  Min_SL_MBN                     !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         real, dimension (:), allocatable ::  Min_SL_SHN                   !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         real, dimension (:), allocatable ::  Min_MB_SHN                        !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         real, dimension (:), allocatable ::  Min_MB_PHN                        !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         real, dimension (:), allocatable ::  Min_SH_MBN                       !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         real, dimension (:), allocatable ::  Min_SH_PHN                         !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         real, dimension (:), allocatable ::  Min_PH_MBN                           !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                                
         real, dimension (:), allocatable ::  IMM_ML_MBN                    ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         real, dimension (:), allocatable ::  IMM_SL_MBN                        ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         real, dimension (:), allocatable ::  IMM_SL_SHN                       ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         real, dimension (:), allocatable ::  IMM_MB_SHN                      ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         real, dimension (:), allocatable ::  IMM_MB_PHN                       ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         real, dimension (:), allocatable ::  IMM_SH_MBN                          !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         real, dimension (:), allocatable ::  IMM_SH_PHN                            ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         real, dimension (:), allocatable ::  IMM_PH_MBN                        ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
    
        integer:: basnh3i   
        real :: hru_haFor,  hru_haAgr,  hru_haGra,  hru_haWat 
        real::  hru_haAgr1	
        real, dimension (:), allocatable :: tillage_emix
    
         real,dimension(:,:),allocatable::R_LSCTP
         real,dimension(:,:),allocatable::R_LMCTP
         real,dimension(:,:),allocatable::R_BMCTP
         real,dimension(:,:),allocatable::R_HSCTP
         real,dimension(:,:),allocatable::R_HPCTP
         real,dimension(:,:),allocatable::CMF
         real,dimension(:,:),allocatable::WATF 
         real,dimension(:,:),allocatable::TEMF
         real,dimension(:,:),allocatable::OXGF
         real,dimension(:,:),allocatable::RANF
         real,dimension(:,:),allocatable::TILLF
         real,dimension(:,:),allocatable::LSCTA 
         real,dimension(:,:),allocatable::LSLCTA 
         real,dimension(:,:),allocatable::LSLnCTA 
         real,dimension(:,:),allocatable::LMCTA
         real,dimension(:,:),allocatable::BMCTA
         real,dimension(:,:),allocatable::HSCTA
         real,dimension(:,:),allocatable::HPCTA
         real,dimension(:,:),allocatable::LSNTA
         real,dimension(:,:),allocatable::LMNTA 
         real,dimension(:,:),allocatable::BMNTA
         real,dimension(:,:),allocatable::HSNTA
         real,dimension(:,:),allocatable::HPNTA
         real,dimension(:,:),allocatable::LSCTP
         real,dimension(:,:),allocatable::LSLCTP
         real,dimension(:,:),allocatable::LSLnCTP
         real,dimension(:,:),allocatable::LMCTP
         real,dimension(:,:),allocatable::BMCTP   
         real,dimension(:,:),allocatable::HSCTP
         real,dimension(:,:),allocatable::HPCTP
         real,dimension(:,:),allocatable::LSNTP
         real,dimension(:,:),allocatable::LMNTP
         real,dimension(:,:),allocatable::BMNTP
         real,dimension(:,:),allocatable::HSNTP
         real,dimension(:,:),allocatable::HPNTP
           
        integer, dimension (:), allocatable ::  Inhibday,Inhibdu
        integer :: idc_till,idc_sw,idc_tmp 
       


    
      !!============== GHG simulations=========================={
       !!----parameter------
      real, dimension (:), allocatable ::  turnovfr_nitn2o
      real, dimension (:), allocatable :: MaxRate
      real, dimension (:), allocatable :: N2Oadjust_wp  
      real, dimension (:), allocatable :: N2Oadjust_fc  
      real, dimension (:), allocatable :: min_nitrate
      real, dimension (:), allocatable :: wfpsdnitadj 
      real, dimension (:), allocatable :: combined_factor
      real, dimension (:), allocatable :: temp_factor
      real, dimension (:), allocatable :: water_factor
      real, dimension (:), allocatable :: oxygen_factor
      real, dimension (:,:), allocatable ::  Rn2_n2o
      real, dimension (:,:), allocatable :: Rno_n2o
      !!----fluxes-----
      real, dimension (:), allocatable :: CH4
      real, dimension (:), allocatable :: N2O
      real, dimension (:), allocatable :: N2O_den
      real, dimension (:), allocatable :: NO
      real, dimension (:), allocatable :: NO_den
      real, dimension (:), allocatable :: N2_den
      real, dimension (:), allocatable :: cal_temp 
      !!----state variable----
      real, dimension (:), allocatable :: co2con
	  real, dimension (:,:), allocatable :: sol_C
	  real, dimension (:), allocatable :: porespace_tot
      real, dimension (:), allocatable :: sol_wpmm_tot
      real, dimension (:), allocatable :: WFPS_ave
      real, dimension (:), allocatable :: st_ave
      real, dimension (:), allocatable :: ph_ave
	  real, dimension (:,:), allocatable :: sol_clayNH4
	  real, dimension (:,:), allocatable :: sol_soluNH4
	  real, dimension (:,:), allocatable :: sol_totNH4
      real, dimension (:,:), allocatable :: CEC
	!!============== GHG simulations==========================}
	
     real, dimension (:,:), allocatable :: sol_tex
        

      

!Miscellaneous
      real :: er_POC_para, CFB_para, Sf_para_sur, Sf_para_sub
            ! 1.5       !er_POC_para           ! 0.0-5.0          !POC enrichment ratio      ! 0-10       MOST SENSITIVE  
            !0.42       !CFB_para		!Cf	=	Carbon fraction of residue (0.42; from data of Pinck et al., 1950) 
            !0.05       !Sf_para_sur	=	Fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter
            !0.10       !Sf_para_sub	=	Fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter
      real :: peroc_DIC_para, ABL_para, peroc_DOC_para, part_DOC_para, hlife_doc_para
!Dissovled carbon
      !Calculated       !ABL_para 	=	Carbon allocation from Microbial Biomass to Leaching.			
            !0.95       !peroc_DIC_para	!! 0-1  DIC percolation coefficient
            !0.70       !peroc_DOC_para	!! 0-1  DOC percolation coefficient
            !4000       !part_DOC_para        !! 500-2000 !organic carbon partition coefficient,basin sacle parameter,1000 to 1200		!!replacing KOC 	=	Liquid-solid partition coefficient for Microbial Biomass (10^3 m3 Mg-1)
            !50.0       !hlife_doc_para	!! 0-100     !DOC half life (days) in groudwater,calculating DOC decay in groundwater 
!Allocation of CO2 and Carbon transformation
      real :: ABCO2_para_sur, ABCO2_para_sub, ABP_para_sur, ABP_para_sub, ALMCO2_para_sur, ALMCO2_para_sub, ALSLNCO2_para_sur
      real :: ALSLNCO2_para_sub, ASP_para_sur, ASP_para_sub, ALSLCO2_para, APCO2_para, ASCO2_para
	     !0.6       !ABCO2_para_sur ABCO2	=	Allocation from Microbial Biomass C pool to CO2; 0.6 (surface Litter), 0.85 - 0.68 × (CLAY+SILT) (all other layers) (Parton et al., 1993, 1994)
      !Calculated       !ABCO2_para_sub ABCO2	=	Allocation from Microbial Biomass C pool to CO2; 0.6 (surface Litter), 0.85 - 0.68 × (CLAY+SILT) (all other layers) (Parton et al., 1993, 1994)
	     !0.0       !ABP_para_sur	=	Allocation from Biomass to passive Humus; 0 (surface Litter), 0.003 + 0.032 x SOL_CLAY (all other layers) (Parton et al., 1993, 1994)
      !Calculated       !ABP_para_sub	=	Allocation from Biomass to passive Humus; 0 (surface Litter), 0.003 + 0.032 x SOL_CLAY (all other layers) (Parton et al., 1993, 1994)
             !0.6       !ALMCO2_para_sur	=	Allocation from metabolic Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
            !0.55       !ALMCO2_para_sub	=	Allocation from metabolic Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
             !0.6       !ALSLNCO2_para_sur	=	Allocation from non-lignin of structural Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
            !0.55       !ALSLNCO2_para_sub	=	Allocation from non-lignin of structural Litter to CO2; 0.6 (surface Litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
             !0.0       !ASP_para_sur	=	Allocation from slow Humus to passive; 0 (surface Litter), 0.003 + 0.00009 × CLAF (all other layers) (Parton et al., 1993, 1994)
      !Calculated       !ASP_para_sub	=	Allocation from slow Humus to passive; 0 (surface Litter), 0.003 + 0.00009 × CLAF (all other layers) (Parton et al., 1993, 1994)
             !0.3       !ALSLCO2_para	=	Allocation from lignin of structural Litter to CO2; 0.3 (Parton et al., 1993, 1994)
            !0.55       !APCO2_para	=	Allocation from passive Humus to CO2; 0.55 (Parton et al., 1993, 1994)
            !0.55       !ASCO2_para	=	Allocation from slow Humus to CO2; 0.55 (Parton et al., 1993, 1994)
!decomposition rates
      real :: PRMT_51_para, PRMT_45_para, BMR_para_sur, BMR_para_sub, HPR_para, HSR_para, LMR_para_sur, LMR_para_sub, LSR_para_sur, LSR_para_sub
             !1.0       PRMT_51_para   !COEF ADJUSTS MICROBIAL ACTIVITY FUNCTION IN TOP SOIL LAYER (0.1_1.),
           !0.003       PRMT_45_para       !COEF IN CENTURY EQ ALLOCATING SLOW TO PASSIVE HUMUS(0.001_0.05) ORIGINAL VALUE = 0.003, ASP=MAX(.001,PRMT_45-.00009*sol_clay(k,j)), ASP=MAX(.001,PRMT_45+.009*sol_clay(k,j)/100)
	  !0.0164       !BMR_para_sur 	=	Rate of transformation of microbial Biomass and associated products under optimal conditions (surface = 0.0164 d-1; all other layers = 0.02 d-1) (Parton et al., 1993, 1994)
	    !0.02       !BMR_para_sub 	=	Rate of transformation of microbial Biomass and associated products under optimal conditions (surface = 0.0164 d-1; all other layers = 0.02 d-1) (Parton et al., 1993, 1994)
        !0.000012       !HPR_para 	=	Rate of transformation of passive Humus under optimal conditions (subsurface layers = 0.000012 d-1) (Parton et al., 1993, 1994)
        !0.000548       !HSR_para 	=	Rate of transformation of slow Humus under optimal conditions (all layers = 0.0005 d-1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
          !0.0405       !LMR_para_sur	=	Rate of transformation of metabolic Litter under optimal conditions (surface = 0.0405 d-1; all other layers = 0.0507 d-1) (Parton et al., 1994)
          !0.0507       !LMR_para_sub	=	Rate of transformation of metabolic Litter under optimal conditions (surface = 0.0405 d-1; all other layers = 0.0507 d-1) (Parton et al., 1994)
          !0.0107       !LSR_para_sur	=	Rate of potential transformation of structural Litter under optimal conditions (surface = 0.0107 d-1; all other layers = 0.0132 d-1) (Parton et al., 1994)
          !0.0132       !LSR_para_sub	=	Rate of potential transformation of structural Litter under optimal conditions (surface = 0.0107 d-1; all other layers = 0.0132 d-1) (Parton et al., 1994)
!Soil texutre controls of microbioa activity
      real :: XBM_para_sur, XBM_para_sub, XLSLF_para
             !1.0       !XBM_para_sur 	=	Control on transformation of microbial Biomass by soil texture and structure. Its values: surface Litter layer = 1; all other layers = 1 – 0.75 × (SILT + CLAY) (Parton et al., 1993, 1994)
      !Calculated       !XBM_para_sub 	=	Control on transformation of microbial Biomass by soil texture and structure. Its values: surface Litter layer = 1; all other layers = 1 – 0.75 × (SILT + CLAY) (Parton et al., 1993, 1994)
      !Calculated       !XLSLF_para	=	Control on potential transformation of structural Litter by lignin fraction of structural Litter [XLSLF = exp(-3 × LSLF) (Parton et al., 1993, 1994)]
!Oxygen factor control parameters
       real :: OX_aa_para, OX_bb_para
            !10.0       !OX_aa_para 	=	Control on transformation of microbial Biomass by soil texture and structure. Its values: surface Litter layer = 1; all other layers = 1 – 0.75 × (SILT + CLAY) (Parton et al., 1993, 1994)
           !0.035       !OX_bb_para 	=	Control on transformation of microbial Biomass by soil texture and structure. Its values: surface Litter layer = 1; all other layers = 1 – 0.75 × (SILT + CLAY) (Parton et al., 1993, 1994)
!!Other Derived
                       !ASX=1.-ASCO2-ASP 		!ASX: Slow to Microbial Biomass
                       !(1.-ABP-ABCO2)*NCHS     	!Biomass to Slow
                       !APX=1.-APCO2			!APX: Passive to Microbial Biomass
                       !(1.0 - ALMCO2)			!Metabolic to S1 (Biomass)
		               !LMF	=	Fraction of the Litter that is metabolic 
                       !LMNF	=	Fraction of metabolic Litter that is N (kg kg-1)
                       !LSF	=	Fraction of the Litter that is structural 
                       !LSLF	=	Fraction of structural Litter that is lignin (kg kg-1)  
                       !LSNF	=	Fraction of structural Litter that is N (kg kg-1)
                       !NCBM 	=	N/C ratio of newly formed Microbial Biomass
                       !NCHP 	=	N/C ratio of newly formed passive Humus
                       !NCHS 	= 	N/C ratio of newly formed slow Humus
        real :: tf_nit
 

     end module parm_subC