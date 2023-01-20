      subroutine sim_initday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initialized arrays at the beginning of the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i_mo        |none          |current month being simulated
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dayl(:)     |hr            |day length for the day in HRU
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    frad(:,:)   |none          |fraction of solar radiation occuring during 
!!                               |hour in day in HRU
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
!!    mo_chk      |none          |check for month being simulated; when mo_chk
!!                               |differs from mo, monthly output is printed
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto 
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |pesticide loading from HRU in the water phase
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    rchdy(:,:)  |varies        |daily reach output array
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    hrupstd(:,:,:)|varies      |HRU daily pesticide output array
!!    sol_cnsw(:) |mm H2O        |amount of water stored in soil profile on day
!!                               |(value used to calculated CN number for day)
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    strsw(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |water stress
!!    sub_bactlp(:)|# bact/ha     |less persistent bacteria loading on day
!!                               |from subbasin
!!    sub_bactp(:)|# bact/ha     |persistent bacteria loading on day from 
!!                               |subbasin
!!    sub_bd(:)   |Mg/m^3        |average bulk density for subbasin
!!    sub_cbod(:) |kg cbod       |carbonaceous biological oxygen demand loading
!!                               |on day from subbasin
!!    sub_chl(:)  |kg chl-a      |chlorophyll-a loading on day from subbasin
!!    sub_dox(:)  |kg O2         |dissolved oxygen loading on day from subbasin
!!    sub_etday(:)|mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_gwq(:)  |mm H2O        |groundwater loading on day in subbasin
!!    sub_latno3(:)|kg N/ha       |NO3 in lateral flow on day in subbasin
!!    sub_no3(:)  |kg N/ha       |NO3 in surface runoff on day in subbasin
!!    sub_orgn(:) |kg N/ha       |organic nitrogen in soil of subbasin
!!    sub_orgp(:) |kg P/ha       |organic phosphorus in soil of subbasin
!!    sub_precip(:)|mm H2O        |water reaching ground surface on day in 
!!                               |subbasin
!!    sub_pst(:,:)|kg/ha         |pesticide in soil of subbasin
!!    sub_qd(:)   |mm H2O        |surface runoff loading on day in subbasin
!!    sub_sedy(:) |metric tons   |sediment loading on day from subbasin
!!    sub_sep(:)  |mm H2O        |percolation out of soil profile on day in 
!!                               |subbasin
!!    sub_snom(:) |mm H2O        |snow melt for day in subbasin
!!    sub_solp(:) |kg P/ha       |soluble P in surface runoff on day in subbasin
!!    sub_solpst(:)|mg pst        |soluble pesticide loading on day in subbasin
!!    sub_sorpst(:)|mg pst        |sorbed pesticide loading on day in subbasin
!!    sub_subp(:) |mm H2O        |precipitation for day in subbasin
!!    sub_sumfc(:)|mm H2O        |amount of water in soil at field capacity in
!!                               |subbasin
!!    sub_surfq(:)|mm H2O        |surface runoff generated on day in subbasin
!!    sub_sw(:)   |mm H2O        |amount of water in soil on day in subbasin
!!    sub_tileno3 |kg N/ha       |NO3 in tile flow on day in subbasin       
!!    sub_tran(:) |mm H2O        |transmission losses on day in subbasin
!!    sub_wyld(:) |mm H2O        |water yield on day in subbasin
!!    sub_yorgn(:)|kg N/ha       |organic N loading on day in subbasin
!!    sub_yorgp(:)|kg P/ha       |organic P loading on day in subbasin
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)    |deg C         |average temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    wcklsp(:)   |
!!    wpstdayo(:,:)|varies        |watershed daily pesticide output array
!!    wshddayo(:) |varies        |watershed daily output array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para    ! Added by Du
      use parm
       use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      real, dimension(:), allocatable ::  subtilep           !!R683 1/13/22 nbs

      !!initialize variables at beginning of day
      tilep = 0.                                !!R683 1/13/22 nbs
      subtilep = 0.                             !!R683 1/13/22 nbs
      cbodu = 0.
      chl_a = 0.
      cnday = 0.
      dayl = 0.
      doxq = 0.
      drift = 0.
      flat = 0.
      frad = 0.
!      gwq_ru = 0.
      hru_ra = 0.
      hru_rmx = 0.
      hrupstd = 0.
	irr_flag = 0
      latno3 = 0.
      latq = 0.
      minpgw = 0.
      no3gw = 0.
      nplnt = 0.
      pcpband = 0.
      percn = 0.
      petmeas = 0.
      potsa = 0.
      pplnt = 0.
      pst_sed = 0.
      pst_surq = 0.
      qdr = 0.
      rainsub = 0.
      rchdy = 0.
      rchrg_src = 0.    !CB 8/24/09
      rhd = 0.
      sedminpa = 0.
      sedminps = 0.
      sedorgn = 0.
      sedorgp = 0.

      sedyld = 0.
      sanyld = 0.
      silyld = 0.
      clayld = 0.
      sagyld = 0.
      lagyld = 0.
      grayld = 0.

      sepbtm = 0.
      sol_cnsw = 0.
      sol_prk = 0.
      strsa = 1.
      strsn = 1.
      strsp = 1.
      strstmp = 1.
!!  NUBZ    strsw = 1.
      sub_bactlp = 0.
      sub_bactp = 0.
      sub_bd = 0.
      sub_cbod = 0.
      sub_chl = 0.
      sub_dox = 0.
      sub_etday = 0.
      sub_gwno3 = 0.
      sub_gwsolp = 0.
      sub_gwq = 0.
      sub_hhqd = 0.
      sub_hhwtmp = 0.
      sub_latno3 = 0.
      sub_latq = 0.
      sub_tileq = 0.        !Added after !!1/19/22 nbs
      sub_minp = 0.
      sub_minpa = 0.
      sub_minps = 0.
      sub_no3 = 0.
      sub_orgn = 0.
      sub_orgp = 0.
      sub_pet = 0.
      sub_precip = 0.
      sub_pst = 0.
      sub_qd = 0.
      sub_sedpa = 0.
      sub_sedps = 0.

      sub_sedy = 0.
	sub_dsan = 0.
	sub_dsil = 0.
	sub_dcla = 0.
	sub_dsag = 0.
	sub_dlag = 0.
	!sub_dgra = 0.

      sub_sep = 0.
      sub_snom = 0.
      sub_solp = 0.
      sub_solpst = 0.
      sub_sorpst = 0.
      sub_subp = 0.
      sub_sumfc = 0.
      sub_surfq = 0.
      sub_sw = 0.
      sub_tileno3 = 0.
      sub_tilep = 0.                !Added after !!1/13/22 nbs
      sub_vaptile = 0.   !!R664 02/15/17  nbs
      sub_tran = 0.
      sub_wtmp = 0.
      sub_wyld = 0.
      sub_gwq_d = 0.
      sub_yorgn = 0.
      sub_yorgp = 0.
      subp = 0.
      surfq = 0.
      surqno3 = 0.
      surqsolp = 0.
      tavband = 0.
      tileno3 = 0.    !CB 8/24/09
      tmn = 0.
      tmnband = 0.
      tmpav = 0.
      tmx = 0.
      tmxband = 0.
      u10 = 0.
      wcklsp = 0.
      wpstdayo = 0.
      wshddayo = 0.
      wshddayN = 0.   
      mo_chk = i_mo
!----------------------------------------------------        
! added by J.Jeong for urban modeling 4/29/2008
      ubnrunoff = 0.
      ubntss = 0.
      sub_ubnrunoff = 0.
      sub_ubntss = 0.
      latq = 0.
      sub_subp_dt = 0.
      sub_hhsedy = 0.
      sub_atmp = 0.
	rchhr = 0.
!-----------------------------------------------------        
      sub_sur_tmp = 0.
      sub_soltmp_50 = 0.
      sub_soltmp_100 = 0.
      sub_soltmp_150 = 0.
      sub_soltmp_200 = 0.
      sub_soltmp_300 = 0.
      sub_soltmp_500 = 0.
      sub_soltmp_1000 = 0.
      soltmp_50=0.
      soltmp_100=0.
      soltmp_150=0.
      soltmp_200 =0.
      soltmp_300 =0.
      soltmp_500 =0.
      soltmp_1000 =0. 
      swc_50=0.
      swc_100=0.
      swc_150=0.
      swc_200 =0.
      swc_300 =0.
      swc_500 =0.
      swc_1000=0. 


      !!add by zhang
      !!==========================
        sedc_d = 0.
        surfqc_d =0.
        latc_d = 0.
        percc_d = 0.
        NPPC_d = 0.
        GPPC_d = 0.
        C_PROD = 0.
        GR_d = 0.
        MR_d = 0.
        ctowoodc1 = 0.
        ctowoodc2 = 0.
        ctowoodc3 = 0.

        wood1_SOM1 = 0.
        wood1_SOM2 = 0.
        wood2_SOM1 = 0.
        wood2_SOM2 = 0.
        wood3_SOM1 = 0.
        wood3_SOM2 = 0.

        eavail=0.
        denitrify = 0.
        nh4_vol = 0.
        
        
      !no3loss = 0.
      no3_loss_pothole = 0.
      absorbed_no3 = 0.
      absorbed_nh3 = 0.
      nh4_fert = 0.
      no3_fert = 0.
      solp_fert = 0.
      no3_autof=0.
      nh4_autof= 0.
      solp_autof =0.
      orgn_autof =0.
      orgp_autof =0.
          orgn_grazf=0.    
          nh3_grazf =0.    
          no3_grazf=0.     
          solp_grazf=0.    
          orgp_grazf=0.    
      no3_conf = 0.
      nh4_conf = 0.
      orgn_conf = 0.
      orgp_conf = 0.
      solp_conf = 0.
      
      no3_up = 0.
      no3_immo=0.
      nh4_immo=0.
      immo_err1=0.
      nh4_min=0.
      
      nh4_min_ly=0.
      no3_immo_ly=0.
      nh4_immo_ly=0.   
            
      solp_min=0.
      sol_solp2minp=0.
      solp_up =0.
      no3_min=0.
      no3_denit=0.
      no3_nitr=0.

      no3_perc=0.
      no3_surf=0.
      no3_lat= 0.
      
      no3_rain=0.
      no3_adsortion_parti=0.

	  leafdiec=0.
	  frootjdiec=0.
	  frootmdiec=0.        
        rsdc_d = 0. 
        rsdn_d = 0.
        grainc_d = 0.
        stoverc_d = 0.
        emitc_d = 0.
        soc_d = 0.  
        rspc_d = 0.   

	   sub_sedc_d =0.
	sub_surfqc_d=0.
	sub_latc_d=0.
	sub_percc_d=0.
	!sub_NEPC_d=0.
	sub_rsdc_d=0.
	sub_grainc_d=0.
	!sub_stover_c_d=0.
	!sub_emit_c_d=0.
	sub_soc_d	=0.
	sub_rspc_d =0.

      erosion= 0.
      usle_sed = 0.
      ccp = 0.
	
      !!add by zhang
      !!==========================
      !f_gpp = 0.
      !f_pgpp = 0.
      !f_npp = 0.
      !f_mr = 0.
      !f_gr = 0.  
      cal_temp =0.
      !pwther = 0.
      mfprd = 0.
      tree_cfrac = 0.

 
      !!>>>>>>>>>> qichun>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CH4 = 0.
      N2O = 0.
      NO = 0.
      N2O_den = 0.
      NO_den = 0.
      cal_temp =0.
      !!<<<<<<<<<<<qichun<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

!-----------------added by Du, initialize the value of new carbon transport variables to zero------------!
	N2_den=0.
	porespace_tot =0.
      sol_wpmm_tot =0.
      
	 sub_n2o =0.     !! g/ha day
       sub_no= 0.           !! g/ha day
       sub_nh4 = 0.             !!kg N/ha 
       sub_ch4= 0.
	
	 sub_dnit=0.        !!kg N/ha 
       sub_nit=0.
	 sub_percno3=0.
       sub_upno3=0.
       sub_ferno3=0.
       sub_fernh4=0.
       sub_ferorgn=0.
       sub_rainno3=0.
       sub_fixn=0.
       sub_solno3=0.
       sub_solnh3=0.
       sub_solorgn=0.
       sub_solorgp=0.
       sub_sedc=0.
       sub_surfqc=0.
       sub_latc=0.
       sub_percc=0.
       sub_NPPC=0.
       sub_rspc=0.
       sub_solorgc =0.
       sub_solorgsc=0.
       sub_snodep=0.
       sub_snohru=0.
       sub_snofall=0.
       sub_frozday=0.
       orgn_fert=0.
       orgp_fert=0.
       sub_airtmp=0.
       sub_swc=0.
      
      solc_doc=0.
      solc_dic=0.

      sub_hwyld=0.
      
      sub_wtmp_surq=0.
      sub_wtmp_latq=0.
      sub_wtmp_gwq=0.
      sub_wtmp_gwdp=0.
      wtmp_surq=0.
      wtmp_latq=0.
      wtmp_gwq=0.
      latq_tot=0.
      sub_gwq_deep=0.
      
      rchhr2 = 0.
  
      !!---------------------------------------------------
      sedgen=0.
  
      
      rspc_dnew=0.
      sol_RSPC=0.
      sol_RSPC1=0.
      
     
      Sed_RPOC =0.
      Sed_LPOC =0.
      PerQ_DOC =0.
      PerQ_DIC =0.
      LatQ_DOC =0.
      LatQ_DIC =0.
      SurQ_DOC =0.
 
      SurQ_DIC=0.
      
      
      GwQ_DOC=0.
      GwQ_DIC=0.
      LatQT_DIC=0.
      LatQT_DOC=0.
      HRU_LDOC=0.
      HRU_RDOC=0.
      HRU_DIC=0.
      PerQB_DOC=0.
      PerQB_DIC=0.
     
      HRU_CH4s=0.
      HRU_CH4g=0.
      
      SUB_RPOC = 0.    !RPOC amount in subbasin for routing (kg)
      SUB_LPOC = 0.     !LPOC amount in subbasin for routing (kg)
      SUB_RDOC = 0.    !RDOC amount in subbasin for routing (kg)
      SUB_LDOC = 0.    !LDOC amount in subbasin for routing (kg)
      SUB_DIC = 0.       !DIC amount  in subbasin for routing (kg) 
        
      SUB_CH4s = 0. 
      SUB_CH4g = 0.
      SUB_RPON = 0. 
      SUB_LPON = 0. 
      SUB_RDON = 0. 
      SUB_LDON = 0. 
      SUB_DIN = 0.
      SUB_N2Os = 0.
      SUB_N2s = 0.
      
      SUB_RPOP = 0. 
      SUB_LPOP = 0. 
      SUB_RDOP = 0. 
      SUB_LDOP = 0. 
      SUB_DIP = 0.
      
      Sed_RPON = 0.
      Sed_LPON = 0.
      SurQ_DON = 0.
      LatQT_DON = 0.
      LatQ_DON = 0.                           
      PerQ_DON = 0.
      HRU_RDON = 0.
      HRU_LDON = 0.
      HRU_DIN = 0.
      
      HRU_DIP = 0.
      Sed_RPOP = 0.
      Sed_LPOP = 0.
      HRU_RDOP= 0.
      HRU_LDOP = 0.
      
       rchrg_don = 0.
       GwQ_DON = 0.
       shallst_don_decay = 0.
       revap_don = 0.
       gwseep_don = 0.            
       revap_doc = 0.
       gwseep_doc = 0.
       shallst_doc_decay = 0.
       revapn = 0.
       gwseepn = 0.
       revap_dic = 0.
       gwseep_dic = 0.
     
       gw_no3loss = 0.
       OrgC_Plt2Rsd = 0.
       OrgN_Plt2Rsd = 0.
       OrgP_Plt2Rsd = 0.
       OrgC_Fer = 0.
      !!=============================
        qtile_0 = 0.
	latq_0 = 0.
        latno3_0 = 0.
        tileno3_0 = 0.
        sedyld_0 = 0.
        sedorgn_0 = 0.
        sedorgp_0 =0.
        surqno3_0 =0.
        surqsolp_0 =0.
        sedminpa_0 =0.
        sedminps_0 =0.
        solc_no3_ini =0.
        solc_nh4_ini =0.
        solc_urea_ini =0.
        solc_orgn_ini =0.
        solc_orgp_ini =0.
        solc_orgc_ini =0.
        solc_orgcs_ini =0.
        SurQ_DON_0 =0.
        LatQT_DON_0 =0.
        Sed_RPON_0 =0.
        Sed_LPON_0 =0.
        
        Sed_RPOP_0 =0.
        Sed_LPOP_0 =0.        
        
        SurQ_DON_0 =0.
        LatQT_DON_0 =0.
        Sed_RPOC_0 =0.
        Sed_LPOC_0 =0.      
        SurQ_DOC_0 =0.
        LatQT_DOC_0 =0.
        SurQ_DIC_0 =0.
        LatQT_DIC_0 =0.
        solc_doc_ini =0.
        solc_dic_ini =0.
        bio_ms_ini =0.
        shallst_doc_0=0.
        shallst_dic_0=0.
        solc_solp_ini=0.
        solc_minp_ini=0.
      
 
 
       !frad_rch = 0.           
       IMMO_ERR = 0.
       sol_BMC_In = 0.
       Sed_BMC = 0.
       nh4_rain = 0.
       N2O_nit = 0.
       NO_nit = 0.
       grainN = 0.
       sol_soc = 0.
       solc_no3 = 0.
       solc_nh4 = 0.
       solc_urea = 0.
       solc_orgn = 0.
       solc_orgc = 0.
       solc_orgcs = 0.
       solc_orgp = 0.
       solc_solp = 0.
       solc_minp = 0. 
          
            
        
        
          Min_ML_MBN =0.                ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
          Min_SL_MBN =0.                    !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
          Min_SL_SHN  =0.                 !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
          Min_MB_SHN =0.                       !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
          Min_MB_PHN =0.                       !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
          Min_SH_MBN =0.                      !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
          Min_SH_PHN =0.                        !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
          Min_PH_MBN  =0.                         !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                                
          IMM_ML_MBN =0.                    ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
          IMM_SL_MBN =0.                        ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
          IMM_SL_SHN =0.                      ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
          IMM_MB_SHN =0.                     ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
          IMM_MB_PHN =0.                      ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
          IMM_SH_MBN =0.                         !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
          IMM_SH_PHN =0.                            ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
          IMM_PH_MBN =0.                     ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                   
          hru_haAgr1=0.
	    hru_haFor=0.
	    hru_haAgr=0.
	    hru_haGra=0.
	    hru_haWat=0. 
       
          R_LSCTP =0.
          R_LMCTP =0.
          R_BMCTP =0.
          R_HSCTP =0.
          R_HPCTP =0. 
           CMF=0.
           WATF =0.
           TEMF=0.
           OXGF=0.
           RANF=0.
        
           LSCTA =0.
           LSLCTA =0.
           LSLnCTA =0.
           LMCTA=0.
           BMCTA=0.
           HSCTA=0.
           HPCTA=0.
           LSNTA=0.
           LMNTA =0.
           BMNTA=0.
           HSNTA=0.
           HPNTA=0.
           LSCTP=0.
           LSLCTP=0.
           LSLnCTP=0.
           LMCTP=0.
           BMCTP =0.  
           HSCTP=0.
           HPCTP=0.
           LSNTP=0.
           LMNTP=0.
           BMNTP=0.
           HSNTP=0.
           HPNTP =0.
           
         !  defac = 0.
 
       CFPltSTR = 0.
       CFPltMET = 0.
       CFOrfSTR = 0.
       CFOrfMET = 0.
       CBurntSTR = 0.
       CBurntMET = 0.
        
       NFPltSTR = 0.
       NFPltMET = 0.
       NFOrfSTR = 0.
       NFOrfMET = 0.
       NBurntSTR = 0.
       NBurntMET = 0.   
            
       iniorgn = 0.     
	 inino3 = 0.  
       ininh3 = 0.  
       solc_orgn_fnl = 0.     
	 solc_no3_fnl = 0.  
       solc_nh4_fnl = 0.  
       WFPS_ave = 0.
       st_ave = 0.
       ph_ave = 0.


!         Set all the carbon and nitrogen flows to zero. 
          CFMETS1 = 0.
          CFSTRS1 = 0.
          CFSTRS2 = 0.
          
          !MNRMETS1(:,1,j) = 0.              !!NBS/JGA debug
          !MNRSTRS1(:,1,j) = 0.              !!NBS/JGA debug
          !MNRSTRS2(:,1,j) = 0.              !!NBS/JGA debug
          
          !DO IEL = 1, NELEM
            EFMETS1 = 0.
            EFSTRS1 = 0.
            EFSTRS2 = 0. 
            IMMMETS1 = 0.
            IMMSTRS1 = 0.
            IMMSTRS2 = 0. 
            MNRMETS1 = 0.
            MNRSTRS1 = 0.
            MNRSTRS2 = 0.
          !END DO
          CO2FMET = 0.
          CO2FSTR = 0.
          CO2FSTR = 0.

!         Set all the soil flows to zero.
          CFS1S2 = 0.
          CFS1S3 = 0.
          CFS2S1 = 0.
          CFS2S3 = 0.
          CFS3S1 = 0.
          !DO IEL = 1, NELEM
            EFS1S2 = 0.
            EFS1S3 = 0.
            EFS2S1 = 0.
            EFS2S3 = 0.
            EFS3S1 = 0. 
            IMMS1S2 = 0.
            IMMS1S3 = 0.
            IMMS2S1 = 0.
            IMMS2S3 = 0.
            IMMS3S1 = 0.
            MNRS1S2 = 0.
            MNRS1S3 = 0.
            MNRS2S1 = 0.
            MNRS2S3 = 0.
            MNRS3S1 = 0.
          !END DO
          CO2FS1 = 0.
          CO2FS2 = 0.
          CO2FS3 = 0.


      yield = 0.
      yieldgrn = 0.
      yieldbms = 0.
      yieldtbr = 0.
      yieldn = 0.
      yieldp = 0.
      yieldrsd = 0.

       
      return
      end