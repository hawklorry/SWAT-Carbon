      subroutine allocate_parms_subC
      use parm 
      use parm_subC
      implicit none
      
       !! By Zhang for C/N cycling
      !! ============================
	 !allocate(sol_PH(mlyr,mhru))
	 allocate(sol_CAC(mlyr,mhru)) 
	 allocate(sol_CEC(mlyr,mhru)) 
	
	 allocate(sol_BMC(mlyr+1,mhru)) 
	 allocate(sol_BMN(mlyr+1,mhru)) 
	 allocate(sol_HSC(mlyr+1,mhru)) 
	 allocate(sol_HSN(mlyr+1,mhru)) 
	 allocate(sol_HPC(mlyr+1,mhru)) 
	 allocate(sol_HPN(mlyr+1,mhru)) 
	 allocate(sol_LM(mlyr+1,mhru)) 
	 allocate(sol_LMC(mlyr+1,mhru)) 
	 allocate(sol_LMN(mlyr+1,mhru)) 
	 allocate(sol_LS(mlyr+1,mhru)) 
	 allocate(sol_LSC(mlyr+1,mhru)) 
	 allocate(sol_LSN(mlyr+1,mhru)) 
      allocate(sol_LSL(mlyr+1,mhru)) 
      allocate(sol_RNMN(mlyr+1,mhru))
      allocate(sol_LSLC(mlyr+1,mhru))
      allocate(sol_LSLNC(mlyr+1,mhru))
      allocate(sol_RSPC(mlyr+1,mhru))      
      allocate(sol_WOC(mlyr+1,mhru))
      allocate(sol_WON(mlyr+1,mhru))
      allocate(sol_HP(mlyr+1,mhru))
      allocate(sol_HS(mlyr+1,mhru))
      allocate(sol_BM(mlyr+1,mhru))

	!daily update
	allocate(sol_percc(mlyr,mhru))
	allocate(sol_latc(mlyr,mhru))
	      
	!!for print out at daily, monthly, and annual scale
	allocate(sedc_d(mhru))
	allocate(surfqc_d(mhru))
	allocate(latc_d(mhru))
	allocate(percc_d(mhru))
	allocate(NPPC_d(mhru))
	allocate(GPPC_d(mhru))
	allocate(C_PROD(mhru))
	
	!allocate(tree_cfrac(6))
	!allocate(mfprd(6))	
	
	allocate(GR_d(mhru))
	allocate(MR_d(mhru))
	allocate(ctowoodc1(mhru))
	allocate(ctowoodc2(mhru))
	allocate(ctowoodc3(mhru))
	
	allocate(denitrify(mhru))

      allocate(wood1_SOM1(mhru))
      allocate(wood1_SOM2(mhru))
      allocate(wood2_SOM1(mhru))
      allocate(wood2_SOM2(mhru))
      allocate(wood3_SOM1(mhru))
      allocate(wood3_SOM2(mhru))

      allocate(eavail(3,mhru))

	
	allocate(leafdiec(mhru))
	allocate(frootjdiec(mhru))
	allocate(frootmdiec(mhru))
 
	allocate(rsdc_d(mhru)) 
	allocate(rsdn_d(mhru)) 
	allocate(grainc_d(mhru))
	allocate(stoverc_d(mhru))
	allocate(emitc_d(mhru))
	allocate(soc_d(mhru))
	allocate(rspc_d(mhru))

	allocate(sub_sedc_d(msub))
	allocate(sub_surfqc_d(msub))
	allocate(sub_latc_d(msub))
	allocate(sub_percc_d(msub))
	allocate(sub_NPPC_d(msub))
	allocate(sub_rsdc_d(msub))
	allocate(sub_grainc_d(msub))
	allocate(sub_stoverc_d(msub))
	allocate(sub_emitc_d(msub))
	allocate(sub_soc_d(msub))
	allocate(sub_rspc_d(mhru))
	   
	allocate(sedc_m(mhru))
	allocate(surfqc_m(mhru))
	allocate(latc_m(mhru))
	allocate(percc_m(mhru))
	allocate(foc_m(mhru))
	allocate(NPPC_m(mhru))
	allocate(rsdc_m(mhru)) 
	allocate(grainc_m(mhru))
	allocate(stoverc_m(mhru))
	allocate(emitc_m(mhru))
	allocate(soc_m(mhru))
	allocate(rspc_m(mhru))	
	
	allocate(sedc_a(mhru))
	allocate(surfqc_a(mhru))
	allocate(latc_a(mhru))
	allocate(percc_a(mhru))
	allocate(foc_a(mhru))
	allocate(NPPC_a(mhru))
	allocate(rsdc_a(mhru)) 
	allocate(grainc_a(mhru))
	allocate(stoverc_a(mhru))
	allocate(emitc_a(mhru))
	allocate(soc_a(mhru))
	allocate(rspc_a(mhru))	

      allocate(absorbed_no3(mhru))
      allocate(absorbed_nh3(mhru))
      allocate(no3_loss_pothole(mhru))
      allocate(nh4_vol(mhru))
      allocate(nh4_fert(mhru))
      allocate(no3_fert(mhru))
      allocate(solp_fert(mhru))
      allocate(no3_lat(mhru))
      allocate(no3_surf(mhru))
      allocate(no3_up(mhru))
      allocate(no3_autof(mhru))
      allocate(nh4_autof(mhru))
      allocate(solp_autof(mhru))
      allocate(orgn_autof(mhru))
      allocate(orgp_autof(mhru))
      allocate(orgn_grazf(mhru))    
      allocate(nh3_grazf(mhru))      
      allocate(no3_grazf(mhru))       
      allocate(solp_grazf(mhru))    
      allocate(orgp_grazf(mhru))   
      allocate(no3_conf(mhru)) 
      allocate(nh4_conf(mhru)) 
      allocate(orgn_conf(mhru))
      allocate(orgp_conf(mhru))
      allocate(solp_conf(mhru))
      allocate(no3_immo(mhru))
      allocate(nh4_immo(mhru))
      allocate(immo_err1(mhru))
      allocate(nh4_min(mhru))      
      
      allocate(nh4_min_ly(mlyr,mhru))
      allocate(no3_immo_ly(mlyr,mhru))
      allocate(nh4_immo_ly(mlyr,mhru))      
            
      allocate(no3_min(mhru))
      allocate(no3_denit(mhru))
      allocate(no3_nitr(mhru))
      allocate(no3_perc(mhru))
      allocate(no3_rain(mhru))
      allocate(no3_adsortion_parti(mhru))
      allocate(solp_min(mhru))
      allocate(sol_solp2minp(mhru))
      allocate(solp_up(mhru))
!!The following variables should be initialized in Allocate_parm.f


      allocate(CFPltSTR(1:mlyr,mhru)) 
      allocate(CFPltMET(1:mlyr,mhru))
      allocate(CFOrfSTR(1:mlyr,mhru)) 
      allocate(CFOrfMET(1:mlyr,mhru))
      allocate(CBurntSTR(1:mlyr,mhru))
      allocate(CBurntMET(1:mlyr,mhru))

      allocate(NFPltSTR(1:mlyr,mhru)) 
      allocate(NFPltMET(1:mlyr,mhru))
      allocate(NFOrfSTR(1:mlyr,mhru)) 
      allocate(NFOrfMET(1:mlyr,mhru))
      allocate(NBurntSTR(1:mlyr,mhru))
      allocate(NBurntMET(1:mlyr,mhru))

      allocate(CFMETS1(1:mlyr,mhru))
      allocate(CFSTRS1(1:mlyr,mhru))
      allocate(CFSTRS2(1:mlyr,mhru))
      allocate(CFS1S2(1:mlyr,mhru))
      allocate(CO2FMET(1:mlyr,mhru))
      allocate(CO2FSTR(1:mlyr,2, mhru))
      allocate(CO2FS1(1:mlyr,mhru))       

      
 
      allocate(CFS3S1(1:mlyr,mhru))        
      allocate(CFS1S3(1:mlyr,mhru)) 
      allocate(CFS2S1(1:mlyr,mhru)) 
      allocate(CFS2S3(1:mlyr,mhru))
      
      allocate(CO2FS2(1:mlyr,mhru))
      allocate(CO2FS3(1:mlyr,mhru))
      

      allocate(EFMETS1(1:mlyr,3, mhru))
      allocate(EFS1S2(1:mlyr,3, mhru))
      allocate(EFS1S3(1:mlyr,3, mhru))
      allocate(EFS2S1(1:mlyr,3, mhru))
      allocate(EFS2S3(1:mlyr,3, mhru))
      allocate(EFS3S1(1:mlyr,3, mhru))
      allocate(EFSTRS1(1:mlyr,3, mhru))
      allocate(EFSTRS2(1:mlyr,3, mhru))
      
      allocate(IMMMETS1(1:mlyr,3, mhru))
      allocate(IMMS1S2(1:mlyr,3, mhru))
      allocate(IMMS1S3(1:mlyr,3, mhru))
      allocate(IMMS2S1(1:mlyr,3, mhru))
      allocate(IMMS2S3(1:mlyr,3, mhru))
      allocate(IMMS3S1(1:mlyr,3, mhru))
      allocate(IMMSTRS1(1:mlyr,3, mhru))
      allocate(IMMSTRS2(1:mlyr,3, mhru))
      
      allocate(MINERALIZE(1:mlyr,3, mhru))
      allocate(MNRMETS1(1:mlyr,3, mhru))
      allocate(MNRS1S2(1:mlyr,3, mhru))
      allocate(MNRS1S3(1:mlyr,3, mhru))
      allocate(MNRS2S1(1:mlyr,3, mhru))
      allocate(MNRS2S3(1:mlyr,3, mhru))
      allocate(MNRS3S1(1:mlyr,3, mhru))
      allocate(MNRSTRS1(1:mlyr,3, mhru))
      allocate(MNRSTRS2(1:mlyr,3, mhru))

      allocate(ccp(mhru))
      allocate(usle_sed(mhru))
      
         !Tillage factor on SOM decomposition
       allocate(tillage_switch(mhru))
       allocate(tillage_depth(mhru))
       allocate(tillage_days(1:mlyr,mhru))
       allocate(tillage_factor(mhru))
       tillage_switch = 1
       tillage_depth = 0.
       tillage_days = 0
       tillage_factor = 0.
      
      !! By Zhang for C/N cycling
      !! ============================      	  
      
      !!>>>>>>>>>>>>>>>>>>>>> qichun>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
       allocate (CH4(mhru))
       allocate (N2O(mhru))
       allocate (NO(mhru))
       allocate (N2O_den(mhru))
       allocate (NO_den(mhru))
       allocate(temp_factor(mhru))
       allocate(water_factor(mhru))
       allocate(oxygen_factor(mhru))
       allocate(combined_factor(mhru))
       allocate (cal_temp(11))
       allocate(MaxRate(mhru))
       allocate(N2Oadjust_wp(mhru))
       allocate(N2Oadjust_fc(mhru))
       allocate(min_nitrate(mhru))
       allocate(wfpsdnitadj(mhru))
       !!--------qichun co2---------------------
       allocate(co2con(2500))
       
      !!<<<<<<<<<<<<<<<<<<<<qichun<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      
      
      !!------------------------
         
         allocate(sol_soc(mlyr,mhru))
         allocate (sedgen(mhru))          
        
         
          !! GWC============
        allocate (GwQ_DOC(mhru)) 
        allocate (shallst_doc_decay(mhru))     
        allocate (PerQB_DIC(mhru))     
        allocate (rchrg_dic(mhru))     
        allocate (shallst_dic(mhru))     
        allocate (GwQ_DIC (mhru))     
        allocate (rchrg_doc (mhru))    
        allocate (shallst_doc (mhru))    
        
         allocate(rspc_dnew(mhru)) 
         allocate (sol_RSPC1(mlyr,mhru))     
      
       

        allocate( Sed_RPON (mhru)) 
        allocate( Sed_LPON (mhru)) 
        allocate( SurQ_DON (mhru)) 
        allocate( LatQT_DON (mhru)) 
        allocate( HRU_RDON (mhru)) 
        allocate( HRU_LDON (mhru)) 
        allocate (HRU_DIN (mhru))    
        allocate (LatQ_DON (mlyr,mhru))     
        allocate (PerQ_DON (mlyr,mhru))     
      
        allocate (HRU_DIP (mhru))    
        allocate( Sed_RPOP (mhru)) 
        allocate( Sed_LPOP (mhru)) 
        allocate( HRU_RDOP (mhru)) 
        allocate( HRU_LDOP (mhru)) 
        
        allocate( rchrg_don (mhru)) 
        allocate( shallst_don (mhru)) 
        allocate(GwQ_DON (mhru)) 
        allocate(shallst_don_decay (mhru)) 
        allocate(revap_don(mhru)) 
        allocate(gwseep_don(mhru)) 
        
        allocate(revap_doc(mhru)) 
        allocate(gwseep_doc(mhru))
       
        allocate(revapn(mhru)) 
        allocate(gwseepn(mhru))
        
        allocate( revap_dic (mhru)) 
        allocate( gwseep_dic (mhru))
        
        allocate(Sed_RPOC(mhru))
        allocate(Sed_LPOC(mhru))
  
        allocate(PerQ_DOC(mlyr,mhru))
        allocate(PerQ_DIC (mlyr,mhru))
        allocate(LatQ_DOC (mlyr,mhru))
        allocate(LatQ_DIC (mlyr,mhru))
        allocate(Sol_DOC (mlyr,mhru))
        allocate(Sol_DIC(mlyr,mhru))
        
        allocate(SurQ_DIC(mhru))
        allocate(SurQ_DOC (mhru))
      
        allocate(LatQT_DOC(mhru))
        allocate(LatQT_DIC(mhru))        
         
         
        allocate(HRU_LDOC(mhru))
        allocate(HRU_RDOC(mhru))
        allocate(HRU_DIC(mhru))
        allocate(PerQB_DOC(mhru))
     
        allocate(HRU_CH4s(mhru))
        allocate(HRU_CH4g(mhru))
           
        allocate(SUB_RPOC(msub))
        allocate(SUB_LPOC (msub))
        allocate(SUB_RDOC (msub))
        allocate( SUB_LDOC(msub))
        allocate(SUB_DIC (msub))

       allocate( SUB_CH4g (msub))
       allocate( SUB_CH4s (msub))
       
       allocate( SUB_RPON(msub))
       allocate( SUB_LPON(msub))
       allocate( SUB_RDON(msub))
       allocate( SUB_LDON(msub))
       allocate( SUB_DIN(msub))
       allocate( SUB_N2Os(msub))
       allocate( SUB_N2s(msub))
      
       allocate( SUB_RPOP(msub))
       allocate( SUB_LPOP(msub))
       allocate( SUB_RDOP(msub))
       allocate( SUB_LDOP(msub))
       allocate( SUB_DIP(msub))
      
       allocate(part_DOC(mhru))
       allocate(peroc_DOC(mhru))
       allocate(peroc_DIC(mhru))
       allocate(er_POC(mhru))
       allocate(hlife_doc(mhru))
    
        allocate(gw_no3loss(mhru))
        allocate(OrgC_Plt2Rsd (mhru) )
        allocate(OrgN_Plt2Rsd (mhru) )
        allocate(OrgP_Plt2Rsd (mhru) )
        allocate(OrgC_Fer (mhru) )
        allocate(Sol_ApH(mhru))
        allocate(Sol_pCO2(mhru)) 
        allocate(Sol_H(mhru) ) 
        !!---------------
        allocate (latq_0(mhru))
        allocate (latno3_0(mhru)) 
        allocate (tileno3_0(mhru))
        allocate (sedyld_0(mhru)) 
        allocate (sedorgn_0(mhru))
        allocate (sedorgp_0(mhru)) 
        allocate (surqno3_0(mhru))
        allocate (surqsolp_0(mhru)) 
        allocate (sedminpa_0(mhru))
        allocate (sedminps_0(mhru)) 
        allocate (solc_no3_ini(mhru))
        allocate (solc_nh4_ini(mhru)) 
        allocate (solc_urea_ini(mhru)) 
        allocate (solc_orgn_ini(mhru))
        allocate (solc_orgp_ini(mhru)) 
        allocate (solc_orgc_ini(mhru))
        allocate (solc_orgcs_ini(mhru)) 
        allocate (solc_doc(mhru)) 
        allocate (solc_dic(mhru)) 
        allocate (solc_doc_ini(mhru)) 
        allocate (solc_dic_ini(mhru))
        allocate (Sed_RPON_0(mhru))
        allocate (Sed_LPON_0(mhru))
        
        allocate (Sed_RPOP_0(mhru))
        allocate (Sed_LPOP_0(mhru))
        
        allocate (SurQ_DON_0(mhru))
        allocate (LatQT_DON_0(mhru))
        allocate (Sed_RPOC_0(mhru))
        allocate (Sed_LPOC_0 (mhru))      
        allocate (SurQ_DOC_0(mhru))
        allocate (LatQT_DOC_0(mhru))
        allocate (SurQ_DIC_0 (mhru))
        allocate (LatQT_DIC_0(mhru))
        allocate (bio_ms_ini(mhru)) 
        allocate (shallst_doc_0(mhru)) 
        allocate (shallst_dic_0(mhru)) 
        allocate (turnovfr_nitn2o(mhru)) 
        allocate (solc_solp_ini(mhru)) 
        allocate (solc_minp_ini(mhru)) 
       
       
       
       
       allocate(IMMO_ERR(mhru)) 
       allocate(sol_BMC_In(mhru)) 
       allocate(Sed_BMC(mhru)) 
       allocate(nh4_rain(mhru))
       allocate(N2O_nit(mhru))
       allocate(NO_nit(mhru))
       allocate(grainN (mhru))
       allocate(solc_no3(mhru)) 
       allocate(solc_nh4(mhru))
       allocate(solc_urea(mhru))
       allocate(solc_orgn(mhru))
       allocate(solc_orgc(mhru))
       allocate(solc_orgcs(mhru)) 
       allocate(solc_orgp(mhru)) 
       allocate(solc_solp(mhru))  
       allocate(solc_minp(mhru))
      	
      
	   allocate(  iniorgn(mhru))  
	   allocate(  inino3(mhru)) 
         allocate(  ininh3(mhru))            
	   allocate(  solc_orgn_fnl(mhru)) 
	   allocate(  solc_no3_fnl(mhru)) 
         allocate(  solc_nh4_fnl(mhru))      
         	 
       	 allocate(  Min_ML_MBN (mhru))           ! mineralizationduring transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)     
         allocate(  Min_SL_MBN (mhru))                     !mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
         allocate(  Min_SL_SHN (mhru))                   !mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)
         allocate(  Min_MB_SHN (mhru))                       !mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
         allocate(  Min_MB_PHN(mhru))                         !mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
         allocate(  Min_SH_MBN (mhru))                       !mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
         allocate(  Min_SH_PHN (mhru))                         !mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)        
         allocate(  Min_PH_MBN (mhru))                           !mineralization.during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                                
         allocate(  IMM_ML_MBN(mhru))                     ! immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
         allocate(  IMM_SL_MBN (mhru))                        ! immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
         allocate(  IMM_SL_SHN (mhru))                        ! immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
         allocate(  IMM_MB_SHN (mhru))                     ! immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)   
         allocate(  IMM_MB_PHN (mhru))                       ! immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus) 
         allocate(  IMM_SH_MBN (mhru))                          !immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
         allocate(  IMM_SH_PHN (mhru))                           ! immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus
         allocate(  IMM_PH_MBN (mhru))                        ! immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)    
      
         allocate(  tillage_emix (mhru)) 
           tillage_emix = 0.
      
          allocate(R_LSCTP(mlyr,mhru))
          allocate(R_LMCTP(mlyr,mhru))
          allocate(R_BMCTP(mlyr,mhru))
          allocate(R_HSCTP(mlyr,mhru))
          allocate(R_HPCTP(mlyr,mhru)) 
          
           allocate(CMF(mlyr,mhru))
           allocate(WATF(mlyr,mhru))
           allocate(TEMF(mlyr,mhru))
           allocate(OXGF(mlyr,mhru))
           allocate(RANF(mlyr,mhru))
           allocate(TILLF(mlyr,mhru))
           allocate(LSCTA (mlyr,mhru))
           allocate(LSLCTA (mlyr,mhru))
           allocate(LSLnCTA (mlyr,mhru))
           allocate(LMCTA(mlyr,mhru))
           allocate(BMCTA(mlyr,mhru))
           allocate(HSCTA(mlyr,mhru))
           allocate(HPCTA(mlyr,mhru))
           allocate(LSNTA(mlyr,mhru))
           allocate(LMNTA (mlyr,mhru))
           allocate(BMNTA(mlyr,mhru))
           allocate(HSNTA(mlyr,mhru))
           allocate(HPNTA(mlyr,mhru))
           allocate(LSCTP(mlyr,mhru))
           allocate(LSLCTP(mlyr,mhru))
           allocate(LSLnCTP(mlyr,mhru))
           allocate(LMCTP(mlyr,mhru))
           allocate(BMCTP(mlyr,mhru))  
           allocate(HSCTP(mlyr,mhru))
           allocate(HPCTP(mlyr,mhru))
           allocate(LSNTP(mlyr,mhru))
           allocate(LMNTP(mlyr,mhru))
           allocate(BMNTP(mlyr,mhru))
           allocate(HSNTP(mlyr,mhru))
           allocate(HPNTP(mlyr,mhru))
        
           allocate( Inhibday(mhru))
           allocate( Inhibdu(mhru))
      
      
       !!  Soil N --------------------------
      allocate(sol_C(mlyr,mhru))
      allocate(Rn2_n2o(mlyr,mhru))
      allocate(Rno_n2o(mlyr,mhru)) 
      allocate(N2_den(mhru))
      allocate( porespace_tot(mhru))
      allocate(sol_wpmm_tot(mhru))
      allocate(WFPS_ave(mhru)) 
      allocate(st_ave(mhru)) 
      allocate(ph_ave(mhru)) 
      allocate(abs_f(mhru)) 
      allocate(sol_clayNH4 (mlyr,mhru))
      allocate(sol_soluNH4(mlyr,mhru))
      allocate(sol_totNH4(mlyr,mhru))
      allocate(CEC(mlyr,mhru))
   !!-------------Soil N ----------------------------------
      
       !!!---------------------output.sub-------------------------------------------------
    	  
       allocate(sub_n2o(msub))
       allocate(sub_no(msub))
       allocate(sub_nh4(msub))
       allocate(sub_ch4(msub)) 
       allocate(sub_dnit(msub))
       allocate(sub_nit(msub))
       allocate(sub_percno3 (msub))
       allocate(sub_upno3(msub))
       allocate(sub_ferno3(msub))
       allocate(sub_fernh4(msub))
       allocate(sub_ferorgn(msub))
       allocate(sub_rainno3 (msub))
       allocate(sub_fixn (msub))
       allocate(sub_solno3(msub))
       allocate(sub_solnh3(msub))
       allocate(sub_solorgn(msub))
       allocate(sub_solorgp(msub))
      
       allocate(sub_sedc(msub))
       allocate(sub_surfqc(msub))
       allocate(sub_latc(msub))
       allocate(sub_percc(msub))
       allocate(sub_NPPC(msub))
       allocate(sub_rspc (msub))
       allocate(sub_solorgc (msub))
       allocate(sub_solorgsc (msub))

       allocate (orgn_fert(mhru))
       allocate (orgp_fert(mhru))
       allocate(sol_tex(mlyr,mhru))
      
      
      
      end subroutine