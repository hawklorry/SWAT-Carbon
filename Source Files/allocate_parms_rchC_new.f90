       subroutine allocate_parms_rchC
       use parm 
       use parm_rchC
       implicit none

      allocate (rchin_RPOC(mch))
      allocate (rchin_LPOC(mch))
      allocate (rchin_RDOC(mch))
      allocate (rchin_LDOC(mch))
      allocate (rchin_DIC(mch))
      allocate (rchini_RPOC(mch))
      allocate (rchini_LPOC(mch))
      allocate (rchini_RDOC(mch))
      allocate (rchini_LDOC(mch))
      allocate (rchini_DIC(mch))   
      allocate (rchini_Alg(mch))
      allocate (rchre_RPOC(mch))
      allocate (rchre_LPOC(mch))
      allocate (rchre_RDOC(mch))
      allocate (rchre_LDOC(mch))
      allocate (rchre_DIC(mch))
 
      allocate (rch_waterarea (mch))
       
  	    !!--algea-------------------------------
  	   
  	allocate (rch_Alg_Growth(mch))
      allocate (rch_Alg_Death(mch))
      allocate (rch_Alg_Resp(mch))
      allocate (rch_Alg_Set(mch))   
      allocate (rch_Alg_Exc(mch))   
    
              
      allocate (ch_area(mch))      
              
        !!----mch--------
  	   allocate (rchout_RPOC(mch))
         allocate (rchout_LPOC(mch))
         allocate (rchout_RDOC(mch))
         allocate (rchout_LDOC(mch))
         allocate (rchout_DIC(mch))
         allocate (rchout_CH4s(mch))  
         allocate (rchout_Alg(mch))
       
        
          allocate ( rchin_alg (mch))
          allocate ( rchin_orgn(mch))
          allocate ( rchin_nh4(mch))
          allocate ( rchin_no2(mch))
          allocate ( rchin_no3 (mch))
          allocate ( rchin_orgp (mch))
          allocate ( rchin_solp(mch))
          allocate ( rchin_cbod(mch))
          allocate ( rchin_dox(mch))
        
         ! allocate (  rchini_alg(mch)) 
          allocate ( rchini_orgn(mch))
          allocate ( rchini_nh4(mch))
          allocate ( rchini_no2(mch))
          allocate (  rchini_no3 (mch))
          allocate ( rchini_orgp (mch))
          allocate ( rchini_solp(mch))
          allocate ( rchini_cbod(mch))
          allocate ( rchini_dox(mch))
         ! rchout_chl
          !allocate ( rchout_alg (mch))
          allocate ( rchout_orgn (mch))
          allocate ( rchout_nh4(mch))
          allocate ( rchout_no2(mch))
          allocate ( rchout_no3(mch))
          allocate ( rchout_orgp(mch))
          allocate ( rchout_solp(mch))
          allocate ( rchout_cbod (mch))
          allocate (  rchout_dox (mch))
       
         allocate ( rchre_alg(mch))
         allocate ( rchre_orgn(mch))
         allocate ( rchre_nh4 (mch))                                    
         allocate ( rchre_no2(mch))                                              
         allocate ( rchre_no3(mch))                                             
         allocate ( rchre_orgp (mch))                                          
         allocate ( rchre_solp (mch))                                                    
         allocate ( rchre_cbod (mch))                                            
         allocate ( rchre_dox(mch))
 
            !!----------NEW water quality parameters-------------------------------------------------
       
       	allocate (rch_AbM (mch))
        allocate (rch_AbN  (mch))
        allocate (rch_AbP  (mch))
        allocate (rch_RPOC (mch))
        allocate (rch_LPOC  (mch))
        allocate (rch_RDOC  (mch))
        allocate (rch_LDOC    (mch))
        allocate (rch_DIC   (mch))
        allocate (rch_CH4s(mch))
        allocate (rch_SedC (mch))
        allocate (rch_BuryC (mch))
       
        allocate (rch_AlgSet(mch))
        allocate (rch_Alg_deathC(mch))
        allocate (rch_Ab_deathC(mch))
        allocate (rch_Alg_LPOC(mch))
        allocate (rch_Ab_LPOC(mch))
        allocate (rch_LPOC_Set(mch))
        allocate (rch_LPOC_LDOC(mch))
        allocate (rch_LPOC_DIC(mch))
        allocate (rch_LPOC_RPOC(mch))
        allocate (rch_Alg_RPOC(mch))
        allocate (rch_Ab_RPOC(mch))
        allocate (rch_RPOC_LDOC(mch))
        allocate (rch_RPOC_DIC (mch))
        allocate (rch_RPOC_Set  (mch))
        allocate (rch_Alg_LDOC  (mch))
        allocate (rch_Ab_LDOC      (mch))
        allocate (rch_LDOC_DIC   (mch))
        allocate (rch_LDOC_RDOC  (mch))
        allocate (rch_LDOC_NO3 (mch))
        allocate (rch_Alg_RDOC  (mch))
        allocate (rch_Ab_RDOC     (mch))
        allocate (rch_RDOC_DIC  (mch))
        allocate (rch_Atm_DIC  (mch))
        allocate (rch_Alg_DIC   (mch))
        allocate (rch_DIC_Alg  (mch))
        allocate (rch_Ab_DIC (mch))
        allocate (rch_DIC_Ab (mch))
        allocate ( rch_Bed_SedR(mch))
        allocate (rch_Sed_DIC(mch))
        allocate (rch_LDOC_AbExc(mch))
        allocate (rch_LDOC_AlgExc(mch))
        allocate (rch_Bed_LPOCR (mch)) 
        allocate (rch_Bed_RPOCR(mch)) 
        allocate (rch_Bed_CH4R(mch)) 
    
        allocate (rch_Bed_DenC(mch))
        allocate (rch_CH4s_Atm (mch)) 
        allocate (rch_CH4g_Atm(mch))
        allocate (rch_N2g_Atm (mch))
        allocate (rch_CH4s_CO2 (mch))
        allocate (rch_CH4g_CH4s (mch))
       
       
         allocate (rch_Bed_BOC(mch))
         allocate (rch_Bed_DIC(mch))
       
            
       
       
       

      !!----------------------Water quality----------------------
         
 

        allocate ( rch_Ab_Death(mch))   
        allocate ( rch_Ab_Photo(mch)) 
        allocate ( rch_Ab_Resp(mch))  
        allocate (rch_Ab_Exc(mch)) 

       
        
   
         
       
       
       
       end subroutine