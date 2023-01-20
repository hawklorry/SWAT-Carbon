      subroutine zero2
       
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros all array values
      use carbon_para  !! added by Du
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      hrumono = 0.
      wtrmon = 0.
      submono = 0.
      rchmono = 0.
      resoutm = 0.
      yldaa = 0.
      bio_aams = 0.
      lai_aamx = 0.
      
      aairr = 0.
      rch_Alg = 0.
      anion_excl = 0.
      alpha_bnk = 0.
      alpha_bnke = 0.
      !fr_curb = 0.
      bactlpcnst = 0.
      bactlpmon = 0.
      bactlpq = 0.
      bactlps = 0.
      bactlpyr = 0.
      bactpcnst = 0.
      bactpmon = 0.
      bactpq = 0.
      bactps = 0.
      bactpyr = 0.
      bankst = 0.
      bio_aahv = 0.
      biomix = 0.
      bp1 = 0.
      bp2 = 0.
      br1 = 0.
      br2 = 0.
      bw1 = 0.
      bw2 = 0.
      ch_l1 = 0.
      ch_l2 = 0.
      ch_revap = 0.
      chlap = 0.
      chlar = 0.
      chlaw = 0.
      cht = 0.
      cklsp = 0.
      cmtl1cnst = 0.
      cmtl1mon = 0.
      cmtl1yr = 0.
      cmtl2cnst = 0.
      cmtl2mon = 0.
      cmtl2yr = 0.
      cmtl3cnst = 0.
      cmtl3mon = 0.
      cmtl3yr = 0.
      crdep = 0.
      elevp = 0
      elevt = 0
      erorgn = 0.
      erorgp = 0.
      fcimp = 0.
      flocnst = 0.
      flowfr = 0.
      floyr = 0.
      flwin = 0.
      flwout = 0.
      fsred = 0.
      gwminp = 0.
      gwno3 = 0.
      harveff = 0.
      hhvaroute = 0.
      hru_sub = 0
      hru1 = 0
      hru_seq = 0
      hrutot = 0
      hvstiadj = 0.
      hydgrp = ""
      icanal = 0
      iflod1r = 0
      iflod2r = 0
      ihgage = 0
      ipnd1 = 0
      ipnd2 = 0
      
      imp_trig = 0   !!Srini pothole
      pot_orgn = 0.
      pot_orgp = 0.
      pot_mps = 0.
      pot_mpa = 0.
      pot_no3 = 0.
      pot_solp = 0.
      pot_evap = 0.
      tile_out = 0.
      tile_sedo = 0.
      tile_no3o = 0.
      tile_solpo = 0.
      tile_orgno = 0.
      tile_orgpo = 0.
      tile_minpso = 0.
      tile_minpao = 0.
      
      !irelease = 0
      imp_trig = 1
      ires1 = 0
      ires2 = 0
      iresco = 0
      isgage = 0
      iwgage = 0
      iyres = 0
      laimxfr = 0.
      ldrain = 0
      lkpst_conc = 0.
      lkpst_koc = 0.
      lkpst_mix = 0.
      lkpst_rea = 0.
      lkpst_rsp = 0.
      lkpst_stl = 0.
      lkpst_vol = 0.
      lkspst_act = 0.
      lkspst_bry = 0.
      lkspst_conc = 0.
      lkspst_rea = 0.
      minpcnst = 0.
      minpmon = 0.
      minpyr = 0.
      mores = 0
      ndeat = 0
      ndtargr = 0
      nh3cnst = 0.
      nh3mon = 0.
      nh3yr = 0.
      no2cnst = 0.
      no2mon = 0.
      no2yr = 0.
      no3cnst = 0.
      no3mon = 0.
      no3yr = 0.
      nsetlp = 0.
      nsetlr = 0.
      nsetlw = 0.
      oflowmn = 0.
      oflowmx = 0.
      oflowmn_fps = 0.
      olai = 0.
      orgncnst = 0.
      orgnmon = 0.
      orgnyr = 0.
      orgpcnst = 0.
      orgpmon = 0.
      orgpyr = 0.
      orig_lkpstconc = 0.
      orig_lkspstconc = 0.
      orig_pndno3 = 0.
      orig_pndorgn = 0.
      orig_pndorgp = 0.
      orig_pndsolp = 0.
      orig_resnh3 = 0.
      orig_resno2 = 0.
      orig_resno3 = 0.
      orig_resorgn = 0.
      orig_resorgp = 0.
      orig_ressed = 0.
      orig_ressolp = 0.
      orig_resvol = 0.
      orig_solst = 0.
      orig_solsw = 0.
      orig_soltmp = 0.
      orig_volcr = 0.
      orig_wetno3 = 0.
      orig_wetorgn = 0.
      orig_wetorgp = 0.
      orig_wetsolp = 0.
      ovrlnd = 0.
      pcf = 1.
      phi = 0.
      plt_et = 0.
      plt_pet = 0.
      plantn = 0.
      plantp = 0.
      pltfr_n = 0.
      pltfr_p = 0.
      pnd_chla = 0.
      pnd_no3 = 0.
      pnd_no3g = 0.
      pnd_no3s = 0.
      pnd_orgn = 0.
      pnd_orgp = 0.
      pnd_seci = 0.
      pnd_psed = 0.
      pnd_solp = 0.
      pnd_solpg = 0.
      pot_volx = 0.
      pot_volxmm = 0.
      potflwi = 0.
      potsedi = 0.
      potsani = 0.
      potsili = 0.
      potclai = 0.
      potsagi = 0.
      potlagi = 0.

      psetlp = 0.
      psetlr = 0.
      psetlw = 0.
      rchstor = 0.
      rch_bactp = 0.
      rch_bactlp = 0.
      res_bactlp = 0.
      res_bactp = 0.
      res_chla = 0.
      res_esa = 0.
      res_evol = 0.
      res_k = 0.
      res_nh3 = 0.
      res_no2 = 0.
      res_no3 = 0.
      res_nsed = 0.
      res_orgn = 0.
      res_orgp = 0.
      res_out = 0.
      res_psa = 0.
      res_pvol = 0.
      res_rr = 0.
      res_seci = 0.
      res_sed = 0.
      res_solp = 0.
      res_sub = 0
      res_vol = 0.
      resdata = 0.
      rnd2 = 0.
      rnd3 = 0.
      rnd8 = 0.
      rnd9 = 0.
      rndseed = 0
      rsdco_pl = 0.
      rwt = 0.
      sci = 0.
      seccip = 0.
      seccir = 0.
      secciw = 0.
      sed_stl = 0.
      sed_stlr = 0.
      sedcnst = 0.
      sedmon = 0.
      sedyr = 0.

	  sedyld = 0.
	  sanyld = 0.
	  silyld = 0.
	  clayld = 0.
	  sagyld = 0.
	  lagyld = 0.

      shallirr = 0.
      shyd = 0.
      smx = 0.
      snotmp = 0.
      snotmpeb = 0.
      sol_avbd = 0.
      sol_avpor = 0.
      sol_awc = 0.
      sol_fc = 0.
      sol_hk = 0.
      sol_st = 0.
      sol_sumfc = 0.
      sol_sumul = 0.
      sol_sw = 0.
      !sol_tmp = 0.
      sol_ul = 0.
      sol_wpmm = 0.
      starg = 0.
      starg_fps = 0.
      sub_tc = 0.
      surf_bs = 0.
      tmp_hi = 0.
      tmp_lo = 0.
      tmpavp = 0.
      twash = 0.
      urblu = 0
      urbname = ""
      usle_mult = 0.
      values = 0
      varoute = 0.
      vartran = 0.
      volcr = 0.
      welev = 0.
      wet_chla = 0.
      wet_no3 = 0.
      wet_no3g = 0.
      wet_no3s = 0.
      wet_orgn = 0.
      wet_orgp = 0.
      wet_psed = 0.
      wet_seci = 0.
      wet_solp = 0.
      wet_solpg = 0.
      wgnold = 0.
      wlat = 0.
      wpstaao = 0.
      wpstmono = 0.
      wpstyro = 0.
      wrt = 0.
      wshd_pstdg = 0.
      wshdaao = 0.
      wshdmono = 0.
      wshdyro = 0.
      wshdaaN = 0.
      wshdmonN = 0.
      wshdyrN = 0.
      wtraa = 0.
      wtryr = 0.
      wupnd = 0.
      wuresn = 0.
      wurtnf = 0.
      yldn = 0.
      zdb = 0.
	  
	!! MJW
	sol_P_model = 0
      
      bmp_flag = 0
      !! surface
      bmp_flo = 1.      !! Surface Flow
      bmp_sed = 1.      !! Sediment
      bmp_pp = 1.       !! Particulate P
      bmp_sp = 1.       !! Soluble P
      bmp_pn =  1.      !! Particulate N
      bmp_sn = 1.       !! Soluble N
      bmp_bac = 1.      !! Bacteria
      !! subsurface
      bmp_flos = 1.      !! Subsurface Flow
      bmp_seds = 1.      !! Sediment
      bmp_pps = 1.       !! Particulate P
      bmp_sps = 1.       !! Soluble P
      bmp_pns =  1.      !! Particulate N
      bmp_sns = 1.       !! Soluble N
      bmp_bacs = 1.      !! Bacteria
      !! tile
      bmp_flot = 1.      !! Tile Flow
      bmp_sedt = 1.      !! Sediment
      bmp_ppt = 1.       !! Particulate P
      bmp_spt = 1.       !! Soluble P
      bmp_pnt =  1.      !! Particulate N
      bmp_snt = 1.       !! Soluble N
      bmp_bact = 1.      !! Bacteria

      ro_bmp_flag = 0    !! Flag to turn on or off user BMP
 
      !! surface runoff removal efficiency
      ro_bmp_flo = 0.    !! Flow
      ro_bmp_sed = 0.    !! Sediment
      ro_bmp_pp = 0.     !! Particulate P
      ro_bmp_sp = 0.     !! Soluble P
      ro_bmp_pn = 0.     !! Particulate N
      ro_bmp_sn = 0.     !! Soluble N
      ro_bmp_bac = 0.    !! Bacteria
      !! subsurface - lateral soil and groundwater         
      ro_bmp_flos = 0.   !! Flow
      ro_bmp_seds = 0.   !! Sediment
      ro_bmp_pps = 0.    !! Particulate P
      ro_bmp_sps = 0.    !! Soluble P
      ro_bmp_pns = 0.    !! Particulate N
      ro_bmp_sns = 0.    !! Soluble N
      ro_bmp_bacs = 0.   !! Bacteria
      !! tile flow removal efficiency   
      ro_bmp_flot = 0.   !! Flow
      ro_bmp_sedt = 0.   !! Sediment
      ro_bmp_ppt = 0.    !! Particulate P
      ro_bmp_spt = 0.    !! Soluble P
      ro_bmp_pnt = 0.    !! Particulate N
      ro_bmp_snt = 0.    !! Soluble N
      ro_bmp_bact = 0.   !! Bacteria

	ssp_store = 0.
	psp_store = 0.
	a_days = 0
	b_days = 0
	sol_ph = 0.
	ori_sol_ph =0.
	sol_cal = 0  
	bio_init = 0
	lai_init = 0
	  
	
	 sol_ksat=0.
	 sol_satu=0.
       sol_ke=0.
       sol_kd=0.
      snotmp1=0.
	sol_tmp=0.
	sol_tmp1=0.
	sur_tmp=0.
	bot_tmp=0.
	sol_ice=0.
	sol_icev=0.
	snosurtmp=0.
	t_bare=0.
	ca_sol=1.
	k_sol=100.
      sol_icev=0.
	sol_wc=0.
	d_sols=0.
      sol_minv=0
	sol_orgv=0
      !sedyld1 = 0.
	sol_pormm = 0.
	d_ice=0.
	!om_watsat=0.
	sol_cd=0
	
	
      sol_C=0.
      Rn2_n2o=0.
      !Fr_C=0.
      Rno_n2o=0.
      sol_clayNH4 = 0.
      sol_soluNH4= 0.
      sol_totNH4=0.
      CEC=0.
      sol_frozday=0.
     
      
  
      rchaao2 = 0.   
      rchyro2 = 0. 
      rchmono2= 0.  
      rchdy2= 0.       
      !rchstor_pre = 0.
      rch_SedC=0.
      rch_BuryC=0.
      ch_area=0.
      ! rch_Cond=0.
      !rch_Alk=0.                    
      !rch_pH=7.

      rch_waterarea =0.
      HRU_CH4g=0.
      HRU_CH4s=0.
      !rch_SedBed=0.
      !rch_SedThick=0.
      !rch_H1=0.
      !rch_BEN_STR=0.
       !!!-------------------subdaily water quality----------------
       cbn_rch%RPOC=0.      !RPOC intial concentration 
       cbn_rch%LPOC =0.      !LPOC intial concentration 
       cbn_rch%RDOC=0.     !RDOC intial concentration
       cbn_rch%LDOC=0.      !LDOC intial concentration
       cbn_rch%DIC  =0.         !DIC intial concentration
       Ab_rch%Ab =0.           ! bottom algea iniial concentration (mg/L)
       Ab_rch%INb=0.
       Ab_rch%IPb=0.
        !!!-------------------subdaily water quality----------------
     
       rch_hwattemp_pre=0.
       tmpav_pre=0.
           !!--------------snowmelt---------------
      sno_den=1.
      sno_dep=1.
           !!--------------snowmelt---------------
       !ifirst_lst=1
       !iy_lst=0
       !ida_lst=0
    
       !irch_wet=0
  
       !musle_pow=0.
       sol_st_pre=0.
       
       
       abs_f = 0.
       
       turnovfr_nitn2o=0
       
      
       rch_AbM=0.
     
       rch_SedC = 0.
 
     
        
   
           
      
         rch_RPOC = 0.                                           !!outflow concentration mg/l ---------------------------------------State variable-------------------- 
         rch_LPOC = 0.                                            !!outflow concentration mg/l----------------------------------------State variable--------------------
         rch_RDOC = 0.                                           !!outflowconcentration mg/l-----------------------------------------State variable--------------------
         rch_LDOC = 0.                                            !!outflowconcentration mg/l-----------------------------------------State variable--------------------
         rch_DIC= 0.                                               !!ouflow concentration mg/l------------------------------------------State variable--------------------
         rch_CH4s= 0.
         !!----------------------------
         
     
      
           
     
 
       !!------------------------------
        Sol_DIC=0.
        sol_DOC=0.
        Sol_ApH=0.
        Sol_pCO2=0.
        Sol_H=0.
 
 
       shallst_don = 0.
       rchrg_don = 0.
       
       shallst_dic = 0.
       shallst_doc = 0.
       rchrg_doc = 0.
       rchrg_dic = 0.
       
       solc_no3  = 0.
       solc_nh4  = 0.
       solc_urea = 0.
       solc_orgn = 0.
       solc_orgc  = 0.
       solc_orgcs  = 0.
       solc_orgp  = 0.
       solc_solp  = 0. 
       solc_minp  = 0.
       
       basnh3i = 0.
       
       tillagef = 0.5
       
      
       idc_till = 4
       idc_sw = 2
       idc_tmp = 2
       Inhibday = 0
       Inhibdu = 10
       TILLF=1.
       
       hrumonN = 0.  
       hruyrN = 0.   
       hruaaN = 0.   
       sol_thick=0.
       
       
      return
      end