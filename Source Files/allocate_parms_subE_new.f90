      subroutine allocate_parms_subE
      use parm 
      use parm_subE
      use parm_control
      implicit none
    !!============soil and snow temperature module ===========
   

      allocate (a_sno(mhru))
      allocate (b_sno(mhru))
      allocate (c_sno(mhru))
      allocate (d_sno(mhru))
      allocate (p_sno(mhru))
      allocate (q_sno(mhru))
      allocate (k_ss(mhru))
      allocate (ca_sno(mhru))
      allocate (k_sno(mhru))
      allocate (snotmp1(mhru))
      allocate (sur_tmp(mhru))
      allocate (bot_tmp(mhru))
      allocate (sno_hru1(mhru))
      allocate (sno_den(mhru))
      allocate (sno_dep(mhru))
      allocate (snoco(mhru))
      allocate (t_bare(mhru))
      allocate (a_sol(mlyr+10))
      allocate (b_sol(mlyr+10))
      allocate (c_sol(mlyr+10))
      allocate (d_sol(mlyr+10))
      allocate (p_sol(mlyr+10))
      allocate (q_sol(mlyr+10))
      allocate (k_int(mlyr+10))
      allocate (ca_sol(mlyr+10,mhru))
      allocate (k_sol(mlyr+10,mhru))
      allocate (sol_ice(mlyr+10,mhru))
      allocate (sol_thic(mlyr+10,mhru))
      allocate (sol_cd(mlyr+10,mhru))
      allocate (sol_wc(mlyr+10,mhru))
      allocate (sol_wcv(mlyr+10,mhru))
      allocate (sol_orgv(mlyr+10,mhru))
      allocate (sol_org(mlyr+10,mhru))
      allocate (sol_minv(mlyr+10,mhru))
      allocate (sol_icev(mlyr+10,mhru))
      allocate (sol_tmp1(mlyr+10,mhru))
      allocate (sol_pormm(mlyr+10,mhru))
      allocate (d_ice(mlyr+10,mhru))
      allocate (sol_air(mlyr+10,mhru))
      allocate (d_sols(mlyr+10,mhru))
      allocate (sol_satu(mlyr+10,mhru))
      allocate (sol_kd(mlyr+10,mhru))
      allocate (sol_ke(mlyr+10,mhru))
      allocate (sol_ksat(mlyr+10,mhru))
      allocate (sol_zc(mlyr+10,mhru))     
      
      allocate (ddepth(mhru))  
      allocate (ansnodep (mhru))  
      allocate(snosurtmp(mhru))
      allocate(ambtmp(mhru))
      
      allocate (k_coe(mlyr+10,mhru))
      allocate (c_coe(mhru))
      allocate (ks_coe(mhru))
      allocate (eff_coe(mhru))
      
      allocate (presnotmp(mhru))
      allocate (sno_iday(mhru))
      allocate (tma(mhru)) 
      allocate (soltmp_50(mhru)) 
      allocate (soltmp_100(mhru)) 
      allocate (soltmp_150(mhru))
      allocate (soltmp_200(mhru))
      allocate (soltmp_300(mhru))
      allocate (soltmp_500(mhru))
      allocate (soltmp_1000(mhru))
      allocate (sub_sur_tmp(msub))
      allocate (sub_soltmp_50(msub))
      allocate (sub_soltmp_100(msub))
      allocate (sub_soltmp_150(msub))
      allocate (sub_soltmp_200(msub))
      allocate (sub_soltmp_300(msub))
      allocate (sub_soltmp_500(msub))
      allocate (sub_soltmp_1000(msub))
      allocate (swc_50(mhru)) 
      allocate (swc_100(mhru)) 
      allocate (swc_150(mhru))
      allocate (swc_200(mhru))
      allocate (swc_300(mhru))
      allocate (swc_500(mhru))
      allocate (swc_1000(mhru))
      
      
       allocate(sub_frozday(msub))
       allocate(sub_snodep(msub))
       allocate(sub_snohru(msub))
       allocate(sub_snofall(msub))
       allocate(sub_airtmp(msub)) 
       
       allocate (sol_frozday(mhru)) 
       allocate (sub_swc(mhru))
      
       allocate (wtmp_surq(mhru))
       allocate (wtmp_latq(mhru))
       allocate (wtmp_gwq (mhru))
       allocate (latq_tot (mhru)) 
       allocate (tmpav_pre(itempa))
       allocate (sub_hwyld(mch,nstep_rch)) 
        
       allocate (sub_wtmp_surq(msub))
       allocate (sub_wtmp_latq(msub))
       allocate (sub_wtmp_gwq(msub))
       allocate (sub_wtmp_gwdp(msub)) 
       
       
          !!--------------------stream temperature----------------------------
	   allocate(wtmppara(msub,3,8)) !limted to 4 sets of parameters
       allocate(tmpav_lag(30,msub))
      !!--------------------stream temperature---------------------------- 
       
       allocate (sub_gwq_deep(msub))
      
      
      
       end subroutine 