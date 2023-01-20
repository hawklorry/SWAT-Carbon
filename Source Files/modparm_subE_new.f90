      module parm_subE
      
      !!\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
      !!============== Pysical-Based Soil Temperature Mudule==================={
 
      integer :: nly1
      real:: eff_conr
      !! for (nly)
      real, dimension (:), allocatable :: a_sol
      real, dimension (:), allocatable :: b_sol
      real, dimension (:), allocatable :: c_sol
      real, dimension (:), allocatable :: d_sol
      real, dimension (:), allocatable :: p_sol
      real, dimension (:), allocatable :: q_sol
      real, dimension (:), allocatable :: k_int
      real, dimension (:), allocatable :: ks_coe
      !! for (ihru)
      real, dimension (:), allocatable :: a_sno
      real, dimension (:), allocatable :: b_sno
      real, dimension (:), allocatable :: c_sno
      real, dimension (:), allocatable :: d_sno
      real, dimension (:), allocatable :: p_sno
      real, dimension (:), allocatable :: q_sno
      real, dimension (:), allocatable :: k_ss
      real, dimension (:), allocatable :: ca_sno
      real, dimension (:), allocatable :: k_sno
      real, dimension (:), allocatable :: snotmp1
      real, dimension (:), allocatable :: sno_hru1
      real, dimension (:), allocatable :: sur_tmp
      real, dimension (:), allocatable :: bot_tmp
      real, dimension (:), allocatable :: t_bare
      real, dimension (:), allocatable :: sno_den
      real, dimension (:), allocatable :: sno_dep
      real, dimension (:), allocatable :: c_coe
      real, dimension (:), allocatable :: eff_coe
      real, dimension (:), allocatable :: ddepth
      real, dimension (:), allocatable :: ansnodep
      real, dimension (:), allocatable :: snosurtmp
      real, dimension (:), allocatable :: ambtmp
      real, dimension (:), allocatable :: sno_por
      real, dimension (:), allocatable :: snoco 
      real, dimension (:), allocatable :: presnotmp
      real, dimension (:), allocatable :: sno_iday
      !! for (nly,ihru)
      real, dimension (:,:), allocatable :: ca_sol
      real, dimension (:,:), allocatable :: k_sol
      real, dimension (:,:), allocatable :: sol_ice
      real, dimension (:,:), allocatable :: sol_wc
      real, dimension (:,:), allocatable :: sol_wcv
      real, dimension (:,:), allocatable :: sol_orgv
      real, dimension (:,:), allocatable :: sol_minv
      real, dimension (:,:), allocatable :: sol_thic
      real, dimension (:,:), allocatable :: sol_org
      real, dimension (:,:), allocatable :: sol_icev
      real, dimension (:,:), allocatable :: sol_tmp1
      real, dimension (:,:), allocatable :: sol_pormm
      real, dimension (:,:), allocatable :: d_ice
      real, dimension (:,:), allocatable :: sol_air
      real, dimension (:,:), allocatable :: d_sols
      real, dimension (:,:), allocatable :: sol_satu
      real, dimension (:,:), allocatable :: sol_kd
      real, dimension (:,:), allocatable :: sol_ke
      real, dimension (:,:), allocatable :: sol_ksat
      real, dimension (:,:), allocatable :: sol_zc
      real, dimension (:,:), allocatable :: k_coe
      real, dimension (:,:), allocatable :: sol_cd
      !! temperature output
	  real, dimension (:), allocatable :: tma
	  real, dimension (:), allocatable :: soltmp_50
	  real, dimension (:), allocatable :: soltmp_100
	  real, dimension (:), allocatable :: soltmp_150
	  real, dimension (:), allocatable ::soltmp_200
	  real, dimension (:), allocatable :: soltmp_300
	  real, dimension (:), allocatable :: soltmp_500
	  real, dimension (:), allocatable :: soltmp_1000
	  real, dimension (:), allocatable :: sub_sur_tmp
	  real, dimension (:), allocatable :: sub_soltmp_50
      real, dimension (:), allocatable :: sub_soltmp_100
      real, dimension (:), allocatable :: sub_soltmp_150
      real, dimension (:), allocatable :: sub_soltmp_200
      real, dimension (:), allocatable :: sub_soltmp_300
      real, dimension (:), allocatable :: sub_soltmp_500
      real, dimension (:), allocatable :: sub_soltmp_1000
      real, dimension (:), allocatable :: swc_50
      real, dimension (:), allocatable :: swc_100
      real, dimension (:), allocatable :: swc_150
	  real, dimension (:), allocatable :: swc_200
	  real, dimension (:), allocatable :: swc_300
	  real, dimension (:), allocatable :: swc_500
	  real, dimension (:), allocatable :: swc_1000
      !!============== Pysical-Based Soil Temperature Mudule===================}
     
        real, dimension (:), allocatable :: sub_frozday
        real, dimension (:), allocatable :: sub_snodep
        real, dimension (:), allocatable :: sub_snohru
        real, dimension (:), allocatable :: sub_snofall
        real, dimension (:), allocatable :: sub_airtmp
        real, dimension (:), allocatable :: sol_frozday
        real, dimension (:), allocatable :: sub_swc
       
     !!=====================Water temperature============={
      real, dimension(:,:,:), allocatable :: wtmppara
     
      real, dimension(:,:),allocatable:: tmpav_lag
      real, dimension(:), allocatable::  tmpav_pre
      real, dimension(:),allocatable:: wtmp_surq
      real, dimension(:),allocatable:: wtmp_latq
      real, dimension(:),allocatable:: wtmp_gwq   
      real, dimension(:),allocatable:: latq_tot
      real, dimension(:),allocatable:: sub_gwq_deep 
      real, dimension(:,:), allocatable:: sub_hwyld 
      real, dimension (:),allocatable:: sub_wtmp_surq
      real, dimension (:),allocatable:: sub_wtmp_latq
      real, dimension (:),allocatable:: sub_wtmp_gwq
      real, dimension (:),allocatable:: sub_wtmp_gwdp
      
      !!====================Water temperature=============}
     
     
     
     
     end module parm_subE 