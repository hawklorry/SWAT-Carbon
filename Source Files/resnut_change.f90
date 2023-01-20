      subroutine resnut_Du
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes soluble nitrogen and soluble phosphorus through reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlar(:)    |none          |chlorophyll-a production coefficient for
!!                               |reservoir
!!    inum1       |none          |reservoir number
!!    inum2       |none          |inflow hydrograph storage location number
!!    ires1(:)    |none          |beginning of mid-year nutrient settling
!!                               |"season"
!!    ires2(:)    |none          |end of mid-year nutrient settling "season"
!!    i_mo        |none          |current month of simulation
!!    nsetlr(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlr(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    psetlr(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlr(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_solp(:) |kg P          |amount of soluble P in reservoir
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    ressa       |ha            |surface area of reservoir on day
!!    seccir(:)   |none          |water clarity coefficient for reservoir
!!    varoute(4,:)|kg N          |organic nitrogen
!!    varoute(5,:)|kg P          |organic posphorus
!!    varoute(6,:)|kg N          |nitrate
!!    varoute(7,:)|kg P          |soluble phosphorus
!!    varoute(14,:)|kg N         |ammonia
!!    varoute(15,:)|kg N         |nitrite
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    res_chla(:) |kg chl-a      |amount of chlorophyll-a in reservoir
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_seci(:) |m             |secchi-disk depth
!!    res_solp(:) |kg P          |amount of soluble P in reservior
!!    reschlao    |kg chl-a      |amount of chlorophyll-a leaving reaservoir
!!                               |on day
!!    resnh3o     |kg N          |amount of ammonia leaving reservoir on day
!!    resno2o     |kg N          |amount of nitrite leaving reservoir on day
!!    resno3o     |kg N          |amount of nitrate leaving reservoir on day
!!    resorgno    |kg N          |amount of organic N leaving reservoir on day
!!    resorgpo    |kg P          |amount of organic P leaving reservoir on day
!!    ressolpo    |kg P          |amount of soluble P leaving reservoir on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlaco      |ppb (ug/L)    |chlorophyll-a concentration
!!    iseas       |none          |nutrient settling rate season
!!    jres        |none          |reservior number
!!    nitrok      |none          |fraction of nitrogen in reservoir removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in reservoir removed
!!                               |by settling
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in water
!!                               |on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para    !! added by Du
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none  
      integer :: jres, iseas
      real :: nitrok, phosk, tpco, chlaco, conc_p, conc_n
      !! added by Du---------------------------------------
      real :: lpoc_con,rpoc_con,ldoc_con,rdoc_con,dic_con,dic_bed,xx     !initial carbon concentration in the reservoir
      real :: wtmp,depth,f_rdb,kac,kh_DIC,CO2_sat,cbn_bur,solpcon,algcon !local variable for carbon processes
      real :: Ab_con,Ab_UN,Ab_UP,DIN_con,qN_Ab,qP_Ab,fnl_Ab,fll_Ab       !local variables for botom algae
      real :: fnl_dic,I0_Ab,IH_Ab,sedc,ke_Ab,POMcon,fsl_Ab,kph_Ab,wtmpk  !local variables for botom algae
      !! added by Du-----------------------------------------------
      
      real, external :: THETA
      
      jres = 0
      jres = inum1

!! if reservoir volume less than 1 m^3, set all nutrient levels to
!! zero and perform no nutrient calculations
      if (res_vol(jres) < 1.) then
        res_orgn(jres) = 0.
        res_orgp(jres) = 0.
        res_no3(jres) = 0.
        res_nh3(jres) = 0.
        res_no2(jres) = 0.
        res_solp(jres) = 0.
        res_chla(jres) = 0.
        res_seci(jres) = 0.
       !------For carbon module,added by Du------------------!
        if (cswat == 2 ) then
           cbn_res(jres)%RPOC   = 0.     !(Kg-C)
           cbn_res(jres)%LPOC   = 0.     !(Kg-C)
           cbn_res(jres)%RDOC   = 0.     !(Kg-C)
           cbn_res(jres)%LDOC   = 0.     !(Kg-C)
           cbn_res(jres)%DIC    = 0.      !(Kg-C)
        endif
    !-------For carbon module,added by Du------------------!  
      end if
      if (res_vol(jres) < 1.) return

!! if reservoir volume greater than 1 m^3, perform nutrient calculations
      if (i_mo >= ires1(jres) .and. i_mo <= ires2(jres)) then
        iseas = 1
      else
        iseas = 2
      endif

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      res_orgn(jres) = res_orgn(jres) + varoute(4,inum2)
      res_orgp(jres) = res_orgp(jres) + varoute(5,inum2)
      res_no3(jres) = res_no3(jres) + varoute(6,inum2)
      res_nh3(jres) = res_nh3(jres) + varoute(14,inum2)
      res_no2(jres) = res_no2(jres) + varoute(15,inum2)
      res_solp(jres) = res_solp(jres) + varoute(7,inum2)
      
      conc_p = (res_orgp(jres) + res_solp(jres)) / res_vol(jres)
      conc_n = (res_orgn(jres) + res_no3(jres) + res_nh3(jres) +  res_no2(jres)) / res_vol(jres)
      conc_n = res_no3(jres) / res_vol(jres)

      !! settling rate/mean depth
      !! part of equation 29.1.3 in SWAT manual
      !! ires_nut = 1 new equations 0 = old equations (Ikenberry)
      if (ires_nut == 1) then
        phosk = ressa * 10000. * (conc_p - con_pirr(jres)) * theta(psetlr(iseas,jres), theta_p(jres), tmpav(res_sub(jres)))
        nitrok = ressa * 10000. * (conc_n - con_nirr(jres)) *  theta(nsetlr(iseas,jres), theta_n(jres), tmpav(res_sub(jres)))
      else
        phosk = psetlr(iseas,jres) * ressa * 10000. / (res_vol(jres) + resflwo)
        nitrok = nsetlr(iseas,jres) * ressa * 10000. / (res_vol(jres) + resflwo)
      endif
      nitrok = Max(nitrok, 0.)
      phosk = Max(phosk, 0.)
      nitrok = Min(nitrok, 1.)
      phosk = Min(phosk, 1.)
      
        !!! charles ikenberry output file 
        !      write (2222,2222) iyr, i, res_vol(jres), res_no3(jres), ressa, 
        !     &   conc_n, con_nirr(jres), nsetlr(iseas,jres), theta_n(jres),
        !     &   tmpav(res_sub(jres)), nitrok
        !2222  format (2i6, 9f12.4)

      !! remove nutrients from reservoir by settling
      !! other part of equation 29.1.3 in SWAT manual
      res_solp(jres)    = res_solp(jres) * (1. - phosk)
      res_orgp(jres)    = res_orgp(jres) * (1. - phosk)
      res_orgn(jres)    = res_orgn(jres) * (1. - nitrok)
      res_no3(jres)     = res_no3(jres) * (1. - nitrok)
      res_nh3(jres)     = res_nh3(jres) * (1. - nitrok)
      res_no2(jres)     = res_no2(jres) * (1. - nitrok)

      !! calculate chlorophyll-a and water clarity
      tpco  = 0.
      chlaco    = 0.
      res_chla(jres)    = 0.
      res_seci(jres)    = 0.
      tpco  = 1.e+6 * (res_solp(jres) + res_orgp(jres)) /  (res_vol(jres) + resflwo)
      ! tpco        |ppb (ug/L)    |concentration of phosphorus in water on day
      if (tpco > 1.e-4) then
        !! equation 29.1.6 in SWAT manual
        chlaco  = chlar(jres) * 0.551 * (tpco**0.76)
        res_chla(jres)  = chlaco * (res_vol(jres) + resflwo) * 1.e-6
      endif
      if (chlaco > 1.e-4) then
        !! equation 29.1.8 in SWAT manual
        res_seci(jres)  = seccir(jres) * 6.35 * (chlaco**(-0.473))
      endif

      !! calculate amount of nutrients leaving reservoir
      if (res_no3(jres) < 1.e-4) res_no3(jres) = 0.0
      if (res_orgn(jres) < 1.e-4) res_orgn(jres) = 0.0
      if (res_orgp(jres) < 1.e-4) res_orgp(jres) = 0.0
      if (res_solp(jres) < 1.e-4) res_solp(jres) = 0.0
      if (res_chla(jres) < 1.e-4) res_chla(jres) = 0.0
      if (res_nh3(jres) < 1.e-4) res_nh3(jres) = 0.0
      if (res_no2(jres) < 1.e-4) res_no2(jres) = 0.0
      resno3o   = res_no3(jres) * resflwo / (res_vol(jres) + resflwo)
      resorgno  = res_orgn(jres) * resflwo / (res_vol(jres) + resflwo)
      resorgpo  = res_orgp(jres) * resflwo / (res_vol(jres) + resflwo)
      ressolpo  = res_solp(jres) * resflwo / (res_vol(jres) + resflwo)
      reschlao  = res_chla(jres) * resflwo / (res_vol(jres) + resflwo)
      resnh3o   = res_nh3(jres) * resflwo / (res_vol(jres) + resflwo)
      resno2o   = res_no2(jres) * resflwo / (res_vol(jres) + resflwo)
      res_orgn(jres)    = res_orgn(jres) - resorgno
      res_orgp(jres)    = res_orgp(jres) - resorgpo
      res_no3(jres) = res_no3(jres) - resno3o
      res_nh3(jres) = res_nh3(jres) - resnh3o
      res_no2(jres) = res_no2(jres) - resno2o
      res_solp(jres)    = res_solp(jres) - ressolpo
      res_chla(jres)    = res_chla(jres) - reschlao
!------------Calculating carbon processes in the reservoir,added by Du-----------------!
      if (cswat == 2 ) then
               if (i == 1 .and. curyr == 1) then
                    Ab_res(jres)%Ab = 100.                    
                    Ab_res(jres)%INb = Ab_res(jres)%Ab * Ab_respara(jres)%qoN
                    Ab_res(jres)%IPb = Ab_res(jres)%Ab * Ab_respara(jres)%qoP                    
                end if
      
      
           wtmp   = 0.
           wtmp   = 5.0 + 0.75 * tmpav(res_sub(jres)) !??? calculating water temperature
           if (wtmp <= 0.) wtmp   = 0.1
           depth  =0.
           depth  =res_vol(jres) / (ressa * 10000.0) !calculating the depth?
           !Add incoming carbon to those in reservoir
           cbn_res(jres)%RPOC    = cbn_res(jres)%RPOC+varoute(33,inum2)
           cbn_res(jres)%LPOC    = cbn_res(jres)%LPOC+varoute(34,inum2)
           cbn_res(jres)%RDOC    = cbn_res(jres)%RDOC+varoute(35,inum2)
           cbn_res(jres)%LDOC    = cbn_res(jres)%LDOC+varoute(36,inum2)
           cbn_res(jres)%DIC     = cbn_res(jres)%DIC+varoute(37,inum2)
           !Caculate initial carbon concentration (mg/L) in the reservoir
           rpoc_con  = 0.
           rpoc_con  = cbn_res(jres)%RPOC / res_vol(jres) * 1000.0
           lpoc_con  = 0.
           lpoc_con  = cbn_res(jres)%LPOC / res_vol(jres) * 1000.0
           rdoc_con  = 0.
           rdoc_con  = cbn_res(jres)%RDOC  /res_vol(jres) * 1000.0
           ldoc_con  = 0.
           ldoc_con  = cbn_res(jres)%LDOC / res_vol(jres) * 1000.0
           dic_con   = 0.
           dic_con   = cbn_res(jres)%DIC/res_vol(jres) * 1000.0
           !----calculating bottom algae processes in the reservoir------------!
           Ab_con    = 0.
           Ab_con    = Ab_res(jres)%Ab
           !bottom algea death procese
           Ab_res(jres)%death    = 0.
           Ab_res(jres)%death    = Ab_con * Theta((Ab_respara(jres)%kdb),1.04,wtmp)  ! bottom algea death biomass (g/m2/day)
           !----bottom algae photosynthesis/growth process
           !For calculating nutrient limitation factor
           qN_Ab = Ab_res(jres)%INb/Ab_con  !the cell quotas of N (mgN/gD)
           qP_Ab = Ab_res(jres)%IPb/Ab_con  !the cell quotas of P (mgP/gD)
           fnl_dic   = 0.
           fnl_dic   = Ab_respara(jres)%fdic * dic_con  !H2CO3* and HCO3- concentration (mg/L)
           fnl_Ab    = 0.
           fnl_Ab    = min((1. - Ab_respara(jres)%qoN / qN_Ab),(1. - Ab_respara(jres)%qoP / qP_Ab), (fnl_dic / (fnl_dic + Ab_respara(jres)%kscb)))
           
           if (fnl_Ab < 0.) fnl_Ab = 0.
           !kinetics for intracellular N and P process in bottom algae
           Ab_UN    = 0.     !uptake rates for N in bottom algae (mgN/m2/day)
           Ab_UP    = 0.     !uptake rates for P in bottom algae (mgP/m2/day)
           DIN_con  = (res_no3(jres) + res_nh3(jres) + res_no2(jres)) / res_vol(jres) * 1000.0 !calculate DIC concentration (mgN/L)
           Ab_UN    = Ab_respara(jres)%pmN * (DIN_con / (DIN_con + Ab_respara(jres)%Ksnb)) * Ab_con	&
                        * (Ab_respara(jres)%KqN / (Ab_respara(jres)%KqN + (qN_Ab - Ab_respara(jres)%qoN))) !calculating N uptakes in bottom algae
           if (Ab_UN < 0.) Ab_UN    = 0.
           solpcon  = 0.
           solpcon  = res_solp(jres) / res_vol(jres) * 1000.0       !calculate DIP concentration (mgP/L)
           Ab_UP    = Ab_respara(jres)%pmP * (solpcon / (solpcon + Ab_respara(jres)%Kspb)) * Ab_con            &
                        * (Ab_respara(jres)%KqP / (Ab_respara(jres)%KqP + (qP_Ab - Ab_respara(jres)%qoP))) !calculating P uptakes in bottom algae
           if (Ab_UP<0.) Ab_UP = 0.
           Ab_res(jres)%INb = Ab_res(jres)%INb + Ab_UN - Ab_res(jres)%death               &
                                * qN_Ab - qN_Ab * Theta(Ab_respara(jres)%kexb, 1.04, wtmp) * Ab_con    !intracellular N mass balance
           if (Ab_res(jres)%INb < 0.) Ab_res(jres)%INb  = 0.
           Ab_res(jres)%IPb = Ab_res(jres)%IPb + Ab_UP - Ab_res(jres)%death              &
                                * qP_Ab-qP_Ab * Theta(Ab_respara(jres)%kexb, 1.04, wtmp) * Ab_con    !intracellular P mass balance
           if (Ab_res(jres)%IPb < 0.) Ab_res(jres)%IPb  = 0.
           
           !For calculating light limation factor
           I0_Ab    = 0.
           I0_Ab    = hru_ra(hru1(res_sub(jres))) * tfact !0.47    !photosynthetically-active solar radiation reaching water surface
           POMcon   = (lpoc_con+rpoc_con) / 0.58 !Convert POC to POM concentration (mg/L)
           sedc     = 0.
           sedc     = res_sed(jres)*1.e6    !TSS concentration (mg/L)
           algcon   = 0.
           algcon   = chlaco / ai0     !floating algae concentration (mg/L)   
           ke_Ab    = Ab_respara(jres)%keb + Ab_respara(jres)%kiss * sedc + Ab_respara(jres)%kpom             &
                        * POMcon + Ab_respara(jres)%kap * algcon + Ab_respara(jres)%kapn                         &
                        * algcon**(.66667) + Ab_respara(jres)%kAb * Ab_con / depth             !the light extinction coefficient
           IH_Ab   = 0.
           IH_Ab   = I0_Ab * exp(-ke_Ab * depth)          !the quantity of bottom light
           fll_Ab  = 0.
           select case (Ab_respara(jres)%light)     ! caculating light limitation factor using different light models
              case (1)
                !! Half-Saturation Light Model 
                fll_Ab  = IH_Ab / (Ab_respara(jres)%klb + IH_Ab)
              case (2)
                !! Smith’s Function for light model 
                fll_Ab  = IH_Ab / (Ab_respara(jres)%klb**2 + IH_Ab**2)**(0.5)
              case (3)
                !! Steele’s Equation for light model
                fll_Ab = (IH_Ab/Ab_respara(jres)%klb) * exp(1. + IH_Ab/Ab_respara(jres)%klb)
           end select
           
           kph_Ab = 0.
           kph_Ab = Theta(Ab_respara(jres)%kph, 1.04, wtmp)       !temperature correction for photosynthesis rate
           Ab_res(jres)%photo = 0.
           if (Ab_respara(jres)%ipho == 0) then !zero order growth model
                Ab_res(jres)%photo  = kph_Ab * fll_Ab * fnl_Ab                     
           else    !first order growth model
                fsl_Ab  = 0.
                fsl_Ab  = max(1. - Ab_con / Ab_respara(jres)%Abmax, 0.) ! Space limitation factor
                Ab_res(jres)%photo  = kph_Ab * fll_Ab * fnl_Ab * fsl_Ab * Ab_con
           endif
             
             !bottom algea respiration process
           Ab_res(jres)%resp   = 0.
           Ab_res(jres)%resp  = Ab_respara(jres)%krb1 * Ab_con + Ab_respara(jres)%krb2 * Ab_res(jres)%photo
	     	                        
           !Bottom algae biomass mass balanc
           Ab_res(jres)%Ab    = Ab_res(jres)%Ab + Ab_res(jres)%photo - Ab_res(jres)%resp - Ab_res(jres)%death
           if (Ab_res(jres)%Ab < 0.) Ab_res(jres)%Ab   = 0.000001
           !--Finish calculating bottom algae processes in the reservoir-------!
           !calculating algal death to organic carbon
           cbn_rec%Ab_cbn    = 0.
           cbn_rec%Ab_cbn    = cbn_respara(jres)%rca * Ab_res(jres)%death / depth    !bottom algae death to total organic carbon (mg/L)
           !LPOC's reaction pathways
           cbn_rec%Ab_LP     = 0.
           cbn_rec%Ab_LP     = cbn_respara(jres)%f_lpb * cbn_rec%Ab_cbn ! bottom algae to LPOC 
           cbn_rec%LP_set    = 0.
           cbn_rec%LP_set    = cbn_respara(jres)%sv_lp / depth * lpoc_con              !LPOC settling to bed
           cbn_rec%LP_LD     = 0.
           cbn_rec%LP_LD     = Theta((cbn_respara(jres)%klp), 1.047, wtmp) * lpoc_con    !LPOC dissolution to LDOC
           cbn_rec%LP_DIC    = 0.
           cbn_rec%LP_DIC    = Theta((cbn_respara(jres)%kd_lp), 1.047, wtmp) * lpoc_con !LPOC decay to DIC
           cbn_rec%LR_POC    = 0.
           cbn_rec%LR_POC    = Theta((cbn_respara(jres)%klrp), 1.047, wtmp) * lpoc_con  ! LPOC decay to RPOC
           xx    = 0.
           xx    = lpoc_con
           lpoc_con  = lpoc_con + cbn_rec%Ab_LP - cbn_rec%LP_set - cbn_rec%LP_LD - cbn_rec%LP_DIC - cbn_rec%LR_POC  !update LPOC concentration for transformations   
           if (lpoc_con < 0.) then
             lpoc_con = 0.
           !correct LPOC settling amount using mass balance equation if settling is too much
             cbn_rec%LP_set = xx + cbn_rec%Ab_LP - cbn_rec%LP_LD  - cbn_rec%LP_DIC - cbn_rec%LR_POC
           endif
           
           !RPOC reaction pathway   
           cbn_rec%Ab_RP     = 0.
           cbn_rec%Ab_RP     = cbn_respara(jres)%f_rpb * cbn_rec%Ab_cbn    ! bottom algae to RPOC        
           cbn_rec%RP_LD     = 0. 
           cbn_rec%RP_LD     = Theta((cbn_respara(jres)%krp),1.047,wtmp) * rpoc_con      !RPOC dissolution to LDOC
           cbn_rec%RP_DIC    = 0.
           cbn_rec%RP_DIC    = Theta((cbn_respara(jres)%kd_rp),1.047,wtmp) * rpoc_con   !RPOC decay to DIC
           cbn_rec%RP_set    = 0.
           cbn_rec%RP_set    = cbn_respara(jres)%sv_rp/depth * rpoc_con                !RPOC settling to bed
           xx    = 0.
           xx    = rpoc_con
           rpoc_con  = rpoc_con + cbn_rec%Ab_RP - cbn_rec%RP_set - cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC   !update RPOC concentration for transformations  
           if (rpoc_con < 0.) then
             rpoc_con = 0.
           !correct RPOC settling amount using mass balance equation if settling is too much
             cbn_rec%RP_set = xx + cbn_rec%Ab_RP - cbn_rec%RP_LD - cbn_rec%RP_DIC + cbn_rec%LR_POC
           endif
           !LDOC reaction pathways
           cbn_rec%Ab_LD     = 0.
           cbn_rec%Ab_LD     = cbn_respara(jres)%f_ldb * cbn_rec%Ab_cbn   ! bottom algae death to LDOC
           cbn_rec%LD_DIC    = 0.
           cbn_rec%LD_DIC    = Theta((cbn_respara(jres)%kld), 1.047, wtmp) * ldoc_con  !LDOC decay to DIC
           cbn_rec%LR_DOC    = 0.
           cbn_rec%LR_DOC    = Theta((cbn_respara(jres)%klrd), 1.047, wtmp) * ldoc_con ! LDOC decay to RDOC
           cbn_rec%LD_NO3    = 0.
           cbn_rec%LD_NO3    = conc_n * (15.0/14.0) * Theta((cbn_respara(jres)%kdnit), 1.045, wtmp) !LDOC consumbed by NO3 denitrification  
           ldoc_con          = ldoc_con + cbn_rec%Ab_LD + cbn_rec%LP_LD	    &
      	  	                    + cbn_rec%RP_LD - cbn_rec%LR_DOC - cbn_rec%LD_DIC - cbn_rec%LD_NO3  !update LDOC concentration for transformations  
           if (ldoc_con<0.) ldoc_con  = 0.      
           !RDOC reation pathways
           f_rdb           = 1. - cbn_respara(jres)%f_lpb - cbn_respara(jres)%f_rpb - cbn_respara(jres)%f_ldb  !fraction of bottom algae death to RDOC
           cbn_rec%Ab_RD   = 0.
           cbn_rec%Ab_RD   = f_rdb * cbn_rec%Ab_cbn ! bottom algae to RDOC
           cbn_rec%RD_DIC  = 0.
           cbn_rec%RD_DIC  = Theta((cbn_respara(jres)%krd), 1.047, wtmp) * rdoc_con  !RDOC decay to DIC
           rdoc_con        = rdoc_con + cbn_rec%Ab_RD + cbn_rec%LR_DOC	- cbn_rec%RD_DIC  !update RDOC concentration for transformations 
           if (rdoc_con < 0.) rdoc_con = 0.
           !DIC reaction pathways
           cbn_rec%Atm_DIC = 0.
           kac             = Theta(rk2(res_sub(jres)), 1.024, wtmp) * 0.923           !CO2 reaeration rate 
           wtmpk           = wtmp + 273.15  !convet the unit of water temp to Kelvin
           kh_DIC          = 10.0**((2385.73 / wtmpk) + 0.0152642 * wtmpk - 14.0184)    !Henry's constant [mol/L/atm] 
           if(ico2 == 1) cbn_respara(jres)%p_co2 = co2con(iyr) 
           !CO2_sat         = 12.0 * kh_DIC * cbn_respara(jres)%p_co2 / 1000.0       !CO2 saturation (mol/L unit converting to mgC/L)
           !updated
           CO2_sat         = kh_DIC * (cbn_respara(jres)%p_co2 / 1000000) * 1000.0 * 12.0     !CO2 saturation (mol/L unit converting to mgC/L)
           cbn_rec%Atm_DIC = kac * (CO2_sat - cbn_respara(jres)%f_co2 * dic_con)   !Atmospheric CO2 reaeration
           cbn_rec%Ab_DIC  = 0.
           cbn_rec%Ab_DIC  = cbn_respara(jres)%rca * Ab_res(jres)%resp / depth    !bottom algae respiration to DIC  
           cbn_rec%DIC_Ab  = 0.
           cbn_rec%DIC_Ab  = cbn_respara(jres)%rca * Ab_res(jres)%photo / depth    !DIC consumbed by bottom algae photosynthesis
           !first order sediment diagenesis model-from W2 model
           scbn_res(jres)%scbn  = scbn_res(jres)%scbn + cbn_rec%RP_set + cbn_rec%LP_set !add RPOC and LPOC settling from water column     
           dic_bed              = 0.
           dic_bed              = scbn_res(jres)%scbn * scbn_respara(jres)%ksed   !DIC release from bed sediment compartment 
           cbn_bur              = 0.
           cbn_bur              = scbn_res(jres)%scbn * scbn_respara(jres)%kbur         !sediment burial amount 
           scbn_res(jres)%scbn  = scbn_res(jres)%scbn - dic_bed - cbn_bur !update sediment carbon amount after loss
           if (scbn_res(jres)%scbn < 0.) scbn_res(jres)%scbn  = 0.
       
           cbn_rec%bed_DIC      = dic_bed           !DIC release from bed sediment         
           dic_con              = dic_con + cbn_rec%Atm_DIC + cbn_rec%LP_DIC           &
                                + cbn_rec%RP_DIC + cbn_rec%LD_DIC + cbn_rec%RD_DIC     & !transformations from organic carbon pools
                                + cbn_rec%Ab_DIC - cbn_rec%DIC_Ab + cbn_rec%bed_DIC   !update DIC concentration change after transformation             
           if (dic_con<0.) dic_con  = 0.  !Finish calculating carbon tranformations among different species
           ! calculating carbon amount leaving reservoir on day
           resrpoc              = rpoc_con * resflwo / 1000.0   !RPOC leaving reservoir on day [kg C]
           reslpoc              = lpoc_con * resflwo / 1000.0   !LPOC leaving reservoir on day [kg C]
           resrdoc              = rdoc_con * resflwo / 1000.0   !RDOC leaving reservoir on day [kg C]
           resldoc              = ldoc_con * resflwo / 1000.0   !LDOC leaving reservoir on day [kg C]
           resdic               = dic_con * resflwo / 1000.0     !DIC leaving reservoir on day [kg C] 
           !update carbon mass after transport by outflow
           cbn_res(jres)%RPOC   = res_vol(jres) * rpoc_con / 1000.0 - resrpoc
           if (cbn_res(jres)%RPOC < 0.) cbn_res(jres)%RPOC  = 0.
           cbn_res(jres)%LPOC   = res_vol(jres) * lpoc_con / 1000.0 - reslpoc
           if (cbn_res(jres)%LPOC < 0.) cbn_res(jres)%LPOC    = 0.
           cbn_res(jres)%RDOC   = res_vol(jres) * rdoc_con / 1000.0 - resrdoc
           if (cbn_res(jres)%RDOC < 0.) cbn_res(jres)%RDOC  = 0.
           cbn_res(jres)%LDOC   = res_vol(jres) * ldoc_con / 1000.0 - resldoc
           if (cbn_res(jres)%LDOC < 0.) cbn_res(jres)%LDOC    = 0.
           cbn_res(jres)%DIC    =res_vol(jres) * dic_con / 1000.0 - resdic
           if (cbn_res(jres)%DIC < 0.) cbn_res(jres)%DIC  = 0.
      endif !if (cswat == 2 ) then
   !------------Finish calculating carbon processes in the reservoir,added by Du-----------!
      return
end