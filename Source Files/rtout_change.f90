       subroutine rtout
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for reaches

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units      |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rch_NH4(:)   |mg N/L     |ammonia concentration in reach
!!    bury          |mg pst     |loss of pesticide from active sediment layer
!!                              |by burial
!!    ch_l2(:)      |km         |length of main channel
!!    ch_w(2,:)     |m          |average width of main channel
!!    rch_Chla(:)     |mg chl-a/L |chlorophyll-a concentration in reach
!!    difus         |mg pst     |diffusion of pesticide from sediment to reach
!!    rch_SolP(:)    |mg P/L     |dissolved phosphorus concentration in reach
!!    hbactlp(:)    |# cfu/100mL|less persistent bacteria in reach/outflow
!!                              |during hour
!!    hbactp(:)     |# cfu/100mL|persistent bacteria in reach/outflow during
!!                              |hour
!!    rch_hCBOD(:)       |mg O2/L    |carbonaceous biochemical oxygen demand in
!!                              |reach at end of hour
!!    rch_hChla(:)      |mg chl-a/L |chlorophyll-a concentration in reach at end of
!!                              |hour
!!    rch_hDOX(:)     |mg O2/L    |dissolved oxygen concentration in reach at
!!                              |end of hour
!!    rch_hNH4(:)       |mg N/L     |ammonia concentration in reach at end of hour
!!    rch_hNO2(:)       |mg N/L     |nitrite concentration in reach at end of hour
!!    rch_hNO3(:)       |mg N/L     |nitrate concentration in reach at end of hour
!!    rch_hOrgN(:)      |mg N/L     |organic nitrogen concentration in reach at
!!                              |end of hour
!!    rch_hOrgP(:)      |mg P/L     |organic phosphorus concentration in reach at
!!                              |end of hour
!!    hsedyld(:)    |metric tons|sediment transported out of reach during hour
!!    rch_hSolP(:)      |mg P/L     |dissolved phosphorus concentration in reach at
!!                              |end of hour
!!    hsolpst(:)    |mg pst/m^3 |soluble pesticide concentration in outflow
!!                              |on day
!!    hsorpst(:)    |mg pst/m^3 |sorbed pesticide concentration in outflow
!!                              |on day
!!    hrtwtr(:)     |m^3 H2O    |water leaving reach during hour
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihout         |none       |outflow hydrograph location
!!    inum1         |none       |reach number
!!    inum2         |none       |inflow hydrograph location
!!    rch_NO3(:)   |mg N/L     |nitrate concentration in reach
!!    rch_NO2(:)   |mg N/L     |nitrite concentration in reach
!!    rch_OrgN(:)   |mg N/L     |organic nitrogen concentration in reach
!!    rch_OrgP(:)   |mg P/L     |organic phosphorus concentration in reach
!!    rch_bactlp(:) |# cfu/100ml|less persistent bacteria in reach/outflow
!!                              |at end of day
!!    rch_bactp(:)  |# cfu/100ml|persistent bacteria in reach/outflow at end
!!                              |of day
!!    rch_CBOD(:)   |mg O2/L    |carbonaceous biochemical oxygen demand in
!!                              |reach
!!    rch_DOX(:)    |mg O2/L    |dissolved oxygen concentration in reach
!!    reactb        |mg pst     |amount of pesticide in sediment that is lost
!!                              |through reactions
!!    reactw        |mg pst     |amount of pesticide in reach that is lost
!!                              |through reactions
!!    resuspst      |mg pst     |amount of pesticide moving from sediment to
!!                              |reach due to resuspension
!!    rnum1         |none       |fraction of inflow that is overland flow
!!    rtevp         |m^3 H2O    |evaporation from reach on day
!!    rttlc         |m^3 H2O    |transmission losses from reach on day
!!    rtwtr         |m^3 H2O    |water leaving reach on day
!!    sedpst_act(:) |m          |depth of active sediment layer in reach for
!!                              |pesticide
!!    sedpst_conc(:)|mg/(m**3)  |inital pesticide concentration in river bed
!!                              |sediment
!!    sedrch        |metric tons|sediment transported out of channel
!!                              |during time step
!!    setlpst       |mg pst     |amount of pesticide moving from water to
!!                              |sediment due to settling
!!    solpesto      |mg pst/m^3 |soluble pesticide concentration in outflow
!!                              |on day
!!    sorpesto      |mg pst/m^3 |sorbed pesticide concentration in outflow
!!                              |on day
!!    volatpst      |mg pst     |amount of pesticide in reach lost by
!!                              |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units      |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhvaroute(1,:,:) |deg C      |temperature
!!    hhvaroute(2,:,:) |m^3 H2O    |water
!!    hhvaroute(3,:,:) |metric tons|sediment or suspended solid load
!!    hhvaroute(4,:,:) |kg N       |organic nitrogen
!!    hhvaroute(5,:,:) |kg P       |organic phosphorus
!!    hhvaroute(6,:,:) |kg N       |nitrate
!!    hhvaroute(7,:,:) |kg P       |mineral phosphorus
!!    hhvaroute(11,:,:)|mg pst     |pesticide in solution
!!    hhvaroute(12,:,:)|mg pst     |pesticide sorbed to sediment
!!    hhvaroute(13,:,:)|kg         |chlorophyll-a
!!    hhvaroute(16,:,:)|kg         |carbonaceous biological oxygen demand
!!    hhvaroute(17,:,:)|kg         |dissolved oxygen
!!    hhvaroute(18,:,:)|# cfu/100ml|persistent bacteria
!!    hhvaroute(19,:,:)|# cfu/100ml|less persistent bacteria
!!    hhvaroute(20,:,:)|kg         |conservative metal #1
!!    hhvaroute(21,:,:)|kg         |conservative metal #2
!!    hhvaroute(22,:,:)|kg         |conservative metal #3
!!    rchdy(1,:)       |m^3/s      |flow into reach on day
!!    rchdy(2,:)       |m^3/s      |flow out of reach on day
!!    rchdy(3,:)       |m^3/s      |evaporation from reach on day
!!    rchdy(4,:)       |m^3/s      |transmission losses from reach on day
!!    rchdy(5,:)       |metric tons|sediment transported into reach on day
!!    rchdy(6,:)       |metric tons|sediment transported out of reach on day
!!    rchdy(7,:)       |mg/L       |sediment concentration in outflow
!!    rchdy(8,:)       |kg N       |organic N transported into reach on day
!!    rchdy(9,:)       |kg N       |organic N transported out of reach on day
!!    rchdy(10,:)      |kg P       |organic P transported into reach on day
!!    rchdy(11,:)      |kg P       |organic P transported out of reach on day
!!    rchdy(12,:)      |kg N       |nitrate transported into reach on day
!!    rchdy(13,:)      |kg N       |nitrate transported out of reach on day
!!    rchdy(14,:)      |kg N       |ammonia transported into reach on day
!!    rchdy(15,:)      |kg N       |ammonia transported out of reach on day
!!    rchdy(16,:)      |kg N       |nitrite transported into reach on day
!!    rchdy(17,:)      |kg N       |nitrite transported out of reach on day
!!    rchdy(18,:)      |kg P       |soluble P transported into reach on day
!!    rchdy(19,:)      |kg P       |soluble P transported out of reach on day
!!    rchdy(20,:)      |kg chla    |chlorophyll-a transported into reach on day
!!    rchdy(21,:)      |kg chla    |chlorophyll-a transported out of reach on 
!!                                 |day
!!    rchdy(22,:)      |kg O2      |CBOD transported into reach on day
!!    rchdy(23,:)      |kg O2      |CBOD transported out of reach on day
!!    rchdy(24,:)      |kg O2      |dissolved oxygen transported into reach on 
!!                                 |day
!!    rchdy(25,:)      |kg O2      |dissolved oxygen transported out of reach on
!!                                 |day
!!    rchdy(26,:)      |mg pst     |soluble pesticide transported into reach on
!!                                 |day
!!    rchdy(27,:)      |mg pst     |soluble pesticide transported out of reach
!!                                 |on day
!!    rchdy(28,:)      |mg pst     |sorbed pesticide transported into reach on 
!!                                 |day
!!    rchdy(29,:)      |mg pst     |sorbed pesticide transported out of reach on
!!                                 |day
!!    rchdy(30,:)      |mg pst     |amount of pesticide lost through reactions
!!                                 |in reach on day
!!    rchdy(31,:)      |mg pst     |amount of pesticide lost through 
!!                                 |volatilization from reach on day
!!    rchdy(32,:)      |mg pst     |amount of pesticide settling out of reach to
!!                                 |bed sediment on day
!!    rchdy(33,:)      |mg pst     |amount of pesticide resuspended from bed
!!                                 |sediment to reach on day
!!    rchdy(34,:)      |mg pst     |amount of pesticide diffusing from reach to
!!                                 |bed sediment on day
!!    rchdy(35,:)      |mg pst     |amount of pesticide in sediment layer lost 
!!                                 |through reactions on day
!!    rchdy(36,:)      |mg pst     |amount of pesticide in sediment layer lost
!!                                 |through burial on day
!!    rchdy(37,:)      |mg pst     |amount of pesticide stored in river bed
!!                                 |sediments
!!    rchdy(38,:)      |# cfu/100mL|persistent bacteria transported out of reach
!!                                 |on day
!!    rchdy(39,:)      |# cfu/100mL|less persistent bacteria transported out of
!!                                 |reach on day
!!    rchdy(40,:)      |kg         |amount of conservative metal #1 transported
!!                                 |out of reach on day
!!    rchdy(41,:)      |kg         |amount of conservative metal #2 transported
!!                                 |out of reach on day
!!    rchdy(42,:)      |kg         |amount of conservative metal #3 transported
!!                                 |out of reach on day
!!    rchmono(1,:)     |m^3/s      |flow into reach during month
!!    rchmono(2,:)     |m^3/s      |flow out of reach during month
!!    rchmono(3,:)     |metric tons|sediment transported into reach during month
!!    rchmono(4,:)     |metric tons|sediment transported out of reach during 
!!                                 |month
!!    rchmono(5,:)     |mg/L       |sediment concentration in outflow during 
!!                                 |month
!!    rchmono(6,:)     |kg N       |organic N transported into reach during 
!!                                 |month
!!    rchmono(7,:)     |kg N       |organic N transported out of reach during 
!!                                 |month
!!    rchmono(8,:)     |kg P       |organic P transported into reach during 
!!                                 |month
!!    rchmono(9,:)     |kg P       |organic P transported out of reach during
!!                                 |month
!!    rchmono(10,:)    |m^3/s      |evaporation from reach during month
!!    rchmono(11,:)    |m^3/s      |transmission losses from reach during month
!!    rchmono(12,:)    |kg         |conservative metal #1 transported out of
!!                                 |reach during month
!!    rchmono(13,:)    |kg         |conservative metal #2 transported out of
!!                                 |reach during month
!!    rchmono(14,:)    |kg         |conservative metal #3 transported out of
!!                                 |reach during month
!!    rchmono(15,:)    |kg N       |nitrate transported into reach during month
!!    rchmono(16,:)    |kg N       |nitrate transported out of reach during 
!!                                 |month
!!    rchmono(17,:)    |kg P       |soluble P transported into reach during 
!!                                 |month
!!    rchmono(18,:)    |kg P       |soluble P transported out of reach during 
!!                                 |month
!!    rchmono(19,:)    |mg pst     |soluble pesticide transported into reach
!!                                 |during month
!!    rchmono(20,:)    |mg pst     |soluble pesticide transported out of reach
!!                                 |during month
!!    rchmono(21,:)    |mg pst     |sorbed pesticide transported into reach
!!                                 |during month
!!    rchmono(22,:)    |mg pst     |sorbed pesticide transported out of reach
!!                                 |during month
!!    rchmono(23,:)    |mg pst     |amount of pesticide lost through reactions
!!                                 |in reach during month
!!    rchmono(24,:)    |mg pst     |amount of pesticide lost through
!!                                 |volatilization from reach during month
!!    rchmono(25,:)    |mg pst     |amount of pesticide settling out of reach to
!!                                 |bed sediment during month
!!    rchmono(26,:)    |mg pst     |amount of pesticide resuspended from bed
!!                                 |sediment to reach during month
!!    rchmono(27,:)    |mg pst     |amount of pesticide diffusing from reach to
!!                                 |bed sediment during month
!!    rchmono(28,:)    |mg pst     |amount of pesticide in sediment layer lost
!!                                 |through reactions during month
!!    rchmono(29,:)    |mg pst     |amount of pesticide in sediment layer lost
!!                                 |through burial during month
!!    rchmono(30,:)    |kg chla    |chlorophyll-a transported into reach during
!!                                 |month
!!    rchmono(31,:)    |kg chla    |chlorophyll-a transported out of reach 
!!                                 |during month
!!    rchmono(32,:)    |kg N       |ammonia transported into reach during month
!!    rchmono(33,:)    |kg N       |ammonia transported out of reach during 
!!                                 |month
!!    rchmono(34,:)    |kg N       |nitrite transported into reach during month
!!    rchmono(35,:)    |kg N       |nitrite transported out of reach during 
!!                                 |month
!!    rchmono(36,:)    |kg O2      |CBOD transported into reach during month
!!    rchmono(37,:)    |kg O2      |CBOD transported out of reach during month
!!    rchmono(38,:)    |kg O2      |dissolved oxygen transported into reach
!!                                 |during month
!!    rchmono(39,:)    |kg O2      |dissolved oxygen transported out of reach
!!                                 |during month
!!    rchmono(40,:)    |# cfu/100mL|persistent bacteria transported out of reach
!!                                 |during month
!!    rchmono(41,:)    |# cfu/100mL|less persistent bacteria transported out of
!!                                 |reach during month
!!    varoute(2,:)     |m^3 H2O    |water
!!    varoute(3,:)     |metric tons|sediment or suspended solid load
!!    varoute(4,:)     |kg N       |organic nitrogen
!!    varoute(5,:)     |kg P       |organic phosphorus
!!    varoute(6,:)     |kg N       |nitrate
!!    varoute(7,:)     |kg P       |soluble phosphorus
!!    varoute(11,:)    |mg pst     |pesticide in solution
!!    varoute(12,:)    |mg pst     |pesticide sorbed to sediment
!!    varoute(13,:)    |kg         |chlorophyll-a
!!    varoute(14,:)    |kg N       |ammonia
!!    varoute(15,:)    |kg N       |nitrite
!!    varoute(16,:)    |kg         |carbonaceous biological oxygen demand
!!    varoute(17,:)    |kg         |dissolved oxygen
!!    varoute(18,:)    |# cfu/100mL|persistent bacteria
!!    varoute(19,:)    |# cfu/100mL|less persistent bacteria
!!    varoute(20,:)    |kg         |conservative metal #1
!!    varoute(21,:)    |kg         |conservative metal #2
!!    varoute(22,:)    |kg         |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bedvol      |m^3           |volume of river bed sediment
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    sedcon      |mg/L          |sediment concentration in outflow
!!    sedpest     |mg pst        |pesticide in river bed sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para   !! added by Du
      use parm
       use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none

      integer :: jrch, ii
      real :: sedcon, bedvol, sedpest, wtmp
      real:: wtrin,sub_ha    !!
      
    
      jrch = 0
      jrch = inum1
      sub_ha = 0. 
      sub_ha = da_ha * sub_fr(jrch)  

      varoute(1,ihout) = wattemp(jrch)
      varoute(2,ihout) = rtwtr       !|m^3 H2O       |water leaving reach on day       
      varoute(3,ihout) = sedrch      !|metric tons   |sediment transported out of reach on day   
      varoute(8,ihout) = 0.
      varoute(9,ihout) = 0.
      varoute(10,ihout) = 0.
      varoute(18,ihout) = rch_bactp(jrch)           !|# cfu/100mL|persistent bacteria
      varoute(19,ihout) = rch_bactlp(jrch)          !|# cfu/100mL|less persistent bacteria
      varoute(20,ihout) = varoute(20,inum2) * (1. - rnum1)    !kg conservative metal #1
      varoute(21,ihout) = varoute(21,inum2) * (1. - rnum1)    !kg conservative metal #2
      varoute(22,ihout) = varoute(22,inum2) * (1. - rnum1)    !kg conservative metal #3
!!    sediment routing
      varoute(23,ihout) = rch_san
      varoute(24,ihout) = rch_sil
      varoute(25,ihout) = rch_cla
      varoute(26,ihout) = rch_sag
      varoute(27,ihout) = rch_lag
      varoute(28,ihout) = rch_gra
  
      if (ievent_rch == 0) then    !!  for daily simulation    
       varoute(4,ihout) = rch_OrgN(jrch) * rtwtr / 1000. + ch_orgn(jrch)  !kg C     !rch_OrgN(:) |mg N/L        |organic nitrogen concentration in reach
       varoute(5,ihout) = rch_OrgP(jrch) * rtwtr / 1000. + ch_orgp(jrch)  !kg C  
       varoute(6,ihout) = rch_NO3(jrch) * rtwtr / 1000.     !kg C 
       varoute(7,ihout) = rch_SolP(jrch) * rtwtr / 1000.    !kg C 
       varoute(11,ihout) = solpesto * rtwtr                  
       varoute(12,ihout) = sorpesto * rtwtr           
       varoute(13,ihout) = rch_Chla(jrch) * rtwtr / 1000.
       varoute(14,ihout) = rch_NH4(jrch) * rtwtr / 1000.
       varoute(15,ihout) = rch_NO2(jrch) * rtwtr / 1000.
       varoute(16,ihout) = rch_CBOD(jrch) *  rtwtr/ 1000.
       varoute(17,ihout) = rch_DOX(jrch) *  rtwtr/ 1000.

       varoute(33,ihout)= cbn_rch(jrch)%RPOC*rtwtr/ 1000.  !kg C 
       varoute(34,ihout)= cbn_rch(jrch)%LPOC*rtwtr/ 1000.  !kg C
       varoute(35,ihout)= cbn_rch(jrch)%RDOC*rtwtr/ 1000.  !kg C
       varoute(36,ihout)= cbn_rch(jrch)%LDOC*rtwtr/ 1000.  !kg C
       varoute(37,ihout)= cbn_rch(jrch)%DIC*rtwtr/ 1000.   !kg C
     
    
       
      else   !!  for subdaily simulation
       do ii = 1, nstep_rch 
          hhvaroute(1,ihout,ii) = rch_hwattemp(inum1,ii)           !! 
          hhvaroute(2,ihout,ii) = hrtwtr(ii)     ! urban modeling by J.Jeong
          hhvaroute(3,ihout,ii) = hsedyld(ii)    ! urban modeling by J.Jeong 
          hhvaroute(4,ihout,ii) = rch_hOrgN(ii) * hrtwtr(ii) / 1000.
          hhvaroute(5,ihout,ii) = rch_hOrgP(ii) *  hrtwtr(ii) / 1000.
          hhvaroute(6,ihout,ii) = rch_hNO3(ii) * hrtwtr(ii) / 1000.
          hhvaroute(7,ihout,ii) = rch_hSolP(ii) * hrtwtr(ii) / 1000.
          hhvaroute(8,ihout,ii) = 0.
          hhvaroute(9,ihout,ii) = 0.
          hhvaroute(10,ihout,ii) = 0.
          hhvaroute(11,ihout,ii) = hsolpst(ii) * hrtwtr(ii)
          hhvaroute(12,ihout,ii) = hsorpst(ii) * hrtwtr(ii)
          hhvaroute(13,ihout,ii) = rch_hChla(ii) * hrtwtr(ii) / 1000.
          hhvaroute(14,ihout,ii) = rch_hNH4(ii) * hrtwtr(ii) / 1000.
          hhvaroute(15,ihout,ii) = rch_hNO2(ii) * hrtwtr(ii) / 1000.
          hhvaroute(16,ihout,ii) = rch_hCBOD(ii) *  hrtwtr(ii)/ 1000.
          hhvaroute(17,ihout,ii) = rch_hDOX(ii) *  hrtwtr(ii)/ 1000.
          hhvaroute(18,ihout,ii) = hbactp(ii)
          hhvaroute(19,ihout,ii) = hbactlp(ii)
          hhvaroute(20,ihout,ii) = hhvaroute(20,inum2,ii) * (1. - rnum1)
          hhvaroute(21,ihout,ii) = hhvaroute(21,inum2,ii) * (1. - rnum1)
          hhvaroute(22,ihout,ii) = hhvaroute(22,inum2,ii) * (1. - rnum1)   
          !!--------C/N/P----------
          hhvaroute(33,ihout,ii) = rch_hRPOC(ii) * hrtwtr(ii) / 1000.
          hhvaroute(34,ihout,ii) = rch_hLPOC(ii) * hrtwtr(ii) / 1000.
          hhvaroute(35,ihout,ii) = rch_hRDOC(ii) * hrtwtr(ii) / 1000.
          hhvaroute(36,ihout,ii) = rch_hLDOC(ii) *  hrtwtr(ii)/ 1000.
          hhvaroute(37,ihout,ii) = rch_hDIC(ii) *  hrtwtr(ii)/ 1000.
          !hhvaroute(38,ihout,ii) = rch_hCH4s(ii) *  hrtwtr(ii)/ 1000.    
          !hhvaroute(39,ihout,ii) = rch_hRPON(ii) * hrtwtr(ii) / 1000.
          !hhvaroute(40,ihout,ii) = rch_hLPON(ii) * hrtwtr(ii) / 1000.
          !hhvaroute(41,ihout,ii) = rch_hRDON(ii) * hrtwtr(ii) / 1000.
          !hhvaroute(42,ihout,ii) = rch_hLDON(ii) *  hrtwtr(ii)/ 1000.
          !hhvaroute(43,ihout,ii) = rch_hDIN(ii) *  hrtwtr(ii)/ 1000.
          !hhvaroute(44,ihout,ii) = rch_hN2O(ii) *  hrtwtr(ii)/ 1000.
          !hhvaroute(45,ihout,ii) = rch_hN2(ii) *  hrtwtr(ii)/ 1000.
          !hhvaroute(46,ihout,ii) = rch_hRPOP(ii) * hrtwtr(ii) / 1000.
          !hhvaroute(47,ihout,ii) = rch_hLPOP(ii) * hrtwtr(ii) / 1000.
          !hhvaroute(48,ihout,ii) = rch_hRDOP(ii) * hrtwtr(ii) / 1000.
          !hhvaroute(49,ihout,ii) = rch_hLDOP(ii) *  hrtwtr(ii)/ 1000.
          !hhvaroute(50,ihout,ii) = rch_hDIP(ii) *  hrtwtr(ii)/ 1000.
         

          !! Accumulated daily output
          !varoute(1,ihout) = varoute(1,ihout) + hhvaroute(1,ihout,ii)   !wattemp(jrch) 
          !varoute(2,ihout) = varoute(2,ihout) + hhvaroute(2,ihout,ii)   !rtwtr
          !varoute(3,ihout) = varoute(3,ihout) + hhvaroute(3,ihout,ii)   !sedrch
          varoute(4,ihout) = varoute(4,ihout) + hhvaroute(4,ihout,ii)
          varoute(5,ihout) = varoute(5,ihout) + hhvaroute(5,ihout,ii)
          varoute(6,ihout) = varoute(6,ihout) + hhvaroute(6,ihout,ii)
          varoute(7,ihout) = varoute(7,ihout) + hhvaroute(7,ihout,ii)
          varoute(11,ihout) = varoute(11,ihout) + hhvaroute(11,ihout,ii)
          varoute(12,ihout) = varoute(12,ihout) + hhvaroute(12,ihout,ii)
          varoute(13,ihout) = varoute(13,ihout) + hhvaroute(13,ihout,ii)
          varoute(14,ihout) = varoute(14,ihout) + hhvaroute(14,ihout,ii)
          varoute(15,ihout) = varoute(15,ihout) + hhvaroute(15,ihout,ii)
          varoute(16,ihout) = varoute(16,ihout) + hhvaroute(16,ihout,ii)
          varoute(17,ihout) = varoute(17,ihout) + hhvaroute(17,ihout,ii)  
          !! Junyu Qi--------C/N/P----------    
          varoute(33,ihout)=varoute(33,ihout) + hhvaroute(33,ihout,ii)                                 !  cbn_rch(jrch)%RPOC*rtwtr/ 1000.  !kg C 
          varoute(34,ihout)=varoute(34,ihout) + hhvaroute(34,ihout,ii)                                 !  cbn_rch(jrch)%LPOC*rtwtr/ 1000.  !kg C
          varoute(35,ihout)=varoute(35,ihout) + hhvaroute(35,ihout,ii)                                 !  cbn_rch(jrch)%RDOC*rtwtr/ 1000.  !kg C
          varoute(36,ihout)=varoute(36,ihout) + hhvaroute(36,ihout,ii)                                 !  cbn_rch(jrch)%LDOC*rtwtr/ 1000.  !kg C
          varoute(37,ihout)=varoute(37,ihout) + hhvaroute(37,ihout,ii)                                 !  cbn_rch(jrch)%DIC*rtwtr/ 1000.   !kg C
          varoute(38,ihout)=varoute(38,ihout) + hhvaroute(38,ihout,ii)                                 !  CH4   
          varoute(39,ihout)=varoute(39,ihout) + hhvaroute(39,ihout,ii)                                 !  RDON  kgN
          varoute(40,ihout)=varoute(40,ihout) + hhvaroute(40,ihout,ii)                                 !  LPON  kgN
          varoute(41,ihout)=varoute(41,ihout) + hhvaroute(41,ihout,ii)                                 !  RDON  kgN
          varoute(42,ihout)=varoute(42,ihout) + hhvaroute(42,ihout,ii)                                 !  LDON  kgN
          varoute(43,ihout)=varoute(43,ihout) + hhvaroute(43,ihout,ii)                                 !  DIN  kgN
          varoute(44,ihout)=varoute(44,ihout) + hhvaroute(44,ihout,ii)                                 !  N2O  kgN
          varoute(45,ihout)=varoute(45,ihout) + hhvaroute(45,ihout,ii)                                 !  N2  kgN
          varoute(46,ihout)=varoute(46,ihout) + hhvaroute(46,ihout,ii)                                 !  RDOP  kgP
          varoute(47,ihout)=varoute(47,ihout) + hhvaroute(47,ihout,ii)                                 !  LPOP kgP
          varoute(48,ihout)=varoute(48,ihout) + hhvaroute(48,ihout,ii)                                 !  RDOP  kgP
          varoute(49,ihout)=varoute(49,ihout) + hhvaroute(49,ihout,ii)                                 !  LDOP  kgP
          varoute(50,ihout)=varoute(50,ihout) + hhvaroute(50,ihout,ii)                                 !  DIP  kgP

        end do
       
      end if



!! set subdaily reach output    - by jaehak jeong for urban project, subdaily output in output.rch file
	if ( ievent_rch==1  .and.   iprint==3 ) then
	
	  do ii=1,nstep_rch
!! determine sediment concentration in outflow
          sedcon = 0.
          if (hrtwtr(ii) > 0.01) then
            sedcon = hsedyld(ii) / hrtwtr(ii) * 1.e6
          else
            sedcon = 0.
          end if
          rchhr(1,jrch,ii) = hhvaroute(2,inum2,ii) * (1. - rnum1) / (idt_rch * 60.) !!flow in (m^3/s)	
           		       
          rchhr(2,jrch,ii) = hrtwtr(ii) / (idt_rch * 60.)            !!flow out (m^3/s)
          rchhr(3,jrch,ii) = hrtevp(ii) / (idt_rch * 60.)            !!evap (m^3/s)
          rchhr(4,jrch,ii) = hrttlc(ii) / (idt_rch * 60.)            !!tloss (m^3/s)
          rchhr(5,jrch,ii) = hhvaroute(3,inum2,ii) * (1. - rnum1)   !!sed in (tons)
          rchhr(6,jrch,ii) = hsedyld(ii)                         !!sed out (tons)
          rchhr(7,jrch,ii) = sedcon						       !!sed conc (mg/L)
	 
	  ! rchhr(73, jrch,ii) = hhstor(ii)
        !  rchhr(74, jrch,ii) = hharea
        ! rchhr(75, jrch,ii) = hdepth
        ! rchhr(76, jrch,ii) = hsdti
          
	!  if (jrch==1) then
	!  print*, jrch,i,ii,hhstor(ii),  rchstor(jrch)!,hharea,hdepth,hsdti
	!  endif 
	  
	  end do
	endif     !if (ievent_rch==1.and.iprint==3) then

!! determine sediment concentration in outflow
      sedcon = 0.
      if (rtwtr > 0.01) then
        sedcon = sedrch / rtwtr * 1.e6
      else
        sedcon = 0.
      end if
      if (sedcon > 200000.) sedcon = 200000.

!! determine amount of pesticide in river bed sediments
      bedvol = 0.
      sedpest = 0.
      bedvol = ch_w(2,jrch) * ch_l2(jrch) * 1000. * sedpst_act(jrch)
      sedpest = sedpst_conc(jrch) * bedvol

!! set daily reach output
      rchdy(1,jrch) = varoute(2,inum2) * (1. - rnum1) / 86400. !!flow in (m^3/s)
      rchdy(2,jrch) = rtwtr / 86400.                                          !!flow out (m^3/s)
      rchdy(3,jrch) = rtevp / 86400.                     !!evap (m^3/s)
      rchdy(4,jrch) = rttlc / 86400.                     !!tloss (m^3/s)
      rchdy(5,jrch) = varoute(3,inum2) * (1. - rnum1)    !!sed in (tons)
      rchdy(6,jrch) = sedrch                             !!sed out (tons)
      rchdy(7,jrch) = sedcon                             !!sed conc (mg/L)
      rchdy(8,jrch) = varoute(4,inum2) * (1. - rnum1)    !!orgN in (kg N)
      rchdy(9,jrch) = varoute(4,ihout)                   !!orgN out (kg N)
      rchdy(10,jrch) = varoute(5,inum2) * (1. - rnum1)   !!orgP in (kg P)
      rchdy(11,jrch) = varoute(5,ihout)                  !!orgP out (kg P)
      rchdy(12,jrch) = varoute(6,inum2) * (1. - rnum1)   !!NO3 in (kg N)
      rchdy(13,jrch) = varoute(6,ihout)                  !!NO3 out (kg N)
      rchdy(14,jrch) = varoute(14,inum2) * (1. - rnum1)  !!NH4 in (kg)
      rchdy(15,jrch) = varoute(14,ihout)                 !!NH4 out (kg)
      rchdy(16,jrch) = varoute(15,inum2) * (1. - rnum1)  !!NO2 in (kg)
      rchdy(17,jrch) = varoute(15,ihout)                 !!NO2 out (kg)
      rchdy(18,jrch) = varoute(7,inum2) * (1. - rnum1)   !!solP in (kg P)
      rchdy(19,jrch) = varoute(7,ihout)                  !!solP out (kg P)
      rchdy(20,jrch) = varoute(13,inum2) * (1. - rnum1)  !!chl-a in (kg)
      rchdy(21,jrch) = varoute(13,ihout)                 !!chl-a out (kg)
      rchdy(22,jrch) = varoute(16,inum2) * (1. - rnum1)  !!CBOD in (kg)
      rchdy(23,jrch) = varoute(16,ihout)                 !!CBOD out (kg)
      rchdy(24,jrch) = varoute(17,inum2) * (1. - rnum1)  !!dis O2 in (kg)
      rchdy(25,jrch) = varoute(17,ihout)                 !!dis O2 out (kg)
      rchdy(26,jrch) = varoute(11,inum2) * (1. - rnum1)  !!solpst in (mg pst)
      rchdy(27,jrch) = varoute(11,ihout)                 !!solpst out (mg pst)
      rchdy(28,jrch) = varoute(12,inum2) * (1. - rnum1)  !!srbpst in (mg pst)
      rchdy(29,jrch) = varoute(12,ihout)                 !!srbpst out (mg pst)
      rchdy(30,jrch) = reactw                            !!reacted pst (mg pst)
      rchdy(31,jrch) = volatpst                          !!volatilized pst (mg)
      rchdy(32,jrch) = setlpst                           !!pst settling (mg pst)
      rchdy(33,jrch) = resuspst                          !!pst resuspension (mg)
      rchdy(34,jrch) = -difus                            !!pst diffuse to sed mg
      rchdy(35,jrch) = reactb                            !!react pst/sed (mg)
      rchdy(36,jrch) = bury                              !!pst bury (mg)
      rchdy(37,jrch) = sedpest                           !!pst in rivbed sed mg
      rchdy(38,jrch) = varoute(18,ihout)                 !!persistent bact out
      rchdy(39,jrch) = varoute(19,ihout)                 !!lpersistent bact out
      rchdy(40,jrch) = varoute(20,ihout)                 !!cmetal #1
      rchdy(41,jrch) = varoute(21,ihout)                 !!cmetal #2
      rchdy(42,jrch) = varoute(22,ihout)                 !!cmetal #3
      rchdy(60,jrch) = varoute(1,ihout)                  !!water temp deg c
      !!--------C/N/P---------- 
      rchdy(63,jrch) = varoute(33,inum2) * (1. - rnum1)  !!RPOC in (kg C)
      rchdy(64,jrch) = varoute(33,ihout)                 !!RPOC out (kg C)
      rchdy(65,jrch) = varoute(34,inum2) * (1. - rnum1)  !!LPOC in (kg C)
      rchdy(66,jrch) = varoute(34,ihout)                 !!LPOC out (kg C)
      rchdy(67,jrch) = varoute(35,inum2) * (1. - rnum1)  !!RDOC in (kg C)
      rchdy(68,jrch) = varoute(35,ihout)                 !!RDOC out (kg C)
      rchdy(69,jrch) = varoute(36,inum2) * (1. - rnum1)  !!LDOC in (kg C)
      rchdy(70,jrch) = varoute(36,ihout)                 !!LDOC out (kg C)
      rchdy(71,jrch) = varoute(37,inum2) * (1. - rnum1)  !!DIC in (kg C))
      rchdy(72,jrch) = varoute(37,ihout)                 !!DIC out (kg C)

      !rchdy(73,jrch) = varoute(38,ihout)
      !rchdy(74,jrch) = varoute(39,ihout)
      !rchdy(75,jrch) = varoute(40,ihout) 
      !rchdy(76,jrch) = varoute(41,ihout)
      

    
    
!!    sediment routing 
!!    Assumed all silt for default sediment routine
!!    For other sediment routing models particle size are tracked
       if (ch_eqn(jrch) .NE. 0) then
       rchdy(43,jrch) = varoute(23,inum2) * (1. - rnum1)  !!sand in   
       rchdy(44,jrch) = varoute(23,ihout)                 !!sand out   
       rchdy(45,jrch) = varoute(24,inum2) * (1. - rnum1)  !!silt in    
       rchdy(46,jrch) = varoute(24,ihout)                 !!silt out   
       rchdy(47,jrch) = varoute(25,inum2) * (1. - rnum1)  !!clay in    
       rchdy(48,jrch) = varoute(25,ihout)                 !!clay out   
       rchdy(49,jrch) = varoute(26,inum2) * (1. - rnum1)  !!sm ag in   
       rchdy(50,jrch) = varoute(26,ihout)                 !!sm ag out 
       rchdy(51,jrch) = varoute(27,inum2) * (1. - rnum1)  !!lg ag in  
       rchdy(52,jrch) = varoute(27,ihout)                 !!lg ag out  
       rchdy(53,jrch) = varoute(28,inum2) * (1. - rnum1)  !!gravel in
       rchdy(54,jrch) = varoute(28,ihout)                 !!gravel out
	 else
       rchdy(43,jrch) = 0.                 !!sand in   
       rchdy(44,jrch) = 0.                 !!sand out   
       rchdy(45,jrch) = varoute(3,inum2) * (1. - rnum1)   !!silt in    
       rchdy(46,jrch) = varoute(3,ihout)                  !!silt out   
       rchdy(47,jrch) = 0.                 !!clay in    
       rchdy(48,jrch) = 0.                 !!clay out   
       rchdy(49,jrch) = 0.                 !!sm ag in   
       rchdy(50,jrch) = 0.                 !!sm ag out 
       rchdy(51,jrch) = 0.                 !!lg ag in  
       rchdy(52,jrch) = 0.                 !!lg ag out  
       rchdy(53,jrch) = 0.                 !!gravel in
       rchdy(54,jrch) = 0.                 !!gravel out
       end if


    
!! summarize monthly reach output
      rchmono(1,jrch) = rchmono(1,jrch) + rchdy(1,jrch)
      rchmono(2,jrch) = rchmono(2,jrch) + rchdy(2,jrch)
      rchmono(3,jrch) = rchmono(3,jrch) + rchdy(5,jrch)
      rchmono(4,jrch) = rchmono(4,jrch) + rchdy(6,jrch)
      rchmono(5,jrch) = rchmono(5,jrch) + rchdy(7,jrch)
      rchmono(6,jrch) = rchmono(6,jrch) + rchdy(8,jrch)
      rchmono(7,jrch) = rchmono(7,jrch) + rchdy(9,jrch)
      rchmono(8,jrch) = rchmono(8,jrch) + rchdy(10,jrch)
      rchmono(9,jrch) = rchmono(9,jrch) + rchdy(11,jrch)
      rchmono(10,jrch) = rchmono(10,jrch) + rchdy(3,jrch)
      rchmono(11,jrch) = rchmono(11,jrch) + rchdy(4,jrch)
      rchmono(12,jrch) = rchmono(12,jrch) + rchdy(40,jrch)
      rchmono(13,jrch) = rchmono(13,jrch) + rchdy(41,jrch)
      rchmono(14,jrch) = rchmono(14,jrch) + rchdy(42,jrch)
      rchmono(15,jrch) = rchmono(15,jrch) + rchdy(12,jrch)
      rchmono(16,jrch) = rchmono(16,jrch) + rchdy(13,jrch)
      rchmono(17,jrch) = rchmono(17,jrch) + rchdy(18,jrch)
      rchmono(18,jrch) = rchmono(18,jrch) + rchdy(19,jrch)
      rchmono(19,jrch) = rchmono(19,jrch) + rchdy(26,jrch)
      rchmono(20,jrch) = rchmono(20,jrch) + rchdy(27,jrch)
      rchmono(21,jrch) = rchmono(21,jrch) + rchdy(28,jrch)
      rchmono(22,jrch) = rchmono(22,jrch) + rchdy(29,jrch)
      rchmono(23,jrch) = rchmono(23,jrch) + rchdy(30,jrch)
      rchmono(24,jrch) = rchmono(24,jrch) + rchdy(31,jrch)
      rchmono(25,jrch) = rchmono(25,jrch) + rchdy(32,jrch)
      rchmono(26,jrch) = rchmono(26,jrch) + rchdy(33,jrch)
      rchmono(27,jrch) = rchmono(27,jrch) + rchdy(34,jrch)
      rchmono(28,jrch) = rchmono(28,jrch) + rchdy(35,jrch)
      rchmono(29,jrch) = rchmono(29,jrch) + rchdy(36,jrch)
      rchmono(30,jrch) = rchmono(30,jrch) + rchdy(20,jrch)
      rchmono(31,jrch) = rchmono(31,jrch) + rchdy(21,jrch)
      rchmono(32,jrch) = rchmono(32,jrch) + rchdy(14,jrch)
      rchmono(33,jrch) = rchmono(33,jrch) + rchdy(15,jrch)
      rchmono(34,jrch) = rchmono(34,jrch) + rchdy(16,jrch)
      rchmono(35,jrch) = rchmono(35,jrch) + rchdy(17,jrch)
      rchmono(36,jrch) = rchmono(36,jrch) + rchdy(22,jrch)
      rchmono(37,jrch) = rchmono(37,jrch) + rchdy(23,jrch)
      rchmono(38,jrch) = rchmono(38,jrch) + rchdy(24,jrch)
      rchmono(39,jrch) = rchmono(39,jrch) + rchdy(25,jrch)
      rchmono(40,jrch) = rchmono(40,jrch) + rchdy(38,jrch)
      rchmono(41,jrch) = rchmono(41,jrch) + rchdy(39,jrch)
      !!---C-----------!  
      rchmono(63,jrch) = rchmono(63,jrch) + rchdy(63,jrch)
      rchmono(64,jrch) = rchmono(64,jrch) + rchdy(64,jrch)
      rchmono(65,jrch) = rchmono(65,jrch) + rchdy(65,jrch)
      rchmono(66,jrch) = rchmono(66,jrch) + rchdy(66,jrch)
      rchmono(67,jrch) = rchmono(67,jrch) + rchdy(67,jrch)
      rchmono(68,jrch) = rchmono(68,jrch) + rchdy(68,jrch)
      rchmono(69,jrch) = rchmono(69,jrch) + rchdy(69,jrch)
      rchmono(70,jrch) = rchmono(70,jrch) + rchdy(70,jrch)
      rchmono(71,jrch) = rchmono(71,jrch) + rchdy(71,jrch)
      rchmono(72,jrch) = rchmono(72,jrch) + rchdy(72,jrch)
      !rchmono(73,jrch) = rchmono(73,jrch) + rchdy(73,jrch)
      ! rchmono(74,jrch) = rchmono(74,jrch) + rchdy(74,jrch)
      ! rchmono(75,jrch) = rchmono(75,jrch) + rchdy(75,jrch)
      ! rchmono(76,jrch) = rchmono(76,jrch) + rchdy(76,jrch)
      

!!    sediment routing
       rchmono(42,jrch) = rchmono(42,jrch) + rchdy(43,jrch)
       rchmono(43,jrch) = rchmono(43,jrch) + rchdy(44,jrch)
       rchmono(44,jrch) = rchmono(44,jrch) + rchdy(45,jrch)
       rchmono(45,jrch) = rchmono(45,jrch) + rchdy(46,jrch)
       rchmono(46,jrch) = rchmono(46,jrch) + rchdy(47,jrch)
       rchmono(47,jrch) = rchmono(47,jrch) + rchdy(48,jrch)
       rchmono(48,jrch) = rchmono(48,jrch) + rchdy(49,jrch)
       rchmono(49,jrch) = rchmono(49,jrch) + rchdy(50,jrch)
       rchmono(50,jrch) = rchmono(50,jrch) + rchdy(51,jrch)
       rchmono(51,jrch) = rchmono(51,jrch) + rchdy(52,jrch)
       rchmono(52,jrch) = rchmono(52,jrch) + rchdy(53,jrch)
       rchmono(53,jrch) = rchmono(53,jrch) + rchdy(54,jrch)
       rchmono(54,jrch) = rchmono(54,jrch) + rchdy(55,jrch)
       rchmono(55,jrch) = rchmono(55,jrch) + rchdy(56,jrch)
       rchmono(56,jrch) = rchmono(56,jrch) + rchdy(57,jrch)
       rchmono(57,jrch) = rchmono(57,jrch) + rchdy(58,jrch)
	 rchmono(58,jrch) = rchmono(58,jrch) + rchdy(59,jrch)
	 !!---stream temperature-------------------------
       rchmono(60,jrch) = rchmono(60,jrch) + rchdy(60,jrch) 
       

      
      if (cswat == 2 ) then
      
      
       wtrin = 0.
       wtrin = varoute(2,inum2) * (1. - rnum1)
    
      varoute2(1,ihout) = rchin_RPOC(jrch)         !!  inflow RPOC ->kg
      varoute2(2,ihout) = rchin_LPOC(jrch)         !!  inflow LPOC ->kg
      varoute2(3,ihout) = rchin_RDOC (jrch)        !!  inflow RDOC ->kg
      varoute2(4,ihout) = rchin_LDOC(jrch)         !!  inflow LDOC ->kg
      varoute2(5,ihout) = rchin_DIC (jrch)         !!  inflow DIC ->kg
      varoute2(6,ihout) = rchin_Alg(jrch)*cbn_rchpara(jrch)%rca   !!  inflow Floating Alg >kg  C  
      varoute2(7,ihout)= cbn_rch(jrch)%RPOC*wtrtot1/ 1000.   !! RPOC updated before outflow  kg C 
      varoute2(8,ihout)= cbn_rch(jrch)%LPOC*wtrtot1/ 1000.   !! kg C
      varoute2(9,ihout)= cbn_rch(jrch)%RDOC*wtrtot1/ 1000.   !! kg C
      varoute2(10,ihout)= cbn_rch(jrch)%LDOC*wtrtot1/ 1000.  !! kg C
      varoute2(11,ihout)= cbn_rch(jrch)%DIC*wtrtot1/ 1000.      !!kg C
      varoute2(12,ihout) = rch_Alg(jrch)*cbn_rchpara(jrch)%rca*wtrtot1 /1000             ! !!(mg/L) ->kg C    Floating algea 
      varoute2(13,ihout ) = rchini_RPOC(jrch)    !!  beginning of day RPOC stored in streamKG--------------------------State variable--------------
      varoute2(14,ihout ) = rchini_LPOC(jrch)    !!KG-----------------------------State variable
      varoute2(15,ihout ) = rchini_RDOC(jrch)    !!KG---------------------------------------------------State variable---------
      varoute2(16,ihout ) = rchini_LDOC(jrch)    !!KG--------------------------------------------------------State variable---------
      varoute2(17,ihout ) = rchini_DIC(jrch)     !!KG---------------------------------------------------------State variable-----
      varoute2(18,ihout ) = rchini_Alg(jrch)*cbn_rchpara(jrch)%rca              !!KG C ---------------------------------------State variable------------------
      varoute2(19,ihout ) = rchre_RPOC(jrch)       !!  end of day RPOC stored in stream KG-------------------------State variable------------
      varoute2(20,ihout ) = rchre_LPOC(jrch)         !!KG --------------------------------------------------------State variable-
      varoute2(21,ihout ) = rchre_RDOC(jrch)         !!KG------------------------------------------------------State variable-
      varoute2(22,ihout ) = rchre_LDOC(jrch)         !!KG------------------------------------------------------State variable---
      varoute2(23,ihout ) = rchre_DIC(jrch)          !!KG------------------------------------------------------------State variable---
      varoute2(24,ihout ) = rchre_Alg(jrch) *cbn_rchpara(jrch)%rca               !!KG C-------------------------------------------State variable--------------------
      varoute2(25,ihout)= cbn_rch(jrch)%RPOC*rtwtr/ 1000.   !outflow kg C 
      varoute2(26,ihout)= cbn_rch(jrch)%LPOC*rtwtr/ 1000.   !outflow kg C
      varoute2(27,ihout)= cbn_rch(jrch)%RDOC*rtwtr/ 1000.   !outflow kg C
      varoute2(28,ihout)= cbn_rch(jrch)%LDOC*rtwtr/ 1000.   !outflow kg C
      varoute2(29,ihout)= cbn_rch(jrch)%DIC*rtwtr/ 1000.    !ouflow kg C
      varoute2(30,ihout) = rch_Alg(jrch)*cbn_rchpara(jrch)%rca*rtwtr/ 1000.                     !!outflow kg     Floating  algea biomass-
    
      varoute2(31,ihout) = Ab_rch(jrch)%Ab*(ch_area(jrch)/wtrtot1)   	&                         !bottom algae biomass (g/m2/day) ->(mg/L) ->kg   c-------------------------------sState variable----------
                                  *cbn_rchpara(jrch)%rca  *wtrtot1 /1000          
      varoute2(32,ihout) = Ab_rch(jrch)%death*(ch_area(jrch)/wtrtot1)  	&                            !bottom algae death biomass (g/m2/day) ->(mg/L) ->kg c
                                  *cbn_rchpara(jrch)%rca *wtrtot1 /1000 
      varoute2(33,ihout) = Ab_rch(jrch)%photo*(ch_area(jrch)/wtrtot1)	&                         !bottom algae photosynthesis biomass (g/m2/day) ->(mg/L) ->kg d
                                 *cbn_rchpara(jrch)%rca  *wtrtot1 /1000
      varoute2(34,ihout) = Ab_rch(jrch)%resp*(ch_area(jrch)/wtrtot1) 	&                       !bottom algae respiration biomass  (g/m2/day) ->(mg/L) ->kg d
                              *cbn_rchpara(jrch)%rca *wtrtot1 /1000
      varoute2(35,ihout) = cbn_rec%Alg_cbn  *wtrtot1 /1000        ! Floating Algae death to total organic carbon (mg/L) ->kg
      varoute2(36,ihout) = cbn_rec%Ab_cbn  *wtrtot1 /1000         ! bottom algae death to total organic carbon(mg/L) ->kg
      varoute2(37,ihout) = cbn_rec%Alg_LP  *wtrtot1 /1000         ! Floating Algae -> LPOC  (mg/L) ->kg
      varoute2(38,ihout) = cbn_rec%Ab_LP  *wtrtot1 /1000          ! Bottom Algae -> LPOC    (mg/L) ->kg
      varoute2(39,ihout) = cbn_rec%LP_set   *wtrtot1 /1000        ! LPOC -> bed settling     (mg/L) ->kg
      varoute2(40,ihout ) = cbn_rec%LP_LD   *wtrtot1 /1000        ! LPOC dissolution to LDOC (mg/L) ->kg
      varoute2(41,ihout ) = cbn_rec%LP_DIC *wtrtot1 /1000         ! LPOC decay to DIC       (mg/L) ->kg
      varoute2(42,ihout ) = cbn_rec%LR_POC *wtrtot1 /1000       ! LPOC decay to RPOC       (mg/L) ->kg
      varoute2(43,ihout ) = cbn_rec%Alg_RP  *wtrtot1 /1000          ! Floating Algae -> RPOC   (mg/L) ->kg
      varoute2(44,ihout ) = cbn_rec%Ab_RP   *wtrtot1 /1000         ! Bottom Algae -> RPOC     (mg-C/L/day)
      varoute2(45,ihout ) = cbn_rec%RP_LD  *wtrtot1 /1000          ! RPOC dissolution to LDOC (mg/L) ->kg
      varoute2(46,ihout ) = cbn_rec%RP_DIC  *wtrtot1 /1000         ! RPOC dissolution to LDOC(mg/L) ->kg
      varoute2(47,ihout ) = cbn_rec%RP_set  *wtrtot1 /1000         !RPOC settling to bed (mg/L) ->kg
      varoute2(48,ihout ) = cbn_rec%Alg_LD  *wtrtot1 /1000         ! Algal death to LDOC (mg/L) ->kg
      varoute2(49,ihout ) = cbn_rec%Ab_LD   *wtrtot1 /1000         ! Bottom Algae -> LDOC  (mg/L) ->kg
      varoute2(50,ihout ) = cbn_rec%LD_DIC *wtrtot1 /1000          ! LDOC mineralization to DIC (mg/L) ->kg
      varoute2(51,ihout ) = cbn_rec%LR_DOC  *wtrtot1 /1000       ! LDOC decay to RDOC  (mg/L) ->kg
      varoute2(52,ihout ) = cbn_rec%LD_NO3 *wtrtot1 /1000        ! LDOC consumed by NO3- denitrification (mg/L) ->kg
      varoute2(53,ihout ) = cbn_rec%Alg_RD   *wtrtot1 /1000         ! Floating Algae -> RDOC (mg/L) ->kg
      varoute2(54,ihout ) = cbn_rec%Ab_RD    *wtrtot1 /1000        ! Bottom Algae -> RDOC  (mg/L) ->kg
      varoute2(55,ihout ) = cbn_rec%RD_DIC*wtrtot1 /1000          ! RDOC mineralization to DIC  (mg/L) ->kg
      varoute2(56,ihout ) = cbn_rec%Atm_DIC  *wtrtot1 /1000       ! Atmospheric CO2 reaeration (mg/L) ->kg
      varoute2(57,ihout ) = cbn_rec%Alg_DIC  *wtrtot1 /1000        ! Algal respiration to DIC  (mg/L) ->kg
      varoute2(58,ihout ) = cbn_rec%DIC_Alg *wtrtot1 /1000         ! DIC consumed by algal photosynthesis (mg/L) ->kg
      varoute2(59,ihout ) = cbn_rec%Ab_DIC   *wtrtot1 /1000        ! Bottom algae respiration to DIC(mg/L) ->kg
      varoute2(60,ihout ) = cbn_rec%DIC_Ab   *wtrtot1 /1000        ! DIC consumed by algae photosynthesis (mg/L) ->kg
      varoute2(61,ihout ) = cbn_rec%bed_DIC  *wtrtot1 /1000        ! Sediment release DIC to water column (mg/L) ->kg    !cbn_rec%CBOD_DIC  *wtrtot1 /1000   ! CBOD oxidation (mg/L) ->kg
      varoute2(62,ihout ) = 0.                       ! Floating algea settling in sediment kg C
    
      varoute2(63,ihout ) = rchin_RPOC(jrch)+ rchin_LPOC(jrch)       !! Total POC inflow KG
      varoute2(64,ihout ) = rchin_RDOC(jrch)+ rchin_LDOC(jrch)      !! Total DOC inflow KG
      varoute2(65,ihout ) = rchin_RPOC(jrch)+ rchin_LPOC(jrch)       &       !!Total  Org-C inflow  KG
         + rchin_RDOC(jrch)+ rchin_LDOC(jrch)                       &
         +  rchin_Alg(jrch)*cbn_rchpara(jrch)%rca 
      varoute2(66,ihout ) = rchin_RPOC(jrch)+ rchin_LPOC(jrch)          &       !! Total C inflow KG
                                + rchin_RDOC(jrch)+ rchin_LDOC(jrch)    &
                 + rchin_DIC(jrch)+  rchin_Alg(jrch)*cbn_rchpara(jrch)%rca   
      varoute2(67,ihout ) =  (cbn_sub(jrch)%RPOC+cbn_sub(jrch)%LPOC)	& !!Total POC from sub KG
                               *sub_ha
      varoute2(68,ihout ) =  (cbn_sub(jrch)%RDOC+cbn_sub(jrch)%LDOC)	&  !!Total DOC from sub KG
                           	*sub_ha
      varoute2(69,ihout ) =   (cbn_sub(jrch)%RPOC+cbn_sub(jrch)%LPOC+       &        !!Total  Org-C from sub  KG
                        + cbn_sub(jrch)%RDOC+cbn_sub(jrch)%LDOC)*sub_ha     &
             +1000*sub_chl(jrch) / ai0*cbn_rchpara(jrch)%rca 
      varoute2(70,ihout ) = (cbn_sub(jrch)%RPOC+cbn_sub(jrch)%LPOC+        &        !!Total C from sub  KG
                        + cbn_sub(jrch)%RDOC+cbn_sub(jrch)%LDOC             &
             +cbn_sub(jrch)%DIC)*sub_ha                                     &
             +1000*sub_chl(jrch) / ai0 *cbn_rchpara(jrch)%rca 
      varoute2(71,ihout ) =  cbn_sub(jrch)%DIC*sub_ha                                            !! DIC from sub KG
      
      varoute2(72,ihout ) = rch_Bed_BOC(jrch)                                          !! burying C during time step    KG
      varoute2(73,ihout ) = varoute2(39,ihout) + varoute2(47,ihout )   	&   !! RPOC,LPOC, Floating algea settling during time step  KG
                                  +varoute2(62,ihout )     
      varoute2(74,ihout ) = scbn_rch(jrch)%scbn                                 !  Sediment C                   KG-------------------------------------------------State varible---
      varoute2(75,ihout ) = rch_BuryC(jrch)                                              !! buried C     KG-------------------------------------------------------------State varible-------
      varoute2(76,ihout ) = varoute2(58,ihout ) + varoute2(60,ihout )   !DIC consumed by Photo  kg                 
      varoute2(77,ihout ) =  0.  !rch_CO2(jrch) *wtrtot1 /1000                   !! CO2 emission from reach kg--
      varoute2(78,ihout ) = wtrtot1   !rch_HCO3(jrch)*wtrtot1 /1000         !             .inflow+stor m3      
      varoute2(79,ihout ) = rchwtr     !rch_CO3(jrch) *wtrtot1 /1000        !             !  ini stor  m3           
      varoute2(80,ihout ) = ch_area(jrch)/10000   ! HA rch_pH(jrch)       !rchstor(jrch)     !  end stor m3                                        -----State varible-------
      varoute2(81,ihout ) = rch_waterarea(jrch)/10000   !rch_Alk(jrch) ! rttime / 24.0  !rch_Alk(jrch)  !  tday                               -----State varible-------
    
      varoute2(82,ihout ) =  rch_Alg_Growth(jrch)*cbn_rchpara(jrch)%rca  	&  !! mg/L-> KG
                                             *wtrtot1 /1000              
      varoute2(83,ihout ) =  rch_Alg_Death(jrch)*cbn_rchpara(jrch)%rca   	&   !! mg/L-> KG
                                             *wtrtot1 /1000               
       varoute2(84,ihout ) =  rch_Alg_Resp(jrch)*cbn_rchpara(jrch)%rca       &	!! mg/L-> KG
                                              *wtrtot1 /1000                  
       varoute2(85,ihout ) =  rch_Alg_Set(jrch)*cbn_rchpara(jrch)%rca        &    !! mg/L-> KG
                                              *wtrtot1 /1000                        
       varoute2(86,ihout ) = varoute(1,ihout)   ! rchini_Ab(jrch)*cbn_rchpara(jrch)%rca              !!KG C ---------------------------------State variable------------------                                   
       varoute2(87,ihout ) = rchdep ! rchre_Ab(jrch)*cbn_rchpara(jrch)%rca              !!KG C ---------------------------------State variable------------------      
    
       
      


      rchdy2(1,jrch) = varoute2(1,ihout)
      rchdy2(2,jrch) = varoute2(2,ihout)
      rchdy2(3,jrch) = varoute2(3,ihout)            
      rchdy2(4,jrch) = varoute2(4,ihout)
      rchdy2(5,jrch) = varoute2(5,ihout)
      rchdy2(6,jrch) = varoute2(6,ihout)
      rchdy2(7,jrch) = varoute2(7,ihout)
      rchdy2(8,jrch) = varoute2(8,ihout)
      rchdy2(9,jrch) = varoute2(9,ihout)
      rchdy2(10,jrch) = varoute2(10,ihout)
      rchdy2(11,jrch) = varoute2(11,ihout)
      rchdy2(12,jrch) = varoute2(12,ihout)
      rchdy2(13,jrch) = varoute2(13,ihout)
      rchdy2(14,jrch) = varoute2(14,ihout)
      rchdy2(15,jrch) = varoute2(15,ihout)
      rchdy2(16,jrch) = varoute2(16,ihout)
      rchdy2(17,jrch) = varoute2(17,ihout)
      rchdy2(18,jrch) = varoute2(18,ihout)
      rchdy2(19,jrch) = varoute2(19,ihout)
      rchdy2(20,jrch) = varoute2(20,ihout)
      rchdy2(21,jrch) = varoute2(21,ihout)
      rchdy2(22,jrch) = varoute2(22,ihout)
      rchdy2(23,jrch) = varoute2(23,ihout)
      rchdy2(24,jrch) = varoute2(24,ihout)
      rchdy2(25,jrch) = varoute2(25,ihout)
      rchdy2(26,jrch) = varoute2(26,ihout)
      rchdy2(27,jrch) = varoute2(27,ihout)
      rchdy2(28,jrch) = varoute2(28,ihout)
      rchdy2(29,jrch) = varoute2(29,ihout)
      rchdy2(30,jrch) = varoute2(30,ihout)
      rchdy2(31,jrch) = varoute2(31,ihout)
      rchdy2(32,jrch) = varoute2(32,ihout)
      rchdy2(33,jrch) = varoute2(33,ihout)
      rchdy2(34,jrch) = varoute2(34,ihout)
      rchdy2(35,jrch) = varoute2(35,ihout)
      rchdy2(36,jrch) = varoute2(36,ihout)
      rchdy2(37,jrch) = varoute2(37,ihout)
      rchdy2(38,jrch) = varoute2(38,ihout)
      rchdy2(39,jrch) = varoute2(39,ihout)
      rchdy2(40,jrch) = varoute2(40,ihout)
      rchdy2(41,jrch) = varoute2(41,ihout)
      rchdy2(42,jrch) = varoute2(42,ihout)
      rchdy2(43,jrch) = varoute2(43,ihout)
      rchdy2(44,jrch) = varoute2(44,ihout)
      rchdy2(45,jrch) = varoute2(45,ihout)
      rchdy2(46,jrch) = varoute2(46,ihout)
      rchdy2(47,jrch) = varoute2(47,ihout)
      rchdy2(48,jrch) = varoute2(48,ihout)
      rchdy2(49,jrch) = varoute2(49,ihout)
      rchdy2(50,jrch) = varoute2(50,ihout)
      rchdy2(51,jrch) = varoute2(51,ihout)
      rchdy2(52,jrch) = varoute2(52,ihout)
      rchdy2(53,jrch) = varoute2(53,ihout)
      rchdy2(54,jrch) = varoute2(54,ihout)
      rchdy2(55,jrch) = varoute2(55,ihout)
      rchdy2(56,jrch) = varoute2(56,ihout)            
      rchdy2(57,jrch) = varoute2(57,ihout)
      rchdy2(58,jrch) = varoute2(58,ihout)
      rchdy2(59,jrch) = varoute2(59,ihout)
      rchdy2(60,jrch) = varoute2(60,ihout)
      rchdy2(61,jrch) = varoute2(61,ihout)
      rchdy2(62,jrch) = varoute2(62,ihout)
      rchdy2(63,jrch) = varoute2(63,ihout)
      rchdy2(64,jrch) = varoute2(64,ihout)
      rchdy2(65,jrch) = varoute2(65,ihout)
      rchdy2(66,jrch) = varoute2(66,ihout)
      rchdy2(67,jrch) = varoute2(67,ihout)
      rchdy2(68,jrch) = varoute2(68,ihout)
      rchdy2(69,jrch) = varoute2(69,ihout)
      rchdy2(70,jrch) = varoute2(70,ihout)
      rchdy2(71,jrch) = varoute2(71,ihout)
      rchdy2(72,jrch) = varoute2(72,ihout)
      rchdy2(73,jrch) = varoute2(73,ihout)
      rchdy2(74,jrch) = varoute2(74,ihout)
      rchdy2(75,jrch) = varoute2(75,ihout)
      rchdy2(76,jrch) = varoute2(76,ihout)
      rchdy2(77,jrch) = varoute2(77,ihout)
      rchdy2(78,jrch) = varoute2(78,ihout)
      rchdy2(79,jrch) = varoute2(79,ihout)
      rchdy2(80,jrch) = varoute2(80,ihout)
      rchdy2(81,jrch) = varoute2(81,ihout)
      rchdy2(82,jrch) = varoute2(82,ihout)
      rchdy2(83,jrch) = varoute2(83,ihout)
      rchdy2(84,jrch) = varoute2(84,ihout)
      rchdy2(85,jrch) = varoute2(85,ihout)
      rchdy2(86,jrch) = varoute2(86,ihout )                            
      rchdy2(87,jrch) = varoute2(87,ihout )
      rchdy2(88,jrch) = varoute2(88,ihout )                            
      rchdy2(89,jrch) = varoute2(89,ihout )    
      rchdy2(90,jrch) = varoute2(90,ihout )       
      rchdy2(91,jrch) = varoute2(91,ihout )                            
      rchdy2(92,jrch) = varoute2(92,ihout )
      rchdy2(93,jrch) = varoute2(93,ihout )                            
      rchdy2(94,jrch) = varoute2(94,ihout )    
      rchdy2(95,jrch) = varoute2(95,ihout )         
      rchdy2(96,jrch) = varoute2(96,ihout )   
          
      rchmono2(1,jrch) = rchmono2(1,jrch) + rchdy2(1,jrch)
      rchmono2(2,jrch) = rchmono2(2,jrch) + rchdy2(2,jrch)
      rchmono2(3,jrch) = rchmono2(3,jrch) + rchdy2(3,jrch)
      rchmono2(4,jrch) = rchmono2(4,jrch) + rchdy2(4,jrch)
      rchmono2(5,jrch) = rchmono2(5,jrch) + rchdy2(5,jrch)
      rchmono2(6,jrch) = rchmono2(6,jrch) + rchdy2(6,jrch)
      rchmono2(7,jrch) = rchmono2(7,jrch) + rchdy2(7,jrch)
      rchmono2(8,jrch) = rchmono2(8,jrch) + rchdy2(8,jrch)
      rchmono2(9,jrch) = rchmono2(9,jrch) + rchdy2(9,jrch)
      rchmono2(10,jrch) = rchmono2(10,jrch) + rchdy2(10,jrch)
      rchmono2(11,jrch) = rchmono2(11,jrch) + rchdy2(11,jrch)
      rchmono2(12,jrch) = rchmono2(12,jrch) + rchdy2(12,jrch)
      rchmono2(13,jrch) = rchmono2(13,jrch) + rchdy2(13,jrch)
      rchmono2(14,jrch) = rchmono2(14,jrch) + rchdy2(14,jrch)
      rchmono2(15,jrch) = rchmono2(15,jrch) + rchdy2(15,jrch)
      rchmono2(16,jrch) = rchmono2(16,jrch) + rchdy2(16,jrch)
      rchmono2(17,jrch) = rchmono2(17,jrch) + rchdy2(17,jrch)
      rchmono2(18,jrch) = rchmono2(18,jrch) + rchdy2(18,jrch)
      rchmono2(19,jrch) = 0. !rchmono2(19,jrch) + rchdy2(19,jrch)
      rchmono2(20,jrch) = 0.  !rchmono2(20,jrch) + rchdy2(20,jrch)
      rchmono2(21,jrch) = 0. !rchmono2(21,jrch) + rchdy2(21,jrch)
      rchmono2(22,jrch) = 0. !rchmono2(22,jrch) + rchdy2(22,jrch)
      rchmono2(23,jrch) = 0.  !rchmono2(23,jrch) + rchdy2(23,jrch)
      rchmono2(24,jrch) = 0. !rchmono2(24,jrch) + rchdy2(24,jrch)
      rchmono2(25,jrch) = 0.  !rchmono2(25,jrch) + rchdy2(25,jrch)
      rchmono2(26,jrch) =  rchdy2(26,jrch)                                                       !!RPOC kg  last day of the month
      rchmono2(27,jrch) =  rchdy2(27,jrch)                                                        !!LPOC kg  last day of the month 
      rchmono2(28,jrch) =  rchdy2(28,jrch)                                                        !!RDOC kg  last day of the month  
      rchmono2(29,jrch) =  rchdy2(29,jrch)                                                         !!LDOC kg  last day of the month
      rchmono2(30,jrch) =  rchdy2(30,jrch)                                                         !!DIC kg  last day of the month
      rchmono2(31,jrch) =  rchdy2(31,jrch)                                                          !!Floating Algea kg  last day of the month
      rchmono2(32,jrch) =  rchdy2(32,jrch)                                                         !!Bottom Algea kg  last day of the month
      rchmono2(33,jrch) = rchmono2(33,jrch) + rchdy2(33,jrch)
      rchmono2(34,jrch) = rchmono2(34,jrch) + rchdy2(34,jrch)
      rchmono2(35,jrch) = rchmono2(35,jrch) + rchdy2(35,jrch)
      rchmono2(36,jrch) = rchmono2(36,jrch) + rchdy2(36,jrch)
      rchmono2(37,jrch) = rchmono2(37,jrch) + rchdy2(37,jrch)
      rchmono2(38,jrch) = rchmono2(38,jrch) + rchdy2(38,jrch)
      rchmono2(39,jrch) = rchmono2(39,jrch) + rchdy2(39,jrch)
      rchmono2(40,jrch) = rchmono2(40,jrch) + rchdy2(40,jrch)
      rchmono2(41,jrch) = rchmono2(41,jrch) + rchdy2(41,jrch)
      rchmono2(42,jrch) = rchmono2(42,jrch) + rchdy2(42,jrch)
      rchmono2(43,jrch) = rchmono2(43,jrch) + rchdy2(43,jrch)
      rchmono2(44,jrch) = rchmono2(44,jrch) + rchdy2(44,jrch)
      rchmono2(45,jrch) = rchmono2(45,jrch) + rchdy2(45,jrch)
      rchmono2(46,jrch) = rchmono2(46,jrch) + rchdy2(46,jrch)
      rchmono2(47,jrch) = rchmono2(47,jrch) + rchdy2(47,jrch)
      rchmono2(48,jrch) = rchmono2(48,jrch) + rchdy2(48,jrch)
      rchmono2(49,jrch) = rchmono2(49,jrch) + rchdy2(49,jrch)
      rchmono2(50,jrch) = rchmono2(50,jrch) + rchdy2(50,jrch)
      rchmono2(51,jrch) = rchmono2(51,jrch) + rchdy2(51,jrch)
      rchmono2(52,jrch) = rchmono2(52,jrch) + rchdy2(52,jrch)
      rchmono2(53,jrch) = rchmono2(53,jrch) + rchdy2(53,jrch)
      rchmono2(54,jrch) = rchmono2(54,jrch) + rchdy2(54,jrch)
      rchmono2(55,jrch) = rchmono2(55,jrch) + rchdy2(55,jrch)
      rchmono2(56,jrch) = rchmono2(56,jrch) + rchdy2(56,jrch)
      rchmono2(57,jrch) = rchmono2(57,jrch) + rchdy2(57,jrch)
      rchmono2(58,jrch) = rchmono2(58,jrch) + rchdy2(58,jrch)
      rchmono2(59,jrch) = rchmono2(59,jrch) + rchdy2(59,jrch)
      rchmono2(60,jrch) = rchmono2(60,jrch) + rchdy2(60,jrch)
      rchmono2(61,jrch) = rchmono2(61,jrch) + rchdy2(61,jrch)
      rchmono2(62,jrch) = rchmono2(62,jrch) + rchdy2(62,jrch)
      rchmono2(63,jrch) = rchmono2(63,jrch) + rchdy2(63,jrch)
      rchmono2(64,jrch) = rchmono2(64,jrch) + rchdy2(64,jrch)
      rchmono2(65,jrch) = rchmono2(65,jrch) + rchdy2(65,jrch)
      rchmono2(66,jrch) = rchmono2(66,jrch) + rchdy2(66,jrch)
      rchmono2(67,jrch) = rchmono2(67,jrch) + rchdy2(67,jrch)
      rchmono2(68,jrch) = rchmono2(68,jrch) + rchdy2(68,jrch)
      rchmono2(69,jrch) = rchmono2(69,jrch) + rchdy2(69,jrch)
      rchmono2(70,jrch) = rchmono2(70,jrch) + rchdy2(70,jrch)
      rchmono2(71,jrch) = rchmono2(71,jrch) + rchdy2(71,jrch)
      rchmono2(72,jrch) = 0. !rchmono2(72,jrch) + rchdy2(72,jrch)
      rchmono2(73,jrch) =  rchdy2(73,jrch)                                                  !!Sediment C kg  last day of the month  
      rchmono2(74,jrch) = rchmono2(74,jrch) + rchdy2(74,jrch)
      rchmono2(75,jrch) = rchmono2(75,jrch) + rchdy2(75,jrch)      
      rchmono2(76,jrch) =  rchdy2(76,jrch)                                                  !!Sediment C kg  last day of the month  
      rchmono2(77,jrch) =  rchdy2(77,jrch)                                                  !!Bury C kg  last day of the month   
      rchmono2(78,jrch) = rchmono2(78,jrch) + rchdy2(78,jrch)
      rchmono2(79,jrch) = rchmono2(79,jrch) + rchdy2(79,jrch)
      rchmono2(80,jrch) = rchmono2(80,jrch) + rchdy2(80,jrch)
      rchmono2(81,jrch) = rchmono2(81,jrch) + rchdy2(81,jrch)
      rchmono2(82,jrch) = rchmono2(82,jrch) + rchdy2(82,jrch)
      rchmono2(83,jrch) = rchmono2(83,jrch) + rchdy2(83,jrch)
      rchmono2(84,jrch) = rchmono2(84,jrch) + rchdy2(84,jrch)
      rchmono2(85,jrch) = rchmono2(85,jrch) + rchdy2(85,jrch)
      rchmono2(86,jrch) = rchmono2(86,jrch) + rchdy2(86,jrch)
      rchmono2(87,jrch) = rchmono2(87,jrch) + rchdy2(87,jrch)
      rchmono2(88,jrch) = rchmono2(88,jrch) + rchdy2(88,jrch)
      rchmono2(89,jrch) = rchmono2(89,jrch) + rchdy2(89,jrch)
      rchmono2(90,jrch) = rchmono2(90,jrch) + rchdy2(90,jrch) 
      rchmono2(91,jrch) = rchmono2(91,jrch) + rchdy2(91,jrch)
      rchmono2(92,jrch) = rchmono2(92,jrch) + rchdy2(92,jrch)
      rchmono2(93,jrch) = rchmono2(93,jrch) + rchdy2(93,jrch)
      rchmono2(94,jrch) = rchmono2(94,jrch) + rchdy2(94,jrch)
      rchmono2(95,jrch) = rchmono2(95,jrch) + rchdy2(95,jrch)
      rchmono2(96,jrch) = rchmono2(96,jrch) + rchdy2(96,jrch) 
      
   
     
       
       
       if(rch_std==1) then
      wshddayo(75) = wshddayo(75) + rch_Bed_BOC(jrch)                            !!buried C kg
      wshddayo(76) = wshddayo(76) + rch_Atm_DIC(jrch)                         !! atm CO2   kg
      wshddayo(77) = wshddayo(77) + rch_LDOC_NO3(jrch)                   !! NO3 Denitrification ton C kg
      wshddayo(78) = wshddayo(78) + rch_SedC(jrch)                        !! sediment C kg   state
      wshddayo(79) = wshddayo(79) + 		&
            rch_AbM(jrch)*cbn_rchpara(jrch)%rca* ch_area(jrch)/1000.          !! Bottom algea  C kg    state

      wshddayo(80) = wshddayo(80) + ch_area(jrch)/10000   ! HA      state
      wshddayo(81) = wshddayo(81) + rch_waterarea(jrch)/10000   ! HA            state
       if(jrch==jrch_out) then
      wshddayo(82) =  rchout_RPOC(jrch_out)                                          !  kg C
      wshddayo(83) =  rchout_LPOC(jrch_out)                                          !  kg C
      wshddayo(84) =  rchout_RDOC(jrch_out)                                          !  kg C   
      wshddayo(85) =  rchout_LDOC(jrch_out)                                          !  kg C
      wshddayo(86) = rchout_DIC(jrch_out)                                               !  kg C
      wshddayo(87) =  rchout_Alg(jrch_out) *cbn_rchpara(jrch_out)%rca           !  kg C
      else
      wshddayo(82) = 0.
      wshddayo(83) =  0.
      wshddayo(84) =  0
      wshddayo(85) = 0.
      wshddayo(86) = 0.
      wshddayo(87) =  0.
      end if
      wshddayo(88) = wshddayo(88) + rch_RPOC_Set(jrch)                              !  kg C
      wshddayo(89) = wshddayo(89) + rch_LPOC_Set(jrch)                              ! kg C
      wshddayo(90) = wshddayo(90) + rch_Alg_Set(jrch)*cbn_rchpara(jrch)%rca         ! kg C
      !wshddayo(91) = wshddayo(91) + rch_Bed_LPOCR(jrch)                               ! kg C
      !wshddayo(92) = wshddayo(92) + rch_Bed_RPOCR(jrch)                               !  kg C
      !wshddayo(93) = wshddayo(93) + rch_Bed_CH4R(jrch)                                ! kg C       
      wshddayo(94) = wshddayo(94) + rch_Alg_LPOC(jrch)+rch_Ab_LPOC(jrch)              !      kg C                                        
      wshddayo(95) = wshddayo(95) + rch_Alg_RPOC(jrch)+rch_Ab_RPOC(jrch)   &          !      kg C       
                      + rch_LPOC_RPOC(jrch)                                          
      wshddayo(96) = wshddayo(96) + rch_Alg_LDOC(jrch)+rch_Ab_LDOC(jrch)  &           !     kg C       
                     +  rch_LPOC_LDOC(jrch) + rch_LPOC_LDOC(jrch)         
      wshddayo(97) = wshddayo(97)+ rch_Alg_RDOC(jrch)+rch_Ab_RDOC(jrch)    &        !    kg C       
                     + rch_LPOC_RPOC(jrch)          
      wshddayo(98) = wshddayo(98) + rch_Alg_DIC(jrch) +rch_Ab_DIC(jrch)		&                       !    kg C         
            + rch_RPOC_DIC(jrch)+rch_LPOC_DIC(jrch)+rch_RDOC_DIC(jrch)		&
            + rch_LDOC_DIC(jrch)
      !wshddayo(99) = wshddayo(99)+rch_CO2_sed(jrch)                                               !     kgC                       
      ! wshddayo(100) = wshddayo(100)+rch_CH4s_sed(jrch)                                          !     kgC
      ! wshddayo(101) = wshddayo(101)+rch_CH4g_sed(jrch)                                          !     kgC   
      
    ! wshddayo(102) = wshddayo(102)+sub_sedy(jrch)                                            !     TonC                       
    !     if(jrch==jrch_out) then
   !      wshddayo(103) = wshddayo(103)+rchout_Sed(jrch)                                       !     TonC                        
    !     else
    !     wshddayo(103) =0.
    !     end if
   !   wshddayo(104) = wshddayo(104)+depch(jrch)                                                        !   TonC      State variable
    !  wshddayo(105) = wshddayo(105)+ rch_SedDep(jrch)                                            !   TonC     
    !  wshddayo(106) = wshddayo(106)+ rch_SedGra1(jrch)  !  rch_Bed_SedR(jrch)               !   TonC     
      
      

 
      
      
      else
      wshddayo(75) = 0.  
      wshddayo(76) = 0.
      wshddayo(77) = 0.
      wshddayo(78) = 0.
      wshddayo(79) = 0.
      wshddayo(80) = 0.
      wshddayo(81) = 0.
      wshddayo(82) = 0.
      wshddayo(83) = 0.
      end if
     
      end if
      
      
	
      
      
      
      
    !   if (ievent_rch==1) then
      !  do ii=1,nstep_rch
      ! hhvaroute1(1,ihout,ii) = hhvaroute(2,inum2,ii) * (1. - rnum1)                       !flow in
      !hhvaroute1(2,ihout,ii) = hrtwtr(ii)                                                              !flow out 
      ! hhvaroute1(3,ihout,ii) = hrchwtr(ii)                                 !! water storage at the beginning of time step
      !hhvaroute1(4,ihout,ii) = hhstor(ii)                                    !! water storage at the end of time step
      ! hhvaroute1(5,ihout,ii) = hhvol(ii)                                      !! total water in the reach 
      !hhvaroute1(6,ihout,ii) = hrtevp(ii)                                    !! E
      !hhvaroute1(7,ihout,ii) = hrttlc(ii)                                      !! T
      ! hhvaroute1(8,ihout,ii) =( hhstor(ii) -hrchwtr(ii) )
    ! &     -( hhvaroute2(88,ihout,ii)-hrtwtr(ii)-hrtevp(ii)-hrttlc(ii))
    !  end do
    !   end if
      
       ! if (ievent_rch==1.and.iprint==3) then
	 ! do ii=1, nstep_rch
         ! rchhr1(1,jrch,ii) = hhvaroute1(1,ihout,ii)
        !  rchhr1(2,jrch,ii) = hhvaroute1(2,ihout,ii)
        !  rchhr1(3,jrch,ii) = hhvaroute1(3,ihout,ii)
        !  rchhr1(4,jrch,ii) = hhvaroute1(4,ihout,ii)
         ! rchhr1(5,jrch,ii) = hhvaroute1(5,ihout,ii)
         ! rchhr1(6,jrch,ii) =  hhvaroute1(6,ihout,ii)   
        !  rchhr1(7,jrch,ii) = hhvaroute1(7,ihout,ii)
        !  rchhr1(8,jrch,ii) =  hhvaroute1(8,ihout,ii)   
        ! end do
        !end if
        
        
        
        
      return
      end