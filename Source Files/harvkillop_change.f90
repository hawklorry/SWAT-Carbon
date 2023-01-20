      subroutine harvkillop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest and kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha |annual biomass (dry weight) in the HRU
!!    cnop        |none           |SCS runoff curve number for moisture
!!                                |condition II
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                                |planting
!!    hru_dafr(:) |km2/km2        |fraction of watershed in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                                |growing season
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idplt(:)    |none           |land cover code from crop.dat
!!    ihru        |none           |HRU number
!!    ncrops(:,:,:)|
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)      |none           |sequence number for year in rotation
!!    nyskip      |none           |number of years to not summarize/print output
!!    plantp(:)   |kg P/ha        |amount of phosphorus in plant biomass
!!    plt_et(:)   |mm H2O         |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O         |potential ET simulated during life of plant
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    pltfr_n(:)  |none           |fraction of plant biomass that is nitrogen
!!    rwt(:)      |none           |fraction of total plant biomass that is
!!                                |in roots
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!                                |organic (residue) pool0
!!    sol_rsd(:,:)|kg/ha          |amount of organic matter in the soil
!!                                |classified as residue
!!    wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    yldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha          |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_hv(:,:,:)|kg/ha         |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha|annual biomass (dry weight) in the HRU
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                                |growing season
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing (not dormant)
!!                               |1 land cover dormant
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    ncrops(:,:,:)|
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant biomass
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    wshd_yldn   |kg N/ha       |amount of nitrogen removed from soil in
!!                               |watershed in the yield
!!    wshd_yldp   |kg P/ha       |amount of phosphorus removed from soil in
!!                               |watershed in the yield
!!    yldanu(:)   |metric tons/ha|annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hiad1       |
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    wur         |
!!    yield       |kg            |yield (dry weight)
!!    yieldn      |
!!    yieldp      |
!!    yldpst      |kg pst/ha     |pesticide removed in yield
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: j, k
      
!!   change per JGA 8/31/2011 gsm PUT YIELD IN modparm.f
!!    real :: wur, hiad1, yield, yieldn, yieldp, yldpst
      real :: wur, hiad1, yldpst
	  real :: resnew, rtresnew 
	  real :: xx,ff1,ff2,RLN,RLR 
      integer:: ly	  


	!!By Zhang
	!!=============
      real :: BLG1, BLG2, BLG3,  CLG  
      real :: sol_min_n, resnew_n, resnew_ne
      real :: LMF, LSF, LSLF, LSNF,LMNF 
      real :: orgc_f = 0.
      BLG1 = 0.
      BLG2 = 0.
      BLG3 = 0.
      CLG = 0.
      sol_min_n = 0.
      resnew = 0.
      resnew_n = 0.
      resnew_ne = 0.
      LMF = 0.
      LSF = 0.
      LSLF = 0.
      LSNF = 0.
      LMNF = 0.
	!!By Zhang
	!!==================
	

      j = 0
      j = ihru

      yield(j) = 0.
      yieldgrn(j) = 0.
      yieldbms(j) = 0.
      yieldtbr(j) = 0.
      yieldrsd(j) = 0.
      yieldn(j) = 0.
      yieldp(j) = 0.

	!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = 0.
      tnyld(j) = (1. - rwt(j)) * bio_ms(j) * pltfr_n(j) * auto_eff(j)
!     if (icr(j) > 1) then
!       tnyld(nro(j),icr(j)-1,j) = tnyld(nro(j),icr(j),j)
!     else
!       tnyld(nro(j),icr(j)+1,j) = tnyld(nro(j),icr(j),j)
!     end if


      hiad1 = 0.
      if (hi_targ(j) > 0.) then
        hiad1 = hi_targ(j)
      else
        if (plt_pet(j) < 10.) then
          wur = 100.
        else
          wur = 0.
          wur = 100. * plt_et(j) / plt_pet(j)
        endif
        hiad1 = (hvstiadj(j) - wsyf(idplt(j))) * (wur / (wur + Exp(6.13 - .0883 * wur))) + wsyf(idplt(j))

        if (hiad1 > hvsti(idplt(j))) then 
          hiad1 = hvsti(idplt(j))
        end if
      end if

!! check if yield is from above or below ground

      resnew = 0.
      rtresnew = 0.

!! stover fraction during harvkillop
      xx = frac_harvk
      if (xx .lt. 1.e-6) then
        xx = hi_ovr                      
      endif
!! stover fraction during harvkillop
      if (hi_ovr > 1.e-6) then
        yield(j) = bio_ms(j) * hi_ovr
        resnew = bio_ms(j) - yield(j)
      else
      if (idc(idplt(j)) == 7) then         !1 warm season annual legume; 2 cold season annual legume; 3 perennial legume; 4 warm season annual;5 cold season annual;6 perennial
!!                                  |7 trees
        !yield = bio_ms(j) * (1. - bio_leaf(idplt(j)))
        !resnew = bio_ms(j) - yield
               
        yield(j) = bio_ms(j) * (1. - bio_leaf(idplt(j))-rwt(j)) !! stem yield
        rtresnew = rwt(j) * bio_ms(j) !! root left
        resnew = bio_ms(j) - yield(j) - rtresnew !! leaf fall
        hiad1 = yield(j) / ((1-rwt(j)) * bio_ms(j)) !! harvest index: crop yield/aboveground
      else
        if (hvsti(idplt(j)) > 1.001) then
          yield(j) = bio_ms(j) * (1. - 1. / (1. + hiad1))
          resnew = bio_ms(j) / (1. + hiad1)
          resnew = resnew * (1. - xx)
        else
          yield(j) = (1. - rwt(j)) * bio_ms(j) * hiad1
          resnew = (1. - rwt(j)) * (1. - hiad1) * bio_ms(j)
          !! remove stover during harvkillop
          resnew = resnew * (1. - xx)
          rtresnew = rwt(j) * bio_ms(j)	
        endif
      endif
      end if 
      
      if (yield(j) < 0.) yield(j) = 0.
      if (resnew < 0.) resnew = 0.
	  if (rtresnew < 0.) rtresnew = 0.	! Armen 19 May 2008
										! I would avoid this check, it is
										! safer to know if variable is negative

      !!add by zhang
      !!=================
      !!use idplt(:,:,:) to calculate the crop type, then
      !! decide which type of crop yield should be used.
      if (cswat == 2) then
          grainc_d(j) = grainc_d(j) + yield(j) * CFB
          stoverc_d(j) = stoverc_d(j)+(bio_ms(j)-yield(j)-rtresnew)*CFB*xx
          rsdc_d(j) = rsdc_d(j) + resnew * CFB
          rsdc_d(j) = rsdc_d(j) + rtresnew * CFB
          OrgC_Plt2Rsd(j)=OrgC_Plt2Rsd(j)+(resnew+rtresnew)*CFB  !!residue C input 
      end if
      !!add by zhang
      !!=================

     

	!! calculate nutrients removed with yield
      yieldn(j) = 0.
      yieldp(j) = 0.
      yieldn(j) = yield(j) * cnyld(idplt(j))
      yieldp(j) = yield(j) * cpyld(idplt(j))
      yieldn(j) = Min(yieldn(j), 0.80 * plantn(j))
      yieldp(j) = Min(yieldp(j), 0.80 * plantp(j))

	!! Armen 19 May 2008 / 21 January 2008	
	!! fraction of roots in each layer
	call rootfr

	!! fraction of N, P in residue (ff1) or roots (ff2)
	ff1 = (1 - hiad1) / (1 - hiad1 + rwt(j))
	ff2 = 1 - ff1

	!! update residue, N, P on soil surface
      sol_rsd(1,j) = resnew + sol_rsd(1,j)
	sol_fon(1,j) = sol_fon(1,j) + FF1 * (plantn(j) - yieldn(j))
      sol_fop(1,j) = sol_fop(1,j) + FF1 * (plantp(j) - yieldp(j)) 
      sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
	sol_fon(1,j) = Max(sol_fon(1,j),0.)
	sol_fop(1,j) = Max(sol_fop(1,j),0.)
      
      
      OrgN_Plt2Rsd(j)=OrgN_Plt2Rsd(j)+ (plantn(j) - yieldn(j))   !!residue N input 
      OrgP_Plt2Rsd(j)=OrgP_Plt2Rsd(j)+ (plantp(j) - yieldp(j))   !!residue P input
      rsdn_d(j) = rsdn_d(j) + (plantn(j) - yieldn(j))  
      grainN(j)= yieldn(j)                          
      
            !!insert new biomss by zhang
            !!=================================
            if (cswat == 2 .and. resnew > 0.) then
	          !!all the lignin from STD is assigned to LSL, 
	            !!add STDL calculation
	          !!
	          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
	          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
	          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
                ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
                !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
                !SUBROUTINE ASCRV(X1,X2,X3,X4)
                !EPIC0810
                !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
                !USE PARM
                !XX=LOG(X3/X1-X3)
                !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
                !X1=XX+X3*X2
                !RETURN
                !END 
                !HUI(JJK)=HU(JJK)/XPHU               
                
                BLG1 = 0.01/0.10
                BLG2 = 0.99
                BLG3 = 0.10
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*phuacc(j)/(phuacc(j)+EXP(BLG1-BLG2*phuacc(j)))
    

	          !if (k == 1) then
		        !abs_f(j) = 0.05
	          !else
		        !abs_f(j) = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (sol_no3(1,j)+sol_nh4(1,j))
	          
	          resnew = resnew
	          resnew_n = ff1 * (plantn(j) - yieldn(j))    
	    
	            if (resnew > 10.) then   
        	        resnew_ne = resnew_n + abs_f(j) * sol_min_n
        	    else
        	        resnew_ne = resnew_n
        	    end if        	    
       	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    !RLN is the ratio of lignin to nitrogen in the newly added residue
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    RLR = MIN(.8, resnew * CLG/(resnew+1.E-5))
        	    
        	    LMF = 0.85 - 0.018 * RLN
        	    if (LMF <0.01) then
        	        LMF = 0.01
        	    else
        	        if (LMF >0.7) then
        	            LMF = 0.7
        	        end if
        	    end if      	  
	          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
		        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
	          !else
		        !    LMF = 0.
	          !end if 	

	          LSF =  1 - LMF  
        	  
	          sol_LM(1,j) = sol_LM(1,j) + LMF * resnew
	          sol_LS(1,j) = sol_LS(1,j) + LSF * resnew
        	  
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          sol_LSL(1,j) = sol_LSL(1,j) + RLR*resnew	          
	          sol_LSC(1,j) = sol_LSC(1,j) + CFB*LSF * resnew  
	          
	          sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*CFB*resnew
	          sol_LSLNC(1,j) = sol_LSC(1,j) - sol_LSLC(1,j)              
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_n >= (CFB * LSF * resnew /150)) then
		         sol_LSN(1,j) = sol_LSN(1,j) + CFB * LSF * resnew / 150
		         sol_LMN(1,j) = sol_LMN(1,j) + resnew_n - (CFB * LSF * resnew / 150) + 1.E-25
	          else
		         sol_LSN(1,j) = sol_LSN(1,j) + resnew_n
		         sol_LMN(1,j) = sol_LMN(1,j) + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          sol_LMC(1,j) = sol_LMC(1,j) + CFB * LMF * resnew	
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
             if (resnew > 10.) then   
                absorbed_no3(j) = absorbed_no3(j) + sol_no3(1,j) * abs_f(j) 
                absorbed_nh3(j) = absorbed_nh3(j) + sol_nh4(1,j) * abs_f(j)                         
                sol_no3(1,j) = sol_no3(1,j) * (1.0-abs_f(j))
                sol_nh4(1,j) = sol_nh4(1,j) * (1.0-abs_f(j))            
             end if            

             CFPltSTR(1,j) = CFB * LSF * resnew 
             CFPltMET(1,j) = CFB * LMF * resnew

	          if (resnew_ne >= (CFB * LSF * resnew /150)) then
		         !sol_LSN(1,j) = sol_LSN(1,j) + CFB * LSF * resnew / 150.
		         !sol_LMN(1,j) = sol_LMN(1,j) + resnew_ne - (CFB * LSF * resnew / 150.) + 1.E-25
                 NFPltSTR(1,j) = CFB * LSF * resnew / 150.
                 NFPltMET(1,j) = resnew_ne - (CFB * LSF * resnew / 150.) + 1.E-25
	          else
		         !sol_LSN(1,j) = sol_LSN(1,j) + resnew_ne
		         !sol_LMN(1,j) = sol_LMN(1,j) + 1.E-25
                 NFPltSTR(1,j) = resnew_ne
                 NFPltMET(1,j) = 1.E-25		         
	          end if	
            end if
            !!insert new biomss by zhang
            !!===============================


	!! allocate dead roots, N, P to soil layers
	do ly=1, sol_nly(j)
	        sol_rsd(ly,j) = sol_rsd(ly,j) + rtfr(ly) *rtresnew
            sol_fon(ly,j) = sol_fon(ly,j) + rtfr(ly) *ff2 * (plantn(j) - yieldn(j))
            sol_fop(ly,j) = sol_fop(ly,j) + rtfr(ly) *ff2 * (plantp(j) - yieldp(j))

          !!insert new biomss by zhang
          !!==============================
          if (cswat == 2 .and. rtresnew > 0.) then
          !!all the lignin from STD is assigned to LSL, 
            !!add STDL calculation
          !!
          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
            ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
            !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
            !SUBROUTINE ASCRV(X1,X2,X3,X4)
            !EPIC0810
            !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
            !USE PARM
            !XX=LOG(X3/X1-X3)
            !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
            !X1=XX+X3*X2
            !RETURN
            !END 
            !HUI(JJK)=HU(JJK)/XPHU               
            
            BLG1 = 0.01/0.10
            BLG2 = 0.99
            BLG3 = 0.10
            XX = log(0.5/BLG1-0.5)
            BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
            BLG1 = XX + 0.5*BLG2
            CLG=BLG3*phuacc(j)/(phuacc(j)+EXP(BLG1-BLG2*phuacc(j)))
     

       !   if (ly == 1) then
	       ! abs_f(j) = 0.05
        !  else
	       ! abs_f(j) = 0.1
      !    end if	

           !kg/ha  
          sol_min_n = 0.	
          sol_min_n = (sol_no3(ly,j)+sol_nh4(ly,j))
          	          
          resnew = rtfr(ly) *rtresnew 
          resnew_n = rtfr(ly) *ff2 * (plantn(j) - yieldn(j))   	   

          if (resnew > 10.) then   
    	        resnew_ne = resnew_n + abs_f(j) * sol_min_n
    	    else
    	        resnew_ne = resnew_n
    	    end if        	        !Not sure 1000 should be here or not!
    	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
    	    RLN = (resnew * CLG/(resnew_n+1.E-5))
    	    RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
    	    
    	    LMF = 0.85 - 0.018 * RLN
    	    if (LMF <0.01) then
    	        LMF = 0.01
    	    else
    	        if (LMF >0.7) then
    	            LMF = 0.7
    	        end if
    	    end if      	  
          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
	        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
          !else
	        !    LMF = 0.
          !end if 	

          LSF =  1 - LMF  
    	  
          sol_LM(ly,j) = sol_LM(ly,j) + LMF * resnew
          sol_LS(ly,j) = sol_LS(ly,j) + LSF * resnew
    	  

            
          !here a simplified assumption of 0.5 LSL
          LSLF = 0.0
          LSLF = CLG          
          
          sol_LSL(ly,j) = sol_LSL(ly,j) + RLR * resnew	          
          sol_LSC(ly,j) = sol_LSC(ly,j) + CFB * LSF * resnew  
          
          sol_LSLC(ly,j) = sol_LSLC(ly,j) + RLR * CFB* resnew
          sol_LSLNC(ly,j) = sol_LSC(ly,j) - sol_LSLC(ly,j)              
            
          !X3 = MIN(X6,0.42*LSF * resnew/150) 
            
          if (resnew_ne >= (CFB * LSF * resnew /150)) then
	         sol_LSN(ly,j) = sol_LSN(ly,j) + CFB * LSF * resnew / 150
	         sol_LMN(ly,j) = sol_LMN(ly,j) + resnew_ne - (CFB * LSF * resnew / 150) + 1.E-25
          else
	         sol_LSN(ly,j) = sol_LSN(ly,j) + resnew_ne
	         sol_LMN(ly,j) = sol_LMN(ly,j) + 1.E-25
          end if	
    	
          !LSNF = sol_LSN(ly,j)/(sol_LS(ly,j)+1.E-5)	
    	  
          sol_LMC(ly,j) = sol_LMC(ly,j) + CFB * LMF * resnew	
          !LMNF = sol_LMN(ly,j)/(sol_LM(ly,j) + 1.E-5)           
           
          if (resnew > 10.) then    
                !update no3 and nh3 in soil
                absorbed_no3(j)= absorbed_no3(j)+sol_no3(ly,j) * abs_f(j) 
                absorbed_nh3(j)=absorbed_nh3(j) + sol_nh4(ly,j) * abs_f(j)                 
                sol_no3(ly,j) = sol_no3(ly,j) * (1.0-abs_f(j))
                sol_nh4(ly,j) = sol_nh4(ly,j) * (1.0-abs_f(j))
          end if

          CFPltSTR(ly,j) = CFB * LSF * resnew 
          CFPltMET(ly,j) = CFB * LMF * resnew

          if (resnew_ne >= (CFB * LSF * resnew /150)) then
	         !sol_LSN(1,j) = sol_LSN(1,j) + CFB * LSF * resnew / 150.
	         !sol_LMN(1,j) = sol_LMN(1,j) + resnew_ne - (CFB * LSF * resnew / 150.) + 1.E-25
             NFPltSTR(ly,j) = CFB * LSF * resnew / 150.
             NFPltMET(ly,j) = resnew_ne - (CFB * LSF * resnew / 150.) + 1.E-25
          else
	         !sol_LSN(1,j) = sol_LSN(1,j) + resnew_ne
	         !sol_LMN(1,j) = sol_LMN(1,j) + 1.E-25
             NFPltSTR(ly,j) = resnew_ne
             NFPltMET(ly,j) = 1.E-25		         
          end if	

        end if
        
        
        !!insert new biomss by zhang    
        !!=============================== 

	end do
   
	!! adjust foliar pesticide for plant removal
      if (hrupest(j) == 1) then
        do k = 1, npmx
          !! calculate amount of pesticide removed with yield
          yldpst = 0.
          if (hvsti(idplt(j)) > 1.001) then
            yldpst = plt_pst(k,j)
            plt_pst(k,j) = 0.
          else
            yldpst = hiad1 * plt_pst(k,j)
            plt_pst(k,j) = plt_pst(k,j) - yldpst
            if (plt_pst(k,j) < 0.) plt_pst(k,j) = 0.
          endif
          !! add pesticide in residue to soil surface
          sol_pst(k,j,1) = sol_pst(k,j,1) + plt_pst(k,j)
          plt_pst(k,j) = 0.
        end do
      end if

	!! summary calculations
      if (curyr > nyskip) then
           wshd_yldn = wshd_yldn + yieldn(j) * hru_dafr(j)
           wshd_yldp = wshd_yldp + yieldp(j) * hru_dafr(j)
           yldkg(icr(j),j) = yldkg(icr(j),j) + yield(j)
           bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
           yldanu(j) = yldanu(j) + yield(j) / 1000.
           bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.
           ncrops(icr(j),j) = ncrops(icr(j),j) + 1
      endif

	!! update curve number
      if (cnop > 0.) call curno(cnop,j)

	!! reset variables
      igro(j) = 0
      idorm(j) = 0
      bio_ms(j) = 0.
	  rwt(j) = 0.
      plantn(j) = 0.
      plantp(j) = 0.
      strsw(j) = 1.
      laiday(j) = 0.
      hvstiadj(j) = 0.
	  rtfr = 0. ! resetting root fraction per layer array to 0

      return
      end