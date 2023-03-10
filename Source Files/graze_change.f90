      subroutine graze
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates biomass lost to grazing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)  |none          |bacteria partition coefficient:
!!                                |1: all bacteria in solution
!!                                |0: all bacteria sorbed to soil particles
!!    bactlp_plt(:)|# cfu/m^2     |less persistent bacteria on foliage
!!    bactlpdb(:)  |# cfu/g       |concentration of less persistent
!!                                |bacteria in manure(fertilizer)
!!    bactlpq(:)   |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)   |# cfu/m^2     |less persistent bacteria attached to soil
!!                                |particles
!!    bactp_plt(:) |# cfu/m^2     |persistent bacteria on foliage
!!    bactpdb(:)   |# cfu/g       |concentration of persistent bacteria
!!                                |in manure(fertilizer)
!!    bactpq(:)    |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)    |# cfu/m^2     |persistent bacteria attached to soil particles
!!    bio_min(:)   |kg/ha         |minimum plant biomass for grazing
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_eat(:)   |(kg/ha)/day   |dry weight of biomass removed by grazing
!!                                |daily
!!    bio_trmp(:)  |(kg/ha)/day   |dry weight of biomass removed by
!!                                |trampling daily
!!    curyr        |none          |current year of simulation
!!    fminn(:)     |kg minN/kg frt|fraction of mineral N (NO3 + NH3) in 
!!                                |fertilizer/manure
!!    fminp(:)     |kg minP/kg frt|fraction of mineral P in fertilizer/manure
!!    fnh3n(:)     |kg NH3-N/kg minN|fraction of NH3-N in mineral N in 
!!                                |fertilizer/manure
!!    forgn(:)     |kg orgN/kg frt|fraction of organic N in fertilizer/manure
!!    forgp(:)     |kg orgP/kg frt|fraction of organic P in fertilizer/manure
!!    grazn        |kg N/ha       |total amount of nitrogen applied to soil
!!                                |during grazing in HRU on day
!!    grazp        |kg P/ha       |total amount of phosphorus applied to soil
!!                                |during grazing in HRU on day
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    iida         |julian date   |day being simulated (current julian day
!!    manure_id(:) |none          |manure (fertilizer) identification
!!                                |number from fert.dat
!!    igrz(:)      |none          |grazing flag for HRU:
!!                                |0 HRU currently not grazed
!!                                |1 HRU currently grazed
!!    ihru         |none          |HRU number
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    grz_days(:)  |none          |number of days grazing will be simulated
!!    ngr(:)       |none          |sequence number of grazing operation
!!                                |within the year
!!    nro(:)       |none          |sequence number of year in rotation
!!    nyskip       |none          |number of years to skip output summarization
!!                                |and printing
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    phug(:,:,:)  |none          |fraction of plant heat units at which
!!                                |grazing begins
!!    plantn(:)    |kg N/ha       |amount of nitrogen in plant
!!    plantp(:)    |kg P/ha       |amount of phosphorus in plant
!!    pltfr_n(:)   |none          |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)   |none          |fraction of plant biomass that is phosphorus
!!    sol_bd(:,:)  |Mg/m**3       |bulk density of the soil
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_nh4(:,:) |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                |pool in soil layer
!!    sol_no3(:,:) |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                                |in soil layer
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    manure_kg(:) |(kg/ha)/day   |dry weight of manure deposited on HRU
!!                                |daily
!!    wshd_fminp   |kg P/ha       |average annual amount of mineral P applied
!!                                |in watershed
!!    wshd_fnh3    |kg N/ha       |average annual amount of NH3-N applied in
!!                                |watershed
!!    wshd_fno3    |kg N/ha       |average annual amount of NO3-N applied in
!!                                |watershed
!!    wshd_orgn    |kg N/ha       |average annual amount of organic N applied
!!                                |in watershed
!!    wshd_orgp    |kg P/ha       |average annual amount of organic P applied
!!                                |in watershed
!!    wshd_ftotn   |kg N/ha       |average annual amount of N (mineral &
!!                                |organic) applied in watershed
!!    wshd_ftotp   |kg P/ha       |average annual amount of P (mineral &
!!                                |organic) applied in watershed
!!    yldkg(:,:,:) |kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlp_plt(:)|# cfu/m^2    |less persistent bacteria on foliage
!!    bactlpq(:)  |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)  |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles
!!    bactp_plt(:)|# cfu/m^2     |persistent bacteria on foliage
!!    bactpq(:)   |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)   |# cfu/m^2     |persistent bacteria attached to soil particles
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    grazn       |kg N/ha       |total amount of nitrogen applied to soil
!!                               |during grazing in HRU on day
!!    grazp       |kg P/ha       |total amount of phosphorus applied to soil
!!                               |during grazing in HRU on day
!!    igrz(:)     |none          |grazing flag for HRU:
!!                               |0 HRU currently not grazed
!!                               |1 HRU currently grazed
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    ndeat(:)    |days          |number of days HRU has been grazed
!!    ngr(:)      |none          |sequence number of grazing operation
!!                               |within the year
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_nh4(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus stored in solution
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_orgn   |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_orgp   |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    yldkg(:,:,:)|kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dmi         |kg/ha         |biomass in HRU prior to grazing
!!    dmii        |kg/ha         |biomass prior to trampling
!!    frt_t       |
!!    gc          |
!!    gc1         |
!!    it          |none          |manure/fertilizer id number from fert.dat
!!    j           |none          |HRU number
!!    l           |none          |number of soil layer that manure is applied
!!    swf         |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: Erfc

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
      
      integer :: j, ly, it
      real :: dmi, dmii, gc, gc1, swf, frt_t, xx, XXX
      real :: resnew_n = 0.
      real :: resnew_C = 0.
      real :: BLG1, BLG2, BLG3, CLG, sol_min_n, resnew, resnew_ne
      real :: RLN, RLR, LMF, LSF, ORGC_F, X1, X8, X10, YY, ZZ, XZ, YZ
      
      j = 0
      j = ihru

      !! graze only if adequate biomass in HRU
      if (bio_ms(j) > bio_min(j)) then

        !! determine new biomass in HRU
        dmi = 0.
        dmi = bio_ms(j)
        bio_ms(j) = bio_ms(j) - bio_eat(j)
        if (bio_ms(j) < bio_min(j)) bio_ms(j) = bio_min(j)

        !!add by zhang
        !!=================
        if (cswat == 2) then
            emitc_d(j) = emitc_d(j) + dmi - bio_ms(j)
        end if
        !!add by zhang
        !!=================        
                 
        !! adjust nutrient content of biomass
        plantn(j) = plantn(j) - (dmi - bio_ms(j)) * pltfr_n(j)
        plantp(j) = plantp(j) - (dmi - bio_ms(j)) * pltfr_p(j)
        if (plantn(j) < 0.) plantn(j) = 0.
        if (plantp(j) < 0.) plantp(j) = 0.

        !! remove trampled biomass and add to residue
        dmii = 0.
        dmii = bio_ms(j)
        bio_ms(j) = bio_ms(j) - bio_trmp(j)                   
        if (bio_ms(j) < bio_min(j))  then
          sol_rsd(1,j) = sol_rsd(1,j) + dmii - bio_min(j)
          bio_ms(j) = bio_min(j)
        else
          sol_rsd(1,j) = sol_rsd(1,j) + bio_trmp(j)   
        endif
        sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
        bio_ms(j) = Max(bio_ms(j),0.)


        !!add by zhang
        !!=================
        if (cswat == 2) then
            rsdc_d(j) = rsdc_d(j) + (dmii - bio_ms(j)) * CFB
            rsdn_d(j) = rsdn_d(j)+ (dmii-bio_ms(j)) * pltfr_n(j)           
        end if
        !!add by zhang
        !!=================   
          
         OrgC_Plt2Rsd(j)=OrgC_Plt2Rsd(j)+ (dmii - bio_ms(j)) *CFB                !!residue C input
         OrgN_Plt2Rsd(j)=OrgN_Plt2Rsd(j)+ (dmii-bio_ms(j)) * pltfr_n(j)   !!residue N input
         OrgP_Plt2Rsd(j)=OrgP_Plt2Rsd(j)+ (dmii-bio_ms(j)) * pltfr_p(j)   !!residue P input              
 

        !! adjust nutrient content of residue and biomass for
        !! trampling
        plantn(j) = plantn(j) - (dmii - bio_ms(j)) * pltfr_n(j)
        plantp(j) = plantp(j) - (dmii - bio_ms(j)) * pltfr_p(j)
        if (plantn(j) < 0.) plantn(j) = 0.
        if (plantp(j) < 0.) plantp(j) = 0.
        if (dmii - bio_ms(j) > 0.) then
            sol_fon(1,j) = (dmii - bio_ms(j)) * pltfr_n(j) + sol_fon(1,j)
            sol_fop(1,j) = (dmii - bio_ms(j)) * pltfr_p(j) + sol_fop(1,j) 
            
            !!insert new biomss by zhang
            !!===========================
            if (cswat == 2) then
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
                XXX = log(0.5/BLG1-0.5)
                BLG2 = (XXX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XXX + 0.5*BLG2
                CLG=BLG3*phuacc(j)/(phuacc(j)+ &
				EXP(BLG1-BLG2*phuacc(j)))

	          !if (k == 1) then
		        !abs_f(j) = 0.05
	          !else
		        !abs_f(j) = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (sol_no3(1,j)+sol_nh4(1,j))
	          	          
	          resnew = (dmii - bio_ms(j)) 
	          resnew_n = (dmii - bio_ms(j)) * pltfr_n(j)   	  
	         
        	    
	          if (resnew > 10.) then   
        	        resnew_ne = resnew_n + abs_f(j) * sol_min_n
        	    else
        	        resnew_ne = resnew_n
        	    end if        	    
        	    
        	    
        	        !Not sure 1000 should be here or not!
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
        	  
	          sol_LM(1,j) = sol_LM(1,j) + LMF * resnew
	          sol_LS(1,j) = sol_LS(1,j) + LSF * resnew
        	  

                
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          sol_LSL(1,j) = sol_LSL(1,j) + RLR*resnew	          
	          sol_LSC(1,j) = sol_LSC(1,j) + CFB*LSF * resnew  
	          
	          sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*CFB* resnew
	          sol_LSLNC(1,j) = sol_LSC(1,j) - sol_LSLC(1,j)              
                
                !X3 = MIN(X6,CFB*LSF * resnew/150) 
                
	          if (resnew_ne >= (CFB * LSF * resnew /150)) then
		         sol_LSN(1,j) = sol_LSN(1,j) + CFB * LSF * resnew / 150
		         sol_LMN(1,j) = sol_LMN(1,j) + resnew_ne - (CFB * LSF * resnew / 150) + 1.E-25
	          else
		         sol_LSN(1,j) = sol_LSN(1,j) + resnew_ne
		         sol_LMN(1,j) = sol_LMN(1,j) + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          sol_LMC(1,j) = sol_LMC(1,j) + CFB * LMF * resnew	
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
              if (resnew > 10.) then    
                absorbed_no3(j) = absorbed_no3(j) + sol_no3(1,j) * abs_f(j)    !!Zhang added
                absorbed_nh3(j) = absorbed_nh3(j) + sol_nh4(1,j) * abs_f(j)     !!Zhang added                     
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
             
            end if      !if (cswat == 2) then
            !!insert new biomss by zhang
            !!===========================

        end if  !if (dmii - bio_ms(j) > 0.) then


        !! apply manure
        it = 0
        it = manure_id(j)
        
        ly = 1
        if (manure_kg(j) > 0.) then 
          
          if (cswat == 0) then

              sol_no3(ly,j) = sol_no3(ly,j) + manure_kg(j)    * (1. - fnh3n(it)) * fminn(it)
              sol_fon(ly,j) = sol_fon(ly,j) + manure_kg(j)    *  forgn(it)
              sol_nh4(ly,j) = sol_nh4(ly,j) + manure_kg(j)    *  fnh3n(it) * fminn(it)
              sol_solp(ly,j) = sol_solp(ly,j) + manure_kg(j)  * fminp(it)
              sol_fop(ly,j) = sol_fop(ly,j) + manure_kg(j)    * forgp(it)
          end if
          if (cswat == 1) then
              sol_no3(ly,j) = sol_no3(ly,j) + manure_kg(j)    * (1. - fnh3n(it)) * fminn(it)
              sol_mn(ly,j) = sol_mn(ly,j) + manure_kg(j)      * forgn(it)
              sol_nh4(ly,j) = sol_nh4(ly,j) + manure_kg(j)    * fnh3n(it) * fminn(it)
              sol_solp(ly,j) = sol_solp(ly,j) + manure_kg(j)  * fminp(it)
              sol_mp(ly,j) = sol_mp(ly,j) + manure_kg(j)      * forgp(it)          
              sol_mc(ly,j) = sol_mc(ly,j) + manure_kg(j)       * forgn(it) * 10.
          end if
          
          !!By Zhang for C/N cycling
          !!===============================  
          if (cswat == 2) then
                sol_no3(ly,j) = sol_no3(ly,j) + manure_kg(j) * (1. - fnh3n(it)) * fminn(it)
                no3_fert(j) = no3_fert(j)+ manure_kg(j) * (1. - fnh3n(it)) * fminn(it)
                sol_nh4(ly,j) = sol_nh4(ly,j) + manure_kg(j) *  fnh3n(it) * fminn(it)
                nh4_fert(j) = nh4_fert(j) + manure_kg(j) *  fnh3n(it) * fminn(it)
                sol_solp(ly,j) = sol_solp(ly,j) + manure_kg(j) *  fminp(it)
                sol_fop(ly,j) = sol_fop(ly,j) + manure_kg(j) * forgp(it)  
           
                orgn_grazf(j) =  orgn_grazf(j)+ manure_kg(j) * forgn(it)    
                nh3_grazf(j) =  nh3_grazf(j)+manure_kg(j) * fnh3n(it) * fminn(it)  
                no3_grazf(j) =  no3_grazf(j)+ manure_kg(j) *	(1. - fnh3n(it)) * fminn(it)
                solp_grazf(j) =  solp_grazf(j)+ manure_kg(j) * fminp(it) 
                          
                orgp_grazf(j) =  orgp_grazf(j)+ manure_kg(j) * forgp(it) 
 


                if (forgn(it)  .gt. 1.E-6) then    
                    !sol_fon(ly,j) = sol_fon(ly,j) + manure_kg(j) * forgn(it)
                    orgc_f = 0.35  
                    X1 = manure_kg(j) 
                    X8 = X1 * orgc_f  
                  
                    OrgC_Fer(j) = OrgC_Fer(j) + X8       !(kg/ha)/day   |dry weight of manure C deposited on HRU  
                  
                    !RLN = .175 *(orgc_f)/(fminp(it) + forgn(it) + 1.e-5)
                    RLN = .175 *(orgc_f)/(forgn(it) + 1.e-5)
                    X10 = .85-.018*RLN
                    if (X10<0.01) then
                        X10 = 0.01
                    else
                        if (X10 > .7) then
                            X10 = .7
                        end if
                    end if
                    XX = X8 * X10
                    sol_LMC(ly,j) = sol_LMC(ly,j) + XX
                  
                    YY = manure_kg(j) * X10
                    sol_LM(ly,j) = sol_LM(ly,j) + YY
                  
                    ZZ = manure_kg(j) *forgn(it) * X10
                    sol_LMN(ly,j) = sol_LMN(ly,j) + ZZ
                    sol_LSN(ly,j) = sol_LSN(ly,j) + manure_kg(j) *forgn(it) -ZZ
                  
                    sol_fon(ly,j) = sol_LMN(ly,j) + sol_LSN(ly,j)
                  
                    XZ = manure_kg(j) *orgc_f-XX
                    sol_LSC(ly,j) = sol_LSC(ly,j) + XZ
                    sol_LSLC(ly,j) = sol_LSLC(ly,j) + XZ * .175          
                    sol_LSLNC(ly,j) = sol_LSLNC(ly,j) + XZ * (1.-.175) 
                  
                    YZ = manure_kg(j) - YY
                    sol_LS(ly,j) = sol_LS(ly,j) + YZ
                    sol_LSL(ly,j) = sol_LSL(ly,j) + YZ*.175

                    CFOrfSTR(1,j) = XZ
                    CFOrfMET(1,j) = XX                   

                    NFPltSTR(ly,j) = manure_kg(j) *forgn(it) -ZZ
                    NFPltMET(ly,j) = ZZ    
                else
                    orgc_f = 0.
                    CFOrfSTR(1,j) = 0.
                    CFOrfMET(1,j) = 0.                   

                    NFPltSTR(ly,j) = 0.
                    NFPltMET(ly,j) = 0.                  
                
                End if  
    
          end if
          !!By Zhang for C/N cycling
          !!=============================== 




!! add bacteria - #cfu/g * t(manure)/ha * 1.e6 g/t * ha/10,000 m^2 = 100.
!! calculate ground cover
          gc = 0.
          gc = (1.99532 - Erfc(1.333 * laiday(j) - 2.)) / 2.1
          if (gc < 0.) gc = 0.

          gc1 = 0.
          gc1 = 1. - gc

          swf = .15

          frt_t = 0.
          frt_t = bact_swf * manure_kg(j) / 1000.

          bactp_plt(j) = gc * bactpdb(it) * frt_t * 100. + bactp_plt(j)
          bactlp_plt(j) = gc * bactlpdb(it) * frt_t * 100.+bactlp_plt(j)

          bactpq(j) = gc1 * bactpdb(it)  * frt_t * 100. + bactpq(j)
          bactpq(j) = bactkddb(it) * bactpq(j)

          bactps(j) = gc1 * bactpdb(it) * frt_t * 100. + bactps(j)
          bactps(j) = (1. - bactkddb(it)) * bactps(j)

          bactlpq(j) = gc1 * bactlpdb(it) * frt_t * 100. + bactlpq(j)
          bactlpq(j) = bactkddb(it) * bactlpq(j)

          bactlps(j) = gc1 * bactlpdb(it) * frt_t * 100. + bactlps(j)
          bactlps(j) = (1. - bactkddb(it)) * bactlps(j)

        endif

        !! reset leaf area index and fraction of growing season
        if (dmi > 1.) then
          laiday(j) = laiday(j) * bio_ms(j) / dmi
          phuacc(j) = phuacc(j) * bio_ms(j) / dmi
        else
          laiday(j) = 0.05
          phuacc(j) = 0.
        endif


        !! summary calculations
        !! I do not understand these summary calculations Armen March 2009
        grazn = grazn + manure_kg(j)               *  &
			(fminn(it) + forgn(it))
        grazp = grazp + manure_kg(j)               *  &
			 (fminp(it) + forgp(it))
        tgrazn(j) = tgrazn(j) + grazn
        tgrazp(j) = tgrazp(j) + grazp

        if (curyr > nyskip) then
          wshd_ftotn = wshd_ftotn + manure_kg(j)               *     	&   
                    hru_dafr(j) * (fminn(it) + forgn(it))
          wshd_forgn = wshd_forgn + manure_kg(j)               *     	&   
                    hru_dafr(j) * forgn(it)
          wshd_fno3 = wshd_fno3 + manure_kg(j)               *      	&    
                    hru_dafr(j) * fminn(it) * (1. - fnh3n(it))
          wshd_fnh3 = wshd_fnh3 + manure_kg(j)               *          &
                    hru_dafr(j) * fminn(it) * fnh3n(it)
          wshd_ftotp = wshd_ftotp + manure_kg(j)               *        &
                    hru_dafr(j) * (fminp(it) + forgp(it))
          wshd_fminp = wshd_fminp + manure_kg(j)               *        &
                    hru_dafr(j) * fminp(it)
          wshd_forgp = wshd_forgp + manure_kg(j)               *        & 
                    hru_dafr(j) * forgp(it)
  !       yldkg(nro(j),1,j) = yldkg(nro(j),1,j) + (dmi - bio_ms(j))
      yldkg(icr(j),j)=yldkg(icr(j),j) + (dmi - bio_ms(j))
        end if
      end if

!! check to set if grazing period is over
      if (ndeat(j) == grz_days(j)) then
        igrz(j) = 0
        ndeat(j) = 0
        ngr(j) = ngr(j) + 1
      end if


      return
      end