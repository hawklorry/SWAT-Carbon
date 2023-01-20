      subroutine newtillmix(jj,bmix)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and 
!!    biological mixing
!!    New version developed by Armen R. Kemanian in collaboration with Stefan Julich and Cole Rossi
!!    Mixing was extended to all layers
!!    A subroutine to simulate stimulation of organic matter decomposition was added
!!    March 2009: testing has been minimal and further adjustments are expected
!!    use with caution and report anomalous results to akemanian@brc.tamus.edu and jeff.arnold@ars.usda.edu

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# coLonies/ha |Less persistent bacteria in soiL soLution
!!    bactLps(:)    |# coLonies/ha |Less persistent bacteria attached to soiL
!!                                 |particLes
!!    bactpq(:)     |# coLonies/ha |persistent bacteria in soiL soLution
!!    bactps(:)     |# coLonies/ha |persistent bacteria attached to soiL 
!!                                 |particLes
!!    cnop          |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    curyr         |none          |current year of simuLation
!!    deptiL(:)     |mm            |depth of mixing caused by tillage
!!                                 |operation
!!    effmix(:)     |none          |mixing efficiency of tillage operation
!!    soL_nLy(jj)          |none          |maximum number of soiL Layers
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simuLation
!!    nro(:)        |none          |sequence number of year in rotation
!!    ntiL(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    nyskip        |none          |number of years to skip output printing/
!!                                 |summarization
!!    soL_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineraL phosphorus pooL
!!    soL_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pooL
!!    soL_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pooL
!!    soL_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pooL
!!    soL_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pooL in soiL Layer
!!    soL_nLy(:)    |none          |number of soiL Layers
!!    soL_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pooL.
!!    soL_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stabLe
!!                                 |organic N pooL
!!    soL_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pooL
!!    soL_pst(:,:,:)|kg/ha         |amount of pesticide in Layer
!!    soL_rsd(:,:)  |kg/ha         |amount of organic matter in the soiL
!!                                 |cLassified as residue
!!    soL_soLp(:,:) |kg P/ha       |amount of phosohorus stored in soLution
!!    soL_stap(:,:) |kg P/ha       |amount of phosphorus in the soiL Layer
!!                                 |stored in the stabLe mineraL phosphorus pooL
!!    soL_z(:,:)    |mm            |depth to bottom of soiL Layer
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactLpq(:)    |# coLonies/ha |Less persistent bacteria in soiL soLution
!!    bactLps(:)    |# coLonies/ha |Less persistent bacteria attached to soiL
!!                                 |particLes
!!    bactpq(:)     |# coLonies/ha |persistent bacteria in soiL soLution
!!    bactps(:)     |# coLonies/ha |persistent bacteria attached to soiL 
!!                                 |particLes
!!    ntiL(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    soL_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineraL phosphorus pooL
!!    soL_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pooL
!!    soL_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pooL
!!    soL_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pooL
!!    soL_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pooL in soiL Layer
!!    soL_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pooL.
!!    soL_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stabLe
!!                                 |organic N pooL
!!    soL_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pooL
!!    soL_rsd(:,:)  |kg/ha         |amount of organic matter in the soiL
!!                                 |cLassified as residue
!!    soL_soLp(:,:) |kg P/ha       |amount of phosohorus stored in soLution
!!    soL_stap(:,:) |kg P/ha       |amount of phosphorus in the soiL Layer
!!                                 |stored in the stabLe mineraL phosphorus pooL
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    min_res(:)	|kg/ha		   |Min residue aLLowed due to impLementation of 
!!                                 |residue managment in the OPS fiLe.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bmix        |none          |bioLogicaL mixing efficiency: this 
!!                               |number is zero for tillage operations
!!    dg          |mm            |depth of soiL Layer
!!    dtiL        |mm            |depth of mixing
!!    emix        |none          |mixing efficiency
!!    jj          |none          |HRU number
!!    k           |none          |counter
!!    L           |none          |counter
!!    nL          |none          |number of Layers being mixed
!!    smix(:)     |varies        |amount of substance in soiL profiLe
!!                               |that is being redistributed between 
!!                               |mixed Layers
!!    thtiLL(:)   |none          |fraction of soiL Layer that is mixed
!!    soL_msm					 | soL_mass mixed
!!    soL_msn					 | soL_mass not mixed
!!    maxmix      |none          | maximum mixing eff to preserve specified minimum residue cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
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
      
      integer, intent (in) :: jj
      reaL, intent (in) :: bmix
!$$$$$$       integer :: L, k, nL, a
      integer :: ly, k              !CB 12/2/09 nL and a are not used.
      reaL :: emix, dtil, XX, WW1, WW2, WW3, WW4, maxmix
!$$$$$$       reaL :: thtiLL(soL_nLy(jj)), smix(20+npmx)  
      !!by zhang
      !!=============   
      reaL :: smix(22+npmx+14)        !CB 12/2/09 thtiLL is not used. mjw rev 490
      real :: tot_mass                 !sum of total soio mass for layers 2:dtill
      !!changed the dimension from 22 + npmx to 22 + npmx + 12
      !!by zhang
      !!=============
      reaL :: soL_mass(soL_nly(jj))
      reaL :: sol_msm(sol_nly(jj))
      reaL :: sol_msn(sol_nly(jj))
   

      XX = 0.
      WW1 = 0.
      WW2 = 0.
      WW3 = 0.
      WW4 = 0.
      emix = 0.
      dtil = 0.
      if (bmix > 1.e-6) then
        !! biological mixing
        emix = bmix     !bmix MJW (rev 412)
        dtil = Min(sol_z(sol_nly(jj),jj), 50.) ! it was 300.  MJW (rev 412)
      else 
        !! tillage operation
        emix = effmix(idtill)
        dtil = deptil(idtill)
      end if
       
      !! change bulk density in the plow layer added by Junyu Qi 03/2021
      !sol_bdtil(k,j)= sol_bd(k,j)-( sol_bd(k,j)-0.667*sol_bd(k,j) )*emix

      !sz(k,j)=0.2*xx*(1.0+2.0*sol_san(k,j)/(sol_sand(k,j)
    ! &            +exp(8.597-0.075*sol_sand(k,j))))/sol_z(k,j)**0.6
     
      !! f=sz(k,j)/(sz(k,j)+exp(3.92-0.0226*sz(k,j)))
      
      ! sol_bd(k,j)= sol_bd(k,j)+f*(sol_bdset(k,j)
      !till_eff(j)= exp(0.6*0.333*sol_bd(2,j)*emix)
     
      if (bmix < 1.e-6) then          

              tillage_depth(jj) = dtil
              tillage_days(1,jj) = 0
              do ly = 2, sol_nly(jj)
                    if (sol_z(ly,jj) < dtil) then
                        tillage_days(ly,jj) = 0
                    else
                        if (sol_z(ly,jj) >= dtil .and. sol_z(ly-1,jj) <= dtil) then
                            tillage_days(ly,jj) = 0
                        end if
                    end if
              end do
          
              tillage_switch(jj) = 1
              tillage_emix(jj)= emix

             if(idc_till ==3) then
                call tillfactor(jj,bmix,emix,dtil)   !! tillage factor 
             end if 
      end if                     
       
      smix = 0.
      sol_mass = 0.
      !sol_thick = 0.
      sol_msm = 0.
      sol_msn = 0.

	!! incorporate bacteria - no mixing - lost from transport
      if (dtil > 10.) then     
        bactpq(jj) = bactpq(jj) * (1. - emix)
        bactps(jj) = bactps(jj) * (1. - emix)
        bactlpq(jj) = bactlpq(jj) * (1. - emix)
        bactlps(jj) = bactlps(jj) * (1. - emix)
	end if
      	
	!! calculate max mixing to preserve target surface residue MJW rev 490
	!! Assume residue in all other layers is negligible to simplify calculation and remove depth dependency
      if (min_res(jj) > 1. .and. bmix < 0.001) then
	  maxmix = 1 - min_res(jj)/sol_rsd(1,jj)
	  if (maxmix <0.05)  maxmix = 0.05	
	  if (emix > maxmix)  emix = maxmix
      end if


      do ly=1, sol_nly(jj)
        sol_mass(ly) = (sol_thick(ly,jj) / 1000.) * 10000. *  sol_bd(ly,jj) * 1000. * (1.- sol_rock(ly,jj) / 100.)
      end do

!       do l=1,20+npmx
!         smix(ly)=0.
!       end do
      smix = 0.

      if (dtil > 0.) then
!!!  added by Armen 09/10/2010 next line only
        if (dtil < 10.0) dtil = 11.0
	    do ly=1, sol_nly(jj)

          if (sol_z(ly,jj) <= dtil) then
            !! msm = mass of soil mixed for the layer
            !! msn = mass of soil not mixed for the layer		
            sol_msm(ly) = emix * sol_mass(ly)	
            sol_msn(ly) = sol_mass(ly) - sol_msm(ly)	
          else if (sol_z(ly,jj) > dtil.AND.sol_z(ly-1,jj) < dtil) then 
            sol_msm(ly) = emix * sol_mass(ly) * (dtil - sol_z(ly-1,jj)) / sol_thick(ly,jj)
            sol_msn(ly) =  sol_mass(ly) -  sol_msm(ly)
          else
            sol_msm(ly) = 0.
            sol_msn(ly) = sol_mass(ly)
          end if
			
          !! calculate the mass or concentration of each mixed element 
          !! mass based mixing
          WW1 = sol_msm(ly)/(sol_msm(ly) + sol_msn(ly))
          smix(1) = smix(1) + sol_no3(ly,jj) * WW1
          smix(2) = smix(2) + sol_orgn(ly,jj) * WW1
          smix(3) = smix(3) + sol_nh4(ly,jj) * WW1
          smix(4) = smix(4) + sol_solp(ly,jj) * WW1
          smix(5) = smix(5) + sol_orgp(ly,jj) * WW1
          smix(6) = smix(6) + sol_aorgn(ly,jj) * WW1
          smix(7) = smix(7) + sol_actp(ly,jj) * WW1
          smix(8) = smix(8) + sol_fon(ly,jj) * WW1
          smix(9) = smix(9) + sol_fop(ly,jj) * WW1
          smix(10) = smix(10) + sol_stap(ly,jj) * WW1
          smix(11) = smix(11) + sol_rsd(ly,jj) * WW1
          smix(12) = smix(12) + sol_mc(ly,jj) * WW1
          smix(13) = smix(13) + sol_mn(ly,jj) * WW1
          smix(14) = smix(14) + sol_mp(ly,jj) * WW1

		!! concentration based mixing
          WW2 = XX + sol_msm(ly)
          smix(15) = (XX * smix(15) + sol_cbn(ly,jj) * sol_msm(ly)) /WW2
          smix(16) = (XX * smix(16) + sol_n(ly,jj) * sol_msm(ly)) /WW2
          smix(17) = (XX * smix(17) + sol_clay(ly,jj) * sol_msm(ly)) /WW2
          smix(18) = (XX * smix(18) + sol_silt(ly,jj) * sol_msm(ly)) /WW2
          smix(19) = (XX * smix(19) + sol_sand(ly,jj) * sol_msm(ly)) /WW2
!          smix(20) = (XX * smix(20) + sol_rock(ly,jj) * sol_msm(ly)) / WW2
!          smix(21) = (XX * smix(21) + sol_ph(ly,jj) * sol_msm(ly)) /WW2 !! mjw rev490
!          smix(22) = (XX * smix(22) + sol_cal(ly,jj) * sol_msm(ly)) /WW2 !! mjw rev490
		!! mass based distribution
          do k = 1, npmx
          	smix(20+k) = smix(20+k) + sol_pst(k,jj,ly) * WW1
          end do

            !!by zhang
            !!============== 
            if (cswat == 2 ) then         
	        smix(20+npmx+1) = smix(20+npmx+1) +sol_LSC(ly,jj)* WW1
	        smix(20+npmx+2) = smix(20+npmx+2) +sol_LSLC(ly,jj)* WW1
	        smix(20+npmx+3) = smix(20+npmx+3) +sol_LSLNC(ly,jj)* WW1
	        smix(20+npmx+4) = smix(20+npmx+4) +sol_LMC(ly,jj)* WW1
	        smix(20+npmx+5) = smix(20+npmx+5) +sol_LM(ly,jj)* WW1
	        smix(20+npmx+6) = smix(20+npmx+6) +sol_LSL(ly,jj)* WW1
	        smix(20+npmx+7) = smix(20+npmx+7) +sol_LS(ly,jj)* WW1	        

              smix(20+npmx+8) = smix(20+npmx+8) + sol_HSC(ly,jj)*WW1
              smix(20+npmx+9) = smix(20+npmx+9) + sol_HPC(ly,jj)*WW1
             	        
	        smix(20+npmx+10) = smix(20+npmx+10) +sol_LSN(ly,jj)* WW1
	        smix(20+npmx+11) = smix(20+npmx+11) +sol_LMN(ly,jj)* WW1
	        smix(20+npmx+12) = smix(20+npmx+12) +sol_BMN(ly,jj)* WW1
	        smix(20+npmx+13) = smix(20+npmx+13) +sol_HSN(ly,jj)* WW1
	        smix(20+npmx+14) = smix(20+npmx+14) +sol_HPN(ly,jj)* WW1  
	      end if
            !!by zhang 	
            !!=============

           

 			
          XX = XX + sol_msm(ly)
        end do

          do ly=1, sol_nly(jj)
			
            ! reconstitute each soil layer 
            WW3 = sol_msn(ly) / sol_mass(ly)
            WW4 = sol_msm(ly) / XX

            sol_no3(ly,jj) = sol_no3(ly,jj) * WW3 + smix(1) * WW4
            sol_orgn(ly,jj) = sol_orgn(ly,jj) * WW3 + smix(2) * WW4
            sol_nh4(ly,jj) = sol_nh4(ly,jj) * WW3 + smix(3) * WW4
            sol_solp(ly,jj) = sol_solp(ly,jj) * WW3 + smix(4) * WW4
            sol_orgp(ly,jj) = sol_orgp(ly,jj) * WW3 + smix(5) * WW4
            sol_aorgn(ly,jj) = sol_aorgn(ly,jj) * WW3 + smix(6) * WW4
            sol_actp(ly,jj) = sol_actp(ly,jj) * WW3 + smix(7) * WW4
            sol_fon(ly,jj) = sol_fon(ly,jj) * WW3 + smix(8) * WW4
            sol_fop(ly,jj) = sol_fop(ly,jj) * WW3 + smix(9) * WW4
            sol_stap(ly,jj) = sol_stap(ly,jj) * WW3 + smix(10) * WW4
            sol_rsd(ly,jj) = sol_rsd(ly,jj) * WW3 + smix(11) * WW4
            if (sol_rsd(ly,jj) < 1.e-10) sol_rsd(ly,jj) = 1.e-10
            sol_mc(ly,jj) = sol_mc(ly,jj) * WW3 + smix(12) * WW4
            sol_mn(ly,jj) = sol_mn(ly,jj) * WW3 + smix(13) * WW4
            sol_mp(ly,jj) = sol_mp(ly,jj) * WW3 + smix(14) * WW4

            sol_cbn(ly,jj) = (sol_cbn(ly,jj) * sol_msn(ly) + smix(15) * sol_msm(ly)) / sol_mass(ly)
            sol_n(ly,jj) = (sol_n(ly,jj) * sol_msn(ly) + smix(16) * sol_msm(ly)) / sol_mass(ly)
            sol_clay(ly,jj) = (sol_clay(ly,jj) * sol_msn(ly) + smix(17) * sol_msm(ly)) / sol_mass(ly)
            sol_silt(ly,jj) = (sol_silt(ly,jj) * sol_msn(ly) + smix(18) * sol_msm(ly)) / sol_mass(ly)
            sol_sand(ly,jj) = (sol_sand(ly,jj) * sol_msn(ly) + smix(19) * sol_msm(ly)) / sol_mass(ly)
!		sol_rock(ly,jj) = (sol_rock(ly,jj) * sol_msn(ly) + smix(20) * sol_msm(ly)) / sol_mass(ly)
!            sol_ph(ly,jj) = (sol_ph(ly,jj) * sol_msn(ly) + smix(21)        &
!     &           * sol_msm(ly)) / sol_mass(ly) !! mjw rev 490 simplified, PH not linear
!            sol_cal(ly,jj) = (sol_cal(ly,jj) * sol_msn(ly) + smix(22)      &
!     &           * sol_msm(ly)) / sol_mass(ly) !! mjw rev 490 


			
            do k = 1, npmx
              sol_pst(k,jj,ly) = sol_pst(k,jj,ly) * WW3 + smix(20+k) * WW4
            end do

             !!by zhang
             !!=============
             if (cswat == 2   ) then
             
             sol_LSC(ly,jj) = sol_LSC(ly,jj)*WW3+smix(20+npmx+1)* WW4
             sol_LSLC(ly,jj) = sol_LSLC(ly,jj)*WW3+smix(20+npmx+2)* WW4
             sol_LSLNC(ly,jj) = sol_LSLNC(ly,jj)*WW3+smix(20+npmx+3)* WW4
             sol_LMC(ly,jj) = sol_LMC(ly,jj)*WW3 + smix(20+npmx+4)* WW4
             sol_LM(ly,jj) = sol_LM(ly,jj)*WW3 + smix(20+npmx+5)* WW4
             sol_LSL(ly,jj) = sol_LSL(ly,jj)*WW3 + smix(20+npmx+6)* WW4
             sol_LS(ly,jj) = sol_LS(ly,jj)*WW3 + smix(20+npmx+7)* WW4
             sol_HSC(ly,jj) = sol_HSC(ly,jj)*WW3 + smix(20+npmx+8)* WW4
             sol_HPC(ly,jj) = sol_HPC(ly,jj)*WW3 + smix(20+npmx+9)* WW4    
             
             sol_LSN(ly,jj) = sol_LSN(ly,jj)*WW3 + smix(20+npmx+10)* WW4
             sol_LMN(ly,jj) = sol_LMN(ly,jj)*WW3 + smix(20+npmx+11)* WW4
             sol_BMN(ly,jj) = sol_BMN(ly,jj)*WW3 + smix(20+npmx+12)* WW4
             sol_HSN(ly,jj) = sol_HSN(ly,jj)*WW3 + smix(20+npmx+13)* WW4
             sol_HPN(ly,jj) = sol_HPN(ly,jj)*WW3 + smix(20+npmx+14)* WW4
             end if
            !!by zhang 
            !!==============

           

	  end do !End of do l=1, sol_nly(jj)

  

       !!EPIC
       !!redistribute SOM3C and SOM3E in the SRFC layer to layers 2: dtill
       !!=============
       if (cswat == 2) then
        sol_msm = 0.
        sol_msn = 0.
        tot_mass = 0.
	    do ly=1, sol_nly(jj)        
	        if (ly /= SRFC) then
              if (sol_z(ly,jj) <= dtil) then
                !! msm = mass of soil mixed for the layer
                !! msn = mass of soil not mixed for the layer		
                sol_msm(ly) = emix * sol_mass(ly)	
                sol_msn(ly) = sol_mass(ly) - sol_msm(ly)	
                tot_mass = tot_mass + sol_msn(ly)
              else if (sol_z(ly,jj) > dtil.AND.sol_z(ly-1,jj) < dtil) then
                sol_msm(ly) = emix * sol_mass(ly) *  (dtil - sol_z(ly-1,jj)) / sol_thick(ly,jj)
                sol_msn(ly) =  sol_mass(ly) -  sol_msm(ly)
                tot_mass = tot_mass + sol_msn(ly)
              end if
              
             end if          
        end do

        sol_msm = 0.
        sol_msn = 0.
	    do ly=1, sol_nly(jj)       
	        if (ly /= SRFC) then
                  if (sol_z(ly,jj) <= dtil) then
                    !! msm = mass of soil mixed for the layer
                    !! msn = mass of soil not mixed for the layer		
                    sol_msm(ly) = emix * sol_mass(ly)	
                    sol_msn(ly) = sol_mass(ly) - sol_msm(ly)	
                    sol_HPC(ly,jj)=  sol_HPC(ly,jj)+ sol_HPC(SRFC,jj) *sol_msn(ly)/tot_mass
                    sol_HPN(ly,jj)= sol_HPN(ly,jj)+ sol_HPN(SRFC,jj)  *sol_msn(ly)/tot_mass  
                  else if (sol_z(ly,jj) > dtil.AND.sol_z(ly-1,jj) < dtil) then
                    sol_msm(ly) = emix * sol_mass(ly) *  (dtil - sol_z(ly-1,jj)) / sol_thick(ly,jj)
                    sol_msn(ly) =  sol_mass(ly) -  sol_msm(ly)
                    sol_HPC(ly,jj)=sol_HPC(ly,jj)+ sol_HPC(SRFC,jj) *sol_msn(ly)/tot_mass
                    sol_HPN(ly,jj)=sol_HPN(ly,jj)+ sol_HPN(SRFC,jj) *sol_msn(ly)/tot_mass  
                  end if              
   
            end if          
        end do
       end if   !if (cswat == 2) then
       sol_HPC(1,jj) = 0.
       sol_HPN(1,jj) = 0.
       !!redistribute SOM3C and SOM3E in the SRFC layer to layers 2: dtill
       !!=============


        if (cswat == 1) then
       !     call tillfactor(jj,bmix,emix,dtil,sol_thick)    
        end if
      
      	!! summary calculations
        if (curyr > nyskip) then
            sumix(jj) = sumix(jj) + emix
        end if

       
    
      end if
	
      !! perform final calculations for tillage operation
 
      !! count the tillage only if it is a scheduled operation biomix does not count MJW Rev 490
      if (bmix <= 1.e-6) then
        ntil(jj) = ntil(jj) + 1
      end if
      if (cnop > 1.e-4) call curno(cnop,jj)
      
      !ntil(jj) = ntil(jj) + 1 ' orig code

      return
      end
