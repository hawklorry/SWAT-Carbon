        subroutine carbon2
        use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
        implicit none
        !!============================================
        !!Input variables
        !!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil	
        !!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer on
        !!                                 |current day
        !!    sol_fc(:,:)   |mm H2O        |amount of water available to plants in soil 
        !!                                 |layer at field capacity (fc - wp),Index:(layer,HRU)
        !!    sol_wp(:,:)   |mm H20/mm soil|water content of soil at -1.5 MPa (wilting
        !!                                 |point)
        !!    sol_wpmm(:,:) |mm H20        |water content of soil at -1.5 MPa (wilting
        !!                                 |point)
        !!==============================================
        !!Transput variables;
        !!  sol_HSC(:,:)    : mass of C present in slow humus (kg ha-1)
        !!  sol_HSN(:,:)    : mass of N present in slow humus (kg ha-1)
        !!  sol_HPC(:,:)    : mass of C present in passive humus (kg ha-1)
        !!  sol_HPN(:,:)    : mass of N present in passive humus (kg ha-1)
        !!  sol_LM(:,:)     : mass of metabolic litter (kg ha-1)
        !!  sol_LMC(:,:     : mass of C in metabolic litter (kg ha-1)
        !!  sol_LMN(:,:)    : mass of N in metabolic litter (kg ha-1)
        !!  sol_LS(:,:)     : mass of structural litter (kg ha-1)
        !!  sol_LSC(:,:)    : mass of C in structural litter (kg ha-1)
        !!  sol_LSL(:,:)    : mass of lignin in structural litter (kg ha-1)
        !!  sol_LSN(:,:)    : mass of N in structural litter (kg ha-1)
        !!  STD(:)          : standing dead (kg ha-1)                                               (Not used)
        !!  STDL(:)         : mass of lignin in standing dead (kg ha-1)                             (Not used)
        !!  STDN(:)         : mass of N in standing dead (dead plants + sorbed from soil; kg ha-1)  (Not used)
        !!  STDNEl(:)       : standing dead N after enrichment with sorbed N in a soil layer (kg ha-1)
        !!  sol_no3(:,:)    : weight of NO3-N in soil layer (kg ha-1)
        !!  sol_nh4(:,:)    : weight of NH3-N in soil layer (kg ha-1)

        !!==============================================
        !!read in parameters
       !Sf      : fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter     

       !LSR     : rate of potential transformation of structural litter under optimal conditions
                !(surface = 0.0107 day-1; all other layers= 0.0132 day-1) (Parton et al., 1994)


        !!==============================================


       !BMC     : mass of C in soil microbial biomass and associated products (kg ha-1)
       !BMCTP   : potential transformation of C in microbial biomass (kg ha-1 day-1)
       !BMN     : mass of N in soil microbial biomass and associated products (kg ha-1)
       !BMNTP   : potential transformation of N in microbial biomass (kg ha-1 day-1)

       !CDG     : soil temperature control on biological processes
       !CNR     : C/N ratio of standing dead
       !CS      : combined factor controlling biological processes [CS = sqrt(CDG×SUT)* 0.8*OX*X1), CS < 10; CS = 10, CS>=10 (Williams, 1995)]
       !DBp     : soil bulk density of plow layer (Mg m-3) (Not used)
       !HSCTP   : potential transformation of C in slow humus (kg ha-1 day-1)
       !HSNTP   : potential transformation of N in slow humus (kg ha-1 day-1)
       !HPCTP(k,j)   : potential transformation of C in passive humus (kg ha-1 day-1)
       !HPNTP   : potential transformation of N in passive humus (kg ha-1 day-1)    
       !LMF     : fraction of the litter that is metabolic    
       !LMNF    : fraction of metabolic litter that is N (kg kg-1)  
       !LMCTP   : potential transformation of C in metabolic litter (kg ha-1 day-1)
       !LMNTP   : potential transformation of N in metabolic litter (kg ha-1 day-1)
       !LSCTP   : potential transformation of C in structural litter (kg ha-1 day-1)
       !LSF     : fraction of the litter that is structural
       !LSLF    : fraction of structural litter that is lignin (kg kg-1)
       !LSNF    : fraction of structural litter that is N (kg kg-1)
       !LSLCTP  : potential transformation of C in lignin of structural litter (kg ha-1 day-1)
       !LSLNCTP : potential transformation of C in nonlignin structural litter (kg ha-1 day-1)  
       !LSNTP   : potential transformation of N in structural litter (kg ha-1 day-1)
       !NCBM    : N/C ratio of biomass
       !NCHP    : N/C ratio passive humus
       !NCHS    : N/C ratio of the slow humus
       !OX      : oxygen control on biological processes with soil depth
       !SUT     : soil water control on biological processes 
       !X1      : tillage control on residue decomposition (Not used)
       !XLSLF   : control on potential transformation of structural litter by lignin fraction
                !of structural litter [XLSLF = exp(-3* LSLF) (Parton et al., 1993, 1994)]
       integer :: j, k, kk, iel, NONLIG, LIG
       
       
       real :: LMF, LSF, LSLF, XLSLF   

       real ::  AMETS1, ASX, APX  !!Intermediate variables used to calcualte N demand
       !AMETS1 is not used
       !AMETS1=1.-ALMCO2  !AMETS1=1.-A1CO2
       !ASX=1.-ASCO2-ASP
       !APX=1.-APCO2
       
       
       !real :: LSCTA, LSLCTA, LSLnCTA,LSNTA, LMCTA, LMNTA, BMCTA, BMNTA, HSCTA, HSNTA, HPCTA, HPNTA
       !real :: LSCTP, LSLCTP, LSLNCTP,LSNTP, LMCTP, LMNTP, BMCTP, BMNTP, HSCTP, HSNTP, HPCTP, HPNTP
       real :: NCHP, Nf, NCBM, NCHS
       real :: DF1, DF2, SNMN,  DF3, DF4, DF5, DF6, ADD, ADF1, ADF2, ADF3, ADF4, ADF5
       real :: TOT
       real :: PN1, PN2, PN3, PN4, PN5, PN6, PN7, PN8, PN9
       real :: SUM, CPN1, CPN2, CPN3, CPN4, CPN5, SUM1, SUM2, SUM3, SUM4, SUM5
       real :: REDUC
       real :: WMIN,DMDN, wdn, RNMN
       real :: t_scale,w_scale, vmc, wfp, fc_fp, satfp !! qichun
       real :: tcalc, wcalc,rwcf  !! qichun
       real :: RUMMY
       
       real:: mint   !! ------------------qichun forestry
       !local temporal variables
       real :: X1,X3, XX
       real :: fc, wc, sat, void, sut,cdg,fcgd, wf,fwf,of,fof  !till_eff !CS,,OX, 
       real :: sol_mass, sol_min_n, rsdN_percent, mineralN_ppm
       real :: csf, XBM, hmp, hmp_rate, rmp, decr, RTO, HMN, RWN, RMN1, rprpet, RLR
       real :: aa,bb
       real :: tilf,ftilf,till_fact
       

       integer:: IMM_MIN = 1    !!  check balance for min/immo N ; 0= check balance; 1= no check balance  			
       real:: wt1
       integer:: first_loop, loop_count

       
       !! initilize local variables
       NONLIG = 1
       LIG = 2
       
       wdn = 0. 
      ! till_eff= 0.  
       X1 = 0.
       X3 = 0.
       XX = 0.
       fc = 0.
       wc = 0.
       sat = 0.
       void = 0.
       !sut = 0.
       !cdg = 0.
       !OX = 0.
       !CS = 0.

       
       !LSCTA = 0.
       !LSLCTA = 0.
       !LSLnCTA = 0.
       !LSNTA = 0.
       !LMCTA = 0.
       !LMNTA = 0.
       !BMCTA = 0.
       !BMNTA = 0.
       !HSCTA = 0.
       !HSNTA= 0. 
       !HPCTA= 0.
       !HPNTA= 0.
       !LSCTP = 0.
       !LSNTP= 0.
       !LSLCTP= 0.
       !LSLnCTP= 0.
       !LMCTP= 0.
       !LMNTP= 0.
       !BMCTP= 0.
       !BMNTP = 0.
       !HSCTP= 0.
       !HSNTP= 0.
       !HPCTP= 0.
       !HPNTP= 0.
       
       !!Initialize variables from modparm_subC-Qi.f90
       LMF = 0.
       LSF = 0.
       LSLF = 0.
       XLSLF = 0.
       
       LMR= 0.
       BMR= 0.
       XBMT= 0.
       HSR= 0.
       HPR= 0.
       

       ALSLCO2= 0.
       ALSLNCO2= 0.
       ALMCO2= 0.
       ABCO2= 0.
       ALMCO2= 0.
       APCO2= 0.
       ASCO2= 0.
       ABP= 0.
       ASP= 0.
       !!Initialize variables from modparm_subC-Qi.f90
       
       
       !!Initialize local variables
       NCHP= 0.
       Nf= 0.
       NCBM= 0.
       NCHS= 0.
       AMETS1= 0.
       ASX= 0.
       APX= 0.
       
       
       !!
       DF1= 0.
       DF2= 0.
       SNMN= 0.
       DF3= 0.
       DF4= 0.
       DF5= 0.
       DF6= 0.
       ADD= 0.
       ADF1= 0.
       ADF2= 0.
       ADF3= 0.
       ADF4= 0.
       ADF5= 0.
       PN1= 0.
       PN2= 0.
       PN3= 0.
       PN4= 0.
       PN5= 0.
       PN6= 0.
       PN7= 0.
       PN8= 0.
       PN9= 0.
       TOT= 0.
       SUM= 0.
       CPN1= 0.
       CPN2= 0.
       CPN3= 0.
       CPN4= 0.
       CPN5= 0.
       WMIN= 0.
       DMDN= 0.
       !!Initialize local variables
        
       j=0
       j=ihru
       
 
      !!calculate C/N dynamics for each soil layer
      !!====================================================================================================================================================
       Do k = 1, sol_nly(j)
       
         
        if(idc_till==3) then
           !! till factor updated daily
           wc=0.
           sat=0.
           wc = sol_st(k,j) + sol_wpmm(k,j)        ! units mm
           sat = sol_ul(k,j) + sol_wpmm(k,j)       ! units mm
           tillagef(k,j) = tillagef(k,j) * (1. - 0.02 * wc/sat) 
           if (tillagef(k,j) < 0.) tillagef(k,j)  = 0.
        end if
        
       
!!         Set all the flows to zero. 
!          CFMETS1(k,j) = 0.
!          CFSTRS1(k,j) = 0.
!          CFSTRS2(k,j) = 0.
!          
!          !MNRMETS1(:,1,j) = 0.              !!NBS/JGA debug
!          !MNRSTRS1(:,1,j) = 0.              !!NBS/JGA debug
!          !MNRSTRS2(:,1,j) = 0.              !!NBS/JGA debug
!          
!          DO IEL = 1, NELEM
!            EFMETS1(k,1,j) = 0.
!            EFSTRS1(k,1,j) = 0.
!            EFSTRS2(k,1,j) = 0. 
!            IMMMETS1(k,1,j) = 0.
!            IMMSTRS1(k,1,j) = 0.
!            IMMSTRS2(k,1,j) = 0. 
!            MNRMETS1(k,1,j) = 0.
!            MNRSTRS1(k,1,j) = 0.
!            MNRSTRS2(k,1,j) = 0.
!          END DO
!          CO2FMET(k,j) = 0.
!          CO2FSTR(k,LIG,j) = 0.
!          CO2FSTR(k,NONLIG,j) = 0.
!
!!         Set all the soil flows to zero.
!          CFS1S2(k,j) = 0.
!          CFS1S3(k,j) = 0.
!          CFS2S1(k,j) = 0.
!          CFS2S3(k,j) = 0.
!          CFS3S1(k,j) = 0.
!          DO IEL = 1, NELEM
!            EFS1S2(k,1,j) = 0.
!            EFS1S3(k,1,j) = 0.
!            EFS2S1(k,1,j) = 0.
!            EFS2S3(k,1,j) = 0.
!            EFS3S1(k,1,j) = 0. 
!            IMMS1S2(k,1,j) = 0.
!            IMMS1S3(k,1,j) = 0.
!            IMMS2S1(k,1,j) = 0.
!            IMMS2S3(k,1,j) = 0.
!            IMMS3S1(k,1,j) = 0.
!            MNRS1S2(k,1,j) = 0.
!            MNRS1S3(k,1,j) = 0.
!            MNRS2S1(k,1,j) = 0.
!            MNRS2S3(k,1,j) = 0.
!            MNRS3S1(k,1,j) = 0.
!          END DO
!          CO2FS1(k,j) = 0.
!          CO2FS2(k,j) = 0.
!          CO2FS3(k,j) = 0.

         !! for the first soil layer---------
          if (k == 1) then
            !10 cm / 1000 = 0.01m; 1 ha = 10000 m2; ton/m3; * 1000 --> final unit is kg/ha; rock fraction is considered
 		    sol_mass = (10) / 1000.* 10000. * sol_bd(k,j)* 1000. *(1- sol_rock(k,j) / 100.)            
          else
		    sol_mass = (sol_z(k,j) - sol_z(k-1,j)) / 1000.* 10000. * sol_bd(k,j)* 1000. *(1- sol_rock(k,j) / 100.)
	      end if
          sol_min_n = sol_no3(k,j) + sol_nh4(k,j)
          
          !!If k = 1, then using temperature, soil moisture in layer 2 to calculate decomposition factor
          !!Not 
	      kk =0 
	      if (k == 1) then
	        kk = 2
	      else
	        kk = k
	      end if	  	
          !! mineralization can occur only if temp above 0 deg
          !check sol_st soil water content in each soil ayer mm H2O
       
    
         if (ifor==1) then     !!!--------qichun  forestry----
           mint = -5.0
         else
           mint=0.0
         end if 
    
   

        if (sol_tmp(k,j) > mint .AND. sol_st(k,j) > 0.) then
         
         !!================================================= Decomp rates===================================================={
         
              !!from Armen
              fc = sol_fc(k,j) + sol_wpmm(k,j)        ! units mm
              wc = sol_st(k,j) + sol_wpmm(k,j)        ! units mm
              sat = sol_ul(k,j) + sol_wpmm(k,j)       ! units mm
              void = sol_por(k,j) * (1. - wc / sat)   ! fraction
              if (void < 0.) then 
                void = 0.
              end if
              !!from Armen
             
             
             !!compute soil water factor - sut
              Select case(idc_sw)
              case(1)
	              IF((wc-sol_wpmm(k,j))<0.)THEN
	                  WATF(k,j)=.1*(sol_st(kk,j) /sol_wpmm(k,j))**2
	              ELSE
	                  WATF(k,j) = .1 + .9 * Sqrt(sol_st(k,j) / sol_fc(k,j))
                  END IF             
                  WATF(k,j) = Min(1., WATF(k,j))
                  WATF(k,j) = Max(.05, WATF(k,j))
              
             
              case(2)
                  !from Armen 
                  wf = 0.
                  of = 0.
                  sut = 0.
                  wf = fwf(fc,wc,sol_wpmm(k,j))
                  of = fof(void,sol_por(k,j))
                  sut = wf * of
                  !from Armen
                  WATF(k,j) = sut
              End select 
              
              
              
             !!compute soil temperature factor
              Select case(idc_tmp)
                  case(1)
                      !! From EPIC
		              if(sol_tmp(k,j)<=35.) then
	                  TEMF(k,j) = sol_tmp(k,j) / (sol_tmp(k,j) + exp(5.058459 - 0.2503591 * sol_tmp(k,j)))
                      else
                      TEMF(k,j) = 1.0 - 0.04*sol_tmp(k,j)
                      end if
               
                  case(2)
                      !!from Armen
                      cdg = 0.
    		          cdg = fcgd(sol_tmp(k,j))
                      !!from Armen
                      TEMF(k,j)= cdg

                  case(3)
                      !!original SWAT                  
                      TEMF(k,j) = 0.
                      TEMF(k,j) = .9 * sol_tmp(k,j) / (sol_tmp(k,j) + exp(9.93 - .312 * sol_tmp(k,j))) + .1
                      TEMF(k,j) = max(.1, TEMF(k,j)) 
              End select
             
             
              !calculate tillage factor
              Select case(idc_till)
            
            
                  case(1)
                    ! from DSSAT   ---- having the least effect on decomp.
		            if (tillage_switch(j) .eq. 1 .and. tillage_days(k,j) .le. 30) then
                       
		                if (k == 1) then
                            TILLF(k,j) = 1.6                  
		                else
		                    if (sol_z(k,j) .le. tillage_depth(j)) then   
		                        TILLF(k,j) = 1.6
                            elseif (sol_z(k-1,j) .le. tillage_depth(j) .and. sol_z(k,j) .ge. tillage_depth(j)) then
		                        TILLF(k,j) = 1.0 + 0.6*(tillage_depth(j) - sol_z(k-1,j))/(sol_z(k,j) - sol_z(k-1,j))
		                    end if		         
		                end if
		            else
		                TILLF(k,j) = 1.0
		            end if
    		     
                  case(2)
                    ! modified EPIC -----having the greatest effect on decomp.
		            if (tillage_switch(j) .eq. 1 .and. tillage_days(k,j) .le. 30) then
		                if (k == 1) then 
                            TILLF(k,j)= exp(6*0.333*sol_bd(k,j)*tillage_emix(j))                   
		                else
		                    if (sol_z(k,j) .le. tillage_depth(j)) then
		                        TILLF(k,j)= exp(6*0.333*sol_bd(k,j)*tillage_emix(j))     
                            elseif (sol_z(k-1,j) .lt. tillage_depth(j) .and. sol_z(k,j) .ge. tillage_depth(j)) then
		                        TILLF(k,j)= 1.0 + (exp(6*0.333*sol_bd(k,j)*tillage_emix(j)) -1.0) * (tillage_depth(j) - sol_z(k-1,j))/(sol_z(k,j) - sol_z(k-1,j))
                                !TILLF(k,j) = 1.0 + 0.6*(tillage_depth(j) - sol_z(k-1,j))/(sol_z(k,j) - sol_z(k-1,j))
		                    end if		         
		                end if
		            else
		                TILLF(k,j) = 1.0
		            end if	
                  
                  case(3)
		            !from Armen    ----having modi
                    TILLF(k,j) = 1. + tillagef(k,j)  
		            !tilf = ftilf(tillagef(k,j), wc, sat)	
                  
                  case(4)
                    !! from DNDC
                    if ( tillage_switch(j) == 1 .and. tillage_days(k,j) == 1) then
                    
                        if( tillage_depth(j) < 0.0) then
                            till_fact = 1.0
				        else if ( tillage_depth(j) > 0.3*1000.)then
				            till_fact = 6.0    !//9.0
				        else if ( tillage_depth(j) > 0.2*1000.)then
				            till_fact = 5.0    !;//7.0
				        else if ( tillage_depth(j) > 0.1*1000.)then
				            till_fact = 4.0    !;//5.0
				        else if ( tillage_depth(j) > 0.05*1000.)then
				            till_fact = 3.0    !;//4.5
				        else 
				            till_fact = 1.0
				        endif
    	
                        if (k == 1) then
                            TILLF(k,j) = till_fact                 
		                else
		                    if (sol_z(k,j) .le. tillage_depth(j)) then   
		                        TILLF(k,j) = till_fact
		                    elseif (sol_z(k-1,j) .lt. tillage_depth(j) .and. sol_z(k,j) .ge. tillage_depth(j)) then
		                        TILLF(k,j) = 1.0 + (till_fact-1.0) * (tillage_depth(j) - sol_z(k-1,j))/(sol_z(k,j) - sol_z(k-1,j))
		                    end if		         
		                end if
                 
                    end if
                   
                    if(precipday > 5.)then
                        TILLF(k,j) = TILLF(k,j)- 0.1
	                else 
	                    TILLF(k,j) = TILLF(k,j)- 0.01
	                end if
    	           
	                if(TILLF(k,j) <= 1.0) then
	                    TILLF(k,j) = 1.0 
	                    tillage_days(k,j) = 31
	                    !tillage_switch(j) = 0
	                end if
              
              End select
 

		   !!compute oxygen (OX)
		   !OXGF(k,j) = 0.
		   !OXGF(k,j) = 1 - (0.9* sol_z(k,j)/1000.) / (sol_z(k,j)/1000.+ exp(1.50-3.99*sol_z(k,j)/1000.))
		   !OXGF(k,j) = 1 - (0.8* sol_z(k,j)) / (sol_z(k,j)+ exp(1.50-3.99*sol_z(k,j))) 
		  
		
		   !if(k==1)then
		   ! OXGF(k,j)=1.-0.8*((sol_z(k,j))/2)/(((sol_z(kk,j))/2)+EXP(18.40961-0.023683632*((sol_z(kk,j))/2))) 
		   ! else 
		   !OXGF(k,j)=1.-0.8*((sol_z(k,j)+sol_z(k-1,j))/2)/(((sol_z(k,j)+sol_z(k-1,j))/2)+EXP(18.40961-0.023683632*((sol_z(k,j)+sol_z(k-1,j))/2))) 
 		   !end if
 		 
 		   !! Calibration for oxygen effect 
 		   aa = OX_aa_para  !10.
 		   bb = OX_bb_para  !0.035        
 		   if(k==1)then
		    OXGF(k,j)=1.-0.8*((sol_z(k,j))/2)/(((sol_z(kk,j))/2)+EXP(aa-bb*((sol_z(kk,j))/2))) 
		   else 
		    OXGF(k,j)=1.-0.8*((sol_z(k,j)+sol_z(k-1,j))/2)/(((sol_z(k,j)+sol_z(k-1,j))/2)+EXP(aa-bb*((sol_z(k,j)+sol_z(k-1,j))/2))) 
 		   end if
 		   
       !!--------------Zhang ---------------------}
       
          !! qichun code from century----------------{
          if (ifor==1) then
          !! temperature effects
              tcalc = 0.  
              tcalc = 0.0 + 0.125 * exp(0.07 * sol_tmp(k,j)) 
          !! water effects
              wcalc = 0.
          !!Check added to handle underflow potential in exp intrinsic
              rwcf = (vmc-sol_wpmm(k,j)) / (fc-sol_wpmm(k,j)) !what is vmc     
                    if (rwcf > 13.) then
	                wcalc = 1.
                    else
                    wcalc = 1./(1. + 4.0 * exp(-6.0*rwcf))
                    endif
          !!Check added to handle underflow potential in exp intrinsic
	              if (rprpet > 9.0) then
	                wcalc = 1.
                  else
                    wcalc = 1./(1. + 30.0 * exp(-8.5 * rprpet))
                 endif
                 if (wcalc > 1.0) wcalc = 1.0
          end if
          !! qichun code from century-----------------}    
             
             
         !!------------original SWAT--------------{ 
         select case(1)
         case(0)
        
            !! compute soil water factor
            WATF(k,j) = 0.
	        !! change for domain error 1/29/09 gsm check with Jeff !!!
	        if (sol_st(kk,j) < 0.) sol_st(kk,j) = .0000001
            WATF(k,j) = .1 + .9 * Sqrt(sol_st(kk,j) / sol_fc(kk,j))
            ! WATF(k,j) = Min(1., WATF(k,j))
            WATF(k,j) = Max(.05, WATF(k,j))

            !!compute soil temperature factor
            xx = 0.
            TEMF(k,j) = 0.
            xx = sol_tmp(kk,j)
            TEMF(k,j) = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
            TEMF(k,j) = Max(.1, TEMF(k,j))

            !! compute combined factor
            xx = 0.
            csf = 0.
            xx = TEMF(k,j) * WATF(k,j)
            if (xx < 0.) xx = 0.
            if (xx > 1.e6) xx = 1.e6
            csf = Sqrt(xx)
          end select 
          !!------------original SWAT---------------}       
   
           !OXGF(k,j)=1.
   
           Select case(1)  
           case(1)
                !! compute combined factor
		        !CS = 0.
		        if (ifor==1) then
		            CMF(k,j)=MIN(10.,SQRT(tcalc *wcalc )*0.9*OXGF(k,j)*TILLF(k,j))        !! -----qichun-forestry-
		        else
		            CMF(k,j)=MIN(10.,SQRT(TEMF(k,j)*WATF(k,j))*OXGF(k,j)*TILLF(k,j))  !! --------zhang--------
		            !CMF(k,j)= csf                                                       !! -------original-SWAT-    
                end if 
       
          case(2)
                !! compute combined factor
		        xx = 0.
                !!		  xx = sqrt(cdg * sut)
		        xx = (TEMF(k,j)*WATF(k,j)) ** cf(j)             !readhru_change.f90(279):     	if (cf(ihru) <= 0.) cf(ihru)= 1.0
		        if (xx < 0.) xx = 0.
		        if (xx > 1.) xx = 1.
		        CMF(k,j)= xx
       
          case(3)
          
                !DO L = SRFC, sol_nly(hru_num)   !With SRFC layer.
                !call calcdefac(k,j)
                !End Do
                !CMF(k,j)= defac(k,j)
          End select
       
       
         !!================================================= Decomp rates===================================================={
       
          
             
             
             
             

    
              !!====================clculating the N:C ration in the newly formed SOM for each pool==================================={
              
              
              !!The following codes are clculating of the N:C ration in the newly formed SOM for each pool
              !!please note that in the surface layer, no new materials enter Passive pool, therefore, no NCHP is 
              !!calculated for the first layer.
              
               !!!------------ First Layer---------{
              PRMT_51 =0.   !COEF ADJUSTS MICROBIAL ACTIVITY FUNCTION IN TOP SOIL LAYER (0.1_1.),
              PRMT_51 = PRMT_51_para !1.
              
              IF(k==1)THEN
                  CMF(k,j)=CMF(k,j)*PRMT_51
                  
                  ABCO2 = ABCO2_para_sur !0.55
                  ALMCO2 = ALMCO2_para_sur  !0.6
                  ALSLNCO2 = ALSLNCO2_para_sur    !0.6 
                  ABP= ABP_para_sur !.003+.00032*sol_clay(k,j)
                  ASP=0.
                  BMR = BMR_para_sur !0.0164
                  LMR = LMR_para_sur    !0.0405
                  LSR = LSR_para_sur    !0.0107
                  NCHP = 0.1
                  XBM = XBM_para_sur    !1.0
            !     COMPUTE N/C RATIOS
                  !X1=.1*(WLMN(LD1)+WLSN(LD1))/(RSD(LD1)+1.E-5)
                  !X1 = 0.1*(sol_LSN(k,j)+sol_LMN(k,j))/(sol_rsd(k,j)/1000+1.E-5) !relative notrogen content in residue (%)
                  rsdN_percent= 0.1*(sol_LSN(k,j)+sol_LMN(k,j))/(sol_rsd(k,j)/1000+1.E-5)
                  IF(rsdN_percent>2.)THEN
                      NCBM=.1
                      GO TO 6
                  END IF
                  IF(rsdN_percent>.01)THEN
                      NCBM=1./(20.05-5.0251*rsdN_percent)
                  ELSE
                      NCBM=.05
                  END IF    
                6 NCHS=NCBM/(5.*NCBM+1.)
              
              ELSE
                  !!!------------- First Layer---------------------} 
                  
                  !!----------------- >1 Layers-------------------{
                  !ABCO2=.17+.0068*SAN(ISL)
                  
                  ABCO2 = 0.17 + 0.0068 * sol_sand(k,j)
                  ALMCO2 = ALMCO2_para_sub  !.55
                  ALSLNCO2 = ALSLNCO2_para_sub  !0.55
                  ABP=.003+.00032*sol_clay(k,j)
                  
                  !SMS(3,ISL)=SMS(3,ISL)+CMF(k,j)
                  !PRMT_45 = 0.  !COEF IN CENTURY EQ ALLOCATING SLOW TO PASSIVE HUMUS(0.001_0.05) ORIGINAL VALUE = 0.003,                  
                  !ASP=MAX(.001,PRMT_45 - .00009*sol_clay(k,j))
                  PRMT_45 = PRMT_45_para    !0.003
                  ASP=MAX(.001,PRMT_45 + .009*sol_clay(k,j)/100.)
                  
                  BMR = BMR_para_sub    !.02
                  LMR = LMR_para_sub    !.0507
                  LSR = LSR_para_sub    !.0132
                  XBM =.25+.75*sol_sand(k,j)/100.  !.25+.0075*sol_sand(k,j)        !!1 – 0.75 × (SILT + CLAY)
                  !X1=1000.*(WNH3(ISL)+WNO3(ISL))/WT(ISL)
                  !WT is soil mass in tons
                  !X1 = 1000. * sol_min_n/(sol_mass/1000) !soil mineral N concentration in PPM
                  mineralN_ppm = 1000. * sol_min_n/(sol_mass/1000.)
                  IF(mineralN_ppm>7.15)THEN
                      NCBM=.33
                      NCHS=.083
                      NCHP=.143       
                  ELSE
                      NCBM=1./(15.-1.678*mineralN_ppm)
                      NCHS=1./(20.-1.119*mineralN_ppm)
                      NCHP=1./(10.-.42*mineralN_ppm)
                  END IF              
                  !!------------------ >1 Layers-------------------}
             END IF
             
             !!====================clculating the N:C ration in the newly formed SOM for each pool===================================}                  
                            

            
                !lignin content in structural litter (fraction)          
              RLR = 0.
              RLR = min(0.8,sol_LSL(k,j)/(sol_LS(k,j) + 1.E-5))  

	          !HSR=PRMT(47) !CENTURY SLOW HUMUS TRANSFORMATION RATE D^- 1(0.00041_0.00068) ORIGINAL VALUE = 0.000548,
	          HSR = HSR_para    !5.4799998E-04
              !HPR=PRMT(48) !CENTURY PASSIVE HUMUS TRANSFORMATION RATE D^-1(0.0000082_0.000015) ORIGINAL VALUE = 0.000012 
              HPR = HPR_para    !1.2000000E-05
              !HPR = 0.0000082
              !HPR = 0.000005
              APCO2 = APCO2_para  !.55        !This parameter is not used for the first layer, k = 1
              ASCO2 = ASCO2_para  !.60
              ALSLCO2 = ALSLCO2_para    !0.3          
            
             !!=========================Potential Transformations==========================================================={
           

        !     POTENTIAL TRANSFORMATIONS STRUCTURAL LITTER
              X1=0.
              X1=LSR*CMF(k,j)*EXP(-3.*RLR)          ! LSR     : rate of potential transformation of structural litter under optimal conditions
                                              ! RLR     : lignin content in structural litter (fraction) 
              LSCTP(k,j)=X1*sol_LSC(k,j)           ! LSCTP(k,j)   : potential transformation of C in structural litter (kg ha-1 day-1)
              LSLCTP(k,j)=LSCTP(k,j)*RLR
              LSLnCTP(k,j)=LSCTP(k,j)*(1.-RLR)
              LSNTP(k,j)=X1*sol_LSN(k,j)
              R_LSCTP(k,j)= X1              

        !     POTENTIAL TRANSFORMATIONS METABOLIC LITTER
              X1=0.
              X1=LMR*CMF(k,j)                                  !LMR     : rate of transformation of metabolic litter under optimal conditions (surface = 
              LMCTP(k,j)=sol_LMC(k,j)*X1
              LMNTP(k,j)=sol_LMN(k,j)*X1
              R_LMCTP(k,j)= X1               
        !     POTENTIAL TRANSFORMATIONS MICROBIAL BIOMASS
              X1=0.
              X1=BMR*CMF(k,j)*XBM
              BMCTP(k,j)=sol_BMC(k,j)*X1
              BMNTP(k,j)=sol_BMN(k,j)*X1
              R_BMCTP(k,j)= X1               
        !     POTENTIAL TRANSFORMATIONS SLOW HUMUS
              X1=0.
              X1=HSR*CMF(k,j)
              HSCTP(k,j)=sol_HSC(k,j)*X1
              HSNTP(k,j)=sol_HSN(k,j)*X1
              R_HSCTP(k,j)= X1               
        !     POTENTIAL TRANSFORMATIONS PASSIVE HUMUS
              X1=0.
              X1=CMF(k,j)*HPR
              HPCTP(k,j)=sol_HPC(k,j)*X1
              HPNTP(k,j)=sol_HPN(k,j)*X1
              R_HPCTP(k,j)= X1               
        !     ESTIMATE N DEMAND
              !A1=1.-A1CO2                   
              ASX=1.-ASCO2-ASP              !!Slow to Passive, Biomass and CO2
              APX=1.-APCO2                  !!Passive to Biomass and CO2


               PN1= 0.
               PN2= 0.
               PN3= 0.
               PN4= 0.
               PN5= 0.
               PN6= 0.
               PN7= 0.
               PN8= 0.
               PN9= 0.
 
              PN1= LSLnCTP(k,j)*(1.0-ALSLNCO2)*NCBM    !LSLnCTP(k,j)*A1*NCBM               !Structural Litter to Biomass
              !Structural (non-lignin) to CO2 and Biomass
              PN2= (1.0-ALSLCO2)*LSLCTP(k,j)*NCHS                  !.7*LSLCTP(k,j)*NCHS               !Structural Litter to Slow
              !Structural (lignin) to CO2 and Slow
              PN3=LMCTP(k,j)*(1.0-ALMCO2)*NCBM       !LMCTP(k,j)*A1*NCBM           !Metabolic Litter to Biomass
              !Metabolic to CO2 and Biomass
              !PN4=BMCTP(k,j)*ABL*NCBM                !Biomass to Leaching (calculated in NCsed_leach)
              if (k == 1) then
                  PN5=0. !BMCTP(k,j)*ABP*NCHP                !Biomass to Passive
                  PN6=BMCTP(k,j)*(1.-ABCO2)*NCHS     !Biomass to Slow    
                  !Biomass to Slow and CO2           
              else
                  PN5=BMCTP(k,j)*ABP*NCHP                !Biomass to Passive
                  PN6=BMCTP(k,j)*(1.-ABP-ABCO2)*NCHS     !Biomass to Slow
                  !Biomass to Passive, Slow and CO2
              end if
              if (k == 1) then
                  PN7=HSCTP(k,j)*(ASX+ASP)*NCBM                !Slow to Biomass
                  PN8=0. !HSCTP(k,j)*ASP*NCHP                !Slow to Passive    
                  !Slow to Biomass and CO2        
              else
                  PN7=HSCTP(k,j)*ASX*NCBM                !Slow to Biomass
                  PN8=HSCTP(k,j)*ASP*NCHP                !Slow to Passive
                  !Slow to Biomass and Passive
              end if              
              

              PN9=HPCTP(k,j)*APX*NCBM                !Passive to Biomass
              !Passive only goes to Biomass and CO2
 
              !PN1=LSLnCTP(k,j)*A1*NCBM
              !PN2=.7*LSLCTP(k,j)*NCHS
              !PN3=LMCTP(k,j)*A1*NCBM
              !PN5=BMCTP(k,j)*ABP*NCHP
              !PN6=BMCTP(k,j)*(1.-ABP-ABCO2)*NCHS
              !PN7=HSCTP(k,j)*ASX*NCBM
              !PN8=HSCTP(k,j)*ASP*NCHP
              !PN9=HPCTP(k,j)*APX*NCBM
        !     COMPARE SUPPLY AND DEMAND FOR N
              SUM=0.
              SUM1=0.
              SUM2=0.
              SUM3=0.
              SUM4=0.
              SUM5=0.
              CPN1=0.
              CPN2=0.
              CPN3=0.
              CPN4=0.
              CPN5=0.
              X1=PN1+PN2
              IF(LSNTP(k,j)<X1)THEN
                  CPN1=X1-LSNTP(k,j)
              ELSE
                  SUM1=LSNTP(k,j)-X1
              END IF
              IF(LMNTP(k,j)<PN3)THEN
                  CPN2=PN3-LMNTP(k,j)
              ELSE
                  SUM2=LMNTP(k,j)-PN3
              END IF
              X1=PN5+PN6
              IF(BMNTP(k,j)<X1)THEN
                  CPN3=X1-BMNTP(k,j)
              ELSE
                  SUM3=BMNTP(k,j)-X1
              END IF      
              X1=PN7+PN8
              IF(HSNTP(k,j)<X1)THEN
                  CPN4=X1-HSNTP(k,j)
              ELSE
                  SUM4=HSNTP(k,j)-X1
              END IF
              IF(HPNTP(k,j)<PN9)THEN
                  CPN5=PN9-HPNTP(k,j)
              ELSE
                  SUM5=HPNTP(k,j)-PN9
              END IF
        !     WNH3(ISL)=WNH3(ISL)+SUM
              !total available N
              SUM = SUM1+SUM2+SUM3+SUM4+SUM5
              WMIN=MAX(1.E-6,sol_no3(k,j) + sol_nh4(k,j)+SUM)
              !WMIN=MAX(1.E-5,sol_no3(k,j) +SUM)
              !total demand for potential tranformaiton of SOM
              DMDN=CPN1+CPN2+CPN3+CPN4+CPN5
              
              REDUC=1.
        !     REDUCE DEMAND IF SUPPLY LIMITS
              IF(WMIN<DMDN) then
                  REDUC=WMIN/DMDN
              end if
              !SMS(5,ISL)=SMS(5,ISL)+X3
        !     ACTUAL TRANSFORMATIONS
        
 
         first_loop = 1
         loop_count = 0
         do while (((WMIN<DMDN) .or. first_loop == 1) .and. loop_count < 10)     
              first_loop = 0
              loop_count = loop_count + 1
              
              IF(CPN1>0.)THEN
                  LSCTA(k,j)=LSCTP(k,j)*REDUC
                  LSNTA(k,j)=LSNTP(k,j)*REDUC
                  LSLCTA(k,j)=LSLCTP(k,j)*REDUC
                  LSLnCTA(k,j)=LSLnCTP(k,j)*REDUC
              ELSE
                  LSCTA(k,j)=LSCTP(k,j)
                  LSNTA(k,j)=LSNTP(k,j)
                  LSLCTA(k,j)=LSLCTP(k,j)
                  LSLnCTA(k,j)=LSLnCTP(k,j)
              END IF
              IF(CPN2>0.)THEN
                  LMCTA(k,j)=LMCTP(k,j)*REDUC
                  LMNTA(k,j)=LMNTP(k,j)*REDUC
              ELSE
                  LMCTA(k,j)=LMCTP(k,j)
                  LMNTA(k,j)=LMNTP(k,j)
              END IF
              IF(CPN3>0.)THEN
                  BMCTA(k,j)=BMCTP(k,j)*REDUC
                  BMNTA(k,j)=BMNTP(k,j)*REDUC
              ELSE
                  BMCTA(k,j)=BMCTP(k,j)
                  BMNTA(k,j)=BMNTP(k,j)
              END IF
              IF(CPN4>0.)THEN
                  HSCTA(k,j)=HSCTP(k,j)*REDUC
                  HSNTA(k,j)=HSNTP(k,j)*REDUC
              ELSE
                  HSCTA(k,j)=HSCTP(k,j)
                  HSNTA(k,j)=HSNTP(k,j)
              END IF
              IF(CPN5>0.)THEN
                  HPCTA(k,j)=HPCTP(k,j)*REDUC
                  HPNTA(k,j)=HPNTP(k,j)*REDUC
              ELSE
                  HPCTA(k,j)=HPCTP(k,j)
                  HPNTA(k,j)=HPNTP(k,j)
              END IF        
             
              !!=========================Potential Transformations===========================================================} 
              
           
              !!=========================Actural Transformations==========================================================={ 
               PN1= 0.
               PN2= 0.
               PN3= 0.
               PN4= 0.
               PN5= 0.
               PN6= 0.
               PN7= 0.
               PN8= 0.
               PN9= 0.              
              !Recalculate demand using actural transformations
              !revised from EPIC code by Zhang 
                  PN1=LSLnCTA(k,j)*(1.0-ALSLNCO2)*NCBM  !LSLnCTA(k,j)*A1*NCBM               !Structural Litter to Biomass
                  !Structural (non-lignin) to CO2 and Biomass
                  PN2= (1.0-ALSLCO2)* LSLCTA(k,j)*NCHS  !.7*LSLCTA(k,j)*NCHS                !Structural Litter to Slow
                  !Structural (lignin) to CO2 and Slow
                  PN3=LMCTA(k,j)*(1.0-ALMCO2)*NCBM      !LMCTA(k,j)*A1*NCBM                 !Metabolic Litter to Biomass
                  !Metabolic to CO2 and Biomass
                  !PN4=BMCTP(k,j)*ABL*NCBM                !Biomass to Leaching (calculated in NCsed_leach)
                  if (k == 1) then
                      PN5=0. !BMCTA(k,j)*ABP*NCHP                !Biomass to Passive
                      PN6=BMCTA(k,j)*(1.-ABCO2)*NCHS     !Biomass to Slow  
                      !Biomass to Slow and CO2
                  else
                      PN5=BMCTA(k,j)*ABP*NCHP                !Biomass to Passive
                      PN6=BMCTA(k,j)*(1.-ABP-ABCO2)*NCHS     !Biomass to Slow      
                      !Biomass to Passive, Slow and CO2            
                  end if

                  if (k == 1) then
                      PN7=HSCTA(k,j)*(ASX+ASP)*NCBM                !Slow to Biomass
                      PN8=0. !HSCTA(k,j)*ASP*NCHP                !Slow to Passive 
                      !Slow to Biomass and CO2 
                  else
                      PN7=HSCTA(k,j)*ASX*NCBM                !Slow to Biomass
                      PN8=HSCTA(k,j)*ASP*NCHP                !Slow to Passive   
                      !Slow to Biomass and Passive               
                  end if
                  
                  PN9=HPCTA(k,j)*APX*NCBM                !Passive to Biomass     
                  !Passive only goes to Biomass and CO2         
          
                !!=========================Actural Transformations===========================================================}  
                  
                !!===============================================================Min/Imo NH4 and NO3================================{  
          
          
          
            !     COMPARE SUPPLY AND DEMAND FOR N
                  SUM=0.
                  SUM1=0.
                  SUM2=0.
                  SUM3=0.
                  SUM4=0.
                  SUM5=0.
                  CPN1=0.
                  CPN2=0.
                  CPN3=0.
                  CPN4=0.
                  CPN5=0.
                  X1=PN1+PN2
                  IF(LSNTA(k,j)<X1)THEN
                      CPN1=X1-LSNTA(k,j)
                  ELSE
                      SUM1=LSNTA(k,j)-X1
                  END IF
                  IF(LMNTA(k,j)<PN3)THEN
                      CPN2=PN3-LMNTA(k,j)
                  ELSE
                      SUM2=LMNTA(k,j)-PN3
                  END IF
                  X1=PN5+PN6
                  IF(BMNTA(k,j)<X1)THEN
                      CPN3=X1-BMNTA(k,j)
                  ELSE
                      SUM3=BMNTA(k,j)-X1
                  END IF      
                  X1=PN7+PN8
                  IF(HSNTA(k,j)<X1)THEN
                      CPN4=X1-HSNTA(k,j)
                  ELSE
                      SUM4=HSNTA(k,j)-X1
                  END IF
                  IF(HPNTA(k,j)<PN9)THEN
                      CPN5=PN9-HPNTA(k,j)
                  ELSE
                      SUM5=HPNTA(k,j)-PN9
                  END IF
            !     WNH3(ISL)=WNH3(ISL)+SUM
                  !total available N
                  SUM = SUM1+SUM2+SUM3+SUM4+SUM5
                  WMIN=MAX(1.E-5,sol_no3(k,j) + sol_nh4(k,j)+ SUM)
                  !WMIN=MAX(1.E-5,sol_no3(k,j) +SUM)
                 !total demand for potential tranformaiton of SOM
                  DMDN=CPN1+CPN2+CPN3+CPN4+CPN5              
                  
                  IF(WMIN<DMDN) then
                      REDUC=WMIN/DMDN*REDUC*0.99 !REDUC=WMIN/DMDN
                  end if
                  
             end do !!do while (WMIN<DMDN)
                              
                
                  !DMDN=DMDN*X3
                  !SGMN=SGMN+SUM
                  !supply - demand
                  RNMN = SUM-DMDN
                  !sol_RNMN(k,j)=SUM-DMDN
        
                  IF (IMM_MIN==1) THEN
                !     UPDATE
                      IF(RNMN>0.)THEN
                          sol_nh4(k,j)=sol_nh4(k,j)+RNMN
                          nh4_min(j) = nh4_min(j)+ RNMN
                          nh4_min_ly(k,j) = nh4_min_ly(k,j)+ RNMN
                !  	      WNO3(ISL)=WNO3(ISL)+RNMN(ISL)
                          !GO TO 21
                      ELSE
                          IF ((sol_nh4(k,j)+RNMN)<0.) then

                              if ((sol_no3(k,j)+sol_nh4(k,j)+RNMN)< 0.) then
                                  immo_err1(j) = immo_err1(j) + (sol_no3(k,j)+sol_nh4(k,j)+RNMN)                                    
                                  no3_immo(j) = no3_immo(j) + sol_no3(k,j)
                                  no3_immo_ly(k,j) = no3_immo_ly(k,j) + sol_no3(k,j)
                                  sol_no3(k,j) = 0.
                              else                                 
                                 no3_immo(j) = no3_immo(j) + (sol_nh4(k,j)+RNMN)*(-1)
                                 no3_immo_ly(k,j) = no3_immo_ly(k,j) + (sol_nh4(k,j)+RNMN)*(-1.0)
                                 sol_no3(k,j) = sol_no3(k,j) + (sol_nh4(k,j)+RNMN)
                              end if                      
                              
                              nh4_immo(j) = nh4_immo(j)+sol_nh4(k,j)
                              nh4_immo_ly(k,j) = nh4_immo_ly(k,j)+sol_nh4(k,j)
                              sol_nh4(k,j) = 0.
                          else
                            sol_nh4(k,j) = sol_nh4(k,j) + RNMN
                            nh4_immo(j) = nh4_immo(j) + RNMN*(-1.0) 
                            nh4_immo_ly(k,j) = nh4_immo_ly(k,j) + RNMN*(-1.0)  
                          end if
                         
 	                      !IF((sol_no3(k,j)+RNMN)<0.)THEN
	                      !    !sol_RNMN(k,j)=-sol_no3(k,j)
	                      !    sol_no3(k,j)=1.E-10
	                      !    no3_immo(j)=no3_immo(j) + RNMN - 1.E-10
	                      !ELSE
	                      !    sol_no3(k,j)=sol_no3(k,j)+RNMN
	                      !    no3_immo(j)=no3_immo(j) + abs(RNMN)
                          !END IF
                      END IF
                    ENDIF
              
            
 
 
	        !!===============================================================Min/Imo NH4 and NO3================================}   
            
            
            !!===============================================================Min SolP================================{    
	         
	         
	          !calculate P flows
              !! compute humus mineralization on active organic p
              hmp = 0.
              hmp_rate = 0.
              hmp_rate = 1.0* (HSNTA(k,j) + HPNTA(k,j))/(sol_HSN(k,j) + sol_HPN(k,j) + 1.e-6)
              !hmp_rate = 1.4* (HSNTA(k,j) )/(sol_HSN(k,j) + sol_HPN(k,j) + 1.e-6)
              hmp = hmp_rate*sol_orgp(k,j)
              hmp = Min(hmp, sol_orgp(k,j))
              sol_orgp(k,j) = sol_orgp(k,j) - hmp
              sol_solp(k,j) = sol_solp(k,j) + hmp	          
	          
	          !! compute residue decomp and mineralization of 
              !! fresh organic n and p (upper two layers only)  
                rmp = 0.             
                decr = 0.
                decr = (LSCTA(k,j) + LMCTA(k,j))/(sol_LSC(k,j) + sol_LMC(k,j) + 1.e-10)
                decr = min(1., decr)
                rmp = decr * sol_fop(k,j)

                sol_fop(k,j) = sol_fop(k,j) - rmp
                sol_solp(k,j) = sol_solp(k,j) + .8 * rmp
                sol_orgp(k,j) = sol_orgp(k,j) + .2 * rmp	          
	          !calculate P flows
       
              solp_min(j) = solp_min(j)+ hmp +.8 * rmp            
              
          !!===============================================================Min SolP================================} 
       
       
       
              !!!================================ final rate ================================================================={
              !!Determine the final rate of the decomposition of each carbon pool and 
              !!allocation of C and Nutrients to different SOM pools, as well as CO2 emissions from different pools
	          LSCTA(k,j) = Min(sol_LSC(k,j),LSCTA(k,j))              
              LSLCTA(k,j) = Min(sol_LSLC(k,j),LSLCTA(k,j))
              
              CO2FSTR(k,LIG,j) = ALSLCO2 * LSLCTA(k,j)       !.3*LSLCTA(k,j)
              CO2FSTR(k,NONLIG,j) = ALSLNCO2 * LSLnCTA(k,j)  ! A1CO2*LSLnCTA(k,j)
              
              CFSTRS1(k,j) = (1.0 - ALSLNCO2) * LSLnCTA(k,j) ! A1*LSLnCTA(k,j)            !A1=1.-A1CO2
              CFSTRS2(k,j) = (1.0 - ALSLCO2) * LSLCTA(k,j)   !.7*LSLCTA(k,j)             !0.7 = 1.-ALSLCO2
              
              LMCTA(k,j) = MIN(sol_LMC(k,j), LMCTA(k,j))
              CO2FMET(k,j) = ALMCO2 * LMCTA(k,j)
              
              !AMS1 = 1.0 - ALMCO2
              CFMETS1(k,j) = (1.0 - ALMCO2)*LMCTA(k,j) !AMS1  *LMCTA(k,j)                !A1  *LMCTA(k,j)
              
              CO2FS1(k,j) = ABCO2 * BMCTA(k,j)
              CO2FS2(k,j) = ASCO2 * HSCTA(k,j)
              CO2FS3(k,j) = APCO2 * HPCTA(k,j)
           
           
            !!!============================== final rate ===================================================================}  
              
              
              
            !!  ===================================Fianl  transformation processes  ==============================================={
              !!Transformation processes from Passive (S3), Slow (S2), Metabolic (Met), and non-lignin structural (STR) pools to microbial pool
              
                      !!S3 (Passive humus) to S1 (Microbial)
                      CFS3S1(k,j) = APX*HPCTA(k,j)              
                      CALL NP_FLOW (  1,       &
                            sol_HPC(k,j), sol_HPN(k,j),              & !Input
                            1./NCBM, CFS3S1(k,j),                     & !Input
                            CO2FS3(k,j),                             & !Input
                            EFS3S1(k,1,j), IMMS3S1(k,1,j),           & !Output
                            MNRS3S1(k,1,j))                            !Output 
                                    
                      !!S2 (Slow humus) to S1 (Microbial)
                      if (k == 1) then
                        CFS2S1(k,j) = (ASX+ASP)*HSCTA(k,j) 
                      else
                        CFS2S1(k,j) = ASX*HSCTA(k,j) 
                      end if                      
                      CALL NP_FLOW ( 1,  &
                            sol_HSC(k,j), sol_HSN(k,j),              & !Input
                            1./NCBM, CFS2S1(k,j),                     & !Input
                            CO2FS2(k,j),                             & !Input
                            EFS2S1(k,1,j), IMMS2S1(k,1,j),           & !Output
                            MNRS2S1(k,1,j))                            !Output   
                                       
                      !!S2 to S3 (Passive humus)
                      if (k == 1) then
                        CFS2S3(k,j) = 0. 
                      else
                        CFS2S3(k,j) = HSCTA(k,j)*ASP
                      end if  
                         
                      CALL NP_FLOW ( 1,   &
                            sol_HSC(k,j), sol_HSN(k,j),        & !Input
                            1./NCHP, CFS2S3(k,j),               & !Input
                            -99.0,                               & !Input  
                            EFS2S3(k,1,j), IMMS2S3(k,1,j),     & !Output
                            MNRS2S3(k,1,j))                      !Output
                            !-99 is used to here to avoid repeated calculation of 
                            !N supply during CO2 emission during the decomposition of microbial biomass
                            ! as this process has been accounted for duing S1 to S2 transormaiton.
                      
                      !!Metabolic litter to S1 (Microbial)
                      CFMETS1(k,j) = (1.0 - ALMCO2)*LMCTA(k,j)  !A1*LMCTA(k,j) 
                      !Metabolic to CO2 and Biomass             
                      CALL NP_FLOW ( 1,   &
                             sol_LMC(k,j), sol_LMN(k,j),        &  !Input
                             1./NCBM, CFMETS1(k,j),              &  !Input
                             CO2FMET(k,j),                      &  !Input
                             EFMETS1(k,1,j), IMMMETS1(k,1,j),   &  !Output
                             MNRMETS1(k,1,j))                      !Output             
                         
                      !!Structural to S1   
                      CFSTRS1(k,j) = (1.0 - ALSLNCO2)*LSLnCTA(k,j) !A1*LSLnCTA(k,j)   
                      !Structural (non-lignin) to CO2 and Biomass     
                      CALL NP_FLOW (  1,  &
                             sol_LSC(k,j), sol_LSN(k,j),        &  !Input
                             1./NCBM, CFSTRS1(k,j),              &  !Input
                             CO2FSTR(k,NONLIG,j),               &  !Input
                             EFSTRS1(k,1,j), IMMSTRS1(k,1,j),   &  !Output
                             MNRSTRS1(k,1,j))                      !Output              
              
              !!!=================================
              !!Transformation processes from lignin structural (STR) and Metabolic (Met) and  pools to S2 (Slow Humus)

                      !!STR (Structrual litter) to S2 (Slow Humus)
                      CFSTRS2(k,j) = (1.0 - ALSLCO2)*LSLCTA(k,j) !.7*LSLCTA(k,j)  
                      !Structural (lignin) to CO2 and Slow            
                      CALL NP_FLOW (  1,  & 
                             sol_LSC(k,j), sol_LSN(k,j),            & !Input
                             1./NCHS, CFSTRS2(k,j),                  & !Input
                             CO2FSTR(k,LIG,j),                      & !Input
                             EFSTRS2(k,1,j), IMMSTRS2(k,1,j),       & !Output
                             MNRSTRS2(k,1,j))                         !Output              
                      
                      !!S1 (Microbial Biomass)to S2 (Slow Humus)
                      if (k == 1) then
                          CFS1S2(k,j) = BMCTA(k,j)*(1.-ABCO2) 
                          !Biomass to Slow and CO2               
                      else
                          CFS1S2(k,j) = BMCTA(k,j)*(1.-ABP-ABCO2) 
                          !Biomass to Passive, Slow and CO2               
                      end if
                      CALL NP_FLOW ( 1,    &
                            sol_BMC(k,j), sol_BMN(k,j),                  & !Input
                            1./NCHS, CFS1S2(k,j),                         & !Input
                            CO2FS1(k,j),                                 & !Input
                            EFS1S2(k,1,j), IMMS1S2(k,1,j),               & !Output
                            MNRS1S2(k,1,j))                                !Output 

              !!!=================================
              !!Transformation processes from lignin structural (STR) and Metabolic (Met) and  pools to S2 (Slow Humus)
                          
                      !!S1 (Microbial Biomass) to S3 (Passive humus)
                      if (k == 1) then
                        CFS1S3(k,j) = 0. !BMCTA(k,j)*ABP 
                        !Biomass to Slow and CO2
                      else
                        CFS1S3(k,j) = BMCTA(k,j)*ABP  !ABP= ABP_para_sur; ABP=.003+.00032*sol_clay(k,j)
                        !Biomass to Passive, Slow and CO2
                      end if
                      CALL NP_FLOW ( 1,    &
                            sol_BMC(k,j), sol_BMN(k,j),        & !Input
                            1./NCHS, CFS1S3(k,j),               & !Input
                            -99.0,                             & !Input  
                            EFS1S3(k,1,j), IMMS1S3(k,1,j),     & !Output
                            MNRS1S3(k,1,j))                      !Output  
                            
                            !-99 is used to here to avoid repeated calculation of 
                            !N supply during CO2 emission during the decomposition of microbial biomass
                            ! as this process has been accounted for duing S1 to S2 transormaiton.
                      
              
              
              !!!=================================
              !!EPIC procedures (Not used): calculating N supply - N demand 
                  !!DF1 is the supply of N during structural litter decomposition (LSNTA) - demand of N to meet the transformaitons of other pools
                  !! C pools into structural litter (0 as no other pools transformed into structural litter)  
	              DF1=LSNTA(k,j) 

                  
                  !!DF2 is the supply of N during metabolic litter decomposition (LSNTA) - demand of N to meet the transformaitons of other pools
                  !! C pools into metabolic litter (0 as no other pools transformed into structural litter)  
                  DF2=LMNTA(k,j)

                  !!!=================================  
                  if (k == 1) then
                    !X3=APX*HPCTA(k,j)+(ASX+ASP)*HSCTA(k,j)+A1*(LMCTA(k,j)+LSLnCTA(k,j))
                    X3=APX*HPCTA(k,j)+(ASX+ASP)*HSCTA(k,j)+(1.0-ALMCO2)*LMCTA(k,j)+ (1.0-ALSLNCO2)*LSLnCTA(k,j)
                  else
                    !X3=APX*HPCTA(k,j)+ASX*HSCTA(k,j)+A1*(LMCTA(k,j)+LSLnCTA(k,j))
                    X3=APX*HPCTA(k,j)+ ASX*HSCTA(k,j)+(1.0-ALMCO2)*LMCTA(k,j)+ (1.0-ALSLNCO2)*LSLnCTA(k,j)
                  end if            
                    
                  !!X3 = Amount of C transformed from Passive, Slow, Metabolic, and non-lignin structural pools to microbial pool              
                  DF3=BMNTA(k,j)-NCBM*X3              
                  !!DF3 is the supply of N during structural litter decomposition (LSNTA(k,j)) - demand of N to meet the transformaitons of Passive, Slow, Metabolic, and Non-lignin Structural 
                  
                  !! C flows into microbiomass pool     
                  sol_BMC(k,j)=sol_BMC(k,j)-BMCTA(k,j)+X3    
                  sol_BMC_In(j)= sol_BMC_In(j) -BMCTA(k,j)+X3   
                  Sol_DOC(k,j)= sol_BMC(k,j) 
                  !!!================================= 
                  if (k == 1) then
                    X1= (1.0-ALSLCO2)*LSLCTA(k,j)+BMCTA(k,j)*(1.-ABCO2)    !.7*LSLCTA(k,j)+BMCTA(k,j)*(1.-ABCO2)
                    !Structural (lignin) to CO2 and Slow
                    !Biomass to Slow and CO2
                  else
                    X1= (1.0-ALSLCO2)*LSLCTA(k,j)+BMCTA(k,j)*(1.-ABP-ABCO2) !.7*LSLCTA(k,j)+BMCTA(k,j)*(1.-ABP-ABCO2)
                    !Structural (lignin) to CO2 and Slow
                    !Biomass to Passive, Slow and CO2
                  end if
                  
                  !!X1 = Amount of C transformed from  lignin structural and Metabolic pools into slow humus             
                  DF4=HSNTA(k,j)-NCHS*X1               
                  !!DF4 is the supply of N during slow humus decomposition (HSNTA(k,j)) - demand of N to meet the transformaitons of lignin structural and Metabolic pools 
                  !! C pools into slow humus      
                  sol_HSC(k,j)=sol_HSC(k,j)-HSCTA(k,j)+X1

                  !!!=================================
                  if (k == 1) then
                    X1=0. !HSCTA(k,j)*ASP+BMCTA(k,j)*ABP
                    !No C flow From Slow and Biomass to Passive.                    
                  else
                    X1=HSCTA(k,j)*ASP+BMCTA(k,j)*ABP
                    !From Slow and Biomass to Passive.
                  end if
                  
                  !!X1 = Amount of C transformed from Slow and Biomass to Passive. !S1 (Microbial Biomass) into S3 (Passive humus)
                  DF5=HPNTA(k,j)-NCHP*X1
                  !!DF5 is the supply of N during Passive humus decomposition (HPNTA(k,j)) - demand of N to meet the transformaitons of Microbial Biomass 
                  !! C pools into Passive humus              
                  sol_HPC(k,j)=sol_HPC(k,j)-HPCTA(k,j)+X1
                  !CO2FS3(k,j) = APCO2*HPCTA(k,j)
                  !CFS2S3(k,j) = HSCTA(k,j)*ASP
                  !CFS1S3(k,j) = BMCTA(k,j)*ABP
                  !CFS3S1(k,j) = APX*HPCTA(k,j)
                                              
                  !!!=================================              
                  DF6=sol_min_n-sol_no3(k,j)-sol_nh4(k,j)
                  !!DF6 Supply of mineral N - available mineral N = N demanded from mineral pool
                  
                  !!!=================================
                  ADD=DF1+DF2+DF3+DF4+DF5+DF6
                  ADF1=abs(DF1)                     !!R669 4/20/18 nbs
                  ADF2=abs(DF2)
                  ADF3=abs(DF3)
                  ADF4=abs(DF4)
                  ADF5=abs(DF5)
                  TOT=ADF1+ADF2+ADF3+ADF4+ADF5
                  XX=ADD/(TOT+1.E-10)
                  !sol_LSN(k,j)=MAX(.001,sol_LSN(k,j)-DF1+XX*ADF1)
                  !sol_LMN(k,j)=MAX(.001,sol_LMN(k,j)-DF2+XX*ADF2)
                  !sol_BMN(k,j)=sol_BMN(k,j)-DF3+XX*ADF3
                  !sol_HSN(k,j)=sol_HSN(k,j)-DF4+XX*ADF4
                  !sol_HPN(k,j)=sol_HPN(k,j)-DF5+XX*ADF5
                  
                  
            !!  ===================================Fianl  transformation processes  ===============================================} 
        
        
            !!=================Update C and N of different SOM pools======================================================{
            
              sol_LSC(k,j)=MAX(1.E-10,sol_LSC(k,j)-LSCTA(k,j))
              sol_LSLC(k,j)=MAX(1.E-10,sol_LSLC(k,j)-LSLCTA(k,j))
              sol_LSLNC(k,j)=MAX(1.E-10,sol_LSLNC(k,j)-LSLnCTA(k,j))
                            
              sol_LSL(k,j)=MAX(1.E-10,sol_LSL(k,j)-LSLCTA(k,j)/CFB) !MAX(1.E-10,sol_LSL(k,j)-LSLCTA(k,j)/.42)
              sol_LS(k,j)=MAX(1.E-10,sol_LS(k,j)-LSCTA(k,j)/CFB) !MAX(1.E-10,sol_LS(k,j)-LSCTA(k,j)/.42)
              
              IF (sol_LM(k,j) > 0.) THEN
                RTO = MAX(CFB,sol_LMC(k,j)/sol_LM(k,j)) !MAX(0.42,sol_LMC(k,j)/sol_LM(k,j))
                sol_LM(k,j) = sol_LM(k,j) - LMCTA(k,j)/RTO
                sol_LMC(k,j) = sol_LMC(k,j) - LMCTA(k,j)
              END IF
              !sol_LMC(k,j)=MAX(1.E-10,sol_LMC(k,j)-LMCTA(k,j))
              !sol_LM(k,j)=MAX(1.E-10,sol_LM(k,j)-LMCTA(k,j)/.42)              
              
              sol_LMN(k,j)=MAX(1.E-10,sol_LMN(k,j) - EFMETS1(k,1,j)& !Subtract N flow from MET (Metabolic Litter) to S1 (microbial biomass)
                            - MNRMETS1(k,1,j))                    !Subtract N immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)

              sol_LSN(k,j)=MAX(1.E-10,sol_LSN(k,j) - EFSTRS1(k,1,j)&!Subtract N flow from STR (Structural Litter) to S1 (microbial biomass)
                            - EFSTRS2(k,1,j)                     &!Subtract N flow from STR (Structural Litter) to S2 (slow humus)
                            - MNRSTRS1(k,1,j)                    &!Subtract mineralization during tansformation from STR (Structural Litter) to S1 (microbial biomass)
                            - MNRSTRS2(k,1,j))                    !Subtract mineralization during tansformation from STR (Structural Litter) to S2 (slow humus)

              sol_BMN(k,j)=sol_BMN(k,j)+ EFMETS1(k,1,j)         & !Add N flow from MET (Metabolic Litter) to S1 (microbial biomass)
                    + EFSTRS1(k,1,j)                            & !Add N flow from STR (Structural Litter) to S1 (microbial biomass)
                    - EFS1S2(k,1,j)                             & !Subtract N flow from S1 (microbial biomass) to S2 (slow humus)
                    - EFS1S3(k,1,j)                             & !Subtract N flow from S1 (microbial biomass) to S3 (Passive Humus)
                    + EFS2S1(k,1,j)                             & !Add N flow from S2 (slow humus) to  S1 (microbial biomass)
                    + EFS3S1(k,1,j)                             & !Add N flow from S3 (Passive Humus) to S1 (microbial biomass)                  
                    - MNRS1S2(k,1,j)                            & !Subtract mineralization during tansformation from S1 (microbial biomass) to S2 (slow humus)
                    - MNRS1S3(k,1,j)                            & !Subtract mineralization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)                   
                    + IMMMETS1(k,1,j)                           & !Add immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
                    + IMMSTRS1(k,1,j)                           & !Add immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
                    + IMMS2S1(k,1,j)                            & !Add immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
                    + IMMS3S1(k,1,j)                              !Add immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                    
              sol_HSN(k,j)=sol_HSN(k,j)+ EFSTRS2(k,1,j) +       & !Add N flow from STR (Structural Litter) to S2 (slow humus)
                    EFS1S2(k,1,j)                               & !Add N flow from S1 (microbial biomass) to S2 (slow humus)
                    - EFS2S1(k,1,j)                             & !Subtract N flow from S2 (slow humus) to  S1 (microbial biomass)
                    - EFS2S3(k,1,j)                             & !Subtract N flow from S2 (slow humus) to  S3 (Passive Humus)                  
                    - MNRS2S1(k,1,j)                            & !Subtract mineralization during tansformation from S2 (slow humus) to  S1 (microbial biomass)
                    - MNRS2S3(k,1,j)                            & !Subtract mineralization during tansformation from S2 (slow humus) to  S3 (Passive Humus)                   
                    + IMMSTRS2(k,1,j)                           & !Add immobilization during tansformation from STR (Structural Litter) to S2 (slow humus)
                    + IMMS1S2(k,1,j)                              !Add immobilization during tansformation from S1 (microbial biomass) to S2 (slow humus)
              
              sol_HPN(k,j)=sol_HPN(k,j)+ EFS1S3(k,1,j) +        & !Add N flow from S1 (microbial biomass) to S3 (Passive Humus)
                    EFS2S3(k,1,j)                               & !Add N flow from S2 (slow humus) to S3 (Passive Humus)                   
                    - EFS3S1(k,1,j)                             & !Subtract N flow from S3 (Passive Humus) to S1 (Microbial Biomass)
                    - MNRS3S1(k,1,j)                            & !Subtract mineralization.
                    + IMMS1S3(k,1,j)                            & !Add immobilization during tansformation from S1 (microbial biomass) to S3 (Passive Humus)
                    + IMMS2S3(k,1,j)                              !Add immobilization during tansformation from S2 (slow humus) to S3 (Passive Humus)
           
            !!=================Update C and N of different SOM pools======================================================}  
             
             
             
              
            !!=================Update soil respiration=================================================================={
              !!soil RSPC for layer k
              if (k == 1) then
                !sol_RSPC(k,j)=.3*LSLCTA(k,j)+ALSLNCO2*LSLnCTA(k,j)+ALMCO2*LMCTA(k,j)+ABCO2*BMCTA(k,j)+ASCO2*HSCTA(k,j)+APCO2*HPCTA(k,j)                
                sol_RSPC(k,j)=ALSLCO2*LSLCTA(k,j)+ALSLNCO2*LSLnCTA(k,j)+ALMCO2*LMCTA(k,j)+ABCO2*BMCTA(k,j)+ASCO2*HSCTA(k,j)!+APCO2*HPCTA(k,j)
                !for layer 1, HPCTA is 0.
              else
                !sol_RSPC(k,j)=.3*LSLCTA(k,j)+ALSLNCO2*LSLnCTA(k,j) + ALMCO2*LMCTA(k,j)+ABCO2*BMCTA(k,j)+ASCO2*HSCTA(k,j)+APCO2*HPCTA(k,j)
                sol_RSPC(k,j)=ALSLCO2*LSLCTA(k,j)+ALSLNCO2*LSLnCTA(k,j)+ALMCO2*LMCTA(k,j)+ABCO2*BMCTA(k,j)+ASCO2*HSCTA(k,j)+APCO2*HPCTA(k,j)
              end if
              
              !!rspc_da is accounting variable summarizing CO2 emissions from all soil layers
              !rspc_d(j) = rspc_d(j) +  sol_RSPC(k,j) 
              rspc_d(j) = rspc_d(j) +  sol_RSPC(k,j)     
            !!=================Update soil respiration==================================================================}  
              
              
        
        !The following code was commented out by setting IMM_MIN = 1. Not used now.
          IF (IMM_MIN==0) THEN
         
                sol_nh4(k,j) = sol_nh4(k,j)+MNRMETS1(k,1,j)+MNRSTRS1(k,1,j)+MNRSTRS2(k,1,j)+MNRS1S2(k,1,j)+MNRS1S3(k,1,j)+MNRS2S1(k,1,j)+MNRS2S3(k,1,j)+MNRS3S1(k,1,j)
                nh4_min(j) = nh4_min(j)+MNRMETS1(k,1,j)+MNRSTRS1(k,1,j)+MNRSTRS2(k,1,j)+MNRS1S2(k,1,j)+MNRS1S3(k,1,j)+MNRS2S1(k,1,j)+MNRS2S3(k,1,j)+MNRS3S1(k,1,j)
	            sol_no3(k,j) = sol_no3(k,j) - (IMMMETS1(k,1,j)+IMMSTRS1(k,1,j)+IMMSTRS2(k,1,j)+IMMS1S2(k,1,j)+IMMS1S3(k,1,j)+IMMS2S1(k,1,j)+IMMS2S3(k,1,j)+IMMS3S1(k,1,j))
	            !no3_immo(j) = no3_immo(j) + IMMMETS1(k,1,j)+IMMSTRS1(k,1,j)+IMMSTRS2(k,1,j)+IMMS1S2(k,1,j)+IMMS1S3(k,1,j)+IMMS2S1(k,1,j)+IMMS2S3(k,1,j)+IMMS3S1(k,1,j)
         
                IF (sol_no3(k,j) <0.) THEN    
                    sol_BMN(k,j)=sol_BMN(k,j) + sol_no3(k,j) 
                    IMMMETS1(k,1,j)=  IMMMETS1(k,1,j)  + sol_no3(k,j)                          !Add immobilization during transformaiton from MET (Metabolic Litter) to S1 (microbial biomass)
                    sol_no3(k,j) = 0.     
                ENDIF
          
                if (IMMMETS1(k,1,j) <0.) then
                    IMMSTRS1(k,1,j)   =   IMMSTRS1(k,1,j)  + IMMMETS1(k,1,j)                      !Add immobilization during transformaiton from STR (Structural Litter) to S1 (microbial biomass)
                    IMMMETS1(k,1,j) = 0. 
                end if    
                  
                if (IMMSTRS1(k,1,j) <0.)  then
                   IMMS2S1(k,1,j)  =  IMMS2S1(k,1,j) +  IMMSTRS1(k,1,j)                              !Add immobilization during transformaiton from S2 (slow humus) to  S1 (microbial biomass)
                   IMMSTRS1(k,1,j) = 0.  
                End if 
            
                if (IMMS2S1(k,1,j)  <0.) then
                   IMMS3S1(k,1,j)  = IMMS3S1(k,1,j)   +    IMMS2S1(k,1,j)                              !Add immobilization during transformaiton from S3 (Passive Humus) to S1 (microbial biomass)
                   IMMS2S1(k,1,j)  = 0. 
                End if
             
                if(IMMS3S1(k,1,j) <0. ) then
     	              sol_BMN(k,j) = sol_BMN(k,j) - IMMS3S1(k,1,j)  
     	              sol_HSN(k,j) =  sol_HSN(k,j) + IMMS3S1(k,1,j)
     	              IMMS1S2(k,1,j) = IMMS1S2(k,1,j) + IMMS3S1(k,1,j) 
                      IMMS3S1(k,1,j) = 0.
                endif
            
                if(IMMS1S2(k,1,j) < 0.) then
                    IMMSTRS2(k,1,j) = IMMSTRS2(k,1,j)  +IMMS1S2(k,1,j) 
                    IMMS1S2(k,1,j)=0.   
                end if
   
                if(IMMSTRS2(k,1,j) < 0.) then
                     sol_HSN(k,j) =  sol_HSN(k,j) - IMMSTRS2(k,1,j)
                     sol_HPN(k,j) =  sol_HPN(k,j) + IMMSTRS2(k,1,j)
                     IMMS1S3(k,1,j)  = IMMS1S3(k,1,j)  + IMMSTRS2(k,1,j)
                     IMMSTRS2(k,1,j) = 0.   
                end if
          
                if(IMMS1S3(k,1,j)  < 0.) then
                     IMMS2S3(k,1,j)  =  IMMS2S3(k,1,j)   + IMMS1S3(k,1,j) 
                     IMMS1S3(k,1,j)  = 0.   
                end if

                if(IMMS2S3(k,1,j)    < 0.) then
                    IMMO_ERR(j) = IMMO_ERR(j)+ abs(IMMS2S3(k,1,j))
                    IMMS2S3(k,1,j)   = 0.   
                end if
             
             
                !  if( IMMS1S2(k,1,j) < 0.) then
                !  print*,i,j,k, IMMS1S2(k,1,j)
                !  end if 
   
                no3_immo(j) = no3_immo(j) + IMMMETS1(k,1,j) + IMMSTRS1(k,1,j) + IMMSTRS2(k,1,j) + IMMS1S2(k,1,j) + IMMS1S3(k,1,j) + IMMS2S1(k,1,j) + IMMS2S3(k,1,j) + IMMS3S1(k,1,j)
         
         ENDIF
      !! ///////////////////////////////////////////////////////////////////////////////////////////////////////}
              
              
              
              !!=========================Update other vairables used in SWAT================================================{
              sol_rsd(k,j)= sol_LS(k,j)+sol_LM(k,j)            
              sol_orgn(k,j) = sol_HPN(k,j)   !!    sol_orgn(:,:)    |kg N/ha       |amount of nitrogen stored in the stable organic N pool                               
              sol_aorgn(k,j) = sol_HSN(k,j)   ! sol_aorgn(:,:)|kg N/ha     |amount of nitrogen stored in the active organic (humic) nitrogen pool in soil layer                        
              sol_fon(k,j) = sol_LMN(k,j) + sol_LSN(k,j)     !!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
              !sol_cbn(k,j) = 100*(sol_LSC(k,j)+sol_LMC(k,j) +sol_HSC(k,j) + sol_HPC(k,j) + sol_BMC(k,j))/sol_mass   
              sol_cbn(k,j) = 100*(sol_HSC(k,j) + sol_HPC(k,j) + sol_BMC(k,j))/sol_mass   !!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
              !SMM(74,MO)=SMM(74,MO)+RSPC(ISL)
              !SMS(8,ISL)=SMS(8,ISL)+RSPC(ISL)
              !TRSP=TRSP+RSPC(ISL)      
              !VAR(74)=VAR(74)+RSPC(ISL)
              !RSD(ISL)=.001*(WLS(ISL)+WLM(ISL)) 

              wt1 = 0. 
              wt1 = sol_bd(k,j) * sol_thick(k,j) / 100.  
              sol_hum(k,j) = sol_cbn(k,j) * wt1 * 17200.   !|kg humus/ha    
              !!===========================Update other vairables used in SWAT================================================}

          !! ===============================================Summary Calculations==========================================={
          !! calculations are based on century model, and not alighned with SWAT old algorithm yet. 
          if (curyr > nyskip) then
            hmn = 0.
            hmn = sol_RNMN(k,j)
            wshd_hmn = wshd_hmn + hmn * hru_dafr(j)
            rwn = 0.
            rwn = HSNTA(k,j)
            wshd_rwn = wshd_rwn + rwn * hru_dafr(j)
            
            wshd_hmp = wshd_hmp + hmp * hru_dafr(j)
            rmn1 = 0.
            rmn1 = (LSNTA(k,j)+LMNTA(k,j))
            wshd_rmn = wshd_rmn + rmn1 * hru_dafr(j)
            wshd_rmp = wshd_rmp + rmp * hru_dafr(j)
           ! wshd_dnit = wshd_dnit + wdn * hru_dafr(j)   
            hmntl = hmntl + hmn
            rwntl = rwntl + rwn
            hmptl = hmptl + hmp
            rmn2tl = rmn2tl + rmn1
            rmptl = rmptl + rmp
           ! wdntl = wdntl + wdn   
          end if
           !! ===============================================Summary Calculations===========================================}
              	  
        end if                       !!        if (sol_tmp(k,j) > mint .AND. sol_st(k,j) > 0.) then
    !!-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    end do                   !!     do k = 1, sol_nly(j)
!!==============================================================================================================================================

    return
    end

