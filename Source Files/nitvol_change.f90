      subroutine nitvol_new

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine 
!!    determine NH4 adsorption, NH3  volatilization, HN4 nitrification, N2O and NO emission from nitrification for each soil layer 

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
      real :: sw25, swwp, swf, xx, dmidl, dpf, akn, akv, rnv, rnit, rvol
      real :: tf 
      real :: cecf = 0.15 
      real :: turnovfrac  !!, N2Oadjust_wp, N2Oadjust_fc   
      real :: dDO_wp, dDO_fc
      real :: swcfrac_fc
      real :: swcfrac_wp
      real :: swcfrac_dDO
      real :: wfpslyr_fc
      real :: wfpslyr_wp
      real :: wfpslyr_dDO
      real :: Kmax, Fd_wfps
      real :: xx1,xx2,xx3,a, b, c, d
      
      real :: porespace   
      real :: porespace_dDO        
      real :: WFPS_fc
      real :: WFPS_wp
      real :: WFPS_dDO 
      real :: dDO
      real :: rnit_to_n2o   
      real :: tem_no
      real :: krain_no  !! rain impacts on n2o emission
      real :: canopy_red
      real :: no_adsorb  
      real :: rel_wc
      real :: avgstemp
      real :: fNwfps
      real :: fNsoilt
      real :: fNph 
      real :: fNnh4
      real :: abiotic
      real :: absoluteMaxRate
      real :: grams_soil
      real :: nh4_conc 
      real :: swclimit
      real :: deltamin
      real :: maxt
      real :: base_flux 
      !real :: MaxRate !! faction of ammonia that is nitrified = 0.15;  
      
      real :: diffusiv  
      integer :: mon
      real :: totnh4
      real :: f_gen_poisson_density   
      real :: rnit1 
      real :: WFPS , Fsw
      real :: pi, sw, aa, bb, cc,dd, Fw, Ft, Fph ,fc
      real ::   Fclay, Fdepth
      real :: wd1,Kw,V_water,generate_H,hydrogen,hydroxide,mol_rnit 
      real:: soil_T
       pi =3.1415926
   
       Fsw=0.
      !N2Oadjust_wp = 0.002  
      !N2Oadjust_fc = 0.015
      dDO_wp = 0.
      dDO_fc = 0. 
      swcfrac_fc = 0.
      swcfrac_wp = 0.
      swcfrac_dDO = 0.
      porespace = 0.   
      porespace_dDO = 0.        
      WFPS_fc = 0.
      WFPS_wp = 0.
      WFPS_dDO = 0.          
      dDO_fc = 0.
      dDO_wp = 0.
      dDO = 0.
      rnit_to_n2o = 0.   
      tem_no = 0.
      krain_no  = 0.
      canopy_red = 0.
      no_adsorb = 0.
      rel_wc = 0.
      avgstemp = 0.
      fNwfps = 0.
      fNsoilt = 0.
      fNph  = 0.
      fNnh4 = 0.
      abiotic = 0.
      absoluteMaxRate = 0.
      grams_soil = 0.
      nh4_conc = 0. 
      swclimit = 0.
      deltamin = 0.
      maxt = 0.
      base_flux = 0.
      !MaxRate  = 0. !! faction of ammonia that is nitrified = 0.15; 
      
      mon  = 0
      totnh4 = 0.    

      j = 0
      j = ihru 
     
      do k = 1, sol_nly(j)     !! determine NH4 adsorption, NH3  volatilization, HN4 nitrification, N2O and NO emission from nitrification for each soil layer 
         
          
      
          if (k == 1) then     !! used for calculation of mid point depth of a soil layer
            xx = 0.
          else
            xx = 0.
            xx = sol_z(k-1,j)
          endif
          
 
          !! calculate water filled pore space

          WFPS = 0.
          WFPS =(  (sol_st(k,j)+sol_wpmm(k,j)) / (sol_ul(k,j) + sol_wpmm(k,j) ) )   !*sol_por(k,j) 
      !  if (sol_nh4(k,j) > 0. .and. tf >= 0.001) then
          if (sol_nh4(k,j) > 0. .and. sol_tmp(k,j) >=-5 ) then         !! when soil temperature >=-5 , volatilization, nitrification, N2O and NO emissions
       
       
          !!volatilization------------------------------------
          dmidl = 0.
          dpf = 0.
          akv = 0.
          rvol = 0.
          tf = 0.
          tf_nit = 0.41     !Temperature factor in controlling nitrification
          tf = tf_nit * (sol_tmp(k,j) - 5.) / 10.               !!  temperature factor
          dmidl = (sol_z(k,j) + xx) / 2.    
          dpf = 1. - dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))             !! depth factor 
          akv = tf * dpf * cecf                              !!cecf  =0.15 |volatilization CEC factor
          rvol = 1. - Exp(-akv)                              !!      [0,1)      
          rvol= sol_nh4(k,j)*rvol              !kg N/ha   |amount of nitrogen lost from the NH3 pool due|to volatilization  
          
    
	    !! apply septic algorithm only to active septic systems          
          if (k/=i_sep(j).or.isep_opt(j)/= 1) then  ! J.Jeong for septic, biozone layer
               
            
       !! calculate nitrification (NH3 => NO3)
          rnit =0.
          select case (nit_method)
         
          case (0)   !! original nitrification 
          akn=0.
          sw25 = 0.
          swwp = 0.
          sw25 = sol_wpmm(k,j) + 0.25 * sol_fc(k,j)
          swwp = sol_wpmm(k,j) + sol_st(k,j)
          if (swwp < sw25) then     !! water factor
          swf = 0.
          swf = (swwp - sol_wpmm(k,j)) /(sw25 - sol_wpmm(k,j))
          else
          swf = 1.
          endif     
          akn = tf * swf    
          rnit = sol_nh4(k,j)*(1. - Exp(-akn))               ! kgN/ha day
         
          case(1)      !! Daycent by Qichun Yang
       
          grams_soil = sol_bd(k,j)* sol_thick(k,j)/1000. * 1000000. !! g/soil /m2
          nh4_conc =  sol_nh4(k,j) / 10. / grams_soil* 1000000. !! ppm nh4
          !MaxRate  = 0.15  ! original is 0.15 !! faction of ammonia that is nitrified = 0.15;
      
          !! calculate water factor
          deltamin = 0.042  !! 0.042   in daycent this value ranges from 0.012 to 0.062
          swclimit = sol_wpmm(k,j)/sol_thick(k,j) - deltamin   
      
          if (swclimit < 0.) swclimit = 0.
          rel_wc = ((sol_st(k, j)+ sol_wpmm(k,j)) / sol_thick(k,j) - swclimit) / ((sol_fc(k, j)+sol_wpmm(k,j)) / sol_thick(k,j) -swclimit)  
      
          if (rel_wc < 0.) rel_wc = 0.
          if (rel_wc > 1.) rel_wc = 1.
       
          fNwfps = 1.0/(1.0 + 30.0 * exp(-9.0 * rel_wc))
       
         !! calculate temperature factor  
          do mon = 1,12
          if (tmpmx(mon,1) > maxt) maxt = tmpmx(mon,1)
          end do 
      
          if (maxt .ge. 35.0) then
          fNsoilt = f_gen_poisson_density(sol_tmp(k,j), maxt)       
          else  
          fNsoilt = f_gen_poisson_density(sol_tmp(k,j) + (35. - maxt), maxt)        
          endif
      
          !! calculate soil pH factor      
          fNph = 0.56 + (1./ 3.14159) * atan(3.1415926 * 0.45 * (sol_ph(k,j) - 5))
 
          !! calculate ammonia factor     
          fNnh4 = 1.0 - exp(-0.0105 * nh4_conc)
      
         !! calculate nitrification rate
          base_flux = 0.1/10000.0 !! origianl is 0.1
          abiotic = max(fNwfps * fNsoilt, 0.03)
          absoluteMaxRate = min(0.4*(sol_nh4(k,j)/totnh4), sol_nh4(k,j)*0.1 * MaxRate(j)) !! here is g N /m2   
          rnit = 10. * (absoluteMaxRate * fNph * abiotic  +  base_flux ) !!change unit to kg/ha
          rnit = Min(rnit, sol_nh4(k,j))   !!  kgN/ha day
        
        case (2)   !! 0l nitrification 
        rnit=0.
        rnit1=0.
        
        case(3)  !! considering pH
          akn=0.
          sw25 = 0.
          swwp = 0.
          sw25 = sol_wpmm(k,j) + 0.25 * sol_fc(k,j)
          swwp = sol_wpmm(k,j) + sol_st(k,j)
          if (swwp < sw25) then     !! water factor
          swf = 0.
          swf = (swwp - sol_wpmm(k,j)) /(sw25 - sol_wpmm(k,j))
          else
          swf = 1.
          endif     
          akn = tf * swf    
          rnit = sol_nh4(k,j)*(1. - Exp(-akn))               ! kgN/ha day
          
          Fph = 0.56 + (atan(pi*0.45*(-5.+ sol_ph(k,j)))) / pi   
          Fph = min(1.,Fph)
          rnit = rnit*Fph
          
          
        case(4)  !! --------Century-----Parton 1996-& 2001---------------
          xx1 = 0.
          xx2 = 0.
          xx3 = 0.
          Kmax = 0.10 !! the maximum faction of Nh4 nitrified Kmax=0.1 /day  Parton 2001
          
          if(sol_tex(k,j)==1 .or. sol_tex(k,j)==2)then     !sand
          a = 0.55
          b = 1.70
          c = -0.007
          d = 3.22
          else                                             !Others clay & loam
          a = 0.60
          b = 1.27
          c = 0.0012
          d = 2.84
          end if 
         
          xx1 = (WFPS-b)/(a-b)
          xx2 = d * (b-a)/(a-c)
          xx3 = (WFPS-c)/(a-c)
     
          Fd_wfps= (xx1**xx2)*xx3 !! calculate soil moisture factor Parton 1996
          Fd_wfps = min(1.,Fd_wfps)  

          Ft = -0.06 + 0.13 * exp( 0.07*sol_tmp(k,j) ) !! calculate soil temperature factor Parton 1996
          Ft = min(1.,Ft)
          
          Fph = 0.56 + (atan(pi*0.45*(-5.+ sol_ph(k,j)))) / pi   !! calculate soil temperature factor
          Fph = min(1.,Fph)          
          
          
          rnit = Kmax * sol_nh4(k,j) * Fd_wfps * Ft * Fph ! soil nitrificaiton rate kg N /ha
          
          
        
        end select 
        
        
        
        !! Adjust nitrification and volatilization   
         if (rvol < 0.) rvol = 0.                            !! kgN/ha day
         if (rnit < 0.) rnit = 0.                            !! kgN/ha day
         if (sol_nh4(k,j) > 1.0E-6) then     
            if ( rvol + rnit > sol_nh4(k,j) ) then
               rvol = sol_nh4(k,j) * rvol / (rvol + rnit)    !! kgN/ha day
               rnit = sol_nh4(k,j) - rvol                    !! kgN/ha day
            end if 
         else 
              rvol = 0.      !!  kgN/ha day
              rnit = 0.      !!  kgN/ha day
         end if  
         sol_nh4(k,j) = sol_nh4(k,j) - rnit- rvol         !! kgN/ha
         no3_nitr(j) = no3_nitr(j) +  rnit                 !! kgN/ha
         rnit1=0.                                         !! kgN/ha
         rnit1= rnit                                      !! kgN/ha
     
         nh4_vol(j) = nh4_vol(j) + rvol                   !! kgN/ha
      
       
       !!  Nitrification effect on pH based on DNDC
       select case(0) 
        case(1)
        if(rnit>0.) then
         
            if(sol_tmp(k,j)<0.0)then 
                soil_T = 0.0
            else 
                soil_T = sol_tmp(k,j)
            end if
         
            wd1 = 10.0**( -15.0)
	        Kw = 1.945 * exp(0.0645 * soil_T) * wd1   !water dissociation constant  
	        hydrogen = 10.0** (-sol_ph(k,j))          !mol H/L
	        hydroxide = Kw / hydrogen                 !mol OH/L
	        !! mol concentration
	        V_water = (sol_st(k,j)+sol_wpmm(k,j))/1000.*10000.*1000. !!L water in soil layer
	        mol_rnit = rnit * 1000.0 / 14.0 / V_water      !! kgN/ha->mol N/L
	        !! generated H during nitrifaction ; mol H/L
	        generate_H = 2. * mol_rnit    
	        !! New H and OH concentration
	        hydrogen = hydrogen + generate_H   !mol H/L
	        hydroxide = Kw / hydrogen          !mol OH/L
	 
	        sol_ph(k,j) = log(hydrogen) /-1.  !(-2.3026)
	        sol_ph(k,j) = max(3.0, min(11.0, sol_ph(k,j)))
        
        end if 
       end select
      
        !! N2O emission from nitrified N        
        if (rnit > 1.0E-6) then    
        
   
          select case (nit_n2o_method)
          
          case(1)        !! Qichun Yang 


                swcfrac_fc = (sol_fc(k, j)+sol_wpmm(k,j)) / sol_thick(k,j)
                swcfrac_wp = sol_wpmm(k,j)/ sol_thick(k,j)
                swcfrac_dDO = (sol_fc(2, j)+sol_wpmm(2,j)) / (sol_z(2,j) - sol_z(1,j))
                porespace = 1.0 - sol_bd(k,j)/2.56      
                porespace_dDO = 1.0 - sol_bd(2,j)/2.56           
                wfpslyr_fc = swcfrac_fc/porespace
                wfpslyr_wp = swcfrac_wp/porespace
                wfpslyr_dDO = (sol_st(2, j)+sol_wpmm(2,j)) / (sol_z(2,j) - sol_z(1,j)) /porespace_dDO
                   
                dDO_fc = diffusiv(swcfrac_fc, sol_bd(k,j), wfpslyr_fc)
                dDO_wp =  diffusiv(swcfrac_wp, sol_bd(k,j), wfpslyr_wp)
                dDO =  diffusiv(swcfrac_dDO, sol_bd(2,j), wfpslyr_dDO)

         
                !WFPS_fc =(  (sol_fc(k, j)+sol_wpmm(k,j)) / (sol_ul(k,j) + sol_wpmm(k,j) ) ) 
   
                !WFPS_wp =(  (sol_wpmm(k,j)) / (sol_ul(k,j) + sol_wpmm(k,j) ) )   
   
                !WFPS_dDO =(  (sol_fc(1, j)+sol_wpmm(1,j)) / (sol_ul(1,j) + sol_wpmm(1,j) ) )
                
                !dDO_fc = diffusiv(swcfrac_fc, sol_bd(k,j), WFPS_fc)
                !dDO_wp =  diffusiv(swcfrac_wp, sol_bd(k,j), WFPS_wp)
                !dDO =  diffusiv(swcfrac_fc_dDO, sol_bd(1,j), WFPS_dDO)
                
                turnovfrac = (N2Oadjust_wp(j) - N2Oadjust_fc(j)) / (dDO_wp - dDO_fc) *  (dDO - dDO_wp) + N2Oadjust_wp(j)
                turnovfrac = max(turnovfrac, N2Oadjust_wp(j))
                turnovfrac = min(turnovfrac, N2Oadjust_fc(j))
                rnit_to_n2o = rnit * turnovfrac       !! kg/ha
                
          case(2)    !!   DAYCENT model
          
                turnovfrac=0.02 
                rnit_to_n2o = rnit * turnovfrac       !! kg/ha
           
          case(3)    !!   NOE model
               
                Fsw = (0.4*WFPS-1.04) / (WFPS-1.04)
                 rnit_to_n2o = rnit * Fsw * 0.0016             !! kg/ha
         
          case(4)   !! WNMM model 
                Fw = 0.
                Ft = 0.
                sw25=0.
                sw=0
                fc=0.
                Ft = 0.9 *( sol_tmp(k,j) / (sol_tmp(k,j) + exp(9.93-0.312*sol_tmp(k,j))) )+0.1
                sw25= sol_wp(k,j)+0.25*(sol_fc(k,j)/sol_thick(k,j))
                sw= (sol_wpmm(k,j) + sol_st(k,j) ) /sol_thick(k,j)
                fc=sol_wp(k,j)+sol_fc(k,j)/sol_thick(k,j)
                if ( sw < sw25) then
                    Fw =(sol_st(k,j)/sol_thick(k,j)) / (sw25-sol_wp(k,j))
                else if  (sw25 <=sw .and. sw <=fc ) then 
                    Fw =  1.
                else if ( sw   >    fc ) then        
                    Fw =    1- ( sw - fc) / (sol_por(k,j)-fc)
                end if 
                turnovfrac = 0.002
                rnit_to_n2o = rnit * turnovfrac * Fw * Ft         !! kg/ha
      
          case (5)   !! FASSET model 
                 Fw = 0.
                Ft = 0.
                aa=0.
                aa=exp( -0.5*((sol_tmp(k,j)-2*17.1)/17.1)**2 )
                Ft = min(1., aa )
                Fw =WFPS
                turnovfrac=0.047
                rnit_to_n2o = rnit * turnovfrac * Fw * Ft   !! potential N2O production     !! kg/ha
                Ft = 0.
                Fsw = 0.
                Fclay = 0.
                Fdepth = 0.
                aa=0.
                bb=0.
                cc=0.
                dd=0
                Ft = 1./ (1.+ exp(-0.64 + 0.08*sol_tmp(k,j)) )
                aa= min(1., 0.0116 + 1.36 / (1+exp(- (WFPS-0.815)/0.0896) ) )   
                Fsw = max(0.,  aa)
                bb= min( 1., 1.26*exp(-0.0116*sol_clay(k,j)-0.249)  )
                 Fclay = max(0., bb)
                cc = ((sol_z(k,j) + xx) / 2.) / 1000.     !!soil depth  m 
                dd=min ( 1., 1.0008-0.0343*cc-3.186*cc**2  )                      
                Fdepth = max(0., dd )
                rnit_to_n2o =  rnit_to_n2o*Ft * Fclay * Fdepth * (1 - Fsw)  !! acturalN2O production     !! kg/ha
 
          case (6)     !!   Wagena et al 2017
                sw=0.
                aa=0.
                Fw=0.
                Ft=0.
                Fph=0.
                sw = sol_wpmm(k,j) + sol_st(k,j) !! mm water
                aa = 0.25 * (sol_fc(k,j)+sol_wpmm(k,j)) -0.75*sol_wpmm(k,j)
          
                if (sw < aa)   then
                    Fw = sol_st(k,j) / 0.25*sol_fc(k,j)
                else
                    Fw = 1.
                end if
                Fw = min(1.,Fw)
          
          
                Ft = -0.06 + 0.13 * exp( 0.07*sol_tmp(k,j) )
                Ft = min(1.,Ft)
          
                Fph = 0.56 + (atan(pi*0.45*(-5.+ sol_ph(k,j)))) / pi   
                Fph = min(1.,Fph)
          
                !turnovfrac = 0.02 !! original 
                turnovfrac = 0.0006 !! calibration 
                rnit_to_n2o = rnit * turnovfrac * Fw * Ft * Fph       !! in Wagena -2017 rnit is calculated with the original nitrification algorithm in SWAT    !! kg/ha
      
          case (7)     !!   CLM4.5    used in Fu et al 2018     also used in DNDC 0.0006
                turnovfrac = 0.0006
                rnit_to_n2o = rnit * turnovfrac         !! kg/ha
        
          case(8)         
                
                rnit_to_n2o = rnit * turnovfr_nitn2o(j) !! kg/ha
        
          case (9)     !!!Century----- Parton 2001----- 
                turnovfrac = 0.02
                rnit_to_n2o = rnit * turnovfrac         !! kg/ha        
        
         end select     
          
     
         
      !! Calculating NO based on N2O-----------------------------------
         select case(nit_no_method)
         
         case(0)
            tem_no  =0.                    !!  kgN/ha day
      
         case(1)     !  Qichun Yang     
         !WFPS_dDO = ( (sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j) )
    ! &                    /sol_por(1,j)
           WFPS_dDO =(  (sol_fc(1, j)+sol_wpmm(1,j)) /  (sol_ul(1,j) + sol_wpmm(1,j) ) )   !*sol_por(k,j) 
     
            dDO =  diffusiv((sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j), sol_bd(1,j), WFPS_dDO)
            krain_no = 1.0
            Rno_n2o(k,j) = 8.0 + (18.0*atan(0.75*PI*(10.*dDO-1.86))) /PI
            if (idplt(j) == 4) Rno_n2o(k,j) = Rno_n2o(k,j)* 0.5 
            tem_no = Rno_n2o(k,j) * rnit_to_n2o * krain_no      !!  kgN/ha day
     

         case(2)             !!DAYCENT used by Fang, et al. 2015           
       !  WFPS_dDO = ( (sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j) )
   !  &          /sol_por(1,j)
            WFPS_dDO =(  (sol_fc(1, j)+sol_wpmm(1,j)) / (sol_ul(1,j) + sol_wpmm(1,j) ) )   !*sol_por(k,j) 
            dDO =  diffusiv((sol_fc(1, j)+sol_wpmm(1,j)) / sol_z(1,j), 	&
                                                sol_bd(1,j), WFPS_dDO)
            Rno_n2o(k,j) =4.+(9.*atan((0.75*PI*(10.*dDO-1.86)))/PI)
            tem_no = Rno_n2o(k,j) * rnit_to_n2o
      
         end select 
      
   
         !! Adjust N2O, NO and nitrification       
         rnit_to_n2o = max(0., rnit_to_n2o ) 
         tem_no = max(0.,tem_no)
         if (rnit > (rnit_to_n2o+tem_no) )then 
          rnit = rnit - (rnit_to_n2o+tem_no)                                      !! kg/ha
         else
          rnit_to_n2o = rnit* rnit_to_n2o/(rnit_to_n2o+tem_no)       !! kgN/ha
          tem_no = rnit - rnit_to_n2o                                               !! kgN/ha
          rnit = 0.
         end if    
         
   
          !! update N2O and NO emissions for each soil layer
          if (rnit_to_n2o < 1.0E-6) rnit_to_n2o = 0.     !! kgN/ha  
          N2O_nit(j)  = N2O_nit(j) + rnit_to_n2o                !! kgN/ha
          N2O(j)  = N2O(j) + rnit_to_n2o                 !! kgN/ha
          if (tem_no < 1.0E-6) tem_no = 0.               !! kgN/ha 
          NO_nit(j)  = NO_nit(j) + tem_no                    !! kgN/ha
          NO(j)  = NO(j) + tem_no                        !! kgN/ha


          sol_no3(k,j) = sol_no3(k,j) + rnit                !! kg/ha
        
         endif  !if (rnit > 1.0E-6)  


         !! summary calculations
         if (curyr > nyskip) then
         wshd_voln = wshd_voln + rvol * hru_dafr(j)          !! kg/ha
         wshd_nitn = wshd_nitn + rnit1 * hru_dafr(j)             !! kg/ha
         end if
                 
        end if !!  if (k/=i_sep(j).or.isep_opt(j)/= 1) 
       end if !!  if (sol_nh4(k,j) > 0.)
      end do  !! do k = 1, sol_nly(j)
 
        !! reduce NO emission by plant obsorption
      !    if (laiday(j) > 0.) then
          !  if (laiday(j) > 8.) then
         !       canopy_red = 0.4428
         !   else
         !       canopy_red = (0.0077 * laiday(j)**2.0  -0.13 * laiday(j)
    !  &         + 0.99)  
          !  endif   
      !     no_adsorb = NO(j) * (1.-canopy_red)     
        !   plantn(j) = plantn(j) + no_adsorb 
         !  NO(j) = NO(j) - no_adsorb      
      !    end if
        
     
      return
      end
 !!===================================================================================================================================================================
      !!nitrification fuctions

      function f_gen_poisson_density (x1, x2)    
       use parm
      implicit none
      
      real :: x1, x2
      real :: aa1, aa2, aa3, aa4 
      real :: f_gen_poisson_density
      real :: temp1, temp2, temp3
     
       aa1 =  35.
       aa2 = -5.
       aa3 = 4.5
       aa4 = 7.
      
        if ( x2 .ge. 35.) aa1 = x2
      
      if (aa2 == aa1) then
       f_gen_poisson_density = 0.   
       else
         temp1 = (aa2-x1)/(aa2 -aa1)   
         if (temp1 .le. 0.) then
            f_gen_poisson_density = 0.   
         else
           temp2 = 1.0 - (temp1**aa4)
             temp3 = temp1** aa3
            f_gen_poisson_density = exp(aa3 * temp2 / aa4 * temp3)
         end if
       end if    
       
       return 
       end function
       
      