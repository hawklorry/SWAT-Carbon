      subroutine fern_hyd
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   Following application of urea to the soil,
!!   urea is rapidly hydrolyzed by extracellular enzyme
!!   urease into plant-available NH4
!!   CO(NH2)2 + 2H2O + H+  ---Urease--->  2NH4+  +  HCO3


      use parm
      use parm_subC
      implicit none  
      
      real :: Bt1, Bt2, Bs1, Bs2, WFPS 
      real :: Fst,FsM,rate_hyd,urea_ppm,grams_soil
      real :: Vmax,Km, Kui
      
      real :: soil_T,Fsoc,ddf,urea_hydro
      real :: wd1, Kw, V_water, consume_H, hydrogen, mol_hydro_urea, hydroxide
      real :: wd2,Ka, mol_nh4, mol_nh3
      real :: old_nh3,cvf,TtN,d_nh3 
      
      integer :: j, k 
      j = 0
      j = ihru
           
       if(Inhibday(j)>0) Inhibday(j) = Inhibday(j)+1
	   if(Inhibday(j)==Inhibdu(j)) then
	   Inhibday(j)= 0
	   end if
      
      Bt1 = 15.1500
      Bt2 = 0.8666
      Bs1 = 0.5910
      Bs2 = 1.6890
      
      Vmax = 0.0166   !!ppm/s; the maximum urea hydrolysis rate that is achieved by the system. 
      Km = 43.0000   !!ppm; the Michaelis constant representing the urea concentration at which the reaction rate is half of Vmax. 
      Kui = 0.  !!ppm; the additional effect of urease inhibitors on the Michaelis constant (KUI= 0 with no urease inhibitor applied).
      !urea_ppm  !!ppm; the urea concentration.
    
    
      do k = 1,sol_nly(j)           
     
       
 
       WFPS = 0.
       WFPS = ((sol_st(k,j)+sol_wpmm(k,j)) /(sol_ul(k,j) + sol_wpmm(k,j) ) )

       !! calculate  concentration in PPM (mg/kg)
	   urea_ppm=0.
	   grams_soil=0.
	   grams_soil = sol_bd(k,j) * sol_thick(k,j)*0.1 * 100.0 * 100.0	   
	   urea_ppm = sol_urea(k,j)*0.1/grams_soil*1.0E6
       

       if(sol_tmp(k,j)<0.0)then 
       soil_T = 0.0
       else 
       soil_T = sol_tmp(k,j)
       end if
       
       
       select case(2)
       
       !! Ram B. Gurung- 2021-Modeling ammonia volatilization from urea application
       !! to agricultural soils in the DayCent model
       case(1)
       
     
       
           Fst = 0.
           FsM = 0. 
           Fst = 1./ (1. + Bt1*exp(-Bt2*soil_T))
           FsM = Bs1 + Bs2*WFPS 
           Fst = min(1.0, max(0.0, Fst))
           FsM = min(1.0, max(0.0, FsM))
          
           rate_hyd = 0. 
           rate_hyd =( Vmax* urea_ppm)/(Km + Kui + urea_ppm)*Fst*FsM
           !! to kg N/ha day
           urea_hydro = 0.
           urea_hydro = rate_hyd*grams_soil/1.0E6/0.1*24.*60.*60.   
           urea_hydro = min(sol_urea(k,j),urea_hydro)
           
          ! if(j==14  .and.  k==1 .and. i==149)then
          ! print*,sol_urea(k,j),urea_hydro,sol_nh4(k,j)
          ! end if
           !! update state variables in kg N/ha
           sol_urea(k,j) = sol_urea(k,j) - urea_hydro
           sol_nh4(k,j)= sol_nh4(k,j) + urea_hydro
           nh4_fert(j) = nh4_fert(j) + urea_hydro
    
      
       !! DNDC--based on experimental data from Helen Suter, Univ of Melbourne, Australia 
       case(2)
       
            !ttoo[l]= sol_soc(k,j)soc[l]/mmm[1]
            Fsoc = 5.0 * sol_cbn(k,j)/100.
            Fsoc = min(1.0, max(0.0, Fsoc))
            if(Inhibday(j)==0) then
                ddf = (0.5 * soil_T + 0.4) * Fsoc
            else
                ddf = 0.5 *exp(0.0805 * soil_T) * Fsoc
            end if
            ddf = max(0.0, min(1.0, ddf))
            if(ddf>0.0) then
            urea_hydro = 0.
            urea_hydro = sol_urea(k,j) * ddf  
            !urea[l] -= urea_hydro
            !nh4[l] += urea_hydro
          
            !! update state variables in kg N/ha
            sol_urea(k,j) = sol_urea(k,j) - urea_hydro
            sol_nh4(k,j)= sol_nh4(k,j) +  urea_hydro
            nh4_fert(j) = nh4_fert(j) + urea_hydro
            end if
       end select 
     
      
     
      !! impact of urea hydrolisis on soil pH based on DNDC
      select case(0)
      case(1)	
           if(urea_hydro>0.) then
           
               wd1 = 10.0**( -15.0)
	           Kw = 1.945 * exp(0.0645 * soil_T) * wd1  !water dissociation constant  
	           hydrogen = 10.0** (-sol_ph(k,j))          !mol H/L
	           hydroxide = Kw / hydrogen                 !mol OH/L
        	   
	           V_water = (sol_st(k,j)+sol_wpmm(k,j))/1000.*10000.*1000. !!L water in soil layer
	           mol_hydro_urea = urea_hydro * 1000.0 / 14.0 / V_water  !!!! kgN/ha->mol N/L
	           !! Consumed H or generated OH during hydrologysis; mol H/L
	           !consume_H = 0.5 * mol_hydro_urea   !! original function
	           consume_H = 0.005 * mol_hydro_urea  !! after considering soil buffer effect  
               
	           !! New H and OH concentration
	           hydroxide = hydroxide + consume_H         !mol OH/L
	           hydrogen = Kw / hydroxide                 !mol H/L
        	   
	           sol_ph(k,j) = log(hydrogen) /  (-2.3026)
	           sol_ph(k,j) = max(3.0, min(11.0, sol_ph(k,j)))
           
           end if
      end select 
      
      
      !! Chemical equilibrium: NH4+ OH- = NH3 + H2O   
       select case(0)
       case(1)
        
           if( sol_nh4(k,j)>0.) then
          
               old_nh3=0.
               old_nh3 = sol_nh3(k,j)
               
               wd1 = 10.0**( -15.0)
	           Kw = 1.945 * exp(0.0645 * soil_T) * wd1 
	           hydrogen = 10.0** (-sol_ph(k,j))          !mol H/L
	           hydroxide = Kw / hydrogen                 !mol OH/L
	           V_water = (sol_st(k,j)+sol_wpmm(k,j))/1000.*10000.*1000. !!L water 
               wd2 = 10.0**( -5.0)
               Ka = (1.416 + 0.01357 * soil_T) * wd2 !!NH4+/NH3 equilibrium constant
              
               mol_nh4 = 1000.0 * sol_nh4(k,j) / 14.0 / V_water   !!;//kg N -> mol/L
               mol_nh3 = mol_nh4 * hydroxide/ Ka   !mol/L
              
             
               cvf = mol_nh4 / (mol_nh4 + mol_nh3)
               TtN = sol_nh4(k,j) + sol_nh3(k,j)
               sol_nh4(k,j)= TtN * cvf 
               sol_nh3(k,j) = TtN - sol_nh4(k,j)   !;//kg N/ha	
              
              
               d_nh3 = sol_nh3(k,j) - old_nh3
               if (d_nh3 > 0.0) then
		        sol_ph(k,j) = sol_ph(k,j)-(0.5 * d_nh3)
		        sol_ph(k,j) = max(3.0, min(11.0, sol_ph(k,j))) 
               end if

           end if 
      
      
       !if(j==37  .and.  k==1  .and. i>117  .and. curyr==2 )then
      !   print*,i,j,sol_ph(k,j),sol_nh4(k,j), sol_nh3(k,j)
       !end if
       
       end select
       
      end do    
      
      
      
      return 
      end 