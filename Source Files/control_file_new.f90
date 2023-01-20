        subroutine control_file
        use carbon_para 
        use parm  
        use parm_control
        use parm_output
        implicit none  
        
        !!************************************Initializing control variables**********************************************************
        iwea = 0                        !! 0 =daily input; 1=hourly input call clicon_hour then call pmeas_hour....  
        idh = 0                           !! 0=do not calculate hourly weather input based on daily weather input;  1 =calculate hourly weather data from daily input
        nstep1=0                       !!  hourly weather used for either iwea=0 and idh=1 or iwea=1 idh=0
        idt1=0                            !! minutes
        ievent_rch = 0                 !!  0=daily reach routing; 1=reach routing subdaily time step
        nstep_rch = 0                 !! subdaily time step 
        idt_rch = 0                     !! minutes  
        tswat = 0                       !!  0 =original soil temperature module; 1=heat transfer/soil temperature module; 
        nswat = 0                      !! 0 =original  nitrification and denitrification module;  1= other modules 
        denit_method = 0           !! 0 =original SWAT algorithm;  1= Yang et al 2017; 2=Wagena et al 2017; 3= no denitrication
        denit_n2o_method =0    !! 1 =Yang et al 2017  ;  2= Daycent ((Fang et al 2015) ;3=   NOE model  (Fang et al 2015) ; !! 4 =WNMM model  (Fang et al 2015); 5= FASSET model (Fang et al 2015); 6=  Wagena et al 2017           
        denit_no_method = 0     !! 0= no NO calculation ; 1= Yang et al 2017; 2= DAYCENT used by Fang, et al. 2015
        nit_method = 0               !! 0 = original SWAT algorithm;  1= Yang et al 2017   ; 3= no nitrification
        nit_n2o_method = 0      !! 1 =Yang et al 2017 ;   2=Daycent ((Fang et al 2015);   3=NOE model (Fang et al 2015); !! 4=WNMM model  (Fang et al 2015); 5= FASSET model (Fang et al 2015); 6= Wagena et al 2017; 7= CLM4.5 (Li et al., 2000)(Fu et al 2018)                    
        nit_no_method = 0        !! 0 = no NO calculation ; 1= Yang et al 2017; 2= DAYCENT used by Fang, et al. 2015
        ico2 = 0                        !! 0=using default CO2 value; 1=reading in co2 concentration.    NOTE: need co2.dat file
        ifor = 0                         !! 0=default forestry module;  1=Yang et al 2019 forestry module.     NOTE: need updataed plant.dat file
        iwtmp_rch=0                      !! 0=default stream water temperature model; 1=Du's stream temperature model
        iwtmp_sub=0                 !!iwtmp_sub = 0 !! 0=default ; 1= F's subbasin water temperaute; 2= Qi's hru/subbasin water temperature WHEN tswat=1
        wtmp_option=0            !!1=Ficklin's model   !!2= Du's stream temperature ;
        !!********************************************************************************************************************
        
          !!********************************************  Projects Overwrite  *************************************************************************
       !  irchwet = 1
         
        SELECT CASE (2)
        CASE(0)  !! Default condition
        
             !! Weather Input Control==============
            iwea = 0                        !! 0 =daily input; 1=hourly input call clicon_hour then call pmeas_hour....  
            idh = 0                           !! 0=do not calculate hourly weather input based on daily weather input;  1 =calculate hourly weather data from daily input
            !! Reach  Routing Control==============
            ievent_rch =0                 !!  0=daily reach routing; 1=reach routing subdaily time step
            !!Soil Temperature Control===============
            tswat = 1                     !!  0 = original soil temperature module; 1 = heat transfer/soil temperature module; 
            !!Nitrogen cycling Control===============
            !! effective only with cswat=2:
            nswat = 1                      !! 0 =original  nitrification and denitrification module;  1= other modules 
             !!CO2 Input Control ==================   
            ico2 = 0                       !! 0=using default CO2 value; 1=reading in co2 concentration.    NOTE: need co2.dat file
            !!Forestry Modeling Control =============
            ifor = 0                       !! 0=default forestry module;  1=Yang et al 2019 forestry module.     NOTE: need updataed plant.dat file    
            !! Stream Water Temperature Control ====================
            iwtmp_rch = 0    !!0=default stream water temperature model; 1=new stream water temperature model
            !! Subbasin Water Temperature Control ===========================
            iwtmp_sub = 0 !! 0=default ; 1= F's subbasin water temperaute; 2= Qi's hru/subbasin water temperature WHEN tswat=1
            isurface_tmp = 1   !!=0 soil surface temperature; =1 quilibrium temperature
      

        CASE(1)    !! UMRB_2005 N
             !! Weather Input Control==============
            iwea = 0                        !! 0 =daily input; 1=hourly input call clicon_hour then call pmeas_hour....  
            idh = 0                           !! 0=do not calculate hourly weather input based on daily weather input;  1 =calculate hourly weather data from daily input
            !! Reach  Routing Control==============
            ievent_rch =0                 !!  0=daily reach routing; 1=reach routing subdaily time step
            !!Soil Temperature Control===============
            tswat = 1                     !!  0 = original soil temperature module; 1 = heat transfer/soil temperature module; 
            !!Nitrogen cycling Control===============
            !! effective only with cswat=2:
            nswat = 1                      !! 0 =original  nitrification and denitrification module;  1= other modules 
             !!CO2 Input Control ==================   
            ico2 = 0                       !! 0=using default CO2 value; 1=reading in co2 concentration.    NOTE: need co2.dat file
            !!Forestry Modeling Control =============
            ifor = 0                       !! 0=default forestry module;  1=Yang et al 2019 forestry module.     NOTE: need updataed plant.dat file    
            !! Stream Water Temperature Control ====================
            iwtmp_rch = 0    !!0=default stream water temperature model; 1=new stream water temperature model
            !! Subbasin Water Temperature Control ===========================
            iwtmp_sub = 0 !! 0=default ; 1= F's subbasin water temperaute; 2= Qi's hru/subbasin water temperature WHEN tswat=1
            isurface_tmp = 0   !!=0 soil surface temperature; =1 quilibrium temperature
        

     
        CASE(2)    !! Choptank River
             !! Weather Input Control==============
            iwea = 0                        !! 0 =daily input; 1=hourly input call clicon_hour then call pmeas_hour....  
            idh = 0                           !! 0=do not calculate hourly weather input based on daily weather input;  1 =calculate hourly weather data from daily input
            !! Reach  Routing Control==============
            ievent_rch =1                 !!  0=daily reach routing; 1=reach routing subdaily time step
            !!Soil Temperature Control===============
            tswat =1                     !!  0 = original soil temperature module; 1 = heat transfer/soil temperature module; 
            !!Nitrogen cycling Control===============
            !! effective only with cswat=2:
            nswat = 1                      !! 0 =original  nitrification and denitrification module;  1= other modules 
             !!CO2 Input Control ==================   
            ico2 = 0                       !! 0=using default CO2 value; 1=reading in co2 concentration.    NOTE: need co2.dat file
            !!Forestry Modeling Control =============
            ifor = 0                       !! 0=default forestry module;  1=Yang et al 2019 forestry module.     NOTE: need updataed plant.dat file    
            !! Stream Water Temperature Control ====================
            iwtmp_rch = 0    !!0=default stream water temperature model; 1=new stream water temperature model
            !! Subbasin Water Temperature Control ===========================
            iwtmp_sub = 0 !! 0=default ; 1= F's subbasin water temperaute; 2= Qi's hru/subbasin water temperature WHEN tswat=1
            isurface_tmp = 0   !!=0 soil surface temperature; =1 quilibrium temperature
        

      
        CASE(3)    !! Vantaanjoki Finland
             !! Weather Input Control==============
            iwea = 0                        !! 0 =daily input; 1=hourly input call clicon_hour then call pmeas_hour....  
            idh = 0                           !! 0=do not calculate hourly weather input based on daily weather input;  1 =calculate hourly weather data from daily input
            !! Reach  Routing Control==============
            ievent_rch =1                 !!  0=daily reach routing; 1=reach routing subdaily time step
            !!Soil Temperature Control===============
            tswat = 0                     !!  0 = original soil temperature module; 1 = heat transfer/soil temperature module; 
            !!Nitrogen cycling Control===============
            !! effective only with cswat=2:
            nswat = 0                      !! 0 =original  nitrification and denitrification module;  1= other modules 
             !!CO2 Input Control ==================   
            ico2 = 0                       !! 0=using default CO2 value; 1=reading in co2 concentration.    NOTE: need co2.dat file
            !!Forestry Modeling Control =============
            ifor = 0                       !! 0=default forestry module;  1=Yang et al 2019 forestry module.     NOTE: need updataed plant.dat file    
            !! Stream Water Temperature Control ====================
            iwtmp_rch = 0    !!0=default stream water temperature model; 1=new stream water temperature model
            !! Subbasin Water Temperature Control ===========================
            iwtmp_sub = 0 !! 0=default ; 1= F's subbasin water temperaute; 2= Qi's hru/subbasin water temperature WHEN tswat=1
            isurface_tmp = 1   !!=0 soil surface temperature; =1 quilibrium temperature
        

        CASE(4)    
        
        END SELECT 
        
        
        !!************************************************* Projects Overwrite ******************************************************************
        
       if( ievent_rch ==1) then
            nstep_rch = 1           !! subdaily time step   1440= 1min; 144=10min; 48=30min; 24= hourly;   12=2hour; 6= 4hour; 4=6hour; 2=12hour; 1= daily; 
            idt_rch = 1440/ nstep_rch          !! minutes
            idh = 1                             !! 0=do not calculate hourly weather input based on daily weather input;  1 =calculate hourly weather data from daily input
           
            irch_SedFlux =1                    !!=1 simulating sediment flux
            ires_SedFlux =1                    !!=1 simulating sediment flux
            irch_SedResusp=0                 !! =1 simulating sediment resuspension flux induced nutrient flux
            ires_SedResusp=0                 !! =1 simulating sediment resuspension flux induced nutrient flux
       end if 
       
        if( idh == 1 .or.  iwea == 1) then
            nstep1=24                        !!  hourly weather used for either iwea=0 and idh=1 or iwea=1 idh=0
            idt1=60                            !! minutes
        end if 
        
        !! Snow model       
        if(tswat==1) then  !!  0 = original snow model;  1= WITH tswat=1   
         isnowm= 1  
        else
         isnowm= 0  
        endif
     
        !! effective only when nswat=1:
        if(nswat ==1) then
            denit_method = 2           !! 0 =original SWAT algorithm;  1= Yang et al 2017  ; 2=Wagena et al 2017; 3= no denitrication
            denit_n2o_method = 2  !! 1 =Yang et al 2017  ;  2= Daycent ((Fang et al 2015) ;3=   NOE model  (Fang et al 2015) ;
                                    !! 4 =WNMM model  (Fang et al 2015); 5= FASSET model (Fang et al 2015); 6=  Wagena et al 2017 
            denit_no_method = 1    !! 0= no NO calculation ; 1= Yang et al 2017; 2= DAYCENT used by Fang, et al. 2015
              
            nit_method = 0               !! 0 = original SWAT algorithm;  1= Yang et al 2017   ; 3= no nitrification
            nit_n2o_method = 8      !! 1 =Yang et al 2017 ;   2=Daycent ((Fang et al 2015);   3=NOE model (Fang et al 2015);
                                      !! 4=WNMM model  (Fang et al 2015); 5= FASSET model (Fang et al 2015); 6= Wagena et al 2017; 7= CLM4.5 (Li et al., 2000)(Fu et al 2018)£» 8= calibration
            nit_no_method = 1        !! 0 = no NO calculation ; 1= Yang et al 2017; 2= DAYCENT used by Fang, et al. 2015
       
        end if
        
      
        if(iwtmp_rch == 1) then
            wtmp_option = 2  !! 1=Ficklin's model   !!2= Du's stream temperature ;
        end if 
        
      
        
        !! Special Projects ===========================
        !iproject_lst = 0   !! 1= readin MODIS Land surface temperature 
         
        !! Print out options===========================
        rch_std = 1   !! 1= print out rch variables in output.std 
        jrch_out= mch-1
        rch2 =1
       ! rch3 =1
        !rch4 =1
        !rch5 =1
        !rch6 =1
        !rch7 =1
       
       ! res2 =1
       ! res3 =1
        !res4 =1
        !res5 =1
       ! res6 =1
       ! res7 =1
      
      !!  subdaily rch and res output
       ! isubdaily_output = 0     !! 0= not printing subdaily output; 1 = printing subdaily output for reach and res 
        
       !Cannonsville cswat=2 nswat=1 ifor=1
       !Choptank cswat=2 nswat=1
      
      
    
      
      
      
       !References:
       !Fang, Q.X., Ma, L., Halvorson, A.D., Malone, R.W., Ahuja, L.R., Del Grosso, S.J. and Hatfield, J.L., 2015. Evaluating four nitrous oxide emission algorithms in response to N rate on an irrigated corn field. Environmental Modelling & Software, 72, pp.56-70.
       !Yang, Q., Zhang, X., Abraha, M., Del Grosso, S., Robertson, G.P. and Chen, J., 2017. Enhancing the soil and water assessment tool model for simulating N2O emissions of three agricultural systems. Ecosystem Health and Sustainability, 3(2), p.e01259.
       !Wagena, M.B., Bock, E.M., Sommerlot, A.R., Fuka, D.R. and Easton, Z.M., 2017. Development of a nitrous oxide routine for the SWAT model to assess greenhouse gas emissions from agroecosystems. Environmental modelling & software, 89, pp.131-143.
       !Yang, Q., Zhang, X., Almendinger, J.E., Huang, M., Leng, G., Zhou, Y., Zhao, K., Asrar, G.R., Li, X. and Qiu, J., 2019. Improving the SWAT forest module for enhancing water resource projections: A case study in the St. Croix River basin. Hydrological processes, 33(5), pp.864-875.
       !Yang, Q., Almendinger, J.E., Zhang, X., Huang, M., Chen, X., Leng, G., Zhou, Y., Zhao, K., Asrar, G.R., Srinivasan, R. and Li, X., 2018. Enhancing SWAT simulation of forest ecosystems for water resource assessment: A case study in the St. Croix River basin. Ecological engineering, 120, pp.422-431.
       !Yang, Q. and Zhang, X., 2016. Improving SWAT for simulating water and carbon fluxes of forest ecosystems. Science of the Total Environment, 569, pp.1478-1488.
       !Li, C., Aber, J., Stange, F., Butterbach-Bahl, K., & Papen, H. (2000). A process-oriented model of N2O and NO emissions from forest soils. 1:Model development. Journal of Geophysical Research, 105(D4), 4369?384.
       !Fu, C., Lee, X., Griffis, T.J., Baker, J.M. and Turner, P.A., 2018. A modeling study of direct and indirect N2O emissions from a representative catchment in the US Corn Belt. Water Resources Research, 54(5), pp.3632-3653.
       
       return
       end