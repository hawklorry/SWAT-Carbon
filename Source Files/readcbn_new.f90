     subroutine readcbn
     USE parm 
     use parm_control
     use parm_output
     use parm_subC
     IMPLICIT  NONE
     character (len=80) :: titldum
     real :: tillagef_para, inhibday_para, inhibdu_para
     integer :: eof, counter, counter1
     !!    initialize variables
      eof = 0
      
      open (basins_cbn_num,file="basins.cbn")

      !! read basin_C parameters
      do
      read (basins_cbn_num,1000) titldum
      !Modeling Options:
      read (basins_cbn_num,1000) titldum      
      read (basins_cbn_num,*) cswat
      
      !Soil layer standarization
      read (basins_cbn_num, 1000) titldum
      read (basins_cbn_num,*) soil_layer_std
      
      !Weather Input:
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) iwea
      read (basins_cbn_num,*) idh
      read (basins_cbn_num,*) nstep1
      read (basins_cbn_num,*) idt1
      
      !Temperature:
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) tswat
      read (basins_cbn_num,*) iwtmp_sub
      read (basins_cbn_num,*) iwtmp_rch
      
      !Nitrogen: 
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) nswat
      read (basins_cbn_num,*) denit_method
      read (basins_cbn_num,*) denit_n2o_method
      read (basins_cbn_num,*) denit_no_method
      read (basins_cbn_num,*) nit_method
      read (basins_cbn_num,*) nit_n2o_method
      read (basins_cbn_num,*) nit_no_method
      read (basins_cbn_num,*) inhibday_para
      do counter = 1, mhru ![,step]   
        Inhibday(counter) = inhibday_para
      end do
      
      read (basins_cbn_num,*) inhibdu_para
      do counter = 1, mhru ![,step]   
        Inhibdu(counter) = inhibdu_para
      end do      
      
      !Reach/Reservoir:
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) ievent_rch
      read (basins_cbn_num,*) nstep_rch
      read (basins_cbn_num,*) irch_SedFlux
      read (basins_cbn_num,*) irch_SedResusp
      read (basins_cbn_num,*) ires_SedFlux
      read (basins_cbn_num,*) ires_SedResusp
      !Factors affecting decompostion
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) idc_till
      read (basins_cbn_num,*) idc_sw
      read (basins_cbn_num,*) idc_tmp
      read (basins_cbn_num,*) tillagef_para
      do counter = 1, mhru ![,step] 
        do counter1 = 1, mlyr  
            tillagef(counter1,counter) = tillagef_para
        end do
      end do     
      
      !CO2 Input:
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) ico2
      !Forest Modeling:
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) ifor
      !Print Options:
      read (basins_cbn_num,1000) titldum
      read (basins_cbn_num,*) rch_std
      read (basins_cbn_num,*) rch2
      !read (basins_cbn_num,*) rch3
      !read (basins_cbn_num,*) rch4
     ! read (basins_cbn_num,*) rch5
     ! read (basins_cbn_num,*) rch6
     ! read (basins_cbn_num,*) rch7
     ! read (basins_cbn_num,*) res2
     ! read (basins_cbn_num,*) res3
     ! read (basins_cbn_num,*) res4
     ! read (basins_cbn_num,*) res5
     ! read (basins_cbn_num,*) res6
     ! read (basins_cbn_num,*) res7
      !read (basins_cbn_num,*) isubdaily_output
     ! read (basins_cbn_num,1000) titldum
     ! read (basins_cbn_num,*) iproject_lst
     
      
      exit

      end do
     
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
     
      if(iwtmp_sub==2) then
            isurface_tmp = 0   !!=0 soil surface temperature; =1 quilibrium temperature
      end if
       
      if(iwtmp_rch == 1) then
            wtmp_option = 2  !! 1=Ficklin's model   !!2= Du's stream temperature ;
      end if 
      
      !! only daily time step allowed when ievent_rch==0
      if(ievent_rch==0)then   
            nstep_rch = 1
      end if
     
      !! only hourly time step allowed when ievent_rch==1 
      if( ievent_rch ==1) then
        nstep_rch = 24               
        idt_rch = 1440/nstep_rch           !! minutes
        idh = 1                     
      end if
        
    
     !! Print out options===========================
      jrch_out = mch-1
      
     
      
      close (basins_cbn_num) 


      if (cswat == 1) then
	 open (100,file="cswat_profile.txt",recl=280)
	 write (100,*) 'year',';','day',';','hru',';','cmass',';','sol_rsd',&
      ';','mancmass'
      end if

 
       !!add by zhang
      !=====================
      if (cswat == 2) then
      open (cswat_profile_num,file="cswat_profile.txt")
       write (cswat_profile_num,5102)   'year',     'day',      'ly',       'hru',      'CFMETS1',  'CFSTRS1',  'CFSTRS2',  'CFS1S2',   'CFS1S3',    'CFS2S1', & !10
                                        'CFS2S3',   'CFS3S1',   'EFS1S2',   'EFMETS1',  'EFSTRS1',  'EFSTRS2',  'EFS1S3',   'EFS2S1',   'EFS2S3',   'EFS3S1', & !20
                                        'IMMMETS1', 'IMMSTRS1', 'IMMSTRS2', 'IMMS1S2',  'IMMS1S3',  'IMMS2S1',  'IMMS2S3',  'IMMS3S1',  'MNRMETS1', 'MNRSTRS1', & !30
                                        'MNRSTRS2', 'MNRS1S2',  'MNRS1S3',  'MNRS2S1',  'MNRS2S3',  'MNRS3S1',  'CO2FMET',  'CO2FSTR_L','CO2FSTR_N','CO2FS1',   & !40
                                        'CO2FS2',   'CO2FS3',                                                                                                   & !42
                                        'STRUCC',   'STRUCC_bal',                                                                                               & !44
                                        'METABC',   'METABC_bal',                                                                                                & !46
                                        'SOM1C',    'SOM1C_bal',                                                                                                 & !48
                                        'SOM2C',    'SOM2C_bal',                                                                                                 & !50
                                        'SOM3C',    'SOM3C_bal',                                                                                                 & !52
                                        'STRUCE',   'STRUCE_bal',                                                                                                & !54
                                        'METABE',   'METABE_bal',                                                                                                & !56
                                        'SOM1E',    'SOM1E_bal',                                                                                                 & !58
                                        'SOM2E',    'SOM2E_bal',                                                                                                 & !60
                                        'SOM3E',    'SOM3EPbal',                                                                                                 & !62
                                        'NO3','NH3','AMINRL','nh4_immo','no3_immo','nh4_min','LatC','PerC'                                                        !70
                                        !'SedYield','USLEC','USLEMult','CKLSP',&
                                        !'SoilCover','Biomass','Residue'

      !! open (1001,file="cswat_daily.txt",recl=826)
      open (cswat_daily_num,file="cswat_daily.txt")!! modified by Qichun
      write (cswat_daily_num,5104)     'year',           'day',          'hru',         'crop',         'rsdc',         'rsdn',       'ORGC_f',       'ORGN_f',        'MUSLE',         'USLE', &  !10
                                         'er',           'cbn',         'sedc',        'percc',         'latc',        'emitc',       'grainc',      'surfq_c',      'stoverc',         'NPPC', & !20
                                       'GPPC',           'foc',         'rspc',     'tot_mass',    'tot_cmass',    'tot_nmass',      'tot_LSC',     'tot_LSLC',    'tot_LSLNC',      'tot_LMC', & !30
                                    'tot_HSC',       'tot_HPC',      'tot_BMC',      'tot_LSN',      'tot_LMN',      'tot_HSN',      'tot_HPN',      'tot_BMN',       'Biom_C',         'rwtf', & !40
                                         'ET',   'Tillfactor',       'CFMETS1',      'CFSTRS1',      'CFSTRS2',       'CFS1S2',       'CFS1S3',                                                 & !47
                                     'CFS2S1',        'CFS2S3',       'CFS3S1',       'EFS1S2',      'EFMETS1',      'EFSTRS1',      'EFSTRS2',       'EFS1S3',       'EFS2S1',       'EFS2S3', & !57
                                     'EFS3S1',      'IMMMETS1',     'IMMSTRS1',     'IMMSTRS2',      'IMMS1S2',      'IMMS1S3',      'IMMS2S1',      'IMMS2S3',      'IMMS3S1',     'MNRMETS1', & !67
                                   'MNRSTRS1',      'MNRSTRS2',      'MNRS1S2',      'MNRS1S3',      'MNRS2S1',      'MNRS2S3',      'MNRS3S1',      'CO2FMET',    'CO2FSTR_L',    'CO2FSTR_N', & !77
                                     'CO2FS1',        'CO2FS2',       'CO2FS3',  'Metabolic_C', 'Structural_C',         'S1_C',         'S2_C',         'S3_C',  'Metabolic_N', 'Structural_N', & !87
                                       'S1_N',          'S2_N',         'S3_N',    'uptakeno3',      'rainno3',      'sol_rsd',       'bio_ms',      'sol_cov',      'cfactor','NO3loss_Pothole', & !97
                                     'NO3_up',  'absorbed_no3', 'absorbed_nh3',     'no3_immo',      'nh4_min',      'surqno3',       'latno3',     'no3_perc',    'no3_denit',       'no3_rain', & !107
                                   'no3_nitr',      'nh4_immo',    'immo_err1',      'nh4_vol',     'no3_fert',     'nh4_fert',    'no3_autof',    'nh4_autof',          'NO3',            'NH3', & !117
                                       'TIMM',          'TMNR',       'no3_in',      'no3_out',       'nh4_in',      'nh4_out',          'CH4',          'N2O',      'N2O_den',           'NO',   & !127
                                      'NO_den' !128
     
           
      !!add by zhang
      !!=====================


      open (cswat_daily1_num,file="cswat_daily1.txt")
      write (cswat_daily1_num,5111) 'year','day','hru', &
     'CMF1','CMF2','CMF3','CMF4','CMF5',& 
     'WATF1','WATF2','WATF3','WATF4','WATF5',& 
     'TEMF1','TEMF2','TEMF3','TEMF4','TEMF5',& 
     'OXGF1','OXGF2','OXGF3','OXGF4','OXGF5',& 
     'TILF1','TILF2','TILF3','TILF4','TILF',& 
     'R_LSCTP1','R_LSCTP2','R_LSCTP3','R_LSCTP4','R_LSCTP5',& 
     'R_LMCTP1','R_LMCTP2','R_LMCTP3','R_LMCTP4','R_LMCTP5',& 
     'R_BMCTP1','R_BMCTP2','R_BMCTP3','R_BMCTP4','R_BMCTP5',& 
     'R_HSCTP1','R_HSCTP2','R_HSCTP3','R_HSCTP4','R_HSCTP5',& 
     'R_HPCTP1','R_HPCTP2','R_HPCTP3','R_HPCTP4','R_HPCTP5',& 
     'LSLC1','LSLC2','LSLC3','LSLC4','LSLC5',& 
     'LSnLC1','LSnLC2','LSnLC3','LSnLC4','LSnLC5',& 
     'LMC1','LMC2','LMC3','LMC4','LMC5',& 
     'BMC1','BMC2','BMC3','BMC4','BMC5',& 
     'HSC1','HSC2','HSC3','HSC4','HSC5',& 
     'HPC1','HPC2','HPC3','HPC4','HPC5',& 
     'LSN1','LSN2','LSN3','LSN4','LSN5',& 
     'LMN1','LMN2','LMN3','LMN4','LMN5',& 
     'BMN1','BMN2','BMN3','BMN4','BMN5',& 
     'HSN1','HSN2','HSN3','HSN4','HSN5',& 
     'HPN1','HPN2','HPN3','HPN4','HPN5',& 
     'LSCTA1','LSCTA2','LSCTA3','LSCTA4','LSCTA5',& 
     'LSLCTA1','LSLCTA2','LSLCTA3','LSLCTA4','LSLCTA5',& 
     'LSLnCTA1','LSLnCTA2','LSLnCTA3','LSLnCTA4','LSLnCTA5',& 
     'LMCTA1','LMCTA2','LMCTA3','LMCTA4','LMCTA5',& 
     'BMCTA1','BMCTA2','BMCTA3','BMCTA4','BMCTA5',& 
     'HSCTA1','HSCTA2','HSCTA3','HSCTA4','HSCTA5',& 
     'HPCTA1','HPCTA2','HPCTA3','HPCTA4','HPCTA5',& 
     'LSNTA1','LSNTA2','LSNTA3','LSNTA4','LSNTA5',& 
     'LMNTA1','LMNTA2','LMNTA3','LMNTA4','LMNTA5',& 
     'BMNTA1','BMNTA2','BMNTA3','BMNTA4','BMNTA5',& 
     'HSNTA1','HSNTA2','HSNTA3','HSNTA4','HSNTA5',& 
     'HPNTA1','HPNTA2','HPNTA3','HPNTA4','HPNTA5',& 
     'LSCTP1','LSCTP2','LSCTP3','LSCTP4','LSCTP5',& 
     'LSLCTP1','LSLCTP2','LSLCTP3','LSLCTP4','LSLCTP5',& 
     'LSLnCTP1','LSLnCTP2','LSLnCTP3','LSLnCTP4','LSLnCTP5',& 
     'LMCTP1','LMCTP2','LMCTP3','LMCTP4','LMCTP5',& 
     'BMCTP1','BMCTP2','BMCTP3','BMCTP4','BMCTP5',& 
     'HSCTP1','HSCTP2','HSCTP3','HSCTP4','HSCTP5',& 
     'HPCTP1','HPCTP2','HPCTP3','HPCTP4','HPCTP5',& 
     'LSNTP1','LSNTP2','LSNTP3','LSNTP4','LSNTP5',& 
     'LMNTP1','LMNTP2','LMNTP3','LMNTP4','LMNTP5',& 
     'BMNTP1','BMNTP2','BMNTP3','BMNTP4','BMNTP5',& 
     'HSNTP1','HSNTP2','HSNTP3','HSNTP4','HSNTP5',& 
     'HPNTP1','HPNTP2','HPNTP3','HPNTP4','HPNTP5',& 
     'etday',  &  
     'soltmp1','soltmp2','soltmp3','soltmp4','soltmp5',& 
     'solwt1','solwt2','solwt3','solwt4','solwt5'
      endif       



      
      return
 1000 format (a)     
 5102 format (a4,a4,a4,a8,66a16)     
 5104 format (a4,a4,a8,a8,163a16)
 5111 format (a4,a4,a8,236a16)  
 
 
      end 