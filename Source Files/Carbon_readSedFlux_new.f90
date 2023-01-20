    subroutine readCarbonSedFlux

    !!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads parameters from the subbasin instream water 
!!    quality file (.swq) and initializes the QUAL2E variables which apply to
!!    the individual subbasins

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    bc1(:)      |1/day or 1/hr |rate constant for biological oxidation of NH3
!!                               |to NO2 in reach at 20 deg C
!!    bc2(:)      |1/day or 1/hr |rate constant for biological oxidation of NO2
!!                               |to NO3 in reach at 20 deg C
!!    bc3(:)      |1/day or 1/hr |rate constant for hydrolysis of organic N to
!!                               |ammonia in reach at 20 deg C
!!    bc4(:)      |1/day or 1/hr |rate constant for the decay of organic P to
!!                               |dissolved P in reach at 20 deg C
!!    chpst_koc(:)  |m**3/g      |pesticide partition coefficient between
!!                               |water and sediment in reach
!!    chpst_mix(:)  |m/day       |mixing velocity (diffusion/dispersion) for
!!                               |pesticide in reach
!!    chpst_rea(:)  |1/day       |pesticide reaction coefficient in reach
!!    chpst_rsp(:)  |m/day       |resuspension velocity in reach for pesticide
!!                               |sorbed to sediment
!!    chpst_stl(:)  |m/day       |settling velocity in reach for pesticide
!!                               |sorbed to sediment
!!    chpst_vol(:)  |m/day       |pesticide volatilization coefficient in reach
!!    rk1(:)      |1/day or 1/hr |CBOD deoxygenation rate coefficient in reach 
!!                               |at 20 deg C
!!    rk2(:)      |1/day or 1/hr |reaeration rate in accordance with Fickian
!!                               |diffusion in reach at 20 deg C
!!    rk3(:)      |1/day or 1/hr |rate of loss of CBOD due to settling in reach
!!                               |at 20 deg C
!!    rk4(:)      |mg O2/        |sediment oxygen demand rate in reach
!!                |  ((m**2)*day)|at 20 deg C
!!                |or mg O2/((m**2)*hr)
!!    rk5(:)      |1/day         |coliform die-off rate in reach
!!    rk6(:)      |1/day         |decay rate for arbitrary non-conservative
!!                               |constituent in reach
!!    rs1(:)      |m/day or m/hr |local algal settling rate in reach at 20 deg C
!!    rs2(:)      |(mg disP-P)/  |benthos source rate for dissolved phosphorus
!!                |  ((m**2)*day)|in reach at 20 deg C
!!                |or (mg disP-P)/((m**2)*hr)|
!!    rs3(:)      |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in
!!                |  ((m**2)*day)|reach at 20 deg C
!!                |or (mg NH4-N)/((m**2)*hr)|
!!    rs4(:)      |1/day or 1/hr |rate coefficient for organic nitrogen 
!!                               |settling in reach at 20 deg C
!!    rs5(:)      |1/day or 1/hr |organic phosphorus settling rate in reach at
!!                               |20 deg C
!!    rs6(:)      |1/day         |rate coefficient for settling of arbitrary 
!!                               |non-conservative constituent in reach
!!    rs7(:)      |(mg ANC)/     |benthal source rate for arbitrary 
!!                   ((m**2)*day)|non-conservative constituent in reach
!!    sedpst_act(:) |m           |depth of active sediment layer in reach for
!!                               |pesticide
!!    sedpst_bry(:) |m/day       |pesticide burial velocity in river bed
!!                               |sediment
!!    sedpst_conc(:)|mg/(m**3)   |inital pesticide concentration in river bed
!!                               |sediment
!!    sedpst_rea(:) |1/day       |pesticide reaction coefficient in river bed
!!                               |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line in .wq file (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
        use carbon_para
        use parm_subC
        use parm_sedflux
      implicit none
      
      character (len=80) :: titldum
      integer :: eof, counter

      eof = 0

      open (readCarbonSedFlux_num,file='basins_sedflux.dia')  
        
      do
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          !Sediment nutrient fluxes and sediment oxygen demand 
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).m1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).m2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).Dp
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).Dd
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).w2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).H2
          if (eof < 0) exit  
          
          !Reaction velocities 
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaNH3f
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaNH3s
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaNO3_1f
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaNO3_1s
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaNO3_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaCH4
          if (eof < 0) exit            
          
          !Half saturation constants 
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KM_NH3
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KM_O2_NH3
          if (eof < 0) exit     

          !Partitioning coefficients
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KdNH3
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KdPO42
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).dKDPO41f
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).dKDPO41s
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).O2critPO4
          if (eof < 0) exit     


          !Temperature coefficients
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaDp
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaDd
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaKmNH3
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaNH3
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaNO3
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaCH4
          if (eof < 0) exit  

          !Salinity thresholds
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).SALTSW
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).SALTND
          if (eof < 0) exit    

          !Sulfide constants
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaH2Sd1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KappaH2Sp1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaH2S
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KMHSO2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KdH2S1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KdH2S2
          if (eof < 0) exit  

          !Fractions of G classes 1 and 2 for settling PON, POC, and POP	
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).frpon1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).frpon2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).frpoc1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).frpoc2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).frpop1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).frpop2
          if (eof < 0) exit 

          !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P	
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpon1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpon2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpon3
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpoc1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpoc2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpoc3
          if (eof < 0) exit  
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpop1
          if (eof < 0) exit 
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpop2
          if (eof < 0) exit 
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kpop3
          if (eof < 0) exit 

          !Sediment nutrient fluxes and sediment oxygen demand 
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPON1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPON2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPON3
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPOC1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPOC2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPOC3
          if (eof < 0) exit            
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPOP1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPOP2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).ThtaPOP3
          if (eof < 0) exit            
          
          !Parameters for partical mixing and benthic stress
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POC1R
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).kBEN_STR
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KM_O2_Dp
          if (eof < 0) exit     


          !Parameters for silica
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KSI
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).THTASI
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).CSISAT
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).THTASISAT
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).DPIE1SI
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).PIE2SI
          if (eof < 0) exit  
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).KMPSI
          if (eof < 0) exit  
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).O2CRITSI
          if (eof < 0) exit  


          !Particulate organic C, N, and P in layer 2
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POC2_1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POC2_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POC2_3
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).PON2_1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).PON2_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).PON2_3
          if (eof < 0) exit  
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POP2_1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POP2_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POP2_3
          if (eof < 0) exit  
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).POS2
          if (eof < 0) exit 
                    
          !Dissolved constituents in layer 1 and 2 porewater
          read (readCarbonSedFlux_num,5100,iostat=eof) titldum
          if (eof < 0) exit
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).NH3_1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).NH3_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).NO3_1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).NO3_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).PO4_1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).PO4_2
          if (eof < 0) exit  
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).Si_1
          if (eof < 0) exit      
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).Si_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).HS_1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).HS_2
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).CH4_1
          if (eof < 0) exit     
          read (readCarbonSedFlux_num,*,iostat=eof) Sed_rchpara(1).CH4_2
          if (eof < 0) exit  

          exit
      end do



          !Sediment nutrient fluxes and sediment oxygen demand 
          if (Sed_rchpara(1).m1 <= 0.) Sed_rchpara(1).m1 = 0.5
          if (Sed_rchpara(1).m2 <= 0.) Sed_rchpara(1).m2 = 0.5
          if (Sed_rchpara(1).Dp <= 0.) Sed_rchpara(1).Dp = 0.00006
          if (Sed_rchpara(1).Dd <= 0.) Sed_rchpara(1).Dd = 0.0025
          if (Sed_rchpara(1).w2 <= 0.) Sed_rchpara(1).w2 = 0.00000685
          if (Sed_rchpara(1).H2 <= 0.) Sed_rchpara(1).H2 = 0.1
 
          !Reaction velocities 
          if (Sed_rchpara(1).KappaNH3f <= 0.) Sed_rchpara(1).KappaNH3f = 0.131
          if (Sed_rchpara(1).KappaNH3s <= 0.) Sed_rchpara(1).KappaNH3s = 0.131
          if (Sed_rchpara(1).KappaNO3_1f <= 0.) Sed_rchpara(1).KappaNO3_1f = 0.1
          if (Sed_rchpara(1).KappaNO3_1s <= 0.) Sed_rchpara(1).KappaNO3_1s = 0.1
          if (Sed_rchpara(1).KappaNO3_2 <= 0.) Sed_rchpara(1).KappaNO3_2 = 0.25
          if (Sed_rchpara(1).KappaCH4 <= 0.) Sed_rchpara(1).KappaCH4 = 0.7

          !Half saturation constants 
          if (Sed_rchpara(1).KM_NH3 <= 0.) Sed_rchpara(1).KM_NH3 = 0.728
          if (Sed_rchpara(1).KM_O2_NH3 <= 0.) Sed_rchpara(1).KM_O2_NH3 = 0.37

          !Partitioning coefficients
          if (Sed_rchpara(1).KdNH3 <= 0.) Sed_rchpara(1).KdNH3 = 1
          if (Sed_rchpara(1).KdPO42 <= 0.) Sed_rchpara(1).KdPO42 = 20
          if (Sed_rchpara(1).dKDPO41f <= 0.) Sed_rchpara(1).dKDPO41f = 20
          if (Sed_rchpara(1).dKDPO41s <= 0.) Sed_rchpara(1).dKDPO41s = 20
          if (Sed_rchpara(1).O2critPO4 <= 0.) Sed_rchpara(1).O2critPO4 = 2

          !Temperature coefficients
          if (Sed_rchpara(1).ThtaDp <= 0.) Sed_rchpara(1).ThtaDp = 1.117
          if (Sed_rchpara(1).ThtaDd <= 0.) Sed_rchpara(1).ThtaDd = 1.08
          if (Sed_rchpara(1).ThtaKmNH3 <= 0.) Sed_rchpara(1).ThtaKmNH3 = 1.125
          if (Sed_rchpara(1).ThtaNH3 <= 0.) Sed_rchpara(1).ThtaNH3 = 1.123
          if (Sed_rchpara(1).ThtaNO3 <= 0.) Sed_rchpara(1).ThtaNO3 = 1.08
          if (Sed_rchpara(1).ThtaCH4 <= 0.) Sed_rchpara(1).ThtaCH4 = 1.079

          !Salinity thresholds
          if (Sed_rchpara(1).SALTSW <= 0.) Sed_rchpara(1).SALTSW = 1
          if (Sed_rchpara(1).SALTND <= 0.) Sed_rchpara(1).SALTND = 1

          !Sulfide constants
          if (Sed_rchpara(1).KappaH2Sd1 <= 0.) Sed_rchpara(1).KappaH2Sd1 = 0.2
          if (Sed_rchpara(1).KappaH2Sp1 <= 0.) Sed_rchpara(1).KappaH2Sp1 = 0.4
          if (Sed_rchpara(1).ThtaH2S <= 0.) Sed_rchpara(1).ThtaH2S = 1.079
          if (Sed_rchpara(1).KMHSO2 <= 0.) Sed_rchpara(1).KMHSO2 = 4
          if (Sed_rchpara(1).KdH2S1 <= 0.) Sed_rchpara(1).KdH2S1 = 100
          if (Sed_rchpara(1).KdH2S2 <= 0.) Sed_rchpara(1).KdH2S2 = 100


          !Fractions of G classes 1 and 2 for settling PON, POC, and POP	
          if (Sed_rchpara(1).frpon1 <= 0.) Sed_rchpara(1).frpon1 = 0.65
          if (Sed_rchpara(1).frpon2 <= 0.) Sed_rchpara(1).frpon2 = 0.25
          if (Sed_rchpara(1).frpoc1 <= 0.) Sed_rchpara(1).frpoc1 = 0.65
          if (Sed_rchpara(1).frpoc2 <= 0.) Sed_rchpara(1).frpoc2 = 0.2
          if (Sed_rchpara(1).frpop1 <= 0.) Sed_rchpara(1).frpop1 = 0.65
          if (Sed_rchpara(1).frpop2 <= 0.) Sed_rchpara(1).frpop2 = 0.2	


          !Diagenesis rate constants for G clase 1, 2, and 3 N/C/P	
          if (Sed_rchpara(1).kpon1 <= 0.) Sed_rchpara(1).kpon1 = 0.035
          if (Sed_rchpara(1).kpon2 <= 0.) Sed_rchpara(1).kpon2 = 0.0018
          if (Sed_rchpara(1).kpon3 <= 0.) Sed_rchpara(1).kpon3 = 0
    
          if (Sed_rchpara(1).kpoc1 <= 0.) Sed_rchpara(1).kpoc1 = 0.035 
          if (Sed_rchpara(1).kpoc2 <= 0.) Sed_rchpara(1).kpoc2 = 0.0018
          if (Sed_rchpara(1).kpoc3 <= 0.) Sed_rchpara(1).kpoc3 = 0
          
          if (Sed_rchpara(1).kpop1 <= 0.) Sed_rchpara(1).kpop1 = 0.035 
          if (Sed_rchpara(1).kpop2 <= 0.) Sed_rchpara(1).kpop2 = 0.0018
          if (Sed_rchpara(1).kpop3 <= 0.) Sed_rchpara(1).kpop3 = 0
                    
          !Sediment nutrient fluxes and sediment oxygen demand 
          if (Sed_rchpara(1).ThtaPON1 <= 0.) Sed_rchpara(1).ThtaPON1 = 1.1
          if (Sed_rchpara(1).ThtaPON2 <= 0.) Sed_rchpara(1).ThtaPON2 = 1.15
          if (Sed_rchpara(1).ThtaPON3 <= 0.) Sed_rchpara(1).ThtaPON3 = 1.17
    
          if (Sed_rchpara(1).ThtaPOC1 <= 0.) Sed_rchpara(1).ThtaPOC1 = 1.1
          if (Sed_rchpara(1).ThtaPOC2 <= 0.) Sed_rchpara(1).ThtaPOC2 = 1.15
          if (Sed_rchpara(1).ThtaPOC3 <= 0.) Sed_rchpara(1).ThtaPOC3 = 1.17
          
          if (Sed_rchpara(1).ThtaPOP1 <= 0.) Sed_rchpara(1).ThtaPOP1 = 1.1
          if (Sed_rchpara(1).ThtaPOP2 <= 0.) Sed_rchpara(1).ThtaPOP2 = 1.15
          if (Sed_rchpara(1).ThtaPOP3 <= 0.) Sed_rchpara(1).ThtaPOP3 = 1.17
       
          
          !Parameters for partical mixing and benthic stress
          if (Sed_rchpara(1).POC1R <= 0.) Sed_rchpara(1).POC1R = 0.2667
          if (Sed_rchpara(1).kBEN_STR <= 0.) Sed_rchpara(1).kBEN_STR = 0.03
          if (Sed_rchpara(1).KM_O2_Dp <= 0.) Sed_rchpara(1).KM_O2_Dp = 4



          !Parameters for silica
          if (Sed_rchpara(1).KSI <= 0.) Sed_rchpara(1).KSI = 0.5
          if (Sed_rchpara(1).THTASI <= 0.) Sed_rchpara(1).THTASI = 1.1
          if (Sed_rchpara(1).CSISAT <= 0.) Sed_rchpara(1).CSISAT = 40
          if (Sed_rchpara(1).THTASISAT <= 0.) Sed_rchpara(1).THTASISAT = 1.023         
          
          if (Sed_rchpara(1).DPIE1SI <= 0.) Sed_rchpara(1).DPIE1SI = 10
          if (Sed_rchpara(1).PIE2SI <= 0.) Sed_rchpara(1).PIE2SI = 100
          if (Sed_rchpara(1).KMPSI <= 0.) Sed_rchpara(1).KMPSI = 50000
          if (Sed_rchpara(1).O2CRITSI <= 0.) Sed_rchpara(1).O2CRITSI = 2

          !Particulate organic C, N, and P in layer 2
          if (Sed_rchpara(1).POC2_1 <= 0.) Sed_rchpara(1).POC2_1 = 115.2384431
          if (Sed_rchpara(1).POC2_2 <= 0.) Sed_rchpara(1).POC2_2 = 1047.173118
          if (Sed_rchpara(1).POC2_3 <= 0.) Sed_rchpara(1).POC2_3 = 2046.350365
          if (Sed_rchpara(1).PON2_1 <= 0.) Sed_rchpara(1).PON2_1 = 63.87748906
          if (Sed_rchpara(1).PON2_2 <= 0.) Sed_rchpara(1).PON2_2 = 725.569389
          if (Sed_rchpara(1).PON2_3 <= 0.) Sed_rchpara(1).PON2_3 = 756.2043796
          
          if (Sed_rchpara(1).POP2_1 <= 0.) Sed_rchpara(1).POP2_1 = 0.92486712
          if (Sed_rchpara(1).POP2_2 <= 0.) Sed_rchpara(1).POP2_2 = 8.404278637
          if (Sed_rchpara(1).POP2_3 <= 0.) Sed_rchpara(1).POP2_3 = 16.42335766
          if (Sed_rchpara(1).POS2 <= 0.) Sed_rchpara(1).POS2 = 2898.096108        
          
          !Dissolved constituents in layer 1 and 2 porewater
          if (Sed_rchpara(1).NH3_1 <= 0.) Sed_rchpara(1).NH3_1 = 0.562033649
          if (Sed_rchpara(1).NH3_2 <= 0.) Sed_rchpara(1).NH3_2 = 4.376815752
          if (Sed_rchpara(1).NO3_1 <= 0.) Sed_rchpara(1).NO3_1 = 0.228998474
          if (Sed_rchpara(1).NO3_2 <= 0.) Sed_rchpara(1).NO3_2 = 0.038186719
          if (Sed_rchpara(1).PO4_1 <= 0.) Sed_rchpara(1).PO4_1 = 0.077450401
          if (Sed_rchpara(1).PO4_2 <= 0.) Sed_rchpara(1).PO4_2 = 0.292541657

          if (Sed_rchpara(1).Si_1 <= 0.) Sed_rchpara(1).Si_1 = 1.483185229
          if (Sed_rchpara(1).Si_2 <= 0.) Sed_rchpara(1).Si_2 = 8.27812963
          if (Sed_rchpara(1).HS_1 <= 0.) Sed_rchpara(1).HS_1 = 0.
          if (Sed_rchpara(1).HS_2 <= 0.) Sed_rchpara(1).HS_2 = 0.
          if (Sed_rchpara(1).CH4_1 <= 0.) Sed_rchpara(1).CH4_1 = 0.
          if (Sed_rchpara(1).CH4_2 <= 0.) Sed_rchpara(1).CH4_2 = 0.
          
          
          
      do counter = 2, mch, 1 ![,step] 
        Sed_rchpara(counter) = Sed_rchpara(1)      
      end do   

      do counter = 1, mhru
        Sed_wetpara(counter) = Sed_rchpara(1)
        sed_pndpara(counter) = Sed_rchpara(1)
      end do
      
      do counter = 1, mres
        sed_respara(counter) = sed_rchpara(1)
      end do

      
      close (readCarbonSedFlux_num)
      
      
  
      
      
      return
 5100 format (a)
      end