      subroutine readCarbonSoil

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
      implicit none
      
      character (len=80) :: titldum
      integer :: eof, counter

      eof = 0

      open (readCarbonSoil_num,file='basins_carbon.tes')  
        
      do
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit

      !Miscellaneous
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readCarbonSoil_num,*,iostat=eof) er_POC_para 
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) CFB_para
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) Sf_para_sur
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) Sf_para_sub
      if (eof < 0) exit   
      
      !Dissovled carbon
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readCarbonSoil_num,5100,iostat=eof) titldum         !!Skip ABL_para             
      if (eof < 0) exit   
      read (readCarbonSoil_num,*,iostat=eof) peroc_DIC_para
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) peroc_DOC_para 
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) part_DOC_para 
      if (eof < 0) exit 
      read (readCarbonSoil_num,*,iostat=eof) hlife_doc_para
      if (eof < 0) exit       
      
      !Allocation of CO2 and Carbon transformation
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) ABCO2_para_sur 
      if (eof < 0) exit 
      read (readCarbonSoil_num,5100,iostat=eof) titldum         !!Skip ABCO2_para_sub 
      if (eof < 0) exit 
      read (readCarbonSoil_num,*,iostat=eof) ABP_para_sur 
      if (eof < 0) exit       
      read (readCarbonSoil_num,5100,iostat=eof) titldum         !!Skip ABP_para_sub
      if (eof < 0) exit 
      read (readCarbonSoil_num,*,iostat=eof) ALMCO2_para_sur
      if (eof < 0) exit 
      read (readCarbonSoil_num,*,iostat=eof) ALMCO2_para_sub
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) ALSLNCO2_para_sur 
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) ALSLNCO2_para_sub
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) ASP_para_sur
      if (eof < 0) exit 
      read (readCarbonSoil_num,5100,iostat=eof) titldum         !!ASP_para_sub
      if (eof < 0) exit 
      read (readCarbonSoil_num,*,iostat=eof) ALSLCO2_para 
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) APCO2_para
      if (eof < 0) exit       
      read (readCarbonSoil_num,*,iostat=eof) ASCO2_para
      if (eof < 0) exit      

      !decomposition rates
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readCarbonSoil_num,*,iostat=eof) PRMT_51_para
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) PRMT_45_para 
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) BMR_para_sur 
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) BMR_para_sub
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) HPR_para 
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) HSR_para 
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) LMR_para_sur
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) LMR_para_sub
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) LSR_para_sur
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) LSR_para_sub
      if (eof < 0) exit      
            
      !Soil texutre controls of microbioa activity
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readCarbonSoil_num,*,iostat=eof) XBM_para_sur
      if (eof < 0) exit      
      read (readCarbonSoil_num,5100,iostat=eof) titldum         !XBM_para_sub 
      if (eof < 0) exit
      read (readCarbonSoil_num,5100,iostat=eof) titldum         !XLSLF_para 
      if (eof < 0) exit
    
      
      !Oxygen factor control parameters
      read (readCarbonSoil_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readCarbonSoil_num,*,iostat=eof) OX_aa_para
      if (eof < 0) exit      
      read (readCarbonSoil_num,*,iostat=eof) OX_bb_para
      if (eof < 0) exit          
      
      
      !read (readCarbonSoil_num,*,iostat=eof) er_POC_para 
      !if (eof < 0) exit      
      !read (readCarbonSoil_num,*,iostat=eof) er_POC_para 
      !if (eof < 0) exit      
      !read (readCarbonSoil_num,*,iostat=eof) er_POC_para 
      !if (eof < 0) exit      
      exit
      end do

!!    set default values for undefined parameters

      !Miscellaneous
      if (er_POC_para  <= 0.) er_POC_para  = 1.5
      if (CFB_para <= 0.) CFB_para = 0.42
      if (Sf_para_sur <= 0.) Sf_para_sur = 0.05
      if (Sf_para_sub <= 0.) Sf_para_sub = 0.10
   
      
      !Dissovled carbon
      ABL_para = 0.0
      if (peroc_DIC_para <= 0.) peroc_DIC_para = 0.95
      if (peroc_DOC_para  <= 0.) peroc_DOC_para = 0.70
      if (part_DOC_para <=0.) part_DOC_para  = 4000
      if (hlife_doc_para <= 0.) hlife_doc_para = 50.0
      
      !Allocation of CO2 and Carbon transformation
      if (ABCO2_para_sur <= 0.) ABCO2_para_sur  = 0.6    
      ABCO2_para_sub = 0.
      if (ABP_para_sur  <= 0.) ABP_para_sur   = 0.0    
      ABP_para_sub = 0.
      if (ALMCO2_para_sur <= 0.) ALMCO2_para_sur = 0.6
      if (ALMCO2_para_sub <= 0.) ALMCO2_para_sub = 0.55
      if (ALSLNCO2_para_sur <= 0.) ALSLNCO2_para_sur = 0.6
      if (ALSLNCO2_para_sur <= 0.) ALSLNCO2_para_sur = 0.55
      if (ASP_para_sur <= 0.) ASP_para_sur = 0.0
      ASP_para_sub = 0.0
      if (ALSLCO2_para <= 0.) ALSLCO2_para = 0.3
      if (APCO2_para <= 0.) APCO2_para = 0.55
      if (ASCO2_para <= 0.) ASCO2_para = 0.55

      !decomposition rates
      if (PRMT_51_para <= 0.) PRMT_51_para = 1.0
      if (PRMT_45_para <= 0.) PRMT_45_para = 0.003
      if (BMR_para_sur <= 0.) BMR_para_sur = 0.0164
      if (BMR_para_sub <= 0.) BMR_para_sub = 0.02
      if (HPR_para <= 0.) HPR_para = 0.000012
      if (HSR_para <= 0.) HSR_para = 0.000548
      if (LMR_para_sur <= 0.) LMR_para_sur = 0.0405
      if (LMR_para_sub <= 0.) LMR_para_sub = 0.0507
      if (LSR_para_sur <= 0.) LSR_para_sur = 0.0107
      if (LSR_para_sub <= 0.) LSR_para_sub = 0.0132
            
      !Soil texutre controls of microbioa activity
      if (XBM_para_sur <= 0.) XBM_para_sur = 1.0
      XBM_para_sub = 0.0
      XLSLF_para = 0.0

      !Oxygen factor control parameters
      if (OX_aa_para <= 0.) OX_aa_para = 10.0
      if (OX_bb_para <= 0.) OX_bb_para = 0.035

      
      close (readCarbonSoil_num)
      
      
      !Currently, the parameters set in cbn_sp in carbon_para.f90 are not used.
      !type carbon_soil_para
      !     real :: k_eva=0.1        !DIC evasion rate, DIC to air
      !     real :: DIC_sat=0.01     !DIC saturation constant (kg/m3),DIC to air
      !     real :: peroc_DIC=0.95   !DIC percolation coefficient
      !     real :: peroc_DOC=0.7    !DOC percolation coefficient
      !     real :: enr_POC =1.5     !POC enrichment ratio 
      !     real :: hlife_docgw=50.0   !DOC half life (days) in groudwater,calculating DOC decay in groundwater 
      !     real :: kd_OC=500.0  !organic carbon partition coefficient,basin sacle parameter,1000 to 1200
      !end type carbon_soil_para
      !type (carbon_soil_para),dimension(:), allocatable:: cbn_sp !carbon parameters in soil layers      
      
      
      return
 5100 format (a)
      end