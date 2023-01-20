      subroutine readEnergyPara

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
        use parm_subE
      implicit none
      
      character (len=80) :: titldum
      integer :: eof, counter, counter1

      eof = 0

      open (readEnergyPara_num,file='basins_energy.bal')  
        
      do
      read (readEnergyPara_num,5100,iostat=eof) titldum
      if (eof < 0) exit

      !Soil temperature parameters
      read (readEnergyPara_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readEnergyPara_num,*,iostat=eof) eff_coe(1)
      if (eof < 0) exit      
      read (readEnergyPara_num,*,iostat=eof) k_coe(1,1)
      if (eof < 0) exit       
      read (readEnergyPara_num,*,iostat=eof) ks_coe(1)
      if (eof < 0) exit      
      read (readEnergyPara_num,*,iostat=eof) c_coe(1)
      if (eof < 0) exit   
      
      !Stream water temperature parameters
      read (readEnergyPara_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readEnergyPara_num,*,iostat=eof) wtmp_add_para
      if (eof < 0) exit      
      exit
      end do

!!    set default values for undefined parameters

      !Soil temperature parameters
      if (eff_coe(1)  <= 0.) eff_coe(1)  = 50
      if (k_coe(1,1) <= 0.) k_coe(1,1) = 10
      if (ks_coe(1) <= 0.) ks_coe(1) = 1
      if (c_coe(1) <= 0.) c_coe(1) = 1
   
      
      !Stream water temperature parameters
      if (wtmp_add_para <= 0.) wtmp_add_para = 0

      do counter = 1, mhru
        eff_coe(counter) = eff_coe(1)
        ks_coe(counter) = ks_coe(1)
        c_coe(counter) = c_coe(1)
        
        do counter1 = 1, mlyr+10
            k_coe(counter1,counter) = k_coe(1,1)
        end do         
      end do
      
      close (readEnergyPara_num)
      
      
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