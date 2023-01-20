      subroutine readCEQUAL

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
      implicit none
      
      character (len=80) :: titldum
      integer :: eof, counter

      eof = 0

      open (readCEQUAL_num,file='basins_cequal.wcp')  
        
      do
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      !LPOC parameters
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_lpp
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_lpb 
      if (eof < 0) exit       
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%rca
      if (eof < 0) exit          
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%sv_lp
      if (eof < 0) exit    
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%klp
      if (eof < 0) exit 
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kd_lp
      if (eof < 0) exit 
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%klrp
      if (eof < 0) exit
      !RPOC parameters
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit     
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_rpp
      if (eof < 0) exit
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_rpb
      if (eof < 0) exit        
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%sv_rp
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%krp
      if (eof < 0) exit
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kd_rp
      if (eof < 0) exit     
      !LDOC parameters
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_ldp
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_ldb
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kld
      if (eof < 0) exit    
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%ksdocf
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%klrd
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kdnit
      if (eof < 0) exit
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%ksoxdn
      if (eof < 0) exit      
      !RDOC parameters
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit     
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%krd
      if (eof < 0) exit
      !DIC parameters
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit     
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_co2
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%p_co2
      if (eof < 0) exit   
      !Sediment parameters
      read (readCEQUAL_num,5100,iostat=eof) titldum
      if (eof < 0) exit     
      read (readCEQUAL_num,*,iostat=eof) scbn_rchpara(1)%ksed
      if (eof < 0) exit      
      read (readCEQUAL_num,*,iostat=eof) scbn_rchpara(1)%kbur
      if (eof < 0) exit         
  
      exit
      end do

!!    set default values for undefined parameters


        !LPOC parameters
      if (cbn_rchpara(1)%f_lpp <= 0.) cbn_rchpara(1)%f_lpp = 0.1 

      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_lpb 
      if (cbn_rchpara(1)%f_lpb <= 0.) cbn_rchpara(1)%f_lpb  = 0.1 
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%rca
      if (cbn_rchpara(1)%rca <= 0.) cbn_rchpara(1)%rca = 0.4
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%sv_lp
      if (cbn_rchpara(1)%sv_lp <= 0.) cbn_rchpara(1)%sv_lp = 2.5
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%klp
      if (cbn_rchpara(1)%klp <= 0.) cbn_rchpara(1)%klp = 0.075
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kd_lp
      if (cbn_rchpara(1)%kd_lp <= 0.) cbn_rchpara(1)%kd_lp = 0.08
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%klrp
      if (cbn_rchpara(1)%klrp <= 0.) cbn_rchpara(1)%klrp = 0.01 
      
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_rpp      
      if (cbn_rchpara(1)%f_rpp <= 0.) cbn_rchpara(1)%f_rpp  = 0.8 
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_rpb
      if (cbn_rchpara(1)%f_rpb <= 0.) cbn_rchpara(1)%f_rpb = 0.8
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%sv_rp
      if (cbn_rchpara(1)%sv_rp <= 0.) cbn_rchpara(1)%sv_rp = 2.5 
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%krp
      if (cbn_rchpara(1)%krp <= 0.) cbn_rchpara(1)%krp = 0.0025 
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kd_rp
      if (cbn_rchpara(1)%kd_rp <= 0.) cbn_rchpara(1)%kd_rp = 0.001 
      
      !read (readCEQUAL_num,5100,iostat=eof) titldum
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_ldp
      if (cbn_rchpara(1)%f_ldp <= 0.) cbn_rchpara(1)%f_ldp = 0.05
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_ldb
      if (cbn_rchpara(1)%f_ldb <= 0.) cbn_rchpara(1)%f_ldb = 0.05 
      if (cbn_rchpara(1)%kld <= 0.) cbn_rchpara(1)%kld = 0.25
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%ksdocf
      if (cbn_rchpara(1)%ksdocf <= 0.) cbn_rchpara(1)%ksdocf = 1.0
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%klrd
      if (cbn_rchpara(1)%klrd <= 0.) cbn_rchpara(1)%klrd = 0.01
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%kdnit
      if (cbn_rchpara(1)%kdnit <= 0.) cbn_rchpara(1)%kdnit = 0.002 
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%ksoxdn
      if (cbn_rchpara(1)%ksoxdn <= 0.) cbn_rchpara(1)%ksoxdn = 0.1
      
      !read (readCEQUAL_num,5100,iostat=eof) titldum
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%krd
      if (cbn_rchpara(1)%krd <= 0.) cbn_rchpara(1)%krd = 0.006 
      
      !read (readCEQUAL_num,5100,iostat=eof) titldum
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%f_co2
      if (cbn_rchpara(1)%f_co2 <= 0.) cbn_rchpara(1)%f_co2 = 0.2 
      !read (readCEQUAL_num,*,iostat=eof) cbn_rchpara(1)%p_co2
      if (cbn_rchpara(1)%p_co2 <= 0.) cbn_rchpara(1)%p_co2 = 391.0 

      if (scbn_rchpara(1)%ksed <= 0.) scbn_rchpara(1)%ksed = 0.02 
      if (scbn_rchpara(1)%kbur <= 0.) scbn_rchpara(1)%kbur = 0.05

      
      do counter = 2, mch, 1 ![,step]    
            cbn_rchpara(counter)%f_lpp = cbn_rchpara(1)%f_lpp 

            cbn_rchpara(counter)%f_lpb = cbn_rchpara(1)%f_lpb
            cbn_rchpara(counter)%rca = cbn_rchpara(1)%rca
            cbn_rchpara(counter)%sv_lp = cbn_rchpara(1)%sv_lp
            cbn_rchpara(counter)%klp = cbn_rchpara(1)%klp
            cbn_rchpara(counter)%kd_lp = cbn_rchpara(1)%kd_lp
            cbn_rchpara(counter)%klrp = cbn_rchpara(1)%klrp 
      
            cbn_rchpara(counter)%f_rpp = cbn_rchpara(1)%f_rpp 
            cbn_rchpara(counter)%f_rpb = cbn_rchpara(1)%f_rpb
            cbn_rchpara(counter)%sv_rp = cbn_rchpara(1)%sv_rp
            cbn_rchpara(counter)%krp = cbn_rchpara(1)%krp  
            cbn_rchpara(counter)%kd_rp = cbn_rchpara(1)%kd_rp
      
            cbn_rchpara(counter)%f_ldp = cbn_rchpara(1)%f_ldp
            cbn_rchpara(counter)%f_ldb = cbn_rchpara(1)%f_ldb  
            cbn_rchpara(counter)%kld = cbn_rchpara(1)%kld
            cbn_rchpara(counter)%ksdocf = cbn_rchpara(1)%ksdocf 
            cbn_rchpara(counter)%klrd = cbn_rchpara(1)%klrd
            cbn_rchpara(counter)%kdnit = cbn_rchpara(1)%kdnit 
            cbn_rchpara(counter)%ksoxdn = cbn_rchpara(1)%ksoxdn
      
            cbn_rchpara(counter)%krd = cbn_rchpara(1)%krd 
      
            cbn_rchpara(counter)%f_co2 = cbn_rchpara(1)%f_co2 
            cbn_rchpara(counter)%p_co2 = cbn_rchpara(1)%p_co2

            scbn_rchpara(counter)%ksed = scbn_rchpara(1)%ksed
            scbn_rchpara(counter)%kbur = scbn_rchpara(1)%kbur
      end do
      
      do counter = 1, mhru
        cbn_wetpara(counter) = cbn_rchpara(1)
        cbn_pndpara(counter) = cbn_rchpara(1)
        
        scbn_wetpara(counter) = scbn_rchpara(1)
        scbn_pndpara(counter) = scbn_rchpara(1)
      end do
      
      do counter = 1, mres
        cbn_respara(counter) = cbn_rchpara(1)
        scbn_respara(counter) = scbn_rchpara(1)        
      end do
 
      close (readCEQUAL_num)
      return
 5100 format (a)
      end