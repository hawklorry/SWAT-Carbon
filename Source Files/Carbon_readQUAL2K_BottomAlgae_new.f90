      subroutine readBottomAlgae

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

      open (readBottomAlgae_num,file='basins_bottom.alg')  
        
      do
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      !parameter for bottom algae photosynthesis
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%ipho
      if (eof < 0) exit      
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kph 
      if (eof < 0) exit       
      
      !For nutrient limitation
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%pmN
      if (eof < 0) exit          
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%pmP
      if (eof < 0) exit    
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%KqN
      if (eof < 0) exit 
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%KqP
      if (eof < 0) exit 
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%Ksnb
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%Kspb
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%qoN
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%qoP
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kexb
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kscb
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%fdic
      if (eof < 0) exit      
      
      !For light limitation            
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit     
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%keb
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kiss
      if (eof < 0) exit        
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kpom
      if (eof < 0) exit      
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kap
      if (eof < 0) exit
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kapn
      if (eof < 0) exit     
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kAb
      if (eof < 0) exit   
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%light
      if (eof < 0) exit   
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%klb
      if (eof < 0) exit   
      
      
      !For space limitation factor      
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit      
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%abmax
      if (eof < 0) exit 
      
      !parameters for respiration and death process    
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit   
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%kdb
      if (eof < 0) exit      
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%krb1
      if (eof < 0) exit    
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%krb2
      if (eof < 0) exit      
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%ksob
      if (eof < 0) exit      
    
      !Initial condition
      read (readBottomAlgae_num,5100,iostat=eof) titldum
      if (eof < 0) exit   
      read (readBottomAlgae_num,*,iostat=eof) Ab_rchpara(1)%Ab_ini
      if (eof < 0) exit  
      
         
      exit
      end do

!!    set default values for undefined parameters
      !parameter for bottom algae photosynthesis
      if (Ab_rchpara(1)%ipho <= 0.) Ab_rchpara(1)%ipho = 1
      if (Ab_rchpara(1)%kph <= 0.) Ab_rchpara(1)%kph = 36.2
      
      !For nutrient limitation
      if (Ab_rchpara(1)%pmN <= 0.) Ab_rchpara(1)%pmN = 284.7
      if (Ab_rchpara(1)%pmP <= 0.) Ab_rchpara(1)%pmP = 37.85
      if (Ab_rchpara(1)%KqN <= 0.) Ab_rchpara(1)%KqN = 2.54
      if (Ab_rchpara(1)%KqP <= 0.) Ab_rchpara(1)%KqP = 4.93
      if (Ab_rchpara(1)%Ksnb <= 0.) Ab_rchpara(1)%Ksnb = 0.07
      if (Ab_rchpara(1)%Kspb <= 0.) Ab_rchpara(1)%Kspb = 0.098
      if (Ab_rchpara(1)%qoN <= 0.) Ab_rchpara(1)%qoN = 4.17
      if (Ab_rchpara(1)%qoP <= 0.) Ab_rchpara(1)%qoP = 1.93
      if (Ab_rchpara(1)%kexb <= 0.) Ab_rchpara(1)%kexb = 0.21
      if (Ab_rchpara(1)%kscb <= 0.) Ab_rchpara(1)%kscb = 0.792
      if (Ab_rchpara(1)%fdic <= 0.) Ab_rchpara(1)%fdic = 0.6
      
      !For light limitation            
      if (Ab_rchpara(1)%keb <= 0.) Ab_rchpara(1)%keb = 0.02
      if (Ab_rchpara(1)%kiss <= 0.) Ab_rchpara(1)%kiss = 0.052
      if (Ab_rchpara(1)%kpom <= 0.) Ab_rchpara(1)%kpom = 0.174
      if (Ab_rchpara(1)%kap <= 0.) Ab_rchpara(1)%kap = 0.0088
      if (Ab_rchpara(1)%kapn <= 0.) Ab_rchpara(1)%kapn = 0.054
      if (Ab_rchpara(1)%kAb <= 0.) Ab_rchpara(1)%kAb = 0.024
      if (Ab_rchpara(1)%light <= 0.) Ab_rchpara(1)%light = 1
      if (Ab_rchpara(1)%klb <= 0.) Ab_rchpara(1)%klb = 1.5807      
      
      !For space limitation factor      
      if (Ab_rchpara(1)%abmax <= 0.) Ab_rchpara(1)%abmax = 200.
      
      !parameters for respiration and death process    
      if (Ab_rchpara(1)%kdb <= 0.) Ab_rchpara(1)%kdb = 0.02
      if (Ab_rchpara(1)%krb1 <= 0.) Ab_rchpara(1)%krb1 = 0.042
      if (Ab_rchpara(1)%krb2 <= 0.) Ab_rchpara(1)%krb2 = 0.389
      if (Ab_rchpara(1)%ksob <= 0.) Ab_rchpara(1)%ksob = 0.6

      if (Ab_rchpara(1)%Ab_ini <=0.) Ab_rchpara(1)%Ab_ini = 0.
      
      do counter = 2, mch, 1 ![,step]    
          !parameter for bottom algae photosynthesis
          Ab_rchpara(counter)%ipho = Ab_rchpara(1)%ipho
          Ab_rchpara(counter)%kph = Ab_rchpara(1)%kph
          
          !For nutrient limitation
          Ab_rchpara(counter)%pmN = Ab_rchpara(1)%pmN
          Ab_rchpara(counter)%pmP = Ab_rchpara(1)%pmP
          Ab_rchpara(counter)%KqN = Ab_rchpara(1)%KqN
          Ab_rchpara(counter)%KqP = Ab_rchpara(1)%KqP
          Ab_rchpara(counter)%Ksnb = Ab_rchpara(1)%Ksnb
          Ab_rchpara(counter)%Kspb = Ab_rchpara(1)%Kspb
          Ab_rchpara(counter)%qoN = Ab_rchpara(1)%qoN
          Ab_rchpara(counter)%qoP = Ab_rchpara(1)%qoP
          Ab_rchpara(counter)%kexb = Ab_rchpara(1)%kexb
          Ab_rchpara(counter)%kscb = Ab_rchpara(1)%kscb
          Ab_rchpara(counter)%fdic = Ab_rchpara(1)%fdic
          
          !For light limitation            
          Ab_rchpara(counter)%keb = Ab_rchpara(1)%keb
          Ab_rchpara(counter)%kiss = Ab_rchpara(1)%kiss
          Ab_rchpara(counter)%kpom = Ab_rchpara(1)%kpom
          Ab_rchpara(counter)%kap = Ab_rchpara(1)%kap
          Ab_rchpara(counter)%kapn = Ab_rchpara(1)%kapn
          Ab_rchpara(counter)%kAb = Ab_rchpara(1)%kAb
          Ab_rchpara(counter)%light = Ab_rchpara(1)%light
          Ab_rchpara(counter)%klb = Ab_rchpara(1)%klb
          
          
          !For space limitation factor      
          Ab_rchpara(counter)%abmax = Ab_rchpara(1)%abmax
          
          !parameters for respiration and death process    
          Ab_rchpara(counter)%kdb = Ab_rchpara(1)%kdb
          Ab_rchpara(counter)%krb1 = Ab_rchpara(1)%krb1
          Ab_rchpara(counter)%krb2 = Ab_rchpara(1)%krb2
          Ab_rchpara(counter)%ksob = Ab_rchpara(1)%ksob
        
          Ab_rchpara(counter)%Ab_ini = Ab_rchpara(1)%Ab_ini
      end do

      do counter = 1, mhru
        Ab_wetpara(counter) = Ab_rchpara(1)
        Ab_pndpara(counter) = Ab_rchpara(1)
      
      end do
      
      do counter = 1, mres
        Ab_respara(counter) = Ab_rchpara(1)
      end do
 
      close (readBottomAlgae_num)
      return
 5100 format (a)
      end