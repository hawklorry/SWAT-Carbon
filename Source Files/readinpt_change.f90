      subroutine readinpt

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls subroutines which read input data for the 
!!    databases and the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nhru          |none          |total number of HRUs in the watershed
!!    npmx          |none          |total number of pesticides modeled in
!!                                 |in watershed plus 1
!!    nope(:)       |none          |sequence number of pesticide in NPNO(:)
!!    pstflg(:)     |none          |array defining different pesticides
!!                                 |used in the watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |number of specific reservoir or HRU
!!    irtpest       |none          |redefined to the sequence number of 
!!                                 |pestcide in NPNO(:) which is to be routed 
!!                                 |through the watershed
!!    ndays(:)      |julian date   |julian date for last day of preceding 
!!                                 |month (where the array location is the 
!!                                 |number of the month) The dates are for
!!                                 |leap years
!!    npmx          |none          |total number of pesticides modeled in
!!                                 |watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: soil_chem, soil_phys, rteinit, h2omgt_init, hydro_init,
!!    impnd_init

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      
      implicit none

!! By Zhang for C/N cycling 
!!==============================  
      !initilizaing several soil parameters
      sol_WOC = 0.
      sol_WON = 0.
!! By Zhang for C/N cycling  
!!=============================

      if (irtpest > 0) irtpest = nope(irtpest)
      npmx = 0
      npmx = Sum(pstflg)         !! set equal to # pesticides modeled in
                                 !! watershed

      do i = 1, nhru
        !call soil_par
      call soil_chem           !! initialize soil chemical parameters
      call soil_phys           !! initialize soil physical parameters 
      end do
      call Soil_Texture       
      call rteinit               !! initialize routing parameters
      call h2omgt_init           !! initialize water management parameters
      call hydroinit             !! initialize hydrology parameters
      call impnd_init            !! initialize impoundment parameters
       
      call carbon_cal   
   
      if (ico2==1) call readco2               !! read co2 concentration   ---------------qichun co2----------------------------------------- 
      return
      end