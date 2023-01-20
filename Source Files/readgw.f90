      subroutine readgw

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the parameters from the HRU/subbasin groundwater
!!    input file (.gw)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    alpha_bf(:) |1/days        |alpha factor for groundwater recession curve
!!    alpha_bf_d(:) | 1/days     |alpha factor for groudwater recession curve of the deep aquifer
!!    alpha_bfe(:)|none          |Exp(-alpha_bf(:))
!!    alpha_bfe_d (:) |1/days    |Exp(-alpha_bf_d(:)) for deep aquifer
!!    ch_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from bank storage to the root
!!                               |zone as a result of soil moisture depletion
!!    deepst(i)   |mm H2O        |depth of water in deep aquifer
!!    delay(:)    |days          |groundwater delay: time required for water
!!                               |leaving the bottom of the root zone to 
!!                               |reach the shallow aquifer
!!    gw_delaye(:)|none          |Exp(-1./(delay(:))
!!    gw_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from the shallow aquifer to
!!                               |the root zone as a result of soil moisture
!!                               |depletion
!!    gw_spyld(:) |m**3/m**3     |specific yield for shallow aquifer
!!    gwht(:)     |m             |groundwater height
!!    gwminp(:)   |mg P/L        |soluble P concentration in groundwater
!!                               |loading to reach
!!    gwno3(:)    |mg N/L        |nitrate-N concentration in groundwater 
!!                               |loading to reach
!!    gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required before groundwater flow will occur
!!    rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of 
!!                               |root zone percolation that reaches the deep
!!                               |aquifer
!!    revapmn(:)  |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required to allow revap to occur
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    shallst_n(:)|ppm NO3-N     |nitrate concentration in shallow aquifer
!!                               |converted to kg/ha
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line for .gw file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp    

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
      
      character (len=80) :: titldum
      integer :: eof
      real :: hlife_ngw

      eof = 0
      hlife_ngw = 0.0

      do
      read (gwfile_num,5000) titldum
      read (gwfile_num,*) shallst(ihru)
      read (gwfile_num,*) deepst(ihru)
      read (gwfile_num,*) delay(ihru)
      read (gwfile_num,*) alpha_bf(ihru)
      read (gwfile_num,*) gwqmn(ihru)
      read (gwfile_num,*) gw_revap(ihru)
      read (gwfile_num,*) revapmn(ihru)
      read (gwfile_num,*) rchrg_dp(ihru)
      read (gwfile_num,*,iostat=eof) gwht(ihru)
      if (eof < 0) exit
      read (gwfile_num,*,iostat=eof) gw_spyld(ihru)
      if (eof < 0) exit
      read (gwfile_num,*,iostat=eof) shallst_n(ihru)
      shallst_n(ihru) = 5.
      if (eof < 0) exit
      read (gwfile_num,*,iostat=eof) gwminp(ihru)
      if (eof < 0) exit
      read (gwfile_num,*,iostat=eof) hlife_ngw
      if (eof < 0) exit
!! organic n and p in the lateral flow     - by J.Jeong BREC 2011
      read (gwfile_num,*,iostat=eof) lat_orgn(ihru)         
      if (eof < 0) exit
      read (gwfile_num,*,iostat=eof) lat_orgp(ihru)
      if (eof < 0) exit
      read (gwfile_num,*,iostat=eof) alpha_bf_d(ihru)
      exit
      end do
  
!!    set default values for mike van liew
      if (hlife_ngw <= 0.) hlife_ngw = hlife_ngw_bsn
!!    set default values for mike van liew

!!    set default values
      if (deepst(ihru) <= 0.) deepst(ihru) = 1000.
      !if (hlife_ngw <= 0.) hlife_ngw = 365.    
      if (lat_orgn(ihru) <= 1.e-6) lat_orgn(ihru) = 0.
      if (lat_orgp(ihru) <= 1.e-6) lat_orgp(ihru) = 0.

!!    perform additional calculations
      alpha_bfe(ihru) = Exp(-alpha_bf(ihru))
      if(delay(ihru) < .1) delay(ihru) = .1
	    gw_delaye(ihru) = Exp(-1./(delay(ihru) + 1.e-6))
      shallst_n(ihru) = shallst_n(ihru) * shallst(ihru) / 100.       
      gw_nloss(ihru) = Exp(-.693 / hlife_ngw)
      
!!    alpha baseflow factor for deep aquifer according to Yi Luo      
      alpha_bfe_d(ihru) = Exp(-alpha_bf_d(ihru))


!! assign values to channels
      ch_revap(i) = gw_revap(ihru)

!! assign values to channels
      ch_revap(i) = gw_revap(ihru)

      close (gwfile_num)
      return
 5000 format (a)
      end