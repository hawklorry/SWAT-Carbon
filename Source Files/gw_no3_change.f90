      subroutine gw_no3
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates groundwater contribution to
!!    streamflow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bf(:) |1/days        |alpha factor for groundwater recession curve
!!    alpha_bfe(:)|none          |Exp(-alpha_bf(:))
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    ihru        |none          |HRU number
!!    gw_delaye(:)|none          |Exp(-1./(delay(:)) where delay(:) is the 
!!                               |groundwater delay (time required for water
!!                               |leaving the bottom of the root zone to reach
!!                               |the shallow aquifer; units-days)
!!    gw_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from the shallow aquifer to
!!                               |the root zone as a result of soil moisture
!!                               |depletion
!!    gw_spyld(:) |m**3/m**3     |specific yield for shallow aquifer
!!    gwht(:)     |m             |groundwater height
!!    gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required before groundwater flow will occur
!!    pet_day     |mm H2O        |potential evapotranspiration on current day
!!                               |in HRU
!!    rchrg(:)    |mm H2O        |amount of water entering shallow aquifer on
!!                               |previous day in HRU
!!    rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of
!!                               |root zone percolation that reaches the deep
!!                               |aquifer
!!    revapmn(:)  |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required to allow revap to occur
!!    sepbtm      |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    gw_q(:)     |mm H2O        |groundwater contribution to streamflow from
!!                               |HRU on current day
!!    gwht(:)     |m             |groundwater height
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day in HRU
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow 
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    rchrg1      |mm H2O        |amount of water entering shallow aquifer on
!!                               |previous day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!    revap is subtracted and rchrg is delayed (johnson, 1977)

      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
        
      implicit none
      
      integer :: j
      real :: rchrgn1,xx

      j = 0
      j = ihru

      rchrgn1 = 0.
      rchrgn1 = rchrg_n(j)
      if (rchrgn1 < 1.e-6) rchrgn1 = 0.0

!! compute nitrate aquifer loading from recharge for current day
      rchrg_n(j) = 0.
      rchrg_n(j) = (1.- gw_delaye(j)) * percn(j) + gw_delaye(j) * rchrgn1
      							
      shallst_n(j) = shallst_n(j) + rchrg_n(j)

      if (shallst_n(j) < 1.e-6) shallst_n(j) = 0.0
      if (shallst(j) < 1.e-6) shallst(j) = 0.0
      if (gw_q(j) < 1.e-6) gw_q(j) = 0.0
      if (revapday < 1.e-6) revapday = 0.0
      if (gwseep < 1.e-6) gwseep = 0.0

!! compute nitrate groundwater contribution to streamflow for day
      xx = shallst(j) + gw_q(j) + revapday + gwseep 
      if (xx > 1.) then
        xx = shallst_n(j) / (shallst(j) + gw_q(j) + revapday + gwseep) 
      else
        xx = 0.
      end if
      if (xx < 1.e-6) xx = 0.0
      no3gw(j) = xx * gw_q(j)
      !! bmp adjustment
      !no3gw(j) = no3gw(j) * bmp_sns(j)             !!R682 10/20/21 nbs

      revapn(j) = xx * revapday
      gwseepn(j) = xx * gwseep

      revapn(j) = amax1(1.e-6,revapn(j))
      gwseepn(j) = amax1(1.e-6,gwseepn(j))

!! subtract nitrate losses from the shallow aquifer
      shallst_n(j) = shallst_n(j) - no3gw(j) - revapn(j) - gwseepn(j)
      shallst_n(j) = amax1 (0., shallst_n(j))

!! compute nitrate losses in the groundwater
      gw_no3loss(j)=  shallst_n(j) * (1.0-gw_nloss(j))   
      !hlife_ngw   |days          |?Half-life of nitrogen in groundwater
      !gw_nloss(ihru) = Exp(-.693 / hlife_ngw)
      !readbsn-Qi.f90(512):      read (bsnfile_num,*,iostat=eof) hlife_ngw_bsn
      
      !Smith, R.L., Böhlke, J.K., Garabedian, S.P., Revesz, K.M. and Yoshinari, T., 2004. Assessing denitrification in groundwater using natural gradient tracer tests with 15N: In situ measurement of a sequential multistep reaction. Water Resources Research, 40(7).
      !The model indicated that nitrite production (0.036–0.047 umol N (L aquifer)-1 d-1) was faster than 
      !the subsequent denitrification steps (0.013–0.016 umol N (L aquifer)-1 d-1for nitrous oxide 
      !and 0.013–0.020 umol N (L aquifer)-1 d-1 for nitrogen gas) and that the total rate of reaction was slower than 
      !indicated by both acetylene block tracer tests and laboratory incubations. 
      !The rate of nitrate removal by denitrification was much slower than the rate of transport, 
      !indicating that nitrate would migrate several kilometers down-gradient before being completely consumed.
      
      !using 0.03 umol N L-1 d-1 as an example, for an aquifer with 2000 mm water, the denitrification rate is ca. 0.00000042 gN L-1 d-1 or 0.0084 gN ha-1 d-1 or ca. 3.066 gN ha-1 yr-1.
      !therefore, gw_no3loss is negilible.
      !In addition, NO3 in GW can be supplied through nitrification. 
      !So here, it is reasonable to assume gw_no3loss is 0.
      
      !Bouwman, A.F., Beusen, A.H.W., Griffioen, J., Van Groenigen, J.W., Hefting, M.M., Oenema, O., Van Puijenbroek, P.J.T.M., Seitzinger, S., Slomp, C.P. and Stehfest, E., 2013. Global trends and uncertainties in terrestrial denitrification and N2O emissions. Philosophical Transactions of the Royal Society B: Biological Sciences, 368(1621), p.20130112.
      !The annual soil N budget includes the N inputs and outputs for 0.58  0.58 grid cells for agricultural and natural land
      !Figure 3, groundwater denitrification (N2, N2O, NO) ranges between 0.125 and 8 Tg N, or from 0.41322314	kg N/ha/yr to 26.44628099	kg N/ha/yr
      ! or, from 0.001132118 kg N/ha/day to 0.072455564 kg N/ha/day

      shallst_n(j) =  shallst_n(j) !- gw_no3loss(j)
      shallst_n(j) = amax1(0., shallst_n(j))
  
      return
      end