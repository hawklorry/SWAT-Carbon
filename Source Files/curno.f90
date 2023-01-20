      subroutine curno(cnn,ii)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the curve numbers for moisture conditions
!!    I and III and calculates coefficents and shape parameters for the 
!!    water retention curve
!!    the coefficents and shape parameters are calculated by one of two methods:
!!    the default method is to make them a function of soil water, the 
!!    alternative method (labeled new) is to make them a function of 
!!    accumulated PET, precipitation and surface runoff.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnn         |none          |SCS runoff curve number for moisture
!!                               |condition II
!!    h           |none          |HRU number
!!    sol_sumfc(:)|mm H2O        |amount of water held in soil profile at
!!                               |field capacity
!!    sol_sumul(:)|mm H2O        |amount of water held in soil profile at
!!                               |saturation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cn1(:)      |none          |SCS runoff curve number for moisture
!!                               |condition I
!!    cn3(:)      |none          |SCS runoff curve number for moisture
!!                               |condition III
!!    sci(:)      |none          |retention coefficient for cn method based on
!!                               |plant ET
!!    smx(:)      |none          |retention coefficient for cn method based on
!!                               |soil moisture
!!    wrt(1,:)    |none          |1st shape parameter for calculation of 
!!                               |water retention
!!    wrt(2,:)    |none          |2nd shape parameter for calculation of 
!!                               |water retention
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition   
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c2          |none          |variable used to hold calculated value
!!    rto3        |none          |fraction difference between CN3 and CN1 
!!                               |retention parameters
!!    rtos        |none          |fraction difference between CN=99 and CN1 
!!                               |retention parameters
!!    s3          |none          |retention parameter for CN3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      integer, intent (in) :: ii
      real, intent (in) :: cnn
      real :: c2, s3, rto3, rtos, smxold, sumfc_ul
   
      cn2(ii) = cnn
      if (cn1(ii) > 1.e-6) smxold = 254.* (100. / cn1(ii) - 1.)
      c2 = 0.
      cn3(ii) = 0.
      s3 = 0.
      cn1(ii) = 0.

!! calculate moisture condition I and III curve numbers
      c2 = 100. - cnn
      cn1(ii) = cnn - 20. * c2 / (c2 + Exp(2.533 - 0.0636 * c2))
      cn1(ii) = Max(cn1(ii), .4 * cnn)
      cn3(ii) = cnn * Exp(.006729 * c2)

!! calculate maximum retention parameter value
      smx(ii) = 254. * (100. / cn1(ii) - 1.)

!! calculate retention parameter value for CN3
      s3 = 254. * (100. / cn3(ii) - 1.)

!! calculate fraction difference in retention parameters
      rto3 = 0.
      rtos = 0.
      rto3 = 1. - s3 / smx(ii)
      rtos = 1. - 2.54 / smx(ii)
      
      sumfc_ul = sol_sumfc(ii) !+ r2adj(ii) * (sol_sumul(ii) - sol_sumfc(ii))
!! calculate shape parameters
      call ascrv(rto3,rtos,sumfc_ul,sol_sumul(ii),wrt(1,ii),wrt(2,ii))

      if (curyr == 0) then
	    sci(ii) = 0.9 * smx(ii)
	  else
        sci(ii) = (1. - ((smxold - sci(ii)) / smxold)) * smx(ii)      !! plant ET
	  end if

      return
      end