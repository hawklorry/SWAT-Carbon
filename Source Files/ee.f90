      real function ee(tk) result (r_ee)      !!R669 4/20/18 nbs
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This function calculates saturation vapor pressure at a given 
!!    air temperature. 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    tk          |deg C         |mean air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ee          |kPa           |saturation vapor pressure
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      implicit none
      
      real, intent (in) :: tk

      r_ee = 0.                         !!R669 4/20/18 nbs
      if (tk + 237.3 /= 0.) then
        r_ee = (16.78 * tk - 116.9) / (tk + 237.3)        !!R669 4/20/18 nbs
        r_ee = Exp(r_ee)                                  !!R669 4/20/18 nbs
      end if

      return
      end