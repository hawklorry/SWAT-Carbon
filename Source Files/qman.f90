      function qman(x1,x2,x3,x4) result (r_qman)             !!R669 6/20/18 nbs
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates flow rate or flow velocity using Manning's
!!    equation. If x1 is set to 1, the velocity is calculated. If x1 is set to
!!    cross-sectional area of flow, the flow rate is calculated.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    x1          |m^2 or none   |cross-sectional flow area or 1.
!!    x2          |m             |hydraulic radius
!!    x3          |none          |Manning's "n" value for channel
!!    x4          |m/m           |average slope of channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    qman        |m^3/s or m/s  |flow rate or flow velocity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      implicit none
      
      real, intent (in) :: x1, x2, x3, x4
      real :: r_qman                     !!R669 6/20/18 nbs

      r_qman = 0.                                        !!R669 6/20/18 nbs
      r_qman = x1 * x2 ** .6666 * Sqrt(x4) / x3          !!R669 6/20/18 nbs

      return
      end