      function tair(hr,jj) result (r_tair)       !!R669 6/20/18 nbs

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function approximates hourly air temperature from daily max and
!!    min temperatures as documented by Campbell (1985)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmp_hi(:)   |deg C         |last maximum temperature in HRU
!!    tmp_lo(:)   |deg C         |last minimum temperature in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tair        |deg C         |air temperature for hour in HRU
!!    tmp_hi(:)   |deg C         |last maximum temperature in HRU
!!    tmp_lo(:)   |deg C         |last minimum temperature in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hr          |none          |hour if the day
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Cos

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!    subroutine modified by SLN

      use parm
      implicit none
       
      integer, intent (in) ::  jj
      real, intent(in) :: hr
      real :: r_tair                     !!R669 6/20/18 nbs

!! update hi or lo temperature depending on hour of day
      if (hr == 3) tmp_lo(jj) = tmn(jj)
      if (hr == 15) tmp_hi(jj) = tmx(jj)

!! SWAT manual equation 2.3.1
      r_tair = 0.                                     !!R669 6/20/18 nbs   
      r_tair = 0.5 * (tmp_hi(jj) + tmp_lo(jj) + (tmp_hi(jj) - tmp_lo(jj)  &  !!R669 6/20/18 nbs
                                        * Cos(0.2618 * Real(hr - 15))))

      return
      end