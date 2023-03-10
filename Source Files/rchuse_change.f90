      subroutine rchuse
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine removes water from reach for consumptive water use

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1       |none          |reach number
!!    i_mo        |none          |month of simulation
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach
!!                               |for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jrch        |none          |HRU number
!!    wtrin       |m^3 H2O       |water outflow from reach prior to
!!                               |subtracting irrigation diversions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use parm_control
      implicit none
      
      integer :: jrch, ii
      real :: wtrin

      jrch = 0
      jrch = inum1

      wtrin = 0.
      wtrin = rtwtr

      rtwtr = rtwtr - wurch(i_mo,jrch) * 10000.
      if (rtwtr < 0.) rtwtr = 0.

      if (ievent_rch > 0) then
        do ii = 1, nstep_rch
          hrtwtr(ii) = hrtwtr(ii) - wurch(i_mo,jrch) * 10000.  &
	  		/ real(nstep_rch)
          if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
        end do
      end if

      if (wtrin /= rtwtr .and. wtrin > 0.01) then
        sedrch = sedrch * rtwtr / wtrin

        rch_san = rch_san * rtwtr / wtrin
        rch_sil = rch_sil * rtwtr / wtrin
        rch_cla = rch_cla * rtwtr / wtrin
        rch_sag = rch_sag * rtwtr / wtrin
        rch_lag = rch_lag * rtwtr / wtrin
        rch_gra = rch_gra * rtwtr / wtrin

        if (sedrch  < 1.e-6) then
	    sedrch = 0.
	    rch_san = 0.
          rch_sil = 0.
          rch_cla = 0.
          rch_sag = 0.
          rch_lag = 0.
          rch_gra = 0.
	  end if

        if (ievent_rch > 0) then
          do ii = 1, nstep_rch
            hsedyld(ii) = hsedyld(ii) * rtwtr / wtrin
            if (hrtwtr(ii) == 0.) hsedyld(ii) = 0.
          end do
        end if
      end if

      return
      end