      subroutine ndenit(k,j,cdg,wdn,void)
!!    this subroutine computes denitrification 

	use parm
    use parm_subC 

	implicit none
	integer :: k,j
	real :: cdg, wdn, void,vof
	
	if (void<1.0E-6) void=0.
      wdn = 0.
	vof = 1. / (1. + (void/0.04)**5)
	wdn = sol_no3(k,j) * (1. - Exp(-cdn(j) * cdg * vof * sol_cbn(k,j)))
	sol_no3(k,j) = sol_no3(k,j) - wdn
	no3_denit(j) = no3_denit(j) + wdn

	return
	end