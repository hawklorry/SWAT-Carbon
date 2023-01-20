     subroutine tillfactor(jj,bmix,emix,dtil)
	!!!!!!!!!!!!!!!!!!!!!!!
	!! Armen R. Kemaniana,, Stefan Julichb,1, Valipuram S. Manoranjanc, Jeffrey R. Arnold.
	!  " Integrating soil carbon cycling with that of nitrogen and phosphorus in thewatershed model SWAT: Theory and model testing "
	!  Ecological Modelling, 222 (2011) 1913–1921
	! Armen 16 January 2008
	! This procedure increases tillage factor (tillagef(l,jj) per layer for each operation
	! The tillage factor settling will depend of soil moisture (tentatively) and must be called every day
	! For simplicity the settling is calculated now at the soil carbon sub because soil water content is available

	! The tillage factor depends on the cumulative soil disturbance rating = csdr
	! For simplicity, csdr is a function of emix
	! First step is to calculate "current" csdr by inverting tillage factor function
	! The effect of texture on tillage factor (ZZ) is removed first (and recovered at the end of the procedure)
	! YY = tillagef(l,jj) / ZZ
	! Since the tillage factor function is non linear, iterations are needed 
	! XX = 0.5 is the initial value that works OK for the range of values observed
	! If a layer is only partially tilled then emix is corrected accordingly
     
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
	  integer, intent (in) :: jj
      real, intent (in) :: bmix,emix, dtil
      integer :: l, m1, m2,j
      real :: emix1, dtil1,bmix1
	  !real :: sol_thick(sol_nly(jj))
	  real ::xx,yy,zz,XX1,XX2,CSDR
	  j=jj
	  bmix1 = bmix
	  emix1 = emix
	  dtil1 = dtil
	 
     
	
	
	
	!emix1 = emix1 - bmix1 ! this is to avoid affecting tillage factor with biological mixing
	
	if (emix1 > 0.) then

	  do l=1, sol_nly(j)
			
 
	    if (sol_z(l,j) <= dtil1) then
		  emix1 = emix1
	    else if (l==1) then
	    emix1 = emix1 * (dtil1 - 0.) / sol_thick(l,j)
	    else if (sol_z(l,j) > dtil1 .AND. sol_z(l-1,j) < dtil1) then 
		  emix1 = emix1 * (dtil1 - sol_z(l-1,j)) / sol_thick(l,j)
	    else
	    emix1 = 0.
	    end if
			
	    ! to save computation time if emix = 0 here then the other layers can be avoided
	    ! tillage always proceeds from top to bottom
	    if (emix1 == 0.) exit

	    xx = 0.
	    yy = 0.
	    zz = 0.
	    zz = 3. + (8. - 3.) * exp(-5.5 * sol_clay(l,j)/100.)
	    yy = tillagef(l,j) / zz
	    m1 = 1.
	    m2 = 2.
        
	    ! empirical solution for x when y is known and y=x/(x+exp(m1-m2*x)) 
	    if (yy > 0.01) then
		  xx1 = yy ** exp(-0.13 + 1.06 * yy)
		  xx2 = exp(0.64 + 0.64 * yy ** 100.)
		  xx = xx1 * xx2
	    end if

	    csdr = xx + emix1
	    tillagef(l,j) = zz * (csdr / (csdr + exp(m1 - m2 * csdr)))
        
   
	  end do		
		
	
	end if
		
	return
	end subroutine