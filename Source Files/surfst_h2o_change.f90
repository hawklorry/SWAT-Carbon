      subroutine surfst_h2o

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the net surface runoff reaching the 
!!    main channel on a given day. The net amount of water reaching
!!    the main channel can include water in surface runoff from the 
!!    previous day and will exclude surface runoff generated on the
!!    current day which takes longer than one day to reach the main
!!    channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    brt(:)      |none          |fraction of surface runoff that takes
!!                               |one day or less to reach the subbasin
!!                               |outlet
!!    ihru        |none          |HRU number
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU on the
!!                               |current day
!!    hhqday(:)   |mm H2O        |surface runoff generated in HRU on the
!!                               |current hour at current day  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bsprev      |mm H2O        |surface runoff lagged from prior day
!!    qday        |mm H2O        |surface runoff loading to main channel
!!                               |from HRU on current day
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    hhqday(:)   |mm H2O        |surface runoff generated in HRU on the
!!                               |current hour at current day  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

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
      
      integer :: j,k
      
      j = 0
      j = ihru

      if (ievent == 0) then	
 
         bsprev = surf_bs(1,j)
	   surf_bs(1,j) = Max(1.e-6, surf_bs(1,j) + surfq(j))
	   
	   !!------------------lateral water temperatrue lag----------------------
        if (tswat == 1) then
     
       if( surf_bs(1,j)>0.) then
        surf_bs(22,j)=(surf_bs(22,j)*(surf_bs(1,j)-surfq(j)) +wtmp_surq(j)*surfq(j) ) /surf_bs(1,j)          
       
      end if 
      
          
        wtmp_surq(j)=surf_bs(22,j)  !*brt(j)
       
        surf_bs(22,j)=wtmp_surq(j)
        end if
        !!------------------lateral water temperatrue lag----------------------
        
         qday = surf_bs(1,j) * brt(j)
         surf_bs(1,j) = surf_bs(1,j) - qday
	
      else
		!subdaily runoff lag (applies only to runoff from pervious surface)
         bsprev = hhsurf_bs(1,j,nstep)		! lag from previous day J.Jeong 4/06/2009

	   do k=1,nstep

	   !! Left-over (previous timestep) + inflow (current  timestep)
           hhsurf_bs(1,j,k) = Max(0., bsprev + hhqday(k))
   	
	   !! new estimation of runoff and sediment reaching the main channel
	     hhqday(k) = hhsurf_bs(1,j,k) * brt(j)
	     hhsurf_bs(1,j,k) = hhsurf_bs(1,j,k) - hhqday(k)
   	  
	   !! lagged at the end of time step  
	     bsprev = hhsurf_bs(1,j,k) 
	   end do

	   !! daily total yield from the HRU
	   qday = sum(hhqday) + sum(ubnrunoff)  
	end if
	
	
	
     
      return
      end