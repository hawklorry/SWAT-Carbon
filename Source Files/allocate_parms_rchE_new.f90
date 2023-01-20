
      subroutine allocate_parms_rchE
      use parm 
      use parm_rchE
      use parm_control
      implicit none

      !! water temperature -----
        allocate (rch_hwattemp(msub,nstep_rch))
        
        allocate (rch_hwattemp_pre(mch))
       
        
      end subroutine
  	