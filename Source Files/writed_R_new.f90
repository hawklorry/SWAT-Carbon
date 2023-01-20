      subroutine writed_R
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none

      integer :: j, k
      
      
      
      
        if (iprint == 1.or.iprint==3) then
        !! write daily reach output
      
            if((cswat == 2 ) .and. rch2==1) call rchday2       
    
    
        end if
        
        return 
        end