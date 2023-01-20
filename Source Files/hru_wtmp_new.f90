    subroutine hru_wtmp

   use parm
     use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
   
   implicit none
   integer:: j

    j = 0
    j = ihru
  
 
 
    if (qday>0.) then
     wtmp_surq(j)= 5.0 + 0.75 * tmpav(j)
    else
     wtmp_surq(j)= 0.
    end if 

    if(latq(j) > 0.000001) then
     wtmp_latq(j)= 5.0 + 0.75 * tmpav(j) 
    else 
     wtmp_latq(j)= 0.
    end if

    if(gw_q(j)>0.) then
     wtmp_gwq(j)= 5.0 + 0.75 * tmpav(j)
    else
     wtmp_gwq(j)=0.
    end if
    
    if(wtmp_surq(j)<-1.0) wtmp_surq(j)= -1.0
    if(wtmp_gwq(j)<-1.0) wtmp_gwq(j) = -1.0
    if(wtmp_latq(j) <-1.0) wtmp_latq(j) = -1.0            
                
   return
   end