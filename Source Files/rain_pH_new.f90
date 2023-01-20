    subroutine rain_pH
    !! Rain fall effect on soil pH
    !! based on DNDC
    use parm
    implicit none  
    
    real:: dev_ph, Frain, D_ph
    integer:: j,k
      j = 0
      j = ihru
      
    Frain = 0.02 *precipday + 0.05
    !Frain = 0.2
    do k = 1,sol_nly(j)     
    
    if(precipday > 5) sol_ph(k,j) = ori_sol_ph(k,j) 
    D_ph = ori_sol_ph(k,j)
    if(sol_ph(k,j)/=D_ph)then
    dev_ph = (sol_ph(k,j) - D_ph) * Frain
    sol_ph(k,j) = sol_ph(k,j)-dev_ph
    end if
    
    end do
    
    return
    end