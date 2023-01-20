      subroutine soltbott
      !! This subroutine is used to calculate temperature of bottom boundary
      use parm
      use parm_subE
      implicit none
      
      integer :: j
      real :: f, tamp
 
       j = 0
       j = ihru

       f=0
       tamp=0
       
       !! Mean annual damping depth of snowpack
       f=0.00032*60*60*24*365.25/3.14159265/0.2
       f=Sqrt(f)                                                                                          
	   !! Annual amplitude of monthly mean air temperature 
	   tamp = (tmpmx(i_mo,hru_sub(j)) - tmpmn(i_mo,hru_sub(j))) / 2      
       !! bottom boundary temperature 
       bot_tmp(j) = tmp_an(hru_sub(j))+(0.5*tamp)*(1-Exp(-ansnodep(j)/10/f))
 
      return
      end
