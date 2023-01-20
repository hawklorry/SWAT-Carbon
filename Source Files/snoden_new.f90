      subroutine snoden
      use parm
      use parm_subE
      implicit none
      
      integer :: j
      real :: a, b, c, d, e
      

      j = 0
      j = ihru

 
      c=0.
	  d=0.
      if(sno_hru(j)>0) then
        if(snofall>0) then
	        sno_den(j)= snofall/sno_hru(j)*0.1+(sno_hru(j)-snofall) /sno_hru(j) * sno_den(j)
	    else
            c=sno_den(j)      !!0.2
	        d=0.6
	        sno_den(j)= c+(d-c)/150*sno_iday(j)
	    end if
          
		e=0.
	    if (snomlt>1)  then
	        e=0.5/exp(1/snomlt)
            sno_den(j)= Max( e,sno_den(j) )
        end if

        sno_den(j) = Min(sno_den(j), 6.)
	end if

      
	if (sno_den(j)>0) then
	   sno_dep(j)=sno_hru(j)/sno_den(j) !!mm
	end if  
     
  !!-------------------------------------------------------------
     
         

    


	

      return
      end



