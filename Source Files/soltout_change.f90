      subroutine soltout
      !! This subroutine calculate soil temperature at fixed depths
      use parm
      use parm_subE
      implicit none
      
      integer :: j, k, nly
       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j) 
       
       do k= 1, nly1-1
       
       if (sol_cd(k+1,j)>50. .and. sol_cd(k,j)<=50.) then
       soltmp_50(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-50) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !! soil temperature at 50 mm depth
                   + sol_tmp(k+1,j) * ((50- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>100. .and. sol_cd(k,j)<=100.) then
       soltmp_100(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-100) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !! soil temperature at 100 mm depth
                  + sol_tmp(k+1,j) * ((100- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>150. .and. sol_cd(k,j)<=150.) then
       soltmp_150(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-150) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &   !! soil temperature at 150 mm depth
                  + sol_tmp(k+1,j) * ((150- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>200. .and. sol_cd(k,j)<=200.) then
       soltmp_200(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-200) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &   !! soil temperature at 200 mm depth
                  + sol_tmp(k+1,j) * ((200- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>300. .and. sol_cd(k,j)<=300.) then
       soltmp_300(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-300) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !! soil temperature at 300 mm depth
                  + sol_tmp(k+1,j) * ((300- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>500. .and. sol_cd(k,j)<=500.) then
       soltmp_500(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-500) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !! soil temperature at 500 mm depth
                   +sol_tmp(k+1,j) * ( (500- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>1000. .and. sol_cd(k,j)<=1000.) then
       soltmp_1000(j)= sol_tmp(k,j) * ( (sol_cd(k+1,j)-1000) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &  !! soil temperature at 1000 mm depth
                  + sol_tmp(k+1,j) * ( (1000- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       end do
      
         !! for frezon day accumulation 
      if (sol_tmp(1,j) <=0.) then
        sol_frozday(j)=1.
      else
        sol_frozday(j)=0.
      end if
     
     
   
      
      return
      end

     