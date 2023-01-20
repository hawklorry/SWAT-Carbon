      subroutine solwout
      !! This subroutine calculate soil water content at fixed depths
      use parm
      use parm_subE
      implicit none
      
      integer :: j, k, nly
       j = 0
       j = ihru
       nly = 0
       nly = sol_nly(j) 
      
       
       do k= 1, nly-1
       !sol_wcv(k,j) = sol_wc(k,j)/sol_thic(k,j)                          !! ice fraction  in soil layer  mm/mm
       !           sol_wc(k,j) = sol_wc(k,j) - d_ice(k,j)                  !! soil water content inlcuding  mm
       !    sol_ice(k,j) = sol_ice(k,j)+ d_ice(k,j)                 !! soil ice conteng mm
       if (sol_cd(k+1,j)>50. .and. sol_cd(k,j)<=50.) then
       swc_50(j)= sol_wcv(k,j) * ( (sol_cd(k+1,j)-50) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &     !!  at 50 mm depth
                   + sol_wcv(k+1,j) * ((50- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>100. .and. sol_cd(k,j)<=100.) then
       swc_100(j)= sol_wcv(k,j)  * ( (sol_cd(k+1,j)-100) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &   !!  at 100 mm depth
                  + sol_wcv(k+1,j) * ((100- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>150. .and. sol_cd(k,j)<=150.) then
       swc_150(j)= sol_wcv(k,j) * ( (sol_cd(k+1,j)-150) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !!  at 150 mm depth
                  + sol_wcv(k+1,j) * ((150- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>200. .and. sol_cd(k,j)<=200.) then
       swc_200(j)= sol_wcv(k,j) * ( (sol_cd(k+1,j)-200) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !!  at 200 mm depth
                  + sol_wcv(k+1,j) * ((200- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>300. .and. sol_cd(k,j)<=300.) then
       swc_300(j)= sol_wcv(k,j) * ( (sol_cd(k+1,j)-300) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !!  at 300 mm depth
                  + sol_wcv(k+1,j) * ((300- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>500. .and. sol_cd(k,j)<=500.) then
       swc_500(j)= sol_wcv(k,j) * ( (sol_cd(k+1,j)-500) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &    !! at 500 mm depth
                   +sol_wcv(k+1,j) * ( (500- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       if (sol_cd(k+1,j)>1000. .and. sol_cd(k,j)<=1000.) then
       swc_1000(j)= sol_wcv(k,j) * ( (sol_cd(k+1,j)-1000) / (sol_cd(k+1,j)-sol_cd(k,j)) ) &  !! at 1000 mm depth
                  + sol_wcv(k+1,j) * ( (1000- sol_cd(k,j)) / (sol_cd(k+1,j)-sol_cd(k,j)) )
       end if
       
       end do
      

      
      return
      end