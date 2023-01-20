     subroutine Soil_Texture  
     use parm
     use parm_subC
     implicit none
     
     integer :: j, k   
       j = 0
       k = 0
      do j = 1, nhru
      do k = 1, sol_nly(j)
if((sol_silt(k,j) + 1.5*sol_clay(k,j)) < 15.) sol_tex(k,j)=1	  !!SAND
if((sol_silt(k,j) + 1.5*sol_clay(k,j) >= 15.) .and. (sol_silt(k,j) + 2.*sol_clay(k,j) < 30)) sol_tex(k,j)=2	!LOAMY SAND
if((sol_clay(k,j) >= 7. .and. sol_clay(k,j) < 20.) .and. (sol_sand(k,j) > 52.) .and. ((sol_silt(k,j) + 2.*sol_clay(k,j)) >= 30.) .or. (sol_clay(k,j) < 7. .and. sol_silt(k,j) < 50. .and. (sol_silt(k,j)+2.*sol_clay(k,j))>=30.))	sol_tex(k,j)=3 !SANDY LOAM
if((sol_clay(k,j) >= 7. .and. sol_clay(k,j) < 27.) .and. (sol_silt(k,j) >= 28. .and. sol_silt(k,j) < 50.) .and. (sol_sand(k,j) <= 52.))	sol_tex(k,j)=4  !LOAM
if((sol_silt(k,j) >= 50. .and. (sol_clay(k,j) >= 12. .and. sol_clay(k,j) < 27.)) .or. ((sol_silt(k,j) >= 50. .and. sol_silt(k,j) < 80.) .and. sol_clay(k,j) < 12.))	sol_tex(k,j)=5  !SILT LOAM
if(sol_silt(k,j) >= 80. .and. sol_clay(k,j) < 12.)	sol_tex(k,j)=6  !SILT
if((sol_clay(k,j) >= 20. .and. sol_clay(k,j) < 35.) .and. (sol_silt(k,j) < 28.) .and. (sol_sand(k,j) > 45.)) 	sol_tex(k,j)=7 ! SANDY CLAY LOAM
if((sol_clay(k,j) >= 27. .and. sol_clay(k,j) < 40.) .and. (sol_sand(k,j) > 20. .and. sol_sand(k,j) <= 45.))	sol_tex(k,j)=8  ! CLAY LOAM
if((sol_clay(k,j) >= 27. .and. sol_clay(k,j) < 40.) .and. (sol_sand(k,j)  <= 20.))	sol_tex(k,j)=9  !SILTY CLAY LOAM
if(sol_clay(k,j) >= 35. .and. sol_sand(k,j) > 45.)	sol_tex(k,j)=10  !SANDY CLAY
if(sol_clay(k,j) >= 40. .and. sol_silt(k,j) >= 40.)	sol_tex(k,j)=11  !SILTY CLAY
if(sol_clay(k,j) >= 40. .and. sol_sand(k,j) <= 45. .and. sol_silt(k,j) < 40.)	sol_tex(k,j)=12  !CLAY
     end do
     end do
   
   return
   end 