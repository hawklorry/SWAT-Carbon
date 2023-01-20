      subroutine subyr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub
!!    iyr           |year          |current year of simulation (eg 1980)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    subtot        |none          |number of subbasins in watershed
!!    subyro(1,:)   |mm H2O        |precipitation in subbasin for year
!!    subyro(2,:)   |mm H2O        |snow melt in subbasin for year
!!    subyro(3,:)   |mm H2O        |surface runoff loading in subbasin for year
!!    subyro(4,:)   |mm H2O        |water yield from subbasin for year
!!    subyro(5,:)   |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |year
!!    subyro(6,:)   |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |year
!!    subyro(7,:)   |metric tons/ha|sediment yield from subbasin for year
!!    subyro(8,:)   |kg N/ha       |organic N loading from subbasin for year
!!    subyro(9,:)   |kg P/ha       |organic P loading from subbasin for year
!!    subyro(10,:)  |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for year
!!    subyro(11,:)  |kg P/ha       |soluble P loading from subbasin for year
!!    subyro(12,:)  |mm H2O        |groundwater loading from subbasin for year
!!    subyro(13,:)  |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for year
!!    subyro(14,:)  |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

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
      
      integer :: sb, ii
      real, dimension (msubo) :: pdvab, pdvb

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = subyro(1,sb)
        pdvab(2) = subyro(2,sb)
        pdvab(3) = subyro(5,sb)
        pdvab(4) = subyro(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = subyro(13,sb)
        pdvab(7) = subyro(3,sb)
        pdvab(8) = subyro(12,sb)
        pdvab(9) = subyro(4,sb)
        pdvab(10) = subyro(7,sb)
        pdvab(11) = subyro(8,sb)
        pdvab(12) = subyro(9,sb)
        pdvab(13) = subyro(10,sb)
        pdvab(14) = subyro(11,sb)
        pdvab(15) = subyro(14,sb)
        pdvab(16) = subyro(15,sb)
        pdvab(17) = subyro(16,sb)
        pdvab(18) = subyro(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = subyro(19,sb)    !!tile_no3
        pdvab(23) = subyro(18,sb)    !!qtile jane f.
        pdvab(24) = subyro(20,sb)    !!phos due to crack flow jane f.
!!-----------------------------------------output.sub------------------------------------------------------
       pdvab(25) = subyro(21,sb)   !! N2O(j)*1000    !! g/ha day
       pdvab(26) = subyro(22,sb)   !! NO(j)*1000      !! g/ha day
       pdvab(27) = subyro(23,sb)   !! nh4_vol(j)          !!kg N/ha 
       pdvab(28) = subyro(24,sb)   !! CH4(j)
       pdvab(29) = subyro(25,sb)   !!    kg N/ha 
       pdvab(30) = subyro(26,sb)   !!    kg N/ha 
       pdvab(31) = subyro(27,sb)   !! 
       pdvab(32) = subyro(28,sb)   !! 
       pdvab(33) = subyro(29,sb)   !!    kg N/ha 
       pdvab(34) = subyro(30,sb)   !! 
       pdvab(35) = subyro(31,sb)   !!    kg N/ha 
       pdvab(36) = subyro(32,sb)   !!    kg N/ha 
       pdvab(37) = subyro(33,sb)   !!
      
      
    
      
       subyro(34,sb) = subyro(34,sb)/12.   !!      
       subyro(35,sb) = subyro(35,sb)/12.   !!     kg N/ha 
       subyro(36,sb) = subyro(36,sb)/12.   !! 
       subyro(37,sb) = subyro(37,sb) /12.  !! 
       
       pdvab(38) = subyro(34,sb)   !!      
       pdvab(39) = subyro(35,sb)   !!     kg N/ha 
       pdvab(40) = subyro(36,sb)   !! 
       pdvab(41) = subyro(37,sb)  !! 
      
       pdvab(42) = subyro(38,sb)   !! 
       pdvab(43) = subyro(39,sb)   !! 
       pdvab(44) = subyro(40,sb)   !! 
       pdvab(45) = subyro(41,sb)   !! 
       
       !subyro(42,sb) = subyro(42,sb) /12. !! 
       pdvab(46) = subyro(42,sb) !! 
    
       pdvab(47) = subyro(43,sb) !!    
       pdvab(48) = subyro(44,sb)   !! 
  
       
       
       subyro(45,sb) = subyro(45,sb)/12.   !! 
       subyro(46,sb)= subyro(46,sb)/12.   !! 
       pdvab(49) = subyro(45,sb)   !! 
       pdvab(50) = subyro(46,sb)   !!   
       
       subyro(47,sb) = subyro(47,sb)/12.   !! 
       subyro(48,sb) = subyro(48,sb) /12.  !!    
       subyro(49,sb) = subyro(49,sb) /12.  !!    
       subyro(50,sb) = subyro(50,sb)/12.   !!
       subyro(51,sb) = subyro(51,sb)/12.  !!      
       subyro(52,sb) = subyro(52,sb)/12.   !!    
       subyro(53,sb) = subyro(53,sb) /12.  !! 
       subyro(54,sb) = subyro(54,sb) /12.  !! 
       subyro(56,sb) = subyro(56,sb) /12.  !! 
      
       
       pdvab(51) = subyro(47,sb)  !! 
       pdvab(52) = subyro(48,sb)   !!    
       pdvab(53) = subyro(49,sb)   !!    
       pdvab(54) = subyro(50,sb)   !!
       pdvab(55) = subyro(51,sb) !!      
       pdvab(56) = subyro(52,sb)  !!    
       pdvab(57) = subyro(53,sb)  !! 
       pdvab(58) = subyro(54,sb)   !! 
       
       pdvab(59) = subyro(55,sb)   !! 
       pdvab(60) = subyro(56,sb)   !! 
       
       subyro(57,sb) = subyro(57,sb) /12.  !!     soil moisture content 
       pdvab(61) = subyro(57,sb)   !! 
       
       subyro(58,sb) = subyro(58,sb) /12.  !!    
       subyro(59,sb) = subyro(59,sb) /12.  !!  
       pdvab(62) = subyro(58,sb)   !! 
       pdvab(63) = subyro(59,sb)   !! 
       
       subyro(60,sb) = subyro(60,sb) /12.           !! 
       subyro(61,sb) = subyro(61,sb) /12.  !!    
       subyro(62,sb) = subyro(62,sb) /12.  !!   
       
       pdvab(64) = subyro(60,sb)   !! 
       pdvab(65) = subyro(61,sb)   !! 
       pdvab(66) = subyro(62,sb)   !! 

       if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          write (output_sub_num,1000) sb, subgis(sb), iyr, sub_km(sb),  (pdvb(ii), ii = 1, itotb)
        else
          write (output_sub_num,1000) sb, subgis(sb), iyr, sub_km(sb),  (pdvab(ii), ii = 1, msubo)
        end if
      end do

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i5,1x,i8,1x,i4,1x,e10.4,18f10.3,e10.3,5e10.3, 42f10.3)       !!R666b 7/19/17 nbs
      end 