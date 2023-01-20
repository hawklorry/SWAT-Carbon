      subroutine submon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    mo_chk        |none          |current month of simulation
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    submono(1,:)  |mm H2O        |precipitation in subbasin for month
!!    submono(2,:)  |mm H2O        |snow melt in subbasin for month
!!    submono(3,:)  |mm H2O        |surface runoff loading in subbasin for month
!!    submono(4,:)  |mm H2O        |water yield from subbasin for month
!!    submono(5,:)  |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |month
!!    submono(6,:)  |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |month
!!    submono(7,:)  |metric tons/ha|sediment yield from subbasin for month
!!    submono(8,:)  |kg N/ha       |organic N loading from subbasin for month
!!    submono(9,:)  |kg P/ha       |organic P loading from subbasin for month
!!    submono(10,:) |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for month
!!    submono(11,:) |kg P/ha       |soluble P loading from subbasin for month
!!    submono(12,:) |mm H2O        |groundwater loading from subbasin for month
!!    submono(13,:) |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for month
!!    submono(14,:) |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for month
!!    subtot        |none          |number of subbasins in watershed
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
      integer:: mondays  
       mondays  = 0
      mondays= ndays(mo_chk+1)-ndays(mo_chk)

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = submono(1,sb)
        pdvab(2) = submono(2,sb)
        pdvab(3) = submono(5,sb)
        pdvab(4) = submono(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = submono(13,sb)
        pdvab(7) = submono(3,sb)
        pdvab(8) = submono(12,sb)
        pdvab(9) = submono(4,sb)
        pdvab(10) = submono(7,sb)
        pdvab(11) = submono(8,sb)
        pdvab(12) = submono(9,sb)
        pdvab(13) = submono(10,sb)
        pdvab(14) = submono(11,sb)
        pdvab(15) = submono(14,sb)
        pdvab(16) = submono(15,sb)
        pdvab(17) = submono(16,sb)
        pdvab(18) = submono(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = submono(19,sb)  !! tile_no3
        pdvab(23) = submono(18,sb)  !! qtile   jane f.
        pdvab(24) = submono(20,sb)  !! phos due to crack flow)

        pdvab(25) = submono(21,sb)    !! N2O(j)*1000    !! g/ha day
       pdvab(26) = submono(22,sb)   !! NO(j)*1000      !! g/ha day
       pdvab(27) = submono(23,sb)   !! nh4_vol(j)          !!kg N/ha 
       pdvab(28) = submono(24,sb)   !! CH4(j)
       pdvab(29) = submono(25,sb)   !!        !!kg N/ha 
       pdvab(30) = submono(26,sb)   !!  kg N/ha 
       pdvab(31) = submono(27,sb)    !! N  
       pdvab(32) = submono(28,sb)   !! 
       pdvab(33) = submono(29,sb)   !!       !!kg N/ha 
       pdvab(34) = submono(30,sb)   !!
       pdvab(35) = submono(31,sb)   !!        !!kg N/ha 
       pdvab(36) = submono(32,sb)   !!  !!kg N/ha 
       pdvab(37) = submono(33,sb)   !!  !!kg N/ha 
       
       submono(34,sb) = submono(34,sb)  / float(mondays)     !!        kg N/ha 
       submono(35,sb) = submono(35,sb)  / float(mondays)    !!  !!kg N/ha 
       submono(36,sb) = submono(36,sb)  / float(mondays)   !!  !!kg N/ha 
       submono(37,sb) = submono(37,sb)  / float(mondays)     !!  !!kg N/ha 
       pdvab(38) = submono(34,sb)   !! kg N/ha 
       pdvab(39) = submono(35,sb)   !! kg N/ha 
       pdvab(40) = submono(36,sb)   !! kg N/ha 
       pdvab(41) = submono(37,sb)   !! kg N/ha 
       
       pdvab(42) = submono(38,sb)    
       pdvab(43) = submono(39,sb) 
       pdvab(44) = submono(40,sb)   
       pdvab(45) = submono(41,sb)   
       
       pdvab(46) = submono(42,sb)        
       pdvab(47) = submono(43,sb)  
       pdvab(48) = submono(44,sb)   
       
      submono(45,sb)= submono(45,sb)   / float(mondays)   
      submono(46,sb)= submono(46,sb)  / float(mondays)   
      submono(47,sb) = submono(47,sb)  / float(mondays)   
      submono(48,sb) = submono(48,sb)  / float(mondays)     
      submono(49,sb) = submono(49,sb)  / float(mondays) 
      submono(50,sb) = submono(50,sb)  / float(mondays)  
      submono(51,sb)= submono(51,sb)  / float(mondays) 
      submono(52,sb) = submono(52,sb)  / float(mondays) 
      submono(53,sb) = submono(53,sb)  / float(mondays) 
      submono(54,sb) = submono(54,sb)  / float(mondays) 
      submono(56,sb) = submono(56,sb)  / float(mondays) 
       
       pdvab(49) = submono(45,sb)  
       pdvab(50) = submono(46,sb)  
       pdvab(51) = submono(47,sb) 
       pdvab(52) = submono(48,sb)  
       pdvab(53) = submono(49,sb) 
       pdvab(54) = submono(50,sb) 
       pdvab(55) = submono(51,sb) 
       pdvab(56) = submono(52,sb) 
       pdvab(57) = submono(53,sb)  
       pdvab(58) = submono(54,sb) 
       pdvab(59) = submono(55,sb)  
       pdvab(60) = submono(56,sb)  
      
       submono(57,sb)= submono(57,sb)   / float(mondays)      !! soil water content    
       pdvab(61) = submono(57,sb)  
       
        submono(58,sb)= submono(58,sb)   / float(mondays)      !! 
        submono(59,sb)= submono(59,sb)   / float(mondays)      !! 
       pdvab(62) = submono(55,sb)  
       pdvab(63) = submono(56,sb)  
       
       submono(60,sb)= submono(60,sb)   / float(mondays)      !! 
       submono(61,sb)= submono(61,sb)   / float(mondays)      !! 
       submono(62,sb)= submono(62,sb)   / float(mondays)      !! 
       pdvab(64) = submono(60,sb)  
       pdvab(65) = submono(61,sb)  
       pdvab(66) = submono(62,sb)  
    
       
        if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          write (output_sub_num,1000) sb, subgis(sb), mo_chk, sub_km(sb), (pdvb(ii), ii = 1, itotb)
        else
          write (output_sub_num,1000) sb, subgis(sb), mo_chk, sub_km(sb), (pdvab(ii), ii = 1, msubo)
        end if
      end do

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i5,1x,i8,1x,i4,1x,e10.5,18f10.3,1x,e10.5,5e10.3,42f10.3)       !!R666b 7/19/17 and R670  6/26/18  nbs
      end 