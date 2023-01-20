      subroutine rchmon2(mdays)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the monthly reach output to the .rch file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilog         |none         |streamflow print code
!!                               |0 print streamflow in reach
!!                               |1 print Log10 streamflow in reach
!!    ipdvar(:)    |none         |output variable codes for .rch file
!!    isproj       |none         |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotr        |none         |number of output variables printed (.rch)
!!    mo_chk       |none         |month for current day of simulation
!!    mrcho        |none         |maximum number of variables written to
!!                               |reach output file (.rch)
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchmono(1,:) |m^3/s        |flow into reach during month
!!    rchmono(2,:) |m^3/s        |flow out of reach during month
!!    rchmono(3,:) |metric tons  |sediment transported into reach during month
!!    rchmono(4,:) |metric tons  |sediment transported out of reach during month
!!    rchmono(5,:) |mg/L         |sediment concentration in outflow during month
!!    rchmono(6,:) |kg N         |organic N transported into reach during month
!!    rchmono(7,:) |kg N         |organic N transported out of reach during
!!                               |month
!!    rchmono(8,:) |kg P         |organic P transported into reach during month
!!    rchmono(9,:) |kg P         |organic P transported out of reach during
!!                               |month
!!    rchmono(10,:)|m^3/s        |evaporation from reach during month
!!    rchmono(11,:)|m^3/s        |transmission losses from reach during month
!!    rchmono(12,:)|kg           |conservative metal #1 transported out of reach
!!                               |during month
!!    rchmono(13,:)|kg           |conservative metal #2 transported out of reach
!!                               |during month
!!    rchmono(14,:)|kg           |conservative metal #3 transported out of reach
!!                               |during month
!!    rchmono(15,:)|kg N         |nitrate transported into reach during month
!!    rchmono(16,:)|kg N         |nitrate transported out of reach during month
!!    rchmono(17,:)|kg P         |soluble P transported into reach during month
!!    rchmono(18,:)|kg P         |soluble P transported out of reach during
!!                               |month
!!    rchmono(19,:)|mg pst       |soluble pesticide transported into reach
!!                               |during month
!!    rchmono(20,:)|mg pst       |soluble pesticide transported out of reach
!!                               |during month
!!    rchmono(21,:)|mg pst       |sorbed pesticide transported into reach during
!!                               |month
!!    rchmono(22,:)|mg pst       |sorbed pesticide transported out of reach
!!                               |during month
!!    rchmono(23,:)|mg pst       |amount of pesticide lost through reactions in
!!                               |reach during month
!!    rchmono(24,:)|mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during month
!!    rchmono(25,:)|mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during month
!!    rchmono(26,:)|mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during month
!!    rchmono(27,:)|mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during month
!!    rchmono(28,:)|mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during month
!!    rchmono(29,:)|mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during month
!!    rchmono(30,:)|kg chla      |chlorophyll-a transported into reach during
!!                               |month
!!    rchmono(31,:)|kg chla      |chlorophyll-a transported out of reach during
!!                               |month
!!    rchmono(32,:)|kg N         |ammonia transported into reach during month
!!    rchmono(33,:)|kg N         |ammonia transported out of reach during month
!!    rchmono(34,:)|kg N         |nitrite transported into reach during month
!!    rchmono(35,:)|kg N         |nitrite transported out of reach during month
!!    rchmono(36,:)|kg O2        |CBOD transported into reach during month
!!    rchmono(37,:)|kg O2        |CBOD transported out of reach during month
!!    rchmono(38,:)|kg O2        |dissolved oxygen transported into reach during
!!                               |month
!!    rchmono(39,:)|kg O2        |dissolved oxygen transported out of reach
!!                               |during month
!!    rchmono(40,:)|kg bact      |persistent bacteria transported out of reach
!!                               |during month
!!    rchmono(41,:)|kg bact      |less persistent bacteria transported out of
!!                               |reach during month

!!    rchmono(43,:)|kg           |Total N (org N + no3 + no2 + nh4 outs)
!!    rchmono(44,:)|kg           |Total P (org P + sol p outs)

!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    mdays       |none          |number of days simulated in month
!!    pdvar(:)    |varies        |array of default reach output values
!!    pdvr(:)     |varies        |array of custom reach output values
!!    srch_av(:)  |varies        |array storing average monthly values for
!!                               |reach output data
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para    !! added by Du
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
        implicit none
        
      integer, intent (in) :: mdays
      integer :: j, ii
      real, dimension (mrcho2) :: pdvar, pdvr
      real, dimension (11) :: srch_av

      do j = 1, subtot
        pdvar = 0. 
        pdvr = 0.
      
    
          !! assign monthly values
        pdvar(1) = rchmono2(1,j)   
        pdvar(2) = rchmono2(2,j)    
        pdvar(3) =rchmono2(3,j)     
        pdvar(4) = rchmono2(4,j) 
        pdvar(5) = rchmono2(5,j)   
        pdvar(6) = rchmono2(6,j)   
        pdvar(7) = rchmono2(7,j) 
        pdvar(8) = rchmono2(8,j) 
        pdvar(9) = rchmono2(9,j) 
        pdvar(10) = rchmono2(10,j)  
        pdvar(11) = rchmono2(11,j)  
        pdvar(12) = rchmono2(12,j)  
        pdvar(13) = rchmono2(13,j)    
        pdvar(14) = rchmono2(14,j)   
        pdvar(15) = rchmono2(15,j) 
        pdvar(16) = rchmono2(16,j)   
        pdvar(17) = rchmono2(17,j)  
        pdvar(18) = rchmono2(18,j)  
        pdvar(19) = rchmono2(19,j)   
        pdvar(20) = rchmono2(20,j)    
        pdvar(21) = rchmono2(21,j)  
        pdvar(22) = rchmono2(22,j)  
        pdvar(23) = rchmono2(23,j)   
        pdvar(24) = rchmono2(24,j)   
        pdvar(25) = rchmono2(25,j)  
        pdvar(26) = rchmono2(26,j)   
        pdvar(27) = rchmono2(27,j)   
        pdvar(28) = rchmono2(28,j)   
        pdvar(29) = rchmono2(29,j)    
        pdvar(30) = rchmono2(30,j)   
        pdvar(31) = rchmono2(31,j)   
        pdvar(32) = rchmono2(32,j)    
        pdvar(33) = rchmono2(33,j)    / Real(mdays)  ! Concentration
        pdvar(34) = rchmono2(34,j)    / Real(mdays)  ! Concentration
        pdvar(35) = rchmono2(35,j)    / Real(mdays)  ! Concentration
        pdvar(36) = rchmono2(36,j)   / Real(mdays)   ! Concentration
        pdvar(37) = rchmono2(37,j)   / Real(mdays)   ! Concentration
        pdvar(38) = rchmono2(38,j)    / Real(mdays)   ! Concentration
        pdvar(39) = rchmono2(39,j)  
        pdvar(40) = rchmono2(40,j)
        pdvar(41) = rchmono2(41,j) 
        pdvar(42) = rchmono2(42,j) 
        pdvar(43) = rchmono2(43,j) / Real(mdays)   ! Concentration
        pdvar(44) = rchmono2(44,j)  
        pdvar(45) = rchmono2(45,j) 
        pdvar(46) = rchmono2(46,j) 
        pdvar(47) = rchmono2(47,j) 
        pdvar(48) = rchmono2(48,j)
        pdvar(49) = rchmono2(49,j) 
        pdvar(50) = rchmono2(50,j)
        pdvar(51) = rchmono2(51,j) 
        pdvar(52) = rchmono2(52,j)
        pdvar(53) = rchmono2(53,j) 
        pdvar(54) = rchmono2(54,j)    
        pdvar(55) = rchmono2(55,j)    
        pdvar(56) =rchmono2(56,j)    
        pdvar(57) = rchmono2(57,j)  
        pdvar(58) = rchmono2(58,j)    
        pdvar(59) = rchmono2(59,j)   
        pdvar(60) = rchmono2(60,j)  
        pdvar(61) = rchmono2(61,j)   
        pdvar(62) = rchmono2(62,j)   
        pdvar(63) = rchmono2(63,j)   
        pdvar(64) = rchmono2(64,j)   
        pdvar(65) = rchmono2(65,j)  
        pdvar(66) = rchmono2(66,j)  
        pdvar(67) = rchmono2(67,j)  
        pdvar(68) = rchmono2(68,j)  
        pdvar(69) = rchmono2(69,j) 
        pdvar(70) = rchmono2(70,j) 
        pdvar(71) = rchmono2(71,j)  
        pdvar(72) = rchmono2(72,j)   
        pdvar(73) = rchmono2(73,j) 
        pdvar(74) = rchmono2(74,j)
        pdvar(75) = rchmono2(75,j)  
        pdvar(76) = rchmono2(76,j)
        pdvar(77) = rchmono2(77,j)   
        pdvar(78) = rchmono2(78,j)  
        pdvar(79) = rchmono2(79,j) 
        pdvar(80) = rchmono2(80,j)  
        pdvar(81) = rchmono2(81,j) 
        pdvar(82) = rchmono2(82,j)  
        pdvar(83) = rchmono2(83,j)  
        pdvar(84) = rchmono2(84,j)  
        pdvar(85) = rchmono2(85,j)  
        pdvar(86) = rchmono2(86,j) 
        pdvar(87) = rchmono2(87,j)   
        pdvar(88) = rchmono2(88,j)  
        pdvar(89) = rchmono2(89,j)  
        pdvar(90) = rchmono2(90,j)  
        pdvar(91) = rchmono2(91,j)   
        pdvar(92) = rchmono2(92,j)    
        pdvar(93) = rchmono2(93,j)    
        pdvar(94) = rchmono2(94,j)   
        pdvar(95) = rchmono2(95,j)   
        pdvar(96) = rchmono2(96,j)    / Real(mdays)   ! Concentration
    
       !if (ipdvar(1) > 0) then
          do ii = 1, mrcho2   !!itotr
            pdvr(ii) =pdvar(ii)  !! pdvar(ipdvar(ii))
          end do

          if (iscen == 1 .and. isproj == 0) then
          write (output2_rch_num,5000) j, subgis(j), mo_chk, rch_dakm(j),     (pdvr(ii), ii = 1,mrcho2)          !!-------------------used
                                  
         ! else if (isproj == 1) then
        !  write (20,5000) j, subgis(j), mo_chk, rch_dakm(j),            
     !&                                    (pdvr(ii), ii = 1, itotr)
      !    else if (iscen == 1 .and. isproj == 2) then 
      !    write (output_rch_num,6000) j, subgis(j), mo_chk, rch_dakm(j),             
     !&                              (pdvr(ii), ii = 1, itotr),iyr  
        !endif
    !   else
 !  increase to 44 in loops below from 42 gsm 10/17/2011      
       !   if (iscen == 1 .and. isproj == 0) then
       !   write (output2_rch_num,5000) j, subgis(j), mo_chk, rch_dakm(j),     (pdvar(ii), ii = 1, 39)              !!-------------------used   
                                  
     !     else if (isproj == 1) then
       !   write (20,5000) j, subgis(j), mo_chk, rch_dakm(j),            
     !&                                (pdvar(ii), ii = 1, 44)    
      !    else if (iscen == 1 .and. isproj == 2) then
      !    write (output_rch_num,6000) j, subgis(j), mo_chk, rch_dakm(j),             
     !&                              (pdvar(ii), ii = 1, 44), iyr     

          endif
       ! end if
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,e12.4,96e12.4)   
 
      end