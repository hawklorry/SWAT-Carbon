      subroutine rchaa2(years)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the average annual reach output to the .rch file

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
!!    mrcho        |none         |maximum number of variables written to
!!                               |reach output file (.rch)
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    rchaao2(1,:)  |m^3/s        |flow into reach during simulation
!!    rchaao2(2,:)  |m^3/s        |flow out of reach during simulation
!!    rchaao2(3,:)  |metric tons  |sediment transported into reach during 
!!                               |simulation
!!    rchaao2(4,:)  |metric tons  |sediment transported out of reach during 
!!                               |simulation
!!    rchaao2(5,:)  |mg/L         |sediment concentration in outflow during 
!!                               |simulation
!!    rchaao(6,:)  |kg N         |organic N transported into reach during 
!!                               |simulation
!!    rchaao(7,:)  |kg N         |organic N transported out of reach during
!!                               |simulation
!!    rchaao(8,:)  |kg P         |organic P transported into reach during 
!!                               |simulation
!!    rchaao(9,:)  |kg P         |organic P transported out of reach during
!!                               |simulation
!!    rchaao(10,:) |m^3/s        |evaporation from reach during simulation
!!    rchaao(11,:) |m^3/s        |transmission losses from reach during 
!!                               |simulation
!!    rchaao(12,:) |kg           |conservative metal #1 transported out of reach
!!                               |during simulation
!!    rchaao(13,:) |kg           |conservative metal #2 transported out of reach
!!                               |during simulation
!!    rchaao(14,:) |kg           |conservative metal #3 transported out of reach
!!                               |during simulation
!!    rchaao(15,:) |kg N         |nitrate transported into reach during 
!!                               |simulation
!!    rchaao(16,:) |kg N         |nitrate transported out of reach during 
!!                               |simulation
!!    rchaao(17,:) |kg P         |soluble P transported into reach during 
!!                               |simulation
!!    rchaao(18,:) |kg P         |soluble P transported out of reach during
!!                               |simulation
!!    rchaao(19,:) |mg pst       |soluble pesticide transported into reach
!!                               |during simulation
!!    rchaao(20,:) |mg pst       |soluble pesticide transported out of reach
!!                               |during simulation
!!    rchaao(21,:) |mg pst       |sorbed pesticide transported into reach during
!!                               |simulation
!!    rchaao(22,:) |mg pst       |sorbed pesticide transported out of reach
!!                               |during simulation
!!    rchaao(23,:) |mg pst       |amount of pesticide lost through reactions in
!!                               |reach during simulation
!!    rchaao(24,:) |mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during simulation
!!    rchaao(25,:) |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during simulation
!!    rchaao(26,:) |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during simulation
!!    rchaao(27,:) |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during simulation
!!    rchaao(28,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during simulation
!!    rchaao(29,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during simulation
!!    rchaao(30,:) |kg chla      |chlorophyll-a transported into reach during
!!                               |simulation
!!    rchaao(31,:) |kg chla      |chlorophyll-a transported out of reach during
!!                               |simulation
!!    rchaao(32,:) |kg N         |ammonia transported into reach during 
!!                               |simuation
!!    rchaao(33,:) |kg N         |ammonia transported out of reach during 
!!                               |simuation
!!    rchaao(34,:) |kg N         |nitrite transported into reach during
!!                               |simuation
!!    rchaao(35,:) |kg N         |nitrite transported out of reach during 
!!                               |simuation
!!    rchaao(36,:) |kg O2        |CBOD transported into reach during simulation
!!    rchaao(37,:) |kg O2        |CBOD transported out of reach during 
!!                               |simuation
!!    rchaao(38,:) |kg O2        |dissolved oxygen transported into reach during
!!                               |simuation
!!    rchaao(39,:) |kg O2        |dissolved oxygen transported out of reach
!!                               |during simulation
!!    rchaao(40,:) |kg bact      |persistent bacteria transported out of reach
!!                               |during simulation
!!    rchaao(41,:) |kg bact      |less persistent bacteria transported out of
!!                               |reach during simulation
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchaao(43,:) |kg           |Total N (org N + no3 + no2 + nh4 outs)
!!    rchaao(44,:) |kg           |Total P (org P + sol p outs)

!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    pdvar(:)    |varies        |array of default reach output values
!!    pdvr(:)     |varies        |array of custom reach output values
!!    srch_av(:)  |varies        |annual reach inflow/outflow 
!!    years       |years         |length of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log10

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
        
      real, intent (in) :: years
      integer :: j, ii
      real, dimension (mrcho2) :: pdvar, pdvr
      real, dimension (2) :: srch_av

      do j = 1, subtot

       
        pdvar = 0. 
        pdvr = 0.
    
        !! assign average annual values
   
        pdvar(1) = rchaao2(1,j)    
        pdvar(2) = rchaao2(2,j)     
        pdvar(3) = rchaao2(3,j)     
        pdvar(4) = rchaao2(4,j)  
        pdvar(5) = rchaao2(5,j)    
        pdvar(6) = rchaao2(6,j)    
        pdvar(7) = rchaao2(7,j)   
        pdvar(8) = rchaao2(8,j)    
        pdvar(9) = rchaao2(9,j)    
        pdvar(10) = rchaao2(10,j)   
        pdvar(11) = rchaao2(11,j)  
        pdvar(12) = rchaao2(12,j)  
        pdvar(13) = rchaao2(13,j)  
        pdvar(14) = rchaao2(14,j)  
        pdvar(15) = rchaao2(15,j) 
        pdvar(16) = rchaao2(16,j)  
        pdvar(17) = rchaao2(17,j)  
        pdvar(18) = rchaao2(18,j)  
        pdvar(19) = rchaao2(19,j)  
        pdvar(20) = rchaao2(20,j)  
        pdvar(21) = rchaao2(21,j)  
        pdvar(22) = rchaao2(22,j)  
        pdvar(23) = rchaao2(23,j) 
        pdvar(24) = rchaao2(24,j) 
        pdvar(25) = rchaao2(25,j)    
        pdvar(26) = rchaao2(26,j)  
        pdvar(27) = rchaao2(27,j) 
        pdvar(28) = rchaao2(28,j)  
        pdvar(29) = rchaao2(29,j)  
        pdvar(30) = rchaao2(30,j)  
        pdvar(31) = rchaao2(31,j)  
        pdvar(32) = rchaao2(32,j)  
        pdvar(33) = rchaao2(33,j)  
        pdvar(34) = rchaao2(34,j)  
        pdvar(35) = rchaao2(35,j)  
        pdvar(36) = rchaao2(36,j) 
        pdvar(37) = rchaao2(37,j)
        pdvar(38) = rchaao2(38,j)  
        pdvar(39) = rchaao2(39,j)  
        pdvar(40) = rchaao2(40,j)
        pdvar(41) = rchaao2(41,j) 
        pdvar(42) = rchaao2(42,j) 
        pdvar(43) = rchaao2(43,j)  
        pdvar(44) = rchaao2(44,j)  
        pdvar(45) = rchaao2(45,j)   
        pdvar(46) = rchaao2(46,j)  
        pdvar(47) = rchaao2(47,j)
        pdvar(48) = rchaao2(48,j)  
        pdvar(49) = rchaao2(49,j)   
        pdvar(50) = rchaao2(50,j)  
        pdvar(51) = rchaao2(51,j) 
        pdvar(52) = rchaao2(52,j)
        pdvar(53) = rchaao2(53,j) 
        pdvar(54) = rchaao2(54,j)   
        pdvar(55) = rchaao2(55,j)    
        pdvar(56) = rchaao2(56,j)    
        pdvar(57) = rchaao2(57,j)  
        pdvar(58) = rchaao2(58,j)   
        pdvar(59) = rchaao2(59,j)    
        pdvar(60) = rchaao2(60,j)  
        pdvar(61) = rchaao2(61,j)   
        pdvar(62) = rchaao2(62,j)   
        pdvar(63) = rchaao2(63,j)  
        pdvar(64) = rchaao2(64,j)   
        pdvar(65) = rchaao2(65,j) 
        pdvar(66) = rchaao2(66,j)  
        pdvar(67) = rchaao2(67,j)  
        pdvar(68) = rchaao2(68,j) 
        pdvar(69) = rchaao2(69,j)  
        pdvar(70) = rchaao2(70,j) 
        pdvar(71) = rchaao2(71,j)  
        pdvar(72) = rchaao2(72,j) 
        pdvar(73) = rchaao2(73,j) 
        pdvar(74) = rchaao2(74,j)  
        pdvar(75) = rchaao2(75,j)  
        pdvar(76) = rchaao2(76,j)  
        pdvar(77) = rchaao2(77,j) 
        pdvar(78) = rchaao2(78,j)  
        pdvar(79) = rchaao2(79,j)  
        pdvar(80) = rchaao2(80,j)  
        pdvar(81) = rchaao2(81,j)  
        pdvar(82) = rchaao2(82,j)  
        pdvar(83) = rchaao2(83,j)  
        pdvar(84) = rchaao2(84,j)  
        pdvar(85) = rchaao2(85,j)  
        pdvar(86) = rchaao2(86,j)  
        pdvar(87) = rchaao2(87,j)  
        pdvar(88) = rchaao2(88,j)  
        pdvar(89) = rchaao2(89,j)  
        pdvar(90) = rchaao2(90,j)  
        pdvar(91) = rchaao2(91,j)  
        pdvar(92) = rchaao2(92,j)  
        pdvar(93) = rchaao2(93,j)  
        pdvar(94) = rchaao2(94,j)  
        pdvar(95) = rchaao2(95,j)  
        pdvar(96) = rchaao2(96,j)  
        
        
        !if (ipdvar(1) > 0) then
          do ii = 1,mrcho2    !itotr
            pdvr(ii) = pdvar(ii)  !1 pdvar(ipdvar(ii))
          end do
          if (iscen == 1 .and. isproj == 0) then
          write (output2_rch_num,5000) j, subgis(j), years, rch_dakm(j),        (pdvr(ii), ii = 1,mrcho2)         
                           
   !       else if (isproj == 1) then
  !        write (20,5000) j, subgis(j), years, rch_dakm(j),             
  !   &                                    (pdvr(ii), ii = 1, itotr)
  !        else if (iscen == 1 .and. isproj == 2) then
    !      write (output_rch_num,6000) j, subgis(j), years, rch_dakm(j),              
    ! &                             (pdvr(ii), ii = 1, itotr), iyr  
       !   endif
      !  else
!!  increase to 44 in loops below from 42 gsm 10/17/2011
     !     if (iscen == 1 .and. isproj == 0) then
     !     write (output2_rch_num,5000) j, subgis(j), years, rch_dakm(j),     (pdvar(ii), ii = 1, 39)                                          
    !      else if (isproj == 1) then
    !      write (20,5000) j, subgis(j), years, rch_dakm(j),             
   !  &                                (pdvar(ii), ii = 1, 44)    
    !      else if (iscen == 1 .and. isproj == 2) then
    !      write (output_rch_num,6000) j, subgis(j), years, rch_dakm(j),              
  !   &                             (pdvar(ii), ii = 1, 44), iyr      
          endif
       ! end if
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,f5.1,e12.4,96e12.4)     

      end