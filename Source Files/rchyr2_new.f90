      subroutine rchyr2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the annual reach output to the .rch file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |julian date  |current day of simulation
!!    id1          |julian date  |first day of simulation in current year
!!    ilog         |none         |streamflow print code
!!                               |0 print streamflow in reach
!!                               |1 print Log10 streamflow in reach
!!    ipdvar(:)    |none         |output variable codes for .rch file
!!    isproj       |none         |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotr        |none         |number of output variables printed (.rch)
!!    iyr          |year         |current year of simulation (eg 1980)
!!    mrcho        |none         |maximum number of variables written to
!!                               |reach output file (.rch)
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchyro(1,:)  |m^3/s        |flow into reach during year
!!    rchyro(2,:)  |m^3/s        |flow out of reach during year
!!    rchyro(3,:)  |metric tons  |sediment transported into reach during year
!!    rchyro(4,:)  |metric tons  |sediment transported out of reach during year
!!    rchyro(5,:)  |mg/L         |sediment concentration in outflow during year
!!    rchyro(6,:)  |kg N         |organic N transported into reach during year
!!    rchyro(7,:)  |kg N         |organic N transported out of reach during
!!                               |year
!!    rchyro(8,:)  |kg P         |organic P transported into reach during year
!!    rchyro(9,:)  |kg P         |organic P transported out of reach during
!!                               |year
!!    rchyro(10,:) |m^3/s        |evaporation from reach during year
!!    rchyro(11,:) |m^3/s        |transmission losses from reach during year
!!    rchyro(12,:) |kg           |conservative metal #1 transported out of reach
!!                               |during year
!!    rchyro(13,:) |kg           |conservative metal #2 transported out of reach
!!                               |during year
!!    rchyro(14,:) |kg           |conservative metal #3 transported out of reach
!!                               |during year
!!    rchyro(15,:) |kg N         |nitrate transported into reach during year
!!    rchyro(16,:) |kg N         |nitrate transported out of reach during year
!!    rchyro(17,:) |kg P         |soluble P transported into reach during year
!!    rchyro(18,:) |kg P         |soluble P transported out of reach during
!!                               |year
!!    rchyro(19,:) |mg pst       |soluble pesticide transported into reach
!!                               |during year
!!    rchyro(20,:) |mg pst       |soluble pesticide transported out of reach
!!                               |during year
!!    rchyro(21,:) |mg pst       |sorbed pesticide transported into reach during
!!                               |year
!!    rchyro(22,:) |mg pst       |sorbed pesticide transported out of reach
!!                               |during year
!!    rchyro(23,:) |mg pst       |amount of pesticide lost through reactions in
!!                               |reach during year
!!    rchyro(24,:) |mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during year
!!    rchyro(25,:) |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during year
!!    rchyro(26,:) |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during year
!!    rchyro(27,:) |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during year
!!    rchyro(28,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during year
!!    rchyro(29,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during year
!!    rchyro(30,:) |kg chla      |chlorophyll-a transported into reach during
!!                               |year
!!    rchyro(31,:) |kg chla      |chlorophyll-a transported out of reach during
!!                               |year
!!    rchyro(32,:) |kg N         |ammonia transported into reach during year
!!    rchyro(33,:) |kg N         |ammonia transported out of reach during year
!!    rchyro(34,:) |kg N         |nitrite transported into reach during year
!!    rchyro(35,:) |kg N         |nitrite transported out of reach during year
!!    rchyro(36,:) |kg O2        |CBOD transported into reach during year
!!    rchyro(37,:) |kg O2        |CBOD transported out of reach during year
!!    rchyro(38,:) |kg O2        |dissolved oxygen transported into reach during
!!                               |year
!!    rchyro(39,:) |kg O2        |dissolved oxygen transported out of reach
!!                               |during year
!!    rchyro(40,:) |kg bact      |persistent bacteria transported out of reach
!!                               |during year
!!    rchyro(41,:) |kg bact      |less persistent bacteria transported out of
!!                               |reach during year
!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idlast      |none          |number of days simulated in year
!!    ii          |none          |counter
!!    j           |none          |(counter) reach number
!!    pdvar(:)    |varies        |array of default reach output values
!!    pdvr(:)     |varies        |array of custom reach output values
!!    srch_av(:)  |varies        |annual reach inflow/outflow 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para   !! added by Du
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
        implicit none
        
      integer :: j, ii
      real, dimension (mrcho2) :: pdvar, pdvr
      real, dimension (2) :: srch_av

      idlast = 0
      idlast = i - (id1 - 1)
      

      do j = 1, subtot


        pdvar = 0. 
        pdvr = 0.
  
        pdvar(1) = rchyro2(1,j)  
        pdvar(2) = rchyro2(2,j)   
        pdvar(3) =rchyro2(3,j)    
        pdvar(4) = rchyro2(4,j)  
        pdvar(5) = rchyro2(5,j)   
        pdvar(6) = rchyro2(6,j)    
        pdvar(7) = rchyro2(7,j)   
        pdvar(8) = rchyro2(8,j)   
        pdvar(9) = rchyro2(9,j)   
        pdvar(10) = rchyro2(10,j)   
        pdvar(11) = rchyro2(11,j) 
        pdvar(12) = rchyro2(12,j) 
        pdvar(13) = rchyro2(13,j)   
        pdvar(14) = rchyro2(14,j)   
        pdvar(15) = rchyro2(15,j)  
        pdvar(16) = rchyro2(16,j) 
        pdvar(17) = rchyro2(17,j)  
        pdvar(18) = rchyro2(18,j)  
        pdvar(19) = rchyro2(19,j) 
        pdvar(20) = rchyro2(20,j)  
        pdvar(21) = rchyro2(21,j)   
        pdvar(22) = rchyro2(22,j) 
        pdvar(23) = rchyro2(23,j) 
        pdvar(24) = rchyro2(24,j)  
        pdvar(25) = rchyro2(25,j)   
        pdvar(26) = rchyro2(26,j)  
        pdvar(27) = rchyro2(27,j) 
        pdvar(28) = rchyro2(28,j)  
        pdvar(29) = rchyro2(29,j)  
        pdvar(30) = rchyro2(30,j)  
        pdvar(31) = rchyro2(31,j)  
        pdvar(32) = rchyro2(32,j) 
        pdvar(33) = rchyro2(33,j)  / Real(idlast)   !|none          |number of days simulated in year
        pdvar(34) = rchyro2(34,j)  / Real(idlast)   !|none          |number of days simulated in year
        pdvar(35) = rchyro2(35,j) / Real(idlast)    !|none          |number of days simulated in year
        pdvar(36) = rchyro2(36,j)  / Real(idlast)   !|none          |number of days simulated in year
        pdvar(37) = rchyro2(37,j) / Real(idlast)    !|none          |number of days simulated in year
        pdvar(38) = rchyro2(38,j)  / Real(idlast)   !|none          |number of days simulated in year
        pdvar(39) = rchyro2(39,j)  
        pdvar(40) = rchyro2(40,j)
        pdvar(41) = rchyro2(41,j)
        pdvar(42) = rchyro2(42,j) 
        pdvar(43) = rchyro2(43,j)/ Real(idlast)  !|none          |number of days simulated in year
        pdvar(44) = rchyro2(44,j)  
        pdvar(45) = rchyro2(45,j)  
        pdvar(46) = rchyro2(46,j)  
        pdvar(47) = rchyro2(47,j)
        pdvar(48) = rchyro2(48,j)  
        pdvar(49) = rchyro2(49,j)  
        pdvar(50) = rchyro2(50,j)  
        pdvar(51) = rchyro2(51,j)  
        pdvar(52) = rchyro2(52,j)  
        pdvar(53) = rchyro2(53,j)  
        pdvar(54) = rchyro2(54,j)  
        pdvar(55) = rchyro2(55,j)     
        pdvar(56) = rchyro2(56,j)     
        pdvar(57) = rchyro2(57,j)  
        pdvar(58) = rchyro2(58,j)    
        pdvar(59) = rchyro2(59,j)   
        pdvar(60) = rchyro2(60,j)   
        pdvar(61) = rchyro2(61,j)  
        pdvar(62) = rchyro2(62,j)   
        pdvar(63) = rchyro2(63,j)  
        pdvar(64) = rchyro2(64,j)   
        pdvar(65) = rchyro2(65,j)  
        pdvar(66) = rchyro2(66,j)  
        pdvar(67) = rchyro2(67,j)  
        pdvar(68) = rchyro2(68,j) 
        pdvar(69) = rchyro2(69,j) 
        pdvar(70) = rchyro2(70,j)  
        pdvar(71) = rchyro2(71,j) 
        pdvar(72) = rchyro2(72,j) 
        pdvar(73) = rchyro2(73,j)  
        pdvar(74) = rchyro2(74,j)   
        pdvar(75) = rchyro2(75,j)    
        pdvar(76) = rchyro2(76,j)  
        pdvar(77) = rchyro2(77,j)  
        pdvar(78) = rchyro2(78,j)  
        pdvar(79) = rchyro2(79,j)  
        pdvar(80) = rchyro2(80,j) 
        pdvar(81) = rchyro2(81,j) 
        pdvar(82) = rchyro2(82,j)  
        pdvar(83) = rchyro2(83,j)  
        pdvar(84) = rchyro2(84,j)  
        pdvar(85) = rchyro2(85,j)  
        pdvar(86) = rchyro2(86,j)  
        pdvar(87) = rchyro2(87,j)   
        pdvar(88) = rchyro2(88,j)  
        pdvar(89) = rchyro2(89,j)  
        pdvar(90) = rchyro2(90,j) 
        pdvar(91) = rchyro2(91,j) 
        pdvar(92) = rchyro2(92,j)  
        pdvar(93) = rchyro2(93,j) 
        pdvar(94) = rchyro2(94,j) 
        pdvar(95) = rchyro2(95,j) 
        pdvar(96) = rchyro2(96,j)  / Real(idlast)  
        
        
        
        
        ! if (ipdvar(1) > 0) then
          do ii = 1,mrcho2  ! itotr
            pdvr(ii) =  pdvar(ii)   !pdvar(ipdvar(ii))
          end do
          if (iscen == 1 .and. isproj == 0) then                                             !!-------------------------USED-
          write (output2_rch_num,5000) j, subgis(j), iyr, rch_dakm(j),      (pdvr(ii), ii = 1, mrcho2)              
                                  
  !        else if (isproj == 1) then
  !        write (20,5000) j, subgis(j), iyr, rch_dakm(j),               
  !   &                                    (pdvr(ii), ii = 1, itotr)
  !        else if (iscen == 1 .and. isproj == 2) then
   !       write (output_rch_num,6000) j, subgis(j), iyr, rch_dakm(j),                
   !  &                             (pdvr(ii), ii = 1, itotr), iyr 
      !    endif
      !  else
     !!  increase to 44 in loops below from 42 gsm 10/17/2011
     !     if (iscen == 1 .and. isproj == 0) then
     !     write (output2_rch_num,5000) j, subgis(j), iyr, rch_dakm(j),      (pdvar(ii), ii = 1, 39)                     !!-------------------------USED-
                            
   !       else if (isproj == 1) then
    !      write (20,5000) j, subgis(j), iyr, rch_dakm(j),               
    ! &                                (pdvar(ii), ii = 1, 44)    
   !       else if (iscen == 1 .and. isproj == 2) then
   !       write (output_rch_num,6000) j, subgis(j), iyr, rch_dakm(j),                
    ! &                             (pdvar(ii), ii = 1, 44), iyr     
     
          endif
       ! end if
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,e12.4,96e12.4)             
 
      end