      subroutine rchday2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the daily reach output to the .rch file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida         |julian date  |current day of simulation
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
!!    rchdy(1,:)   |m^3/s        |flow into reach on day
!!    rchdy(2,:)   |m^3/s        |flow out of reach on day
!!    rchdy(3,:)   |m^3/s        |evaporation from reach on day
!!    rchdy(4,:)   |m^3/s        |transmission losses from reach on day
!!    rchdy(5,:)   |metric tons  |sediment transported into reach on day
!!    rchdy(6,:)   |metric tons  |sediment transported out of reach on day
!!    rchdy(7,:)   |mg/L         |sediment concentration in outflow
!!    rchdy(8,:)   |kg N         |organic N transported into reach on day
!!    rchdy(9,:)   |kg N         |organic N transported out of reach on day
!!    rchdy(10,:)  |kg P         |organic P transported into reach on day
!!    rchdy(11,:)  |kg P         |organic P transported out of reach on day
!!    rchdy(12,:)  |kg N         |nitrate transported into reach on day
!!    rchdy(13,:)  |kg N         |nitrate transported out of reach on day
!!    rchdy(14,:)  |kg N         |ammonia transported into reach on day
!!    rchdy(15,:)  |kg N         |ammonia transported out of reach on day
!!    rchdy(16,:)  |kg N         |nitrite transported into reach on day
!!    rchdy(17,:)  |kg N         |nitrite transported out of reach on day
!!    rchdy(18,:)  |kg P         |soluble P transported into reach on day
!!    rchdy(19,:)  |kg P         |soluble P transported out of reach on day
!!    rchdy(20,:)  |kg chla      |chlorophyll-a transported into reach on day
!!    rchdy(21,:)  |kg chla      |chlorophyll-a transported out of reach on day
!!    rchdy(22,:)  |kg O2        |CBOD transported into reach on day
!!    rchdy(23,:)  |kg O2        |CBOD transported out of reach on day
!!    rchdy(24,:)  |kg O2        |dissolved oxygen transported into reach on day
!!    rchdy(25,:)  |kg O2        |dissolved oxygen transported out of reach on
!!                               |day
!!    rchdy(26,:)  |mg pst       |soluble pesticide transported into reach on
!!                               |day
!!    rchdy(27,:)  |mg pst       |soluble pesticide transported out of reach on
!!                               |day
!!    rchdy(28,:)  |mg pst       |sorbed pesticide transported into reach on day
!!    rchdy(29,:)  |mg pst       |sorbed pesticide transported out of reach on
!!                               |day
!!    rchdy(30,:)  |mg pst       |amount of pesticide lost through reactions in
!!                               |reach on day
!!    rchdy(31,:)  |mg pst       |amount of pesticide lost through
!!                               |volatilization from reach on day
!!    rchdy(32,:)  |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment on day
!!    rchdy(33,:)  |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach on day
!!    rchdy(34,:)  |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment on day
!!    rchdy(35,:)  |mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions on day
!!    rchdy(36,:)  |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial on day
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchdy(38,:)  |kg bact      |persistent bacteria transported out of reach
!!                               |on day
!!    rchdy(39,:)  |kg bact      |less persistent bacteria transported out of
!!                               |reach on day
!!    rchdy(40,:)  |kg           |amount of conservative metal #1 transported
!!                               |out of reach on day
!!    rchdy(41,:)  |kg           |amount of conservative metal #2 transported
!!                               |out of reach on day
!!    rchdy(42,:)  |kg           |amount of conservative metal #3 transported
!!                               |out of reach on day
!!    rchday(43,:) |kg           |Total N (org N + no3 + no2 + nh4 outs)
!!    rchday(44,:) |kg           |Total P (org P + sol p outs)

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
!!    srch_av(:)  |varies        |daily flow values for reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use carbon_para   !! added by Du
      use parm
      use parm_output 
      use parm_control
      implicit none
      
      integer :: j,kk,ii
      real, dimension (mrcho2) :: pdvar, pdvr
     ! real, dimension (2) :: srch_av

      do j = 1, subtot

     
        pdvar = 0.
        pdvr = 0.

        !! assign daily values
        pdvar(1) =  rchdy2(1,j)      
        pdvar(2) =  rchdy2(2,j)      
        pdvar(3) = rchdy2(3,j)       
        pdvar(4) = rchdy2(4,j)    
        pdvar(5) = rchdy2(5,j)      
        pdvar(6) = rchdy2(6,j)     
        pdvar(7) = rchdy2(7,j)     
        pdvar(8) = rchdy2(8,j)      
        pdvar(9) = rchdy2(9,j)      
        pdvar(10) = rchdy2(10,j)     
        pdvar(11) = rchdy2(11,j)    
        pdvar(12) = rchdy2(12,j)    
        pdvar(13) = rchdy2(13,j)   
        pdvar(14) = rchdy2(14,j)    
        pdvar(15) = rchdy2(15,j)    
        pdvar(16) = rchdy2(16,j)  
        pdvar(17) = rchdy2(17,j)     
        pdvar(18) = rchdy2(18,j)    
        pdvar(19) = rchdy2(19,j)     
        pdvar(20) = rchdy2(20,j)    
        pdvar(21) = rchdy2(21,j)  
        pdvar(22) = rchdy2(22,j)   
        pdvar(23) = rchdy2(23,j)     
        pdvar(24) = rchdy2(24,j)    
        pdvar(25) = rchdy2(25,j)     
        pdvar(26) = rchdy2(26,j)    
        pdvar(27) = rchdy2(27,j)     
        pdvar(28) = rchdy2(28,j)  
        pdvar(29) = rchdy2(29,j)    
        pdvar(30) = rchdy2(30,j)    
        pdvar(31) = rchdy2(31,j)     
        pdvar(32) = rchdy2(32,j)   
        pdvar(33) = rchdy2(33,j)     
        pdvar(34) = rchdy2(34,j)    
        pdvar(35) = rchdy2(35,j)   
        pdvar(36) = rchdy2(36,j) 
        pdvar(37) = rchdy2(37,j)  
        pdvar(38) = rchdy2(38,j)     
        pdvar(39) = rchdy2(39,j)    
        pdvar(40) = rchdy2(40,j)     
        pdvar(41) = rchdy2(41,j)   
        pdvar(42) = rchdy2(42,j)     
        pdvar(43) = rchdy2(43,j)   
        pdvar(44) = rchdy2(44,j)    
        pdvar(45) = rchdy2(45,j)   
        pdvar(46) = rchdy2(46,j) 
        pdvar(47) = rchdy2(47,j)  
        pdvar(48) = rchdy2(48,j)     
        pdvar(49) = rchdy2(49,j)    
        pdvar(50) = rchdy2(50,j)     
        pdvar(51) = rchdy2(51,j)   
        pdvar(52) = rchdy2(52,j)     
        pdvar(53) = rchdy2(53,j)  
        pdvar(54) = rchdy2(54,j)     
        pdvar(55) = rchdy2(55,j)    
        pdvar(56) = rchdy2(56,j)     
        pdvar(57) = rchdy2(57,j)    
        pdvar(58) = rchdy2(58,j)     
        pdvar(59) = rchdy2(59,j)  
        pdvar(60) = rchdy2(60,j)    
        pdvar(61) = rchdy2(61,j)    
        pdvar(62) = rchdy2(62,j)     
        pdvar(63) = rchdy2(63,j)   
        pdvar(64) = rchdy2(64,j)     
        pdvar(65) = rchdy2(65,j)    
        pdvar(66) = rchdy2(66,j)   
        pdvar(67) = rchdy2(67,j) 
        pdvar(68) = rchdy2(68,j)  
        pdvar(69) = rchdy2(69,j)     
        pdvar(70) = rchdy2(70,j)    
        pdvar(71) = rchdy2(71,j)     
        pdvar(72) = rchdy2(72,j)   
        pdvar(73) = rchdy2(73,j)     
        pdvar(74) = rchdy2(74,j)   
        pdvar(75) = rchdy2(75,j)    
        pdvar(76) = rchdy2(76,j)   
        pdvar(77) = rchdy2(77,j) 
        pdvar(78) = rchdy2(78,j)  
        pdvar(79) = rchdy2(79,j)     
        pdvar(80) = rchdy2(80,j)    
        pdvar(81) = rchdy2(81,j)     
        pdvar(82) = rchdy2(82,j)   
        pdvar(83) = rchdy2(83,j)     
        pdvar(84) = rchdy2(84,j)  
        pdvar(85) = rchdy2(85,j)  
        pdvar(86) = rchdy2(86,j)  
        pdvar(87) = rchdy2(87,j) 
        pdvar(88) = rchdy2(88,j) 
        pdvar(89) = rchdy2(89,j) 
        pdvar(90) = rchdy2(90,j) 
        pdvar(91) = rchdy2(91,j)  
        pdvar(92) = rchdy2(92,j)  
        pdvar(93) = rchdy2(93,j) 
        pdvar(94) = rchdy2(94,j) 
        pdvar(95) = rchdy2(95,j) 
        pdvar(96) = rchdy2(96,j) 
        
        
!!  compute month and day given julian day
        call xmon 
        
  !       print out subdaily reach output in output2.rch 
      if (ievent_rch==1 .and. iprint==3) then
       !if (ipdvar(1) > 0) then
         do kk=1,nstep_rch
          do ii = 1,mrcho2  !! itotr
             pdvr(ii) = rchhr2(ii,j,kk)
          end do
         if (iscen == 1 .and. isproj == 0) then
             write (output2_rch_num,5001) j, subgis(j), iida, kk, rch_dakm(j),   (pdvr(ii), ii = 1,mrcho2)                              
         end if
         end do
	  !else
	   ! if (iscen == 1 .and. isproj == 0) then
         !  do kk=1,nstep_rch
         !    write (output_rch_num,5001) j, subgis(j), iida, kk, rch_dakm(j),      (rchhr(ii,j,kk), ii = 1, 7)     
                            
	   !  end do
	   !  endif
	  !!endif
	  
	  
      else
      
      
      !  if (ipdvar(1) > 0) then                
          do ii = 1, mrcho2  !itotr
            pdvr(ii) = pdvar(ii)  !1pdvar(ipdvar(ii))
          end do
          if (iscen == 1 .and. isproj == 0) then
            if (icalen == 0) write (output2_rch_num,5000) j, subgis(j), iida,   rch_dakm(j), (pdvr(ii), ii = 1, mrcho2)             !!-----------------------used----------------------------------
             
!            if(icalen == 1)write (output_rch_num,5002) j, subgis(j), i_mo, icl(iida),
 !    &             iyr, rch_dakm(j), (pdvr(ii), ii = 1, itotr)
!!    added for binary files 3/25/09 gsm line below and write (77777
!	      if (ia_b == 1) then
  !           write (77777) j, subgis(j), iida, rch_dakm(j),             
  !   &                                         (pdvr(ii), ii = 1, itotr)
 !           endif	        
   !       else if (isproj == 1) then
  !        write (20,5000) j, subgis(j), iida, rch_dakm(j),              
  !   &                                    (pdvr(ii), ii = 1, itotr)
   !       else if (iscen == 1 .and. isproj == 2) then
  !        if(icalen == 0)write (output_rch_num,6000) j, subgis(j), iida, rch_dakm(j),
  !   &                               (pdvr(ii), ii = 1, itotr),iyr 
  !        if (icalen == 1) write (output_rch_num,6002) j, subgis(j), i_mo, icl(iida),
   !  &          iyr, rch_dakm(j),(pdvr(ii), ii = 1, itotr), iyr
      !  endif     !!  if (iscen == 1 .and. isproj == 0) then
      !  else
        
  !  increase to 45 in loops below from 42 gsm 10/26/2011      
      !    if (iscen == 1 .and. isproj == 0) then
      !    if (icalen == 0)write(1996,5000) j, subgis(j), iida, rch_dakm(j),      (pdvar(ii), ii = 1, 39)                         !--------------------USED-----------------------
                                        
!          if (icalen == 1) write (7,5002) j, subgis(j), i_mo, icl(iida),
 !    &            iyr, rch_dakm(j),(pdvar(ii), ii = 1, 45)

!!    added for binary files 3/25/09 gsm line below and write (77777
  !           if (ia_b == 1) then
  !              write (77777) j, subgis(j), iida, rch_dakm(j),          
 !    &                                         (pdvr(ii), ii = 1, itotr)
 !            endif	    
             
    !      else if (isproj == 1) then
    !      write (20,5000) j, subgis(j), iida, rch_dakm(j),              
    ! &                                        (pdvar(ii), ii = 1, 45) 
   !       else if (iscen == 1 .and. isproj == 2) then
   !       if (icalen == 0)write(output_rch_num,6000) j, subgis(j), iida, rch_dakm(j),
   !  &                               (pdvar(ii), ii = 1, 45), iyr 
   !       if (icalen == 1) write (output_rch_num,6002) j, subgis(j), i_mo, icl(iida),
    ! &              iyr, rch_dakm(j), (pdvar(ii), ii = 1, 45)
         endif    !!    if (iscen == 1 .and. isproj == 0) then
      !  end if            !  if (ipdvar(1) > 0) then  
      endif       ! if (ievent==1.and.iprint==3) then
     
     
     
      end do     !! do j = 1, subtot
     
     
     
      return

 5000 format ('REACH ',i4,1x,i8,1x,i5,e12.4,96e12.4)     
 5001 format ('REACH ',i4,1x,i8,1x,i5,1x,i5,e12.4,96e12.4)  
      end