      subroutine hruaa_N(years)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual HRU output to the output.hru file


      
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
      integer :: j, sb, ii, iflag, idplant
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      do j = 1, nhru
        sb = hru_sub(j)

        iflag = 0
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do


        if (iflag == 1) then
        pdvas = 0.
        pdvs = 0.

        
        pdvas(1) = 0. !hruaaN(1,j) 
        pdvas(2) = 0. !hruaaN(2,j) 
        pdvas(3) = 0. !hruaaN(3,j) 
        pdvas(4) = 0. !hruaaN(4,j) 
        pdvas(5) = 0. !hruaaN(5,j)
        pdvas(6) = 0. !hruaaN(6,j)
        pdvas(7) = hruaaN(7,j)
        pdvas(8) = hruaaN(8,j)
        pdvas(9) = hruaaN(9,j)
        pdvas(10) = hruaaN(10,j)
        pdvas(11) = hruaaN(11,j)
        pdvas(12) = hruaaN(12,j)
        pdvas(13) = hruaaN(13,j)
        pdvas(14) = hruaaN(14,j)
        pdvas(15) = hruaaN(15,j)
        pdvas(16) = hruaaN(16,j)
        pdvas(17) = hruaaN(17,j)
        pdvas(18) = hruaaN(18,j)
        pdvas(19) = hruaaN(19,j)
        pdvas(20) = hruaaN(20,j)
        pdvas(21) = hruaaN(21,j)
        pdvas(22) = hruaaN(22,j)
        pdvas(23) = hruaaN(23,j)
        pdvas(24) = hruaaN(24,j)
        pdvas(25) = hruaaN(25,j)
        pdvas(26) = hruaaN(26,j)
        pdvas(27) = hruaaN(27,j) 
        pdvas(28) = hruaaN(28,j)
        pdvas(29) = hruaaN(29,j)
        pdvas(30) = hruaaN(30,j)
        pdvas(31) = hruaaN(31,j)
        pdvas(32) = hruaaN(32,j)
        pdvas(33) = hruaaN(33,j)
        pdvas(34) = hruaaN(34,j)
        pdvas(35) = hruaaN(35,j)
        pdvas(36) = hruaaN(36,j)
        pdvas(37) = hruaaN(37,j)
        pdvas(38) = hruaaN(38,j)
        pdvas(39) = hruaaN(39,j)
        pdvas(40) = hruaaN(40,j)
        pdvas(41) = hruaaN(41,j)
        pdvas(42) = hruaaN(42,j)
        pdvas(43) = hruaaN(43,j)
        pdvas(44) = hruaaN(44,j)
        pdvas(45) = hruaaN(45,j)
        pdvas(46) = hruaaN(46,j)
        pdvas(47) = hruaaN(47,j)
        pdvas(48) = hruaaN(48,j)
        pdvas(49) = hruaaN(49,j)
        pdvas(50) = hruaaN(50,j)
        pdvas(51) = hruaaN(51,j)
        pdvas(52) = hruaaN(52,j)
        pdvas(53) = hruaaN(53,j)
        

       ! if (ipdvas(1) > 0) then
         ! do ii = 1, itots
         !   pdvs(ii) = pdvas(ipdvas(ii))
         ! end do

        idplant = idplt(j)
        if (idplant > 0) then
          cropname = cpnm(idplant)
        else
          cropname = "NOCR"
        endif


        write (output_N_hru_num,1002) cropname, j, subnum(j),        &
          hruno(j), sb, nmgt(j), 0, 0, years, hru_km(j),     &
          (pdvas(ii), ii = 1, 53)
1002  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,f4.1,1x,e10.5,       &
      53f10.3)   

          !if (iscen == 1 .and. isproj == 0) then
         ! write (282,1000) cropname, j, subnum(j), hruno(j), sb,   &       
         !   nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, 53)
      !    else if (isproj == 1) then
      !    write (21,1000) cropname, j, subnum(j), hruno(j),             
   !  &    sb, nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots)
   !       else if (iscen == 1 .and. isproj == 2) then
      !    write (output_hru_num,2000) cropname, j, subnum(j), hruno(j), sb,         
    ! &    nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
       !endif
       ! else
          !if (iscen == 1 .and. isproj == 0) then
         ! write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,         
   !  &            nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        !  else if (isproj == 1) then
        !  write (21,1001) cropname, j, subnum(j), hruno(j),             
   !  &         sb, nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo)
   !       else if (iscen == 1 .and. isproj == 2) then
   !       write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,         
   !  &    nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
          !endif
       ! end if
        end if
      end do

      return

1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,f4.1,1x,e10.4,53f10.3)
        
     !2000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,f4.1,e10.5,66f10.3,1x,
  !   *e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
!1001  format (a4,i7,1x,a5,a4,i5,1x,i4,1x,f4.1,e10.5,66f10.3,1x,
 !    *e10.5,1x,e10.5,8e10.3,3f10.3,1x,i4,66f10.3)       
      
      end