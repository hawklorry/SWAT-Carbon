      subroutine hruyr_N

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual HRU output to the output.hru file

     
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
        implicit none
        
      integer :: j, sb, ii, iflag, idplant
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      do j = 1, nhru
        iflag = 0
	    sb = hru_sub(j)
	  
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do

        if (iflag == 1) then

        pdvas = 0.
        pdvs = 0.
     
        pdvas(1) = hruyrN(1,j)/ Real(366 - leapyr)
        pdvas(2) = hruyrN(2,j)/ Real(366 - leapyr)
        pdvas(3) = hruyrN(3,j)/ Real(366 - leapyr)
        pdvas(4) = hruyrN(4,j)/ Real(366 - leapyr)
        pdvas(5) = hruyrN(5,j)/ Real(366 - leapyr)
        pdvas(6) = hruyrN(6,j)/ Real(366 - leapyr)
        pdvas(7) = hruyrN(7,j)
        pdvas(8) = hruyrN(8,j)
        pdvas(9) = hruyrN(9,j)
        pdvas(10) = hruyrN(10,j)
        pdvas(11) = hruyrN(11,j)
        pdvas(12) = hruyrN(12,j)
        pdvas(13) = hruyrN(13,j)
        pdvas(14) = hruyrN(14,j)
        pdvas(15) = hruyrN(15,j)
        pdvas(16) = hruyrN(16,j)
        pdvas(17) = hruyrN(17,j)
        pdvas(18) = hruyrN(18,j)
        pdvas(19) = hruyrN(19,j)
        pdvas(20) = hruyrN(20,j)
        pdvas(21) = hruyrN(21,j)
        pdvas(22) = hruyrN(22,j)
        pdvas(23) = hruyrN(23,j)
        pdvas(24) = hruyrN(24,j)
        pdvas(25) = hruyrN(25,j)
        pdvas(26) = hruyrN(26,j)
        pdvas(27) = hruyrN(27,j) 
        pdvas(28) = hruyrN(28,j)
        pdvas(29) = hruyrN(29,j)
        pdvas(30) = hruyrN(30,j)
        pdvas(31) = hruyrN(31,j)
        pdvas(32) = hruyrN(32,j)
        pdvas(33) = hruyrN(33,j)
        pdvas(34) = hruyrN(34,j)
        pdvas(35) = hruyrN(35,j)
        pdvas(36) = hruyrN(36,j)
        pdvas(37) = hruyrN(37,j)
        pdvas(38) = hruyrN(38,j)
        pdvas(39) = hruyrN(39,j)
        pdvas(40) = hruyrN(40,j)
        pdvas(41) = hruyrN(41,j)
        pdvas(42) = hruyrN(42,j)
        pdvas(43) = hruyrN(43,j)
        pdvas(44) = hruyrN(44,j)
        pdvas(45) = hruyrN(45,j)
        pdvas(46) = hruyrN(46,j)
        pdvas(47) = hruyrN(47,j)
        pdvas(48) = hruyrN(48,j)
        pdvas(49) = hruyrN(49,j)
        pdvas(50) = hruyrN(50,j)
        pdvas(51) = hruyrN(51,j)
        pdvas(52) = hruyrN(52,j)
        pdvas(53) = hruyrN(53,j)
       
       
   !!-------------------------------------------------------------------------------------------------------
        
        !if (ipdvas(1) > 0) then
       !   do ii = 1, itots
        !    pdvs(ii) = pdvas(ipdvas(ii))
       !   end do
 
        idplant = idplt(j)
        if (idplant > 0) then
          cropname = cpnm(idplant)
        else
          cropname = "NOCR"
        endif

      write (output_N_hru_num,1002) cropname, j, subnum(j),        &
          hruno(j), sb, nmgt(j), 0, 0, iyr, hru_km(j),     &
          (pdvas(ii), ii = 1, 53)
1002  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,       &
      53f10.3)   




       !   if (iscen == 1 .and. isproj == 0) then
       !  write (282,1000) cropname, j, subnum(j), hruno(j), sb,     &    
        !          nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, 53)
      !     else if (isproj == 1) then
    !      write (21,1000) cropname, j, subnum(j), hruno(j),             
    ! &            sb, nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots)
    !      else if (iscen == 1 .and. isproj == 2) then
   !       write (output_hru_num,2000) cropname, j, subnum(j), hruno(j), sb,         
   !  &    nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
         ! endif
       ! else
        ! if (iscen == 1 .and. isproj == 0) then
        ! write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,         
    ! &            nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo)
    !      else if (isproj == 1) then
      !     write (21,1001) cropname, j, subnum(j), hruno(j),             
    ! &           sb, nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo)
      !    else if (iscen == 1 .and. isproj == 2) then
      !    write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,         
      ! &    nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
        !  endif
        !end if
        end if
      end do

      return

 1000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,53f10.3)
    
! 2000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
!     *e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
! 1001 format (a4,i7,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
!     *e10.5,1x,e10.5,3e10.3,6f10.3,1x,i4,66f10.3)    
      end