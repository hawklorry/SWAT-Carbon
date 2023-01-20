      subroutine hrumon_N

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly HRU output to the output.hru file
  
      use parm
      implicit none

      integer :: j, sb, ii, days, iflag, idplant
      real :: dmt, yldt
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      days = 0

      select case(mo_chk)
        case (9, 4, 6, 11)
          days = 30
        case (2)
          days = 29 - leapyr
        case default
          days = 31
      end select

      do j = 1, nhru
        sb = 0
        sb = hru_sub(j)

        iflag = 0
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do

        if (iflag == 1) then

        pdvas = 0.
        pdvs = 0.

        dmt = 0.
        yldt = 0.
        dmt = bio_ms(j) / 1000.
        yldt = (1. - rwt(j)) * dmt * hvstiadj(j)

        pdvas(1) = hrumonN(1,j)/ Real(days)
        pdvas(2) = hrumonN(2,j)/ Real(days)
        pdvas(3) = hrumonN(3,j)/ Real(days)
        pdvas(4) = hrumonN(4,j)/ Real(days)
        pdvas(5) = hrumonN(5,j)/ Real(days)
        pdvas(6) = hrumonN(6,j)/ Real(days)
        pdvas(7) = hrumonN(7,j)
        pdvas(8) = hrumonN(8,j)
        pdvas(9) = hrumonN(9,j)
        pdvas(10) = hrumonN(10,j)
        pdvas(11) = hrumonN(11,j)
        pdvas(12) = hrumonN(12,j)
        pdvas(13) = hrumonN(13,j)
        pdvas(14) = hrumonN(14,j)
        pdvas(15) = hrumonN(15,j)
        pdvas(16) = hrumonN(16,j)
        pdvas(17) = hrumonN(17,j)
        pdvas(18) = hrumonN(18,j)
        pdvas(19) = hrumonN(19,j)
        pdvas(20) = hrumonN(20,j)
        pdvas(21) = hrumonN(21,j)
        pdvas(22) = hrumonN(22,j)
        pdvas(23) = hrumonN(23,j)
        pdvas(24) = hrumonN(24,j)
        pdvas(25) = hrumonN(25,j)
        pdvas(26) = hrumonN(26,j)
        pdvas(27) = hrumonN(27,j) 
        pdvas(28) = hrumonN(28,j)
        pdvas(29) = hrumonN(29,j)
        pdvas(30) = hrumonN(30,j)
        pdvas(31) = hrumonN(31,j)
        pdvas(32) = hrumonN(32,j)
        pdvas(33) = hrumonN(33,j)
        pdvas(34) = hrumonN(34,j)
        pdvas(35) = hrumonN(35,j)
        pdvas(36) = hrumonN(36,j)
        pdvas(37) = hrumonN(37,j)
        pdvas(38) = hrumonN(38,j)
        pdvas(39) = hrumonN(39,j)
        pdvas(40) = hrumonN(40,j)
        pdvas(41) = hrumonN(41,j)
        pdvas(42) = hrumonN(42,j)
        pdvas(43) = hrumonN(43,j)
        pdvas(44) = hrumonN(44,j)
        pdvas(45) = hrumonN(45,j)
        pdvas(46) = hrumonN(46,j)
        pdvas(47) = hrumonN(47,j)
        pdvas(48) = hrumonN(48,j)
        pdvas(49) = hrumonN(49,j)
        pdvas(50) = hrumonN(50,j)
        pdvas(51) = hrumonN(51,j)
        pdvas(52) = hrumonN(52,j)
        pdvas(53) = hrumonN(53,j)

       ! if (itots > 0) then 
	    ! ix = itots
	   ! else
        ! ix = mhruo
	   ! endif


       ! if (ipdvas(1) > 0) then
        !  do ii = 1, ix
         !   pdvs(ii) = pdvas(ipdvas(ii))
        !  end do
 
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

    
         ! if (iscen == 1) then                                          
          !  select case (isproj)
          !  case (0)
         !   write (282,1000) cropname, j, subnum(j), hruno(j), sb,   &     
           !  nmgt(j), mo_chk, hru_km(j), (pdvas(ii), ii = 1, 53)
  
      !      case (1)
        !    write (21,1000) cropname, j, subnum(j), hruno(j),           
    ! &         sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
    !        case (2)
      !       write (output_hru_num,2000) cropname, j, subnum(j), hruno(j), sb,       
    ! &         nmgt(j), mo_chk, hru_km(j),(pdvs(ii), ii = 1, ix), iyr
           !end select
         ! end if
     ! else
!! write with different format for hrus greater than 9999
      !select case (isproj)
         !  case (0)
         !   write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,       &
     !&         nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
    !        case (1) 
!            write (21,1001) cropname, j, subnum(j), hruno(j),           &
!     &         sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
!            case(2) 
!            write (output_hru_num,1001) cropname, j, subnum(j), hruno(j), sb,       &
!     &         nmgt(j), mo_chk, hru_km(j),(pdvs(ii), ii = 1, ix), iyr
       ! end select
         ! end if

        end if
      end do

      return
 1000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,1x,e10.4,53f10.3)   
     
 !2000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
 !    *e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
 !1001 format (a4,i7,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
 !    *e10.5,1x,e10.5,3e10.3,3f10.3,1x,i4,66f10.3)    
      end