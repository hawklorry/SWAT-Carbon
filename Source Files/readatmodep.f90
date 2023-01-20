      subroutine readatmodep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the atmospheric deposition values
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drydep_no3  |kg/ha/yr      |atmospheric dry deposition of nitrates
!!    drydep_nh4  |kg/ha/yr      |atmospheric dry deposition of ammonia
!!    rammo_sub   |mg/l          |atmospheric deposition of ammonium values for
!!                                 entire watershed
!!    rcn_sub     |mg/l          |atmospheric deposition of nitrate for
!!                                 entire watershed
!!    IATMODEP: 0 = average annual inputs 1 = monthly inputs  2 = daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      
      character (len=80) :: titldum
      integer :: eof, idap, iyp, imo, iii, mo_max, momax        !!R670  6/26/18  nbs
  
      eof = 0
  
      if (idaf > 0) then
          id1 = idaf
      else
          id1 = 1
      end if
      
      rcn_sub = rcn_sub_bsn

!!    Atmosperic deposition filename present in file.cio
      if (atmofile /= '             ') then
        open (atmofile_num,file=atmofile)
          do iii = 1, 5
            read (atmofile_num,5101) titldum           !!R670  6/26/18 and R671 4/5/19  nbs
          end do
      !else                                             !!R682 10/20/21 nbs
      !!    no filename present in file.cio - set defaults
      !  rammo_sub = 0.                                 !!R682 10/20/21 nbs
	  !rcn_sub = rcn_sub_bsn                            !!R682 10/20/21 nbs
          !end if                                       !!R682 10/20/21 nbs
        
      select case (iatmodep)
      case (0)
        do isub = 1, subtot
          read (atmofile_num,*,iostat=eof) rammo_sub(isub), rcn_sub(isub),  &
	  		drydep_nh4(isub), drydep_no3(isub)
          if (eof < 0) exit
        end do 
        close (atmofile_num)
      case (1)
        read (atmofile_num,*,iostat=eof) mo_atmo1, iyr_atmo1, mo_max   !!R670  6/26/18  nbs
          iii = 0
          momax = 12 * nbyr
          do iii = 1, msub
            read (atmofile_num,*) (rammo_mo(imo,iii),imo = 1,mo_max)   !!R670  6/26/18  nbs 
            read (atmofile_num,*) (rcn_mo(imo,iii), imo = 1,mo_max)    !!R670  6/26/18  nbs
            read (atmofile_num,*) (drydep_nh4_mo(imo,iii), imo = 1, mo_max)  !!R670  6/26/18  nbs
            read (atmofile_num,*) (drydep_no3_mo(imo,iii), imo = 1,mo_max)   !!R670  6/26/18  nbs
          end do
          close (atmofile_num)
      case (2)
        read (atmofile_num,*) matmo  !!maximum number of subbasins
        do
          iyp = 0
          idap = 0
          read (atmofile_num,*,iostat=eof) iyp, idap
          if (eof < 0) exit
          if (iyp + idap <= 0) exit
          if (iyp == iyr .and. idap == id1) then
            backspace (atmofile_num)
            exit
          end if
        end do
        close (atmofile_num)
      end select
            
            else
      !!    no filename present in file.cio - set defaults
        rammo_sub = 0.
      rcn_sub = rcn_sub_bsn
      end if

!1001  format (2i6)                         !!R670  6/26/18  nbs
!1002  format (1200f10.3)                   !!R670  6/26/18  nbs
!1000  format (8x,4f10.3)                   !!R670  6/26/18  nbs
5101  format (a80)                          !!R670  6/26/18 and R671 4/5/19  nbs
      return
      end