      subroutine headout_R

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes the headings to the major output files

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    header

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
        
      integer :: j, ilen
      
      
      call header_R 


!!/// reach output files
!! write headings to reach output file (output2.rch)
    
      write (output2_rch_num,1000) prog, values(2), values(3), values(1), values(5),values(6), values(7)       
      write (output2_rch_num,1010) title
   !   if (ipdvar(1) > 0) then
      if(ievent_rch==1 .and. iprint==3) then
       write (output2_rch_num,1044) (hedr2(j), j = 1, mrcho2)  !! custom printout
      else 
       write (output2_rch_num,1043) (hedr2(j), j = 1, mrcho2)  !! custom printout
      end if 
    !  else
   !     write (output2_rch_num,1040) (hedr2(j), j = 1, mrcho2)          !! default printout
    ! endif 




      return
 1000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 1010 format (/(t5,20a4))

 1043 format (//7x,'RCH      GIS   MON     AREAkm2',96a12)      !!output2.rch------------------------------------------------------
 1044 format (//7x,'RCH      GIS   MON  Hour     AREAkm2',96a12)      !!output2.rch-----subdaily-------------------------------------------------
 1045 format (//7x,'RCH      GIS   MON     AREAkm2',64a12)      !!output3.rch------------------------------------------------------
 1046 format (//7x,'RCH      GIS   MON  Hour     AREAkm2',64a12)      !!output3.rch-----subdaily-------------------------------------------------
 1047 format (//7x,'RCH      GIS   MON     AREAkm2',96a12)      !!output4.rch------------------------------------------------------
 1048 format (//7x,'RCH      GIS   MON  Hour     AREAkm2',96a12)      !!output4.rch-----subdaily-------------------------------------------------
 1057 format (//7x,'RCH      GIS   MON     AREAkm2',96a12)      !!output5.rch------------------------------------------------------
 1058 format (//7x,'RCH      GIS   MON  Hour     AREAkm2',96a12)      !!output5.rch-----subdaily-------------------------------------------------
 1051 format (//7x,'RCH      GIS   MON     AREAkm2',96a12)      !!output6.rch------------------------------------------------------
 1052 format (//7x,'RCH      GIS   MON  Hour     AREAkm2',96a12)      !!output6.rch-----subdaily-------------------------------------------------
 1053 format (//7x,'RCH      GIS   MON     AREAkm2',96a12)      !!output7.rch------------------------------------------------------
 1054 format (//7x,'RCH      GIS   MON  Hour     AREAkm2',96a12)      !!output7.rch-----subdaily-------------------------------------------------
 
 1101 format (//7x,'RES      GIS   MON     AREAkm2',96a12)      !!output2.res------------------------------------------------------
 1102 format (//7x,'RES      GIS   MON  Hour     AREAkm2',96a12)      !!utput2.res-----subdaily-------------------------------------------------
 1103 format (//7x,'RES      GIS   MON     AREAkm2',64a12)      !!output3.res------------------------------------------------------
 1104 format (//7x,'RES      GIS   MON  Hour     AREAkm2',64a12)      !!output3.res-----subdaily-------------------------------------------------
 1105 format (//7x,'RES      GIS   MON     AREAkm2',96a12)      !!output4.res------------------------------------------------------
 1106 format (//7x,'RES      GIS   MON  Hour     AREAkm2',96a12)      !!output4.res-----subdaily-------------------------------------------------
 1107 format (//7x,'RES      GIS   MON     AREAkm2',96a12)      !!output5.res------------------------------------------------------
 1108 format (//7x,'RES      GIS   MON  Hour     AREAkm2',96a12)      !!output5.res-----subdaily-------------------------------------------------
 1109 format (//7x,'RES      GIS   MON     AREAkm2',96a12)      !!output6.res------------------------------------------------------
 1110 format (//7x,'RES      GIS   MON  Hour     AREAkm2',96a12)      !!output6.res-----subdaily-------------------------------------------------
 1111 format (//7x,'RES      GIS   MON     AREAkm2',96a12)      !!output7.res------------------------------------------------------
 1112 format (//7x,'RES      GIS   MON  Hour     AREAkm2',96a12)      !!output7.res-----subdaily-------------------------------------------------
 


      end