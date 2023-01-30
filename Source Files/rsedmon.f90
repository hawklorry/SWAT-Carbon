      subroutine rsedmon(mdays)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the monthly reach output to the .sed file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      
      implicit none
      
      integer, intent(in) :: mdays
      integer :: j, ii
       
      do j = 1, subtot
          !!~ ~ ~ SQLITE ~ ~ ~
          if(ioutput == 1) then
            call sqlite3_set_column( colsed(1), j )
            call sqlite3_set_column( colsed(2), iyr )
            call sqlite3_set_column( colsed(3), mo_chk )
            call sqlite3_set_column( colsed(4), rchmono(3,j) )
            call sqlite3_set_column( colsed(5), rchmono(4,j) )
            do ii = 42,58
                call sqlite3_set_column( colsed(ii-36), rchmono(ii,j) )
            end do
            call sqlite3_insert_stmt( db, stmtsed, colsed )
          else
          rchmono(58,j) = rchmono(58,j)/Real(mdays)                     
          write (output_sed_num,5000) j, subgis(j), mo_chk, rch_dakm(j), rchmono(3,j), rchmono(4,j),(rchmono(ii,j),ii=42,58)
          end if
          !!~ ~ ~ SQLITE ~ ~ ~
      end do

      return
 5000 format ('REACH ',i6,1x,i8,1x,i5,20e12.4)         !!R682 10/20/21 nbs 
      end