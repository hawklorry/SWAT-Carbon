      subroutine headout_S

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes the headings to the major output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hedb(:)     |NA            |column titles in subbasin output files
!!    hedr(:)     |NA            |column titles in reach output files
!!    hedrsv(:)   |NA            |column titles in reservoir output files
!!    heds(:)     |NA            |column titles in HRU output files
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in 
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ipdvab(:)   |none          |output variable codes for output.sub file
!!    ipdvar(:)   |none          |output variable codes for .rch file
!!    ipdvas(:)   |none          |output variable codes for output.hru file
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotb       |none          |number of output variables printed (output.sub)
!!    itotr       |none          |number of output variables printed (.rch)
!!    itots       |none          |number of output variables printed (output.hru)
!!    msubo       |none          |maximum number of variables written to
!!                               |subbasin output file (output.sub)
!!    mhruo       |none          |maximum number of variables written to 
!!                               |HRU output file (output.hru)
!!    mrcho       |none          |maximum number of variables written to
!!                               |reach output file (.rch)
!!    prog        |NA            |program name and version
!!    title       |NA            |title lines from file.cio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilen        |none          |width of data columns in output file
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    header

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      integer :: j, ilen
      
      call header_S   
      


!! write headings to HRU output file (output.hru)
      ! write (281,1000) prog, values(2), values(3), values(1), values(5), 
      !&               values(6), values(7)
      ! write (281,1010) title
	
      write (output_C_hru_num,1281) (hedsc(j), j = 1, 47)            
      write (output_N_hru_num,1282) (hedsn(j), j = 1, 53)         
      write (output_E_hru_num,1283) (hedse(j), j = 1, 9)    
      write (output_P_hru_num,1284) (hedsp(j), j = 1, 29)           
 
1281  format ('LULC  HRU       GIS  SUB  MGT MO DA   YR','    AREAkm2', 47(a10))	    
1282  format ('LULC  HRU       GIS  SUB  MGT MO DA   YR','    AREAkm2', 53(a10))            
1283  format ('LULC  HRU       GIS  SUB  MGT MO DA   YR','    AREAkm2', 9(a10))           
1284  format ('LULC  HRU       GIS  SUB  MGT MO DA   YR','    AREAkm2', 29(a10))            
    

      
 
!!    standard output_C.bsn file

      write (output_C_lnd_num,1300)(hedbc(j),j =1, 56)
      write (output_N_lnd_num,1400)(hedbn(j),j =1, 44) 
      write (output_Nagr_lnd_num,1400)(hedbn(j),j =1, 44)
      write (output_Nfor_lnd_num,1400)(hedbn(j),j =1, 44)
      write (output_Ngra_lnd_num,1400)(hedbn(j),j =1, 44)
      write (output_Nwet_lnd_num,1400)(hedbn(j),j =1, 44)
      
      
      
1300  format ('Time ',56(a10))                             
1400  format ('year  mon  day ',44(a10))    
    
    
           


      return
 
      end