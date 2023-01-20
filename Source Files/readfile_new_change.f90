      subroutine readfile_new

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   this subroutine opens the main input and output files

      use parm
      use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
       implicit none
       
      character (len=80) :: titldum
      real :: sumv, xx
      integer :: rn, j, ii, eof
      eof = 0

      
     
      !open (basins_rwq_num,file="basins.rwq")
      open (output_C_hru_num,file="output_C.hru",recl=2000)   
      open (output_N_hru_num,file="output_N.hru",recl=2000)  
      open (output_E_hru_num,file="output_E.hru",recl=2000)   
      open (output_P_hru_num,file="output_P.hru",recl=2000)  
      open (output_C_std_num,file="output_C.std")
      open (output_N_std_num,file="output_N.std")
      open (output_C_lnd_num,file="output_C.lnd")
      open (output_N_lnd_num,file="output_N.lnd")
      open (output_Nagr_lnd_num,file="output_Nagr.lnd")
      open (output_Nfor_lnd_num,file="output_Nfor.lnd")
      open (output_Ngra_lnd_num,file="output_Ngra.lnd")
      open (output_Nwet_lnd_num,file="output_Nwet.lnd") 
      open (co2_dat_num,file="co2.dat")      
      open (output2_rch_num,file="output2.rch")   
    

      return 
 5000 format (6a)  
      end