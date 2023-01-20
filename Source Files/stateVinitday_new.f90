      subroutine stateVinitday
      !! state variable at the beginning of the day

      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: j,k
   
      
      j = 0
      j = ihru
      
      
      
       bio_ms_ini(j) = bio_ms(j)
       shallst_doc_0(j) = shallst_doc(j)
       shallst_dic_0(j) = shallst_dic(j)
      
        k = 0
       do k = 1, sol_nly(j)
         solc_no3_ini(j) = solc_no3_ini(j) + sol_no3(k,j)
         solc_nh4_ini(j) = solc_nh4_ini(j) + sol_nh4(k,j)
         solc_urea_ini(j) = solc_urea_ini(j) + sol_urea(k,j)      
         solc_solp_ini(j) = solc_solp_ini(j) + sol_solp(k,j)  
         solc_minp_ini(j) = solc_minp_ini(j) + sol_actp(k,j)+ sol_stap(k,j)     
       
	    if (cswat == 0) then
         solc_orgn_ini(j) = solc_orgn_ini(j) + sol_aorgn(k,j) + sol_orgn(k,j) +  sol_fon(k,j)                          
	     solc_orgp_ini(j) = solc_orgp_ini(j)  + sol_fop(k,j) + sol_orgp(k,j)
	    end if
	    
	    if (cswat == 1) then
      	 solc_orgn_ini(j)  = solc_orgn_ini(j)  + sol_orgn(k,j)  + sol_fon(k,j) +  sol_mn(k,j)                           
		 solc_orgp_ini(j)  = solc_orgp_ini(j)  + sol_fop(k,j)+ sol_orgp(k,j) +  sol_mp(k,j)                                
	    end if
	    
	    if (cswat == 2) then
        solc_orgn_ini(j) = solc_orgn_ini(j) + sol_LMN(k,j) +sol_LSN(k,j)  + sol_HPN(k,j) + sol_BMN(k,j) + sol_HSN(k,j)                 
	    solc_orgp_ini(j) = solc_orgp_ini(j)  + sol_fop(k,j) + sol_orgp(k,j)	   
	    solc_orgc_ini(j) = solc_orgc_ini(j)+ sol_LSC(k,j)+sol_LMC(k,j)+sol_HPC(k,j)  +sol_HSC(k,j)  +sol_BMC(k,j)                         
        solc_orgcs_ini(j)= solc_orgcs_ini(j)+ sol_HPC(k,j)+sol_HSC(k,j)+sol_BMC(k,j)                                           
	    solc_doc_ini(j)= solc_doc_ini(j) + Sol_DOC(k,j)  ! sol_BMC(k,j) !                  !  Total DOC in soil profile ;  kg C/ha                                         
        solc_dic_ini(j)= solc_dic_ini(j) + Sol_DIC(k,j)                          !  Total DIC in soil profile ;  kg C/ha
	    
	    iniorgn(j) = iniorgn(j)+ sol_aorgn(k,j) + sol_orgn(k,j)+sol_fon(k,j) + sol_BMN(k,j)     ! is the varible being used? 
	    inino3(j) =  inino3(j) + sol_no3(k,j)
        ininh3(j) =  ininh3(j) + sol_nh4(k,j)  
	     
	    
	    end if

	   

       end do
     

     
       return
       end