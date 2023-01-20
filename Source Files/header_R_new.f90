         subroutine header_R
            !!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine opens output files and writes headers 

         use parm
         use parm_output 
         implicit none
           
           
!!    column headers for reach output2 file
      hedr2 = (/"  Sub_RPOC  ","  Sub_LDOC  ", "  Sub_RDOC  ",   &
               "  Sub_LDOC  ", "   Sub_DIC  ","    Sub_AlgC",   &
          " Inflow_RPOC"," Inflow_LPOC"," Inflow_RDOC",          &  
              " Inflow_LDOC"," Inflow_DIC","Inflow_FAlgC",    &
              "  Out_RPOC  ","  Out_LPOC  ","  Out_RDOC  ",    &        
              "  Out_LDOC  ","  Out_DIC  ", " Out_FAlgC  ",     &
              "   rch0_RPOC","   rch0_LPOC","   rch0_RDOC",      &      
              "   rch0_LDOC","   rch0_DIC","  rch0_FAlgC",        &
              "  rch0_AbC  ",                                      &                           !!25     
              "   rch1_RPOC","   rch1_LPOC","   rch1_RDOC",         &   
              "   rch1_LDOC","   rch1_DIC","  rch1_FAlgC",    &
              "  rch1_AbC  ",                               &
              " Outflo_RPOC"," Outflo_LPOC"," Outflo_RDOC",  &          
              " Outflo_LDOC","  Outflo_DIC"," OutfloAlgC ",   & 
              " Alg_GrowC  "," Alg_DeathC ","  Alg_RespC ",    &        
              "  Alg_SetC  ","    Ab_C    ","  Ab_PhoC   ",&
              " Ab_DeathC  ", "  Ab_RespC  ",               &                    !!46
              "    Alg-LP  ","    Ab-LP   ","    LP-Set  ",  &          
              "    LP-LD   ","   LP-DIC   ","   LP-RP    ",   &         
              "    Alg-RP  ","   Ab-RP    ","    RP-LD   ",    &        
              "    RP-DIC  ","     RP-Set ","    FlAlg-LD",     &       
              "     Ab-LD  ","    LD-DIC  ","    LD-RD   ",      &      
              "   LD-Dentri","    Alg-RD  ","   Ab-RD    ",       &   
              "    RD-DIC   ","   Atm-DIC  ","   Alg-DIC  ",       &     
              "   DIC-Alg  ","   Ab-DIC   ","   DIC-Ab   ",         &   
              "   Sed-DIC  ", " rch0_SedC  ", " rch1_SedC  ",&
              "   Sed-CH4  ",&
             "   Sed-Bury " ," Sediment_C",  "      Bury_C", &
             "  Bla_RPOC  ", "  Bla_LPOC  ", "  Bla_RDOC  ",&
             "  Bla_LDOC  ", "   Bla_DIC  ","   Bla_Alg  ",&
             "   Bla_Ab   ", "  Bla_SedC  ", "  LPOC_R    " , &
            "  RPOC_R    " ,"   CH4_R    " ,"   SED_R    " ,&
            "   Sub_SED  " ,   &
             "  CH4s_Atm  " ,"  CH4g_Atm  " ,"   N2g_Atm  " ,  &
             "   CH4_ox   " ,"   CH4s-g   " ,"   CH4_con  " &
                        /)    
            
         
    
    
      return


      end