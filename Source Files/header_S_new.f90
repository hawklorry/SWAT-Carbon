      subroutine header_S

!!    ~ ~ ~ PURPOSE ~ ~ ~                                               
!!    This subroutine defines header titles for the different output files
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~                                    
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hedb(:)     |NA            |column titles in subbasin output file
!!    hedr(:)     |NA            |column titles in reach output file
!!    hedrsv(:)   |NA            |column titles in reservoir output file
!!    heds(:)     |NA            |column titles in HRU output file
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~                        

      use parm
      implicit none


 !!---------------------------------------------output_C.hru----------------------------------------
      hedsc = (/" iniOrgCt "," fnlOrgCt "," iniOrCst "," fnlOrCst ", & !4
          " iniDOC   "," fnlDOC   "," iniDIC   "," fnlDIC   ",       & !8
          " OrgC_Rsd "," OrgC_Fer "," CO2_out  "," CO2_gen  ",    &   !12
          " OrgC_Sed "," OrgC_SF  "," OrgC_LF  "," OrgC_Per ",    &   !16
          " RPOC_Sed "," LPOC_Sed "," DOC_In   ",    &
          " DOC_SF   "," DOC_LF   ",          &                       !21
          " DOC_Per  "," DOC_GW   "," rchg_doc "," doc_decy ", &      !25
          " rep_doc  "," seep_doc "," inigwDOC "," fnlgwDOC ", &      !29
          " DIC_SF   "," DIC_LF   "," DIC_Per  "," DIC_GW   ", &      !33
          " rchg_dic "," rep_dic  "," seep_dic "," inigwDIC ",  &     !37
          " fnlgwDIC "," iniBioms "," fnlBioms "," NPPC     ", &      !41
          " grainc   "," BalSOC   "," BalPltC  "," BalgwDOC ", &      !45
          " BalgwDIC "," Sed_BMC  "/)                                                           
     
!!---------------------------------------------output_N.hru----------------------------------------

       hedsn = (/" iniNO3   ","  fnlNO3  "," iniNH3   ","  fnlNH3  ", &  !4
          " iniOrgNt "," fnlOrgNt "," FerNO3   "," auFerNO3 ",  &       !8
          "  FerNH4  "," auFerNH4 "," RainNO3  "," RainNH3  ",  &      !12
          "  FerOrgN ","  ResdN   "," immoNO3  "," upNO3    ",  &      !16
          " nitrNO3  "," denitNO3 "," absNO3   "," minNH4   ",  &      !20
          " volNH4   ","  absNH3"  ," N2O_nit  "," NO_nit   ",  &      !24
          " N2O_den  "," NO_den   ","  N2_den  "," N2O      ",  &      !28
          "   NO     "," surqNO30 "," tileNO3  ","  latNO30 ",  &      !32
          "   percn  "," sedOrgN0 "," Sed_RPON "," Sed_LPON ",  &      !36
          " SurQ_DON "," Lat_DON  "," PerQ_DON "," rchrgDON ",  &      !40
          " revapDON ","  Gwq_DON "," decyDON  "," seep_DON ",  &      !44 
          " shal_DON ","  BalNH4  "," BalNO3   "," BalOrgN  ",  &      !48
          "  Bal_N   ","  plantN  "," grainN   "," Min_Imm  ",  &      !52
          " IMMO_ERR "   /) 
                                                           


!!---------------------------------------------output_P.hru----------------------------------------

       hedsp = (/    &
          " iniSolP  ","  fnlSolP "," iniOrgP  ","  fnlOrgP ",    &      !4
          " iniMinP  "," fnlMinP  "," ferMinP  "," atfMinP  ",    &      !8
          " confMinP "," grzfMinP "," ferOrgP  "," atfOrgP  ",    &      !12
          " confOrgP "," grzfOrgP ","  ResdP   "," SolP_min ",    &      !16
          " SolP_Up  "," MinP_sol "," surqSolP "," sedminPa ",    &       !20
          " sedminPs "," sedorgP  "," percP    ","   tileP  ",    &      !24
          " minPgw   "," gwminP   "," BalSolP  "," BalMinP  ",    &      !28 
          " BalOrgP  "     /)                    
                             


 !!---------------------------------------------output_E.hru----------------------------------------
       hedse = (/" SurTempC" ,  &
         " SolT50cmC",  "  ST100cmC", "  ST150cmC", "  ST200cmC",  &
         "  ST300cmC","  ST350cmC", " ST1000cmC", "  FrozeDay" /)            
       
    
    
    !!-----output_C.bsn--------
       hedbc = (/                 &     
          "  AirT    "," N2Okg/ha "," NOkg/ha  "," NH4kg/ha ",  &       !4
          " CH4kg/h  "," CO2kg/h  "," SdCkg/h  "," SfCkg/h  ",  &       !8
          "  LtCkg/h "," PcCkg/h  "," NPPkg/h  "," SnFmm    ",  &      !12
          " OgPt/ha  "," FrDay    "," RAMj/m2  ","  GwCkg/h ",  &      !16
          " OgCt/ha  "," OgsCt/h  ","RPCkg/h   "," LPCkg/h  ",  &      !20
          "  RDCkg/h "," LDCkg/h  ","  DICkg/h ","  AgCkg/h ",  &      !24
          " BrCkg/h  ","  ArCkg/h "," DnCkg/h  "," BedCkgh  ",  &      !28
          "  BAgCkgh "," RchAha   "," RchWAha  "," oRPCkgh  ",  &      !32
          " oLPCkgh  "," oRDCkgh  ","  oLDCkgh "," oDICkgh  ",  &      !36
          " oAgCkgh  ","  RPDkgh  "," LPDkgh   ","  AgDkgh  ",  &      !40
          " LPRkgh   ","  RPRkgh  "," CH4Rkgh  "," RcLPkgh  ",  &      !44 
          " RcRPkgh  ","  RcLDkgh ","  RcRDkgh "," RcDIkgh  ",  &      !48
          " sCO2kgh  "," CH4skgh  "," CH4gkgh  ","  iSedt/h ",  &      !52
          " oSedt/h  "," sSedt/h  "," Sdept/h  "," Sdegt/h  "   &      !56
          /) 
 
     !!-----output_N.lnd--------
       hedbn = (/                 &     
          "  iniOrgN ","  finOrgN "," iniNo3   ","  finNo3  ",  &       !4
          " iniNh4   "," finNh4   "," No3_fert "," Nh4_fert ",  &       !8
          " OrgNfert ","   fixN   "," minNrain "," sedOrgN  ",  &      !12
          " surqNo3  "," latNo3   "," percNo3  ","  gwNo3   ",  &      !16
          " tileNo3  ","  grainN  "," No3_up   "," No3_den  ",  &      !20
          "  Nh4_vol "," No3_nit  ","  surqDON "," latqDON  ",  &      !24
          " rchrgDON ","  gwqDON  "," totalN2O ","  totalNO ",  &      !28
          "  N2_den  "," N2O_nit  ","  NO_nit  "," gwNo3los ",  &      !32
          " repNo3   "," gwsepNo3 ","  repDON  "," gwDONdec ",  &      !36
          " gwsepDON ","  rsdOrgN "," Nh4_min  "," No3_immo ",  &     !40
          " WFPS_ave ","  st_ave  ","  ph_ave  ","  rspC_d  "   &     !44
          /) 

     
      return
      end                                                            