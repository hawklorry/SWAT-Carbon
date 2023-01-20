    subroutine carbon_cal

    use carbon_para
    use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
   
    implicit none
            !! DIC calibration 
              CFB = CFB_para
              peroc_DIC = peroc_DIC_para

            !!DOC calibration
!            if (wdpq <=0.)  wdpq = 0.7
!            if (wdlpq <=0.) wdlpq = 4000.0     ! new
!            if (wgpq <=0.) wgpq  = 50.0
            !if (wglpq <=0.) wglpq = 0.25
            !if (wdps <=0.) wdps  = 0.006
            !if  (wgps<=0.) wgps = 0.08

           peroc_DOC = peroc_DOC_para     ! WDPQ              ! 0-1  DOC percolation coefficient
           part_DOC = part_DOC_para    ! WDLPQ            ! 500-2000 !organic carbon partition coefficient,basin sacle parameter,1000 to 1200
           hlife_doc = hlife_doc_para      ! WGPQ                              ! 0-100     !DOC half life (days) in groudwater,calculating DOC decay in groundwater 
           !if( wdpq > 0.000001) cbn_sp%peroc_DOC = wdpq    !WDPQ              ! 0-1  DOC percolation coefficient
           !if( wdlpq > 0.000001) cbn_sp%kd_OC = wdlpq    ! WDLPQ            ! 500-2000 !organic carbon partition coefficient,basin sacle parameter,1000 to 1200
           !if( wgpq > 0.000001) cbn_sp%hlife_docgw = wgpq    !WGPQ      
           !if( wglpq > 0.000001) cbn_rchpara%kld = wglpq   ! WGLPQ         0.05-0.3   !LDOC mineralization rate (/day)    LDOC->DIC !!
           !if( wdps > 0.000001) cbn_rchpara%krd = wdps    !WDPS           0.001-0.01  ! RDOC mineralization rate (/day) RDOC->DIC  !!
           !if( wgps > 0.000001) cbn_rchpara%klrd = wgps     !WGPS          0.004-0.03 decay rate of LDOC to RDOC (/day) LDOC->RDOC

   
   !! POC calibration
!            if (wdlps <=0.) wdlps = 1.5
!            if (wglps <=0.) wglps = 2.5
!            if (wof_p <=0.) wof_p = 2.5 
          !  if (wof_lp <=0.) wof_lp = 0.001 
           ! if (wdpf <=0.) wdpf   = 0.08 
           ! if  (wgpf <=0.) wgpf  = 0.01
          !   if (wdlpf <=0.) wdlpf  = 0.0025
           ! if  (wglpf <=0.) wglpf  = 0.075 
     


            er_POC = er_POC_para !wdlps   ! 0.0-5.0          !POC enrichment ratio      ! 0-10       MOST SENSITIVE            
             !if( wdlps > 0.000001) cbn_sp%enr_POC = wdlps   ! 0.0-5.0          !POC enrichment ratio      ! 0-10       MOST SENSITIVE
            if(ievent_rch ==0)then
             !if( wglps > 0.000001) cbn_rchpara%sv_rp = wglps         !0.0-10         !RPOC settling velocity (m/day)            MOST SENSITIVE
             !if( wof_p > 0.000001) cbn_rchpara%sv_lp = wof_p     ! 0.0-10         !LPOC settling velocity (m/day)              MOST SENSITIVE 
            end if
           !if( wof_lp > 0.000001) cbn_rchpara%kd_rp = wof_lp    ! 0.001-0.1   !RPOC decay rate to DIC (/day) RPOC->DIC 
          ! if( wdpf > 0.000001) cbn_rchpara%kd_lp = wdpf        !0.001-0.1        !LPOC decay rate to DIC (/day) LPOC->DIC
          ! if( wgpf > 0.000001) cbn_rchpara%klrp = wgpf         !0.005-0.015       !decay rate of LPOC to RPOC (/day) LPOC->RPOC 
          ! if( wdlpf > 0.000001) cbn_rchpara%krp = wdlpf         !0.0025-0.0075    !RPOC dissolution rate (/day) RPOC->LDOC
          ! if( wglpf > 0.000001) cbn_rchpara%klp = wglpf         !0.0375-0.15      !LPOC dissolution rate (/day) LPOC->LDOC


     !if(ievent_rch==1)then
      ! type carbon_water_para
           !LPOC parameters
         !cbn_rchpara%sv_lp = cbn_rchpara%sv_lp /real(nstep_rch)        !LPOC settling velocity (m/day)
         !cbn_rchpara%klp = cbn_rchpara%klp /real(nstep_rch)                !LPOC dissolution rate (/day) LPOC->LDOC
         !cbn_rchpara%kd_lp = cbn_rchpara%kd_lp /real(nstep_rch)     !LPOC decay rate to DIC (/day) LPOC->DIC
         !cbn_rchpara%klrp = cbn_rchpara%klrp /real(nstep_rch)            !decay rate of LPOC to RPOC (/day) LPOC->RPOC
           !RPOC parameters
         !cbn_rchpara%sv_rp = cbn_rchpara%sv_rp/real(nstep_rch)        !RPOC settling velocity (m/day)
         !cbn_rchpara%krp = cbn_rchpara%krp /real(nstep_rch)              !RPOC dissolution rate (/day) RPOC->LDOC
         !cbn_rchpara%kd_rp = cbn_rchpara%kd_rp/real(nstep_rch)       !RPOC decay rate to DIC (/day) RPOC->DIC
           !LDOC parameters
         !cbn_rchpara%kld = cbn_rchpara%kld /real(nstep_rch)          !LDOC mineralization rate (/day)    LDOC->DIC !!
         !cbn_rchpara%klrd = cbn_rchpara%klrd  /real(nstep_rch)       !decay rate of LDOC to RDOC (/day) LDOC->RDOC
         !cbn_rchpara%kdnit = cbn_rchpara%kdnit /real(nstep_rch)      !NO3 denitrification rate (/day) LDOC consumned  
           !RDOC parameters
         !cbn_rchpara%krd=cbn_rchpara%krd /real(nstep_rch)       ! RDOC mineralization rate (/day) RDOC->DIC  !!

      !end type carbon_water_para
      
         !scbn_rch%kbur = scbn_rch%kbur /real(nstep_rch) 
         !scbn_rch%ksed = scbn_rch%ksed/real(nstep_rch) 
      
       ! bottom algea rates---------------
         !Ab_rchpara%kexb = Ab_rchpara%kexb / real(nstep_rch)             ! bottom algae excretion rate (/day)
         !Ab_rchpara%pmN = Ab_rchpara%pmN / real(nstep_rch)            !maximum uptake rate for N (mgN/gD/day)        
         !Ab_rchpara%pmP = Ab_rchpara%pmP / real(nstep_rch)              !maximum uptake rate for P (mgP/gD/day)          
         !Ab_rchpara%kph = Ab_rchpara%kph / real(nstep_rch)                ! maximum photosynthesis rate [d-1 or gD/m2/d] 
         !Ab_rchpara%Abmax = Ab_rchpara%Abmax / real(nstep_rch)     !First-order carrying capacity of bottom algae [gD/m2/d] 
         !Ab_rchpara%klb = Ab_rchpara%klb / real(nstep_rch)                  !bottom algae light parameter (MJ/m2/day) 
         !Ab_rchpara%kdb = Ab_rchpara%kdb / real(nstep_rch)               !bottom algae death rate (/day)
         !Ab_rchpara%krb1 = Ab_rchpara%krb1 / real(nstep_rch)             !bottom algae basal respiration
         !Ab_rchpara%krb2 = Ab_rchpara%krb2 / real(nstep_rch)             !photo-respiration rate parameter
      ! bottom algea rates---------------
      
      
       !end if
       
       
      ! sediment calibration 
       
       !if(bactminlp <= 0.) bactminlp = 0.56 
       !if(bactminlp > 0.)  musle_pow = bactminlp
       
       
       
return
end subroutine