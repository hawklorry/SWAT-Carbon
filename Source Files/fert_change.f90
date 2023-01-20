      subroutine fert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)   |none          |fraction of bacteria in solution (the
!!                                 |remaining fraction is sorbed to soil
!!                                 |particles)
!!    bactlp_plt(:) |# cfu/m^2     |less persistent bacteria on foliage
!!    bactlpdb(:)   |# cfu/g   frt |concentration of less persistent bacteria
!!                                 |in fertilizer
!!    bactpdb(:)    |# cfu/g   frt |concentration of persistent bacteria in
!!                                 |fertilizer
!!    bactlpq(:)    |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)    |# cfu/m^2     |less persistent bacteria attached to soil
!!                                 |particles
!!    bactp_plt(:)  |# cfu/m^2     |persistent bacteria on foliage
!!    bactpq(:)     |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)     |# cfu/m^2     |persistent bacteria attached to soil 
!!                                 |particles
!!    curyr         |none          |current year of simulation
!!    fertn         |kg N/ha       |total amount of nitrogen applied to soil
!!                                 |in HRU on day
!!    fertp         |kg P/ha       |total amount of phosphorus applied to soil
!!                                 |in HRU on day
!!    fminn(:)      |kg minN/kg frt|fraction of fertilizer that is mineral N
!!                                 |(NO3 + NH4)
!!    fminp(:)      |kg minP/kg frt|fraction of fertilizer that is mineral P
!!    fnh3n(:)      |kgNH3-N/kgminN|fraction of mineral N in fertilizer that
!!                                 |is NH3-N
!!    forgn(:)      |kg orgN/kg frt|fraction of fertilizer that is organic N
!!    forgp(:)      |kg orgP/kg frt|fraction of fertilizer that is organic P
!!    frt_kg        |kg/ha         |amount of fertilizer applied to HRU
!!    frt_surface   |none          |fraction of fertilizer which is applied to
!!                                 |the top 10 mm of soil (the remaining
!!                                 |fraction is applied to first soil layer)
!!    hru_dafr(:)   |km2/km2       |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    laiday(:)     |m**2/m**2     |leaf area index
!!    nfert(:)      |none          |sequence number of fertilizer application
!!                                 |within the year
!!    nro(:)        |none          |sequence number of year in rotation
!!    nyskip        |none          |number of years to not print/summarize output
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_bd(1,:)   |Mg/m^3        |bulk density of top soil layer in HRU
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh4(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                                 |in soil layer
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_solp(:,:) |kg P/ha       |amount of inorganic phosohorus stored in
!!                                 |solution
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    wshd_fminp    |kg P/ha       |average annual amount of mineral P applied
!!                                 |in watershed
!!    wshd_fnh3     |kg N/ha       |average annual amount of NH3-N applied in
!!                                 |watershed
!!    wshd_fno3     |kg N/ha       |average annual amount of NO3-N applied in
!!                                 |watershed
!!    wshd_orgn     |kg N/ha       |average annual amount of organic N applied
!!                                 |in watershed
!!    wshd_orgp     |kg P/ha       |average annual amount of organic P applied
!!                                 |in watershed
!!    wshd_ftotn    |kg N/ha       |average annual amount of N (mineral & 
!!                                 |organic) applied in watershed
!!    wshd_ftotp    |kg P/ha       |average annual amount of P (mineral &
!!                                 |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlp_plt(:) |# cfu/m^2    |less persistent bacteria on foliage
!!    bactlpq(:)    |# cfu/m^2    |less persistent bacteria in soil solution
!!    bactlps(:)    |# cfu/m^2    |less persistent bacteria attached to soil
!!                                |particles
!!    bactp_plt(:)  |# cfu/m^2    |persistent bacteria on foliage
!!    bactpq(:)     |# cfu/m^2    |persistent bacteria in soil solution
!!    bactps(:)     |# cfu/m^2    |persistent bacteria attached to soil 
!!                                |particles
!!    fertn         |kg N/ha      |total amount of nitrogen applied to soil
!!                                |in HRU on day
!!    fertp         |kg P/ha      |total amount of phosphorus applied to soil
!!                                |in HRU on day
!!    nfert(:)      |none         |sequence number of fertilizer application
!!                                |within the year
!!    sol_aorgn(:,:)|kg N/ha      |amount of nitrogen stored in the active
!!                                |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha      |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha      |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_nh4(:,:)  |kg N/ha      |amount of nitrogen stored in the ammonium
!!                                |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha      |amount of nitrogen stored in the nitrate pool
!!                                |in soil layer
!!    sol_orgp(:,:) |kg P/ha      |amount of phosphorus stored in the organic
!!                                |P pool
!!    sol_solp(:,:) |kg P/ha      |amount of inorganic phosohorus stored in
!!                                |solution
!!    wshd_fminp    |kg P/ha      |average annual amount of mineral P applied
!!                                |in watershed
!!    wshd_fnh3     |kg N/ha      |average annual amount of NH3-N applied in
!!                                |watershed
!!    wshd_fno3     |kg N/ha      |average annual amount of NO3-N applied in
!!                                |watershed
!!    wshd_orgn     |kg N/ha      |average annual amount of organic N applied
!!                                |in watershed
!!    wshd_orgp     |kg P/ha      |average annual amount of organic P applied
!!                                |in watershed
!!    wshd_ftotn    |kg N/ha      |average annual amount of N (mineral & 
!!                                |organic) applied in watershed
!!    wshd_ftotp    |kg P/ha      |average annual amount of P (mineral &
!!                                |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units        |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frt_t        |
!!    gc           |
!!    gc1          |
!!    j            |none         |HRU number
!!    k            |none         |counter (soil layer #)
!!    rtof         |none         |weighting factor used to partition the 
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active 
!!                               |organic pools
!!    xx           |none         |fraction of fertilizer applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

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
      
      real, parameter :: rtof=0.5
      integer :: j, k, ifrt
      real :: xx, gc, gc1, swf, frt_t
   
      !!added by zhang
      !!======================
      real :: X1, X8, X10, XXX, YY, ZZ, XZ, YZ, RLN, orgc_f
      X1 = 0.
      X8 = 0.
      X10 = 0.
      XXX = 0.
      YY = 0.
      ZZ = 0.
      XZ = 0.
      YZ = 0.
      RLN = 0.
      orgc_f = 0.
      !!added by zhang
      !!======================  

      j = 0
      j = ihru

      ifrt = 0
      ifrt = ifrttyp


      do k = 1, 2
            xx = 0.
            if (k == 1) then
              xx = frt_surface              !!!    frt_surface   |none      fraction of fertilizer which is applied to the top 10 mm of soil (the remaining fraction is applied to first soil layer)         
            else
              xx = 1. - frt_surface                     
            endif

           !! ///added by Junyu Qi 04/2021
            if(ifrt == 4) then
                Inhibday(j) = 0   !! =1 for simulating inhib.
                sol_urea(k,j)= sol_urea(k,j)+ xx * frt_kg *  fnh3n(ifrt) * fminn(ifrt)
                sol_ph(k,j)= 8.  !! urea application
            else
            !sol_ph(k,j)= ori_sol_ph(k,j)
                sol_no3(k,j) = sol_no3(k,j) + xx * frt_kg * (1. - fnh3n(ifrt)) * fminn(ifrt)
                sol_nh4(k,j) = sol_nh4(k,j) + xx * frt_kg * fnh3n(ifrt) * fminn(ifrt)
                sol_solp(k,j) = sol_solp(k,j) + xx * frt_kg  * fminp(ifrt)
            end if
           !! added by Junyu Qi 04/2021  ///

            if (cswat == 0) then
                sol_fon(k,j) = sol_fon(k,j) + rtof * xx * frt_kg * forgn(ifrt)
	            sol_aorgn(k,j) = sol_aorgn(k,j) + (1. - rtof) * xx *  frt_kg * forgn(ifrt)
                sol_fop(k,j) = sol_fop(k,j) + rtof * xx * frt_kg * forgp(ifrt)
                sol_orgp(k,j) = sol_orgp(k,j) + (1. - rtof) * xx *  frt_kg * forgp(ifrt)
            end if
            
	        if (cswat == 1) then
	          sol_mc(k,j) = sol_mc(k,j) + xx * frt_kg *  forgn(ifrt) * 10.
	          sol_mn(k,j) = sol_mn(k,j) + xx * frt_kg *  forgn(ifrt)
	          sol_mp(k,j) = sol_mp(k,j) + xx * frt_kg *  forgp(ifrt)
	        end if

            !!By Zhang for C/N cycling 
            !!===========================
            
            !For organic fertilizer, the partition of carbon and nitrogen was determined by C:N ratio and they are allocated to structural and metabolic pools.
	        !Instead, SWAT used rtof to partition the organic nitrogen and phosphorus into fresh and stable organic pools.
	        if (cswat == 2) then
                sol_fop(k,j) = sol_fop(k,j) + rtof * xx *  frt_kg * forgp(ifrt)
                sol_orgp(k,j) = sol_orgp(k,j) + (1. - rtof) * xx * frt_kg * forgp(ifrt)
                
                !!Allocate organic fertilizer to Slow (SWAT_active) N pool;
                !sol_HSN(k,j) =sol_HSN(k,j)+(1.-rtof) * xx *frt_kg * forgn(ifrt)                        
                !sol_aorgn(k,j) = sol_HSN(k,j)

                !orgc_f is the fraction of organic carbon in fertilizer
                !for most fertilziers this value is set to 0.
                if (forgn(ifrt)  .gt. 1.E-6) then               
                    orgc_f = 0.35 !0.0

                    !X1 is fertlizer applied to layer (kg/ha)
                    !xx is fraction of fertilizer applied to layer
                    X1 = xx * frt_kg 
                    !X8: organic carbon applied (kg C/ha)
                    X8 = X1 * orgc_f
                      
                    OrgC_Fer(j)=OrgC_Fer(j)+ X8      
                      
                    !RLN is calculated as a function of C:N ration in fertilizer          
                    !RLN = .175 *(orgc_f)/(fminn(ifrt) + forgn(ifrt) + 1.e-5)
                    RLN = .175 *(orgc_f)/(forgn(ifrt) + 1.e-5)
                      
                    !X10 is the fraction of carbon in fertilizer that is allocated to metabolic litter C pool
                    X10 = .85-.018*RLN
                    if (X10<0.01) then
                        X10 = 0.01
                    else
                        if (X10 > .7) then
                            X10 = .7
                        end if
                    end if
                      
                    !XXX is the amount of organic carbon allocated to metabolic litter C pool
                    XXX = X8 * X10
                    sol_LMC(k,j) = sol_LMC(k,j) + XXX
                    !YY is the amount of fertilizer (including C and N) allocated into metabolic litter SOM pool
                    YY = X1 * X10
                    sol_LM(k,j) = sol_LM(k,j) + YY
                      
                    !amount of organic N allocated to metabolic litter N pool    
                    !sol_LMN(k,j) = sol_LMN(k,j) + X1 *rtof *forgn(ifrt) * X10      
                    !!remaining organic N is llocated to structural litter N pool
                    !sol_LSN(k,j) = sol_LSN(k,j) + X1*rtof*forgn(ifrt) *(1.- X10)   

                    !amount of organic N allocated to metabolic litter N pool    
                    sol_LMN(k,j) = sol_LMN(k,j) + X1 * forgn(ifrt) * X10       
                    !!remaining organic N is llocated to structural litter N pool
                    sol_LSN(k,j) = sol_LSN(k,j) + X1* forgn(ifrt) *(1.- X10)   
                         
                    sol_fon(k,j) = sol_LMN(k,j) + sol_LSN(k,j) 
                         
                    !XZ is the amount of organic carbon allocated to structural litter C pool   
                    XZ = X1 *orgc_f-XXX
                    sol_LSC(k,j) = sol_LSC(k,j) + XZ
                      
                    !assuming lignin C fraction of organic carbon to be 0.175; updating lignin amount in strucutral litter pool
                    sol_LSLC(k,j) = sol_LSLC(k,j) + XZ * .175          
                    !non-lignin part of the structural litter C is also updated;
                    sol_LSLNC(k,j) = sol_LSLNC(k,j) + XZ * (1.-.175) 
                      
                    !YZ is the amount of fertilizer (including C and N) allocated into strucutre litter SOM pool
                    YZ = X1 - YY
                    sol_LS(k,j) = sol_LS(k,j) + YZ
                    !assuming lignin fraction of the organic fertilizer allocated into structure litter SOM pool to be 0.175;
                    !update lignin weight in structural litter.
                    sol_LSL(k,j) = sol_LSL(k,j) + YZ*.175
             
                    CFOrfSTR(k,j) = XZ
                    CFOrfMET(k,j) = XXX
                    NFPltSTR(k,j) = X1*rtof*forgn(ifrt) *(1.- X10)
                    NFPltMET(k,j) = X1 *rtof *forgn(ifrt) * X10

                else 
                    orgc_f = 0.
                    CFOrfSTR(k,j) = 0.
                    CFOrfMET(k,j) = 0.
                    NFPltSTR(k,j) = 0.
                    NFPltMET(k,j) = 0.
                End if

                
	        end if
            !!By Zhang for C/N cycling 
            !!=========================== 


      end do 

!!!    write statement for virgina/mari-vaughn study        !!!
!      write (1112,1112) (sol_no3(k,j), sol_fon(k,j), sol_aorgn(k,j),    &
!    &sol_nh4(k,j),  sol_solp(k,j), sol_fop(k,j), sol_orgp(k,j),        &
!     &k = 1,4)
!1112  format (200f8.2)


!! add bacteria - #cfu/g * t(manure)/ha * 1.e6g/t * ha/10,000m^2 = 100.
!! calculate ground cover
      gc = 0.
      gc = (1.99532 - Erfc(1.333 * laiday(j) - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      gc1 = 0.
      gc1 = 1. - gc


      frt_t = 0.
      frt_t = bact_swf * frt_kg / 1000. !|fraction of manure containing active colony

      bactp_plt(j) = gc * bactpdb(ifrt) * frt_t * 100. + bactp_plt(j)
      bactlp_plt(j) = gc * bactlpdb(ifrt) * frt_t * 100. + bactlp_plt(j)

      bactpq(j) = gc1 * bactpdb(ifrt) * frt_t * 100. + bactpq(j)
      bactpq(j) = bactkddb(ifrt) * bactpq(j)

      bactps(j) = gc1 * bactpdb(ifrt) * frt_t * 100. + bactps(j)
      bactps(j) = (1. - bactkddb(ifrt)) * bactps(j)

      bactlpq(j) = gc1 * bactlpdb(ifrt) * frt_t * 100. + bactlpq(j)
      bactlpq(j) = bactkddb(ifrt) * bactlpq(j)

      bactlps(j) = gc1 * bactlpdb(ifrt) * frt_t * 100. + bactlps(j)
      bactlps(j) = (1. - bactkddb(ifrt)) * bactlps(j)


!! summary calculations

       !! //considering UREA
        if(ifrt == 4) then
              fertno3 = 0.
              no3_fert(j) = no3_fert(j) + fertno3   !!Zhang added   
              fertnh4 = 0.
              nh4_fert(j) = nh4_fert(j) + fertnh4
              fertsolp = 0.
              solp_fert(j)=solp_fert(j) + fertsolp    !! added by Junyu Qi
        else
              fertno3 = frt_kg * fminn(ifrt) * (1. - fnh3n(ifrt))
              no3_fert(j) = no3_fert(j) + fertno3   !!Zhang added   
              fertnh4 = frt_kg * (fminn(ifrt) * fnh3n(ifrt))
              nh4_fert(j) = nh4_fert(j) + fertnh4   !!Zhang added
              solp_fert(j)=solp_fert(j) + fertsolp    !! added by Junyu Qi
              fertorgp = frt_kg * forgp(ifrt)  
        end if


      fertorgn = frt_kg * forgn(ifrt)
      orgn_fert(j) = orgn_fert(j) + fertorgn 
      fertsolp = frt_kg * fminp(ifrt)

      orgp_fert(j) = orgp_fert(j) + fertorgp  
      
      fertn = fertn + (frt_kg                    + cfertn) *  (fminn(ifrt) + forgn(ifrt))

      fertp = fertp + (frt_kg                    + cfertp) *  (fminp(ifrt) + forgp(ifrt))

      tfertn(j) = tfertn(j) + fertn
      tfertp(j) = tfertp(j) + fertp

      if (curyr > nyskip) then
      wshd_ftotn = wshd_ftotn + frt_kg * hru_dafr(j) * (fminn(ifrt) + forgn(ifrt))

      wshd_forgn = wshd_forgn + frt_kg  * hru_dafr(j)* forgn(ifrt)

      wshd_fno3 = wshd_fno3 + frt_kg  * hru_dafr(j) *   fminn(ifrt) * (1. - fnh3n(ifrt))

      wshd_fnh3 = wshd_fnh3 + frt_kg  * hru_dafr(j) *   fminn(ifrt) * fnh3n(ifrt)

      wshd_ftotp = wshd_ftotp + frt_kg  * hru_dafr(j)  * (fminp(ifrt) + forgp(ifrt))

      wshd_fminp = wshd_fminp + frt_kg  * hru_dafr(j)   * fminp(ifrt)

      wshd_forgp = wshd_forgp + frt_kg  * hru_dafr(j)   * forgp(ifrt)

      end if


!! increase fertilizer sequence number by one
      nfert(j) = nfert(j) + 1

      return
      end