      subroutine cfactor
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily soil loss caused by water erosion
!!    using the modified universal soil loss equation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cvm(:)      |none          |natural log of USLE_C (the minimum value
!!                               |of the USLE C factor for the land cover)
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    icr(:)      |none          |sequence number of crop grown within a year
!!    idplt(:,:,:)|none          |land cover code from crop.dat
!!    ihru        |none          |HRU number
!!    iwave       |none          |flag to differentiate calculation of HRU and
!!                               |subbasin sediment calculation
!!                               |iwave = 0 for HRU
!!                               |iwave = subbasin # for subbasin
!!    nro(:)      |none          |sequence number of year in rotation
!!    peakr       |m^3/s         |peak runoff rate
!!    rsd_covco   |              |residue cover factor for computing fraction of
!!                                  cover
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    sub_km(:)   |km^2          |area of subbasin in square kilometers
!!    sub_qd(:)   |mm H2O        |surface runoff loading from subbasin for day
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    usle_mult(:)|none          |product of USLE K,P,LS,exp(rock)
!!    wcklsp(:)   |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cklsp(:)    |
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion
!!    usle        |metric tons/ha|daily soil loss predicted with USLE equation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |
!!    j           |none          |HRU number
!!    bio_frcov   |              |fraction of cover by biomass - adjusted for
!!                                  canopy height
!!    grcov_fr    |              |fraction of cover by biomass as function of lai
!!    rsd_frcov   |              |fraction of cover by residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use parm_control
      implicit none

      integer :: j
      real :: c, rsd_frcov, grcov_fr, bio_frcov, d1, d2, xx

      j = 0
      j = ihru
      
      !! initialize variables
      c = 0.

      !! HRU sediment calculations
      if (icfac == 0) then
      
      
              !--------------qichun-----forestry------------------------------------------------------------------------------------
           if(ifor==1) then
          
                if (idplt(j) > 0) then     
                    c = Exp((-.2231 - cvm(idplt(j))) * Exp(-.00115 * (sol_cov(j)+.8 * bio_ms(j))) + cvm(idplt(j)))
                else 
                    if (sol_cov(j) > 1.e-4) then
                        c = Exp(-.2231 * Exp(-.00115 * (sol_cov(j)+.8 * bio_ms(j))))
                    else
                        c = .8
                    end if
                end if 
           
          else 
            
            !!==========Modified by XZ===================
         
             if (idplt(j) > 0) then   
                d1 =  (-.2231 - cvm(idplt(j))) * Exp(-.00115 * sol_cov(j)) 
                d2 = cvm(idplt(j))
                c = Exp((-.2231 - cvm(idplt(j))) * Exp(-.00115 * sol_cov(j)) + cvm(idplt(j)))              
                c = Exp((-.2231 - cvm(idplt(j))) * Exp(-.00015 * sol_cov(j)) + cvm(idplt(j)))             
                xx = min(10.0, sol_cov(j)/1000.)
                c = 0.8*exp(-1.0*xx) + exp(cvm(idplt(j))) !!!from EPIC   
                
                !xx = min(10.0, sol_cov(j)/1000.)
                !c = 0.8*exp(-1.0*xx) + exp(cvm(idplt(j))) !*(.9*(1.-CVP)+.1)  !! from EPIC
             !!==========Modified by XZ===================
             else 
                if (sol_cov(j) > 1.e-4) then 
                    c = Exp(-.2231 * Exp(-.00115 * sol_cov(j)))           
                else
                    c = .8
                end if    
             end if
          
           end if  
            !--------------qichun-----forestry------------------------------------------------------------------------------------

      else
        rsd_frcov = Exp(-rsd_covco * sol_cov(j))
        grcov_fr = laiday(j) / (laiday(j) + Exp(1.748 - 1.748*laiday(j)))
        bio_frcov = 1. - grcov_fr * Exp(-.01*cht(j))
        c = amax1(1.e-10,rsd_frcov*bio_frcov)
      end if


      usle_cfac(ihru) = c
      if (ihru == 212) then
        !write(*,*)
      end if
      
      return
      end