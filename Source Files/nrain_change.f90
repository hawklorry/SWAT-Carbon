      subroutine nrain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year of simulation
!!    drydep_no3  |kg/ha/yr      |atmospheric dry deposition of nitrates
!!    drydep_nh4  |kg/ha/yr      |atmospheric dry deposition of ammonia
!!    hru_dafr(:) |none          |fraction of watershed in HRU
!!    ihru        |none          |HRU number
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    rcn         |mg/L          |Concentration of nitrogen in the rainfall
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    wshd_raino3 |kg N/ha       |average annual amount of NO3 added to soil
!!                               |by rainfall in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    wshd_raino3 |kg N/ha       |average annual amount of NO3 added to soil
!!                               |by rainfall in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

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
      
      integer :: j
      real :: nh3pcp

      j = 0
      j = ihru
      
      select case (iatmodep)        
        case (0)  !! average annual         
!! calculate nitrate in precipitation
            nh3pcp = .01 * rammo_sub(hru_sub(j)) * precipday
            no3pcp = .01 * rcn_sub(hru_sub(j)) * precipday
            sol_nh4(1,j) = sol_nh4(1,j) + nh3pcp + drydep_nh4(hru_sub(j))/365.
            sol_no3(1,j) = sol_no3(1,j) + no3pcp + drydep_no3(hru_sub(j))/365.
            
            no3_rain(j) = no3_rain(j) + no3pcp + drydep_no3(hru_sub(j))/365. 
     
            nh4_rain(j) = nh4_rain(j)+ nh3pcp +  drydep_nh4(hru_sub(j))/365.
     
     
     
        case (1)  !! monthly     
            nh3pcp = .01 * rammo_mo(mo_atmo,hru_sub(j)) * precipday
            no3pcp = .01 * rcn_mo(mo_atmo,hru_sub(j)) * precipday
            sol_nh4(1,j) = sol_nh4(1,j) + nh3pcp + 	drydep_nh4_mo(mo_atmo,hru_sub(j)) / ndays(i_mo+1)   !!!!!nbs/mjw 051515
            sol_no3(1,j) = sol_no3(1,j) + no3pcp + 	drydep_no3_mo(mo_atmo,hru_sub(j)) / ndays(i_mo+1)   !!!!!nbs/mjw 050515
            no3_rain(j) = no3_rain(j) + no3pcp + 	drydep_no3_mo(mo_atmo,hru_sub(j)) / ndays(i_mo)     
            nh4_rain(j) = nh4_rain(j)+ nh3pcp +  	drydep_nh4(hru_sub(j))/365.
     
   
        case (2)  !! daily
           nh3pcp = .01 * rammo_d(hru_sub(j)) * precipday
           no3pcp = .01 * rcn_d(hru_sub(j)) * precipday
           sol_nh4(2,j) = sol_nh4(2,j) + nh3pcp  + drydep_nh4_d(hru_sub(j))
           sol_no3(2,j) = sol_no3(2,j) + no3pcp + drydep_no3_d(hru_sub(j))

           no3_rain(j) = no3_rain(j) + no3pcp +  drydep_no3_d(hru_sub(j))
   
           nh4_rain(j) = nh4_rain(j)+ nh3pcp + 	drydep_nh4(hru_sub(j))/365.
     
     
      end select

!! summary calculations
      if (curyr > nyskip) then
        wshd_raino3 = wshd_raino3 + no3pcp * hru_dafr(j)
      end if

      return
      end