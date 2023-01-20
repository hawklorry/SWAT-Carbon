      subroutine subday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha            |area of watershed in hectares
!!    iida          |julian date   |current day of simulation
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_etday(:)  |mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_fr(:)     |none          |fraction of watershed area in subbasin
!!    sub_gwq(:)    |mm H2O        |groundwater flow on day in subbasin
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_no3(:)    |kg N/ha       |NO3-N in surface runoff on day in subbasin
!!    sub_pet(:)    |mm H2O        |potential evapotranspiration for day in
!!                                 |subbasin
!!    sub_qd(:)     |mm H2O        |surface runoff loading to main channel on
!!                                 |day in subbasin
!!    sub_sedpa(:)  |kg P/ha       |amount of active mineral P attached to 
!!                                 |sediment removed in surface runoff on day 
!!                                 |in subbasin
!!    sub_sedps(:)  |kg P/ha       |amount of stable mineral P attached to 
!!                                 |sediment removed in surface runoff on day 
!!                                 |in subbasin
!!    sub_sedy(:)   |metric tons   |sediment yield for the day in subbasin
!!    sub_sep(:)    |mm H2O        |seepage from bottom of soil profile on day
!!                                 |in subbasin
!!    sub_snom(:)   |mm H2O        |snow melt on day in subbasin
!!    sub_solp(:)   |kg P/ha       |soluble P in surface runoff on day in 
!!                                 |subbasin
!!    sub_subp(:)   |mm H2O        |precipitation for day in subbasin
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    sub_wyld(:)   |mm H2O        |water yield on day in subbasin
!!    sub_yorgn(:)  |kg N/ha       |organic N in surface runoff on day in 
!!                                 |subbasin
!!    sub_yorgp(:)  |kg P/ha       |organic P in surface runoff on day in 
!!                                 |subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    sub_ha      |ha            |area of subbasin in hectares
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
      
      integer :: sb, ii,icl
      real :: sub_ha
      real, dimension (msubo) :: pdvab, pdvb

      sb = 0
      sb = hru_sub(ihru)

      sub_ha = 0.
      sub_ha = da_ha * sub_fr(sb)

      pdvab = 0.
      pdvb = 0.

      pdvab(1) = sub_subp(sb)     !! swat-cup 5
      pdvab(2) = sub_snom(sb)
      pdvab(3) = sub_pet(sb)
      pdvab(4) = sub_etday(sb)
      pdvab(5) = sub_sw(sb)
      pdvab(6) = sub_sep(sb)
      pdvab(7) = sub_qd(sb)
      pdvab(8) = sub_gwq(sb) 
      pdvab(9) = sub_wyld(sb)
      pdvab(10) = sub_sedy(sb)/ sub_ha
      pdvab(11) = sub_yorgn(sb)
      pdvab(12) = sub_yorgp(sb)
      pdvab(13) = sub_no3(sb)
      pdvab(14) = sub_solp(sb)
      pdvab(15) = sub_sedpa(sb) + sub_sedps(sb)
      pdvab(16) = sub_latq(sb)
      pdvab(17) = sub_latno3(sb)
      pdvab(18) = sub_gwno3(sb)
      pdvab(19) = sub_chl(sb) / sub_ha
      pdvab(20) = sub_wtmp(sb)         ! sub_cbod(sb) / sub_ha               
      pdvab(21) = sub_dox(sb) / sub_ha
      pdvab(22) = sub_tileno3(sb)    !! tileno3
      pdvab(23) = sub_tileq(sb)      !! tile flow  jane f.
      pdvab(24) = sub_vaptile(sb)    !! phos due to crack flow
      
      pdvab(25) = sub_n2o(sb)        !! g/ha day
      pdvab(26) = sub_no(sb)           !! g/ha day
      pdvab(27) = sub_nh4(sb)        !!kg N/ha 
      pdvab(28) = sub_ch4(sb) 
      pdvab(29) = sub_dnit(sb)             !!kg N/ha 
      pdvab(30) =  sub_nit(sb)               !!kg N/ha 
      pdvab(31) =  sub_percno3(sb)     !!kg N/ha 
      pdvab(32) = sub_upno3(sb)
      pdvab(33) =  sub_ferno3(sb)              !!kg N/ha 
      pdvab(34) =  sub_fernh4(sb)               !!kg N/ha 
      pdvab(35) =  sub_ferorgn(sb)     !!kg N/ha 
      pdvab(36) =  sub_rainno3(sb)  
      pdvab(37) =  sub_fixn(sb)               !!kg N/ha 
      pdvab(38) =  sub_solno3(sb)                !!kg N/ha 
      pdvab(39) =  sub_solnh3(sb)               !!kg N/ha 
      pdvab(40) =  sub_solorgn(sb)                !!kg N/ha 
      pdvab(41) =  sub_solorgp(sb)              !!kg N/ha 
      pdvab(42) =  sub_sedc(sb) 
      pdvab(43) =  sub_surfqc(sb) 
      pdvab(44) =  sub_latc(sb)
      pdvab(45) =  sub_percc(sb) 
      pdvab(46) =  sub_NPPC(sb) 
      pdvab(47) =  sub_rspc(sb) 
      pdvab(48) = sub_snofall(sb)
      pdvab(49) = sub_snodep(sb)
      pdvab(50) = sub_snohru(sb)
      pdvab(51) =  sub_sur_tmp(sb)             !55
      pdvab(52) = sub_soltmp_50(sb)
      pdvab(53) = sub_soltmp_100(sb)
      pdvab(54) =  sub_soltmp_150(sb)
      pdvab(55) =  sub_soltmp_200(sb)
      pdvab(56) =  sub_soltmp_300(sb)
      pdvab(58) = sub_soltmp_500(sb)
      pdvab(58) =  sub_soltmp_1000(sb)
      pdvab(59) =  sub_frozday(sb)
      pdvab(60) =  sub_airtmp(sb)
      pdvab(61) =  sub_swc(sb)                  !65
      pdvab(62) =  sub_solorgc(sb)
      pdvab(63) =  sub_solorgsc(sb)
      pdvab(64) = sub_wtmp_surq(sb)               !65
      pdvab(65) = sub_wtmp_latq(sb)
      pdvab(66) = sub_wtmp_gwq(sb)

      if (ipdvab(1) > 0) then
        do ii = 1, itotb
          pdvb(ii) = pdvab(ipdvab(ii))
        end do
        if (icalen == 0) write(output_sub_num,1000)sb, subgis(sb), iida, sub_km(sb), (pdvb(ii), ii = 1, itotb)
        if (icalen == 1) write(output_sub_num,1001)sb, subgis(sb), i_mo, icl(iida), iyr, sub_km(sb), (pdvb(ii), ii = 1, itotb)
 
!!    added for binary files 3/25/09 gsm line below and write (66666
	      if (ia_b == 1) then
	        write (outputb_sub_num) sb, subgis(sb), iida, sub_km(sb), (pdvb(ii), ii = 1, itotb)
	      endif
      else
        if (icalen == 0)write(output_sub_num,1000) sb, subgis(sb), iida, sub_km(sb), (pdvab(ii), ii = 1, msubo)
        if (icalen == 1)write(output_sub_num,1001) sb, subgis(sb), i_mo, icl(iida), iyr, sub_km(sb), (pdvab(ii), ii = 1, msubo)
!!    added for binary files 3/25/09 gsm line below and write (6666
	    if (ia_b == 1) then
            write(outputb_sub_num) sb, subgis(sb), iida, sub_km(sb), (pdvab(ii), ii = 1, msubo)
        endif        
	  end if

 !!    output solar, windspeed, relative humidity to new output file (daily only)
      if (iprp == 2) then 
        write (2222,2000) sb, subgis(sb), iida, iyr, sub_km(sb), hru_ra(sb), u10(sb), rhd(sb)
      end if 

      return
      
 1000 format('BIGSUB',i5,1x,i8,1x,i4,1x,e10.4,18f10.3,e10.3,5e10.3, 42f10.3)         	!!R666b 7/19/17 nbs
 1001 format('BIGSUB',i5,1x,i8,1x,i2,1x,i2,1x,i4,1x,e10.5,18e10.3,1x,  e10.5, 5e10.3)   !!R666b 7/19/17 nbs
 2000 format (i5,1x,i8,1x,i4,5x,i4,5x,e10.5,5x,3f8.2)
      end 