        subroutine readco2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads co2 concentrations
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    
!!   
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    !! qichun co2-
      use parm 
      use parm_subC
      implicit none
      integer :: eof, co2year, ttyear, ecyear
      real :: annco2, annco2eq, annKYOTO_CO2EQ, annCH4, annN2O
      character (len=80) :: titldum

      eof = 0
      ttyear = 0
      read (co2_dat_num,*,iostat=eof) titldum
      read (co2_dat_num,*,iostat=eof) titldum
      read (co2_dat_num,*,iostat=eof) titldum
      !read (co2_dat_num,*,iostat=eof) ttyear,annco2 
      !co2con(ttyear) = annco2
      read (co2_dat_num,*,iostat=eof) ttyear, annco2eq, annKYOTO_CO2EQ, annco2, annCH4, annN2O
      co2con(ttyear) = annco2
      do
        co2year = 0
        annco2 = 0.
        annco2eq = 0.
        annKYOTO_CO2EQ = 0.
        annCH4 = 0.
        annN2O = 0.
        !read (co2_dat_num,*,iostat=eof) co2year,annco2
        read (co2_dat_num,*,iostat=eof) co2year, annco2eq, annKYOTO_CO2EQ, annco2, annCH4, annN2O
        if (eof < 0) exit
        co2con(co2year) = annco2
      end do
  
      !! inital co2 concentrations for years without available data
      do ecyear = 1, ttyear
        co2con(ecyear) = co2con(ttyear)
      end do 
      

      close (co2_dat_num)
      return
      end