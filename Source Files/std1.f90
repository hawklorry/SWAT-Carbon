      subroutine std1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes general information to the standard input/output file
!!    and header lines to miscellaneous output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_km       |km**2         |area of the watershed in square kilometers
!!    icrk        |none          |crack flow code
!!                               |1: compute flow in cracks
!!    ideg        |none          |channel degredation code
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    idg(:)      |none          |array location of random generator seed
!!    idt         |minutes       |length of time step used to report
!!                               |precipitation data for sub-daily modeling
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    igen        |none          |random number generator seed code
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality (QUAL2E)
!!    nbyr        |none          |number of calendar years simulated
!!    pcpsim      |none          |rainfall input code
!!    prog        |NA            |program name and version
!!    rndseed(:,:)|none          |random number seeds 
!!    tmpsim      |none          |temperature input code
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      implicit none

!!    input summary file
      write (input_std_num,1000) prog,values(2),values(3),values(1),values(5),values(6),values(7)
      write (input_std_num,1010) title
      write (input_std_num,1020) nbyr, da_km
      if (igen == 0) then
        write (input_std_num,1030)                         ! 1030 format (t10,'Random number generator cycles: 0, use default numbers')
      else
        write (input_std_num,1040) igen                    ! 1040 format (t10,'Random number generator cycles: ',i4)
      end if
      write (input_std_num,1050) rndseed(idg(1),1)         ! 1050 format (/t10,'Initial random number seed: wet/dry day prob  ',1x, i14)
      write (input_std_num,1051) rndseed(idg(2),1)         ! 1051 format (t10,'Initial random number seed: radiation         ',1x,  i14)
      write (input_std_num,1052) rndseed(idg(3),1)         ! 1052 format (t10,'Initial random number seed: precipitation     ',1x,  i14)
      write (input_std_num,1053) rndseed(idg(4),1)         ! 1053 format (t10,'Initial random number seed: 0.5 hr rainfall   ',1x,  i14)
      write (input_std_num,1054) rndseed(idg(5),1)         ! 1054 format (t10,'Initial random number seed: wind speed        ',1x,  i14)
      write (input_std_num,1055) rndseed(idg(6),1)         ! 1055 format (t10,'Initial random number seed: irrigation        ',1x,  i14)
      write (input_std_num,1056) rndseed(idg(7),1)         ! 1056 format (t10,'Initial random number seed: relative humidity ',1x,  i14)
      write (input_std_num,1057) rndseed(idg(8),1)         ! 1057 format (t10,'Initial random number seed: max temperature   ',1x,  i14)
      write (input_std_num,1058) rndseed(idg(9),1)         ! 1058 format (t10,'Initial random number seed: min temperature   ',1x,  i14)
      write (input_std_num,1060)                           ! 1060 format (/t10,'Precipitation data used in run:')

      select case (pcpsim)
        case (1)
          write (input_std_num,1061)
          if (ievent > 0) then
            write (input_std_num,1062) idt         ! 1062 format (t14,'Subdaily rainfall data used, summarized every ',i3,  'min')
          else
            write (input_std_num,1063)             !1063 format (t14,'Daily rainfall data used')
          end if
        case (2)
          write (input_std_num,1064)
      end select
      write (input_std_num,1070)                   ! 1070 format (/t10,'Temperature data used in run:')
      select case (tmpsim)
        case (1)
          write (input_std_num,1071)               ! 1071 format (t11,'Multiple gages read for watershed')
        case (2)
          write (input_std_num,1072)               ! 1072 format (t11,'Multiple gages simulated for watershed')
      end select

      select case (ipet)
        case (0)
          write (input_std_num,1080)               ! 1080 format (/t10,'PET method used: Priestley-Taylor')
        case (1)
          write (input_std_num,1081)               ! 1081 format (/t10,'PET method used: Penman-Monteith')
        case (2)
          write (input_std_num,1082)               ! 1082 format (/t10,'PET method used: Hargreaves')
        case (3)
          write (input_std_num,1083)               ! 1083 format (/t10,'PET method used: read in values')
      end select

      write (input_std_num,1090)                   ! 1090 format (/t10,'Rainfall/Runoff/Routing Option:')   
      select case (ievent)
        case (0)
          write (input_std_num,1091)               ! 1091 format (t11,'Daily rainfall data',/t11,'Runoff estimated with ',   'curve number method',/t11,'Daily stream routing')
        case (1)
          write (input_std_num,1094)               ! 1094 format (t11,'Subdaily rainfall data',/t11,'Runoff estimated with',' Green & Ampt method',/t11,'Hourly stream routing')
      end select
      select case (irte)
        case (0)
          write (input_std_num,1095)               ! 1095 format (t12,'Variable Storage routing method')
        case (1)
          write (input_std_num,1096)               ! 1096 format (t12,'Muskingum routing method')
      end select
      select case (ideg)
        case (0)
          write (input_std_num,1097)               ! 1097 format (t12,'Channel dimensions remain constant')
        case (1)
          write (input_std_num,1098)               ! 1098 format (t12,'Channel dimensions change due to deposition/degrad',  'ation')
      end select
      select case (isubwq)
        case (0)
          write (input_std_num,1101)               ! 1101 format (t12,'Subbasin algae/CBOD loadings not modeled')
        case (1)
          write (input_std_num,1102)               ! 1102 format (t12,'Subbasin algae/CBOD loadings modeled')
      end select
      select case (iwq)
        case (0)
          write (input_std_num,1099)               ! 1099 format (t12,'In-stream nutrient transformations not modeled')
        case (1)
          write (input_std_num,1100)               ! 1100 format (t12,'In-stream nutrient transformations modeled using', ' QUAL2E equations')

      end select

      if (icrk == 1) write (input_std_num,1110)    ! 1110 format (/t10,'Crack flow modeled')

!!    standard output file
      !!~~~ SQLite ~~~
      if(ioutput == 1) then
        !!do nothing
      else
      write (output_std_num,1000) prog,values(2),values(3),values(1),values(5), values(6),values(7)
      write (output_std_num,1010) title
      write (output_std_num,1020) nbyr, da_km
      end if
      !!~~~ SQLite ~~~
      if (isproj == 1) then
        write (output2_std_num,1000) prog,values(2),values(3),values(1),values(5), values(6),values(7)
        write (output2_std_num,1010) title
        write (output2_std_num,1020) nbyr, da_km
      end if
 
!!    hyd.out file
      write (hyd_out_num,5000)                !5000 format ('  icode',t11,'ic',t14,'inum1',t20,'inum2',t26,'inum3',   
                                       !t34,'subed',t41,'recmonps',t50,'reccnstps',t61,'flow(m^3)',      
                                       !t73,'sed(t)',t85,'orgn(kg)',t97,'orgp(kg)',t109,'nitrate(kg)',   
                                       !t121,'sol.p(kg)',t133,'sol.pst(mg)',t145,'sor.pst(mg)')

!!    chan.deg file
      !!~ ~ ~ SQLite ~ ~ ~
      if(ioutput == 0) then
      write (16,7000)                    !7000 format (/,' Initial Dimen',' Channel Dimensions ',/,' Reach',  '    Depth (m)','  Width (m)','  Slope (m/m)')
      end if
      !!~ ~ ~ SQLite ~ ~ ~
      
      return
 1000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 1010 format (/(t5,20a4))
 1020 format (t10,'Number of years in run: ',i4/t10, 'Area of watershed: ',f12.3,' km2')
 1030 format (t10,'Random number generator cycles: 0, use default numbers')
 1040 format (t10,'Random number generator cycles: ',i4)
 1050 format (/t10,'Initial random number seed: wet/dry day prob  ',1x,i14)
 1051 format (t10,'Initial random number seed: radiation         ',1x,i14)
 1052 format (t10,'Initial random number seed: precipitation     ',1x,i14)
 1053 format (t10,'Initial random number seed: 0.5 hr rainfall   ',1x,i14)
 1054 format (t10,'Initial random number seed: wind speed        ',1x, i14)
 1055 format (t10,'Initial random number seed: irrigation        ',1x, i14)
 1056 format (t10,'Initial random number seed: relative humidity ',1x, i14)
 1057 format (t10,'Initial random number seed: max temperature   ',1x, i14)
 1058 format (t10,'Initial random number seed: min temperature   ',1x, i14)
 1060 format (/t10,'Precipitation data used in run:')
 1061 format (t11,'Multiple gages read for watershed')
 1062 format (t14,'Subdaily rainfall data used, summarized every ',i3, 'min')
 1063 format (t14,'Daily rainfall data used')
 1064 format (t11,'Multiple gages simulated for watershed')
 1070 format (/t10,'Temperature data used in run:')
 1071 format (t11,'Multiple gages read for watershed')
 1072 format (t11,'Multiple gages simulated for watershed')
 1080 format (/t10,'PET method used: Priestley-Taylor')
 1081 format (/t10,'PET method used: Penman-Monteith')
 1082 format (/t10,'PET method used: Hargreaves')
 1083 format (/t10,'PET method used: read in values')
 1090 format (/t10,'Rainfall/Runoff/Routing Option:')
 1091 format (t11,'Daily rainfall data',/t11,'Runoff estimated with ',  'curve number method',/t11,'Daily stream routing')
 1094 format (t11,'Subdaily rainfall data',/t11,'Runoff estimated with', ' Green & Ampt method',/t11,'Hourly stream routing')
 1095 format (t12,'Variable Storage routing method')
 1096 format (t12,'Muskingum routing method')
 1097 format (t12,'Channel dimensions remain constant')
 1098 format (t12,'Channel dimensions change due to deposition/degrad', &
 	'ation')
 1099 format (t12,'In-stream nutrient transformations not modeled')
 1100 format (t12,'In-stream nutrient transformations modeled using',   ' QUAL2E equations')
 1101 format (t12,'Subbasin algae/CBOD loadings not modeled')
 1102 format (t12,'Subbasin algae/CBOD loadings modeled')
 1110 format (/t10,'Crack flow modeled')
 5000 format ('  icode',t11,'ic',t14,'inum1',t20,'inum2',t26,'inum3',   &
      t34,'subed',t41,'recmonps',t50,'reccnstps',t61,'flow(m^3)',      &
      t73,'sed(t)',t85,'orgn(kg)',t97,'orgp(kg)',t109,'nitrate(kg)',   &
      t121,'sol.p(kg)',t133,'sol.pst(mg)',t145,'sor.pst(mg)')
 7000 format (/,' Initial Dimen',' Channel Dimensions ',/,' Reach',  '    Depth (m)','  Width (m)','  Slope (m/m)')
      end