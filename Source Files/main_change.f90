      !include 'modparm.f'
      program main
!!    this is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    values(1)   |year          |year simulation is performed
!!    values(2)   |month         |month simulation is performed
!!    values(3)   |day           |day in month simulation is performed
!!    values(4)   |minutes       |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    values(5)   |hour          |hour simulation is performed
!!    values(6)   |minutes       |minute simulation is performed
!!    values(7)   |seconds       |second simulation is performed
!!    values(8)   |milliseconds  |millisecond simulation is performed
!!    zone        |NA            |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: date_and_time
!!    SWAT: getallo, allocate_parms, readfile, readfig
!!    SWAT: readbsn, std1, readwwq, readinpt, std2, storeinitial
!!    SWAT: openwth, headout, simulate, finalbal, writeaa, pestw 
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use parm_control
      use parm_output
      use parm_subC
      use parm_subE
      use parm_subH
      use parm_rchC
      use parm_rchE
      !use ifwin, only : GetCurrentDirectory     !!not sure  nbs
      implicit none

      character(len=256) :: CurrentDir
      integer :: ilen

      prog = "SWAT-Carbon 2022    VER 2022/Jan13"
      write (*,1000)
 1000 format(1x,"            SWAT-Carbon 2022         ",/,             &
               "       Merged SWAT2012 to Rev. 683    ",/,             &
               "      Soil & Water Assessment Tool    ",/,             &
               "               PC Version             ",/,             &
               " Program reading from file.cio . . . executing",/)

!! process input
		

      !ilen = GetCurrentDirectory( len(CurrentDir), CurrentDir )  !! not sure nbs
      !write (*, *) "Current directory: ", trim(CurrentDir)
      !print *, "Current directory: ", trim(CurrentDir)
      
      !!set file no for different types of files
      call Initialize_File_No
      
      call getallo                       !!figfile_num = 27, solfile_num = 9, mgtfile1_num = 10, chmfile1_num = 11
      
      !! for each opened files, after finishing reading the contents, we need to close them
      
      call allocate_parms   
      call allocate_parms_control ! global variables for module control
      call allocate_parms_output  ! global variables for output
      call allocate_parms_subC   ! global variables for C/N cycling at subbasin scale
      call allocate_parms_subE   ! global variables for energy transfer at subbasin scale
      call allocate_parms_subH   ! global variables for hydrology and water movement at subbasin scale
      call allocate_parms_rchC   ! global variables for C/N cycling in river systems, added
      call allocate_parms_rchE   ! global variables for energy transfer in river systems
      call allocate_parms_Du     ! global variables for C cycling
      
      call zero0
      call zero1
      call zero2     
      call zeroini
      call zero_urbn 
      
      call readfile         !initilized iyr in this function
      !!readfile-Qi.f90(749):        open (20,file="output2.rch",recl=600)
      
      ! modified schedule_mgt by Zhang
      mgtyr_hru = iyr     
      
      
      call readfile_new     !this subroutine opens the main input and output files
      !!readfile_new-Qi.f90(36):      open (1996,file="output2.rch")
      
      call readbsn
      call readwwq
      
      call readcbn           !!newly added, read in setups/configurations of the modules/algorithms in SWAT-C
      call readCEQUAL        !newly added
      call readBottomAlgae   !newly added
      call readCarbonSoil    !newly added
      call readEnergyPara    !newly added
      call readCarbonSedFlux !newly added, parameter placeholder for sediment flux module
      
      if (fcstyr > 0 .and. fcstday > 0) call readfcst
      call readplant             !! read in the landuse/landcover database
      call readtill              !! read in the tillage database
      call readpest              !! read in the pesticide database
      call readfert              !! read in the fertilizer/nutrient database
      call readurban             !! read in the urban land types database
      call readseptwq            !! read in the septic types database
      call readlup
      call readfig
      call readatmodep
      call readinpt
      
      call std1
      call std2  
      if (iwea==0) call openwth    
      call headout
      !call sw_init                      !!R673 8/16/19 nbs (new subroutine)
      call headout_S          !! subbasin output head
      call headout_R          !! reach output head
      !! convert integer to string for output.mgt file
      subnum = ""
      hruno = ""
      do i = 1, mhru
        write (subnum(i),fmt=' (i5.5)') hru_sub(i)
        write (hruno(i),fmt=' (i4.4)') hru_seq(i)  
      end do

      if (isproj == 2) then 
        hi_targ = 0.0
      end if

!! save initial values
      if (isproj == 1) then
        scenario = 2
        call storeinitial
      else if (fcstcycles > 1) then
        scenario =  fcstcycles
        call storeinitial
      else
        scenario = 1
      endif
        if (iclb /= 4) then
      do iscen = 1, scenario

     
        !! simulate watershed processes
        call simulate

        !! perform summary calculations
        call finalbal
        call writeaa
        call writeaa_S    !! write annual outputs
        call writeaa_R    !! write annual outputs
        call pestw

        !!reinitialize for new scenario
        if (scenario > iscen) call rewind_init
      end do
         end if
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)
      write (*,1001)
 1001 format (/," Execution successfully completed ")
	
        iscen=1
!! file for Mike White to review to ensure simulation executed normally
      open (fin_fin_num,file='fin.fin')
      write (fin_fin_num,*) 'Execution successful'
      close (fin_fin_num)
 
	stop
      end