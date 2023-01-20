      subroutine getallo

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of HRUs, subbasins, etc. in the 
!!   simulation. These values are used to allocate array sizes.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |maximum number of applications
!!    mch         |none        |maximum number of channels
!!    mcr         |none        |maximum number of crops grown per year
!!    mcrdb       |none        |max number of lu/lc defined in crop.dat
!!    mcut        |none        |maximum number of cuttings per year
!!    mfcst       |none        |maximum number of forecast stations
!!    mfdb        |none        |max number of fertilizers in fert.dat
!!    mgr         |none        |maximum number of grazings per year
!!    mhru        |none        |maximum number of HRUs in watershed
!!    mhyd        |none        |maximum number of hydrograph nodes
!!    mlyr        |none        |maximum number of soil layers 
!!    mnr         |none        |max number of years of rotation
!!    mpst        |none        |max number of pesticides used in wshed
!!    mpdb        |none        |max number of pesticides in pest.dat
!!    mrecc       |none        |maximum number of reccnst files
!!    mrecd       |none        |maximum number of recday files
!!    mrech       |none        |maximum number of rechour files
!!    mrecm       |none        |maximum number of recmon files
!!    mrecy       |none        |maximum number of recyear files
!!    mres        |none        |maximum number of reservoirs
!!    mrg         |none        |max number of rainfall/temp gages
!!    nstep       |none        |max number of time steps per day
!!    msub        |none        |maximum number of subbasins
!!    mtil        |none        |max number of tillage types in till.dat
!!    mudb        |none        |maximum number of urban land types in urban.dat
!!    myr         |none        |max number of years of simulation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |NA          |comment flag
!!    plantdb     |NA          |name of LU/LC database input file (crop.dat)
!!    eof         |none        |end of file flag
!!    fcstfile    |NA          |name of weather forecast input file (.cst)
!!    fcsttot     |none        |total number of forecast regions in database
!!    fertdb      |NA          |name of fertilizer database file (fert.dat)
!!    figfile     |NA          |name of watershed configuration file (.fig)
!!    i           |none        |counter
!!    ic          |none        |number of land cover in crop database
!!    icd         |none        |routing command code (.fig)
!!    ifcst       |none        |number of forecast region in database file
!!    ifnum       |none        |number of fertilizer type in database file
!!    iht         |none        |hydrograph storage location number (.fig)
!!    inm1        |none        |1st routing command variable (.fig)
!!    inm2        |none        |2nd routing command variable (.fig)
!!    inm3        |none        |3rd routing command variable (.fig)
!!                             |if icd=1, inm3=subbasin #
!!    ipnum       |none        |number of pesticide type in database file
!!    itnum       |none        |number of tillage implement in database file
!!    iunum       |none        |number of urban land type in database file
!!    j           |none        |counter
!!    nhtot       |none        |number of relative humidity records in file
!!    nrgage      |none        |number of raingage files
!!    nrgfil      |none        |number of rain gages per file
!!    nrtot       |none        |total number of rain gages
!!    nsave       |none        |number of save commands in .fig file
!!    nstot       |none        |number of solar radiation records in file
!!    ntgage      |none        |number of temperature gage files
!!    ntgfil      |none        |number of temperature gages per file
!!    nttot       |none        |total number of temperature gages
!!    numhru      |none        |number of HRUs listed in subbasin file
!!    nwtot       |none        |number of wind speed records in file
!!    pestdb      |NA          |name of pesticide database input file(pest.dat)
!!    subfile     |NA          |name of subbasin input file (.sub)
!!    tilldb      |NA          |name of tillage database input file(till.dat)
!!    title       |NA          |description lines in file.cio(1st 3 lines)
!!    titldum     |NA          |variable to read in data line
!!    urbandb     |NA          |name of urban land type database file 
!!                             |(urban.dat)
!!    septdb      !  NA        ! name of septic tank database file 
!!                             |(septwq1.dat)  !!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      implicit none

      character (len=13) :: urbandb, plantdb, tilldb, pestdb, figfile, &
      			 fertdb, subfile, fcstfile, bsnfile
      character (len=1) ::  a
      character (len=80) ::  titldum
      integer :: icd, inm1, inm2, inm3, iht, eof, numhru, ic, nlines, jj
      integer :: ipnum, ifnum, iunum, itnum, j, ifcst, fcsttot, k
!     septic database
      integer  :: isnum   !! CS

!!    initialize variables
      title = ""
      plantdb = ""
      tilldb = ""
      pestdb = ""
      fertdb = ""
      urbandb = ""
      figfile = ""
	  bsnfile = ""
!     septic database file
      septdb = ""
      nrgage = 0
      ntgage = 0
      nrtot = 0
      nttot = 0
      nrgfil = 0
      ntgfil = 0
      nstot = 0
      nhtot = 0
      nwtot = 0
      nstep = 0
      myr = 0

      open (file_cio_num,file="file.cio")
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,5100) title
      read (file_cio_num,6000) titldum
      read (file_cio_num,5000) figfile
      read (file_cio_num,*) myr
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,*) dthy !Jaehak 2017 flood routing   !!R666b 7/19/17 nbs
      !read (file_cio_num,*) nstep
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,*) nrgage
      read (file_cio_num,*) nrtot
      read (file_cio_num,*) nrgfil
      read (file_cio_num,6000) titldum
      read (file_cio_num,*) ntgage
      read (file_cio_num,*) nttot
      read (file_cio_num,*) ntgfil
      read (file_cio_num,6000) titldum
      read (file_cio_num,*) nstot
      read (file_cio_num,6000) titldum
      read (file_cio_num,*) nhtot
      read (file_cio_num,6000) titldum
      read (file_cio_num,*) nwtot
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,6000) titldum
      read (file_cio_num,5000) fcstfile
      read (file_cio_num,6000) titldum
      read (file_cio_num,5000) bsnfile
      read (file_cio_num,6000) titldum
      read (file_cio_num,5000) plantdb
      read (file_cio_num,5000) tilldb
      read (file_cio_num,5000) pestdb
      read (file_cio_num,5000) fertdb
      read (file_cio_num,5000) urbandb


!  septic database file
      do nlines = 1, 24
       read (file_cio_num,6000,iostat=eof) titldum
	 if (eof < 0) exit
      end do 

!      do
	read (file_cio_num,5000,iostat=eof) septdb   !! CS

!!    added for binary files 3/25/09 gsm 
!!    ia_b  print ascii or binary files
!!       0 for ascii file 
!!       1 for binary file!!    added for 
      read (file_cio_num, *, iostat=eof) ia_b

      close (file_cio_num)
!! calculate max number of years simulated, daily time increment
      myr = myr + 2
      dthy = dthy / 60. ! time interval, hr, Jaehak 2017 flood routing  !!R666b 7/19/17 nbs 
      if (dthy <= 0) then                                                !!R666b 7/19/17 nbs
        nstep = 1
      else
        nstep = 24 / dthy                                                !!R666b 7/19/17 nbs
      end if
      nstep = nstep + 1
      
      call caps(plantdb)
      call caps(fertdb)
      call caps(pestdb)
      call caps(figfile)
      call caps(tilldb)
      call caps(urbandb)
!     septic database
      call caps(septdb)  !! CS

!! open .bsn file to get ievent input
      open (bsnfile_num,file=bsnfile)
      call caps (bsnfile)
      do nlines = 1, 17
        read (bsnfile_num,6000,iostat=eof) titldum
        if (eof < 0) exit
      end do
        read (bsnfile_num,*,iostat=eof) ievent
      !if (ievent == 1) nstep = 24
      close (bsnfile_num)


!!    open routing file



!!    initialize variables
      a = ""
      icd = 1
      iht = 0
      inm1 = 0
      inm2 = 0
      inm3 = 0
      mhru = 0
      mch = 1
      mru = 1
      msub = 0
      mhyd = 1
      mres = 0 
      mlyr = 0
      mpst = 0
      mcr = 0
      mapp = 0
      mgr = 0
      mcut = 0
      mnr = 0
      mapex = 0
      mrecc = 0
      mrecd = 0
      mrech = 0
      mrecm = 0
      mrecy = 0
      mtran = 0
      nsave = 0
      !nlsu = 0

!! calculate number of records in plant growth database
      eof = 0
      mcrdb = 0
      open (plantdb_num,file=plantdb)
      do
        ic = 0
        read (plantdb_num,*,iostat=eof) ic
        if (eof < 0) exit
        read (plantdb_num,6000,iostat=eof) titldum
        if (eof < 0) exit
        read (plantdb_num,6000,iostat=eof) titldum
        if (eof < 0) exit
        read (plantdb_num,6000,iostat=eof) titldum
        if (eof < 0) exit
        read (plantdb_num,6000,iostat=eof) titldum
        if (eof < 0) exit
        mcrdb = Max(mcrdb,ic)
      end do
      close (plantdb_num)
      if (mcrdb <= 0) mcrdb = 1

!! calculate number of records in urban database
      eof = 0
      mudb = 0
      open (urbandb_num,file=urbandb)
      do
        iunum = 0
        read (urbandb_num,6200,iostat=eof) iunum
        if (eof < 0) exit
        read (urbandb_num,6000,iostat=eof) titldum
        if (eof < 0) exit
        mudb = Max(mudb,iunum)
      end do
      close (urbandb_num)
      if (mudb <= 0) mudb = 1

!!     calculate number of records in septic tank database !! CS

      eof = 0
      msdb = 0
!!    read title lines from septic database file
!     septic database
      if (septdb /= '             ') then
         open (septdb_num,file=septdb) !! CS
         do jj = 1,4
            read (septdb_num,6000) titldum
         end do
         do
          isnum = 0
          read (septdb_num,6200,iostat=eof) isnum  !!
          if (eof < 0) exit
          read (septdb_num,6000,iostat=eof) titldum  !!
          read (septdb_num,6000,iostat=eof) titldum  !!
          if (eof < 0) exit
          msdb = Max(msdb,isnum)
        end do
        if (msdb <= 0) msdb = 1
        close (septdb_num)       
      end if

!! calculate number of records in fertilizer database
      eof = 0
      mfdb = 0
      open (fertdb_num,file=fertdb)
      do
        ifnum = 0
        read (fertdb_num,6300,iostat=eof) ifnum
        if (eof < 0) exit
        mfdb = Max(mfdb,ifnum)
      end do
      if (mfdb <= 0) mfdb = 1
      close (fertdb_num)

!! calculate number of records in pesticide database
      eof = 0
      mpdb = 0
      open (pestdb_num,file=pestdb)
      do
        ipnum = 0
        read (pestdb_num,6200,iostat=eof) ipnum
        if (eof < 0) exit
        mpdb = Max(mpdb,ipnum)
      end do
      close (pestdb_num)
      if (mpdb <= 0) mpdb = 1

!! calculate number of records in tillage database
      eof = 0
      mtil = 0
      open (tilldb_num,file=tilldb)
      do
        itnum = 0
        read (tilldb_num,6300,iostat=eof) itnum
        if (eof < 0) exit
        mtil = Max(mtil,itnum)
      end do
      if (mtil <= 0) mtil = 1
      close (tilldb_num)
      

!! process .fig file
      allocate (pstflg(mpdb))
      pstflg = 0
      mhru1 = 1
      open (figfile_num,file=figfile)
      do while (icd > 0)
        read (figfile_num,5002) a
        if (a /= "*") then
          backspace figfile_num

          read (figfile_num,5001) a, icd, iht, inm1, inm2, inm3

          select case (icd)
          case (1)                      !! icd = 1  SUBBASIN command
            msub = msub + 1             !! # subbasins
            !! calculate total number of HRUs in watershed
            subfile = ""
            numhru = 0
            read (figfile_num,6100) subfile
            call caps(subfile)
            open (subfile_num,file=subfile)
            do j = 1,52 
              read (subfile_num,6000) titldum
            end do
            read (subfile_num,*) numhru
            mhru = mhru + numhru
            do j = 1, 8
              read (subfile_num,6000) titldum
            end do
            call hruallo
            mhru1 = mhru + 1
            close (subfile_num)
          case (2)                      !! icd = 2  ROUTE command
            mch = mch + 1               !! # channels
            read (figfile_num,5002) a   
          case (3)                      !! icd = 3  ROUTE RESERVOIR command
            mres = mres + 1
            read (figfile_num,5002) a
          case (4)
	    read (figfile_num,5002) a             !! icd = 4  TRANSFER command
            mtran = mtran + 1
          case (6)                      !! icd = 6  RECALL HOUR command
            read (figfile_num,5002) a
            mrech = mrech + 1
            mrech = MAX(mrech,inm1)
          case (7)                      !! icd = 7  RECALL MONTH command
            read (figfile_num,5002) a
            mrecm = mrecm + 1
            mrecm = MAX(mrecm,inm1) 
          case (8)                      !! icd = 8  RECALL YEAR command
            read (figfile_num,5002) a
            mrecy = mrecy + 1
            mrecy = MAX(mrecy,inm1) 
          case (9)                      !! icd = 9  SAVE command
            read (figfile_num,5002) a
            nsave = nsave + 1
          case (10)                     !! icd = 10 RECALL DAY command
            read (figfile_num,5002) a
            mrecd = mrecd + 1
            mrecd = MAX(mrecd,inm1)
          case (11)                     !! icd = 11 RECALL CONSTANT command
            read (figfile_num,5002) a
            mrecc = mrecc + 1
            mrecc = MAX(mrecc,inm1)
          case (13)                     !! icd = 13 APEX command
            read (figfile_num,5002) a
            mapex = mapex + 1
            mapex = MAX(mapex,inm1)
          case (14)                     !! icd = 14 SAVECONC command
            read (figfile_num,5002) a
            nsave = nsave + 1
          case (17)                     !! icd = 17 ROUTING UNIT command
            read (figfile_num,5002) a
            mru = mru + 1
          end select

          mhyd = Max(mhyd,iht)

        end if
      end do  
      close (figfile_num)
      
      if (ils_nofig == 1) then
        mru = Max(mru,2*msub)
      end if
      
!      mhyd = mhyd + mrecc + mrecd + mrech + mrecm + mrecy + nsave
!     &                                                       + mtran + 1
      if (mhru <= 0) mhru = 1
      if (msub <= 0) msub = 1
      if (mch <= 0) mch = 1
      if (mrecc <= 0) mrecc = 1
      if (mrecd <= 0) mrecd = 1
      if (mrech <= 0) mrech = 1
      if (mrecm <= 0) mrecm = 1
      if (mrecy <= 0) mrecy = 1
      if (mres <= 0) mres = 1

      mhyd = mhyd + nsave + mtran + 1
      
      if (ils_nofig == 1) then
        mhyd = mhyd + 6 * msub
      end if

!!    septic change 1-28-09 gsm
      mlyr = mlyr + 4 
      mlyr = 21                 !!Zhang changed
!!    septic change 1-28-09 gsm

      mcr = mcr + 1
      mcr = Max(2,mcr)
      mapp = mapp + 1
      mgr = mgr + 1
      mcut = mcut + 1
      mnr = mnr + 1
      mpst = Sum(pstflg) + 1

!! calculate max number of climate gages
      mrg = 0
      mrg = Max(nrtot,nttot,nstot,nhtot,nwtot)
      if (mrg <= 0) mrg = 1

!! calculate max number of forecast stations
      mfcst = 0
      call caps(fcstfile)
      if (fcstfile /= '             ') then
        fcsttot = 0
        open (fcstfile_num,file=fcstfile)
        read (fcstfile_num,5002,end=99) titldum

        read (fcstfile_num,6400) fcsttot
        do j = 1, fcsttot
          read (fcstfile_num,5002) titldum
          read (fcstfile_num,6400) ifcst
          do k = 1, 10
            read (fcstfile_num,5002) titldum
          end do
          mfcst = Max(mfcst, ifcst)
        end do
          mfcst = mfcst + 1
99       close (fcstfile_num)
      else
        mfcst = 1
      end if

      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
!$$$$$$  5200 format (10i4)
 6000 format (a80)
 6100 format (10x,a13)
 6200 format (i3)
 6300 format (i4)
 6400 format (i6)
      end