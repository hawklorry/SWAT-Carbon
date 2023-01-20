      subroutine Initialize_File_No

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros values for single array variables

      use parm
      implicit none
      
    !!added by Zhang for file numbers
        
    
        !input.std = 24
        input_std_num = 24                                  !!done, not closed, open in readfile-Qi.f90, written in readsub.f90, std1.f90, std2.f90
        !open (23,file="file.cio")
        file_cio_num = 23                                   !!done, opened, read, and closed in getallo.f90
        !open (103,file=bsnfile)
        bsnfile_num = 103                                   !!done, opened in readfile.f90, read and closed in readbsn.f90, readbsn is called in main
        !open (29,file=plantdb)
        plantdb_num = 29                                    !!done, opened, read, and closed in getallo.f90

        !open (8,file=urbandb)
        urbandb_num =  8                                    !!done, opened, read, and closed in getallo.f90

        !open (171,file=septdb)
        septdb_num = 171                                    !!done, opened, read, and closed in getallo.f90, then opned, read, and closed in readseptwq.
        !      open (7,file=fertdb)
        fertdb_num = 7                                      !!done, opened, read, and closed in getallo.f90
        !      open (31,file=pestdb)
        pestdb_num = 31                                     !!done, opened, read, and closed in getallo.f90
        !      open (30,file=tilldb)    
        tilldb_num = 30                                     !!done, opened, read, and closed in getallo.f90
        !      open (27,file=figfile)
        figfile_num = 27                                   !!done, opened, read, and closed in getallo.f90, moved to main.f90
        !      open (25,file=subfile)
        subfile_num = 25                                    !!done, opened in getallo, read in getallo.f90 and hruallo.f90 (within getallo.f90), closed in getallo.f90
        !      open (12,file=fcstfile)
        fcstfile_num = 12                                   !!done, opened and read in getallo.f90


        !open (104,file=plantdb)
        plantdb2_num = 104                                  !!done, opened in readfile-Qi.f90, read and closed in readplant-Yang.f90
        !open (105,file=tilldb)
        tilldb2_num = 105                                   !!done, opened in readfile-Qi.f90, read and closed in readtill.f90
        !open (106,file=pestdb)
        pestdb2_num = 106                                   !!done, opened in readfile-Qi.f90, read and closed in readpest.f90
        !open (107,file=fertdb)
        fertdb2_num =107                                    !!done, opened in readfile-Qi.f90, read and closed in readfert.f90
        !open (108,file=urbandb)
        urbandb2_num = 108                                  !!done, opened in readfile-Qi.f90, read and closed in readurban.f90
        
        
        !open (109,file=fcstfile)   
        fcstfile2_num = 109                                 !!done, opened in readfile.f90, and read in readcnst.f90
        
        !      open (9,file=solfile,recl=350)
        solfile_num = 9                                !!done, opened, read, and closed in hruallo.f90, moved to main before getallo.f90 (hruallo.f90)
        !      open (10,file=mgtfile)                       !!hruallo.f90(107)
        mgtfile1_num = 10                              !!done, opend, read, and closed in hruallo.f90, moved to main before getallo.f90 (hruallo.f90)
        !      open (11,file=chmfile)                       !!hruallo.f90(130)
        chmfile1_num = 11                              !!done, opend, read, and closed in hruallo.f90, moved to main before getallo.f90 (hruallo.f90)
        
        !       open (1031,file="basins.cbn")
        !basins_cbn_num = 1031                                              !!moved to main.f90
        !        open (9999,file='fin.fin')
        fin_fin_num = 9999                                                  !!done, in main.f90
        !
        !        open (100+j,file=rfile(j),recl=1850)
        rfile_num = 100                                                     !!done, not closed, opened in openwth.f90, read in pmeas, rewind_init.f90
        !        open (118+j,file=tfile(j),recl=20000)
        tfile_num = 118                                                     !!done, not closed, opened in openwth.f90, read in tmeas.f90, rewind_init.f90
        !        open (137,file=slrfile,recl=15000)
        slrfile_num = 137                                                   !!done, not closed, opened in openwth.f90, read in smeas.f90, rewind_init.f90
        !        open (138,file=rhfile,recl=15000)
        rhfile_num = 138                                                    !!done, not closed, opened in openwth.f90, read in hmeas.f90, rewind_init.f90
        !        open (139,file=wndfile,recl=15000)
        wndfile_num = 139                                                   !!done, not closed, opened in openwth.f90, read in wmeas.f90, rewind_init.f90
        !        open (140,file=petfile)
        petfile_num = 140                                                   !!done, not closed, opened in openwth.f90, read in clicon-Qi.f90, rewind_init.f90(485)
        !        open (127,file=atmofile)
        atmofile_num = 127                                                  !!done, opened in readatmodep.f90, read in readatmodep.f90 for 0, 1,2, and for 2, also read in simulate.f90(252)
        !        open (101,file=wwqfile)
        wwqfile_num = 101                                                   !!done, opened in readbsn-Qi.f90, read and closed in readwwq-Qi.f90
        !        open (98,file="cswat_profile.txt") 
        cswat_profile_num = 98                                              !!done, opened in readbsn-Qi.f90, written in hryday-Qi.f90
        !        open (1001,file="cswat_daily.txt")!! modified by Qichun
        cswat_daily_num = 1001                                              !!done, not closed, opened in readbsn-Qi.f90, written in hruday_Qi.f90
        !        open (1002,file="cswat_daily1.txt")
        cswat_daily1_num = 1002                                             !!done, not closed, opened in readbsn-Qi.f90, written in hruday_Qi.f90
        !
        !        open (101,file=subfile)
        subfile2_num = 101                                                  !!done, opened in readfig.f90, read and closed in readsub.f90
        !        open (103,file=rtefile)
        rtefile_num = 103                                                   !!done, open and closed in readfig.f90, read in readrte.f90
        !        open (104,file=swqfile)
        swqfile_num = 104                                                   !!done, open and closed in readfig.f90, read in readswq.f90
        !        open (105,file=resfile)
        resfile_num = 105                                                   !!done, open and closed in readfig.f90, read in readres.f90

        !        open (106,file=lwqfile)
        lwqfile_num = 106                                                   !!done, open and closed in readfig.f90, read in readlwq.f90
        !        open (107,file=month_in,recl=350)
        month_in_num = 107                                                  !!done, opened in readfig.f90, read in readmon.f90, closed in readfig.f90 and readmon.f90
        !        open (108,file=year_in,recl=350)
        year_in_num = 108                                                   !!done, \readfig.f90(280):            open (108,file=year_in,recl=350)
                                                                            !!read in readyr.f90, closed in both ready.f90 and readfig.f90(283)
        !        open (109,file=annual_in,recl=350)
        annual_in_num = 109                                                 !!done, not used
        !        open (113,file=rufile)
        rufile_num = 113                                                    !!done, open and closed in readfig.f90, read in readru.f90
        !
        !        open (101,file="file.cio")
        file_cio2_num = 101                                                 !!done, open, read and closed in readfile-Qi.f90
        !!open (23,file="file.cio")
                    !!file_cio_num = 23
        !        open (102,file=figfile)
        !integer :: figfile_num                 !      open (27,file=figfile)
                                                    !!figfile_num = 27
        figfile2_num = 102                                                  !!done, opened in readfile.f90, read and closed in readfig.f90
        !
        !        open (200+inum1s(idum),file=hour_in,recl=350)
        hour_in_num = 200                                                   !!done, not closed, opened in readfig.f90, read in readfig.f90, rechour-Qi.f90, rewind_ini.f90
        !        open (40+inum1s(idum),file=day_in,recl=350)
        day_save_num = 40                                                   !!done, not closed, opened in readfig.f90, writen in readfig.f90, save-Qi.f90, could be hourly data
        !        open (555+inum1s(idum),file=day_in,recl=350)
        day_in_num = 555                                                    !!done, not closed, open in readfig.f90, read in readfig.f90, recday-Qi.f90, rwin_ini.f90
                                                                                !!called in command-Qi.f90(179):            call recday
        !        open (112+inum1s(idum),file=apex_in,recl=350)
        apex_in_num = 400                       !!R678 12/12/19 nbs         !!done, not closed, not used
        !        open (50+inum1s(idum),file=day_in,recl=350)
        !integer :: day_in_num
        !

        !        open (121,file='output.snu')
        output_snu_num = 121                                                !!done, not closed, opened in readfile-Qi.f90, written in soil_write.f90(60)
        !        open (122,file='lup.dat')
        lup_dat_num = 122                                                   !!done, not closed, supposed to be closed as the code reads records from this file
        !        open (115,file='output.snw')
        output_snw_num = 115                                                !!done, not closed, opened in readfile-Qi.f90, written in surface-Qi.f90
        !        open (116,file='ebandtemp.out')
        ebandtemp_out_num = 116                                             !!done, not closed, opened in readfile-Qi.f90, written in surface-Qi.f90
        !        open (24,file="input.std")
        !integer :: input_std_num               !input.std = 24
                                            !!input_std_num = 24
        !        open (26,file="output.std")
        output_std_num = 26                                                 !!done, not closed, opened in readfile-Qi.f90, written in std1.f90, std2.f90, and std3.f90
        !        open (28,file="output.hru",recl=2000)
        output_hru_num = 28                                                 !!done, not closed, opened in readfile.f90, written in headout-Qi.f90, 
                                                                                !!hruaa-Qi.f90, hruaa_N-Qi.f90, hruday-Qi.f90, hrumon-Qi.f90, hrumon-N.f90, hruyr-Qi.f90, hruyr_N-Qi.f90, 
                                                                                !!stdaa-Qi.f90, writea-Qi.f90, writed-Qi.f90, writem-Qi.f90
        !        open (33333,file="outputb.hru",form='unformatted')
        outputb_hru_num = 33333                                             !!done, not closed, opened in readfile-Qi.f90, written in bmp_det_pond-Qi.f90
        !        open (30,file="output.pst",recl=600)
        output_pst_num = 30                                                 !!done, not closed, opend in readfile-Qi.f90, written in headout-Qi.f90, writeaa-Qi.f90, writed-Qi.f90, writem-Qi.f90
        !        open (31,file="output.sub",recl=1500)
        output_sub_num = 31                                                 !!done, not closed, opened in readfile-Qi.f90, written in headout-Qi.f90, subaa-Qi.f90, subday-Qi.f90, subday1-Qi.f90, submon-Qi.f90, subyr-Qi.f90
        !        open (66666,file = "outputb.sub", form = 'unformatted')
        outputb_sub_num = 66666                                             !!done, not closed, opened in readfile-Qi.f90, written in subday-Qi.f90, subday1-Qi.f90
        !        open (7,file="output.rch",recl=1500)
        output_rch_num = 7                                                  !!done, not close, opened readfile-Qi.f90(618), written in headout-Qi.f90, rchaa-Qi.f90, rchday-Qi.f90, rchmon-Qi.f90, rchyr-Qi.f90
        !        open (8,file="output.rsv",recl=800)
        output_rsv_num = 8                                                  !!done, not closed, opened in readfile-Qi.f90(619), written in headout-Qi.f90, routres-Qi.f90, writea-Qi.f90, writem-Qi.f90
                                                
        !        open (77777,file = "outputb.rch", form = 'unformatted')
        outputb_rch_num = 77777                                             !!done, not closed, opened in readfile-Qi.f90, written in rchday-Qi.f90
        !        open (84,file="output.sed",recl=800)
        output_sed_num = 84                                                 !!done, not closded, opened in readfile-Qi.f90, written in rsedaa.f90, rsedday.f90, rsedmon.f90, rsedyr.f90
        !        open (77778,file = "bmp-sedfil.out") !jaehak temp urban print out
        bmp_sedfil_out_num = 77778                                          !!done, not closed, opened in readfile-Qi.f90, written in bmpinit.f90, headout-Qi.f90, readfile-Qi.f90
        !        open (77779,file = "bmp-ri.out") !jaehak temp urban print out
        bmp_ri_out_num = 77779                                              !!done, not closed, opened in readfile-Qi.f90, written in bmpinit.f90, headout-Qi.f90, distrib_bmps.f90, readfile-Qi.f90
        !        open (82,file='output.wql')
        output_wql_num = 82                                                 !!done, not closed, opened in readfile-Qi.f90, written in biofilm.f90, watqual-Du-Qi.f90
        !        open (83,file='hourq.out')
        hourq_out_num = 83                                                  !!done, not closed, opened in readfile-Qi.f90, written in print_hyd.f90
        !
        !        open (11,file='rch.dat')
        rch_dat_num = 11                                                    !!done, opened, but not writen into
        !        open (12,file='hru.dat')
        hru_dat_num = 12                                                    !!done, opened, but not writen into
        !        open (13,file='sub.dat')
        sub_dat_num = 13                                                    !!done, opened, but not writen into
        !        open (14,file='rsv.dat')
        rsv_dat_num = 14                                                !!done, opened, but not writen into
        !        open (11123,file='hyd.out')
        hyd_out_num = 11123                                             !!done, opened, but not writen into
        !        open (16,file='chan.deg')
        chan_deg_num = 16                                               !!done, opened, but not writen into
        !
        !        open (17,file='wbl.out')
        wbl_out_num = 17                                                !!done, opened, but not writen into
        !        open (18,file='swat.qst')
        swat_qst_num = 18                                               !!done, opened, but not writen into
        !
        !
        !        open (129,file='output.swr')
        output_swr_num = 129                                            !!done, not closed, opened in readfile-Qi.f90, written in writed-Qi.f90
        !        open (141,file='output.vel')
        output_vel_num = 141                                            !!done, not closed, opened in readfile-Qi.f90, written in writed-Qi.f90
        !        open (142,file='output.dep')
        output_dep_num = 142                                            !!done, not closed, opened in readfile-Qi.f90, written in writed-Qi.f90 
        !        open (143, file="output.mgt", recl=60
        output_mgt_num = 143                                            !!done, not closed, opened in readfile-Qi.f90, written in anfert-Qi.f90, autoirr.f90
                                                                                      !!conapply.f90, confert-Qi.f90, dormant-Yang.f90(466), dormant-Yang.f90(493),
                                                                                      !!irr_rch-Qi.f90, irr_res.f90, sched_mgt.f90
        !        open (29,file="output.wtr",recl=800)
        output_wtr_num = 29                                             !!done, not closed, opened in readfile-Qi.f90, written in headout-Qi.f90, impndaa.f90
                                                                                        !!impndday.f90, impndmon.f9, impndyr.f90
        !        open (125,file='output.pot')
        output_pot_num = 125                                            !!done, not closed, opened in readfile-Qi.f90, written in pothole.f90, potholehr.f90
        !        open (19,file="output2.std")
        output2_std_num = 19                                            !!done, not closed, opened in readfile-Qi.f90, written in pestw.f90, std1.f90, std3.f90, stdaa-Qi.f90
                                                                                !!writea-Qi.f90, writed-Qi.f90, writem-Qi.f90
        
        !        open (20,file="output2.rch",recl=600)      !readfile-Qi.f90(749):        open (20,file="output2.rch",recl=600)
        !output2_rch_num = 20                            !!need to check, it (fileno = 20) is currently not opened, as isproj = 0. in readfile-Qi.f90
                                                         !!also, in writed-Qi.f90, it is not being written.
                                                        
        !!readfile-Qi.f90(749)                                                
        !if (isproj == 1) then 
        !    open (output2_std_num,file="output2.std")
        !    open (20,file="output2.rch",recl=600)
        !    open (output2_hru_num,file="output2.hru",recl=800)
        !    open (output2_rsv_num,file="output2.rsv",recl=800)
        !end if
        
        !!writed-Qi.f90(126)
        !if (iprint == 1.or.iprint==3) then
        !    if (da_ha < 1.e-9) then
	    !    call rchday
	    !    call rseday
	    !    return
	    !end if        
        
        !        open (21,file="output2.hru",recl=800)
        output2_hru_num = 21                                            !!done, not closed, opened in readfile-Qi.f90, written in headout-Qi.f90, no further written in, but commented in hruaa, hruday, hrumon, hrumon_N, hruyr-Qi.f90, hruyr_N-Qi.f90
        !        open (22,file="output2.rsv",recl=800)
        output2_rsv_num = 22                                            !!done, not closed, opened in readfile-Qi.f90, written in headout-Qi.f90, routres-Qi.f90, writea-Qi.f90, writem-Qi.f90
        !        open (173,file='septic.out')
        septic_out_num = 173                                            !!done, not closed, opened in readfile-Qi.f90, written in biozone.f90, writeaa-Qi.f90
        !        open (2222,file='charles.out',recl=800)
        charles_out_num = 2222                                          !!done, not used
        !
        !        open (1032,file="basins.rwq")   !! Added b
        basins_rwq_num = 1032                                           !!done, opened in readfile_new-Qi.f90, no write, not closed
        !        open (281,file="output_C.hru",recl=2000)
        output_C_hru_num = 281                                          !!done, not closed, opened in readfile_new-Qi.f90, written in headout_S-Qi.f90, hruday_C-Qi.f90, 
        !        open (282,file="output_N.hru",recl=2000)
        output_N_hru_num = 282                                          !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, hruaa_N-Qi.f90, hruday_N-Qi.f90, hrumon_N.f90, hrryr_N-Q.f90, 
        !        open (283,file="output_E.hru",recl=2000)
        output_E_hru_num = 283                                          !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, hruday_E-Qi.f90
        !        open (284,file="output_P.hru",recl=2000)
        output_P_hru_num = 284                                          !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, hryday_P.f90
        !        open (261,file="output_C.std")
        output_C_std_num = 261                                          !!done, not closed, openned in readfile_new-Qi.f90, no written 
        !        open (262,file="output_N.std")
        output_N_std_num = 262                                          !!done, not closes, openned in readfile_new-Qi.f90, written in stdaa_N-Qi.f90
        !        open (271,file="output_C.lnd")
        output_C_lnd_num = 271                                          !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, writea_S-Qi.f90 writeaa_S-Qi.f90, writed_S-Qi.f90, writem_S-Qi.f90
        !        open (272,file="output_N.lnd")
        output_N_lnd_num = 272                                          !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, writea_S-Qi.f90 writeaa_S-Qi.f90, writed_S-Qi.f90, writem_S-Qi.f90
        !        open (2721,file="output_Nagr.lnd")
        output_Nagr_lnd_num = 2721                                      !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, writea_S-Qi.f90 writeaa_S-Qi.f90, writed_S-Qi.f90, writem_S-Qi.f90
        !        open (2722,file="output_Nfor.lnd")
        output_Nfor_lnd_num = 2722                                      !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, writea_S-Qi.f90 writeaa_S-Qi.f90, writed_S-Qi.f90, writem_S-Qi.f90
        !        open (2723,file="output_Ngra.lnd")
        output_Ngra_lnd_num = 2723                                      !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, writea_S-Qi.f90 writeaa_S-Qi.f90, writed_S-Qi.f90, writem_S-Qi.f90

        !        open (2724,file="output_Nwet.lnd")
        output_Nwet_lnd_num = 2724                                      !!done, not closed, openned in readfile_new-Qi.f90, written in headout_S-Qi.f90, writea_S-Qi.f90 writeaa_S-Qi.f90, writed_S-Qi.f90, writem_S-Qi.f90
        !        open (1986,file="co2.dat")   !!--qichun---
        co2_dat_num = 1986                                              !!done, opened, in readfile_new-Qi.f90, read in readco2-Yang.f90
        !        open (1996,file="output2.rch")   !!  open (20,file="output2.rch",recl=600)
                                                  !!output2_rch_num
        output2_rch_num = 1996                                          !!done, not clused, cureently used instead of open (20,...).
                                                                        !!writen into in headout_R-Qi.f90, rchaa2.f90, rchday-Qi.f90, rchmon2-Qi.f90, rchyr2-Qi.f90
                                                                        !!opened in readfile_new-Qi.f90
                                                        
        !
        !
        !        open (104,file=dpd_file)
        dpd_file_num = 104                                              !!done, open, read, and closed in readpnd-Qi.f90
        !        open (104,file=wpd_file)
        wpd_file_num = 104                                              !!done, open, read, and closed in readpnd-Qi.f90
        !        open (104,file=rib_file)
        rib_file_num = 104                                              !!done, open, read, and closed in readpnd-Qi.f90
        !        open (104,file=sfb_file)
        sfb_file_num = 104                                              !!done, open, read, and closed in readpnd-Qi.f90
        !        open (104,file=lid_file)
        lid_file_num = 104                                              !!done, open, read, and closed in readpnd-Qi.f90
        !        open daily reservoir outflow file
        !        open (350+i,file=resdayo)
        resdayo_num = 350                                               !!done, not closed, opened in readres.f90, read in res.f90
        !        open (101,file=resmono)
        resmono_num = 101                                               !!done, opened, read, and closed in readres.f90
        !        open (171,file=septdb)     !!open (171,file=septdb)
                                            !!septdb_num = 171
        !integer :: septdb_num
        !        open (114,file=wgnfile)
        wgnfile_num = 114                                               !!done, opened in readsub, read and closed in readwgn.f90
        !        open (104,file=pndfile)
        pndfile_num = 104                                               !!done, opened in readsub, read and closed in readpnd.f90
        !        open (105,file=wusfile)
        wusfile_num = 105                                               !!done, opened in readsub, read and closed in readwus.f90
        !        open (113,file=snofile)
        snofile_num = 113                                               !!done, opened in readsub, read and closed in readsno.f90
        !        open (172,file=septfile, status='old')
        septfile_num = 172                                              !!done, opened in readsub, read and closed in readsepticbz.f90
        !        open (112,file=sdrfile)
        sdrfile_num = 112                                               !!done, opened in readsub, read and closed in readsdr.f90
        !        open (106,file=chmfile)                !!open (11,file=chmfile)
                                                        !! chmfile_num = 11        !integer :: chmfile_num
        chmfile_num = 106                                               !!done, opened in readsub.f90, read and closed in readchm.f90
        
        
        !        open (107,file=solfile)
        solfile_num = 107                                               !!done, opened, read, and closed in hruallo.f90
        !        open (108,file=hrufile)
        hrufile_num = 108                                               !!done, opened in readsub.f90, read and closed in readhru.f90
        !        open (109,file=mgtfile)
        !integer :: mgtfile_num                 !!open (10,file=mgtfile)
                                                !!mgtfile_num = 10
        mgtfile_num = 109                                               !!done, opened in readsub.f90, read and closed in readmgt-Qi.f90
        !        open (110,file=gwfile)
        gwfile_num = 110                                                !!done, opened in readsub.f90, read and closed in readgw.f90
        !        open (111,file=opsfile)
        opsfile_num = 111                                               !!done, opened in readsub.f90, read and closed in readops.f90
        !        open (9123,file=fname(no_lup))
        no_lup_num = 9123                                               !!done, not closed, opened and read in resetlu.f90

        sw_data_out_num = 1234
        sw_data_in_num = 1235
        
        readCEQUAL_num = 10001
        readBottomAlgae_num = 10002
        readCarbonSoil_num = 10003
        readCarbonSedFlux_num = 10004
        readEnergyPara_num = 10005
      return
      end