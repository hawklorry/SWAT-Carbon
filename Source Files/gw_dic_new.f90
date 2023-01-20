Subroutine gw_dic
! ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates DIC transport by baseflow to streamflow

 use carbon_para
 use parm
 use parm_subC
 implicit none

 integer :: j
 real :: rchrg_dic1,xx

 j = 0
 j = ihru
  revap_dic = 0.
 gwseep_dic = 0.

 rchrg_dic1 = 0.
 rchrg_dic1 = rchrg_dic(j)
 if (rchrg_dic1 < 1.e-6) rchrg_dic1 = 0.0

 !! compute DiC aquifer loading from recharge for current day
 rchrg_dic(j)= 0.
 rchrg_dic(j) = (1.- gw_delaye(j))* PerQB_DIC(j)  + gw_delaye(j)*rchrg_dic1                 ! calculate the DIC recharge
 shallst_dic(j) = shallst_dic(j) + rchrg_dic(j)                                                                       !add DIC recharge into shallow aquifer DIC pool

 if (shallst_dic(j) < 1.e-6) shallst_dic(j) = 0.0

 !! compute DIC groundwater contribution to streamflow for day
 xx = shallst(j) + gw_q(j) + revapday + gwseep 
 if (xx > 1.) then
     xx = shallst_dic(j) / (shallst(j) + gw_q(j) + revapday + gwseep) 
 else
     xx = 0.
 end if
 if (xx < 1.e-6) xx = 0.0

 GwQ_DIC(j) = xx * gw_q(j)  !DIC amount transported by GW 
 revap_dic(j) = xx * revapday
 gwseep_dic (j)= xx * gwseep
 revap_dic (j)= amax1(1.e-6,revap_dic(j))
 gwseep_dic(j) = amax1(1.e-6,gwseep_dic(j))

 !! subtract DIC transport losses from the shallow aquifer
 shallst_dic(j) = shallst_dic(j)-GwQ_DIC(j)-revap_dic(j)-gwseep_dic(j)
 shallst_dic(j) = amax1 (0., shallst_dic(j))

 !! compute DOC -> DIC reaction in the groundwater

shallst_dic(j) = shallst_dic(j)+shallst_doc_decay(j) 
shallst_dic(j)  = amax1(0., shallst_dic(j) )
 
 return
 end 