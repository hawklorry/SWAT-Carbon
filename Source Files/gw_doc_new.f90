Subroutine gw_doc
! ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates DOC transport by baseflow to streamflow
 use carbon_para
 use parm
 use parm_subC
 implicit none
 
 integer :: j
 real :: rchrg_doc1,xx

 j = 0
 j = ihru

 rchrg_doc1 = 0.
 rchrg_doc1 = rchrg_doc(j)
 if (rchrg_doc1 < 1.e-6) rchrg_doc1 = 0.0
 !! compute DOC aquifer loading from recharge for current day
  rchrg_doc(j) = 0.
  rchrg_doc(j) = (1.- gw_delaye(j))*PerQB_DOC(j) + gw_delaye(j)*rchrg_doc1     ! calculate the DOC recharge
 shallst_doc(j) = shallst_doc(j)  +  rchrg_doc(j)                                      !add DOC recharge into shallow aquifer DOC pool
 if (shallst_doc(j) < 1.e-6) shallst_doc(j)  = 0.0
 !! compute DOC groundwater contribution to streamflow for day
 xx = shallst(j) + gw_q(j) + revapday + gwseep 
 if (xx > 1.) then
     xx = shallst_doc(j)  / (shallst(j) + gw_q(j) + revapday + gwseep) 
 else
     xx = 0.
 end if
 if (xx < 1.e-6) xx = 0.0

 GwQ_DOC(j) = xx * gw_q(j)  !DOC amount transported by 
 revap_doc(j) = xx * revapday
 gwseep_doc(j) = xx * gwseep
 revap_doc(j) = amax1(1.e-6,revap_doc(j))
 gwseep_doc(j) = amax1(1.e-6,gwseep_doc(j))
 !! subtract DOC transport losses from the shallow aquifer
 shallst_doc(j)  = shallst_doc(j) -GwQ_DOC(j)-revap_doc(j)-gwseep_doc(j)
 shallst_doc(j)  = amax1 (0., shallst_doc(j) )
 !! compute DOC reaction losses in the groundwater
 shallst_doc_decay(j) = shallst_doc(j) - shallst_doc(j) *Exp(-.693 / hlife_doc(j))                               ! DOC reaction, first-order decay
 shallst_doc(j) = shallst_doc(j) -shallst_doc_decay(j) 
 shallst_doc(j)  = amax1(0.,shallst_doc(j) )
 
 return
 end 
