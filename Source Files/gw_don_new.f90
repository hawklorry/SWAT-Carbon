          Subroutine gw_don
! ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates DON transport by baseflow to streamflow

 use carbon_para
 use parm
 use parm_subC
 implicit none
 
 integer :: j
 real :: rchrg_don1,xx

 j = 0
 j = ihru

 rchrg_don1 = 0.
 rchrg_don1 = rchrg_don(j)
 if (rchrg_don1 < 1.e-6) rchrg_don1 = 0.0

 !! compute DOn aquifer loading from recharge for current day
 rchrg_don(j)= 0.
 rchrg_don(j) = (1.- gw_delaye(j))*PerQ_DON(sol_nly(j),j) + gw_delaye(j)*rchrg_don1     ! calculate the DOC recharge
 shallst_don(j) = shallst_don(j) + rchrg_don(j)                                       !add DOC recharge into shallow aquifer DOC pool

 if (shallst_don(j) < 1.e-6) shallst_don(j) = 0.0


 !! compute DOn groundwater contribution to streamflow for day
 xx = shallst(j) + gw_q(j) + revapday + gwseep 
 if (xx > 1.) then
     xx = shallst_don(j) / (shallst(j) + gw_q(j) + revapday + gwseep) 
 else
     xx = 0.
 end if
 if (xx < 1.e-6) xx = 0.0
 GwQ_DON(j) =0.
 GwQ_DON(j) = xx * gw_q(j)  !DOC amount transported by 

 revap_don(j) = xx * revapday
 gwseep_don(j) = xx * gwseep
 revap_don(j) = amax1(1.e-6,revap_don(j))
 gwseep_don(j) = amax1(1.e-6,gwseep_don(j))

 !! subtract DON transport losses from the shallow aquifer
 shallst_don(j) = shallst_don(j)-GwQ_DON(j)-revap_don(j)-gwseep_don(j)
 shallst_don(j) = amax1 (0., shallst_don(j))

 !! compute DON reaction losses in the groundwater
 shallst_don_decay(j) = shallst_don(j)- shallst_don(j)*Exp(-.693 /hlife_doc(j)) ! DON reaction, first-order decay
 shallst_don(j) = shallst_don(j)-shallst_don_decay(j) 
 shallst_don(j) = amax1(0., shallst_don(j))
 
  shallst_n(j) = shallst_n(j) +shallst_don_decay(j)    !! add decay DON to NO3 in groundwater
  shallst_n(j) = amax1(0., shallst_n(j))
 
 
 return
 end subroutine