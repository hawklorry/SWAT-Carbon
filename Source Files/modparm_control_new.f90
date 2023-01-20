      module parm_control
      

      
      
      !!|\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
       !!==============Control Parameters=================================={
      integer:: ccswat
      integer:: soil_layer_std !0 orignal soil layer; 1 standardizaed soil layers in readsol.f90
      integer:: ievent_rch
      integer:: nstep_rch
      integer:: idt_rch
      integer:: isnowm
      integer:: irch_SedFlux
      integer:: ires_SedFlux
      integer:: irch_CNP
      integer:: ires_CNP
     
     
     
      integer :: tswat
      integer :: nswat
      integer :: ico2
      integer :: ifor
      integer :: iwea
      integer :: idh
      integer :: denit_method
      integer :: denit_n2o_method
      integer :: denit_no_method
      integer :: nit_method
      integer :: nit_n2o_method
      integer :: nit_no_method      

       integer,dimension(:),allocatable:: rchini_index
       integer,dimension(:),allocatable:: rchini_index_sed
       integer:: irch_SedResusp
       !real:: musle_pow
  

       integer:: jrch_out 
       integer:: iwtmp_rch
       integer:: iwtmp_sub
       integer:: wtmp_option   
       integer:: isurface_tmp
       integer,dimension(:),allocatable:: irch_wet
       integer::  irchwet
              !! res index
       integer, dimension (:), allocatable :: resini_index  
       integer, dimension (:), allocatable :: resini_index_sed 
       integer, dimension (:), allocatable :: ires_wet 
       integer:: ires_SedResusp
       
       
       integer:: nstep1
	   integer:: idt1
       integer:: rch_std
       integer:: isubdaily_output
       integer:: iproject_lst
       
       
      end module parm_control
         