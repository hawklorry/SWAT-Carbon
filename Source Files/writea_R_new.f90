      subroutine writea_R
      use parm
         use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      integer :: j, k
      
       if (i_mo <= mo_chk .or. (curyr == nbyr .and. i == idal)) then
        if (iprint /= 1 .and.  iprint/= 3) then 
            if(cswat == 2  .and.  rch2==1) call rchyr2                    
        end if
           !!===============
      if (curyr > nyskip) then  
             !  rchaao2 = rchaao2 + rchyro5              
       do j = 1, subtot !|number of subbasins in watershed
      rchaao2(1,j) = rchaao2(1,j) + rchyro2(1,j)
      rchaao2(2,j) = rchaao2(2,j) + rchyro2(2,j)
      rchaao2(3,j) = rchaao2(3,j) + rchyro2(3,j)
      rchaao2(4,j) = rchaao2(4,j) + rchyro2(4,j)
      rchaao2(5,j) = rchaao2(5,j) + rchyro2(5,j)
      rchaao2(6,j) = rchaao2(6,j) + rchyro2(6,j)
      rchaao2(7,j) = rchaao2(7,j) + rchyro2(7,j)
      rchaao2(8,j) = rchaao2(8,j) + rchyro2(8,j)
      rchaao2(9,j) = rchaao2(9,j) + rchyro2(9,j)
      rchaao2(10,j) = rchaao2(10,j) + rchyro2(10,j)
      rchaao2(11,j) = rchaao2(11,j) + rchyro2(11,j)
      rchaao2(12,j) = rchaao2(12,j) + rchyro2(12,j)
      rchaao2(13,j) = rchaao2(13,j) + rchyro2(13,j)
      rchaao2(14,j) = rchaao2(14,j) + rchyro2(14,j)
      rchaao2(15,j) = rchaao2(15,j) + rchyro2(15,j)
      rchaao2(16,j) = rchaao2(16,j) + rchyro2(16,j)
      rchaao2(17,j) = rchaao2(17,j) + rchyro2(17,j)
      rchaao2(18,j) = rchaao2(18,j) + rchyro2(18,j)
      rchaao2(19,j) = 0. !rchaao2(19,j) + rchyro2(19,j)
      rchaao2(20,j) = 0.  !rchaao2(20,j) + rchyro2(20,j)
      rchaao2(21,j) = 0. !rchaao2(21,j) + rchyro2(21,j)
      rchaao2(22,j) = 0. !rchaao2(22,j) + rchyro2(22,j)
      rchaao2(23,j) = 0.  !rchaao2(23,j) + rchyro2(23,j)
      rchaao2(24,j) = 0. !rchaao2(24,j) + rchyro2(24,j)
      rchaao2(25,j) = 0.  !rchaao2(25,j) + rchyro2(25,j)
      rchaao2(26,j) =  rchyro2(26,j)                                                       !!RPOC kg  last day of the last year
      rchaao2(27,j) =  rchyro2(27,j)                                                        !!LPOC kg  last day of the last year
      rchaao2(28,j) =  rchyro2(28,j)                                                        !!RDOC kg  last day of the last year
      rchaao2(29,j) =  rchyro2(29,j)                                                         !!LDOC kg  last day of the last  year
      rchaao2(30,j) =  rchyro2(30,j)                                                         !!DIC kg  last day of the last year
      rchaao2(31,j) =  rchyro2(31,j)                                                          !!Floating Algea kg  last day of the last year
      rchaao2(32,j) =  rchyro2(32,j)                                                         !!Bottom Algea kg  last day of the last year
      rchaao2(33,j) = rchaao2(33,j) + rchyro2(33,j)
      rchaao2(34,j) = rchaao2(34,j) + rchyro2(34,j)
      rchaao2(35,j) = rchaao2(35,j) + rchyro2(35,j)
      rchaao2(36,j) = rchaao2(36,j) + rchyro2(36,j)
      rchaao2(37,j) = rchaao2(37,j) + rchyro2(37,j)
      rchaao2(38,j) = rchaao2(38,j) + rchyro2(38,j)
      rchaao2(39,j) = rchaao2(39,j) + rchyro2(39,j)
      rchaao2(40,j) = rchaao2(40,j) + rchyro2(40,j)
      rchaao2(41,j) = rchaao2(41,j) + rchyro2(41,j)
      rchaao2(42,j) = rchaao2(42,j) + rchyro2(42,j)
      rchaao2(43,j) = rchaao2(43,j) + rchyro2(43,j)
      rchaao2(44,j) = rchaao2(44,j) + rchyro2(44,j)
      rchaao2(45,j) = rchaao2(45,j) + rchyro2(45,j)
      rchaao2(46,j) = rchaao2(46,j) + rchyro2(46,j)
      rchaao2(47,j) = rchaao2(47,j) + rchyro2(47,j)
      rchaao2(48,j) = rchaao2(48,j) + rchyro2(48,j)
      rchaao2(49,j) = rchaao2(49,j) + rchyro2(49,j)
      rchaao2(50,j) = rchaao2(50,j) + rchyro2(50,j)
      rchaao2(51,j) = rchaao2(51,j) + rchyro2(51,j)
      rchaao2(52,j) = rchaao2(52,j) + rchyro2(52,j)
      rchaao2(53,j) = rchaao2(53,j) + rchyro2(53,j)
      rchaao2(54,j) = rchaao2(54,j) + rchyro2(54,j)
      rchaao2(55,j) = rchaao2(55,j) + rchyro2(55,j)
      rchaao2(56,j) = rchaao2(56,j) + rchyro2(56,j)
      rchaao2(57,j) = rchaao2(57,j) + rchyro2(57,j)
      rchaao2(58,j) = rchaao2(58,j) + rchyro2(58,j)
      rchaao2(59,j) = rchaao2(59,j) + rchyro2(59,j)
      rchaao2(60,j) = rchaao2(60,j) + rchyro2(60,j)
      rchaao2(61,j) = rchaao2(61,j) + rchyro2(61,j)
      rchaao2(62,j) = rchaao2(62,j) + rchyro2(62,j)
      rchaao2(63,j) = rchaao2(63,j) + rchyro2(63,j)
      rchaao2(64,j) = rchaao2(64,j) + rchyro2(64,j)
      rchaao2(65,j) = rchaao2(65,j) + rchyro2(65,j)
      rchaao2(66,j) = rchaao2(66,j) + rchyro2(66,j)
      rchaao2(67,j) = rchaao2(67,j) + rchyro2(67,j)
      rchaao2(68,j) = rchaao2(68,j) + rchyro2(68,j)
      rchaao2(69,j) = rchaao2(69,j) + rchyro2(69,j)
      rchaao2(70,j) = rchaao2(70,j) + rchyro2(70,j)
      rchaao2(71,j) = rchaao2(71,j) + rchyro2(71,j)
      rchaao2(72,j) = 0. !rchaao2(72,j) + rchyro2(72,j)
      rchaao2(73,j) =  rchyro2(73,j)                                                  !!Sediment C kg  last day of the last year
      rchaao2(74,j) = rchaao2(74,j) + rchyro2(74,j)
      rchaao2(75,j) = rchaao2(75,j) + rchyro2(75,j)      
      rchaao2(76,j) =  rchyro2(76,j)                                                  !!Sediment C kg  last day of the last year
      rchaao2(77,j) =  rchyro2(77,j)                                                  !!Bury C kg  last day of the last year
      rchaao2(78,j) = rchaao2(78,j) + rchyro2(78,j)
      rchaao2(79,j) = rchaao2(79,j) + rchyro2(79,j)
      rchaao2(80,j) = rchaao2(80,j) + rchyro2(80,j)
      rchaao2(81,j) = rchaao2(81,j) + rchyro2(81,j)
      rchaao2(82,j) = rchaao2(82,j) + rchyro2(82,j)
      rchaao2(83,j) = rchaao2(83,j) + rchyro2(83,j)
      rchaao2(84,j) = rchaao2(84,j) + rchyro2(84,j)
      rchaao2(85,j) = rchaao2(85,j) + rchyro2(85,j)
      rchaao2(86,j) = rchaao2(86,j) + rchyro2(86,j)
      rchaao2(87,j) = rchaao2(87,j) + rchyro2(87,j)
      rchaao2(88,j) = rchaao2(88,j) + rchyro2(88,j)
      rchaao2(89,j) = rchaao2(89,j) + rchyro2(89,j)
      rchaao2(90,j) = rchaao2(90,j) + rchyro2(90,j) 
      rchaao2(91,j) = rchaao2(91,j) + rchyro2(91,j)
      rchaao2(92,j) = rchaao2(92,j) + rchyro2(92,j)
      rchaao2(93,j) = rchaao2(93,j) + rchyro2(93,j)
      rchaao2(94,j) = rchaao2(94,j) + rchyro2(94,j)
      rchaao2(95,j) = rchaao2(95,j) + rchyro2(95,j)
      rchaao2(96,j) = rchaao2(96,j) + rchyro2(96,j) 
     
      end do
      
       !!==================================
       
          
       end if 
               
       rchyro2 = 0.                                                                   
                                                                 
     end if
                 
    return
    end