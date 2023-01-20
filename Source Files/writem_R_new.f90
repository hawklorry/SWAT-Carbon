     subroutine writem_R

     use parm
     use parm_output
     use parm_control
      implicit none
     integer ::  j, k
      
     if (i_mo /= mo_chk .or. (curyr == nbyr .and. i == idal)) then
      
     if (iprint /= 2 .and. curyr > nyskip) then
     if (iprint == 0) then
     
      if(cswat == 2  .and. rch2==1) then
       if (idlast > 0) call rchmon2(idlast)                       
      end if
   
     end if
    end if
    !!===================================
       if (curyr > nyskip) then
      ! rchyro2 = rchyro2 + rchmono2                         
      do j = 1, subtot
      rchyro2(1,j) = rchyro2(1,j) + rchmono2(1,j)
      rchyro2(2,j) = rchyro2(2,j) + rchmono2(2,j)
      rchyro2(3,j) = rchyro2(3,j) + rchmono2(3,j)
      rchyro2(4,j) = rchyro2(4,j) + rchmono2(4,j)
      rchyro2(5,j) = rchyro2(5,j) + rchmono2(5,j)
      rchyro2(6,j) = rchyro2(6,j) + rchmono2(6,j)
      rchyro2(7,j) = rchyro2(7,j) + rchmono2(7,j)
      rchyro2(8,j) = rchyro2(8,j) + rchmono2(8,j)
      rchyro2(9,j) = rchyro2(9,j) + rchmono2(9,j)
      rchyro2(10,j) = rchyro2(10,j) + rchmono2(10,j)
      rchyro2(11,j) = rchyro2(11,j) + rchmono2(11,j)
      rchyro2(12,j) = rchyro2(12,j) + rchmono2(12,j)
      rchyro2(13,j) = rchyro2(13,j) + rchmono2(13,j)
      rchyro2(14,j) = rchyro2(14,j) + rchmono2(14,j)
      rchyro2(15,j) = rchyro2(15,j) + rchmono2(15,j)
      rchyro2(16,j) = rchyro2(16,j) + rchmono2(16,j)
      rchyro2(17,j) = rchyro2(17,j) + rchmono2(17,j)
      rchyro2(18,j) = rchyro2(18,j) + rchmono2(18,j)
      rchyro2(19,j) = 0. !rchyro2(19,j) + rchmono2(19,j)
      rchyro2(20,j) = 0.  !rchyro2(20,j) + rchmono2(20,j)
      rchyro2(21,j) = 0. !rchyro2(21,j) + rchmono2(21,j)
      rchyro2(22,j) = 0. !rchyro2(22,j) + rchmono2(22,j)
      rchyro2(23,j) = 0.  !rchyro2(23,j) + rchmono2(23,j)
      rchyro2(24,j) = 0. !rchyro2(24,j) + rchmono2(24,j)
      rchyro2(25,j) = 0.  !rchyro2(25,j) + rchmono2(25,j)
      rchyro2(26,j) =  rchmono2(26,j)                                                       !!RPOC kg  last day of the year
      rchyro2(27,j) =  rchmono2(27,j)                                                        !!LPOC kg  last day of the year
      rchyro2(28,j) =  rchmono2(28,j)                                                        !!RDOC kg  last day of the year
      rchyro2(29,j) =  rchmono2(29,j)                                                         !!LDOC kg  last day of the year
      rchyro2(30,j) =  rchmono2(30,j)                                                         !!DIC kg  last day of the year
      rchyro2(31,j) =  rchmono2(31,j)                                                          !!Floating Algea kg  last day of the year
      rchyro2(32,j) =  rchmono2(32,j)                                                         !!Bottom Algea kg  last day of the year
      rchyro2(33,j) = rchyro2(33,j) + rchmono2(33,j)
      rchyro2(34,j) = rchyro2(34,j) + rchmono2(34,j)
      rchyro2(35,j) = rchyro2(35,j) + rchmono2(35,j)
      rchyro2(36,j) = rchyro2(36,j) + rchmono2(36,j)
      rchyro2(37,j) = rchyro2(37,j) + rchmono2(37,j)
      rchyro2(38,j) = rchyro2(38,j) + rchmono2(38,j)
      rchyro2(39,j) = rchyro2(39,j) + rchmono2(39,j)
      rchyro2(40,j) = rchyro2(40,j) + rchmono2(40,j)
      rchyro2(41,j) = rchyro2(41,j) + rchmono2(41,j)
      rchyro2(42,j) = rchyro2(42,j) + rchmono2(42,j)
      rchyro2(43,j) = rchyro2(43,j) + rchmono2(43,j)
      rchyro2(44,j) = rchyro2(44,j) + rchmono2(44,j)
      rchyro2(45,j) = rchyro2(45,j) + rchmono2(45,j)
      rchyro2(46,j) = rchyro2(46,j) + rchmono2(46,j)
      rchyro2(47,j) = rchyro2(47,j) + rchmono2(47,j)
      rchyro2(48,j) = rchyro2(48,j) + rchmono2(48,j)
      rchyro2(49,j) = rchyro2(49,j) + rchmono2(49,j)
      rchyro2(50,j) = rchyro2(50,j) + rchmono2(50,j)
      rchyro2(51,j) = rchyro2(51,j) + rchmono2(51,j)
      rchyro2(52,j) = rchyro2(52,j) + rchmono2(52,j)
      rchyro2(53,j) = rchyro2(53,j) + rchmono2(53,j)
      rchyro2(54,j) = rchyro2(54,j) + rchmono2(54,j)
      rchyro2(55,j) = rchyro2(55,j) + rchmono2(55,j)
      rchyro2(56,j) = rchyro2(56,j) + rchmono2(56,j)
      rchyro2(57,j) = rchyro2(57,j) + rchmono2(57,j)
      rchyro2(58,j) = rchyro2(58,j) + rchmono2(58,j)
      rchyro2(59,j) = rchyro2(59,j) + rchmono2(59,j)
      rchyro2(60,j) = rchyro2(60,j) + rchmono2(60,j)
      rchyro2(61,j) = rchyro2(61,j) + rchmono2(61,j)
      rchyro2(62,j) = rchyro2(62,j) + rchmono2(62,j)
      rchyro2(63,j) = rchyro2(63,j) + rchmono2(63,j)
      rchyro2(64,j) = rchyro2(64,j) + rchmono2(64,j)
      rchyro2(65,j) = rchyro2(65,j) + rchmono2(65,j)
      rchyro2(66,j) = rchyro2(66,j) + rchmono2(66,j)
      rchyro2(67,j) = rchyro2(67,j) + rchmono2(67,j)
      rchyro2(68,j) = rchyro2(68,j) + rchmono2(68,j)
      rchyro2(69,j) = rchyro2(69,j) + rchmono2(69,j)
      rchyro2(70,j) = rchyro2(70,j) + rchmono2(70,j)
      rchyro2(71,j) = rchyro2(71,j) + rchmono2(71,j)
      rchyro2(72,j) = 0. !rchyro2(72,j) + rchmono2(72,j)
      rchyro2(73,j) =  rchmono2(73,j)                                                  !!Sediment C kg  last day of the year
      rchyro2(74,j) = rchyro2(74,j) + rchmono2(74,j)
      rchyro2(75,j) = rchyro2(75,j) + rchmono2(75,j)      
      rchyro2(76,j) =  rchmono2(76,j)                                                  !!Sediment C kg  last day of the year
      rchyro2(77,j) =  rchmono2(77,j)                                                  !!Bury C kg  last day of the year
      rchyro2(78,j) = rchyro2(78,j) + rchmono2(78,j)
      rchyro2(79,j) = rchyro2(79,j) + rchmono2(79,j)
      rchyro2(80,j) = rchyro2(80,j) + rchmono2(80,j)
      rchyro2(81,j) = rchyro2(81,j) + rchmono2(81,j)
      rchyro2(82,j) = rchyro2(82,j) + rchmono2(82,j)
      rchyro2(83,j) = rchyro2(83,j) + rchmono2(83,j)
      rchyro2(84,j) = rchyro2(84,j) + rchmono2(84,j)
      rchyro2(85,j) = rchyro2(85,j) + rchmono2(85,j)
      rchyro2(86,j) = rchyro2(86,j) + rchmono2(86,j)
      rchyro2(87,j) = rchyro2(87,j) + rchmono2(87,j)
      rchyro2(88,j) = rchyro2(88,j) + rchmono2(88,j)
      rchyro2(89,j) = rchyro2(89,j) + rchmono2(89,j)
      rchyro2(90,j) = rchyro2(90,j) + rchmono2(90,j) 
      rchyro2(91,j) = rchyro2(91,j) + rchmono2(91,j)
      rchyro2(92,j) = rchyro2(92,j) + rchmono2(92,j)
      rchyro2(93,j) = rchyro2(93,j) + rchmono2(93,j)
      rchyro2(94,j) = rchyro2(94,j) + rchmono2(94,j)
      rchyro2(95,j) = rchyro2(95,j) + rchmono2(95,j)
      rchyro2(96,j) = rchyro2(96,j) + rchmono2(96,j) 
   
      
      end do
   
   
   
   
      !!============================
    
      end if
      
      rchmono2 = 0.                                                   
      call writea_R
   
   
     end if
   
   return
   end