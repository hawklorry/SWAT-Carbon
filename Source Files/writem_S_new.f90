     subroutine writem_S
     use parm
       implicit none
      integer ::  j, k
      real :: sum

!! if last day of month or last day in last year
 
      if (i_mo /= mo_chk .or. (curyr == nbyr .and. i == idal)) then
        mo_atmo = mo_atmo + 1
 
        !! calculate current month (cumulative) of simulation
!       immo = immo + 1
 
        !! calculate number of days in month
        idlast = 0
        if (immo == 1 .and. idaf > 0) then
          idlast = ndays(mo_chk+1) - (idaf - 1)
          if (leapyr == 1 .and. mo_chk == 2) idlast = idlast - 1
        elseif (curyr == nbyr .and. i == idal) then
          idlast = i - ndays(mo_chk)
        else
          idlast = ndays(mo_chk+1) - ndays(mo_chk)
          if (leapyr == 1 .and. mo_chk == 2) idlast = idlast - 1
        end if

   
        !! calculate average temperature for month in watershed
        if (idlast > 0.) then
         
          wshdmono(47) = wshdmono(47) /Real(idlast)   
          wshdmono(59) = wshdmono(59) /Real(idlast)  
          wshdmono(60) = wshdmono(60) /Real(idlast)  
          wshdmono(61) = wshdmono(61) /Real(idlast)  
          wshdmono(62) = wshdmono(62) /Real(idlast)  
          wshdmono(63) = wshdmono(63) /Real(idlast)  
          wshdmono(65) = wshdmono(65) /Real(idlast)      
          wshdmono(67) = wshdmono(67) /Real(idlast)  
          wshdmono(68) = wshdmono(68) /Real(idlast)   
          wshdmono(78) = wshdmono(78) /Real(idlast)  
          wshdmono(79) = wshdmono(79) /Real(idlast)   
          wshdmono(80) = wshdmono(80) /Real(idlast)  
          wshdmono(81) = wshdmono(81) /Real(idlast) 
          wshdmono(104) = wshdmono(104) /Real(idlast) 
        else
        
          wshdmono(47) =0.    
          wshdmono(59) = 0.
          wshdmono(60) = 0.
          wshdmono(61) = 0.
          wshdmono(62) = 0.
          wshdmono(63) = 0.
          wshdmono(66) = 0. 
          wshdmono(67) = 0.
          wshdmono(68) =0.
          wshdmono(78) = 0.
          wshdmono(79) =0.
          wshdmono(80) = 0.
          wshdmono(81) =0.
          wshdmono(104) =0.
        end if

          
          wshdmonN(1)= wshdmonN(1)/Real(idlast)
          wshdmonN(2)= wshdmonN(2)/Real(idlast)
          wshdmonN(3)= wshdmonN(3)/Real(idlast)
          wshdmonN(4)= wshdmonN(4)/Real(idlast)
          wshdmonN(5)= wshdmonN(5)/Real(idlast)
          wshdmonN(6)= wshdmonN(6)/Real(idlast)
          wshdmonN(57)= wshdmonN(57)/Real(idlast)
          wshdmonN(58)= wshdmonN(58)/Real(idlast)
          wshdmonN(59)= wshdmonN(59)/Real(idlast)
          
          
          wshdmonN(300)= wshdmonN(300)/Real(idlast)
          wshdmonN(301)= wshdmonN(301)/Real(idlast)
          wshdmonN(302)= wshdmonN(302)/Real(idlast)
          wshdmonN(303)= wshdmonN(303)/Real(idlast)
          wshdmonN(304)= wshdmonN(304)/Real(idlast)
          wshdmonN(305)= wshdmonN(305)/Real(idlast)
          wshdmonN(356)= wshdmonN(356)/Real(idlast)
          wshdmonN(357)= wshdmonN(357)/Real(idlast)
          wshdmonN(358)= wshdmonN(358)/Real(idlast)
          
          wshdmonN(120)= wshdmonN(120)/Real(idlast)
          wshdmonN(121)= wshdmonN(121)/Real(idlast)
          wshdmonN(122)= wshdmonN(122)/Real(idlast)
          wshdmonN(123)= wshdmonN(123)/Real(idlast)
          wshdmonN(124)= wshdmonN(124)/Real(idlast)
          wshdmonN(125)= wshdmonN(125)/Real(idlast)
          wshdmonN(176)= wshdmonN(176)/Real(idlast)
          wshdmonN(177)= wshdmonN(177)/Real(idlast)
          wshdmonN(178)= wshdmonN(178)/Real(idlast)
          
          wshdmonN(180)= wshdmonN(180)/Real(idlast)
          wshdmonN(181)= wshdmonN(181)/Real(idlast)
          wshdmonN(182)= wshdmonN(182)/Real(idlast)
          wshdmonN(183)= wshdmonN(183)/Real(idlast)
          wshdmonN(184)= wshdmonN(184)/Real(idlast)
          wshdmonN(185)= wshdmonN(185)/Real(idlast)
          wshdmonN(236)= wshdmonN(236)/Real(idlast)
          wshdmonN(237)= wshdmonN(237)/Real(idlast)
          wshdmonN(238)= wshdmonN(238)/Real(idlast)
          
          
          wshdmonN(240)= wshdmonN(240)/Real(idlast)
          wshdmonN(241)= wshdmonN(241)/Real(idlast)
          wshdmonN(242)= wshdmonN(242)/Real(idlast)
          wshdmonN(243)= wshdmonN(243)/Real(idlast)
          wshdmonN(244)= wshdmonN(244)/Real(idlast)
          wshdmonN(245)= wshdmonN(245)/Real(idlast)
          wshdmonN(296)= wshdmonN(296)/Real(idlast)
          wshdmonN(297)= wshdmonN(297)/Real(idlast)
          wshdmonN(298)= wshdmonN(298)/Real(idlast)
          
          
     !  if (iprint /= 2 .and. curyr > nyskip) then
       if (iprint == 0 .and. curyr > nyskip) then
          !! monthly write--output.std
          
          write (output_C_lnd_num,6200) mo_chk,          &    
            wshdmono(47),wshdmono(48),       &
            wshdmono(49),wshdmono(50),wshdmono(51),         &
            wshdmono(52),wshdmono(53),wshdmono(54),         &
            wshdmono(55),wshdmono(56),wshdmono(57),         & 
            wshdmono(58),wshdmono(59),wshdmono(60),          &
            wshdmono(61),wshdmono(62),wshdmono(63),         &
            wshdmono(64),wshdmono(65),wshdmono(66),         &
            wshdmono(67),wshdmono(68),wshdmono(69),          &
            wshdmono(70),wshdmono(71),wshdmono(72),         &
            wshdmono(73),wshdmono(74),wshdmono(75),         & 
            wshdmono(76),wshdmono(77),wshdmono(78),          & 
            wshdmono(79),wshdmono(80),wshdmono(81),          & 
            wshdmono(82),wshdmono(83),wshdmono(84),           &   
            wshdmono(85),wshdmono(86),wshdmono(87),             &
            wshdmono(88),wshdmono(89),wshdmono(90),          &
            wshdmono(91),wshdmono(92),wshdmono(93),         &   
            wshdmono(94),wshdmono(95),wshdmono(96),             & 
            wshdmono(97),wshdmono(98),wshdmono(99),            &
            wshdmono(100),wshdmono(101),                     & 
            wshdmono(102),wshdmono(103),wshdmono(104),         &
            wshdmono(105),wshdmono(106)
  
       
       
       ! monthly write to output_N.bsn
        write (output_N_lnd_num,6300)iyr, mo_chk,mo_chk,    &                   
        wshdmonN(1),wshdmonN(2),wshdmonN(3),wshdmonN(4),wshdmonN(5),  &
        wshdmonN(6),wshdmonN(7),wshdmonN(8),wshdmonN(9),wshdmonN(10),  &
        wshdmonN(11),wshdmonN(12),wshdmonN(13),wshdmonN(14),wshdmonN(15), &
        wshdmonN(16),wshdmonN(17),wshdmonN(18),wshdmonN(19),wshdmonN(20), &
        wshdmonN(21),wshdmonN(22),wshdmonN(23),wshdmonN(24),wshdmonN(25),  &
        wshdmonN(26),wshdmonN(27),wshdmonN(28),wshdmonN(29),wshdmonN(30),  &
        wshdmonN(31),wshdmonN(32),wshdmonN(33),wshdmonN(34),wshdmonN(35),  &
        wshdmonN(36),wshdmonN(37),wshdmonN(54),wshdmonN(55),wshdmonN(56) , &                             
        wshdmonN(57),wshdmonN(58),wshdmonN(59),wshdmonN(60)
  
         ! monthy write to output_Nagr.bsn
        write (output_Nagr_lnd_num,6300) iyr,mo_chk,mo_chk,    &                   
        wshdmonN(300),wshdmonN(301),wshdmonN(302),wshdmonN(303),wshdmonN(304),  &
        wshdmonN(305),wshdmonN(306),wshdmonN(307),wshdmonN(308),wshdmonN(309),  &
        wshdmonN(310),wshdmonN(311),wshdmonN(312),wshdmonN(313),wshdmonN(314), &
        wshdmonN(315),wshdmonN(316),wshdmonN(317),wshdmonN(318),wshdmonN(319), &
        wshdmonN(320),wshdmonN(321),wshdmonN(322),wshdmonN(323),wshdmonN(324),  &
        wshdmonN(325),wshdmonN(326),wshdmonN(327),wshdmonN(328),wshdmonN(329),  &
        wshdmonN(330),wshdmonN(331),wshdmonN(332),wshdmonN(333),wshdmonN(334),  &
        wshdmonN(335),wshdmonN(336),wshdmonN(353),wshdmonN(354),wshdmonN(355),  &                             
        wshdmonN(356),wshdmonN(357),wshdmonN(358),wshdmonN(359)
                             
        ! monthy  write to output_Nfor.bsn
        write (output_Nfor_lnd_num,6300) iyr,mo_chk,mo_chk,    &                   
        wshdmonN(120),wshdmonN(121),wshdmonN(122),wshdmonN(123),wshdmonN(124),  &
        wshdmonN(125),wshdmonN(126),wshdmonN(127),wshdmonN(128),wshdmonN(129),  &
        wshdmonN(130),wshdmonN(131),wshdmonN(132),wshdmonN(133),wshdmonN(134),  &
        wshdmonN(135),wshdmonN(136),wshdmonN(137),wshdmonN(138),wshdmonN(139),  &
        wshdmonN(140),wshdmonN(141),wshdmonN(142),wshdmonN(143),wshdmonN(144),  &
        wshdmonN(145),wshdmonN(146),wshdmonN(147),wshdmonN(148),wshdmonN(149),  &
        wshdmonN(150),wshdmonN(151),wshdmonN(152),wshdmonN(153),wshdmonN(154),  &
        wshdmonN(155),wshdmonN(156),wshdmonN(173),wshdmonN(174),wshdmonN(175),  &                             
        wshdmonN(176),wshdmonN(177),wshdmonN(178),wshdmonN(179)                             
        
        ! monthy  write to output_Ngra.bsn
        write (output_Ngra_lnd_num,6300) iyr,mo_chk,mo_chk,    &                   
        wshdmonN(180),wshdmonN(181),wshdmonN(182),wshdmonN(183),wshdmonN(184),  &
        wshdmonN(185),wshdmonN(186),wshdmonN(187),wshdmonN(188),wshdmonN(189),  &
        wshdmonN(190),wshdmonN(191),wshdmonN(192),wshdmonN(193),wshdmonN(194),  &
        wshdmonN(195),wshdmonN(196),wshdmonN(197),wshdmonN(198),wshdmonN(199),  &
        wshdmonN(200),wshdmonN(201),wshdmonN(202),wshdmonN(203),wshdmonN(204),  &
        wshdmonN(205),wshdmonN(206),wshdmonN(207),wshdmonN(208),wshdmonN(209),  &
        wshdmonN(210),wshdmonN(211),wshdmonN(212),wshdmonN(213),wshdmonN(214),  &
        wshdmonN(215),wshdmonN(216),wshdmonN(233),wshdmonN(234),wshdmonN(235),  &     
        wshdmonN(236),wshdmonN(237),wshdmonN(238),wshdmonN(239)      
        
        ! monthy  write to output_Nwet.bsn
        write (output_Nwet_lnd_num,6300)iyr, mo_chk,mo_chk,    &                   
        wshdmonN(240),wshdmonN(241),wshdmonN(242),wshdmonN(243),wshdmonN(244),  &
        wshdmonN(245),wshdmonN(246),wshdmonN(247),wshdmonN(248),wshdmonN(249),  &
        wshdmonN(250),wshdmonN(251),wshdmonN(252),wshdmonN(253),wshdmonN(254),  &
        wshdmonN(255),wshdmonN(256),wshdmonN(257),wshdmonN(258),wshdmonN(259),  &
        wshdmonN(260),wshdmonN(261),wshdmonN(262),wshdmonN(263),wshdmonN(264),  &
        wshdmonN(265),wshdmonN(266),wshdmonN(267),wshdmonN(268),wshdmonN(269),  &
        wshdmonN(270),wshdmonN(271),wshdmonN(272),wshdmonN(273),wshdmonN(274),  &
        wshdmonN(275),wshdmonN(276),wshdmonN(293),wshdmonN(294),wshdmonN(295),  &      
        wshdmonN(296),wshdmonN(297),wshdmonN(298),wshdmonN(299) 



      !if (iprint == 0) then 
      call hrumon_N   
      !end if 
     

      end if   !!if (iprint /= 2 .and. curyr > nyskip) then
      
      
      if (curyr > nyskip) then
       !! sum annual values 
       wshdyrN = wshdyrN + wshdmonN
       hruyrN = hruyrN + hrumonN       
      end if
     
       wshdmonN = 0.   
       hrumonN = 0.    
      
      call writea_S
       
       
      end if    !! if (i_mo /= mo_chk .or. (curyr == nbyr .and. i == idal)) then
        
        
        
      return 
 6200 format(i5,60f10.2)   !! 
 6300 format(3i5,2f10.2,42f10.4)   
      end 