      subroutine writea_S
 
 
 
 
      
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
      real :: sum



      if (i_mo <= mo_chk .or. (curyr == nbyr .and. i == idal)) then !nbyr |number of calendar years simulated
        !! calculate average annual max and min temperature
       
         wshdyro(47) = wshdyro(47) / 12.     
         wshdyro(59) = wshdyro(59) / 12.  
         wshdyro(60) = wshdyro(60) / 12.  
         wshdyro(61) = wshdyro(61) / 12.  
         wshdyro(62) = wshdyro(62) / 12.  
         wshdyro(63) = wshdyro(63) / 12.  
         wshdyro(65) = wshdyro(65) / 12.  
         wshdyro(67) = wshdyro(67) / 12.  
         wshdyro(68) = wshdyro(68) / 12.  
         wshdyro(78) = wshdyro(78) / 12.  
         wshdyro(79) = wshdyro(79) / 12.  
         wshdyro(80) = wshdyro(80) / 12.  
         wshdyro(81) = wshdyro(81) / 12.  
         wshdyro(104) = wshdyro(104) / 12.  
         
          wshdyrN(1)= wshdyrN(1)/12.
          wshdyrN(2)= wshdyrN(2)/12.
          wshdyrN(3)= wshdyrN(3)/12.
          wshdyrN(4)= wshdyrN(4)/12.
          wshdyrN(5)= wshdyrN(5)/12.
          wshdyrN(6)= wshdyrN(6)/12.
          wshdyrN(57)= wshdyrN(57)/12.
          wshdyrN(58)= wshdyrN(58)/12.
          wshdyrN(59)= wshdyrN(59)/12.
          
          wshdyrN(300)= wshdyrN(300)/12.
          wshdyrN(301)= wshdyrN(301)/12.
          wshdyrN(302)= wshdyrN(302)/12.
          wshdyrN(303)= wshdyrN(303)/12.
          wshdyrN(304)= wshdyrN(304)/12.
          wshdyrN(305)= wshdyrN(305)/12.
          wshdyrN(356)= wshdyrN(356)/12.
          wshdyrN(357)= wshdyrN(357)/12.
          wshdyrN(358)= wshdyrN(358)/12.
          
          wshdyrN(120)= wshdyrN(120)/12.
          wshdyrN(121)= wshdyrN(121)/12.
          wshdyrN(122)= wshdyrN(122)/12.
          wshdyrN(123)= wshdyrN(123)/12.
          wshdyrN(124)= wshdyrN(124)/12.
          wshdyrN(125)= wshdyrN(125)/12.
          wshdyrN(176)= wshdyrN(176)/12.
          wshdyrN(177)= wshdyrN(177)/12.
          wshdyrN(178)= wshdyrN(178)/12.
          
          wshdyrN(180)= wshdyrN(180)/12.
          wshdyrN(181)= wshdyrN(181)/12.
          wshdyrN(182)= wshdyrN(182)/12.
          wshdyrN(183)= wshdyrN(183)/12.
          wshdyrN(184)= wshdyrN(184)/12.
          wshdyrN(185)= wshdyrN(185)/12.
          wshdyrN(236)= wshdyrN(236)/12.
          wshdyrN(237)= wshdyrN(237)/12.
          wshdyrN(238)= wshdyrN(238)/12.
          
          
          wshdyrN(240)= wshdyrN(240)/12.
          wshdyrN(241)= wshdyrN(241)/12.
          wshdyrN(242)= wshdyrN(242)/12.
          wshdyrN(243)= wshdyrN(243)/12.
          wshdyrN(244)= wshdyrN(244)/12.
          wshdyrN(245)= wshdyrN(245)/12.
          wshdyrN(296)= wshdyrN(296)/12.
          wshdyrN(297)= wshdyrN(297)/12.
          wshdyrN(298)= wshdyrN(298)/12.
         
         
         if (iprint == 2 ) then 
         
          !! annual write-output.std
          
          write (output_C_lnd_num,6200) iyr ,      &
               wshdyro(47),wshdyro(48),wshdyro(49),wshdyro(50),    &
               wshdyro(51),wshdyro(52),wshdyro(53),wshdyro(54),    &    
               wshdyro(55),wshdyro(56),wshdyro(57),wshdyro(58),    &
               wshdyro(59),wshdyro(60),wshdyro(61),wshdyro(62),    &
               wshdyro(63),wshdyro(64),wshdyro(65),wshdyro(66),    &
               wshdyro(67),wshdyro(68),wshdyro(69),wshdyro(70),    &
               wshdyro(71),wshdyro(72),wshdyro(73),wshdyro(74),    &
               wshdyro(75),wshdyro(76),wshdyro(77),wshdyro(78),    &
               wshdyro(79),wshdyro(80),wshdyro(81),wshdyro(82),    &
               wshdyro(83),wshdyro(84),wshdyro(85),wshdyro(86),    &
               wshdyro(87),wshdyro(88),wshdyro(89),wshdyro(90),    &
               wshdyro(91),wshdyro(92),wshdyro(93),wshdyro(94),    &
               wshdyro(95),wshdyro(96),wshdyro(97),wshdyro(98),    &
               wshdyro(99),wshdyro(100),wshdyro(101),wshdyro(102), &
               wshdyro(103),wshdyro(104),wshdyro(105),wshdyro(106)    !! 15*4
         
         
        ! annual write to output_N.bsn
        write (output_N_lnd_num,6300) iyr ,iyr, iyr,   &                   
        wshdyrN(1),wshdyrN(2),wshdyrN(3),wshdyrN(4),wshdyrN(5),  &
        wshdyrN(6),wshdyrN(7),wshdyrN(8),wshdyrN(9),wshdyrN(10),  &
        wshdyrN(11),wshdyrN(12),wshdyrN(13),wshdyrN(14),wshdyrN(15), &
        wshdyrN(16),wshdyrN(17),wshdyrN(18),wshdyrN(19),wshdyrN(20), &
        wshdyrN(21),wshdyrN(22),wshdyrN(23),wshdyrN(24),wshdyrN(25),  &
        wshdyrN(26),wshdyrN(27),wshdyrN(28),wshdyrN(29),wshdyrN(30),  &
        wshdyrN(31),wshdyrN(32),wshdyrN(33),wshdyrN(34),wshdyrN(35),  &
        wshdyrN(36),wshdyrN(37),wshdyrN(54),wshdyrN(55),wshdyrN(56) , &                             
        wshdyrN(57),wshdyrN(58),wshdyrN(59),wshdyrN(60) 
         
        
         ! annual write to output_Nag.bsn
        write (output_Nagr_lnd_num,6300) iyr ,iyr, iyr,   &                   
        wshdyrN(300),wshdyrN(301),wshdyrN(302),wshdyrN(303),wshdyrN(304),  &
        wshdyrN(305),wshdyrN(306),wshdyrN(307),wshdyrN(308),wshdyrN(309),  &
        wshdyrN(310),wshdyrN(311),wshdyrN(312),wshdyrN(313),wshdyrN(314), &
        wshdyrN(315),wshdyrN(316),wshdyrN(317),wshdyrN(318),wshdyrN(319), &
        wshdyrN(320),wshdyrN(321),wshdyrN(322),wshdyrN(323),wshdyrN(324),  &
        wshdyrN(325),wshdyrN(326),wshdyrN(327),wshdyrN(328),wshdyrN(329),  &
        wshdyrN(330),wshdyrN(331),wshdyrN(332),wshdyrN(333),wshdyrN(334),  &
        wshdyrN(335),wshdyrN(336),wshdyrN(353),wshdyrN(354),wshdyrN(355),  &                             
        wshdyrN(356),wshdyrN(357),wshdyrN(358),wshdyrN(359)             


       ! annual write to output_Nfor.bsn
        write (output_Nfor_lnd_num,6300) iyr, iyr, iyr,  &                   
        wshdyrN(120),wshdyrN(121),wshdyrN(122),wshdyrN(123),wshdyrN(124),  &
        wshdyrN(125),wshdyrN(126),wshdyrN(127),wshdyrN(128),wshdyrN(129),  &
        wshdyrN(130),wshdyrN(131),wshdyrN(132),wshdyrN(133),wshdyrN(134),  &
        wshdyrN(135),wshdyrN(136),wshdyrN(137),wshdyrN(138),wshdyrN(139),  &
        wshdyrN(140),wshdyrN(141),wshdyrN(142),wshdyrN(143),wshdyrN(144),  &
        wshdyrN(145),wshdyrN(146),wshdyrN(147),wshdyrN(148),wshdyrN(149),  &
        wshdyrN(150),wshdyrN(151),wshdyrN(152),wshdyrN(153),wshdyrN(154),  &
        wshdyrN(155),wshdyrN(156),wshdyrN(173),wshdyrN(174),wshdyrN(175),  &                             
        wshdyrN(176),wshdyrN(177),wshdyrN(178),wshdyrN(179)                             
        
        ! annual write to output_Ngra.bsn
        write (output_Ngra_lnd_num,6300) iyr,iyr,iyr,    &                   
        wshdyrN(180),wshdyrN(181),wshdyrN(182),wshdyrN(183),wshdyrN(184),  &
        wshdyrN(185),wshdyrN(186),wshdyrN(187),wshdyrN(188),wshdyrN(189),  &
        wshdyrN(190),wshdyrN(191),wshdyrN(192),wshdyrN(193),wshdyrN(194),  &
        wshdyrN(195),wshdyrN(196),wshdyrN(197),wshdyrN(198),wshdyrN(199),  &
        wshdyrN(200),wshdyrN(201),wshdyrN(202),wshdyrN(203),wshdyrN(204),  &
        wshdyrN(205),wshdyrN(206),wshdyrN(207),wshdyrN(208),wshdyrN(209),  &
        wshdyrN(210),wshdyrN(211),wshdyrN(212),wshdyrN(213),wshdyrN(214),  &
        wshdyrN(215),wshdyrN(216),wshdyrN(233),wshdyrN(234),wshdyrN(235),  &     
        wshdyrN(236),wshdyrN(237),wshdyrN(238),wshdyrN(239)     
        
        ! annual write to output_Nwet.bsn
        write (output_Nwet_lnd_num,6300) iyr,iyr, iyr,   &                   
        wshdyrN(240),wshdyrN(241),wshdyrN(242),wshdyrN(243),wshdyrN(244),  &
        wshdyrN(245),wshdyrN(246),wshdyrN(247),wshdyrN(248),wshdyrN(249),  &
        wshdyrN(250),wshdyrN(251),wshdyrN(252),wshdyrN(253),wshdyrN(254),  &
        wshdyrN(255),wshdyrN(256),wshdyrN(257),wshdyrN(258),wshdyrN(259),  &
        wshdyrN(260),wshdyrN(261),wshdyrN(262),wshdyrN(263),wshdyrN(264),  &
        wshdyrN(265),wshdyrN(266),wshdyrN(267),wshdyrN(268),wshdyrN(269),  &
        wshdyrN(270),wshdyrN(271),wshdyrN(272),wshdyrN(273),wshdyrN(274),  &
        wshdyrN(275),wshdyrN(276),wshdyrN(293),wshdyrN(294),wshdyrN(295),  &      
        wshdyrN(296),wshdyrN(297),wshdyrN(298),wshdyrN(299) 


       !if (iprint /= 1 .and. iprint/= 3) then      
       call hruyr_N     
       end if
        
       if (curyr > nyskip) then
         wshdaaN = wshdaaN + wshdyrN      
         hruaaN = hruaaN + hruyrN      
       end if

         wshdyrN = 0.
         hruyrN = 0.            

      end if
      
      
      return 
 6200 format (i5,60f10.2)  
 6300  format(3i5,2f10.2,42f10.4)    
      end 