     subroutine writeaa_S
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
       
      real :: yrs, xx, xmm, sumno3, summinp
      integer :: j, nnro, nicr, k, ly, ic, ii

!! calculate number of years simulated
      yrs = 0.
      do j = 1, nbyr
        xx = 0.
        xx = 366. - Real(leapyr)
        if (j > nyskip) then
          if (j == 1 .and. idaf > 0) then
            yrs = yrs + (xx - (Real(idaf) - 1. - Real(fcstcnt))) / xx
          elseif (j == nbyr .and. idal > 0) then
            yrs = yrs + ((Real(idal) - Real(fcstcnt)) / xx)
          else
            yrs = yrs + 1.
          end if
        end if
      end do
      if (yrs <= 0) return
      
   
      
      
     !! calculate average annual values for HRU data
     
      hruaaN = hruaaN / yrs      
      
      !! calculate average annual values for watershed data
      
      wshdaaN = wshdaaN / yrs   
      
      !if (iprint /= 1) then
       if (iprint == 2) then
        !! write average annual output--HRU (output.hru)
      
       call hruaa_N(yrs)             
      !end if
      
      
         
      write (output_C_lnd_num,6200) iyr,          &
             wshdaao(47),wshdaao(48),wshdaao(49),wshdaao(50),  & 
             wshdaao(51),wshdaao(52),wshdaao(53),wshdaao(54),  & 
             wshdaao(55),wshdaao(56),wshdaao(57),wshdaao(58),  &  
             wshdaao(59),wshdaao(60),wshdaao(61),wshdaao(62),  &
             wshdaao(63),wshdaao(64),wshdaao(65),wshdaao(66),  & 
             wshdaao(67),wshdaao(68),wshdaao(69),wshdaao(70), & 
             wshdaao(71),wshdaao(72),wshdaao(73),wshdaao(74),& 
             wshdaao(75),wshdaao(76),wshdaao(77),wshdaao(78),& 
             wshdaao(79),wshdaao(80),wshdaao(81),wshdaao(82),& 
             wshdaao(83),wshdaao(84),wshdaao(85),wshdaao(86), & 
             wshdaao(87),wshdaao(88),wshdaao(89),wshdaao(90),  & 
             wshdaao(91),wshdaao(92),wshdaao(93),wshdaao(94), &
             wshdaao(95),wshdaao(96),wshdaao(97),wshdaao(98), &  
             wshdaao(99),wshdaao(100),wshdaao(101),  & 
             wshdaao(102),wshdaao(103),wshdaao(104),wshdaao(105), &
             wshdaao(106)
      
      
       ! average annual write to output_N.bsn
        write (output_N_lnd_num,6300) nbyr-nyskip ,nbyr-nyskip, nbyr-nyskip,   &                   
        wshdaaN(1),wshdaaN(2),wshdaaN(3),wshdaaN(4),wshdaaN(5),  &
        wshdaaN(6),wshdaaN(7),wshdaaN(8),wshdaaN(9),wshdaaN(10),  &
        wshdaaN(11),wshdaaN(12),wshdaaN(13),wshdaaN(14),wshdaaN(15), &
        wshdaaN(16),wshdaaN(17),wshdaaN(18),wshdaaN(19),wshdaaN(20), &
        wshdaaN(21),wshdaaN(22),wshdaaN(23),wshdaaN(24),wshdaaN(25),  &
        wshdaaN(26),wshdaaN(27),wshdaaN(28),wshdaaN(29),wshdaaN(30),  &
        wshdaaN(31),wshdaaN(32),wshdaaN(33),wshdaaN(34),wshdaaN(35),  &
        wshdaaN(36),wshdaaN(37),wshdaaN(54),wshdaaN(55),wshdaaN(56) , &                             
        wshdaaN(57),wshdaaN(58),wshdaaN(59),wshdaaN(60) 
      
       ! average annual write to output_Nag.bsn
        write (output_Nagr_lnd_num,6300)nbyr-nyskip ,nbyr-nyskip, nbyr-nyskip,    &                    
        wshdaaN(300),wshdaaN(301),wshdaaN(302),wshdaaN(303),wshdaaN(304),  &
        wshdaaN(305),wshdaaN(306),wshdaaN(307),wshdaaN(308),wshdaaN(309),  &
        wshdaaN(310),wshdaaN(311),wshdaaN(312),wshdaaN(313),wshdaaN(314), &
        wshdaaN(315),wshdaaN(316),wshdaaN(317),wshdaaN(318),wshdaaN(319), &
        wshdaaN(320),wshdaaN(321),wshdaaN(322),wshdaaN(323),wshdaaN(324),  &
        wshdaaN(325),wshdaaN(326),wshdaaN(327),wshdaaN(328),wshdaaN(329),  &
        wshdaaN(330),wshdaaN(331),wshdaaN(332),wshdaaN(333),wshdaaN(334),  &
        wshdaaN(335),wshdaaN(336),wshdaaN(353),wshdaaN(354),wshdaaN(355),  &                             
        wshdaaN(356),wshdaaN(357),wshdaaN(358),wshdaaN(359)
      
        ! average annual to output_Nfor.bsn
        write (output_Nfor_lnd_num,6300) nbyr-nyskip ,nbyr-nyskip, nbyr-nyskip,    &                   
        wshdaaN(120),wshdaaN(121),wshdaaN(122),wshdaaN(123),wshdaaN(124),  &
        wshdaaN(125),wshdaaN(126),wshdaaN(127),wshdaaN(128),wshdaaN(129),  &
        wshdaaN(130),wshdaaN(131),wshdaaN(132),wshdaaN(133),wshdaaN(134),  &
        wshdaaN(135),wshdaaN(136),wshdaaN(137),wshdaaN(138),wshdaaN(139),  &
        wshdaaN(140),wshdaaN(141),wshdaaN(142),wshdaaN(143),wshdaaN(144),  &
        wshdaaN(145),wshdaaN(146),wshdaaN(147),wshdaaN(148),wshdaaN(149),  &
        wshdaaN(150),wshdaaN(151),wshdaaN(152),wshdaaN(153),wshdaaN(154),  &
        wshdaaN(155),wshdaaN(156),wshdaaN(173),wshdaaN(174),wshdaaN(175),  &                             
        wshdaaN(176),wshdaaN(177),wshdaaN(178),wshdaaN(179)                             
        
        ! average annual to output_Ngra.bsn
        write (output_Ngra_lnd_num,6300) nbyr-nyskip ,nbyr-nyskip, nbyr-nyskip,   &                   
        wshdaaN(180),wshdaaN(181),wshdaaN(182),wshdaaN(183),wshdaaN(184),  &
        wshdaaN(185),wshdaaN(186),wshdaaN(187),wshdaaN(188),wshdaaN(189),  &
        wshdaaN(190),wshdaaN(191),wshdaaN(192),wshdaaN(193),wshdaaN(194),  &
        wshdaaN(195),wshdaaN(196),wshdaaN(197),wshdaaN(198),wshdaaN(199),  &
        wshdaaN(200),wshdaaN(201),wshdaaN(202),wshdaaN(203),wshdaaN(204),  &
        wshdaaN(205),wshdaaN(206),wshdaaN(207),wshdaaN(208),wshdaaN(209),  &
        wshdaaN(210),wshdaaN(211),wshdaaN(212),wshdaaN(213),wshdaaN(214),  &
        wshdaaN(215),wshdaaN(216),wshdaaN(233),wshdaaN(234),wshdaaN(235),  &     
        wshdaaN(236),wshdaaN(237),wshdaaN(238),wshdaaN(239)      
        
        ! average annual to output_Nwet.bsn
        write (output_Nwet_lnd_num,6300) nbyr-nyskip ,nbyr-nyskip, nbyr-nyskip,    &                   
        wshdaaN(240),wshdaaN(241),wshdaaN(242),wshdaaN(243),wshdaaN(244),  &
        wshdaaN(245),wshdaaN(246),wshdaaN(247),wshdaaN(248),wshdaaN(249),  &
        wshdaaN(250),wshdaaN(251),wshdaaN(252),wshdaaN(253),wshdaaN(254),  &
        wshdaaN(255),wshdaaN(256),wshdaaN(257),wshdaaN(258),wshdaaN(259),  &
        wshdaaN(260),wshdaaN(261),wshdaaN(262),wshdaaN(263),wshdaaN(264),  &
        wshdaaN(265),wshdaaN(266),wshdaaN(267),wshdaaN(268),wshdaaN(269),  &
        wshdaaN(270),wshdaaN(271),wshdaaN(272),wshdaaN(273),wshdaaN(274),  &
        wshdaaN(275),wshdaaN(276),wshdaaN(293),wshdaaN(294),wshdaaN(295),  &      
        wshdaaN(296),wshdaaN(297),wshdaaN(298),wshdaaN(299) 

       end if
      
      
      
      call stdaa_N      
      
      return
 6200 format(i5,60f10.2)   
 6300 format(3i5,2f10.2,42f10.4)  
      end
