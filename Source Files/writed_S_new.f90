      subroutine writed_S
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      integer :: j, k,kk
     
     
         !!-------output.std---for different landuse types----------------------{
        
        !if ( LUC_std == 1) then 
        !do kk= 1, 119
        !wshddayo(kk) = wshddayo(kk) / hru_haAgr1                          !!  KG/ha 
       ! enddo
       ! end if
        if (hru_haFor .gt. 0.) then
            do kk= 120, 179
                wshddayo(kk) = wshddayo(kk) / hru_haFor                            !!  KG/ha 
                wshddayN(kk) = wshddayN(kk) / hru_haFor 
            enddo
        end if
        if (hru_haGra  .gt. 0.) then
            do kk= 180, 239
                wshddayo(kk) = wshddayo(kk) / hru_haGra                            !!  KG/ha 
                wshddayN(kk) = wshddayN(kk) / hru_haGra 
            enddo
        End if
        if (hru_haWat .gt. 0.) then
            do kk= 240, 299
                wshddayo(kk) = wshddayo(kk) / hru_haWat                           !!  KG/ha  
                wshddayN(kk) = wshddayN(kk) / hru_haWat  
            enddo
        end if
        if (hru_haAgr .gt. 0.) then
            do kk= 300, 359
                wshddayo(kk) = wshddayo(kk) / hru_haAgr                            !!  KG/ha 
                wshddayN(kk) = wshddayN(kk) / hru_haAgr 
            enddo
        End if
        !! -------output.std---for different landuse types-----------------------}

        wshddayo(75) = wshddayo(75)/da_ha
        wshddayo(76) = wshddayo(76)/da_ha
        wshddayo(77) = wshddayo(77)/da_ha
        wshddayo(78) = wshddayo(78)/da_ha
        wshddayo(79) = wshddayo(79)/da_ha
        wshddayo(82) = wshddayo(82)/da_ha
        wshddayo(83) = wshddayo(83)/da_ha
        wshddayo(84) = wshddayo(84)/da_ha
        wshddayo(85) = wshddayo(85)/da_ha
        wshddayo(86) = wshddayo(86)/da_ha
        wshddayo(87) = wshddayo(87)/da_ha
        wshddayo(88) = wshddayo(88)/da_ha
        wshddayo(89) = wshddayo(89)/da_ha
        wshddayo(90) = wshddayo(90)/da_ha
        wshddayo(91) = wshddayo(91)/da_ha
        wshddayo(92) = wshddayo(92)/da_ha
        wshddayo(93) = wshddayo(93)/da_ha
        wshddayo(94) = wshddayo(94)/da_ha
        wshddayo(95) = wshddayo(95)/da_ha
        wshddayo(96) = wshddayo(96)/da_ha
        wshddayo(97) = wshddayo(97)/da_ha
        wshddayo(98) = wshddayo(98)/da_ha
        wshddayo(99) = wshddayo(99)/da_ha
        wshddayo(100) = wshddayo(100)/da_ha
        wshddayo(101) = wshddayo(101)/da_ha
        wshddayo(102) = wshddayo(102)/da_ha
        wshddayo(103) = wshddayo(103)/da_ha
        wshddayo(104) = wshddayo(104)/da_ha
        wshddayo(105) = wshddayo(105)/da_ha
        wshddayo(106) = wshddayo(106)/da_ha
        
      
      
       if (iprint == 1.or.iprint==3) then 
       
         ! daily write to ouput_C.bsn
        write (output_C_lnd_num,6200) iida,                      & 
        wshddayo(47), wshddayo(48) ,wshddayo(49),   &
        wshddayo(50), wshddayo(51) ,wshddayo(52),   &  
        wshddayo(53), wshddayo(54) ,wshddayo(55),   & 
        wshddayo(56), wshddayo(57) ,wshddayo(58),   &   
        wshddayo(59), wshddayo(60) ,wshddayo(61),   &  
        wshddayo(62), wshddayo(63) ,wshddayo(64),   &   
        wshddayo(65), wshddayo(66) ,                &   !!20
        wshddayo(67), wshddayo(68) ,wshddayo(69),   &  
        wshddayo(70), wshddayo(71) ,wshddayo(72),   &  
        wshddayo(73), wshddayo(74) ,wshddayo(75),   & 
        wshddayo(76), wshddayo(77) ,wshddayo(78),   & 
        wshddayo(79), wshddayo(80) ,wshddayo(81),   &  
        wshddayo(82), wshddayo(83) ,wshddayo(84),   &
        wshddayo(85), wshddayo(86) ,wshddayo(87),   & 
        wshddayo(88), wshddayo(89) ,wshddayo(90),   &
        wshddayo(91), wshddayo(92) ,wshddayo(93),    &   
        wshddayo(94), wshddayo(95) ,wshddayo(96),    &    
        wshddayo(97), wshddayo(98) ,wshddayo(99),    &    !!33
        wshddayo(100), wshddayo(101),wshddayo(102),  & 
        wshddayo(103) ,wshddayo(104),wshddayo(105),  & 
        wshddayo(106)                                      !!60

        
        ! daily write to output_N.bsn
        write (output_N_lnd_num,6300)iyr,mo_chk,iida,    &                   
        wshddayN(1),wshddayN(2),wshddayN(3),wshddayN(4),wshddayN(5),  &
        wshddayN(6),wshddayN(7),wshddayN(8),wshddayN(9),wshddayN(10),  &
        wshddayN(11),wshddayN(12),wshddayN(13),wshddayN(14),wshddayN(15), &
        wshddayN(16),wshddayN(17),wshddayN(18),wshddayN(19),wshddayN(20), &
        wshddayN(21),wshddayN(22),wshddayN(23),wshddayN(24),wshddayN(25),  &
        wshddayN(26),wshddayN(27),wshddayN(28),wshddayN(29),wshddayN(30),  &
        wshddayN(31),wshddayN(32),wshddayN(33),wshddayN(34),wshddayN(35),  &
        wshddayN(36),wshddayN(37),wshddayN(54),wshddayN(55),wshddayN(56) , &                             
        wshddayN(57),wshddayN(58),wshddayN(59),wshddayN(60)
         ! daily write to output_Nagr.bsn
        write (output_Nagr_lnd_num,6300)iyr,mo_chk, iida,    &                   
        wshddayN(300),wshddayN(301),wshddayN(302),wshddayN(303),wshddayN(304),  &
        wshddayN(305),wshddayN(306),wshddayN(307),wshddayN(308),wshddayN(309),  &
        wshddayN(310),wshddayN(311),wshddayN(312),wshddayN(313),wshddayN(314),  &
        wshddayN(315),wshddayN(316),wshddayN(317),wshddayN(318),wshddayN(319),  &
        wshddayN(320),wshddayN(321),wshddayN(322),wshddayN(323),wshddayN(324),  &
        wshddayN(325),wshddayN(326),wshddayN(327),wshddayN(328),wshddayN(329),  &
        wshddayN(330),wshddayN(331),wshddayN(332),wshddayN(333),wshddayN(334),  &
        wshddayN(335),wshddayN(336),wshddayN(353),wshddayN(354),wshddayN(355),  &                             
        wshddayN(356),wshddayN(357),wshddayN(358),wshddayN(359)
        ! daily write to output_Nfor.bsn
        write (output_Nfor_lnd_num,6300)iyr,mo_chk,  iida,    &                   
        wshddayN(120),wshddayN(121),wshddayN(122),wshddayN(123),wshddayN(124),  &
        wshddayN(125),wshddayN(126),wshddayN(127),wshddayN(128),wshddayN(129),  &
        wshddayN(130),wshddayN(131),wshddayN(132),wshddayN(133),wshddayN(134),  &
        wshddayN(135),wshddayN(136),wshddayN(137),wshddayN(138),wshddayN(139),  &
        wshddayN(140),wshddayN(141),wshddayN(142),wshddayN(143),wshddayN(144),  &
        wshddayN(145),wshddayN(146),wshddayN(147),wshddayN(148),wshddayN(149),  &
        wshddayN(150),wshddayN(151),wshddayN(152),wshddayN(153),wshddayN(154),  &
        wshddayN(155),wshddayN(156),wshddayN(173),wshddayN(174),wshddayN(175),  &                             
        wshddayN(176),wshddayN(177),wshddayN(178),wshddayN(179)
        ! daily write to output_Ngra.bsn
        write (output_Ngra_lnd_num,6300)iyr,mo_chk,  iida,    &                   
        wshddayN(180),wshddayN(181),wshddayN(182),wshddayN(183),wshddayN(184),  &
        wshddayN(185),wshddayN(186),wshddayN(187),wshddayN(188),wshddayN(189),  &
        wshddayN(190),wshddayN(191),wshddayN(192),wshddayN(193),wshddayN(194),  &
        wshddayN(195),wshddayN(196),wshddayN(197),wshddayN(198),wshddayN(199),  &
        wshddayN(200),wshddayN(201),wshddayN(202),wshddayN(203),wshddayN(204),  &
        wshddayN(205),wshddayN(206),wshddayN(207),wshddayN(208),wshddayN(209),  &
        wshddayN(210),wshddayN(211),wshddayN(212),wshddayN(213),wshddayN(214),  &
        wshddayN(215),wshddayN(216),wshddayN(233),wshddayN(234),wshddayN(235),  &     
        wshddayN(236),wshddayN(237),wshddayN(238),wshddayN(239)
        ! daily write to output_Nwet.bsn
        write (output_Nwet_lnd_num,6300)iyr,mo_chk,  iida,    &                   
        wshddayN(240),wshddayN(241),wshddayN(242),wshddayN(243),wshddayN(244),  &
        wshddayN(245),wshddayN(246),wshddayN(247),wshddayN(248),wshddayN(249),  &
        wshddayN(250),wshddayN(251),wshddayN(252),wshddayN(253),wshddayN(254),  &
        wshddayN(255),wshddayN(256),wshddayN(257),wshddayN(258),wshddayN(259),  &
        wshddayN(260),wshddayN(261),wshddayN(262),wshddayN(263),wshddayN(264),  &
        wshddayN(265),wshddayN(266),wshddayN(267),wshddayN(268),wshddayN(269),  &
        wshddayN(270),wshddayN(271),wshddayN(272),wshddayN(273),wshddayN(274),  &
        wshddayN(275),wshddayN(276),wshddayN(293),wshddayN(294),wshddayN(295),  &      
        wshddayN(296),wshddayN(297),wshddayN(298),wshddayN(299)
       end if
        
        
        wshdmonN = wshdmonN + wshddayN           
        
         
        
        return
  6200  format(i5,60f10.2)   
  6300  format(3i5,2f10.2,42f10.4)       
        end