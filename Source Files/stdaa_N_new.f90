      subroutine stdaa_N
     
      use parm
      implicit none
 
    
    
      !! Basin all lands
       write (output_N_std_num,3200) wshdaaN(1), wshdaaN(2),wshdaaN(3),   &
       wshdaaN(4),   &
       wshdaaN(5), wshdaaN(6), wshdaaN(7), wshdaaN(8),   &
       wshdaaN(9), wshdaaN(10), wshdaaN(11), wshdaaN(12),   &
       wshdaaN(13), wshdaaN(14), wshdaaN(15), wshdaaN(16),  & 
       wshdaaN(17), wshdaaN(18), wshdaaN(19), wshdaaN(20),   &
       wshdaaN(21), wshdaaN(22), wshdaaN(23), wshdaaN(24),     &  
       wshdaaN(25), wshdaaN(26), wshdaaN(27), wshdaaN(28),     &
       wshdaaN(29), wshdaaN(30), wshdaaN(31), wshdaaN(32),   &
       wshdaaN(33), wshdaaN(34), wshdaaN(35), wshdaaN(36),   &  
       wshdaaN(37), wshdaaN(38), wshdaaN(39), wshdaaN(40),   &
       wshdaaN(41), wshdaaN(42), wshdaaN(43), wshdaaN(44),   & 
       wshdaaN(45), wshdaaN(46), wshdaaN(47), wshdaaN(48),    &  
       wshdaaN(49), wshdaaN(50), wshdaaN(51), wshdaaN(52),    &  
       wshdaaN(53), wshdaaN(54)

     
      !! Foresty
       write (output_N_std_num,3300) wshdaaN(120), wshdaaN(121),wshdaaN(122),   &
       wshdaaN(123),   &
       wshdaaN(124), wshdaaN(125), wshdaaN(126), wshdaaN(127),   &
       wshdaaN(128), wshdaaN(129), wshdaaN(130), wshdaaN(131),   &
       wshdaaN(132), wshdaaN(133), wshdaaN(134), wshdaaN(135),  & 
       wshdaaN(136), wshdaaN(137), wshdaaN(138), wshdaaN(139),   &
       wshdaaN(140), wshdaaN(141), wshdaaN(142), wshdaaN(143),     &  
       wshdaaN(144), wshdaaN(145), wshdaaN(146), wshdaaN(147),     &
       wshdaaN(148), wshdaaN(149), wshdaaN(150), wshdaaN(151),   &
       wshdaaN(152), wshdaaN(153), wshdaaN(154), wshdaaN(155),   &  
       wshdaaN(156), wshdaaN(157), wshdaaN(158), wshdaaN(159),   &
       wshdaaN(160), wshdaaN(161), wshdaaN(162), wshdaaN(163),   & 
       wshdaaN(164), wshdaaN(165), wshdaaN(166), wshdaaN(167),    &  
       wshdaaN(168), wshdaaN(169), wshdaaN(170), wshdaaN(171),  &  
       wshdaaN(172), wshdaaN(173)
    
    !! Grass
       write (output_N_std_num,3400) wshdaaN(180), wshdaaN(181),wshdaaN(182),   &
       wshdaaN(183),      &
       wshdaaN(184), wshdaaN(185), wshdaaN(186), wshdaaN(187),   & 
       wshdaaN(188), wshdaaN(189), wshdaaN(190), wshdaaN(191),   & 
       wshdaaN(192), wshdaaN(193), wshdaaN(194), wshdaaN(195),   &
       wshdaaN(196), wshdaaN(197), wshdaaN(198), wshdaaN(199),   &  
       wshdaaN(200), wshdaaN(201), wshdaaN(202), wshdaaN(203),    & 
       wshdaaN(204), wshdaaN(205), wshdaaN(206), wshdaaN(207),    &
       wshdaaN(208), wshdaaN(209), wshdaaN(210), wshdaaN(211),    &
       wshdaaN(212), wshdaaN(213), wshdaaN(214), wshdaaN(215),    &
       wshdaaN(216), wshdaaN(217), wshdaaN(218), wshdaaN(219),    &
       wshdaaN(220), wshdaaN(221), wshdaaN(222), wshdaaN(223),    &
       wshdaaN(224), wshdaaN(225), wshdaaN(226), wshdaaN(227),    &
       wshdaaN(228), wshdaaN(229), wshdaaN(230), wshdaaN(231),    &
       wshdaaN(232), wshdaaN(233) 
     
      !! Wetland
       write (output_N_std_num,3500) wshdaaN(240), wshdaaN(241),wshdaaN(242),  &
       wshdaaN(243),    &
       wshdaaN(244), wshdaaN(245), wshdaaN(246), wshdaaN(247),   &
       wshdaaN(248), wshdaaN(249), wshdaaN(250), wshdaaN(251),    & 
       wshdaaN(252), wshdaaN(253), wshdaaN(254), wshdaaN(255),     &
       wshdaaN(256), wshdaaN(257), wshdaaN(258), wshdaaN(259),   &
       wshdaaN(260), wshdaaN(261), wshdaaN(262), wshdaaN(263),   &
       wshdaaN(264), wshdaaN(265), wshdaaN(266), wshdaaN(267),   &
       wshdaaN(268), wshdaaN(269), wshdaaN(270), wshdaaN(271),     &
       wshdaaN(272), wshdaaN(273), wshdaaN(274), wshdaaN(275),    & 
       wshdaaN(276), wshdaaN(277), wshdaaN(278), wshdaaN(279),    &
       wshdaaN(280), wshdaaN(281), wshdaaN(282), wshdaaN(283),    &
       wshdaaN(284), wshdaaN(285), wshdaaN(286), wshdaaN(287),     & 
       wshdaaN(288), wshdaaN(289), wshdaaN(290), wshdaaN(291),    & 
       wshdaaN(292) ,wshdaaN(293) 
     
     
      !! Agri
       write (output_N_std_num,3600) wshdaaN(300), wshdaaN(301),wshdaaN(302),   &
       wshdaaN(303),    & 
       wshdaaN(304), wshdaaN(305), wshdaaN(306), wshdaaN(307),    & 
       wshdaaN(308), wshdaaN(309), wshdaaN(310), wshdaaN(311),    & 
       wshdaaN(312), wshdaaN(313), wshdaaN(314), wshdaaN(315),   &
       wshdaaN(316), wshdaaN(317), wshdaaN(318), wshdaaN(319),    & 
       wshdaaN(320), wshdaaN(321), wshdaaN(322), wshdaaN(323),   & 
       wshdaaN(324), wshdaaN(325), wshdaaN(326), wshdaaN(327),   & 
       wshdaaN(328), wshdaaN(329), wshdaaN(330), wshdaaN(331),    &
       wshdaaN(332), wshdaaN(333), wshdaaN(334), wshdaaN(335),   &
       wshdaaN(336), wshdaaN(337), wshdaaN(338), wshdaaN(339),    &
       wshdaaN(340), wshdaaN(341), wshdaaN(342), wshdaaN(343),    & 
       wshdaaN(344), wshdaaN(345), wshdaaN(346), wshdaaN(347),   &
       wshdaaN(348), wshdaaN(349), wshdaaN(350), wshdaaN(351),   & 
       wshdaaN(352), wshdaaN(353)  
     
          
     return  
     
           
  3200 format ('LANDUSE : All   ',/,t20,     &
        'INITIAL ORG N IN SOIL =  ',f10.3,' (KG/HA)',/,t20,     &       
        'FINAL ORG N IN SOIL =  ',f12.3,' (KG/HA)',/,t20, &
         'INITIAL NO3 IN SOIL =  ',f10.3,' (KG/HA)',/,t20, &             
        'FINAL NO3 IN SOIL =  ',f12.3,' (KG/HA)',/,t20,   &                 
         'INITIAL NH4 IN SOIL=  ',f8.2,' (KG/HA)',/,t20,  &
         'FINAL NH4 IN SOIL =  ',f8.2,' (KG/HA)',/,t20,   &
         'NO3 IN FERT =  ',f8.3,' (KG/HA)',/,t20,       &               
         'AMMONIA IN FERT =  ',f8.3,' (KG/HA)',/,t20,  &                
         'ORG N IN FERT =  ',f8.3,' (KG/HA)',/,t20,     &                   
         'N FIXATION = ',f9.3,' (KG/HA)',/,t20,        &
         'NO3 IN RAINFALL =  ',f8.3,' (KG/HA)',/,t20,    &  
         'ORGANIC N =  ',f8.3,' (KG/HA)',/,t20,           &           
         'NO3 YIELD (SQ) =  ',f8.3,' (KG/HA)',/,t20,       &                   
         'NO3 YIELD (LAT) = ',f8.3,' (KG/HA)', /,t20,    &                     
         'NO3 LEACHED =  ',f8.3,' (KG/HA)',/,t20,       &                        
         'NO3 YIELD (GWQ) =  ',f8.3,' (KG/HA)',/,t20,   &
         'NO3 YIELD (TILE) = ', f8.3,' (KG/HA)',  /,t20,    &
         'N REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,     &          
         'N UPTAKE =  ',f8.3,' (KG/HA)',/,t20,    &  
         'DENITRIFICATION = ',f9.3,' (KG/HA)' ,/,t20,     &
         'AMMONIA VOLATILIZATION =  ',f8.3,' (KG/HA)',/,t20,         &     
         'AMMONIA NITRIFICATION = ',f10.3,' (KG/HA)',/,t20,       &        
         'SURF DON =  ',f8.2,' (KG/HA)',/,t20,  &                   
         'LAT DON = ',f8.2,' (KG/HA)',/,t20,   &
         'PER DON = ',f8.2,' (KG/HA)',/,t20,   &                
         'GW DON = ',f8.2,' (KG/HA)',/,t20,    &                
         'TOTAL N2O = ',f8.2,' (KG/HA)',/,t20,  &                   
         'TOTAL NO = ',f8.2,' (KG/HA)',/,t20,   &                 
         'TOTAL N2 = ',f8.2,' (KG/HA)',/,t20,   &                
         'NITR N2O = ',f8.2,' (KG/HA)',/,t20,  &                
         'NITR NO = ',f8.2,' (KG/HA)',/,t20,   &
         'GW NO3 DECAY = ',f8.2,' (KG/HA)',/,t20, &                  
         'GW NO3 REV = ',f8.2,' (KG/HA)',/,t20,    &              
         'GW NO3 SEEP = ',f8.2,' (KG/HA)',/,t20,   &
         'GW DON DECAY  = ',f8.2,' (KG/HA)',/,t20,   &
         'GW DON REV = ',f8.2,' (KG/HA)',/,t20,    &
         'GW DON SEEP = ',f8.2,' (KG/HA)',/,t20,  &                  
         'Min_ML_MBN =  ',f8.2,' (KG/HA)',/,t20,   &                  
         'Min_SL_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'Min_SL_SHN = ',f8.2,' (KG/HA)',/,t20,    &               
         'Min_MB_SHN = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,    &                 
         'Min_SH_MBN  = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_SH_PHN  = ',f8.2,' (KG/HA)',/,t20,    &               
         'Min_PH_MBN = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_ML_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SL_MBN = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_SL_SHN  = ',f8.2,' (KG/HA)',/,t20,  &               
         'IMM_MB_SHN  = ',f8.2,' (KG/HA)',/,t20,  & 
         'IMM_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,  & 
         'IMM_SH_MBN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_SH_PHN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_PH_MBN = ',f8.2,' (KG/HA)', /t20,  &                  
         'RESD N = ',f8.2,' (KG/HA)', /)                     
       

 3300 format ('LANDUSE : FORESTRY   ',/,t20,     &
        'INITIAL ORG N IN SOIL =  ',f10.3,' (KG/HA)',/,t20,     &       
        'FINAL ORG N IN SOIL =  ',f12.3,' (KG/HA)',/,t20, &
         'INITIAL NO3 IN SOIL =  ',f10.3,' (KG/HA)',/,t20, &             
        'FINAL NO3 IN SOIL =  ',f12.3,' (KG/HA)',/,t20,   &                 
         'INITIAL NH4 IN SOIL=  ',f8.2,' (KG/HA)',/,t20,  &
         'FINAL NH4 IN SOIL =  ',f8.2,' (KG/HA)',/,t20,   &
         'NO3 IN FERT =  ',f8.3,' (KG/HA)',/,t20,       &               
         'AMMONIA IN FERT =  ',f8.3,' (KG/HA)',/,t20,  &                
         'ORG N IN FERT =  ',f8.3,' (KG/HA)',/,t20,     &                   
         'N FIXATION = ',f9.3,' (KG/HA)',/,t20,        &
         'NO3 IN RAINFALL =  ',f8.3,' (KG/HA)',/,t20,    &  
         'ORGANIC N =  ',f8.3,' (KG/HA)',/,t20,           &           
         'NO3 YIELD (SQ) =  ',f8.3,' (KG/HA)',/,t20,       &                   
         'NO3 YIELD (LAT) = ',f8.3,' (KG/HA)', /,t20,    &                     
         'NO3 LEACHED =  ',f8.3,' (KG/HA)',/,t20,       &                        
         'NO3 YIELD (GWQ) =  ',f8.3,' (KG/HA)',/,t20,   &
         'NO3 YIELD (TILE) = ', f8.3,' (KG/HA)',  /,t20,    &
         'N REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,     &          
         'N UPTAKE =  ',f8.3,' (KG/HA)',/,t20,    &  
         'DENITRIFICATION = ',f9.3,' (KG/HA)' ,/,t20,     &
         'AMMONIA VOLATILIZATION =  ',f8.3,' (KG/HA)',/,t20,         &     
         'AMMONIA NITRIFICATION = ',f10.3,' (KG/HA)',/,t20,       &        
         'SURF DON =  ',f8.2,' (KG/HA)',/,t20,  &                   
         'LAT DON = ',f8.2,' (KG/HA)',/,t20,   &
         'PER DON = ',f8.2,' (KG/HA)',/,t20,   &                
         'GW DON = ',f8.2,' (KG/HA)',/,t20,    &                
         'TOTAL N2O = ',f8.2,' (KG/HA)',/,t20,  &                   
         'TOTAL NO = ',f8.2,' (KG/HA)',/,t20,   &                 
         'TOTAL N2 = ',f8.2,' (KG/HA)',/,t20,   &                
         'NITR N2O = ',f8.2,' (KG/HA)',/,t20,  &                
         'NITR NO = ',f8.2,' (KG/HA)',/,t20,   &
         'GW NO3 DECAY = ',f8.2,' (KG/HA)',/,t20, &                  
         'GW NO3 REV = ',f8.2,' (KG/HA)',/,t20,    &              
         'GW NO3 SEEP = ',f8.2,' (KG/HA)',/,t20,   &
         'GW DON DECAY  = ',f8.2,' (KG/HA)',/,t20,   &
         'GW DON REV = ',f8.2,' (KG/HA)',/,t20,    &
         'GW DON SEEP = ',f8.2,' (KG/HA)',/,t20,  &                  
         'Min_ML_MBN =  ',f8.2,' (KG/HA)',/,t20,   &                  
         'Min_SL_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'Min_SL_SHN = ',f8.2,' (KG/HA)',/,t20,    &               
         'Min_MB_SHN = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,    &                 
         'Min_SH_MBN  = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_SH_PHN  = ',f8.2,' (KG/HA)',/,t20,    &               
         'Min_PH_MBN = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_ML_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SL_MBN = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_SL_SHN  = ',f8.2,' (KG/HA)',/,t20,  &               
         'IMM_MB_SHN  = ',f8.2,' (KG/HA)',/,t20,  & 
         'IMM_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,  & 
         'IMM_SH_MBN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_SH_PHN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_PH_MBN = ',f8.2,' (KG/HA)', /t20,  &                  
         'RESD N = ',f8.2,' (KG/HA)', /)             
                                                
 3400 format ('LANDUSE : GRASS  ',/,t20,                &
         'INITIAL ORG N IN SOIL =  ',f10.3,' (KG/HA)',/,t20,     &      
         'FINAL ORG N IN SOIL =  ',f12.3,' (KG/HA)',/,t20, &
         'INITIAL NO3 IN SOIL =  ',f10.3,' (KG/HA)',/,t20,    &         
         'FINAL NO3 IN SOIL =  ',f12.3,' (KG/HA)',/,t20,    &                
         'INITIAL NH4 IN SOIL=  ',f8.2,' (KG/HA)',/,t20,  &
         'FINAL NH4 IN SOIL =  ',f8.2,' (KG/HA)',/,t20,   &
         'NO3 IN FERT =  ',f8.3,' (KG/HA)',/,t20,         &             
         'AMMONIA IN FERT =  ',f8.3,' (KG/HA)',/,t20,    &              
         'ORG N IN FERT =  ',f8.3,' (KG/HA)',/,t20,     &                   
         'N FIXATION = ',f9.3,' (KG/HA)',/,t20,  &
         'NO3 IN RAINFALL =  ',f8.3,' (KG/HA)',/,t20,   & 
         'ORGANIC N =  ',f8.3,' (KG/HA)',/,t20,         &         
         'NO3 YIELD (SQ) =  ',f8.3,' (KG/HA)',/,t20,     &              
         'NO3 YIELD (LAT) = ',f8.3,' (KG/HA)', /,t20,   &                     
         'NO3 LEACHED =  ',f8.3,' (KG/HA)',/,t20,      &                        
         'NO3 YIELD (GWQ) =  ',f8.3,' (KG/HA)',/,t20,    &
         'NO3 YIELD (TILE) = ', f8.3,' (KG/HA)',  /,t20,   &
         'N REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,    &            
         'N UPTAKE =  ',f8.3,' (KG/HA)',/,t20,      &
         'DENITRIFICATION = ',f9.3,' (KG/HA)' ,/,t20,    &
         'AMMONIA VOLATILIZATION =  ',f8.3,' (KG/HA)',/,t20,    &         
         'AMMONIA NITRIFICATION = ',f10.3,' (KG/HA)',/,t20,     &       
         'SURF DON =  ',f8.2,' (KG/HA)',/,t20,         &            
         'LAT DON = ',f8.2,' (KG/HA)',/,t20,     &
         'PER DON = ',f8.2,' (KG/HA)',/,t20,     &              
         'GW DON = ',f8.2,' (KG/HA)',/,t20,      &              
         'TOTAL N2O = ',f8.2,' (KG/HA)',/,t20,   &                  
         'TOTAL NO = ',f8.2,' (KG/HA)',/,t20,      &              
         'TOTAL N2 = ',f8.2,' (KG/HA)',/,t20,     &              
         'NITR N2O = ',f8.2,' (KG/HA)',/,t20,     &             
         'NITR NO = ',f8.2,' (KG/HA)',/,t20,   &
         'GW NO3 DECAY = ',f8.2,' (KG/HA)',/,t20,    &    
         'GW NO3 REV = ',f8.2,' (KG/HA)',/,t20,     &             
         'GW NO3 SEEP = ',f8.2,' (KG/HA)',/,t20,  & 
         'GW DON DECAY  = ',f8.2,' (KG/HA)',/,t20,   &
         'GW DON REV = ',f8.2,' (KG/HA)',/,t20,  &
         'GW DON SEEP = ',f8.2,' (KG/HA)', /,t20,   &              
         'Min_ML_MBN =  ',f8.2,' (KG/HA)',/,t20,  &                   
         'Min_SL_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'Min_SL_SHN = ',f8.2,' (KG/HA)',/,t20,      &             
         'Min_MB_SHN = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,  &                   
         'Min_SH_MBN  = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_SH_PHN  = ',f8.2,' (KG/HA)',/,t20,    &               
         'Min_PH_MBN = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_ML_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SL_MBN = ',f8.2,' (KG/HA)',/,t20,   &                
         'IMM_SL_SHN  = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_MB_SHN  = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,  & 
         'IMM_SH_MBN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_SH_PHN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_PH_MBN = ',f8.2,' (KG/HA)', /,t20, &                                
         'RESD N = ',f8.2,' (KG/HA)', /)                             
                           
 3500 format ('LANDUSE : WETLAND ',/,t20,    &
         'INITIAL ORG N IN SOIL =  ',f10.3,' (KG/HA)',/,t20,     &         
         'FINAL ORG N IN SOIL =  ',f12.3,' (KG/HA)',/,t20,  &
         'INITIAL NO3 IN SOIL =  ',f10.3,' (KG/HA)',/,t20,    &          
         'FINAL NO3 IN SOIL =  ',f12.3,' (KG/HA)',/,t20,      &               
         'INITIAL NH4 IN SOIL=  ',f8.2,' (KG/HA)',/,t20,  &
         'FINAL NH4 IN SOIL =  ',f8.2,' (KG/HA)',/,t20,   &
         'NO3 IN FERT =  ',f8.3,' (KG/HA)',/,t20,         &              
         'AMMONIA IN FERT =  ',f8.3,' (KG/HA)',/,t20,   &               
         'ORG N IN FERT =  ',f8.3,' (KG/HA)',/,t20,      &                  
         'N FIXATION = ',f9.3,' (KG/HA)',/,t20,       &
         'NO3 IN RAINFALL =  ',f8.3,' (KG/HA)',/,t20,    &   
         'ORGANIC N =  ',f8.3,' (KG/HA)',/,t20,        &            
         'NO3 YIELD (SQ) =  ',f8.3,' (KG/HA)',/,t20,     &                  
         'NO3 YIELD (LAT) = ',f8.3,' (KG/HA)', /,t20,   &                         
         'NO3 LEACHED =  ',f8.3,' (KG/HA)',/,t20,       &                        
         'NO3 YIELD (GWQ) =  ',f8.3,' (KG/HA)',/,t20,   &
         'NO3 YIELD (TILE) = ', f8.3,' (KG/HA)',  /,t20,   &
         'N REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,   &             
         'N UPTAKE =  ',f8.3,' (KG/HA)',/,t20,    &
         'DENITRIFICATION = ',f9.3,' (KG/HA)' ,/,t20,    &
         'AMMONIA VOLATILIZATION =  ',f8.3,' (KG/HA)',/,t20,   &        
         'AMMONIA NITRIFICATION = ',f10.3,' (KG/HA)',/,t20,   &          
         'SURF DON =  ',f8.2,' (KG/HA)',/,t20,     &                 
         'LAT DON = ',f8.2,' (KG/HA)',/,t20,    &
         'PER DON = ',f8.2,' (KG/HA)',/,t20,     &               
         'GW DON = ',f8.2,' (KG/HA)',/,t20,     &               
         'TOTAL N2O = ',f8.2,' (KG/HA)',/,t20,   &                   
         'TOTAL NO = ',f8.2,' (KG/HA)',/,t20,    &                  
         'TOTAL N2 = ',f8.2,' (KG/HA)',/,t20,    &                 
         'NITR N2O = ',f8.2,' (KG/HA)',/,t20,     &             
         'NITR NO = ',f8.2,' (KG/HA)',/,t20,   &
         'GW NO3 DECAY = ',f8.2,' (KG/HA)',/,t20,    &                 
         'GW NO3 REV = ',f8.2,' (KG/HA)',/,t20,   &               
         'GW NO3 SEEP = ',f8.2,' (KG/HA)',/,t20,   &
         'GW DON DECAY  = ',f8.2,' (KG/HA)',/,t20,   &  
         'GW DON REV = ',f8.2,' (KG/HA)',/,t20,  &
         'GW DON SEEP = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_ML_MBN =  ',f8.2,' (KG/HA)',/,t20,    &                    
         'Min_SL_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'Min_SL_SHN = ',f8.2,' (KG/HA)',/,t20,   &               
         'Min_MB_SHN = ',f8.2,' (KG/HA)',/,t20,     &               
         'Min_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,     &                
         'Min_SH_MBN  = ',f8.2,' (KG/HA)',/,t20,   &                 
         'Min_SH_PHN  = ',f8.2,' (KG/HA)',/,t20,   &                
         'Min_PH_MBN = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_ML_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SL_MBN = ',f8.2,' (KG/HA)',/,t20,    &               
         'IMM_SL_SHN  = ',f8.2,' (KG/HA)',/,t20,   &               
         'IMM_MB_SHN  = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SH_MBN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_SH_PHN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_PH_MBN = ',f8.2,' (KG/HA)', /,t20,  &                           
         'RESD N = ',f8.2,' (KG/HA)', /)       
                         
 3600 format ('LANDUSE : AGRICULTURE    ',/,t20,       &
         'INITIAL ORG N IN SOIL =  ',f10.3,' (KG/HA)',/,t20,  &         
         'FINAL ORG N IN SOIL =  ',f12.3,' (KG/HA)',/,t20,    &
         'INITIAL NO3 IN SOIL =  ',f10.3,' (KG/HA)',/,t20,  &           
         'FINAL NO3 IN SOIL =  ',f12.3,' (KG/HA)',/,t20,    &                 
         'INITIAL NH4 IN SOIL=  ',f8.2,' (KG/HA)',/,t20,  &
         'FINAL NH4 IN SOIL =  ',f8.2,' (KG/HA)',/,t20,   &
         'NO3 IN FERT =  ',f8.3,' (KG/HA)',/,t20,      &                
         'AMMONIA IN FERT =  ',f8.3,' (KG/HA)',/,t20,  &                
         'ORG N IN FERT =  ',f8.3,' (KG/HA)',/,t20,   &                     
         'N FIXATION = ',f9.3,' (KG/HA)',/,t20,           &
         'NO3 IN RAINFALL =  ',f8.3,' (KG/HA)',/,t20,    &
         'ORGANIC N =  ',f8.3,' (KG/HA)',/,t20,       &           
         'NO3 YIELD (SQ) =  ',f8.3,' (KG/HA)',/,t20,   &                
         'NO3 YIELD (LAT) = ',f8.3,' (KG/HA)', /,t20,   &                      
         'NO3 LEACHED =  ',f8.3,' (KG/HA)',/,t20,      &                        
         'NO3 YIELD (GWQ) =  ',f8.3,' (KG/HA)',/,t20,   &
         'NO3 YIELD (TILE) = ', f8.3,' (KG/HA)',  /,t20,    &
         'N REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,  &            
         'N UPTAKE =  ',f8.3,' (KG/HA)',/,t20,   &
         'DENITRIFICATION = ',f9.3,' (KG/HA)' ,/,t20,   &
         'AMMONIA VOLATILIZATION =  ',f8.3,' (KG/HA)',/,t20,  &         
         'AMMONIA NITRIFICATION = ',f10.3,' (KG/HA)',/,t20,  &           
         'SURF DON =  ',f8.2,' (KG/HA)',/,t20,    &                     
         'LAT DON = ',f8.2,' (KG/HA)',/,t20,   &
         'PER DON = ',f8.2,' (KG/HA)',/,t20,    &               
         'GW DON = ',f8.2,' (KG/HA)',/,t20,    &                
         'TOTAL N2O = ',f8.2,' (KG/HA)',/,t20,    &                    
         'TOTAL NO = ',f8.2,' (KG/HA)',/,t20,   &                   
         'TOTAL N2 = ',f8.2,' (KG/HA)',/,t20,    &               
         'NITR N2O = ',f8.2,' (KG/HA)',/,t20,     &             
         'NITR NO = ',f8.2,' (KG/HA)',/,t20,    &
         'GW NO3 DECAY = ',f8.2,' (KG/HA)',/,t20,   &                 
         'GW NO3 REV = ',f8.2,' (KG/HA)',/,t20,     &              
         'GW NO3 SEEP = ',f8.2,' (KG/HA)',/,t20,    & 
         'GW DON DECAY  = ',f8.2,' (KG/HA)',/,t20,  & 
         'GW DON REV = ',f8.2,' (KG/HA)',/,t20,  &
         'GW DON SEEP = ',f8.2,' (KG/HA)', /,t20,   &             
         'Min_ML_MBN =  ',f8.2,' (KG/HA)',/,t20,    &                  
         'Min_SL_MBN = ',f8.2,' (KG/HA)',/,t20,    &
         'Min_SL_SHN = ',f8.2,' (KG/HA)',/,t20,   &                
         'Min_MB_SHN = ',f8.2,' (KG/HA)',/,t20,      &              
         'Min_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,   &                  
         'Min_SH_MBN  = ',f8.2,' (KG/HA)',/,t20,    &                
         'Min_SH_PHN  = ',f8.2,' (KG/HA)',/,t20,    &               
         'Min_PH_MBN = ',f8.2,' (KG/HA)',/,t20,      &            
         'IMM_ML_MBN = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SL_MBN = ',f8.2,' (KG/HA)',/,t20,     &              
         'IMM_SL_SHN  = ',f8.2,' (KG/HA)',/,t20,    &              
         'IMM_MB_SHN  = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_MB_PHN  = ',f8.2,' (KG/HA)',/,t20,   &
         'IMM_SH_MBN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_SH_PHN = ',f8.2,' (KG/HA)',/,t20,  &
         'IMM_PH_MBN = ',f8.2,' (KG/HA)', /,t20,    &                    
         'RESD N = ',f8.2,' (KG/HA)', /)                       
                           

     end 