      subroutine writeaa_R
      use parm
        use parm_subC
        use parm_subE
        use parm_subH
        use parm_rchC
        use parm_rchE
        use parm_control
        use parm_output
      implicit none
      
      real :: yrs, xx
      integer :: j

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
      
  
     !!=================================
     !  rchaao2 = rchaao2 / yrs 
         do j = 1, subtot
      rchaao2(1,j) = rchaao2(1,j) / yrs  
      rchaao2(2,j) = rchaao2(2,j) / yrs  
      rchaao2(3,j) = rchaao2(3,j) / yrs  
      rchaao2(4,j) = rchaao2(4,j) / yrs  
      rchaao2(5,j) = rchaao2(5,j) / yrs  
      rchaao2(6,j) = rchaao2(6,j) / yrs  
      rchaao2(7,j) = rchaao2(7,j) / yrs  
      rchaao2(8,j) = rchaao2(8,j) / yrs  
      rchaao2(9,j) = rchaao2(9,j) / yrs  
      rchaao2(10,j) = rchaao2(10,j) / yrs  
      rchaao2(11,j) = rchaao2(11,j)/ yrs  
      rchaao2(12,j) = rchaao2(12,j) / yrs  
      rchaao2(13,j) = rchaao2(13,j) / yrs  
      rchaao2(14,j) = rchaao2(14,j) / yrs  
      rchaao2(15,j) = rchaao2(15,j) / yrs  
      rchaao2(16,j) = rchaao2(16,j) / yrs  
      rchaao2(17,j) = rchaao2(17,j) / yrs  
      rchaao2(18,j) = rchaao2(18,j)/ yrs  
      rchaao2(19,j) = 0. 
      rchaao2(20,j) = 0.  
      rchaao2(21,j) = 0. 
      rchaao2(22,j) = 0. 
      rchaao2(23,j) = 0. 
      rchaao2(24,j) = 0.
      rchaao2(25,j) = 0.  
      rchaao2(26,j) =  rchaao2(26,j)                                                        !!RPOC kg  last day of the last year
      rchaao2(27,j) =  rchaao2(27,j)                                                        !!LPOC kg  last day of the last year
      rchaao2(28,j) =  rchaao2(28,j)                                                        !!RDOC kg  last day of the last year
      rchaao2(29,j) =  rchaao2(29,j)                                                        !!LDOC kg  last day of the last  year
      rchaao2(30,j) =  rchaao2(30,j)                                                        !!DIC kg  last day of the last year
      rchaao2(31,j) =  rchaao2(31,j)                                                        !!Floating Algea kg  last day of the last year
      rchaao2(32,j) =  rchaao2(32,j)                                                        !!Bottom Algea kg  last day of the last year
      rchaao2(33,j) = rchaao2(33,j) / yrs  
      rchaao2(34,j) = rchaao2(34,j) / yrs  
      rchaao2(35,j) = rchaao2(35,j) / yrs  
      rchaao2(36,j) = rchaao2(36,j) / yrs  
      rchaao2(37,j) = rchaao2(37,j) / yrs  
      rchaao2(38,j) = rchaao2(38,j) / yrs  
      rchaao2(39,j) = rchaao2(39,j) / yrs  
      rchaao2(40,j) = rchaao2(40,j) / yrs  
      rchaao2(41,j) = rchaao2(41,j) / yrs  
      rchaao2(42,j) = rchaao2(42,j)/ yrs  
      rchaao2(43,j) = rchaao2(43,j) / yrs  
      rchaao2(44,j) = rchaao2(44,j) / yrs  
      rchaao2(45,j) = rchaao2(45,j) / yrs  
      rchaao2(46,j) = rchaao2(46,j) / yrs  
      rchaao2(47,j) = rchaao2(47,j) / yrs  
      rchaao2(48,j) = rchaao2(48,j) / yrs  
      rchaao2(49,j) = rchaao2(49,j) / yrs  
      rchaao2(50,j) = rchaao2(50,j) / yrs  
      rchaao2(51,j) = rchaao2(51,j) / yrs  
      rchaao2(52,j) = rchaao2(52,j) / yrs  
      rchaao2(53,j) = rchaao2(53,j) / yrs  
      rchaao2(54,j) = rchaao2(54,j) / yrs  
      rchaao2(55,j) = rchaao2(55,j) / yrs  
      rchaao2(56,j) = rchaao2(56,j)/ yrs  
      rchaao2(57,j) = rchaao2(57,j) / yrs  
      rchaao2(58,j) = rchaao2(58,j) / yrs  
      rchaao2(59,j) = rchaao2(59,j) / yrs  
      rchaao2(60,j) = rchaao2(60,j) / yrs  
      rchaao2(61,j) = rchaao2(61,j) / yrs  
      rchaao2(62,j) = rchaao2(62,j) / yrs  
      rchaao2(63,j) = rchaao2(63,j) / yrs  
      rchaao2(64,j) = rchaao2(64,j) / yrs  
      rchaao2(65,j) = rchaao2(65,j) / yrs  
      rchaao2(66,j) = rchaao2(66,j) / yrs  
      rchaao2(67,j) = rchaao2(67,j)/ yrs  
      rchaao2(68,j) = rchaao2(68,j) / yrs  
      rchaao2(69,j) = rchaao2(69,j) / yrs  
      rchaao2(70,j) = rchaao2(70,j)/ yrs  
      rchaao2(71,j) = rchaao2(71,j)/ yrs  
      rchaao2(72,j) = 0.
      rchaao2(73,j) =  rchaao2(73,j)                                                  !!Sediment C kg  last day of the last year
      rchaao2(74,j) = rchaao2(74,j) / yrs  
      rchaao2(75,j) = rchaao2(75,j) / yrs    
      rchaao2(76,j) =  rchaao2(76,j)                                                  !!Sediment C kg  last day of the last year
      rchaao2(77,j) =  rchaao2(77,j)                                                  !!Bury C kg  last day of the last year
      rchaao2(78,j) = rchaao2(78,j) / yrs  
      rchaao2(79,j) = rchaao2(79,j) / yrs  
      rchaao2(80,j) = rchaao2(80,j)/ yrs  
      rchaao2(81,j) = rchaao2(81,j) / yrs  
      rchaao2(82,j) = rchaao2(82,j) / yrs  
      rchaao2(83,j) = rchaao2(83,j) / yrs  
      rchaao2(84,j) = rchaao2(84,j) / yrs  
      rchaao2(85,j) = rchaao2(85,j) / yrs  
      rchaao2(86,j) = rchaao2(86,j) / yrs  
      rchaao2(87,j) = rchaao2(87,j) / yrs  
      rchaao2(88,j) = rchaao2(88,j)/ yrs  
      rchaao2(89,j) = rchaao2(89,j) / yrs  
      rchaao2(90,j) = rchaao2(90,j) / yrs  
      rchaao2(91,j) = rchaao2(91,j) / yrs  
      rchaao2(92,j) = rchaao2(92,j) / yrs  
      rchaao2(93,j) = rchaao2(93,j) / yrs  
      rchaao2(94,j) = rchaao2(94,j)/ yrs  
      rchaao2(95,j) = rchaao2(95,j) / yrs  
      rchaao2(96,j) = rchaao2(96,j) / yrs  
    
    
 
        
      end do
      !!=======================
      
      
      
     
        if (iprint /= 1) then
 
        !! write average annual output--reach (.rch)
    
        if(cswat== 2 .and.  rch2==1) then
            if (iprint /= 3) call rchaa2(yrs)   
        end if
       
        end if
         
         
         
         return
         end