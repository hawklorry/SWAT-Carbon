subroutine  Lin_Sys(a11, a12, a21, a22, b1, b2, x1, x2)
real::a11, a12, a21, a22, b1, b2, x1, x2
!'     This subroutine solves a linear system of 2 equations and 2 unknowns
      
x1 = (a22 * b1 - a12 * b2) / (a11 * a22 - a12 * a21)
x2 = (a11 * b2 - a21 * b1) / (a11 * a22 - a12 * a21)

Return
End 