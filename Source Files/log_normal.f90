      subroutine log_normal(mu,sig,c)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function generates a random number from a lognormal distribution curve
!!    for estimating constituent concentration in the effluent of urban bmps 
!!    given mean and standard deviation values.
!!    Jaehak Jeong, 2017       

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mu          |mg/l          |mean value
!!    sig         |mg/l          |standard deviation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |mg/l          |value generated for distribution
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real, intent(in) :: mu,sig                 !!R669 6/20/18 nbs     
      real, intent(out):: c                      !!R669 6/20/18 nbs
      real :: temp(2),r,theta                   !!R669 6/20/18 nbs
      real,PARAMETER :: PI=3.141592653589793238462    !!R669 6/20/18 nbs 
      CALL RANDOM_NUMBER(temp)
      r = (-2.0d0*log(temp(1)))**0.5
      theta = 2.0d0*PI*temp(2)
      c = mu+sig*r*sin(theta)
      c = exp(c)
      end