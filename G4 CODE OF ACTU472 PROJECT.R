A<-0.000023
B<-0.000028
C<-0.000043
a<-0.13
b<-.11
S<-130000
x<- 30
d <- -0.06
t <- seq(0,20)
#Before running the code a package of the name “ bolstad2” should be installed and called for .
#ANSWER OF Q_1 
APVFB1Q1 <- function (t){(exp(d*t))*(exp(((-C*exp(b*x)*((exp(b*t))-1))/b)-(((B*exp(a*x)*(exp(a*t)-1))/a))-A*t))*(C*exp(b*(x+t)))} 
APVFB2Q1 <- function (t){(exp(d*t))*((exp((B*exp(a*x+a*t))/a+A*t)-exp((B*exp(a*x))/a))*exp((C*exp(b*x)-C*exp(b*x+b*t))/b-(B*exp(a*x+a*t))/a-A*t))*(C*exp(b*(x+t)))}
ABVFB1SQ1 <- sintegral(seq(0,20),APVFB1Q1(t),100)  
ABVFB2SQ1 <- sintegral(seq(0,20),APVFB2Q1(t),100)
ABVFBQ1 <- (ABVFB1SQ1$int+ABVFB2SQ1$int)*S
ABVFBQ1

AnnuityQ1 = 0
for(i in 0:239){
  current = exp(-0.005*i)*exp(-((C*exp(b*(i/12+x))-C*exp(x*b))/b+(B*exp(a*(i/12+x))-B*exp(x*a))/a+A*i/12))  # Update variable storing sum
  AnnuityQ1 = AnnuityQ1 + current
}
Mthlyannuity=1/12*AnnuityQ1 
ExpencesandAPVFP <- 12*0.9*Mthlyannuity+0.1-0.6
Premium <- ABVFBQ1/ExpencesandAPVFP
Premium
###END


#ANSWER OF Q_2

Vh0 <-function (h){
  if (h<20){
    #ACTURIAL PRESENT VALUE OF FUTURE BENFITS FOR RESERVE
    m <- seq(0,20-h) 
    c=x+h
    APVFB1Q2<-function(m){(exp(d*m))*(exp(((-C*exp(b*c))*(((exp(b*m))-1))/b)-(((B*exp(a*c)*(exp(a*m)-1))/a))-A*m))*(C*exp(b*(c+m)))}
    APVFB2Q2 <- function (m){(exp(d*m))*((exp((B*exp(a*c+a*m))/a+A*m)-exp((B*exp(a*c))/a))*exp((C*exp(b*c)-C*exp(b*c+b*m))/b-(B*exp(a*c+a*m))/a-A*m))*(C*exp(b*(c+m)))}
    
    ABVFB1SQ2<- sintegral(m,APVFB1Q2 (m),100)  
    APVFB2SQ2<- sintegral(m, APVFB2Q2 (m),100) 
    
    #ACTURIAL PRESENT VALUE OF FUTURE PREMIMS FOR RESERVE
    
    AnnuityQ2=0
    v=((20-h)*12)-1
    for(i in 0:v){
      APVFPQ2_value<-exp((d/12)*i)*exp(-((C*exp(b*(i/12+c))-C*exp(c*b))/b+(B*exp(a*(i/12+c))-B*exp(c*a))/a+A*i/12))
      AnnuityQ2= AnnuityQ2+ APVFPQ2_value  } 
    
    APVFPQ2<-Premium* AnnuityQ2
    
    #ACTURIAL PRESENT VALUE OF FUTURE EXPENSES FOR RESERVE
    
    APVFexpensesQ2<- 0.1*Premium*AnnuityQ2
    
    
    Vh0=S*( ABVFB1SQ2$int+ APVFB2SQ2$int)+ APVFexpensesQ2- APVFPQ2 
    
  }
  else {
    Vh0<-0
    
  }
  return (Vh0)
}
Vh0(5)
Vh0(10)
Vh0(15)
###END

#ANSWER OF Q_3
APVFBRQ3 = rep(NA,3)
for (i in seq(5,15, by=5) ){
  y <- seq(0,20-i)
  x <- 30 +i
  APVFBQ3 <- function (y){exp(-0.06*y)*exp(-(C*(exp(b*y)-1)*exp(b*x))/b)*(C*exp(b*(x+y)))}
  ABVFBSQ3 <- sintegral(seq(0,20-i),APVFBQ3(y),100)
  APVFBRQ3[i/5] <- ABVFBSQ3$int
}
RESERVEQ3 <- S*APVFBRQ3
RESERVEQ3
