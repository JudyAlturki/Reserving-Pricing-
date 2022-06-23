A <- 0.000023
B <- 0.000028
C <- 0.000043
a <- 0.13
b <- 0.11
S <- 130000
t <- seq(0,20)
x<-30
premium_delta=rep(NA,10)
for(i in seq(0,0.1,by=0.001)){

APVFB1 <- function (t){(exp(-i*t))*(exp(((-C*exp(b*x)*((exp(b*t))-1))/b)-(((B*exp(a*x)*(exp(a*t)-1))/a))-A*t))*(C*exp(b*(x+t)))} 
APVFB2 <- function (t){(exp(-i*t))*((exp((B*exp(a*x+a*t))/a+A*t)-exp((B*exp(a*x))/a))*exp((C*exp(b*x)-C*exp(b*x+b*t))/b-(B*exp(a*x+a*t))/a-A*t))*(C*exp(b*(x+t)))}
ABVFB1S <- sintegral(seq(0,20),APVFB1(t),100)  
ABVFB2S <- sintegral(seq(0,20),APVFB2(t),100)
ABVFB <- (ABVFB1S$int+ABVFB2S$int)*S
ABVFB
Annuity = 0
for(j in 0:239){
  current = exp(-j*i/12)*exp(-((C*exp(b*(j/12+x))-C*exp(x*b))/b+(B*exp(a*(j/12+x))-B*exp(x*a))/a+A*j/12))  # Update variable storing sum
  Annuity = Annuity + current
}
Mthlyannuity=1/12*Annuity 
ExpencesandAPVFP<- 12*0.9*Mthlyannuity+0.1-0.6
Premium <- ABVFB/ExpencesandAPVFP
premium_delta[i*1000]=Premium

}
forceofintrest<- seq(0.001,0.1,by=0.001)
plot(forceofintrest,premium_delta,type="l")

premium_age=rep(NA,10)
for(i in seq(0,100,by=5)){
  APVFB1 <- function (t){(exp(-0.06*t))*(exp(((-C*exp(b*i)*((exp(b*t))-1))/b)-(((B*exp(a*i)*(exp(a*t)-1))/a))-A*t))*(C*exp(b*(i+t)))} 
  APVFB2 <- function (t){(exp(-0.06*t))*((exp((B*exp(a*i+a*t))/a+A*t)-exp((B*exp(a*i))/a))*exp((C*exp(b*i)-C*exp(b*i+b*t))/b-(B*exp(a*i+a*t))/a-A*t))*(C*exp(b*(i+t)))}
  ABVFB1S <- sintegral(seq(0,20),APVFB1(t),100)  
  ABVFB2S <- sintegral(seq(0,20),APVFB2(t),100)
  ABVFB <- (ABVFB1S$int+ABVFB2S$int)*S
  ABVFB
  Annuity = 0
  for(j in 0:239){
    current = exp(-j*0.06/12)*exp(-((C*exp(b*(j/12+i))-C*exp(i*b))/b+(B*exp(a*(j/12+i))-B*exp(i*a))/a+A*j/12))  # Update variable storing sum
    Annuity = Annuity + current
  }
  Mthlyannuity=1/12*Annuity 
  ExpencesandAPVFP<- 12*0.9*Mthlyannuity+0.1-0.6
  Premium <- ABVFB/ExpencesandAPVFP
  premium_age[i/5]=Premium
 
}
premium_age

age <- seq(5,100,by=5)
plot(age,premium_age,type="l")
