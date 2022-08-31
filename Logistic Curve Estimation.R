library(readxl)
library(dplyr)
a <- read_excel("C:/Users/Onur/OneDrive/Masaüstü/a.xlsx")
View(a)
a$S <- 500
a$yhat<- log((500/(a$CO))-1,base = exp(1))#ln(s/co-1)#
a$LG <- log(a$GNPPC,base = exp(1))#ln(GNPPC)#


# Logistics Regression

glm<- glm(yhat ~ Year + GNPPC , data = a)
summary(glm)


#yhat = 6.544e+01 -3.097e-02t -1.336e-01LG#
# a = e^a* = e^(1.094e+02) 
# b = -b*  = 5.372e-02
# c =  c   = -6.323e-06

co_est <- function(x,y){
  a1 <- exp(1.094e+02)
  a2 <- 5.372e-02
  a3 <- -6.323e-06
  (500/(1+(a1*(exp(-a2*y+a3*log(x,base = exp(1)))))))
}

a$co_est <- co_est(a$GNPPC,a$Year)

