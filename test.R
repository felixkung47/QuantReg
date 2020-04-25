#引入分量迴歸套件
library(quantreg)
#讀入txt檔案
gdp<-read.table("data.txt", header=TRUE)
#資料特性
str(gdp) 
#前5筆資料
head(gdp,5)
#後5筆資料
tail(gdp,5)
#設定變數與模型，忽略Warnings
taus <- seq(0.05,0.95,0.01)
para <- matrix(0,length(taus),9)
sd <- matrix(0,length(taus),9)
tsM <- matrix(0,length(taus),9)
sig <- matrix(0,length(taus),9)
suppressWarnings(
  for (i in 1:length(taus)){
    sum <- as.matrix(coef(summary(rq(r~t2+t2sq+r1+r2+r3+lnv1+lnv2+lnv3,tau=taus[i],data=gdp))))
    para[i,] <- sum[,1]
    sd[i,] <- sum[,2]
    tsM[i,] <- sum[,3]
    sig[i,] <- sum[,4]
  }
)
# Coefficients of indepedent variables#
round(cbind(taus,para),digits=3)
# Standard error of Coefficients of indepedent variables#
round(cbind(taus,sd),digits=3)
# t-student statistics of coefficient of indepedent variables#
round(cbind(taus,tsM),digits=3) 
# P-Value of t student statistics of coefficient of indepedent variables#
round(cbind(taus,sig),digits=3) 
#繪圖，並單獨印出lnv1,lnv2,lnv3圖形
om1<-suppressWarnings(summary(rq(r~t2+t2sq+r1+r2+r3+lnv1+lnv2+lnv3,tau = seq(0.05, 0.95, by = 0.01),data=gdp)))
plot(om1)
for (ii in 7:9){
  plot(om1, parm = ii)
}