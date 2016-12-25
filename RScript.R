library(ggplot2)
# 輸入
X <- read.table("../MRT_201512.csv",sep=",",header=T,encoding="ANSI")
X
# 加總成2015年12月各站的總人數
Y <- apply(X[,2:83],2,sum)

# 轉成data.frame
Z <- data.frame(station=names(Y),sum=Y)

# 視覺化
#str(Z)
ggplot(Z, aes(x=station, y=sum)) + geom_bar(stat="identity", position="identity", width=0.1,fill = "blue")
# 
setwd("c:/")
A10 <- read.table(file="grade.csv",header=TRUE,sep=",",encoding="Big5")
A10
mode(A10)
A10 <- as.matrix(A10)
NoHeader.A10 <- matrix(A10, ncol = ncol(A10), dimnames = NULL)
NoHeader.A10
mode(NoHeader.A10)
X=A10[,2]
Y=A10[,3]
cor(Y,X)

Lm_model <- lm(Y ~ X)
Lm_model
summary(Lm_model)
# 畫圖
plot(X,Y)
# 加線
abline(lm(Y ~ X))
par(col)
cf <- coef(lm(Y ~ X)) 
cf 
# 帶數值去預測
# lm_function <- function(x) {y <- cf[1]+cf[2]*x; return (y) } 
# sapply(X,lm_function)
