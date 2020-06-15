setwd("C:\\Users\\17246\\Desktop")
M1SL <- read.table("M1SL.csv", header=TRUE,  sep=",")
GS10 <- read.table("GS10.csv", header=TRUE, sep=",")
#省略2000年以前的数据
M1SL2000 <- 0
GS102000 <- 0
#计算某个日期所在的位置
findDateIndex <- function(dateTable, requireDate)
{
  index <- 0
  requireDateIndex <- 0
  for(i in 1:length(dateTable))
    {
      index <- index + 1
      if(dateTable[i] == requireDate)
        requireDateIndex <- index
  }
  return (requireDateIndex)
}
M1SL2000 <- findDateIndex(M1SL$DATE, "2000-01-01")
GS102000 <- findDateIndex(GS10$DATE, "2000-01-01")
y <- M1SL[M1SL2000:nrow(M1SL), ]#z,y为截取的2000年以来的数据框
z <- GS10[GS102000:nrow(GS10), ]
GS10 <- z[,2]
#将两个数据序列放到同一张表中
newTable <- cbind(y, GS10)

#将M1货币供给转换为M1增长率
#tempM1SL <-  newTable[1, 2]#tempM1SL保存当前的M1SL数值
beforeM1SL <- newTable[1, 2] #保存上一个M1SL的数值
#print(tempM1SL)
rateOfincrease <- 0 #保存增长率
newTable[1, 2] <- 0 #新表第一行的M1SL增长率为0
for(i in 2:nrow(newTable))
{
  rateOfincrease <- round(((newTable[i, 2] - beforeM1SL) / beforeM1SL)*100,2)
  beforeM1SL <- newTable[i, 2]
  newTable[i, 2] <- rateOfincrease
}
#修改M1增长率的标签名为"较上年变动的百分比"
names(newTable) <- c("日期","较上年变动的百分比", "GS10")

#画折线图
#par(mfrow=c(1,2))
newTable1 <- newTable
newTable1$日期 <- factor(newTable1$日期, ordered=TRUE)
#使用plot绘制一个空图
plot(c(newTable1$日期[1],newTable1$日期[length(newTable$日期)]),
     c(-5, 15),type="n", xlab="日期",ylab="数值", main="从2000年开始每个月的两项指标的折线图")

#绘制较上年变动的百分比的折线
lines(newTable1$日期, newTable$较上年变动的百分比, col="red")
lines(newTable1$日期, newTable$GS10, col="blue")
#添加图例
legend("topleft",inset=c(0.05,0.05), cex=0.8,bty="n",
       lty=c(1,1),c("M1货币供给较上年变动的百分比","10年期政府债券利率"),col=c("red","blue"))
