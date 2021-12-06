library(boot)
library(dplyr)

df <- read.csv2("bootstrap.csv" )

mean(df$Wartość.tegoroczna)
mean(df$Wartość.przyszłoroczna)
mean(df$Wartość.tegoroczna-df$Wartość.przyszłoroczna)
### seed for reproductive results
set.seed(12345)

foo <- function(data, indices){
  dt<-data[indices,]
  c(
    mean(dt[,2] - dt[,3]),
    mean(dt[,2]),
    mean(dt[,3])
  )
}

df1 <- foo(df)

myBootstrap <- boot(df, foo, R=1000)
View(myBootstrap)
head(myBootstrap$t)
View(myBootstrap$t)

head(myBootstrap$t0)
print(myBootstrap)

#zmiana
plot(myBootstrap, index=1)
boot.ci(myBootstrap, index=1)

#wartosc przyszłoroczna
plot(myBootstrap, index=3)
boot.ci(myBootstrap, index=3)









