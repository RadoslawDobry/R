library(boot)
library(dplyr)

df <- read.csv2("bootstrap.csv")

mean(df$Wartość.tegoroczna)
mean(df$Wartość.przyszłoroczna)
mean(df$Wartość.tegoroczna - df$Wartość.przyszłoroczna)

df$change <- (df[, 3] - df[, 2])
### seed for reproductive results
set.seed(12345)

foo <- function(data, indices) {
  dt <- data[indices, ]
  c(mean(dt[, 6]))
}


myBootstrap <- boot(df, foo, R = 1000)
View(myBootstrap)
head(myBootstrap$t)
View(myBootstrap$t0)

head(myBootstrap$t0)
print(myBootstrap)

#zmiana
plot(myBootstrap, index = 1)
ci <- boot.ci(myBootstrap, index = 1)



mean(df$change)
