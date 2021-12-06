### bootstrap library
install.packages("boot")
library(boot)

### exemplary data
install.packages("datasets")
library(datasets)

### exploring dataset
data(iris)
head(iris)
View(iris)

#
median(iris$Sepal.Length)
median(iris$Sepal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width, method ="s")

### let's make estimating function 
foo <- function(data, indices, cor.type){
  dt<-data[indices,]
  c(
    cor(dt[,1], dt[,2], method=cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}

### seed for reproductive results
set.seed(12345)

### bootstrapping process
myBootstrap <- boot(iris, foo, R=1000, cor.type='s')

### inspecting results
View(myBootstrap)
#
head(myBootstrap$t)
View(myBootstrap$t)
#
head(myBootstrap$t0)
#
print(myBootstrap)


### correlation coefficient
# normal distribution (->Basic)
plot(myBootstrap, index=1)
boot.ci(myBootstrap, index=1)

#rozkład jest normalny, współczynnik ufności wynosi (-0.3142, -0.0253 )
# w populacji korelacja pomiędzy dwoma zmiennymi jest niewielka
### median Sepal.Width

# "unusual" distribution (->Bias-corrected, accelerated)
plot(myBootstrap, index=3)
boot.ci(myBootstrap, index=3)

# eof


