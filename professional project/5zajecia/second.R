library(plyr)
library(psych)
library(dplyr)
library(tidyr)
library (lubridate)
library(imputeTS)

first <- read.csv2("Second.Dataset.csv")

df <- pivot_longer(first, 2:11, names_to = "name", values_drop_na = T )
df <- filter(df, value !="")
df1 <- pivot_wider(df)

#zamiana wartości
df1 <- na.replace(df1, fill=0)
df1 <- mutate_all(df1 = coalesce(df1,0)
summary(df1)                  


second <- read.csv2("Second.DataSet.csv", encoding = 'UTF-8' )
colnames(second)[1] <- "RecordNo"

second_piv <- pivot_longer(second, 2:11, names_to ='name', values_drop_na = TRUE)
second_piv <- (filter( second_piv, value != ""))
second_piv <- select(second_piv, -name)
second_piv <- mutate(second_piv, dummy = 1)                  

data <- pivot_wider(second_piv,
                    values_from = 'dummy',
                    names_from = 'value')
data <- mutate_all(data, coalesce,  0)

data <- data.frame(Freq = colSums(data[2:12]),
                     Pct.of.Resp =(colSums(data[2:12]/sum(data[2:12])) *100),
                     Pct.of.cases = (colSums(data[2:12]/nrow(data[2:12])) *100))%>%
  arrange(., desc(Pct.of.cases))

barplot(data$Pct.of.cases, names.arg = row.names(data),
        main = "% of cases", xlab = "firma", ylab = "%", col = "blue")


                  