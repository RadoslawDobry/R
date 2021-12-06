library(plyr)
library(psych)
library(dplyr)
library(tidyr)

df <- read.csv2("Source.Dafafile.Lesson04.csv")
#wybór kolumn
df <- select(df, RecordNo, Q7M1:Q8M10)


df <-
  pivot_longer(
    df,
    cols = matches("Q[7,8]M") ,
    names_to = c("Q", "index"),
    names_pattern = ("(Q7|Q8)M(\\d)")
  )

df <- mutate(df, index=as.numeric(index))

df <- mutate(df, value = if_else(index==10, 999999, as.numeric(value)))

df <- pivot_wider(df, names_from = Q, values_from = value)

