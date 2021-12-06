
library(dplyr)
library(tidyr)

###############################################################
##### VARS.TO.CASES on two variables <- Radek's Version

df <- read.csv2("Source.Dafafile.Lesson04.csv")


df <- select(df, RecordNo, Q7M1:Q8M10)
#df <- select(df, 'RecordNo', starts_with('Q7M'), starts_with('Q8M'))



df <- pivot_longer(df,
                   cols = matches("Q[78]M")
                   ,names_to = c("Q","index")
                   ,names_pattern = "(Q7|Q8)M(\\d+)"
)


df <- mutate(df, index=as.numeric(index))

df <- mutate(df, value = if_else(index==10, 999999, as.numeric(value)))

df <- pivot_wider(df
                  ,names_from = Q
                  ,values_from = value
)

View(df)

source("AddLabelsScript(mod).R")

df$Q7 <- AddMyLabels(df$Q7)
df$Q8 <- AddMyLabels(df$Q8)

