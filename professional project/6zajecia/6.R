library(plyr)
library(psych)
library(dplyr)
library(tidyr)

df <- read.csv2("Source.Dafafile.Lesson04.csv")
MyFrame <- subset(df, select = c("RecordNo", "Q5", "Q6"))

new_cols <- paste("Index", 1:20, sep = "")

MyFrame[, new_cols] <- NA

MyFrame <-
  gather(
    MyFrame,
    key = "Index",
    value = "Dummy",
    Index1:Index20,
    factor_key = TRUE
  )

MyFrame <- mutate(MyFrame, Index = as.numeric(Index))

##################################3

TopofMind <- c("Q7M1")
df1 = subset(df, select = c("RecordNo", TopofMind))
df1$Index = 1

df1 <- rename(df1, TopofMind = Q7M1)
#################################3
MyFrame <- left_join(MyFrame, df1, by = c("RecordNo", "Index"))

#################################3
Unaided <-
  c("Q7M1",
    "Q7M2",
    "Q7M3",
    "Q7M4",
    "Q7M5",
    "Q7M6",
    "Q7M7",
    "Q7M8",
    "Q7M9",
    "Q7M10")

df2 = subset(df, select = c("RecordNo", Unaided))

df2 <-
  gather(df2,
         key = "Index",
         value = "Unaided",
         Q7M1:Q7M10,
         factor_key = TRUE)
df2 <- mutate(df2, Index = as.numeric(Index))

MyFrame <- left_join(MyFrame, df2, by = c("RecordNo", "Index"))
#################################
aided <-
  c("Q8M1",
    "Q8M2",
    "Q8M3",
    "Q8M4",
    "Q8M5",
    "Q8M6",
    "Q8M7",
    "Q8M8",
    "Q8M9",
    "Q8M10")

df3 = subset(df, select = c("RecordNo", aided))

df3 <-
  gather(df3,
         key = "Index",
         value = "Aided",
         Q8M1:Q8M10,
         factor_key = TRUE)
df3 <- mutate(df3, Index = as.numeric(Index))

MyFrame <- left_join(MyFrame, df3, by = c("RecordNo", "Index"))
#####################################
total <-
  c(
    "Q7M1",
    "Q7M2",
    "Q7M3",
    "Q7M4",
    "Q7M5",
    "Q7M6",
    "Q7M7",
    "Q7M8",
    "Q7M9",
    "Q7M10",
    "Q8M1",
    "Q8M2",
    "Q8M3",
    "Q8M4",
    "Q8M5",
    "Q8M6",
    "Q8M7",
    "Q8M8",
    "Q8M9",
    "Q8M10"
  )

df4 = subset(df, select = c("RecordNo", total))

df4 <-
  gather(df4,
         key = "Index",
         value = "TotalAwarness",
         Q7M1:Q8M10,
         factor_key = TRUE)

df4 <- mutate(df4, Index = as.numeric(Index))

MyFrame <- left_join(MyFrame, df4, by = c("RecordNo", "Index"))

###########etykiety
#MyFrame  <- MyFrame %>%
#  mutate_at(., vars(Unaided, Aided), funs(101 = "Acme"))

#MyFrame <- mutate(MyFrame, TopofMind = if_else(Index == 20,999999, as.numeric(TopofMind)))
#MyFrame <- mutate(MyFrame, Unaided = if_else(Index == 20,999999, as.numeric(Unaided)))

final <- c("TopofMind", "Unaided", "Aided", "TotalAwarness")

MyFrame <- MyFrame %>%
  mutate_at(final, ~ if_else(Index == 20, 999999, as.numeric(.)))

#my_func <- function(Frame, column){
#  return(mutate(MyFrame, column = if_else(Index==20, 99999, as.numeric(column))))
#}

MyFrame <- MyFrame %>%
  mutate_at(
    final,
    factor,
    levels = c(
      "101",
      "102",
      "103",
      "104",
      "105",
      "106",
      "107",
      "108",
      "111",
      "112",
      "113",
      "114",
      "115",
      "116",
      "117",
      "118",
      "119",
      "998",
      "999",
      "999999"
    ),
    labels = c(
      "Acme",
      "Central Perk",
      "Cyberdyne",
      "Weyland-Yutani",
      "Tyrell Corporation",
      "Red Apple",
      "Monsters Inc.",
      "Full House",
      "MomCorp",
      "Rich Industries",
      "Warbucks Industries",
      "Wayne Enterprises",
      "Globex",
      "Wonka Industries",
      "Oceanic Airlines",
      "Clampett Oil",
      "Gringotts",
      "brak odpowiedzi",
      "nie wiem",
      "TOTAL"
    )
  )
write.csv2(MyFrame,
           "brand_awarness_results.csv",
           row.names = F,
           na = "")

