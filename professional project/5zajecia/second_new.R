library(plyr)
library(psych)
library(dplyr)
library(tidyr)
library (lubridate)
library(imputeTS)

new <- read.csv2("Second.DataSet.csv", encoding = 'UTF-8')
colnames(new)[1] <- "RecordNo"

new <- new %>%
  pivot_longer(., 2:11, names_to = 'name', values_drop_na = TRUE) %>%
  filter(., value != "") %>%
  select(., -name) %>%
  mutate(., dummy = 1) %>%
  pivot_wider(.,
              values_from = 'dummy',
              names_from = 'value') %>%
  mutate_all(., coalesce,  0)

new <- data.frame(
  Freq = colSums(new[2:12]),
  Pct.of.Resp = (colSums(new[2:12] / sum(new[2:12])) * 100),
  Pct.of.cases = (colSums(new[2:12] / nrow(new[2:12])) *100)) %>%
  arrange(., desc(Pct.of.cases))

barplot(
  new$Pct.of.cases,
  names.arg = row.names(new),
  main = "% of cases",
  xlab = "firma",
  ylab = "%",
  col = "blue"
)
