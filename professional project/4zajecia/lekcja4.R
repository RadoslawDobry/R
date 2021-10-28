library(plyr)
library(psych)
library(dplyr)
library(tidyr)
library (lubridate)

regional <- read.csv2("Regional.Structure.csv")
region <- read.csv2("RegionC.csv", encoding = "UTF-8")
DataFile <- read.csv2("Zajecia03.Source.Dafafile.Lesson02.csv")


dim(DataFile)


colnames(regional)[1] <- ("RegionA")
colnames(region)[1] <- ("RegionC")

DataFile <- left_join(DataFile,regional, by="RegionA")
DataFile <- left_join(DataFile, region, by="RegionC")


DataFile<-mutate(DataFile, RQ2= case_when(
  Q2<=25 ~ 11,
  between(Q2,25.01,50) ~ 12,
  between(Q2,50.01,100) ~13,
  between(Q2,100.01,250) ~14,
  Q2>250.01 ~ 15
))


keeps <- (c("RecordNo", "Województwo", 'RQ2', "Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6", "Q3_7", "Q4", "Q5", "Q6", "Q2"))
DataFile = DataFile[keeps]
 
DataFile$RQ2 = factor(DataFile$RQ2,
                      levels = c(11,12,13,14,15),
                      labels = c(">25","25-50","51-100","100-250","250+"))
 
b<- gather(DataFile,"company","recomendation","Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6", "Q3_7", factor_key = TRUE)
 
 
b$company = factor(b$company,
                   levels = c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6", "Q3_7"),
                   labels = c("Acme", "Central Park", "Cyberdyne", "Weyland-Yutani", "Tyrell Corporation","Red Apple", "Monsters Inc."))
 
 
b$Q5 = factor(b$Q5,
                   levels = c("101", "102", "103", "104", "994", "999"),
                   labels = c("Wyższe rolnicze", "Wyższe","Średnie technicznezne", "Średnie ogólne", "Inne","Nie wiem / brak odpowiedzi"))
 
 
 
 
b <- filter(b, recomendation != 99)
 
 
b <- mutate(b, Age = year(Sys.Date()) - Q4)
 
b <-mutate(b, Age = case_when(
  Age<=25 ~ 21,
  between(Age,25.01,30) ~ 22,
  between(Age,30.01,35) ~23,
  between(Age,35.01,40) ~24,
  between(Age,40.01,45) ~25,
  between(Age,45.01,50) ~26,
  between(Age,50.01,250) ~27
))

b$Age = factor(b$Age,
              levels = c(21,22,23,24,25,26,27),
              labels = c("<=25", "26-30","31-35", "36-40", "41-45", "46-50", "51<"))
 
b$Q6 = factor(b$Q6,
                      levels = c(101,102,103,104,105, 994, 999),
                      labels = c("dyrektor","główny agronom","agronom ekspert","właściciel","techniczny", "inna", "nie wiem")) 
 
 
b<-mutate(b, recomendations= case_when(
  between(recomendation,1,6) ~ 101,
  between(recomendation,7,8) ~102,
  between(recomendation,9,10) ~103
))
 
 
b$recomendations = factor(b$recomendations,
                          levels = c(101:103),
                          labels = c("detractors", "neutral", "promoters"))
 
write.csv2(b, file = "NPS_results2.csv", row.names = TRUE, fileEncoding = "UTF-8")

