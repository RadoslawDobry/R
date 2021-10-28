library(plyr)
library(dplyr)
library(psych)

regional <- read.csv2("Regional.Structure.csv")
region <- read.csv2("RegionC.csv", encoding = "UTF-8")
DataFile <- read.csv2("Source.Dafafile.Lesson01.csv")

View(region)
View(regional)
View(DataFile)

dim(DataFile)

count(DataFile$RecordNo)

DataFile <- left_join(DataFile,regional, by="RegionA")
DataFile <- left_join(DataFile, region, by="RegionC")
View(DataFile)


DataFile<-mutate(DataFile, RQ2= case_when(
  Q2<=25 ~ 11, 
  between(Q2,25.01,50) ~ 12,
  between(Q2,50.01,100) ~13,
  between(Q2,100.01,250) ~14,
  Q2>250.01 ~ 15
))


keeps <- (c("RecordNo", "Województwo", 'RQ2', "Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6"))
DataFile = DataFile[keeps]

DataFile$RQ2 = factor(DataFile$RQ2,
                      levels = c(11,12,13,14,15),
                      labels = c(">25","25-50","51-100","100-250","250+"))

b<- gather(DataFile,"company","recomendation","Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6", factor_key = TRUE)

summary(b)

b$company = factor(b$company,
                   levels = c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6"),
                   labels = c("Acme", "Central Park", "Cyberdyne", "Weyland-Yutani", "Tyrell Corporation","Red Apple"))
View(b)
summary(b)

#DataFile <- gather(DataFile, "Company", "Recomendation", 
 #           "Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6", factor_key = TRUE )

#DataFile <- mutate(DataFile, Company=as.numeric(Company))

#DataFile$Company = factor(b$company,
#                          levels = c(1,2,3,4,5,6),
#                          labels = c("Acme", "Central Park", "Cyberdyne", "Weyland-Yutani", "Tyrell Corporation","Red Apple"))

b <- filter(b, recomendation != 99)

b<-mutate(b, recomendations= case_when(
  between(recomendation,0,6) ~ 101,
  between(recomendation,7,8) ~102,
  between(recomendation,9,10) ~103
))

b$recomendations = factor(b$recomendations,
                   levels = c(101:103),
                   labels = c("detractors", "neutral", "promoters"))

#b <-  mutate(b, detract= case_when(
#  between(recomendation,0,6) ~ 1,
#  between(recomendation,7,8) ~0,
#  between(recomendation,9,10) ~0
#))

b <- mutate(b, detrac = 
              if_else(between(recomendation,0 ,6), 1, 0))

b <- mutate(b, neutral = 
              if_else(between(recomendation,7 ,8), 1, 0))

b <- mutate(b, promo = 
              if_else(between(recomendation,9 ,10), 1, 0))

b$total <- 1

#b<- subset(b,select =-c(ReagionA, RegionB))

write.csv2(b, file = "NPS_results.csv", row.names = TRUE, fileEncoding = "UTF-8")
