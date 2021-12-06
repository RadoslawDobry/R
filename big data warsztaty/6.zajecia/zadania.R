library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot)

dane <- read.csv2("dane.csv")

sr_cena <- mean(dane$Cena)
odch_std <- sd(dane$Cena)
kwartyle <- quantile(dane$Cena)
summary(dane$Cena)

#srednia powierzchnia, do stanu technicznego
pow_stan <- dane %>%
  group_by(stan) %>%
  summarise(mean(pow))
#srednia powierzchnia, min max Q1,Q2,Q3 do stanu technicznego
pow_stan1 <- dane %>%
  group_by(stan) %>%
  summarise(mean(pow), quantile(pow))
#srednia cena i odchylenie w zależności od stanu
pow_stan2 <- dane %>%
  group_by(stan) %>%
  summarise(sd(pow), mean(pow))
############### tabele
#9 liczba mieszkań stan techniczny
ls <- dane %>%
  group_by(stan) %>%
  count()
#10 liczba mieszkań stan /techniczny /dzielnica
lsd <- dane %>%
  group_by(stan, dzielnica) %>%
  count()
#11 l mieszkań, stan techniczny, cena 
lsc <- dane %>%
  group_by(stan, Cena<199999, Cena>500001, between(Cena,200000,500000)) %>%
  count()

lsc <- dane %>%
  group_by(stan, dane$Cena_grupy, a) %>%
  count()

a <- ifelse(dane$Cena<200000, "poniżej 200k", "powyżej 500k")
b<- ifelse(a<=200000 & a>500000, "pomiedzy", a )
table(cena_grupa)

dane$przedzialy <- dane$Cena<199999

#12stan techniczny z odsetkami

prop.table(table(dane$stan))

#13 stan techniczny, mieszaknie, dzielnica
tabelak <- dane %>%
  group_by(prop.table(table(dane$stan))) %>%
  summarise(dzielnica )

#14   95%  przedział ufności dla śrendiej ceny mieszkania

t.test(dane$Cena)

#z definicji

srednia <- mean(dane$cena)
sd <- sd(dane$cena)
n <- nrow(dane)
z <- qnorm(0.975)


#15
t.test(dane$Cena, conf.level = 0.9)

#17
shapiro.test(dane$Cena)

#18
shapiro.test(dane$pow)

#20 zależność między stanem mieszkania a dzielnicą/ są zależne bo p bliskie 0
chisq.test(dane$dzielnica, dane$stan)


#21
dane$cenab <- ifelse(dane$Cena<300000, "poniżej", "powyżej")
chisq.test(dane$cenab, dane$stan)

#22
model <- lm(Cena ~ pow, dane)
summary(model)

#23
model <- lm(Cena ~ pow + rokBudowy, dane)
summary(model)

#24
cor(dane$Cena, dane$pow)


#25
korelacja <- dane%>%
  select(Cena, lpokoi, pow, pietro)
cor(korelacja)

#26
scatterplot(Cena ~ pow, data = dane)

#27
ggplot()

#28
a<- dane %>%
  group_by(stan)





  
