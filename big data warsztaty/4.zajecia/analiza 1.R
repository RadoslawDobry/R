library(dplyr)
library(tidyverse)
library(readxl)

ros <- openxlsx::read.xlsx("rossmann.xlsx")

roczna_sprzedaz <- ros %>%
  group_by(sklep_id) %>%
  summarise(sprzedaz = sum(sprzedaz))

options(scipen = 999999)
hist(roczna_sprzedaz$suma)

ggplot(roczna_sprzedaz, aes(x=sprzedaz)) +
  geom_density(fill="red") +
  xlim(0,5000000)

rozklad <- roczna_sprzedaz %>%
  summarise(srednia = mean(sprzedaz), mediana= median(sprzedaz),
            Q1=quantile(sprzedaz, 0.25), Q3= quantile(sprzedaz, 0.75),
            odch_std = sd(sprzedaz), wariancja= var(sprzedaz), dominanta = mode(sprzedaz))
#rozkdład prawostronnie niesymatryczny(średnia>mediana>dominanta)

ggplot(roczna_sprzedaz, aes(y=sprzedaz)) +
  geom_boxplot

rozklad$cv <- rozklad$odch_std/rozklad$srednia

library(e1071)
#skośność sprzedaży, oznacza to ze obserwacje skrajne nie są obserwacjami wpływowymi.
#rozkład symtryczny ma skośność 0
skewness(roczna_sprzedaz$sprzedaz)
#kurtoza wynosi 5 co oznacza ze rozkład jest wysmukły (kurtoza większa niż 0)((jeśli byłaby mniejsza niż 0 to byłby rozkład spłaczony platokurtyczny))
kurtosis(roczna_sprzedaz$sprzedaz)

roczna_typ <- ros %>%
  group_by(sklep_typ, sklep_id) %>%
  summarise(sprzedaz = sum(sprzedaz)) %>%
  group_by(sklep_typ) %>%
  summarise(srednia = mean(sprzedaz), mediana= median(sprzedaz),
            Q1=quantile(sprzedaz, 0.25), Q3= quantile(sprzedaz, 0.75),
            odch_std = sd(sprzedaz), wariancja= var(sprzedaz), dominanta = mode(sprzedaz),
            skosnosc= skewness(sprzedaz), kurtoza=kurtosis(sprzedaz)) %>%
  mutate(CV=odch_std/srednia)


ros %>%
  group_by(sklep_typ, sklep_id) %>%
  summarise(sprzedaz = sum(sprzedaz)) %>%
  ggplot(., aes(x=sprzedaz, fill=sklep_typ)) +
  geom_density(alpha=0.5) +
#wykres macieżowy   
#facet_wrap(~sklep_typ)
  geom_boxplot()


ros %>%
  group_by(sklep_typ, sklep_id) %>%
  summarise(sprzedaz = sum(sprzedaz)) %>%
  ggplot(., aes(x=sklep_typ, y=sprzedaz, group=sklep_typ)) +
  geom_boxplot()

swieto <- ros %>%
  group_by(czy_swieto) %>%
  summarise(sprzedaz = sum(sprzedaz))
  
ggplot(swieto, aes(x=czy_swieto, y=sprzedaz, group=czy_swieto)) +
  geom_count()


ros %>%
  group_by(czy_swieto_szkolne) %>%
  summarise(sprzedaz = sum(sprzedaz)) %>%
  ggplot(., aes(x=czy_swieto_szkolne, y=sprzedaz, group=czy_swieto_szkolne)) +
  geom_count()

  
promocja <-ros %>%
  group_by(czy_promocja) %>%
  summarise(sprzedaz = sum(sprzedaz))
  
ggplot(., aes(x=czy_promocja, y=sprzedaz, group=czy_promocja)) +
  geom_count()



