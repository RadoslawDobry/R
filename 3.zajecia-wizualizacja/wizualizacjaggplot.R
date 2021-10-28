ros <- openxlsx::read.xlsx("rossmann.xlsx")

colnames(ros)

library(dplyr)
library(tidyverse)
glimpse(ros)
summary(ros)

roczna_sprzedaz <- ros %>%
  group_by(sklep_id) %>%
  summarise(suma = sum(sprzedaz))

summary(roczna_sprzedaz)
hist(roczna_sprzedaz$suma)
boxplot(roczna_sprzedaz$suma)

options(scipen = 999999)
ggplot(roczna_sprzedaz, aes(x=suma)) +
  geom_histogram()

ggplot(roczna_sprzedaz, aes(y=suma)) +
  geom_boxplot()

#kilka boxplotów
roczna_sprzedaz_tyg <- ros %>%
  group_by(sklep_id, dzien_tyg) %>%
  summarise(suma = sum(sprzedaz))

ggplot(roczna_sprzedaz_tyg, aes(y=suma), group=dzien_tyg) +
  geom_boxplot()

#po paragonie
ros %>%
  group_by(sklep_id, dzien_tyg) %>%
  mutate(wsk = sprzedaz/liczba_klientow) %>%
  summarise(srednia = mean(wsk)) %>%
ggplot(aes(y=srednia, group=dzien_tyg)) +
         geom_boxplot()

##
#ros %>%
#  group_by(sklep_id, dzien_tyg) %>%
#  mutate(wsk = sprzedaz/liczba_klientow) %>%
#  summarise(srednia = mean(wsk, na.rm=T)) %>%
#ggplot(aes(y=srednia, group=dzien_tyg)) +
#  geom_boxplot()

#opis itp
library(ggthemes)
ros %>%
  group_by(sklep_id, dzien_tyg) %>%
  mutate(wsk = sprzedaz/liczba_klientow) %>%
  summarise(srednia = mean(wsk)) %>%
ggplot(aes(y=srednia, group=dzien_tyg)) +
  geom_boxplot()+
  xlab("dzień tygodnia")+
  ylab("średnia wartość paragonu")+
  ylim(c(0,12))+
  theme_solarized_2()

#po typie sklepu
ros$dzien_tyg <- as.character(ros$dzien_tyg)

ros %>%
  filter(liczba_klientow >0) %>%
  group_by(sklep_id, dzien_tyg, sklep_typ) %>%
  mutate(wsk = sprzedaz/liczba_klientow) %>%
  summarise(srednia = mean(wsk)) %>%
  ggplot(aes(y=srednia, x=dzien_tyg, fill=sklep_typ)) +
  geom_boxplot()+
  xlab("dzień tygodnia")+
  ylab("średnia wartość paragonu")+
  theme_solarized_2()

#analiza korelacji
ros %>%
  group_by(sklep_id) %>%
  summarise(sprzedaz = sum(sprzedaz), klienci = sum(liczba_klientow), konk=mean(sklep_konkurencja)) %>%
  ggplot(., aes(x=klienci, y=sprzedaz)) +
  geom_point() +
#"lm robi linear model
# samo smooth robi dopasowanie
  geom_smooth(method = "lm")  

#analiza korelacj dni tygodnia
ros %>%
  group_by(sklep_id, dzien_tyg) %>%
  summarise(sprzedaz = sum(sprzedaz), klienci = sum(liczba_klientow), konk=mean(sklep_konkurencja)) %>%
  ggplot(., aes(x=klienci, y=sprzedaz, color=dzien_tyg)) +
  #jedna wersja
  #scale_color_colorblind()+
  scale_color_manual(values = c("red","green","blue","yellow","black","grey","#f970f9"))+
  #mozna nadawać kolory też hexem
  geom_point()+
  #kazdy elemento z osobna mozna pokolrować
  #theme(axis.text = element_text(size = 18), strip.background = element_rect("red"), panel.background = element_rect("orange"))+
  theme_clean()+
  theme(axis.text = element_text(size = 18))+
  #labels nakłada etykiety na wartosći
  scale_x_continuous(breaks=c(0,100000,200000), labels = c("mało", "średnio", "dużo"))+
  facet_wrap(~dzien_tyg, nrow=4, ncol=2)+
  geom_smooth() #linia dopsaowana do wartości
  ggsave("wykres.png", width = 400, height = 200, units = "mm")
  

#nrow ncol zeby zamienić ilość kolumn itp
  
  