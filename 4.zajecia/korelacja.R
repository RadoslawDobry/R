library(dplyr)
library(tidyverse)
library(readxl)

ros <- openxlsx::read.xlsx("rossmann.xlsx")

ggplot(ros, aes(x=liczba_klientow, y=sprzedaz) +
  geom_point()

ros <- ros %>%
  group_by(sklep_id) %>%
  summarise(sprzedaz =sum(sprzedaz), liczba_klientow=sum(liczba_klientow)) %>%
ggplot(ros, aes(x=liczba_klientow, y=sprzedaz) +
  geom_point()
  geom_smooth()

korelacja <- ros %>%
  group_by(sklep_id) %>%
  summarise(sprzedaz = sum(sprzedaz), liczba_klientow = sum(liczba_klientow))
#współczynnik determinacji (korelacja^2)liniowej liczba klientów ywjasnia wielkosc sprzedazy w 77%
cor(korelacja)^2
cor(mtcars)
#wspolczynnik korelacji rang spearmana dla zaleznosci nieliowej
cor(korelacja$sprzedaz, korelacja$liczba_klientow, method="spearman")
  

korelacja2 <- ros %>%
  group_by(sklep_id, sklep_typ) %>%
  summarise(sprzedaz = sum(sprzedaz), liczba_klientow = sum(liczba_klientow))

ggplot(korelacja2, aes(x=liczba_klientow, y=sprzedaz))+
  geom_point() +
  geom_smooth() +
  facet_wrap(~sklep_typ, scales = "free_x")

korelacja2 %>%
  group_by(sklep_typ) %>%
  cor()
  
korelacja_konk <- ros %>%
  group_by(sklep_id, sklep_konkurencja) %>%
  summarise(sprzedaz = sum(sprzedaz), liczba_klientow = sum(liczba_klientow))

var(korelacja_konk, na.rm=TRUE)

korelacja_konk <- na.omit(korelacja_konk)
cor(korelacja_konk)

ggplot(korelacja_konk, aes(x=liczba_klientow, y=sklep_konkurencja))+
  geom_boxplot()




