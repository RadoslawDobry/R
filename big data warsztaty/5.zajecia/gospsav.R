library(dplyr)
library(tidyverse)
library(readxl)
library(olsrr)
library(haven)

gosp <- read_sav("2015.sav")
ind <- read_sav("ind.sav")

gosp <- gosp %>%
  select(numer_gd:miasto_29_mean, starts_with("h")) %>%
  filter(!is.na(f2015))

ggplot(gosp, aes(x = HL1)) +
  geom_density(fill = "blue") +
  theme_minimal()

gosp %>%
  filter(HL4 < 25000, HL4 / HL1 < 3) %>%
  ggplot(., aes(x = HL1, y = HL4)) +
  geom_point() +
  geom_smooth(method = "lm")



x <- gosp %>%
  filter(HL4 < 25000, HL4 / HL1 < 3)

cor(x$HL1, x$HL4, use = "complete.obs")

model1 <- lm(HL1 ~ HL4, x)
summary(model1)
model1
#y=1,1x + 372 - nie interpretujemy wyrazu wolnego, bo x nie przyjmuje wartości 0

model2 <- lm(HL1 ~ HL4+HA7, x)
model2

y <- ind %>%
  select(numer_gd=Nuner_GD, starts_with("H"),WAGA_2015_OSOBY, F2015, NUMER_OSOBY) %>%
  filter(F2015==1, WAGA_2015_OSOBY !=0 | !is.na(WAGA_2015_OSOBY), NUMER_OSOBY==1)

z <- left_join(x,y, by = "numer_gd")

model3 <- lm(HL1 ~ HL4+HA7+HC9+HC17+HL2, z)
model3
options(scipen=999999)
summary(model3)
#t value - test skutteczności współczynnika parametry wszytkich zmiennych istonie różnią się od 0
#r^2 = 0,92 - 
#błąd standarodwy Residual standard error: 698.4 on 9991 degrees of freedom
698/mean(z$HL1) 
#jezeli w modelu błąd standardowy przekracza 20% to nie jest dobry model jest to granica

ols_test_normality(model3)
ols_vif_tol(model3)

#european social survey






