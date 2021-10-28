#zadanie 1 wczytane poprzednim razem
#gosp <- readxl::read_xlsx("gospodarstwa.xlsx")

#zadanie 2
gosp_wies <- gosp %>%
  filter(klm=="wieś")
# sprawdzenie liczebności itp
#table(gosp_wies$klm)

#zadanie 3
gosp_doch_2000 <- gosp %>%
  filter(dochg>2000)

#zadanie 4
z4 <- gosp %>%
  filter(dochg>3000, woj=="30", klm=="wieś") 

#zadanie 5 Wyświetl informacje o gospodarstwach z województwa dolnośląskiego
#i mazowieckiego z miast powyżej 500 tys. mieszkańców.
z5 <- gosp %>%
  filter(woj=="14"|woj=="02",klm=="500T+")

#zadanie 6 losowowe 30%
z6 <- gosp %>%
  sample_frac(size=0.3)

#zadanie 7 losowanie 100 elementow
z7 <- gosp %>%
  sample_n(size = 100)

#zadanie 8 gospodarstwa z wierszy
z8 <- gosp %>%
  slice(10:15)

# 9
z9 <- gosp %>%
  select(woj, wydg) %>%
  filter(woj=="30")

#10
z10 <- gosp %>%
  select(dochg) %>%
  filter(between(dochg,3000,4000))

#11
z11 <- gosp %>%
  select(klm:zut)
  
#12
z12 <- gosp %>%
  select(starts_with("d"))

#z13
z13 <- gosp %>%
  select(ends_with("oj"))

#14
z14 <- gosp %>%
  mutate(roznica=dochg-wydg) %>%
  select(dochg,wydg, roznica)
 
#15 
z15 <- gosp %>%
  filter(dochg>0) %>%
  mutate(x=log(dochg), y=log(wydg))

#z16
z16<- gosp %>%
  rename(dochod=dochg, wydatki=wydg)

#z17
z17<- gosp %>%
  group_by(klm) %>%
  summarise(liczba_gospodarstw=n())

#z18
z18 <- gosp%>%
  group_by(woj) %>%
  summarise(liczba_gospodarstw=n())

#z19
z19 <- gosp %>%
  group_by(woj,zut) %>%
  summarise(liczba_gospodarstw=n())

#z20
z20 <- gosp %>%
  filter(!is.na(wydg)) %>%
  summarise(mean=mean(wydg))

#z21
z21 <- gosp %>%
  filter(!is.na(wydg)) %>%
  summarise("średnie zarobki"=mean(wydg), "średnie wydatki" = mean(dochg))

#z22 w ujeciu klm
z22 <- gosp %>%
  filter(!is.na(wydg)) %>%
  group_by(klm) %>%
  summarise(sr=mean(wydg), max=max(wydg), odch_std=sd(wydg), mediana=median(wydg))
  
#z23
z23 <- gosp %>%
  filter(!is.na(wydg)) %>%
  group_by(klm) %>%
  summarise("średnie wydatki"= mean(wydg))

