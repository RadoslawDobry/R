install.packages("tidyverse", dependencies =T )
library("tidyverse")

getwd()

x <- read_csv2("Powiaty.csv")

x_head <- head(x)
view(x_head)
summary(x)
glimpse(x)

hist(x$X1_s)
boxplot(x[,3])
#ten sam wynik na inny sposób
boxplot(x$X1_s)

tabela <- table(x$Powiaty)
view(tabela)

gosp <- readxl::read_xlsx("gospodarstwa.xlsx")
glimpse(gosp)

#nadanie factora
gosp$klm <- factor(gosp$klm, levels= c(1:6),
                   labels = c("500T+","200T-500T", "100T-200T", "20T-100T", ">20T", "wieś"))
table(gosp$klm)

# zapis danych na wypadek wywalenia się R
save(gosp, powiaty, x,  file = "dane.RData")
load("dane.RData")

#ćwiczenie nadanie factora
gosp$d61 <- factor(gosp$d61, levels = c(1:5),
                   labels = c("Bardzo dobra", "Raczej dobra", "Przeciętna", "Raczej zła","Zła"))
table(gosp$d61)

#tabela krzyżowa jako tabela
y <- table(gosp$klm, gosp$d61)
#jako ramka danych
y <- as.data.frame(table(gosp$klm, gosp$d61))

#strumieniowanie dplyr %>%
#obliczanie
gosp %>%
  group_by(klm) %>%
  summarise(sr_doch= mean(dochg, na.rm=T))
#na.rm usuwanie NaN

gosp %>%
  filter(!is.na(dochg)) %>%
  group_by(klm) %>%
  summarise(sr_doch=mean(dochg))



  
