library(tidyverse)


klienci <- read.csv("klienci.csv")

ggplot(klienci, aes(x=wiek, y=roczny_dochod))+
  geom_point()


ggplot(klienci, aes(x=wiek, y=wskaznik_wydatkow))+
  geom_point()

ggplot(klienci, aes(x=roczny_dochod, y=wskaznik_wydatkow))+
  geom_point()

klienci_z <- klienci %>%
  select(wskaznik_wydatkow, roczny_dochod)%>%
  scale()

wynik <- kmeans(x=klienci_z, centers = 5)



klienci$grupy <- as.factor(wynik$cluster)

ggplot(klienci, aes(x=roczny_dochod, y=wskaznik_wydatkow, color=grupy))+
  geom_point()

install.packages("ClusterR", dep=T)
library(ClusterR)
Optimal_Clusters_KMeans(data=klienci_z, max_clusters = 15, criterion = "WCSSE")

install.packages("clusterCrit", dep=T)
library(clusterCrit)

gr <- KMeans_rcpp(klienci_z, clusters = 3)

kryt_ch<- numeric(15)

for (i in 1:length(kryt_ch)){
  gr <- KMeans_rcpp(klienci_z, clusters = i)
  kryt_ch[i] <- intCriteria(traj=klienci_z, part=as.integer(gr$clusters), crit='Calinski_Harabasz')
}

kryt_ch
plot(1:15, kryt_ch, type='b')


gr <- KMeans_rcpp(klienci_z, clusters = 5)
klienci$grupy2 <- as.factor(gr$clusters)

ggplot(klienci, aes(x=roczny_dochod, y=wskaznik_wydatkow, color=grupy2))+
  geom_point()


klienci %>%
  group_by(grupy2) %>%
  select(-plec, -klientID, -grupy)%>%
  summarise_all(.funs='mean')


cars_z <- mtcars %>%
  scale()

Optimal_Clusters_KMeans(data=cars_z, max_clusters = 5, criterion = "WCSSE")

kryt_ch<- numeric(10)

for (i in 1:length(kryt_ch)){
  gr <- KMeans_rcpp(cars_z, clusters = i)
  kryt_ch[i] <- intCriteria(traj=cars_z, part=as.integer(gr$clusters), crit='Calinski_Harabasz')
}
plot(1:10, kryt_ch, type='b')

gr <- KMeans_rcpp(cars_z, clusters = 4)
mtcars$grupy <- as.factor(gr$clusters)

srednie <- mtcars%>%
  group_by(grupy)%>%
  summarise_all(.funs='mean')

library(haven)
banki <- read_sav("bankloan_binning.sav")

colnames(banki)

banki <- banki%>%
  select(-ed, -default)

banki_z <- banki%>%
  scale()

Optimal_Clusters_KMeans(data=banki_z, max_clusters = 20, criterion = "WCSSE")

kryt_ch<- numeric(20)

for (i in 1:length(kryt_ch)){
  gr <- KMeans_rcpp(banki_z, clusters = i)
  kryt_ch[i] <- intCriteria(traj=banki_z, part=as.integer(gr$clusters), crit='Calinski_Harabasz')
}
plot(1:20, kryt_ch, type='b')

gr <- KMeans_rcpp(banki_z, clusters = 7)
banki$grupy <- as.factor(gr$clusters)
banki%>%
  group_by(grupy)%>%
  summarise_all(.funs='mean')
