## Przykład 3: pobranie danych z bazy Eurostat
## chatgpt: Czy dostępne są dane dotyczące dzietności na poziomie powiatów?
## są ale nie BDL
##
setwd("~/Projekty/BDO/ESIntro/ESI_short")
##
library('tidyverse')
library('knitr')

## chatgpt: Jakie najbardziej szczegółowe geograficznie dane dotyczące dzietności udostępnia Eurostat?
## demo_r_find3
library('eurostat')

f0 <- get_eurostat('demo_r_find3')
indic.dic <- get_eurostat_dic('indic_de')

f0 %>% select(indic_de) %>% unique()

## interesuje nas TOTFERRT
## na poziomie NUTS3

f <- f0 %>% filter (indic_de == 'TOTFERRT')
geo_dic <- get_eurostat_dic('geo')
##
## regiony NUTS3 mają identyfikator o długości 5 znaków

f <- f0 %>% filter (indic_de == 'TOTFERRT') %>%
   filter(nchar(geo) == 5) %>%
  mutate (rok = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  select (geo, rok, values) %>%
  ## dodaj kraj członkowski
  mutate (member = substr(geo, 1, 2))

## jakie kraje obejmuje tabela

members <- f %>% select(member) %>% unique()
## tylko kody
members <- left_join(members, geo_dic, by=c('member'='code_name'))
members
members$member

UE.members <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", 
                "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")


## Ew usunąć non-EU?

## Wracam do tabel f
## Ok 1300 makroregionów
f.ue <- f %>% filter (member %in% UE.members)

## uwaga zmiana NUTS weź dane za ostatni rok tylko
NUTS3 <- f.ue %>% filter (rok == 2022) %>% select (geo) %>% unique() 

f.2022 <- f.ue %>% filter (rok == 2022 )
f.2022

m1 <- mean(f.2022$values)
me1 <- median(f.2022$values)
kwartyle <- quantile(f.2022$values)
kwartyle

var(f.2022$values)
sd1 <- sd(f.2022$values)

vs1 <- sd1 / m1 * 100
vs1

summary(f.2022$values)

DescTools::Mode(f.2022$values)

moments::skewness(f.2022$values)
(m1 - DescTools::Mode(f.2022$values)) / sd1

## ustalenie liczby klas
## k <= 5 log(N) ; log jest log dziesiętnym
## 15,4
k1 <- 5 * log10(nrow(NUTS3))
## k = 1 + 1,322 log(n)
## k = sqrt(n)
## 34
k3 <- sqrt(nrow(NUTS3))

f.2022.x <- f.2022 %>% mutate(vClass = cut(values, breaks=seq(0.8, 5.0, by=.25))) %>%
  group_by(vClass) |> summarise(n=n())

t2 <- kable(f.2022.x, col.names = c('Wsp. dzietności', 'liczba krajów') )
t2

## Albo

f.2022.x <- f.2022 %>% mutate(vClass = cut(values, breaks=seq(0.8, 5.0, by=.2))) %>%
  group_by(vClass) |> summarise(n=n())
t2 <- kable(f.2022.x, col.names = c('Wsp. dzietności', 'liczba krajów') )
t2

## będzie NA coś pomineliśmy
## b <- c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 4.5)                        
b <- c(0.8, 0.9,
       1.0, 1.1, 1.2, 1.3, 
       1.4, 1.5, 1.6, 1.7, 1.8, 
       1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 50)                        

f.2022.y <- f.2022 %>% mutate(vClass = cut(values, breaks=b)) %>%
  group_by(vClass) |> summarise(n=n())
t2 <- kable(f.2022.y, col.names = c('Wsp. dzietności', 'liczba krajów') )
t2
## Dodać etykiety

### Średnie
## To nie jest średnia wielkość wsp. płodnośc!!!
m1 <- mean(f.2022$values)
m1
str(m1)

summary(f.2022$values)

## wykres pudełkowy
##
p1 <- f.2022 %>%
  ggplot(mapping = aes(x=member, y=values)) +
  geom_hline(yintercept = m1, color = "red", #linetype = "dashed", 
             size = 1) +
  geom_boxplot(color='forestgreen') +
  ggtitle('Wsp. płodności wg krajów')
p1

## czego brakuje w opisie
ggsave(p1, file='fert-rates-by-nuts3.png')

## Albo
## 
p1a <- f.2022 %>%
  ggplot(mapping = aes(y=member, x=values)) +
  geom_vline(xintercept = m1, color = "red", #linetype = "dashed", 
             size = 1) +
  geom_boxplot(color='forestgreen') +
  ggtitle('Wsp. płodności wg krajów')
p1a
ggsave(p1a, file='fert-rates-by-nuts3-alt.png')

##
## Histogram ##
p05 <- ggplot(f.2022, aes(x =  values)) +
  ##geom_histogram(binwidth =bwd,color = "white", fill = "#9FE367",boundary = 0.5) +
  geom_histogram(binwidth =.2, color = "white", fill = "#9FE367")

ggsave(p05, file='fert-rates-p05.png')

## średnia i mediana
median(f.2022$values)
mean(f.2022$values)
sd(f.2022$values)

## Asymetria
library('moments')
skewness(f.2022$values)

## wsp koncentracji giniego
#install.packages("ineq")
library('ineq')

gini <- ineq(f.2022$values, type = "Gini")
gini
## im wyższy tym większa koncentracja
## w tym przypadku koncentracja jest niewielka
## Wykreślenie krzywej Lorenza
plot(Lc(f.2022$values), main = "Krzywa Lorenza", col = "blue", lwd = 2)
