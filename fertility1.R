## chatgpt: Czy eurostat publikuje wielkość GDP na poziomie NUTS3?
## nama_10r_3gdp
## ilc_lvho02. odsetek osób zagrożonych ubóstwem lub wykluczeniem społecznym
## edat_lfse_03  -- wykształcenie wyższe

setwd("~/Projekty/BDO/ESIntro/ESI_short")
##
library('tidyverse')
library('knitr')

## chatgpt: Jakie najbardziej szczegółowe geograficznie dane dotyczące dzietności udostępnia Eurostat?
## 
library('eurostat')

## fertility
## Współczynnik dzietności
f0 <- get_eurostat('demo_r_find3') %>%
  filter (indic_de == 'TOTFERRT') %>%
  ## NUTS2
  filter(nchar(geo) == 4) %>%
  mutate (rok = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  filter (rok == 2022) %>%
  select (geo, fert=values)

## GDP per capita
## 
g0 <- get_eurostat('nama_10r_3gdp') %>%
  filter (unit == 'EUR_HAB') %>%
  ## nuts 2
  filter(nchar(geo) == 4) %>%
  mutate (rok = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  filter (rok == 2022) %>%
  select (geo, gdp=values)
  
summary(g0$gdp)
m1 <- mean(g0$gdp)
p1 <- g0 %>% mutate (member = substr(geo, 1, 2)) %>%
  ggplot(mapping = aes(x=member, y=gdp)) +
  geom_hline(yintercept = m1, color = "red", #linetype = "dashed", 
             size = 1) +
  geom_boxplot(color='forestgreen') +
  ggtitle('GDP wg makroregionów')
p1


## są wartości NA
fg <- left_join(f0, g0, by='geo') %>% na.omit()
nrow(fg)

p2 <- fg %>% ggplot(mapping = aes (x=gdp, y=fert)) +
  geom_point()
p2


p2l <- fg %>% ggplot(mapping = aes (x=log(gdp), y=log(fert))) +
  geom_point()
p2l

cor(fg$fert, fg$gdp)

fg1 <- fg %>% select (- geo )
## wszystkie kolumny muszą być liczbowe
cor(fg1)

########################################################################
## Severe material and social deprivation by NUTS region
# Percentage of total population living in conditions of severe material 
# deprivation by NUTS 2 regions. The collection "material deprivation" covers
# indicators relating to economic strain, durables, housing and environment 
# of the dwelling. Severely materially deprived persons have living conditions
# severely constrained by a lack of resources, they experience 
# at least 4 out of 9 following deprivations items: they cannot afford 
# i) to pay rent or utility bills, ii) keep home adequately warm, 
# iii) face unexpected expenses, iv) eat meat, fish or a protein equivalent 
# every second day, v) a week holiday away from home, vi) a car, 
# vii) a washing machine, viii) a colour TV, ix) a telephone.
#  wskaźnik pogłębionej deprywacji materialnej i społecznej definiowany jest jako odsetek
# osób, które ze względu na sytuację finansową nie mają możliwości zaspokojenia co najmniej 4 z 9 potrzeb
# materialnych i społecznych, które w warunkach europejskich są uznawane za podstawowe.
########################################################################
w0 <- get_eurostat('ilc_mdsd18') %>%
  ## nuts 2
  filter(nchar(geo) == 4) %>%
  mutate (rok = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  filter (rok == 2022) %>%
  select (geo, sd=values)

fgw <- left_join(fg, w0, by='geo') %>% na.omit()
nrow(fgw)

fgw1 <- fgw %>% select (- geo )
cor(fgw1)


p3 <- fgw %>% ggplot(mapping = aes (x=sd, y=fert)) +
  geom_point()
p3

### Education
### edat_lfse_04
### odsetek kobiety w wieku 15-64 z wyższym wykształceniem
e0 <- get_eurostat('edat_lfs_9917') %>%
  ## nuts 2
  filter (sex == 'F') %>%
  filter(nchar(geo) == 4) %>%
  mutate (rok = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  filter (rok == 2022) %>%
  filter (isced11 == 'ED5-8' & c_birth == 'TOTAL' & age == 'Y15-64' ) %>%
  select (geo, edu=values)

summary(e0$edu)

fgwe <- left_join(fgw, e0, by='geo') %>% na.omit()
nrow(fgwe)


fgwe1 <- fgwe %>% select (- geo )
cor(fgwe1)

r1 <- cor(fgwe$fert, fgwe$gdp, method = "pearson")
r1

m1 <- lm(fert ~ gdp, data=fgwe)
summary(m1)

m2 <- lm(fert ~ gdp + sd + edu, data=fgwe)
summary(m2)


UE.members <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", 
                "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

fgwe.xx <- fgwe %>%
  mutate (member = substr(geo, 1,2)) %>%
  filter (member %in% UE.members)

write.csv(fgwe.xx, file='fgwe2022.csv', row.names = FALSE)
