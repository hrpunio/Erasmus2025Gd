#
library("bdl")
library("tidyverse")
library ("DescTools")
library ("hhi")
#install.packages("bdl")
install.packages("hhi")
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')

# Download data from BDL database

# county-level vars
# consumption of electricity per capita
# https://bdl.stat.gov.pl/bdl/metadane/cechy/1880?back=True
## 288063 -- electricity consumption  pc (zamiast wsk urbanizacji)
##
## 498634 -- rate of detectability of the delinquents in ascertained crimes - criminal cases
## 58559 -- crimes detected (total)
## 58564 -- crimes against property
## 77216 -- economic offences
## 58562 -- against life and health
## 54861 -- HOUSING ALLOWANCES (total) ; dodatki mieszkaniowe
## 1646192 -- evictions carried out (total); exmisje
## 60617 -- Population in urban areas
## 63367 -- Urban population in % of total population (half-yearly data)
## 64429 -- average monthly gross wages and salary in relation to the average domestic (Poland = 100)
## 60563 -- non-working age population per 100 persons of working age
## 79214 -- Share of the registered unemployed persons (total)
## 72305 -- population (total)
## =====
##
##
##

p.vars <- c("288063", "58559", "54861", '1646192', '60617', '63367', '64429', '60563',
            '79214', '72305', '77216', '58564', '58562', '498634')
# province-level vars
# w.vars <- c("2018")

d0 <- get_data_by_variable(p.vars, unitParentId=NULL, unitLevel=3)
d1 <- d0 %>% select(id, name, year,
                    elecons=val_288063,
                    allowances=val_54861,
                    ##evictions=val_1646192, ## too much NA
                    crimes=val_58559,
                    ##
                    ##popurban=val_60617,
                    ##
                    urban=val_63367,
                    wages=val_64429,
                    nonworking=val_60563,
                    unemplyshare=val_79214,
                    pop=val_72305,
                    ecocrimes=val_77216,
                    propcrimes=val_58564,
                    lifecrimes=val_58562,
                    detect=val_498634
                    ) %>%
  mutate (
    crimes.pc = crimes / pop * 10000,
    allowances = allowances / pop * 10000) %>%
  ##
  ## from 2013 no NA
  ##
  filter (year > 2012)

d1.clean <- na.omit(d1)

d.h <- d1 %>% filter (crimes.pc > 500) %>% select (name, crimes.pc, crimes, pop)


 write.csv(d1, file='ele_cons.csv', sep=';', row.names = F)


pow <- d1 %>%
  ggplot(aes(y=crimes, x=as.factor(year) )) +
  geom_boxplot() +
  ylab("#") +
  ggtitle("Allowances") +
  xlab('')
pow

m1 <- lm(data=d1, crimes ~ detect)
summary(m1)

names(d1)
m2 <- lm(data=d1, crimes ~ detect + elecons + allowances + urban + wages +
           nonworking + unemplyshare )

summary(m2)

d11 <- d1 %>% filter (year == 2015)

m3 <- lm(data=d1, crimes ~ urban + wages + unemplyshare )

summary(m3)

#####################################################################

f1 <- d1 %>%
      group_by(year) %>% summarise (g = Gini(val, unbiased=F))

## write to csv
#
 write.csv(d1, file='ele_cons.csv', sep=';', row.names = F)
## write.csv has comma as column separator hard coded


na.share <- d1 %>% summarise_all(list(name = ~sum(is.na(.))/length(.)))

## From 2013 data is ok 213--2022
write.table(d1.clean, file='dataset24.csv', sep = ';', row.names = F)
##?write.csv
#cp0 <- read.csv('cycpath.csv', sep=';')
### inspect (str)uctrure
#str(cp0)

## we need only names so limit download to the last year
#
w0 <- get_data_by_variable(w.vars, unitParentId=NULL, unitLevel=2, year = 2022)

w1 <- w0 %>% select (id, name, area=val) %>%
  mutate(wojId = substr(id, 3,4))

## join w1 (provinces) with d1 (counties)

d2 <- d1 %>%
  mutate(wojId = substr(id, 3,4)) %>%
  left_join(w1, by='wojId')
##
## better names:
## start again
w1 <- w0 %>% select (wid=id,
                     wname=name, warea=val) %>%
  mutate(wojId = substr(wid, 3,4))

d2 <- d1 %>%
  mutate(wojId = substr(id, 3,4)) %>%
  left_join(w1, by='wojId')

## OK add macroregions (7)
m0 <- get_data_by_variable(w.vars, unitParentId=NULL, unitLevel=1, year = 2022) %>%
  mutate(mname = gsub("MAKROREGION ", "", name)) %>%
  ## Going international
  mutate (mname = recode(mname,
    "POŁUDNIOWY" = "South",
    "PÓŁNOCNO-ZACHODNI" = "North-West",
    "POŁUDNIOWO-ZACHODNI" ='South-West',
    "PÓŁNOCNY" = 'North',
    "CENTRALNY" = 'Central',
    "WSCHODNI" = 'East',
    "WOJEWÓDZTWO MAZOWIECKIE" = 'Mazovian'
)) %>%
  mutate(mId = substr(id, 1,2)) %>%
  select (mId, mname, marea=val)

## Print column mname
m0$mname

## Join with d2
d3 <- d2 %>%
  mutate(mId = substr(id, 1,2)) %>%
  left_join(m0, by='mId')

## Descriptive statistics
## mean median sd max min

d3s <- d3 %>%
  group_by(mname) %>%
  summarise( mean = mean(cpr),
             median = median(cpr),
             sd = sd(cpr),
             min = min(cpr),
             max = max(cpr),
             n = n())

library("knitr")

## Insert table sorted by 1st column (mname)
kable(d3s)
## Insert table sortted by mean of cpr
table1 <- d3s %>% arrange(desc(mean)) %>%
  kable(col.names = c('Name', 'Mean', 'Median', 'SD', 'Min', 'Max', 'Counties'))
table1

## Graphics
## Histogram
ggplot(d1, aes(x=cpr)) +
  geom_histogram(binwidth = 5, color='skyblue', fill='darkblue')

## Boxplot
ggplot(d3, aes(x=mname, y=cpr)) +
  geom_boxplot(color='skyblue') +
  ggtitle("Cycling path length as % of road length")

## Dotplot
ggplot(d3, aes(x=mname, y=cpr)) +
  geom_jitter(color='skyblue', cex=.8, width=.2) +
  ggtitle("Cycling path length as % of road length")

## mean % by region
## barplot
d3 %>% group_by(mname) %>% summarise(cpr=mean(cpr)) %>%
ggplot(aes(x=mname, y=cpr)) +
  geom_bar(color='skyblue', stat="identity") +
  ggtitle("Cycling path length as % of road lengths")

## Is a relation between cpr and r100?
ggplot(d3, aes(x=r100, y=cpr)) +
  geom_point(color='red') +
  ggtitle("Cycling path length as % of road lengths")

## No [strong] relation

ggplot(d3, aes(x=log(r100), y=log(cpr)) ) +
  geom_point(color='red') +
  geom_smooth(method = "lm") +
  ggtitle("Cycling path length as % of road lengths")

## correlation coefficient
## between r100 and cpr
d3 %>% select (r100, cpr) %>% cor(use = "complete.obs")

## What next?
## ----------
## add text
## Publish @ RPubs.com
## Upload @ github

## ###
## END
## ###
