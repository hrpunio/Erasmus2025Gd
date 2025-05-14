##
library("tidyverse") ## data manipulation
library('knitr')     ## decent tables
library("lmtest")    ##
library("car")       ## D-W test
library('eurostat')

## Consumprions vs GDP
## C = a + b GDP
## b is called marginal propensity to consume (MPC, Keynes)
## MPC should be 0 < MPC < 1

### EU countries
### Eurostat

EU.members <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", 
                "EL", "ES", "FI", "FR", "HR",
                "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
                "PL", "PT", "RO", "SE", "SI", "SK")

## Income quintile share ratio S80/S20 by NUTS 2 region
## EU only

## GDP
## na_item
## P3 == final consumption expenditure
## B1GQ == GDP
gdp <- get_eurostat('nama_10_gdp') %>%
  ## Current prices EUR
  filter (unit == 'CP_MEUR') |>
  filter (na_item == 'P3' | na_item == 'B1GQ') %>%
  mutate (year = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  select (year, geo, na_item, values) |>
  pivot_wider(##id_cols = c('year', 'geo'),
    names_from = 'na_item', values_from = 'values') |>
  filter (year == 2023) |>
  select (geo, c=P3, gdp=B1GQ)

m0 <- lm(c ~ gdp, data=gdp)
summary(m0)

p1 <- gdp |> ggplot(mapping=aes(x=gdp, y=c)) +
  geom_point(size=1) +
  geom_smooth(method='lm', fill=NA) 
p1

### why?

EU.members <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", 
                "EL", "ES", "FI", "FR", "HR",
                "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
                "PL", "PT", "RO", "SE", "SI", "SK")
gdp <- gdp |>
  filter (geo %in% EU.members)

m0 <- lm(c ~ gdp, data=gdp)
summary(m0)

p1 <- gdp |> ggplot(mapping=aes(x=gdp, y=c)) +
  geom_point(size=1) +
  geom_smooth(method='lm', fill=NA) 
p1

## MPC marginal propensity to consume (Keynes)
