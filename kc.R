##
library("tidyverse") ## data manipulation
library('WDI')       ## easy import from WBank
library('knitr')     ## decent tables
library("lmtest")    ##
library("car")       ## D-W test

# Is Kuznets law valid?
# gini = a + b·GDP + c·GDP², where  $c <0
# ========================================

wdi.indicators <- c(
  'SI.POV.GINI', ## Gini Index
  'NY.GDP.PCAP.KD'     ## GDP per capita (constant 2015 US$)
  #'NY.GDP.PCAP.PP.KD'  ## GDP per capita, PPP (constant 2017 international $)
  ## PPP= purchase power parity
)

##
year.start <- 1990

f0 <- WDI(wdi.indicators, country = "all", start=year.start)

## Select appropriate columns.
## Change colum names to something less complicated
f1 <- f0 %>%
  ## Select relevant variables
  ## name1 = name2 denotes _rename_ name2 as name1
  ## eq. EN.ATM.CO2E.PC is renamed to co2 (which is simpler)
  select (
    country,
    iso3c,
    year,
    gini = SI.POV.GINI,
    gdp = NY.GDP.PCAP.KD ) %>%
  ## Compute variables
  mutate(
    lgini = log(gini),
    lgdp = log(gdp),
    lgdp2 = log(gdp)^2,
    gdp2 = gdp * gdp
  )

f.2022 <- f1 |> filter (year == 2022)
#### Regression equation

## Estimate regression equation:
##     ln(gini) = a + b log(GDP) + c log(GDP)²
## results are stored in m0 variable

m0 <- lm(lgini ~ lgdp + lgdp2, data=f.2022)
summary(m0)

m1 <- lm(gini ~ gdp + gdp2, data=f.2022)
summary(m1)

## Filter more clever
##

f.last <- f1 |> group_by(iso3c) |>
  na.omit() |>
  slice(n())
m0 <- lm(lgini ~ lgdp + lgdp2, data=f.last)
summary(m0)
m1 <- lm(gini ~ gdp + gdp2, data=f.last)
summary(m1)

p1 <- f.last |> ggplot(mapping=aes(x=gdp, y=gini)) +
  geom_point(size=.4) +
  geom_smooth(method='lm', fill=NA) 
p1

## Hmmm
library(plm)

f3 <- f1 |> na.omit()
model.3 <- plm(gini ~ gdp + gdp2,
                    data = f3,
                    index = c("iso3c", "year"),
                    model = "within")
summary(model.3)


lmc <- coef(m1);
lmc
lmc[3]
##
apex <- lmc[2] / (-2* lmc[3])
apex.y <- lmc[3] * apex^2 + lmc[2] * apex + lmc[1]
apex


nrow(f.last)
p.kuznets.0 <- ggplot(f.last, aes(x=gdp, y=gini )) +
  geom_point(color='red', alpha=.4) +
  stat_smooth(method = lm, se=F, formula = y ~ poly(x, 2)) +
  annotate("point", x = apex, y = apex.y, colour = "black", size=4) +
  annotate("text", x = apex, y = apex.y, colour = "black", size=4,
           label=sprintf ("%.1f", apex), vjust=2, hjust=0.75)
p.kuznets.0

### EU countries
### Eurostat
library('eurostat')

EU.members <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", 
                "EL", "ES", "FI", "FR", "HR",
                "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
                "PL", "PT", "RO", "SE", "SI", "SK")

## Income quintile share ratio S80/S20 by NUTS 2 region
## EU only
g <- get_eurostat('ilc_di11_r') |>
  mutate (year = substr(TIME_PERIOD, 1, 4),
          year = as.numeric(year)) |>
  ## geo = member state
  mutate (member = substr(geo, 1, 2)) |>
  ## only NUTS2 regions
  filter (str_length(geo) == 4) |>
  ## only EU members
  filter (member %in% EU.members) |>
  select (year, geo, member, iqsr=values)

## GDP
gdp <- get_eurostat('nama_10r_3gdp') %>%
  ## EUR/per capita
  filter (unit == 'EUR_HAB') %>%
  ## nuts 2
  filter(nchar(geo) == 4) %>%
  mutate (year = as.numeric( substr(TIME_PERIOD, 1, 4))) %>%
  ##filter (year == 2022) %>%
  select (year, geo, gdp=values)

df <- left_join(g, gdp, by=c('year', 'geo')) 

df0 <- df |>
  group_by(geo) |>
  na.omit() |>
  slice(n()) |>
  mutate(
    liqsr = log(iqsr),
    lgdp = log(gdp),
    lgdp2 = log(gdp)^2,
    gdp2 = gdp * gdp
  )

m0 <- lm(liqsr ~ lgdp + lgdp2, data=df0)
summary(m0)
m1 <- lm(iqsr ~ gdp + gdp2, data=df0)
summary(m1)
##