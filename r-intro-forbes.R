## ===========================
## R Minimalistic introduction
## ===========================
## NOTE: Everything after # is ignored by R interpreter

## Forbes billionaires
## https://www.forbes.com/billionaires/

## More vectors (think columns in spreadsheets)
## character vector:
milioner <- c('Jeff Bezos', 'Bill Gates', 'Bernard Arnault',
              'Warren Buffett', 
              'Larry Ellison', "Amancio Ortega", 
              "Mark Zuckerberg", "Jim Walton", "Alice Walton", "Rob Walton")

## numeric vector:
## majątek (wealth)
majatek <- c(113, 98,76, 67.5,59,55.1,54.7,54.6,54.4, 54.1)

## another numeric vector
## wiek (age)
wiek <- c(56, 64, 71, 89, 75, 84, 35, 71, 70, 75 )

## character vector (fake) dates
## urodzony (born)
urodzony <- c( 
   '1964-01-01', '1956-01-01', '1949-01-01', '1931-01-01', '1945-01-01',
   '1936-01-01', '1985-01-01', '1949-01-01', '1950-01-01', '1945-01-01' )

## country ISO codes 
kraj <- c( 'US', 'US', 'FR', 'US', 'US', 'ES', 'US', 'US', 'US', 'US' );

## F&I = finance and investment
## branch/sector
## type of vector ??
branza <- c("Technology", "Technology", "Retail", "F&I", "Technology", "Retail",
   "Technology", "Retail", "Retail", "Retail")

## Dataframe = fundamental R data structure (think spreadsheet)
## dataframe = list of named vectors
forbes <- data.frame(milioner, majatek, urodzony, wiek, kraj, branza)


## Print column (more useful)
## df$column = access column from df
## df$colum

## another column:
forbes$majatek

##
## access columns using numbers
## columns are numbered from 1
forbes[, 3]


## Print first 13 rows
## useful functions
head(forbes, n=13)

## Print some rows from the bottom of dataframe
tail(forbes)

## Number of rows in a dataframe
nrow(forbes)
##ncol()
##
## Basic (descriptive) statistics:

## mean of a column majatek (wealth) from dataframe forbes
mean(forbes)

mean(forbes$majatek)

## some summary statistics
summary(forbes$wiek)

## standard deviation
sd(forbes$wiek)

## minimum value
min(forbes$wiek)

## ###########################################################################
## In practice data are loaded from files or URLs
## As a example file FB2020.csv contains data on Forbs billionaires
## 
## Import data into frame (function csv.read):

forbes <- read.csv("FB2020.csv", dec=".", sep = ';',  header=TRUE, na.string="NA");

## dec = determines decimal 
## sep = cell separation character (';')
## header = if there is a header with variable names (T) or not (F)
## na.string = determines how missing values are encodes (here as 'NA')

nrow(forbes)

## Core R can be extended by attaching libraries. Some
## libraries are very useful. Installing a library is very easy:
## install.packages("library") 

## Filtering/selecting/modyfing dataframes with dplyr/tidyverse

## Filtering rows
## install.packages("tidyverse")

library("tidyverse")

## install.packages("dplyr") 
## installation is automatic (upon confirmation) in RStudio

## filter all billionaires who are non US:
nonus.forbes <- filter(forbes, country != "US")
nonus.forbes

## Modification oprations can be connected to one sequence with |> operator
## Example: filter some rows |> select some columns:
library("tidyverse")

nonus.forbes.worth = filter(forbes, country != "US") |> select(worth)

## Compute total wealth:
sum(nonus.forbes.worth)

## Print all countries  without repetitions
countries <- select(forbes, country) |> unique()

## How many countries:
select(forbes, country) |> unique() |> nrow()

## alternative syntax:
forbes |> select(country) |> unique() |> nrow()

## Grouping is useful for summarization
## Example: compute total wealth and number of billionaires by country 
by.country <- forbes |>
    group_by(country) |> ## group by country
    summarise(
      t = mean(worth), 
      n = n()
      ) ## summarise (in groups)

## Print results
by.country

## #####################################################################
## Graphics
## ggplot2 package is recommended tool for making charts
## ####################################################################
##
library ("ggplot2")
## general syntax (the grammar of graphics):
## 1. Data
## 2. Aesthetics (mapping data variables to visual properties)
## 3. Geometric Objects (points, lines, bars, or polygons)
## 4. Statistical Transformations: Operations on data to create derived values,
##    such as aggregations, densities, or smoothing.
## 5. Scales: Functions mapping data values to aesthetic attributes,
##    such as the scale of an axis or a color gradient.
## 6. Coordinate Systems
##
## ggplot(data=df, mapping=aes(x=X, y=Y)) +
##  ## geo-objects:
##  geom_point()

ggplot(data=forbes, mapping = aes(x="", y=worth)) +
  geom_point()

## some dots overlap

ggplot(data=forbes, mapping = aes(x="", y=worth)) +
  geom_jitter() 
  # ## press F1
  geom_jitter(width = 0.15, color='blue', alpha=.3)
  
# by country
  ggplot(data=forbes, mapping = aes(x=country, y=worth)) +
    geom_jitter() 
  # ## press F1
  # geom_jitter(width = 0.15, color='blue', alpha=.3)
## Too much countries
## recode country into 2 categories:
## case_when(condition1 ~ value, condition2 ~ value, TRUE ~ value)
## or
## mutate(variable = ifelse(condition, true, false))
forbes |> 
   mutate ( country = case_when(country == 'US' ~ 'US', country == 'CN' ~ 'CN', TRUE ~ 'RST')  ) |>
   ## or
   #mutate ( country = ifelse(country == 'US',  "US", "RST")  ) |>
  ggplot(mapping = aes(x=country, y=worth)) +
  ## ^^^^ no data= (because of |>)
  geom_jitter(width=.1, size=.2)
  
## box-plot
forbes |> ggplot(mapping = aes(x="", y=worth)) +
    geom_boxplot()

forbes |> ggplot(mapping = aes(x=branch, y=worth)) +
  geom_boxplot()
## Too many branches
levels(as.factor(forbes$branch))
## Select some branches as a vector my.branches
my.branches <- c('Fashion & Retail', 'Finance & Investments', 'Technology')
forbes |> filter (branch %in% my.branches) |>
  ggplot(mapping = aes(x=branch, y=worth)) +
  geom_boxplot(color='forestgreen') +
  ggtitle('Forbs billionaires 2020 (selected branches)')

## bar-plot
## total worth by branch
forbes |> 
  ggplot(mapping = aes(x=branch, y=worth)) +
  ## F1 stat
  ## stat means statistical_transformation (cf 4. in grammar of graphics above)
  geom_bar(stat="identity") +
  ## error: `stat_count()` must only have an x or y aesthetic.
  ## geom_bar(stat="count")
  coord_flip()
## stat='count' :: (default) count the occurrences of each unique value for the y or x variable 
##  and use bars to display the counts (no x or y needed)
## stat='identity' :: calculate the sum of the y variable, grouped by the x variable 
##  and use bars to display the sums

## Problem arrange the bars (for improved readibility)
##
forbes |> group_by(branch) |> 
  summarise(worth = sum(worth)) |>
  ggplot(mapping = aes(x=reorder(branch, worth), y=worth)) +
  geom_bar(stat="identity") +
  coord_flip()

## pie-chart is a bar-plot in polar coordinate system
## not recommended
## pretty complicated (another argument not to use it)
forbes |> group_by(branch) |> 
  summarise(worth = sum(worth)) |>
  ggplot(mapping = aes(x="", y=worth, fill=reorder(branch, worth))) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

## Line plots
##  geom_line()
## 
## see below Querying BDL

## Querying BDL
## https://bdl.stat.gov.pl/bdl/dane/podgrup/temat
## Number of visitors in Polish museums by community (gmina)
## install.packages("bdl")
library('bdl') ## local-data-bank or Bank Danych Lokalnych
options(bdl.api_private_key ='19c1cfc8-faec-4db5-9e9a-08db64593a4a')

p.vars <- c('1243',  ## visitors
           '410642' ## visitors per 10 ths
           ## '' ##
           )
## get data for p.vars for whole Poland
## quite big (380 counties )
museums <- get_data_by_variable(p.vars, 
                                unitParentId=NULL, ## unitLevel4 id
                                unitLevel=5)
## would be even bigger (2500 communities) but we limit to Malbork county
## google: Malbork+castle
museums.in.malbork <- get_data_by_variable(p.vars, 
                                unitParentId='042214209000', ## unitLevel5 id
                                unitLevel=6)

## museums in pomerania region by county
museums.in.pomorskie <- get_data_by_variable(p.vars, 
                                           unitParentId='042200000000', ## unitLevel5 id
                                           unitLevel=5) |>
  select (id, name, year, visitors=val_1243)

## with row.names=F is better
write.csv(museums.in.malbork, file='Malbork.csv')
write.csv(museums.in.pomorskie, file='Pomorskie.csv', row.names = F)

##
## back to charts
## 
museums.in.malbork |>
  filter (id == '042214209011') |>
  ggplot(mapping = aes(x=as.numeric(year), y=val_1243)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  geom_smooth(method='lm', color='red', se=F)

## Another try
## Add numbers as labels
museums.in.malbork |>
  filter (id == '042214209011') |>
  ggplot(mapping = aes(x=as.numeric(year), y=val_1243)) +
  geom_line() +
  geom_text(mapping=aes(label=val_1243), size=3)

## in ths
## Better numbers
museums.in.malbork |>
  filter (id == '042214209011') |>
  mutate (val_1243 = val_1243/1000) |>
  ggplot(mapping = aes(x=as.numeric(year), y=val_1243)) +
  geom_line(color='navyblue', alpha=.4) +
  geom_text(mapping=aes(label=round(val_1243,1)), size=3)

## Multiple lines
museums.in.pomorskie |>
  ggplot(mapping = aes(x=as.numeric(year), y=visitors)) +
  geom_line()
## what is wrong?? guess

## chart is saved into p1 variable
p1 <- museums.in.pomorskie |>
  ggplot(mapping = aes(x=as.numeric(year), y=visitors,
        color=name)) +
  ##    ^^^^^^^^^^                                              
  geom_line()
## show chart
p1
## save chart in a file
ggsave(p1, file='P1.png', width=10)

## Or one can use facets
## =====================
p2 <- museums.in.pomorskie |>
  ggplot(mapping = aes(x=as.numeric(year), y=visitors,
                       ##color=name
                       )) +
  ##    ^^^^^^^^^^                                              
  geom_line() +
  facet_wrap(~ name,
             scales = 'free_y'
             )
## show chart
p2
## save chart in a file
ggsave(p2, file='P2.png', width=10)

library("scales")

## Details
p3 <- p2 + xlab ("year") +
  scale_x_continuous(breaks = seq(1990, 2030, by=10))
ggsave(p3, file='P3.png', width=10)
##
## 

################################################################################
## Querying Eurostat
library("eurostat")

## NUTS
## https://ec.europa.eu/eurostat/documents/345175/629341/NUTS2021-NUTS2024.xlsx
## read as xlsx
## install.packages("readxl")
library('readxl')
nuts <- read_excel("./NUTS2021-NUTS2024.xlsx") |>
  select (cc=`Country code`, nuts=`NUTS Code`, level=`NUTS level`, name=`NUTS label`)

## Polish subregions (about 70 of them)
nuts.pl.3 <- nuts |> filter (cc =='PL' & level =='3')
## convert to vector
nuts.pl.3.list <- nuts.pl.3 |> select (nuts) |> pull()

## Population (Polish subregions)
##################################
pop <- get_eurostat('demo_r_pjanaggr3') |>
  ### 
  filter (sex == 'T') |> 
  ### 
  filter (age == 'Y15-64' ) |>
  ##
  filter (geo %in% nuts.pl.3.list) |>
  ## zamień yyyy-mm-dd na yyyy
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  select (geo, year, pop=values)
## ###############################################################
## GDP (Polish subregions)
gdp <- get_eurostat('nama_10r_3gdp',  stringsAsFactors = FALSE) |>
  ## per capita (EUR):
  filter (unit == 'EUR_HAB') |>
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  select (geo, year, unit, gdp=values)

pop.gdp.data <- left_join(pop, gdp, by=c('geo'='geo', 'year'='year')) |>
  ## Add subregion  name
  left_join(nuts.pl.3, by=c('geo'='nuts'))

## GDP pc of Polish subregions
p4 <- pop.gdp.data |> filter (year > 2010 ) |> 
  ggplot(aes(x=year, y=gdp)) +
  #geom_jitter(position=position_jitter(0.2),
  #            size=1.2, alpha=.4) +
  geom_boxplot() +
  ggtitle('GDP per capita (Polska/subregiony/EUR)')
p4

## Business demography by size class and NUTS 3 region (2008-2020)
## https://ec.europa.eu/eurostat/databrowser/view/bd_size_r3/
## https://ec.europa.eu/eurostat/databrowser/view/bd_size_r/
ee <- get_eurostat('bd_size_r3',  stringsAsFactors = FALSE) |>
  filter ( geo %in% nuts.pl.3.list ) |>
  ## Enterprise size
  filter (sizeclas=='TOTAL') |>
  mutate (year = substr(TIME_PERIOD, 1, 4)) |>
  select (year, indic_sb, geo, values ) |>
  ## long->wide format
  pivot_wider(names_from = 'indic_sb', values_from = 'values') |>
  ## join with pop.gdp.data
  left_join(pop.gdp.data, by=c('geo'='geo', 'year'='year')) |>
mutate(
  enterprises = V11910 / pop * 1000, ## active enterprises
  new.ent = V11920 / pop * 1000,     ## births/new enterprises
  deaths = V11930 / pop * 1000       ## deaths (of enterprises)
)
 
p5 <- ee |> filter (year > 2010 ) |> 
  ggplot(aes(x=year, y=enterprises)) +
  #geom_jitter(position=position_jitter(0.2),
  #            size=1.2, alpha=.4) +
  geom_boxplot() +
  ggtitle('Polish subregions by number of Enterprises per 1000 population')
p5
 
## 
## Querying WorldBank
## 
library("WDI")

##Service sector maybe not so crucial at war.
##Lets check
##Manufacturing, value added (current US$)
##(https://data.worldbank.org/indicator/NV.IND.MANF.CD)
## 1W = World total
## EU = European Union
countries <- c('CN', 'IN', 'RU', '1W', 'US', 'EU')

prod <- WDI(indicator='NV.IND.MANF.CD', country=countries, start=1960, end=2023)

prod1 <- prod %>%
  filter (year >= 1990) %>%
  select (code=iso3c, year, prod=NV.IND.MANF.CD) %>%
  ## we need countries and WLD in one row
  pivot_wider(names_from = code, values_from = prod) %>%
  ##
  mutate(
    CHN = CHN/WLD * 100,
    EU = EUU/WLD * 100,
    IND = IND/WLD * 100,
    RUS = RUS/WLD * 100,
    USA = USA/WLD * 100
  ) %>%
  select (year, CHN, EU, IND, RUS, USA) %>%
  ## back to long format
  pivot_longer(cols= c('CHN', 'EU', 'IND', 'RUS', 'USA'),
               names_to = 'entity', values_to = 'prod')

p3 <- ggplot(prod1, aes(x=year, y=prod, color=entity )) +
  geom_point(size=.6, alpha=.3) +
  geom_smooth(method="loess", se=F, span=.5) +
  ylab(label="% global share") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 32, by=2)) +
  ggtitle("Manufacturing Value Added/Current USD", subtitle="source: World Bank/NV.IND.MANF.CD")
p3

## END ##
