# File to create summaries of census data by precinct or borough
# Output of file is summary .csv files, which are uploaded in the Shiny app

if(!requireNamespace("here"))
  install.packages("here", repos = "https://cloud.r-project.org")
library("here")
here::here()
library(proj4)
library(shiny)
library(dplyr)
library(haven)
library(RSQLite)
require("openintro")
library(tidycensus)
library(xlsx)

#Original Data Cleaning###################################################################################

#Read-in Data. Make sure data is saved in a folder 'data'
data <- read_csv("data/sqf_08_16.csv")
data <- data %>% filter(year >= 2012 & year <= 2016)

#Convert original data coordinates to longitude and latitude
nyc.proj = "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"

coords_all <- proj4::project(list(data$xcoord, data$ycoord), nyc.proj, inverse=TRUE)
data$lat <- coords_all$y
data$lon <- coords_all$x
rm(coords_all)

#Filter to data points in official NYC limits, remove data points without longitude & latitude coordinates
data <- data %>% 
  filter(lon >= -74.257159 & lon <= -73.699215,
         lat >= 40.495992 & lat <= 40.915568,
         is.na(lon) != TRUE,
         is.na(lat) != TRUE)

#Replace NAs in suspect.sex and suspexct.race columns as 'unknown'
#Factor race so that it can be used as colors in the Rshiny app;
#Convert Staten Island borough indicator to common notation ("STATEN ISLAND")

data$suspect.sex[is.na(data$suspect.sex)] <- 'unknown'
data$suspect.race[is.na(data$suspect.race)] <- 'unknown'
data$suspect.weight[is.na(data$suspect.weight)] <- mean(data$suspect.weight, na.rm = T)

data <- data %>%
  mutate(suspect.race = as.factor(suspect.race),
         suspect.sex = as.factor(suspect.sex),
         found.weapon = as.factor(data$found.weapon),
         suspect.build.f = as.factor(suspect.build),
         suspect.age.sc = as.numeric(scale(suspect.age, center=TRUE, scale =TRUE)),
         suspect.height.sc = as.numeric(scale(suspect.height, center=TRUE, scale=TRUE)),
         suspect.weight.sc = as.numeric(scale(suspect.weight, center=TRUE, scale=TRUE)),
         city = ifelse(city == "STATEN IS", "STATEN ISLAND",city))

#found contraband (numerator of hit rate)
data$found.true <- NA
data$found.true <- ifelse(data$found.contraband == T, T, ifelse(data$found.pistol ==T, T, ifelse(data$found.rifle==T, T, ifelse(data$found.assault == T, T, ifelse(data$found.knife== T, T, ifelse(data$found.machinegun==T, T, ifelse(data$found.other== T, T, ifelse(data$found.gun == T, T, ifelse(data$found.weapon==T, T, F)))))))))

#type of contraband found
data$contraband <- NA
data$contraband <- ifelse(data$found.contraband == T, "other.contraband", ifelse(data$found.pistol ==T, "pistol", ifelse(data$found.rifle==T, "rifle", ifelse(data$found.assault == T, "assault", ifelse(data$found.knife== T, "knife", ifelse(data$found.machinegun==T, "machine.gun", ifelse(data$found.other== T, "other.weapon", ifelse(data$found.gun == T, "gun", ifelse(data$found.weapon==T, "other.weapon", "none.found")))))))))

## contraband T/F
data$contraband_count <- data$found.contraband + data$found.pistol + data$found.rifle + data$found.assault + data$found.knife + data$found.machinegun + data$found.other + data$found.gun

#type of force used
data$force.used <- NA
data$force.used <- ifelse(data$force.hands == T, "hands", ifelse(data$force.wall ==T, "wall", ifelse(data$force.ground==T, "ground", ifelse(data$force.drawn == T, "weapon.drawn", ifelse(data$force.pointed== T, "weapon.pointed", ifelse(data$force.baton=="baton", T, ifelse(data$force.handcuffs== T, "handcuffs", ifelse(data$force.pepper == T, "pepper.spray", ifelse(data$force.other==T, "other.force", "no.force.used")))))))))


#why stopped
data$why.stopped <- NA
data$why.stopped <- ifelse(data$stopped.bc.object == T, "object", ifelse(data$stopped.bc.desc ==T, "desc", ifelse(data$stopped.bc.casing==T, "casing", ifelse(data$stopped.bc.lookout == T, "lookout", ifelse(data$stopped.bc.clothing== T, "clothing", ifelse(data$stopped.bc.drugs=="drugs", T, ifelse(data$stopped.bc.furtive== T, "furtive", ifelse(data$stopped.bc.violent == T, "violent", ifelse(data$stopped.bc.bulge==T, "bulge", ifelse(data$stopped.bc.other==T,"other.reason", "no.force.used"))))))))))

#why frisked
data$why.frisked <- NA
data$why.frisked <- ifelse(data$frisked.bc.suspected.crime == T, "suspected.crime", ifelse(data$frisked.bc.weapons ==T, "weapons", ifelse(data$frisked.bc.attire==T, "attire", ifelse(data$frisked.bc.actual.crime == T, "actual.crime", ifelse(data$frisked.bc.noncompliance== T, "noncompliance", ifelse(data$frisked.bc.threats==T, "threats", ifelse(data$frisked.bc.prior== T, "prior", ifelse(data$frisked.bc.furtive == T, "furtive", ifelse(data$frisked.bc.bulge==T, "bulge", ifelse(data$stopped.bc.other==T,"other.reason",ifelse(data$frisked==T,"frisked.no.force","no.force.used")))))))))))

#Census Mapping Section################################################################################################

#census - get crosswalks to link precincts to census tract and county data
census<-read.csv("data/NYC_Blocks_2010CensusData_Plus_Precincts.csv")
geoidcw<-read.csv("data/precinct_blocks_key.csv")

#get data from team cleaned dataset
dta1<-data

#align data types for subsequent merge
typeof(census$precinct)
dta1 <- dta1%>%
  mutate(precinct = as.integer(round(precinct, 0)))
typeof(dta1$precinct)

#create precinct/year level data 
precinct<-unique(dta1$precinct)
year<-unique(dta1$year)
precincts_level<-data.frame(precinct)
precincts_year_level<-data.frame(expand.grid(precinct,year)) %>%
  mutate (precinct=Var1, year=Var2) %>%
  select (precinct,year)
wcensus = dplyr::left_join(precincts_level, census, by = "precinct")
wgeoid = dplyr::left_join(precincts_year_level, geoidcw, by = "precinct")

#note that the GEOIDs on this dataset are 4 longer than the tract geoids in the acs 5 year estimates,
# so create a geoid that takes only the first 11 characters (rather than the 15 it has at start)
wgeoid2 <- wgeoid%>%
  mutate(GEOID = substr(as.character(geoid10), 1, 11)) %>%
  select(-geoid10) %>%
  distinct()

#view available variables by census dataset
#census data key acc671144aa7860d8154e1fc4f97eee2ae0cc0b6
census_api_key("acc671144aa7860d8154e1fc4f97eee2ae0cc0b6",install= TRUE)
#v12 <- load_variables(2012, "acs12", cache = TRUE)
v17 <- load_variables(2017, "acs5", cache = TRUE)

#B03002_006 - Estimate!!Total!!Not Hispanic or Latino!!Asian alone
#B03002_004 - Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
#B03003_003 - Estimate!!Total!!Hispanic or Latino
#B03002_003 - Estimate!!Total!!Not Hispanic or Latino!!White alone
#B03002_005 - Estimate!!Total!!Not Hispanic or Latino!!American Indian and Alaska Native alone
#B03002_007 - Estimate!!Total!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone HISPANIC OR LATINO ORIGIN BY RACE
#B03002_008 - Estimate!!Total!!Not Hispanic or Latino!!Some other race alone HISPANIC OR LATINO ORIGIN BY RACE
#B03002_009 - Estimate!!Total!!Not Hispanic or Latino!!Two or more races

#B03002_001 - Estimate!!Total HISPANIC OR LATINO ORIGIN BY RACE
# This should be all known population -unknown

#2012-2016

#################### 2012 ########################################

ny12 <- get_acs(geography = "tract", 
              variables = c(asian = "B03002_006",
                            black = "B03002_004",
                            hispanic = "B03003_003",
                            white = "B03002_003",
                            native.american ="B03002_005",
                            native.american ="B03002_007",
                            other = "B03002_008",
                            other ="B03002_009"), 
              state = "NY", year = 2012, cache = TRUE) %>%
              mutate(population = estimate,
                     suspect.race = variable)

ny12.2 <- ny12 %>%
          group_by(GEOID, suspect.race) %>%
          summarize(population = sum(population))

#join to data 
wgeoid12 = dplyr::left_join(filter(wgeoid2, year == 2012), ny12.2, by = "GEOID")
head(wgeoid12)

#################### 2013 ########################################
ny13 <- get_acs(geography = "tract", 
                variables = c(asian = "B03002_006",
                              black = "B03002_004",
                              hispanic = "B03003_003",
                              white = "B03002_003",
                              native.american ="B03002_005",
                              native.american ="B03002_007",
                              other = "B03002_008",
                              other ="B03002_009"), 
                state = "NY", year = 2013, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

ny13.2 <- ny13 %>%
  group_by(GEOID, suspect.race) %>%
  summarize(population = sum(population))


#join to data 
wgeoid13 = dplyr::left_join(filter(wgeoid2, year == 2013), ny13.2, by = "GEOID")

#################### 2014 ########################################
ny14 <- get_acs(geography = "tract", 
                variables = c(asian = "B03002_006",
                              black = "B03002_004",
                              hispanic = "B03003_003",
                              white = "B03002_003",
                              native.american ="B03002_005",
                              native.american ="B03002_007",
                              other = "B03002_008",
                              other ="B03002_009"), 
                state = "NY", year = 2014, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

ny14.2 <- ny14 %>%
  group_by(GEOID, suspect.race) %>%
  summarize(population = sum(population))

#join to data 
wgeoid14 = dplyr::left_join(filter(wgeoid2, year == 2014), ny14.2, by = "GEOID")

#################### 2015 ########################################
ny15 <- get_acs(geography = "tract", 
                variables = c(asian = "B03002_006",
                              black = "B03002_004",
                              hispanic = "B03003_003",
                              white = "B03002_003",
                              native.american ="B03002_005",
                              native.american ="B03002_007",
                              other = "B03002_008",
                              other ="B03002_009"), 
                state = "NY", year = 2015, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

ny15.2 <- ny15 %>%
  group_by(GEOID, suspect.race) %>%
  summarize(population = sum(population))

#join to data 
wgeoid15 = dplyr::left_join(filter(wgeoid2, year == 2015), ny15.2, by = "GEOID")


#################### 2016 ########################################
ny16 <- get_acs(geography = "tract", 
                variables = c(asian = "B03002_006",
                              black = "B03002_004",
                              hispanic = "B03003_003",
                              white = "B03002_003",
                              native.american ="B03002_005",
                              native.american ="B03002_007",
                              other = "B03002_008",
                              other ="B03002_009"), 
                state = "NY", year = 2016, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable) 

ny16.2 <- ny16 %>%
  group_by(GEOID, suspect.race) %>%
  summarize(population = sum(population))

#join to data 
wgeoid16 = dplyr::left_join(filter(wgeoid2, year == 2016), ny16.2, by = "GEOID")

####################################################################
#join all years
census_geoid <- bind_rows(wgeoid12,wgeoid13,wgeoid14,wgeoid15,wgeoid16)

#summarize to precinct level
census_precinct <- census_geoid %>%
  group_by(year, precinct, suspect.race) %>%
  summarize(population = sum(population)) %>%
            ungroup() %>%
  data.frame()

write.csv(census_precinct, "data/census_precinct.csv")

#make crosswalk from sqf data
boroxprecinct <- data.frame(borough = data$city, precinct = data$precinct)
boroxprecinct <- boroxprecinct %>% distinct()

#join precincts to borough
census_city = dplyr::left_join(census_precinct, boroxprecinct, by = "precinct")
census_boro <- census_city %>%
  group_by(year, borough, suspect.race) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>%
  data.frame()

write.csv(census_city, file = "data/census_city.csv")
write.csv(census_boro, "data/census_boro.csv")

####################################################################
