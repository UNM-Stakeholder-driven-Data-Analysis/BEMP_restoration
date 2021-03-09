#### read me ####

#script originally from Alex Webster on a guide to explore data and modified for BEMP data

# The purpose of this script is to explore my data, including
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables


#### libraries ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(rgdal) # for mapping


#### load and tidy data
  dat%>%
  drop_na() %>% # drop any rows with NAs
  pivot_longer(cols = canopy_introduced :understory_native,
                      names_to = ("cover_class"), values_to = "abundance")
View(dat_long)


# check data classes
str(dat_long)

# #format date/time - BEMP data is by year


# reduce to # or group sites (maybe by treatment type) after discussions with Kim
#dat = dat[dat$Site_Name=="VR-3"| dat$Site_Name=="VR-2"| dat$Site_Name=="VR-1",] - Alex's example


# convert characters that should be factors (categories) to factors
dat_long$site = as.factor(dat_long$site)
dat_long$site.name = as.factor(dat_long$site.name)
dat_long$age = as.factor(dat_long$age)
dat_long$treatment = as.factor(dat_long$treatment)
dat_long$cover_class = as.factor(dat_long$cover_class)


# convert lat to numeric
dat_long$lat= as.numeric(dat_long$lat)



#### describe dataset size and structure ####

head(dat_long)
str(dat_long)


### check timesteps by looking and time series of most frequently collected parameters -- need to discuss with Kim
# make dataset of one of the most frequently collected parameters

### How many variables are in your dataset for dat_long?
str(dat_long)
# 11 parameters

### How many observations are in your dataset for dat_long?
nrow(dat_long)
# 1300 total

## understand how many observations per variable are available -- for BEMP data there is one data point every year for each column
# if there are a variable # of observations for each
with(dat_long, table(cover_class, site))
range(with(dat_long, table(cover_class, site)))
# Site 1 has the most observations of 18 and site 27 has only 1 observation.


### Are the data nested in time or space?
# No in time - observations were collected on annual basis
# Yes in space - observations were collected 27 sites, need more research/exploration to find out if sites are connected in any way

#### describe data types ####

str(dat_long)

#need to determine what units were used to collect data

# site                = integer (discrete numeric variable)
# year                = integer (discrete numeric variable)
# site.name           = factor (categoric, nominal)
# lat                 = numeric (continuous numeric variable)
# long                = numeric (continuous numeric variable)
# age                 = factor (categoric, ordinal)
# treatment           = factor (categoric, nominal)
# mean_temp           = numeric (continuous numeric variable) -- need units!
# year_gw_mean        = numeric (continuous numeric variable) -- need units!
# cover_class         = factor (categoric, nominal)
# abundance           = numeric (continuous numeric variable) -- need units!



#### check distributions ####

#example:
# temp = dat_r[dat_r$Parameter == "Alkalinity",]
# qqPlot(temp$Value); shapiro.test(temp$Value) # normal
# qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
# qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal
# qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-3']) # normal

#distributions of canopy native
#all sites
temp = dat_long[dat_long$cover_class == "canopy_native",] #not normal
qqPlot(temp$abundance); shapiro.test(temp$abundance)

#site 1
temp = dat_long[dat_long$cover_class == "canopy_native",]
qqPlot(temp$abundance[temp$site=='1']); shapiro.test(temp$abundance[temp$site=='1']) #normal

#site 2
temp = dat_long[dat_long$cover_class == "canopy_native",]
qqPlot(temp$abundance[temp$site=='2']); shapiro.test(temp$abundance[temp$site=='2']) #slightly not normal

#site 3
temp = dat_long[dat_long$cover_class == "canopy_native",]
qqPlot(temp$abundance[temp$site=='3']); shapiro.test(temp$abundance[temp$site=='3']) #normal

#distributions of canopy introduced
temp = dat_long[dat_long$cover_class == "canopy_introduced",] #not normal
qqPlot(temp$abundance); shapiro.test(temp$abundance)

#site 1
temp = dat_long[dat_long$cover_class == "canopy_introduced",]
qqPlot(temp$abundance[temp$site=='1']); shapiro.test(temp$abundance[temp$site=='1']) #normal

#site 2
temp = dat_long[dat_long$cover_class == "canopy_introduced",]
qqPlot(temp$abundance[temp$site=='2']); shapiro.test(temp$abundance[temp$site=='2']) #not normal

#site 3
temp = dat_long[dat_long$cover_class == "canopy_introduced",]
qqPlot(temp$abundance[temp$site=='3']); shapiro.test(temp$abundance[temp$site=='3']) #normal

#distributions of understory native
#all sites
temp = dat_long[dat_long$cover_class == "understory_native",] #not normal
qqPlot(temp$abundance); shapiro.test(temp$abundance)

#site 1
temp = dat_long[dat_long$cover_class == "understory_native",]
qqPlot(temp$abundance[temp$site=='1']); shapiro.test(temp$abundance[temp$site=='1']) #normal

#site 2
temp = dat_long[dat_long$cover_class == "understory_native",]
qqPlot(temp$abundance[temp$site=='2']); shapiro.test(temp$abundance[temp$site=='2']) #not normal, observation 7 is an outlier

#site 3
temp = dat_long[dat_long$cover_class == "understory_native",]
qqPlot(temp$abundance[temp$site=='3']); shapiro.test(temp$abundance[temp$site=='3']) #normal

#distributions of understory introduced
#all sites
temp = dat_long[dat_long$cover_class == "understory_introduced",] #not normal
qqPlot(temp$abundance); shapiro.test(temp$abundance)

#site 1
temp = dat_long[dat_long$cover_class == "understory_introduced",]
qqPlot(temp$abundance[temp$site=='1']); shapiro.test(temp$abundance[temp$site=='1']) # not normal

#site 2
temp = dat_long[dat_long$cover_class == "understory_introduced",]
qqPlot(temp$abundance[temp$site=='2']); shapiro.test(temp$abundance[temp$site=='2']) #not normal, observation 16 and 6 are outliers

#site 3
temp = dat_long[dat_long$cover_class == "understory_introduced",]
qqPlot(temp$abundance[temp$site=='3']); shapiro.test(temp$abundance[temp$site=='3']) #not normal

# etc........ for the rest of the parameters that I think I'll use in this analysis -- Need to talk to Kim about which sites to use and how to group them.

### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal?
# can I justify removing outliers based on my knowledge of the data? --  will talk to kim
# if data is still non-normal, what distribution is it?

#Non-normal data for Canopy_introduced at all sites
#outliers: observation 221
temp = dat_long[dat_long$cover_class == "canopy_native",]
summary(temp$abundance)
hist(temp$abundance) #positively skewed
plot(density(temp$abundance, na.rm = T))

#IF.....
# still not normal!
# this looks like a lognormal, Gamma, or Weibull distribution
# it is bounded above zero and is right-skewed
# what happens if I log-transform it?
temp = dat_long[dat_long$cover_class == "canopy_native",]
qqPlot(log10(temp$abundance)); shapiro.test((temp$abundance)) ## still not normal??

temp = dat_long[dat_long$cover_class == "canopy_native",]
qqPlot(exp(temp$abundance)); shapiro.test((temp$abundance)) ## still not normal??

#Non-normal data for Canopy_introduced at all sites
#outliers: observation 221
temp = dat_long[dat_long$cover_class == "canopy_introduced",]
summary(temp$abundance)
hist(temp$abundance) #positively skewed
plot(density(temp$abundance, na.rm = T))

#Non-normal data for understory_introduced at all sites
temp = dat_long[dat_long$cover_class == "understory_introduced",]
summary(temp$abundance)
hist(temp$abundance) #positively skewed
plot(density(temp$abundance, na.rm = T))

#Non-normal data for understory_introduced at all sites
temp = dat_long[dat_long$cover_class == "understory_introduced",]
summary(temp$abundance)
hist(temp$abundance) #positively skewed
plot(density(temp$abundance, na.rm = T))

#Non-normal data for understory_native at all sites
temp = dat_long[dat_long$cover_class == "understory_native",]
summary(temp$abundance)
hist(temp$abundance) #positively skewed
plot(density(temp$abundance, na.rm = T))

## need to explore more normality ##

#### check for temporal autocorrelation ####

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data.
# BEMP data is annual

is.ts(dat_long$abundance) #FALSE

#### Canopy_introduced at Site 1
### subset data to be one site and one parameter
#example:
  #temp = dat_monthly[dat_monthly$Parameter == "Alkalinity" & dat_monthly$Site_Name=="VR-2" ,]

temp = dat_long[dat_long$abundance == "canopy_introduced" & dat_long$site == "1",]

### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(year)

## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
#example:
# temp_ts =
#   temp %>%
#   complete(date = seq(min(date), max(date), by = "1 month"),
#            fill = list(value = NA)) %>%
#   as_tsibble(index = date)

# temp_ts =
#   temp %>%
#   complete(date = seq(min(date), max(date), by = "1 year"),
#            fill = list(value = NA)) %>%
#   as_tsibble(index = date)


## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occurred. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean.

head (temp)
temp = ts(temp$abundance, frequency=1, start=c(2000), end= c(2017))  #error - 'ts' object must have one or more observations

# check that you specified the ts correctly
print(temp, calendar = T)

### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details).
#
# forecast::Acf(temp, na.action = na.pass)
# forecast::Acf(temp, na.action = na.contiguous) # longest stretch with no NAs
# forecast::Acf(temp, na.action = na.interp) # uses time series model to fill in any NAs with likely values
#
# #partial auto correlation -- accounts for correlation of the data point at that lag with all previous data points - helps identify where the autocorrelation is more important
# forecast::Pacf(temp, na.action = na.pass)
# forecast::Pacf(temp, na.action = na.contiguous)
# forecast::Pacf(temp, na.action = na.interp)

# acf tells me that there is no temporal autocorrelation. Lag 2 is close to the 95% CI.

# pcaf tells me that there is no temporal autocorrelation.


# ....... ect. for each parameter and site combination I might include in the analysis .......



#### check for spatial autocorrelation ####

# I'm interested in spatial and not temporal autocorrelation, so I am going to look at just a few observations across all sites


# how many sites are there?
length(unique(dat_long$site))
# 26

#canopy_introduced spatial autocorrelation
temp = dat_long %>%  filter(cover_class =="canopy_introduced"& year == "2015")

## Mantel test - since data looks not normal will use Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$long, temp$lat))

# generate response distance matrix
resp_dists = dist(temp$abundance)

# run Mantel test - looking for correlation between two distance matrices -- if they are correlated, then there is spatial autocorrelation
mantel.rtest(site_dists, resp_dists, nrepet = 9999)

# 'observation' is the correlation between the distance matrices
# p value > 0.05 (p-value = 0.00498) suggests that they are NOT correlated

#canopy_native spatial autocorrelation
temp = dat_long %>%  filter(cover_class =="canopy_native" & year == "2015")

## Mantel test - since data looks not normal will use Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$long, temp$lat))

# generate response distance matrix
resp_dists = dist(temp$abundance)

# run Mantel test - looking for correlation between two distance matrices -- if they are correlated, then there is spatial autocorrelation
mantel.rtest(site_dists, resp_dists, nrepet = 9999)

# 'observation' is the correlation between the distance matrices
# p value > 0.05 (p-value = 0.00498) suggests that they NOT are correlated

#understory_introduced spatial autocorrelation
temp = dat_long %>%  filter(cover_class =="understory_introduced" & year == "2015")

## Mantel test - since data looks not normal will use Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$long, temp$lat))

# generate response distance matrix
resp_dists = dist(temp$abundance)

# run Mantel test - looking for correlation between two distance matrices -- if they are correlated, then there is spatial autocorrelation
mantel.rtest(site_dists, resp_dists, nrepet = 9999)

# 'observation' is the correlation between the distance matrices
# p value > 0.05 (p-value = 0.00498) suggests that they NOT are correlated

#understory_native spatial autocorrelation
temp = dat_long %>%  filter(cover_class =="understory_native" & year == "2015")

## Mantel test - since data looks not normal will use Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$long, temp$lat))

# generate response distance matrix
resp_dists = dist(temp$abundance)

# run Mantel test - looking for correlation between two distance matrices -- if they are correlated, then there is spatial autocorrelation
mantel.rtest(site_dists, resp_dists, nrepet = 9999)

# 'observation' is the correlation between the distance matrices
# p value > 0.05 (p-value = 0.00498) suggests that they NOT are correlated

## Map -- not working - Error in .local(obj, ...) : NA values in coordinates
# proj = CRS("+proj=longlat +datum=WGS84")
# temp_spatial  <- SpatialPointsDataFrame(coords= cbind(temp$long, temp$lat),
#                                     data = as.data.frame(cbind(temp$site, temp$abundance)),
#                                     proj4string = proj)
# plot(temp_spatial)

# ect.......... for other parameters of interest and for a few other time points, depending on how your data is structured ...........


#### check correlation between variables ####

#reformat data to make it wider, such that parameters get their own columns -- will return to "dat"

# reduce data down to one site
#Site 1
temp = dat %>% filter(site =="1")

# plot correlations (of data columns only)
pairs.panels(temp[,8:13], scale=T)
pairs.panels(temp[,8:13], scale=F)

# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,8:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"


#Site 2
temp = dat %>% filter(site =="2")

# plot correlations (of data columns only)
pairs.panels(temp[,8:13], scale=T)
pairs.panels(temp[,8:13], scale=F)

# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,8:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"

#Site 3
temp = dat %>% filter(site =="3")

# plot correlations (of data columns only)
pairs.panels(temp[,8:13], scale=T)
pairs.panels(temp[,8:13], scale=F)

# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,8:13], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"
View(tab)

