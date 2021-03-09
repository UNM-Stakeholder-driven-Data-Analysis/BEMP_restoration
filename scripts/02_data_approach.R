####read me####
# The purpose of this script is to explore how to approach my data, including
# - explore ratio of canopy introduced to native
# - applying linear models to data
# - applying ordination techinques to data
# -
# -
# -


#### libraries ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(lme4) # for creating mixed models
library (car) #for Anova(), vif(),variance inflation, factors
library(MuMIn) #for AICc
library (emmeans) #for emmeans, emtrends, all the post hoc tests and plotting
library (CCA)
library(vegan)


#### load and tidy data ####

dat = read.csv ("inside_data/filtered_BEMP_data_all.csv", header = TRUE, fileEncoding="UTF-8-BOM")
View(dat)

#tidy data: pivot data to long
# dat_long <-
#   dat%>%
#   drop_na() %>% # drop any rows with NAs
#   pivot_longer(cols = canopy_introduced :understory_native,
#                names_to = ("cover_class"), values_to = "abundance")
# View(dat_long)

dat %>% drop_na()

# convert characters that should be factors (categories) to factors
dat$site = as.factor(dat$site)
dat$site.name = as.factor(dat$site.name)
dat$age = as.factor(dat$age)
dat$treatment = as.factor(dat$treatment)


# convert lat to numeric
dat$lat= as.numeric(dat$lat)

head(dat)
str(dat)
nrow(dat)


#### explore ratio of canopy introduced to native ####

#create introduced to native species ratio for canopy and understory
dat_ratio <-
  dat  %>%
  mutate(canopy_ratio = canopy_native/ canopy_introduced) %>% #native/intro
  mutate(understory_ratio = understory_native  / understory_introduced) #native/intro

View(dat_ratio)
str(dat_ratio)

#remove Inf
dat_ratio[sapply(dat_ratio, is.infinite)] <- NA
dat_ratio <-dat_ratio[complete.cases(dat_ratio), ]
view(dat_ratio)

#### plots####

#plot ratios for all sites over yr_gw_mean - not a good plot
#canopy
# dat_ratio %>%
#   ggplot(aes(x = yr_gw_mean, y = canopy_ratio)) +
#   geom_point(col = "blue") +
#   #geom_line() +
#   #geom_smooth(method = "lm", se = F) +
#   theme_bw()

# understory - not a good plot
dat_ratio %>%
  ggplot(aes(x = yr_gw_mean, y = understory_ratio)) +
  geom_point(col = "blue") +
  #geom_line() +
  #geom_smooth(method = "lm", se = F) +
  theme_bw()

#plot canopy ratio by sites
dat_ratio %>%
  ggplot(aes(x=year, y=canopy_ratio, color=treatment)) + geom_point() + facet_wrap(~site)

#plot understory ratio by sites
dat_ratio %>%
  ggplot(aes(x=year, y=understory_ratio, color=treatment)) + geom_point() + facet_wrap(~site) + labs(title = "Understory ratio over years")

####linear models ####

#create a linear model:
lm_understory_t_a <- lm(understory_ratio ~ treatment * age, data = dat_ratio, na.action = na.omit)

#check assumptions
plot(lm_understory_t_a)
vif(lm_understory_t_a)


