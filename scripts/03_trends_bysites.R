#### read me ####

# the purpose of this script is to understand native abundance trends. (Used 02_timeseries.R from Alex Webster as guide)
# I will start with canopy_native for site 1.

#### libraries ####

library(tidyverse)
library(lubridate)
library(forecast)
library(MARSS)
library(nlme)
library(zoo)
library(beepr)
library(gridExtra)
library(lme4)
library(car)
library(visreg)
library(GGally)
library(purrr)
library(ggplot2)
library(dplyr)
install.packages("broom.helpers") #model coeff


#### load data and format date/time ####

dat = read.csv ("inside_data/filtered_BEMP_data_all.csv", header = TRUE, fileEncoding="UTF-8-BOM")

View(dat)

#date is in year (XXXX)

#### prep time series for canopy native for site one ####

sum(is.na(dat$year))
# there are 0 NAs in dataset

# subset to Site 1: canopy_native and time and remove obs with no date/time
dat_canopynative = dat %>%
                       select(site.name, year, canopy_native) %>%
                       arrange(year)

dat_understorynative = dat %>%
  select(site.name, year, understory_native) %>%
  arrange(year)

# check for duplicate date/time stamps
anyDuplicated(dat_canopynative)
anyDuplicated(dat_understorynative)
#0 dublicated data

# check percentage of dataset with NAs - this is important to document!
sum(is.na(dat_canopynative)/nrow(dat_canopynative)*100)
sum(is.na(dat_understorynative)/nrow(dat_understorynative)*100)
# 0% of dataset with NAs
#no gaps in data

#### create time series ####
#BEMP is annual data

# need to do this to prep for removing seasonality - BEMP is annual data
#canopynative
dat_canopynative_ts = ts(dat_canopynative$canopy_native, start = c(2000), end = c(2017), frequency = 1)
head(dat_canopynative_ts)

par(mfrow=c(1,1))

plot(dat_canopynative_ts)

#understorynative
dat_understorynative_ts = ts(dat_understorynative$understory_native, start = c(2000), end = c(2017), frequency = 1)
head(dat_understorynative_ts)

par(mfrow=c(1,1))

plot(dat_understorynative_ts)

#### remove seasonality #### -- BEMP is annual data

# examine correlation
par(mfrow=c(3,1))
plot(dat_canopynative_ts)
plot(dat_understorynative_ts)


Acf(dat_canopynative_ts) #does not show temporal autocorrelation
Acf(dat_understorynative_ts) #slight correlation at lag 2 and 3

Pacf(dat_canopynative_ts) #does not show temporal autocorrelation
Pacf(dat_understorynative_ts) #slght correlation at lag 2

ggplot(dat_canopynative, aes(x=year, y=canopy_native)) +
  geom_path() + geom_point() + theme_bw()

ggplot(dat_understorynative, aes(x=year, y=understory_native)) +
  geom_path() + geom_point() + theme_bw()

#### linear trends ###
#canopy native
nested_sites <- group_by(dat_canopynative, site.name) %>% nest()

nested_sites
nested_sites$data[[1]]

fit_model <- function(df) lm(canopy_native ~ year, data = df)
nested_sites <- nested_sites %>%
  mutate(model = map(data, fit_model))

nested_sites

#test 1st site to see if it is working
nested_sites$model[[1]]
nested_sites$model[[26]]
nested_sites$model[[25]]
nested_sites$model[[24]]
nested_sites$model[[23]]



#get 95% CI

models <- list("Alameda" = nested_sites$model[[1]], "Rio Grande Nature Center" = nested_sites$model[[2]],
               "Los Lunas" = nested_sites$model[[3]], "Belen" = nested_sites$model[[4]],
               "Santa Ana" = nested_sites$model[[5]], "Savannah" = nested_sites$model[[6]],
               "Lemitar" = nested_sites$model[[7]], "Hispanic Cultural Center (HCC)" = nested_sites$model[[8]],
               "Ohkay Owingeh" = nested_sites$model[[9]], "Diverson" = nested_sites$model[[10]],
               "Calabacillas" = nested_sites$model[[11]], "Minnow" = nested_sites$model[[12]],
                "Harrison" = nested_sites$model[[13]], "Sevilleta" = nested_sites$model[[14]],
                "Valencia Cleared" = nested_sites$model[[15]], "Valencia Forest" = nested_sites$model[[16]],
               "Montano" = nested_sites$model[[17]], "Reynolds Cleared" = nested_sites$model[[18]],
               "Reynolds Forest" = nested_sites$model[[19]], "Route 66" = nested_sites$model[[20]],
               "Badger" = nested_sites$model[[21]], "Bobcat" = nested_sites$model[[22]],
               "BioPark" = nested_sites$model[[23]], "Santo Domingo" = nested_sites$model[[24]],
               "Crawford" = nested_sites$model[[25]], "Bosque Farms" = nested_sites$model[[26]])



ggcoef_compare(models)

#understory native
nested_sites_understory <- group_by(dat_understorynative, site.name) %>% nest()

nested_sites_understory
nested_sites_understory$data[[1]]

fit_model <- function(df) lm(understory_native ~ year, data = df)
nested_sites_understory <- nested_sites_understory %>%
  mutate(model = map(data, fit_model))

nested_sites_understory

#test 1st site to see if it is working
nested_sites_understory$model[[1]]


#get 95% CI

models <- list("Alameda" = nested_sites$model[[1]], "Rio Grande Nature Center" = nested_sites$model[[2]],
               "Los Lunas" = nested_sites$model[[3]], "Belen" = nested_sites$model[[4]],
               "Santa Ana" = nested_sites$model[[5]], "Savannah" = nested_sites$model[[6]],
               "Lemitar" = nested_sites$model[[7]], "Hispanic Cultural Center (HCC)" = nested_sites$model[[8]],
               "Ohkay Owingeh" = nested_sites$model[[9]], "Diverson" = nested_sites$model[[10]],
               "Calabacillas" = nested_sites$model[[11]], "Minnow" = nested_sites$model[[12]],
               "Harrison" = nested_sites$model[[13]], "Sevilleta" = nested_sites$model[[14]],
               "Valencia Cleared" = nested_sites$model[[15]], "Valencia Forest" = nested_sites$model[[16]],
               "Montano" = nested_sites$model[[17]], "Reynolds Cleared" = nested_sites$model[[18]],
               "Reynolds Forest" = nested_sites$model[[19]], "Route 66" = nested_sites$model[[20]],
               "Badger" = nested_sites$model[[21]], "Bobcat" = nested_sites$model[[22]],
               "BioPark" = nested_sites$model[[23]], "Santo Domingo" = nested_sites$model[[24]],
               "Crawford" = nested_sites$model[[25]], "Bosque Farms" = nested_sites$model[[26]])



ggcoef_compare(models)

#explore different models
summary(nested_sites$model[[1]]) #shows a negative trend (-0.9515) that isn't significant
summary(nested_sites$model[[2]]) #shows a negative trend (-4.882) that is significant

par(mfrow=c(1,1))

#visreg((nested_sites$model[[1]]),"year") #fits linear regression through data
#visreg(mod2,"year") #fits linear regression through data


#confint(mod, 't', level=0.95)

## diagnostics ##
#try to create a loop to explore diagnostics
#Acf((nested_sites$model[[1]]) #no significant autocorrelation but looks like original data set
#Acf applies to assessing residuals
#forecast::checkresiduals(mod)

#### test & calculate trends - nlme::gls ####

# see package manual: https://cran.r-project.org/web/packages/nlme/nlme.pdf

# ask auto.arima what it thinks the autocorrelation structure is
auto.arima(dat_canopynative_Site1$canopy_native)

# fit AR(1) regression model with time as a predictor
mod_Ar1 = gls(canopy_native ~ t, data=dat_canopynative_Site1, correlation=corAR1(), method="ML")

# fit some other candidate structures
# summary ARIMA(0,0,0) - no autoregressive order, differencing order, or moving order seems neccessary
mod_AMRAp1q1 = gls(canopy_native ~ t, data=dat_canopynative_Site1, correlation=corARMA(p=1,q=1), method="ML")
mod_AMRAp2 = gls(canopy_native ~ t, data=dat_canopynative_Site1, correlation=corARMA(p=2), method="ML")
mod_AMRAp3 = gls(canopy_native ~ t, data=dat_canopynative_Site1, correlation=corARMA(p=3), method="ML")

# compare models with AIC, AICc, and BIC
# For small data, use AICc – the small sample correction which provides greater penalty for each parameter but approaches AIC as n becomes large. If it makes a difference, you should use it.
# For large data and especially time series data, consider BIC. BIC is better in situations where a false positive is more misleading than a false negative. Remember that false positives are more common with time series.
bbmle::AICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3)  # suggests mod_AMRAp1q1
bbmle::AICctab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3) # better for smaller datasets - suggested using mod_Ar1
bbmle::BICtab(mod_Ar1,mod_AMRAp1q1,mod_AMRAp2,mod_AMRAp3)  # better for larger datasets - optimize to reduce Type 1 (false postives) not applicable with BEMP data

#mod_Ar and mod_AMRAp1q1 have similar AIC scores and because bbmle::AICctab is better for smaller datasets - I will use the mod_Ar1.

summary(mod_Ar1) #shows negative trend ( -1.0326) with a non significant trend
# intervals() for nlme is equivalent to confint() for lm

intervals(mod_Ar1) #CI for nlme package

par(mfrow=c(1,1))
visreg(mod_Ar1,"t")

install.packages("broom.helpers")
ggcoef_model(intervals(mod_Ar1))

### Important notes about extracting residuals from model fits!! ###

# It's important to understand that many extraction fxns in R, such as residuals(modelfit) (same as resid(modelfit)), will detect the object type and call on methods from that package appropriate for that object. So, residuals(modelfit) is using different methods for different model types when the model package requires it, and you need to look up the options for these different methods.
# E.g., residuals(nlme model) calls residuals.lme(nlme model), which has different options than if you call residuals(model fit) on a different kind of model.
# see ?residuals.gls for the methods avaiable for this model type
# type ?residuals. into your console and scroll through the options for other residuals methods for loaded packages

# For gls, you want to assess assumptions on normalized residuals, which is not an option for standard linear models.
# normalized residuals = standardized residuals pre-multiplied by the inverse square-root factor of the estimated error correlation matrix
# see https://stats.stackexchange.com/questions/80823/do-autocorrelated-residual-patterns-remain-even-in-models-with-appropriate-corre

Acf(resid(mod_Ar1))

# extract and assess residuals
par(mfrow=c(1,3))
Acf(resid(mod_Ar1, type = "normalized"), main="GLS AR(1) model residuals")
plot(resid(mod_Ar1, type = "normalized")~c(1:length(dat_canopynative_Site1$t)), main="GLS AR(1) model residuals"); abline(h=0)
qqnorm(resid(mod_Ar1, type = "normalized"), main="GLS AR(1) model residuals", pch=16,
       xlab=paste("shapiro test: ", round(shapiro.test(resid(mod_Ar1, type = "normalized"))$statistic,2))); qqline(resid(mod_Ar1, type = "normalized"))
#residuals meets assumptions

# extract parameter estimates for comparison with MARSS
mod_Ar1.phi = coef(mod_Ar1$modelStruct[[1]], unconstrained=FALSE)
ests.gls = c(b=mod_Ar1.phi, alpha=coef(mod_Ar1)[1],
             time=coef(mod_Ar1)[2],
             logLik=logLik(mod_Ar1))


## Plot fitted values over observations ###
# extract gls results
kf=print(mod_Ar1, what="kfs") # Kalman filter and smoother output

dat = as.vector(dat_canopynative_Site1$canopy_native)

# plot observed data (y)
par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
plot(as.vector(dat) ~ dat_canopynative_Site1$year, type="p", pch=19,
     main = "GLS model predictions conditioned on all y",
     ylab = "Canopy Native", xlab="")

# calc and plot predicted values
predicts = as.vector(kf$xtTkf) +
  coef(mod_Ar1$Intercept) +
  (as.vector(mod_Ar1[["model"]][["fixed"]][["d"]])* coef(t))
# lines(predicts ~ dat_canopynative_Site1$year, col="blue",lwd=2)
# lines(dat_canopynative_Site1$year, predicts-1.96*mod.AR1$states.se,
#       type="l",lwd=1,lty=2,col="blue")
# lines(dat_canopynative_Site1$year, predicts+1.96*mod.AR1$states.se,
#       type="l",lwd=1,lty=2,col="blue")
# # calc and plot predicted values without trend
# predicts.trendless = as.vector(kf$xtT) +
#   coef(mod.AR1)$A[1]
# lines(predicts.trendless ~ RG_abq_mo_DEs$date, col="red",lwd=2)
# lines(dat_canopynative_Site1$year, predicts.trendless-1.96*mod.AR1$states.se,
#       type="l",lwd=1,lty=2,col="red")
# lines(dat_canopynative_Site1$yea, predicts.trendless+1.96*mod.AR1$states.se,
#       type="l",lwd=1,lty=2,col="red")
mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
# dashed lines are 95% CIs, calculated from the standard error. The value of 1.96 is based on the fact that 95% of the area of a normal distribution is within 1.96 standard deviations of the mean.

# blue line predicts is the prediction for fitting the model.
# red line indicates when you take the trend out.


#### test & calculate trends - UARSS ####

## MARSS ##
# User;s guide: https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf
# Package manual: https://cran.r-project.org/web/packages/MARSS/MARSS.pdf
# Lectures: https://nwfsc-timeseries.github.io/atsa/
# Lab book: https://nwfsc-timeseries.github.io/atsa-labs/
# Quick start guide: https://cran.r-project.org/web/packages/MARSS/vignettes/Quick_Start.pdf
# MARSS is a extremely flexible ts modeling framework with a steep learning curve requiring lots of matrix algebra for many applications, so wade into these documents with caution and prepare to get overwhelmed quickly! Also the user's guide is written, in my opinion, pretty poorly for the average user. This class will give you a few common "recipes", but know that almost anything is possible if you take time to learn the modeling framework in full.


## MARSS equivelent to gls corAR1 model ##

## format response var y as a vector
dat = as.vector(dat_canopynative_Site1$canopy_native)
# remove leading and trailing NAs #
dat = na.trim(dat_canopynative_Site1$canopy_native, is.na="any")

## set model parameters - see powerpoint for
mod.list.AR1 = list(
  B=matrix("b"),          # state process model
  Q=matrix("q"),          # state process model
  U="zero",               # state process model
  C="zero",               # state process model
  Z="identity",           # observation process model
  R=matrix("r"),          # observation process model
  A=matrix("intercept"),  # observation process model
  D=matrix("time"),       # observation process model
  d=matrix(c(1:length(dat)), nrow=1), # observation process model
  x0=matrix(dat[1]),
  tinitx=0
)

## fit model
# mod.AR1 <- MARSS(dat, model=mod.list.AR1, method="BFGS")
mod.AR1 <- MARSS(dat, model=mod.list.AR1, control=list(maxit=10000))
beep(2)
# est.AR1 <- MARSSparamCIs(mod.AR1, method = "parametric", alpha = 0.05, nboot = 2000, silent=F)
est.AR1 <- MARSSparamCIs(mod.AR1, method = "hessian", alpha = 0.05)

## extract parameter estimates for comparison to gls
ests.marss = c(b=coef(mod.AR1)$B, alpha=coef(mod.AR1)$A,
               time=coef(mod.AR1)$D[1],
               logLik=logLik(mod.AR1))

## compare UARSS and gls results
# parameter estimates
ests.marss
ests.gls
# 95% CIs on trend estimate
intervals(mod_Ar1)
est.AR1
# notice that UARSS provides a slightly narrower confidence interval

## test residuals for ac
# extract residuals
resids.1 <- residuals(mod.AR1) # see ?residuals.marssMLE
# plot residuals
par(mfrow=c(2,3))
Acf(resids.1$model.residuals[1,], main="Observation process model residuals")
plot(resids.1$model.residuals[1,]~c(1:length(dat)), main="Observation process model residuals"); abline(h=0)
qqnorm(resids.1$model.residuals[1,], main="Observation process model residuals", pch=16,
       xlab=paste("shapiro test: ", round(shapiro.test(resids.1$model.residuals[1,])$statistic,2))); qqline(resids.1$model.residuals[1,])
#
Acf(resids.1$state.residuals[1,], main="State process model residuals", na.action = na.pass)
plot(resids.1$state.residuals[1,]~c(1:length(dat)), main="State process model residuals"); abline(h=0)
qqnorm(resids.1$state.residuals[1,], main="Observation process model residuals", pch=16,
       xlab=paste("shapiro test: ", round(shapiro.test(resids.1$state.residuals[1,])$statistic,2))); qqline(resids.1$state.residuals[1,])


# MARSS is focused on explaining temporal dynamics and is not "willing" to shunt all remaining autocorrelation to error, unlike nlme options. If it can't be explained by an autocorrelated process or covars, it will retain the remaining autocorrelation. This is good information! But not often the most practical option if you can't include the right covars.
# Here, this tells us that a trend over time in observation model is inadequate to capture all the systematic variation in Rio Grande discharge, suggests additional covars are needed.


## Plot fitted values over observations ###
# extract MARSS results
kf=print(mod.AR1, what="kfs") # Kalman filter and smoother output
# plot observed data (y)
par(mfrow=c(1,1),oma = c(0, 0, 2, 0))
plot(as.vector(dat) ~ dat_canopynative_Site1$year, type="p", pch=19,
     main = "UARSS model predictions conditioned on all y",
     ylab = "Discharge (cfs)", xlab="")
# calc and plot predicted values
predicts = as.vector(kf$xtT) +
  coef(mod.AR1)$A[1] +
  (as.vector(mod.AR1[["model"]][["fixed"]][["d"]])* coef(mod.AR1)$D[1])
lines(predicts ~ dat_canopynative_Site1$year, col="blue",lwd=2)
lines(dat_canopynative_Site1$year, predicts-1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="blue")
lines(dat_canopynative_Site1$year, predicts+1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="blue")
# calc and plot predicted values without trend
predicts.trendless = as.vector(kf$xtT) +
  coef(mod.AR1)$A[1]
lines(predicts.trendless ~ RG_abq_mo_DEs$date, col="red",lwd=2)
lines(dat_canopynative_Site1$year, predicts.trendless-1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="red")
lines(dat_canopynative_Site1$yea, predicts.trendless+1.96*mod.AR1$states.se,
      type="l",lwd=1,lty=2,col="red")
mtext("Fitted values over observations", outer = TRUE, cex = 1.5)
# dashed lines are 95% CIs, calculated from the standard error. The value of 1.96 is based on the fact that 95% of the area of a normal distribution is within 1.96 standard deviations of the mean.

# blue line predicts is the prediction for fitting the model.
# red line indicates when you take the trend out.

# you can make a similar plot from nlme::gls results! go try and figure that out :)
