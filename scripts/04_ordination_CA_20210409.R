#### read me ####

# The purpose of this script is to demonstrate calculating and exploring a Correspondence Analysis (CA) for canopy and understory adundance of native and exotic plants.
# Using CA because all abundance data is not linear.

# Steps:
# 1. Check that data meets assumptions of CAs
# 2. Compute CA
# 3. Explore CA
# 4. Plot CA
# 5. Check that results meet assumptions - outliers
# 6. (actually 1!) Decide which axes to retain
# 7. Interpret axes
# 8. Extract scores for further analysis (if applicable)

#### libraries ####

library(tidyverse)
library(psych)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(vegan)
library(ggrepel)


#### load and tidy data ####

dat = read.csv("inside_data/filtered_BEMP_data_20210409.csv", header = TRUE, fileEncoding="UTF-8-BOM", na.strings = c("","NA"))
dat_treatment = read.csv("inside_data/treatment_info_R.csv", header = TRUE, fileEncoding="UTF-8-BOM", na.strings = c("","NA"))
view(dat)

#add initial treatment year to dat
dat_add <-
  dat%>%
  left_join(y=dat_treatment, by = c("site", "site.name"))

view(dat_add)

#create a years since treatment column
dat_years <-
  dat_add  %>%
  mutate(treatment_years = year-initial.treatment.year) #observations since initial treatment

view(dat_years)
#rename to original data
dat <-
  dat_years

# convert characters that should be factors (categories) to factors
dat$treatment = as.factor(dat$treatment)

View(dat)
#write.table(dat, "inside_data/dat.txt", sep="\t")
#write.csv(dat,"inside_data/dat.csv", row.names = FALSE)

#### create different data frames for years of interest after treatment year ####

# 1 year after restoration
dat_1 <-
  dat %>%
   filter(treatment_years == 1)
view(dat_1)

# 5 years after restoration
dat_5 <-
  dat%>%
  filter(treatment_years==5)

# 10 years after restoration -- currently BEMP doesn't have this data
#dat_10 <-
 # dat %>%
 # filter(treatment==10)


#### 1. check assumption for CA ####
#assumptions: unimodal data,
pairs.panels(dat_1[,8:11], scale=TRUE)
pairs.panels(dat_5[,8:11], scale=TRUE)

#check 0 inflation -- these parameters could be 0 inflated
range(dat_1$canopy_introduced)
range(dat_5$canopy_introduced)

range(dat_1$understory_introduced)
range(dat_5$understory_introduced)


range(dat_1$understory_native)
range(dat_5$understory_native)

#### 2. compute CA ####

#subset data to include, sit id, canopy_introduced, canopy_native, undestory_introduced, understory_native

dat_1_2 = dat_1[,9:12]
rownames(dat_1_2) = dat_1$site.abbv

view(dat_1_2)

dat_5_2 = dat_5[,9:12]
rownames(dat_5_2) = dat_5$site.abbv
view(dat_5_2)

ca_1 <- CA(dat_1_2, graph = FALSE)
ca_5 <- CA(dat_5_2, graph = FALSE)



print(ca_1)
print(ca_5)

summary(ca_1)
summary(ca_5)

str(ca_1)
str(ca_5)

#scores:
#The value of each observation on each PC axis
# also known as "scores"
#ca_1$x   #NUL

#### 3. explore CA ####

### view eigenvalues: ##
get_eigenvalue(ca_1)
fviz_eig(ca_1, addlabels = TRUE, choice = c("eigenvalue")) # as scree plot with eigenvalues
fviz_eig(ca_1, addlabels = TRUE, choice = c("variance")) # as scree plot with % variance explained


get_eigenvalue(ca_5)
fviz_eig(ca_5, addlabels = TRUE, choice = c("eigenvalue")) # as scree plot with eigenvalues
fviz_eig(ca_5, addlabels = TRUE, choice = c("variance")) # as scree plot with % variance explained

#### 4. plot CA ####

# biplot of sites + variables #

#axis 1 and 2:

#1 year after treatment
ca_1.p.1.2 =
  fviz_ca_biplot(ca_1,
                 # axes = c(1, 2), # specify axes
                  repel = TRUE,
                  col.var = "blue", # Variables color
                  col.ind = "#696969",  # Individuals color
                 col.row = dat_1$treatment,
                 arrows=c(T,T),
                 addEllipses = TRUE,
                 col.col = "black",
                 title = "1 year Symetric CA biplot (axes 1 and 2)"

  )

#5 years after treatment
ca_5.p.1.2 =
  fviz_ca_biplot(ca_5,
                 # axes = c(1, 2), # specify axes
                 repel = TRUE,
                 col.var = "blue", # Variables color
                 col.ind = "#696969", # Individuals color
                 col.row = dat_5$treatment,
                 arrows=c(T,T),
                 addEllipses = TRUE,
                 col.col = "black",
                 title = "5 years Symetric CA biplot (axes 1 and 2)"

  )

#axis 1 and 3:

#1 year after treatment:

ca_1_2.p.1.3 =
   fviz_ca_biplot(ca_1,
                   axes = c(1, 3), # specify axes
                   repel = TRUE,
                   col.var = "#2E9FDF", # Variables color
                   col.ind = "#696969",  # Individuals color
                   col.row = dat_5$treatment,
                   col.col = "black",
                  arrows=c(T,T),
                  addEllipses = TRUE,
                  title = "1 year Symetric CA biplot (axes 1 and 3)",


   )



#5 years after treatment

ca_5_2.p.1.3 =
  fviz_ca_biplot(ca_5,
                 axes = c(1, 3), # specify axes
                 repel = TRUE,
                 col.var = "#2E9FDF", # Variables color
                 col.ind = "#696969" , # Individuals color
                 col.row = dat_5$treatment,
                 col.col = "black",
                 arrows=c(T,T),
                 addEllipses = TRUE,
                 title = "5 years Symetric CA biplot (axes 1 and 3)"

  )

#plot of sites + 95% CI groupings # ## group by treatment

#Axis 1 and 2

veg.ca_1.symbiplot.1.2 = fviz_ca_biplot(ca_1,
                                        labelsize = 2,
                                        map ="symbiplot",
                                        geom.col = "text",
                                        geom.row = "point",
                                        label = "all",
                                        geom = "text",
                                        lablesize = 1,
                                        repel=T,
                                        col.row=dat_1$treatment,
                                        col.col = "black",
                                        arrows=c(T,T),
                                        addEllipses = TRUE,
                                        ellipse.type="confidence",
                                        legend.title = "Treatment type",
                                        axes=c(1,2),
                                        title = "1 year Symetric CA biplot (axes 1 and 2)")

veg.ca_1.symbiplot.1.2 + theme_bw() + geom_text(label=veg.ca_1.symbiplot.1.2[["data"]][["name"]],
                                                              position = position_dodge(width= 0.9,), size= 3, angle = 60)


veg.ca_5.symbiplot.1.2 = fviz_ca_biplot(ca_5,
                                        labelsize = 2,
                                        map ="symbiplot",
                                        geom.col = "text",
                                        geom.row = "point",
                                        label = "all",
                                        geom = "text",
                                        lablesize = 1,
                                        repel=T,
                                        col.row=dat_1$treatment,
                                        col.col = "black",
                                        arrows=c(F,T),
                                        addEllipses = T,
                                        ellipse.type="confidence",
                                        legend.title = "Treatment type",
                                        axes=c(1,2),
                                        title = "5 year Symetric CA biplot (axes 1 and 2)")

veg.ca_5.symbiplot.1.2 + theme_bw() + geom_text(label=veg.ca_5.symbiplot.1.2[["data"]][["name"]],
                                                           position = position_dodge(width=1),  size= 3, angle = 50)


#Axis 1 and 3:
veg.ca_1.symbiplot.1.3 = fviz_ca_biplot(ca_1,
                                      labelsize = 2,
                                      map ="symbiplot",
                                      geom.col = "text",
                                      geom.row = "point",
                                      lablesize = 1,
                                      repel=T,
                                      col.row=dat_5$treatment,
                                      col.col = "black",
                                      arrows=c(T,T),
                                      addEllipses=T,
                                      ellipse.type="confidence",
                                      stat_conf_ellipse=T,
                                      legend.title = "Treatment type",
                                      axes=c(1,3),
                                      title = "1 year Symetric CA biplot (axes 1 and 3)")
veg.ca_1.symbiplot.1.3 + theme_bw() + geom_text(label=veg.ca_1.symbiplot.1.2[["data"]][["name"]],
                                                            size= 3, angle = 50)


veg.ca_5.symbiplot.1.3 = fviz_ca_biplot(ca_5,
                                        labelsize = 2,
                                        map ="symbiplot",
                                        geom.col = "text",
                                        geom.row = "point",
                                        lablesize = 1,
                                        repel=T,
                                        col.row=dat_5$treatment,
                                        col.col = "black",
                                        arrows=c(T,T),
                                        addEllipses=T,
                                        ellipse.type="confidence",
                                        stat_conf_ellipse=T,
                                        legend.title = "Treatment type",
                                        axes=c(1,3),
                                        title = "5 year Symetric CA biplot (axes 1 and 3)")

veg.ca_5.symbiplot.1.3 + theme_bw() + geom_text(label=veg.ca_5.symbiplot.1.3[["data"]][["name"]],
                                                             position = position_dodge(width=1),  size= 3, angle = 50)


#### 5. check weight of outliers #### = no outliers - maybe examine Valencia Forest

# specify outliers #
outliers = c("")

# remove outliers #
#d4 = d3[!(row.names(d2) %in% outliers),]

# re-compute ca #
#ca2 <- CA(dat_3, graph = FALSE)

# re-plot ca #
#fviz_pca_biplot(ca2,
                #axes = c(1, 2), # specify axes
                #repel = TRUE,
                #col.var = "blue", # Variables color
                #col.ind = "#696969"  # Individuals color
#)
# examine weather to keep outliers based on analysis after removing them.

#### 6. (actually 1!) decide which axes to retain ####

### YOU SHOULD CHOOSE WHICH RULE TO FOLLOW A-PRIORI!!! ###
# Possible a priori rules:
# a)  1 eigenvalue rule: values of 1 indicate that those axes account for more variance than any one original variable (standardized data only). Rule is to retain all axes with an eigenvalue of 1 or more
# b) % total variance rule: retain all axes for which % variance explained sums to > X% (e.g., 90%)
# c) % variance rule: retain all axes for which at least X% variance is explained (e.g., 10%)
# d) Broken stick rule: retain all axes before break in scree plot


fviz_eig(ca_1, addlabels = TRUE, choice = c("eigenvalue")) # as scree plot with eigenvalues
fviz_eig(ca_1, addlabels = TRUE, choice = c("variance")) # as scree plot with % variance explained

#### 7. interpret axes ####

# extract results
row = get_ca_row(ca_1)
row

# extract contribution of each variable to each PC (unit = %) #
contrib = (row$contrib)
contrib

# plot contribution #

corrplot(contrib, is.corr = F)

# axis 1: For Dimension 1, the largest contributers are Alameda, Ohkay Owingeh, and Valencia Forest

# axis 2: For Dimension 2,

# axis 3:


#### 8. extract scores ####

ca.scrs = as.data.frame(ca$x[,1:3])
ca.scrs
