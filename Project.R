rm(list=ls())
getwd()
setwd("SDM")
library(rio)
library(stargazer)
library(moments)
library(car)
library(corrplot)
library(readxl)


# Importing the data

life = import("Project_data.xlsx",sheet = "Lifesatisfaction")
hhincome = import("Project_data.xlsx",sheet = "HouseholdIncome")
gdp = import("Project_data.xlsx",sheet = "GDP")
health = import("Project_data.xlsx",sheet = "Health Index")
crime = import("Project_data.xlsx",sheet = "Crime Rate")
qol = import("Project_data.xlsx",sheet = "Quality of Life")
unemp = import("Project_data.xlsx",sheet = "Unemployment")
livingcost = import("Project_data.xlsx",sheet = "Cost of Living")
pollution = import("Project_data.xlsx",sheet = "Pollution")
pop = import("Project_data.xlsx",sheet = "Population Density")
afford = import("Project_data.xlsx",sheet = "Affordability")
continent = import("Project_data.xlsx",sheet = "Continent")

# Column names to lowercase

colnames(life)=tolower(make.names(colnames(life)))
colnames(hhincome)=tolower(make.names(colnames(hhincome)))
colnames(gdp)=tolower(make.names(colnames(gdp)))
colnames(health)=tolower(make.names(colnames(health)))
colnames(crime)=tolower(make.names(colnames(crime)))
colnames(qol)=tolower(make.names(colnames(qol)))
colnames(unemp)=tolower(make.names(colnames(unemp)))
colnames(livingcost)=tolower(make.names(colnames(livingcost)))
colnames(pollution)=tolower(make.names(colnames(pollution)))
colnames(pop)=tolower(make.names(colnames(pop)))
colnames(afford)=tolower(make.names(colnames(afford)))
colnames(continent)=tolower(make.names(colnames(continent)))


# Merging a dataset and reindexing the continent column to the left

lifesatisfaction <- merge(life,continent, by = "country", all.life = TRUE)
#View(lifesatisfaction)
lifesatisfaction <- lifesatisfaction[, c(3,1,2)]
#View(lifesatisfaction)

summary(ls)

# Merging all the data sets using left join. Join condition is by country and we used left join to get the values of all the countries for which
#we have the life satisfaction rate

library(tidyverse)
ls = list(lifesatisfaction,hhincome,gdp,health,crime,qol,unemp,livingcost,pollution,pop,afford) %>% reduce(left_join, by = "country")
#View(ls)

#Structure of the dataframe

str(ls)


#Counting the NA values in each column

map(ls, ~sum(is.na(.)))


# Making the response variable categorical

ls$life.satisfaction.level <- ifelse(ls$life.satisfaction<4, "Low","other")
ls$life.satisfaction.level <- ifelse(ls$life.satisfaction.level== "other" & ls$life.satisfaction >=4 & ls$life.satisfaction <6 , "Medium",ls$life.satisfaction.level)
ls$life.satisfaction.level <- ifelse(ls$life.satisfaction.level== "other" & ls$life.satisfaction >=6 , "High",ls$life.satisfaction.level)

#View(ls)

#Countries in each categor based on the Life Satisfaction Rate 

library(plyr)

lslevels <- table(ls$life.satisfaction.level)


plot(lslevels, main = " Countries in each categor based on the Life Satisfaction Rate ")

# Reindexing the columns

ls <- ls[, c(1,2,3,20,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
#View(ls)

#ls$life.satisfaction.level = factor(ls$life.satisfaction.level)

ls$gdp <- ls$gdp/10^10


# Data Visualizations and Descriptive Analysis


ls[which.min(ls$health.index),]

par(mfrow=c(2,3))
plot(ls_gdp_cat$life.satisfaction~ ls_gdp_cat$crime.index.,main = " Life Satisfaction Rate vs Crime Index")
plot(ls_gdp_cat$life.satisfaction~ ls_gdp_cat$health.index,main = " Life Satisfaction Rate vs Health Index")
plot(ls_gdp_cat$life.satisfaction~ ls_gdp_cat$pollution,main = " Life Satisfaction Rate vs Pollution")
plot(ls_gdp_cat$life.satisfaction~ ls_gdp_cat$unemployement.rate,main = " Life Satisfaction Rate vs Unemployment Rate")
plot(ls_gdp_cat$life.satisfaction~ ls_gdp_cat$climate.index,main = " Life Satisfaction Rate vs Climate Index")
plot(ls_gdp_cat$life.satisfaction~ ls_gdp_cat$affordability.index,main = " Life Satisfaction Rate vs Population Density")



# Checking the null values in each variable

#install.packages("mice")
#install.packages("VIM")
library(VIM)
library(mice)
md.pattern(ls)
aggr_plot <- aggr(ls, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(ls),ylim = c(0,0.5), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


#K MEANS CLUST IMPUTE for missing values

#Extracting the numerical columns for Clust Impute

ls1 = subset(ls, select = -c(1,2,3,4) )
lifesatisfaction_cat = subset(ls, select = c(1,2,3,4) ) 
#View(ls1)
#View(lifesatisfaction_cat)


nr_iter <- 10 # iterations of procedure
n_end <- 10 # step until convergence of weight function to 1
nr_cluster <- 7# number of clusters
c_steps <- 50 # numer of cluster steps per iteration
#install.packages("ClustImpute")

library(ClustImpute)
ls2 <- ClustImpute(ls1,nr_cluster=nr_cluster, nr_iter=nr_iter, c_steps=c_steps, n_end=n_end, wf = default_wf) 
View(ls2)

str(ls2)

class(ls2)
  
#View(ls2$complete_data)

ls_full <- cbind(lifesatisfaction_cat,ls2$complete_data)
#View(ls_full)
aggr_plot <- aggr(ls_full, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(ls_full), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))



#Correlation plot

xx=cor(ls2$complete_data)
corrplot(xx,method="ellipse",type="lower")
corrplot(xx,method="number",type="lower")



#Ordinal Logistic Regression

View(ls_full)

ls_full$life.satisfaction.level.ordered <- factor(ls_full$life.satisfaction.level, levels=c("Low", "Medium", "High"), ordered=TRUE) 
library(MASS) 
attach(ls_full)


ordinal1 <- polr(life.satisfaction.level.ordered ~ gdp + climate.index + pollution +ls_full$crime.index+health.index+
             ls_full$affordability.index , data=ls_full, Hess=TRUE)
summary(ordinal1)


 
stargazer(ordinal1,type="text")


ls_gdp_cat <- ls_full

#install.packages("binr")
library(binr)

#install.packages("dlookr")
library(dlookr)

#Quantile based binning is a good strategy to use for adaptive binning.
#Quantiles are specific values or cut-points which help in partitioning the continuous valued distribution of a 
#specific numeric field into discrete contiguous bins or intervals.

ls_gdp_cat$gdpbins <- binning(ls_gdp_cat$gdp, nbins = 3,
               labels = c("Low", "Medium", "High"), type = "quantile")
View(ls_gdp_cat)

ls_gdp_cat <- ls_gdp_cat %>% filter(country != c('United States')) %>% filter(country
                                                                                != c('China'))

#Standardization of values for better training

ls_gdp_cat <- ls_gdp_cat %>% mutate_at(c("medianpercapitaincome","medianhouseholdincome","health.index", "gdp","crime.index.","safety.index","traffic.commute.time.index",
                                           "climate.index",
                                           "unemployement.rate","cost.of.living.plus.rent.index","groceries.index","restaurant.price.index",
                                           "pollution","density.pop..mi2","affordability.index","population"), ~(scale(.) %>% as.vector))


#Ordinal logistic Regression with Random effects

install.packages("ordinal")
library(ordinal)
attach(ls_gdp_cat)
fm1 <- clmm2(life.satisfaction.level.ordered ~  continent +health.index + pollution + crime.index.+
               unemployement.rate+ affordability.index+density.pop..mi2, random = gdpbins, data=ls_gdp_cat, Hess=TRUE)
summary(fm1)


ls_gdp_cat$continent <- as.factor(ls_gdp_cat$continent)


fm2 <- clmm2(life.satisfaction.level.ordered ~  gdpbins +health.index + pollution + crime.index.+
               unemployement.rate+ affordability.index+density.pop..mi2, random = continent, data=ls_gdp_cat, Hess=TRUE)
summary(fm2)

levels(ls_gdp_cat$continent)

View(ls_gdp_cat)

# Pool MOdel vs Multilevel models 

library(lme4)


lm <- lm(life.satisfaction ~  gdpbins + health.index + pollution +ls_gdp_cat$crime.index+climate.index+cost.of.living.plus.rent.index+
           unemployement.rate+ density.pop..mi2 , data=ls_gdp_cat)
summary(lm)


random1 <- lmer(life.satisfaction ~ gdpbins+ health.index + pollution +ls_gdp_cat$crime.index+climate.index+
                  unemployement.rate + density.pop..mi2+ (1+1|continent), data=ls_gdp_cat,   REML=FALSE)
summary(random1)


random2 <- lmer(life.satisfaction ~  health.index + pollution +ls_gdp_cat$crime.index+climate.index+cost.of.living.plus.rent.index+
                  unemployement.rate+ density.pop..mi2+ (1|continent)+ (1+1|gdpbins), data=ls_gdp_cat,   REML=FALSE)
summary(random2)



random3 <- lmer(life.satisfaction ~  gdpbins + health.index + pollution +ls_gdp_cat$crime.index+climate.index+
                  cost.of.living.plus.rent.index+
                  unemployement.rate+ density.pop..mi2+ (1|continent) , data=ls_gdp_cat,   REML=FALSE)
summary(random3)



stargazer(lm,ordinal1,random1,random2,random3, title="Results of OLS vs Ordinal vs Fixed effects", align=TRUE,type="text")




ls_da1 = subset(ls_gdp_cat, select = -c(1,2,6,5,4,7,11,12,16,17) )

summary(ls_da)




ls_da1 <- subset(ls_da1,select = -c(11,12))


labels <- paste(c("Life satisfaction", 
                  "GDP",
                  "Health Index",
                  "Crime Index",
                  "Climate Index",
                  "Unemployment Rate",
                  "Cost of Living",
                  "Pollution",
                  "Population Density",
                 "Affordability Index" ))

boxplot(ls_da1, xaxt = "n",  xlab = "")

# x axis with ticks but without labels
axis(1, labels = FALSE)

# Plot x labs at default x position
text(x =  seq_along(labels), y = par("usr")[3] - 1, srt = 30, adj = 1,
     labels = labels, xpd = TRUE)


# Quality Checks

plot(random3, main = " Residual vs Fitted Values to test the Equality of Variances")

 ggplot(data.frame(eta=predict(random3,type="link"),pearson=residuals(random3,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()


qqnorm(residuals(random3))
abline(0,1)


plot(resid(random3),ls_gdp_cat$life.satisfaction,main =  " Residuals vs Life Satisfaction Rate")


plot(factor(ls$country),ls$crime.index.)
