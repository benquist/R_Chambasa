
######################
# Brian J. Enquist
# R for CHAMBASA - Peru Campaign
# BIEN traits for Chambasa
#####################


# Read in file
setwd("/Users/benquist/R_CHAMBASA/")
data.in <- read.csv(file = "compiled_traits_Peru.csv")
names(data.in)
head(data.in)
dim(data.in)
tail (data.in)
summary(data.in)
sapply(data.in, class)
table(data.in$Plot)
table(data.in$Plot, !is.na(data.in$X))
table(data.in$Plot, !is.na(data.in$LNC))
table(data.in$Plot, !is.na(data.in$LNP))
table(data.in$Plot, !is.na(data.in$d13C))


unique(data.in$plot_code)
#number of tree observations in CHAMBASA plots
dim(data.in)
#number of genus-level traits
summary(data.in$d13C)
summary(data.in$SLA)
summary(data.in$WoodDensity)
summary(data.in$LCC)
summary(data.in$LNC)
summary(data.in$LPC)

library(dplyr)
library(plyr)
library(reshape2)

melted <- melt(data.in, id.vars=c("Plot", "WoodDensity"))
ddply(melted, c("Plot", "WoodDensity"), summarise,
      mean = mean(value), sd = sd(value),
      sem = sd(value)/sqrt(length(value)))


# this is the script way of installing packages
install.packages ("ggplot2", dependencies = TRUE)
install.packages("dplyr")
install.packages("plyr")
install.packages("ggthemes")
install.packages("reshape2")

#load libraries
library(ggplot2)
library(reshape2)
library(ggthemes)
library(plyr)
library(MASS)

#histograms - all individuals
hist(data.in$d13C)
hist(data.in$WoodDensity)
hist(data.in$Height)
hist(data.in$SLA)
logSLA <- log10(data.in$SLA)
hist(data.in$Height)
hist(data.in$LNC)
hist(data.in$LPC)

data.plots.in  <- data.in |
  filter(cut=="Plots") |
  select(SLA, WoodDenisity, LNC)
head(data.plots.in )


data.in %>%                                        # Start with the 'diamonds' dataset
  filter(cut == "Plot") %>%                        # Then, filter down to rows where cut == Ideal
  ggplot(aes(x=color,y=price)) +                     # Then, plot using ggplot
  geom_boxplot() 


#basic bivariate plot
logSLA <- log10(data.in$SLA)
logLNC <- log10(data.in$LNC)
myplot <- ggplot(data=data.in, aes(x = log(SLA), y = log(LNC))
summary(myplot)
myplot + geom_point() 

#color bivariate plot by plot
ggplot(data.in, aes(log(SLA), log(LNC), color=Plot)) + geom_point()  # note there are some really high LNC values
ggplot(data.in, aes(log(SLA), log(LPC), color=Plot)) + geom_point()
ggplot(data.in, aes(log(SLA), log(WoodDensity), color=Plot)) + geom_point()
ggplot(data.in, aes(log(SLA), log(SeedMass), color=Plot)) + geom_point()


#boxplot of trait values
library(MASS)
ggplot(data.in, aes(factor(Plot), LNC))+ geom_boxplot()
ggplot(data.in, aes(factor(Plot), LPC))+ geom_boxplot()

 ggplot(iris, aes(n, p, color=plot_code)) + 
  geom_point() +
  facet_grid(plot_code ~ .) 
