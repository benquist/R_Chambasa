
######################
# Brian J. Enquist
# R for CHAMBASA - Peru Campaign
# Accessing CHAMBASA data via database connection
#####################

# install postgreSQL, install.packages("foo", dependencies=...)
install.packages("RPostgreSQL")
install.packages("devtools")
library(devtools)
library(gemtraits)

# first thing that you do is to create a connection with the database
# type con = connect_gemtraits_db()
con = connect_gemtraits_db()

#queries run by commands in the R file db_queries.r
trees_crowns = get_trees_with_crowns(con)
LeafChem = get_leaf_chemistry(con)
WoodDensity = get_wood_density(con)
LeafParts = get_leaf_parts(con)

#data summar
head(LeafParts)
head(LeafChem )
class(LeafChem)
names(LeafChem)
dim(LeafChem)

#summarize number of observed datapoints = True and NAs = False
table(LeafChem$plot_code, !is.na(LeafChem$n_percent))
unique(LeafChem$plot_code)


#summary(a)           # gives min, max, mean, median, 1st & 3rd quartiles
#min(a); max(a)       # }
#range(a)             # } self-explanatory
#mean(a); median(a)   # }
#sd(a); mad(a)        # standard deviation, median absolute deviation
#IQR(a)               # interquartile range
#quantile(a)          # quartiles (by default)
#quantile(a, c(1, 3)/4)  # specific percentiles (25% & 75% in this case)

#summarize data
hist(LeafChem$n15_delta)
hist(LeafChem$chl_b)
hist(LeafChem$delta_13c)

#save as csv
write.csv(trees_crowns, file="tree_crowns.csv")

# install.packages ("ggplot2", dependencies = TRUE)
install.packages("plyr")
install.packages("ggthemes")
install.packages("reshape2")

#load libraries
library(ggplot2)
library(reshape2)
library(ggthemes)
library(plyr)

#basic bivariate plot
myplot <- ggplot(data=LeafChem, aes(x = p, y = n))
summary(myplot)
myplot + geom_point() 

#photosynthesis we need to subset the 
# sunleaves <- subset(pm_type

#color bivariate plot by plot
ggplot(LeafChem, aes(log(n), log(p), color=plot_code)) + geom_point()
ggplot(LeafChem, aes(lma, n_percent, color=plot_code)) + geom_point()
ggplot(LeafChem, aes(lma, p_corrected_percent, color=plot_code)) + geom_point()

ggplot(LeafChem, aes(log(photosynthesis), log(n_percent), color=plot_code)) + geom_point()

ggplot(LeafChem, aes(lma, n_percent, color=plot_code)) + geom_point()


#boxplot of trait values
library(MASS)
ggplot(LeafChem, aes(factor(plot_code), n))+ geom_boxplot()
ggplot(LeafChem, aes(factor(plot_code), p))+ geom_boxplot()

ggplot(iris, aes(n, p, color=plot_code)) + 
  geom_point() +
  facet_grid(plot_code ~ .) 


# From Lisa
library(smatr)
library(plyr)
#join <- read.csv("photo_db_join.csv")

Amax<-subset(LeafChem, (pm_type %in% "AMAX"))
Amax_sun<-subset(Amax, (sun_shade %in% "SUN"))

tests = list()
for (p in unique(Amax_sun$plot_code)) {
  this_plot = subset(Amax_sun, plot_code == p)
  print(paste("Results for Plot ",p))
  test1<-sma(photosynthesis~n_percent,data = this_plot,log="xy",na.omit=T)
  tests[[p]] = test1
  plot(test1)
  summary(test1)
}

Amax_shade<-subset(Amax, (sun_shade %in% "SHADE"))
tests = list()
for (p in unique(Amax_shade$plot_code)) {
  this_plot = subset(Amax_shade, plot_code == p)
  print(paste("Results for Plot ",p))
  test1<-sma(photosynthesis~n_percent,data = this_plot,log="xy",na.omit=T)
  tests[[p]] = test1
  plot(test1)
  summary(test1)
}


