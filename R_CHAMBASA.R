
######################
# Brian J. Enquist
# R for CHAMBASA - Peru Campaign
# Accessing CHAMBASA data via database connection
#####################

# install postgreSQL
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

head(LeafParts)
head(LeafChem )
class(LeafChem)
names(LeafChem)
dim(LeafChem)

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

#color bivariate plot by plot
ggplot(LeafChem, aes(n, p, color=plot_code)) + geom_point()
ggplot(LeafChem, aes(lma, n_percent, color=plot_code)) + geom_point()

#boxplot of trait values
library(MASS)
ggplot(LeafChem, aes(factor(plot_code), n))+ geom_boxplot()
ggplot(LeafChem, aes(factor(plot_code), p))+ geom_boxplot()

ggplot(iris, aes(n, p, color=plot_code)) + 
  geom_point() +
  facet_grid(plot_code ~ .) 


