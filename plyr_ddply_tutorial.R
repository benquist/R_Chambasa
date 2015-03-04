########################################
#  BJ Enquist
#  summarizing data with plyr and dplyr
##
########################################

install.packages ("ggplot2", dependencies = TRUE)
install.packages("plyr")
install.packages("ggthemes")
install.packages("reshape2")

#load libraries
library(ggplot2)
library(reshape2)
library(ggthemes)
library(plyr)
library(dplyr)


head(iris)

# one way
ddply(iris, .(Species), numcolwise(mean))
ddply(iris, .(Species), numcolwise(var))


# here is another way
miris <- melt(iris, id.vars="Species")
x <- ddply(miris, .(Species, variable), summarize, mean=mean(value))
dcast(x, Species~variable, value.var="mean")

# yet another way
group_by(iris, Species) %>% summarise_each(funs(mean))


#ddply(iris, .(Species), summarise)
#df <- melt(iris, id)
#dcast(df, Species ~ variable, mean)

# summary statistics for Petiole width by species
ddply(iris, .(Species), summarise,
      Min = min(Petal.Width),
      Q1 = quantile(Petal.Width, .25),
      Med = median(Petal.Width),
      Q3 = quantile(Petal.Width, .75),
      Max = max(Petal.Width)
)

miris <- melt

