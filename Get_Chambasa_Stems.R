
#######################
# BJEnquist
# extract CHAMBASA stems file to merge with traits
#
################

library(gemtraits)
library(devtools)

con = connect_gemtraits_db()

#queries run by commands in the R file db_queries.r

ChambasaTrees = get_trees(con)
names(ChambasaTrees)
dim(ChambasaTrees)
table(ChambasaTrees$plot_code)
table(data.in$Plot)
View(ChambasaTrees)

ChambasaTreeSizes = get_trees_with_dbh(con)
table(ChambasaTreeSizes$plot_code)
names(ChambasaTreeSizes)
head(ChambasaTreeSizes)

#remove duplicated stems
trees_sm = ChambasaTreeSizes[order(ChambasaTreeSizes$census_id, decreasing = T),]
trees_sm = trees_sm[!duplicated(paste(trees_sm$plot_code, trees_sm$tree_code, sep="-")),]
nrow(trees_sm)

#export file
write.csv(trees_sm, file="Chambasa_tree_stems.csv")
