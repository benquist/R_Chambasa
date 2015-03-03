##CHAMBASA Workshop
#Oxford, March 3, 2015
#Kyle Dexter, kgdexter@gmail.com

setwd("/Users/benquist/R_CHAMBASA/")
##INSTALL PACKAGES
install.packages("ape",dependencies=TRUE)
install.packages("geiger",dependencies=TRUE)
install.packages("phytools",dependencies=TRUE)
install.packages("caper",dependencies=TRUE)
install.packages("picante",dependencies=TRUE)

##LOAD PACKAGES
library(ape)
library(geiger)
library(phytools)
library(caper)
library(picante)


##IMPORTING AND CHECKING PHYLOCOM PHYLOGENY
#tree reading function from phytools package that is the best way to read in trees straight from phylocom
BCI_phylocom <- read.newick("BCI_phylocom.tre")

#this gets rid of singleton nodes (those with only one descendent) to prepare the tree properly for analyses
BCI_phylocom <- collapse.singles(BCI_phylocom)

#dropping the outgroup
BCI_phylocom <- drop.tip(BCI_phylocom,"Ginkgo_biloba")

#checking to make sure the tree is properly temporally calibrated; as the tree is focused on angiosperms, this age should be somewhere between 135 and 180 myrs
max(branching.times(BCI_phylocom))

#checking if the tree is rooted
is.rooted(BCI_phylocom)

#checking if tree is ultrametric
is.ultrametric(BCI_phylocom)

#checking if tree is binary
is.binary.tree(BCI_phylocom)


##IMPORTING AND CHECKING SEQUENCE-BASED PHYLOGENY
#using the standard import function from ape package
BCI_DNA <- read.tree("BCI_DNAbased.tre")

#dropping the outgroup
BCI_DNA <- drop.tip(BCI_DNA,"Ginkgo_biloba")

#checking the age of the tree
max(branching.times(BCI_DNA))

#we see above the tree does not have branch lengths in terms of millions of years
#we can readily fix that using the following command, to set it to have the same age as the phylocom tree; I will do that using a deprecated function in ape, chronopl
BCI_DNA <- chronopl(BCI_DNA,lambda=0,age.min=162,age.max=162)

#check to make sure that worked
max(branching.times(BCI_DNA))

#checking if the tree is rooted
is.rooted(BCI_DNA)

#checking if tree is ultrametric
is.ultrametric(BCI_DNA)

#checking if tree is binary
is.binary.tree(BCI_DNA)

#so, this sequence-based tree is not binary either (which has to do with how the phylogeny was constructed), and this can be a problem for some analyses; I will 'fix' that by making the tree binary in an arbitrary fasion
BCI_DNA <- multi2di(BCI_DNA)

#check and make sure that worked
is.binary.tree(BCI_DNA)

#there will be an issue with some analyses as there will now be zero-length branches in the phylogeny; I am not going to deal with that yet; ideally one makes the phylogeny using methods that do not give any zero-length branches (e.g. most Bayesian methods)
min(BCI_DNA$edge.length)


##SIMULATING SOME DATA ONTO THE SEQUENCE-BASED PHYLOGENY, AND SEEING HOW IT VERSUS THE PHYLOCOM PHYLOGENY PERFORM IN ESTIMATING THE PARAMETERS OF THAT SIMULATION
BCI_trait <- fastBM(BCI_DNA)

#check phylogenetic signal for this trait
(BCI_trait_K <- phylosig(BCI_DNA,BCI_trait))

#now let's check phylogenetic signal on the phylocom phylogeny
BCI_trait_reordered <- BCI_trait[match(BCI_phylocom$tip.label,names(BCI_trait))]

(BCI_trait_phylocom_K <- phylosig(BCI_phylocom,BCI_trait_reordered))



##LOADING AND CHECKING SOME REAL DATA
#loading phylogeny
Inga_phy <- read.tree("Inga_phylog.tre")

#checking if tree is rooted
is.rooted(Inga_phy)

#checking if tree is binary
is.binary.tree(Inga_phy)

#checking if tree is ultrametric
is.ultrametric(Inga_phy)

#dropping outgroup
Inga_phy <- drop.tip(Inga_phy,"Zygia")

#loading in trait data
Inga_traits <- read.csv("Inga_traits_MD.csv",row.names=1)

#check out trait data briefly
head(Inga_traits)
summary(Inga_traits)

#making sure names match
rownames(Inga_traits)[which(!rownames(Inga_traits)%in%Inga_phy$tip.label)]
Inga_phy$tip.label[which(!Inga_phy$tip.label%in%rownames(Inga_traits))]

#functions written to check match between species in phylogeny and species with trait data
Inga_matched_data <- match.phylo.data(Inga_phy,Inga_traits)

#putting traits in same order as phylogeny tips
Inga_traits <- Inga_traits[match(Inga_phy$tip.label,rownames(Inga_traits)),]

#adding a couple of traits
Inga_traits$CtoN <- Inga_traits$C/Inga_traits$N
Inga_traits$NtoP <- Inga_traits$N/Inga_traits$P

#normalising data for some traits
Inga_traits_mod <- Inga_traits

Inga_traits_mod$Leaf_Area <- log(Inga_traits_mod$Leaf_Area)
Inga_traits_mod$SLA <- log(Inga_traits_mod$SLA)
Inga_traits_mod$SiO2 <- log(Inga_traits_mod$SiO2)
Inga_traits_mod$N <- log(Inga_traits_mod$N)
Inga_traits_mod$P <- log(Inga_traits_mod$P)


##FIRST LETS JUST MAKE SOME PRETTY PICTURES
#mapping leaf area onto the phylogeny to see how closely related species differ or are similar for leaflet area
trait <- Inga_traits_mod$Leaf_Area
names(trait) <- rownames(Inga_traits_mod)
contMap(Inga_phy,trait)

#now doing the same for SLA, but making the tip labels slightly smaller to see the species names more easily
trait <- Inga_traits_mod$SLA
names(trait) <- rownames(Inga_traits_mod)
contMap(Inga_phy,trait,fsize=c(0.8,1))

#now doing this for a trait that has less than full data
trait <- Inga_traits_mod$Max_height_sub
names(trait) <- rownames(Inga_traits_mod)
trait <- na.omit(trait)
temp_phylog <- drop.tip(Inga_phy,Inga_phy$tip.label[which(!Inga_phy$tip.label%in%names(trait))])
contMap(temp_phylog,trait)

#now coloring tips of phylogeny by discrete state
tip_colors <- vector("character",nrow(Inga_traits_mod))
tip_colors[which(Inga_traits_mod$Habitat=="floodplain")] <- "blue"
tip_colors[which(Inga_traits_mod$Habitat=="terra_firme")] <- "red"
tip_colors[which(Inga_traits_mod$Habitat=="neutral")] <- "purple"
tip_colors[which(is.na(Inga_traits_mod$Habitat))] <- "black"
plot.phylo(Inga_phy,tip.color=tip_colors,cex=0.6,edge.width=1.5)

##LETS ASSESS CORRELATION BETWEEN TRAIT AND PHYLOGENY (I.E. TEST FOR PHYLOGENETIC SIGNAL)
#first do this using Blomberg's K
trait <- Inga_traits_mod$SLA
names(trait) <- rownames(Inga_traits_mod)
phylosig(Inga_phy,trait,method="K",test=TRUE)

trait <- Inga_traits_mod$Max_height_sub
names(trait) <- rownames(Inga_traits_mod)
trait <- na.omit(trait)
temp_phylog <- drop.tip(Inga_phy,Inga_phy$tip.label[which(!Inga_phy$tip.label%in%names(trait))])
phylosig(temp_phylog,trait,method="K",test=TRUE)

#testing multiple traits at once (note that I remove the first trait which is not continuous)
multiPhylosignal(Inga_traits_mod[,-1],Inga_phy)

#testing phylogenetic signal for a binary trait (i.e. a categorical variable with two stats)
Inga_traits_mod$Species <- rownames(Inga_traits_mod)
Inga_traits_mod$floodplain <- vector("numeric",nrow(Inga_traits_mod))
Inga_traits_mod$floodplain[which(Inga_traits_mod$Habitat=="floodplain")] <- 1
temp_traits <- Inga_traits_mod[which(!is.na(Inga_traits_mod$Habitat)),]
temp_combo <- comparative.data(Inga_phy,temp_traits,names.col=Species,na.omit=F)
phylo.d(temp_combo,binvar=floodplain)

Inga_traits_mod$terra_firme <- vector("numeric",nrow(Inga_traits_mod))
Inga_traits_mod$terra_firme[which(Inga_traits_mod$Habitat=="terra_firme")] <- 1
temp_traits <- Inga_traits_mod[which(!is.na(Inga_traits_mod$Habitat)),]
temp_combo <- comparative.data(Inga_phy,temp_traits,names.col=Species,na.omit=F)
phylo.d(temp_combo,binvar=terra_firme)

#estimate rate of transition between different states and compare to random expectation
temp_phylog <- drop.tip(Inga_phy,which(is.na(Inga_traits_mod$Habitat)))
trait <- as.character(na.omit(Inga_traits_mod$Habitat))
trait <- as.factor(trait)
names(trait) <- temp_phylog$tip.label

#first figure out which model of transition best fits the data
(model_ER <- fitDiscrete(temp_phylog,trait,model="ER"))
(model_ARD <- fitDiscrete(temp_phylog,trait,model="ARD"))
(model_SYM <- fitDiscrete(temp_phylog,trait,model="SYM"))

#than randomize the tips on the phylogeny and calculate rate of switching, then compare to observed rate
#if observed rate is significantly lower, then there is significant phylogenetic signal
nsims <- 10
random_rate <- vector("numeric",nsims)
for (i in 1:nsims){
  print(i)
  temp_trait <- sample(trait,length(trait),replace=F)
  names(temp_trait) <- temp_phylog$tip.label
  temp_model <- fitDiscrete(temp_phylog,temp_trait,model="ER")
  random_rate[i] <- as.numeric(temp_model$opt[1])
}
obs_rate <- as.numeric(model_ER$opt[1])
hist(random_rate,breaks=20)
abline(v=obs_rate,lty=2,lwd=2,col="red")
length(which(random_rate<obs_rate))/nsims


##ESTIMATING PHYLOGENETIC SIGNAL USING PAGEL'S LAMBDA AND FITTING MODELS OF TRAIT EVOLUTION
#first for SLA
trait <- Inga_traits_mod$SLA
names(trait) <- rownames(Inga_traits_mod)
(model_SLA_lambda <- fitContinuous(Inga_phy,trait,model="lambda"))
(model_SLA_BM <- fitContinuous(Inga_phy,trait,model="BM"))
(model_SLA_white <- fitContinuous(Inga_phy,trait,model="white"))

#compare this to result from phylosig, which uses a likelihood ratio test (and does not compare to a Brownian motion model)
phylosig(Inga_phy,trait,method="lambda",test=T)

#next for max height
trait <- Inga_traits_mod$Max_height_sub
names(trait) <- rownames(Inga_traits_mod)
trait <- na.omit(trait)
temp_phylog <- drop.tip(Inga_phy,Inga_phy$tip.label[which(!Inga_phy$tip.label%in%names(trait))])
trait <- as.numeric(as.character(trait))
names(trait) <- temp_phylog$tip.label
(model_height_lambda <- fitContinuous(temp_phylog,trait,model="lambda"))
(model_height_BM <- fitContinuous(temp_phylog,trait,model="BM"))
(model_height_white <- fitContinuous(temp_phylog,trait,model="white"))
#compare this to result from phylosig, which uses a likelihood ratio test (and does not compare to a Brownian motion model)
phylosig(temp_phylog,trait,method="lambda",test=T)


##WHAT OTHER MODELS OF EVOLUTION CAN WE FIT?
#Orstein-Uhlenbeck where there is an optimal trait value to which species are constrained
trait <- Inga_traits_mod$SLA
names(trait) <- rownames(Inga_traits_mod)
(model_SLA_OU <- fitContinuous(Inga_phy,trait,model="OU"))

#early burst model where there is lots of change early in the phylogeny and then less change
(model_SLA_EB <- fitContinuous(Inga_phy,trait,model="EB"))

#comparing AICc values of different models
model_SLA_lambda$opt$aicc
model_SLA_BM$opt$aicc
model_SLA_white$opt$aicc
model_SLA_OU$opt$aicc
model_SLA_EB$opt$aicc


##CALCULATING RATES OF TRAIT EVOLUTION FOR CONTINUOUS CHARACTERS
#First, let's scale the variables so that they all have same mean and variance
Inga_traits_mod2 <- scale(Inga_traits_mod[,2:10])

#using Brownian rate evolution parameter
trait_rates <- vector("numeric",ncol(Inga_traits_mod2))
names(trait_rates) <- colnames(Inga_traits_mod2)

for (i in 1:length(trait_rates)){
  print(i)
  trait <- Inga_traits_mod2[,i]
  names(trait) <- rownames(Inga_traits_mod2)
  trait <- na.omit(trait)
  temp_phylog <- drop.tip(Inga_phy,Inga_phy$tip.label[which(!Inga_phy$tip.label%in%names(trait))])
  trait <- as.numeric(as.character(trait))
  names(trait) <- temp_phylog$tip.label
  temp_model <- fitContinuous(temp_phylog,trait,model="BM")
  trait_rates[i] <- temp_model$opt$sigsq
}
sort(trait_rates)


##ANCESTRAL STATE RECONSTRUCTION
#For continuous characters we actually did this above when we were making pretty pictures of the phylogeny
#But, let's do it again to see the actual values
trait <- Inga_traits_mod$SLA
names(trait) <- rownames(Inga_traits_mod)
(ace_SLA <- ace(trait,Inga_phy,type="continuous"))
sort(ace_SLA$ace)

#Get descendants for node with highest reconstructed value
tips <- getDescendants(Inga_phy,59)
Inga_phy$tip.label[tips]
sort(trait)
contMap(Inga_phy,trait,fsize=c(0.8,1))


##ANCESTRAL STATE RECONSTRUCTION FOR DISCRETE CHARACTERS
#first maximum likelihood reconstruction
trait <- Inga_traits_mod$Habitat
names(trait) <- rownames(Inga_traits_mod)
trait <- na.omit(trait)
temp_phylog <- drop.tip(Inga_phy,Inga_phy$tip.label[which(!Inga_phy$tip.label%in%names(trait))])
ace_habitat <- ace(trait,temp_phylog,type="discrete",marginal=TRUE)
ace_habitat$lik.anc
colors <- c("blue","purple","red")
plot(temp_phylog, tip.color=colors[as.numeric(trait)])
nodelabels(thermo = ace_habitat$lik.anc, piecol = colors, cex = 0.75)

#then maximum parsimony reconstruction
trait <- as.numeric(trait)
temp_phylog <- unroot(temp_phylog)
ace_mpr <- MPR(trait,temp_phylog,47)
plot(temp_phylog, tip.color=colors[as.numeric(trait)],cex=0.7)
nodelabels(pch=21,bg=colors[ace_mpr[,1]])