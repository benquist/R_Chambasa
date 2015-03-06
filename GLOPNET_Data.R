
#####################
##  Brian J. Enquist
##  base code from https://gist.github.com/dfalster/29412ad974d5c5025923
##  David Falster, Simple example of reproducible research, using global leaf traits database
#####################

install.packages ("xlsx", dependencies = TRUE)
install.packages ("smatr", dependencies = TRUE)
library(xlsx)
library(smatr)

# get data, from publication Wright et al 2004. DOI: [10.1038/nature02403](http://doi.org/10.1038/nature02403)
download.file("http://www.nature.com/nature/journal/v428/n6985/extref/nature02403-s2.xls", "wright-2004.xls")
dat.Wright  <- read.xlsx2("wright-2004.xls", sheetIndex=1, startRow=11, stringsAsFactors=FALSE, check.names=TRUE)

## Clean data
dat.Wright <- dat.Wright[names(dat.Wright) != " "] # Drop blank columns
for(v in c("log.LMA","log.LL"))
  dat.Wright[[v]] <- as.numeric(dat.Wright[[v]])

LMA_plot <- function(LMA,LL, G){
  par(mfrow=c(1,1))
  
  sm1 <- sma(1/LL~LMA*G,log='xy')
  plot(NA, xlim = c(0.01, 2), ylim = c(0.04, 15), log="xy", type='n', xlab="", ylab="")
  plot(sm1, add= T, col=make.transparent("grey", 1), lwd=0, pch= 19, cex=1.2)
  plot(sm1, add= T, col="darkgreen", type='l', lwd=2.0, p.lines.transparent =0.15)
  
  title(paste0(length(unique(G)), " sites, ", length(!is.na(LMA) &!is.na(LL) ), " species") )
  
  mtext("Leaf mass per area (kg/m2)", line =3, side = 1, cex=1.5)
  mtext("Leaf turnover rate (/yr)", line =3,side = 2, cex=1.5)
}


## Make colours semitransparent:
make.transparent <- function(col, opacity=0.5) {
  tmp <- col2rgb(col)/255
  rgb(tmp[1,], tmp[2,], tmp[3,], alpha=opacity) 
}

to.pdf <- function(expr, filename, ..., verbose=TRUE) {
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

to.pdf(LMA_plot(G = dat.Wright$Dataset, LL = (10^dat.Wright$log.LL)/12, LMA= 10^dat.Wright$log.LMA/1000)
       , paste0("Wright2004.pdf"), height=6, width =6)


#####
names(dat.Wright)
