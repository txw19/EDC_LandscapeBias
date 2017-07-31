# rm(list=ls())
library(data.table)

# Read in census catchments
cens <- fread('FINAL_EDC_NHD_Summaries_V6.csv')
cens[, .N]
head(cens)
dim(cens)
# summary(cens)

# Read in sample reaches
samp <- fread('Immediate_Catchment_Summaries_V6.csv')
samp[, .N]
head(samp)
dim(samp)

# remove site names
samp <- samp[, c("SiteNm1","SiteNm2") := NULL]
dim(samp)

# Remove character columns from data tables, since those won't be used for cdf's
cens <- Filter(is.numeric, cens)

dim(cens)
dim(samp)

# Keep columns in samp that are in cens
cols <- colnames(cens)
samp <- samp[, cols, with = FALSE]

# Remove comid columns
cens <- cens[, "COMID"  := NULL]
samp <- samp[, "COMID"  := NULL]

dim(cens)
dim(samp)

# str(samp)
# str(cens)

# Test
# vari1c <- cens[, 1, with=FALSE]
# variECDFc <- ecdf(as.matrix(vari1c))
# vari1s <- samp[, 1, with=FALSE]
# variECDFs <- ecdf(as.matrix(vari1s))
# plot(variECDFc, verticals=TRUE, do.points=FALSE, las=1, lwd=2, axes=F, ylab='', xlab='', main='')
# plot(variECDFs, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown', lty=2, lwd=2)
# axis(side=1,cex.axis=0.5, mgp=c(0,0.5,0),tck= -0.01) 
# axis(side=2,tck= -0.01,  mgp=c(0,0.5,0), cex.axis=0.5, las=1)


# CDF plots
# num of pannels per plot
n.pannel <- 50 
n.plot <- ceiling(ncol(samp)/n.pannel)

for(hh in 1:n.plot){
  pdf(paste('cdf_',hh,'.pdf',sep=''),width=8,height=12)
  
  if(hh<n.plot){
    n.fig=n.pannel #number of pannels per plot
    layout(matrix(1:n.fig,nrow=10,ncol=5,byrow=T))
    par(mar=c(2,4,1,1),oma=c(2,2,1,1))	
  }
  
  if(hh==n.plot){
    n.fig=ncol(samp)-(n.plot-1)*n.pannel
    layout(matrix(c(1:n.fig,rep(0,(n.pannel-n.fig))),nrow=10,ncol=5,byrow=T))
    par(mar=c(2,4,1,1),oma=c(2,2,1,1))
  }
  
  for(pp in 1:n.fig){
    vari1c <- cens[, (hh-1)*n.pannel+pp, with=FALSE]
    variECDFc <- ecdf(as.matrix(vari1c))
    
    vari1s <- samp[, (hh-1)*n.pannel+pp, with=FALSE]
    variECDFs <- ecdf(as.matrix(vari1s))
    
    plot(variECDFc, verticals=TRUE, do.points=FALSE, las=1, lwd=2, axes=F, ylab='', xlab='', main=cols[(hh-1)*n.pannel+pp])
    plot(variECDFs, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown', lty=2, lwd=2)
    axis(side=1,cex.axis=0.5, mgp=c(0,0.5,0),tck= -0.01) 
    axis(side=2,tck= -0.01,  mgp=c(0,0.5,0), cex.axis=0.5, las=1)
    
  }

  dev.off()
}