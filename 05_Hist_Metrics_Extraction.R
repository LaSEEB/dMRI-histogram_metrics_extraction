###########################################################################
# 				 Script 				  #
###########################################################################
# Last edit: 10/11/2021


###########################################################################
# 				 Libraries 				  #
###########################################################################
library(ggplot2)
library(oro.nifti)

###########################################################################
# 				Directories				  #
###########################################################################
i=1

folder <- c('sub-patient001','sub-patient002','sub-patient003','sub-patient004',
            'sub-patient005','sub-patient006','sub-patient007','sub-patient008',
	     ..., 'sub-patient0XX')

# Set Directory -------------------------------------------------------
setwd(paste(c("~/PATH TO WORKING FOLDER/",folder[i]), collapse = ""))
path <- paste(c("~/PATH TO WORKING FOLDER/",folder[i]), collapse = "")

###########################################################################
# 				Subjects				  #
###########################################################################
files=list.files(path = path, pattern = "^sub-patient(.*)nii.gz$", all.files = FALSE, full.names = FALSE, 
recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

###########################################################################
# 				Read Images				  #
###########################################################################
img1 = readNIfTI(files[0], reorient = FALSE) 

###########################################################################
# 				Histograms				  #
###########################################################################
# 1.1 - Set histogram parameters according to the DWI map in study:
bin_width = VALUE 
upp_lim = 1
low_lim = 0

# 1.2 - Set histogram range:
x1 = img1[ (img1 > 0.0001) & (img1 < upp_lim) ]

# 1.2 - Generate histogram:
x <- hist( x1, breaks = seq(from = low_lim, to = upp_lim, by = bin_width), plot = FALSE)

# 1.3 - Normalize histogram:

# 1.3.1 - Relative frequency:
x$counts = x$counts/sum(x$counts)

# 1.3.2 - Cumulative sum:
xsum = cumsum(x$counts)

# 1.3.3 - Normalized histogram:
x$counts = x$counts/(sum(x$counts) * bin_width)

###########################################################################
# 			Extrating Histogram Metrics			  #
###########################################################################
# Median (Green) ----------------------------------------------------------
Me1=median(x1)

ix=1
for (ax in xsum) {
  if ( ax <= 0.5){ix=ix+1}
  else{
    print(ix)
    break
  }
}
Me1x=x$mids[ix]

# Peak Height (Grey) ------------------------------------------------------
i1 <- which.max(x$density)
height1 = x$counts[i1]
height1

x$counts=x$counts/(sum(x$counts)*w1)
height1x = x$counts[i1]
height1x

# Peak Value (Yellow) -----------------------------------------------------
pos1 = x$mids[i1]
pos1

# Peak Width (Pink) - difference between the 5th and 95th percentil --------
ae1 = quantile(x1, c(.05,.95))
width1= (ae1[2] - ae1[1])
width1

ix=1
for (ax in xsum) {
  if ( ax <= 0.05){
    ix=ix+1}
  else{
    print(ix)
    break
  }
}
iix=1
for (ax in xsum) {
  if ( ax <= 0.95){
    iix=iix+1}
  else{
    print(iix)
    break
  }
}
q1 = c(x$mids[ix],x$mids[iix])
width1x= (q1[2] - q1[1])
width1x


###########################################################################
# 				   Plots				  #
###########################################################################

# 1.1 - Plot parameters:
line = 2
cex = 1.5
adj  = 0.95

MAP="FA"
Mask = "TBSS"

if (MAP == "FA"){i=1; abc="FA"} else {i=0.0025; abc=bquote("MD (" * mm^2*s^-1*")")}
if (MAP == "FA"){j=3.5; abc="FA"; m=4;l=3} else {l=5000;j=5000; abc=bquote("MD (" * mm^2*s^-1*")"); m=1000}

plot(x, ylim=c(0,j), xlim=c(0,i), main=Mask, ylab=MAP, xlab=NULL, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, col="grey")
title(outer=FALSE,adj=adj,col="black",font=1,line=line)
box(col="gray")
abline(v=c(q1[1],q1[2]),col="red", lwd=5, lty=2)
abline(v=Me1x,col="darkblue", lwd=5, lty=2)
abline(h=height1x,col="pink", lwd=5, lty=2)
abline(v=pos1,col="green", lwd=5, lty=2)
