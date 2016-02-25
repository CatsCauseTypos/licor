################################################################
# Author:Donnelly West 
# Email: Donwest@UCDdavis.edu
# Date:  12/9/15
# Script Name:  LICOR Pooled
# Version: 1.0
#
# <brief description of script's function>
# Trying to find differences between treatment groups from pooled data from GC418 
#Steven Rowland & Mirko Ledda helped SO MUCH with this code

######################################################################################
#steven's suggestion to fix the error bars
options(stringsAsFactors = FALSE)
#still no error bars - turns out, I had left a bunch of licor file junk in my cv and that was freaking out R

#Function for error bars (thanks Julin!)
plot.summary=function(x) {
  x=na.omit(x)
  y=mean(x)
  sem=sd(x)/sqrt(length(x))
  return(data.frame(
    y=y,
    ymin=y-sem,
    ymax=y+sem
  ))} 
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#ALL OF THE LIBRARIES
library(ggplot2); library(reshape2); library(reshape) ; library(dplyr) ; library(cowplot)

#File time
#(it's actually easier to read this in via import data set - it assigns the classes correctly; dunno why the read in code below makes everything a character, but it does!)
data1=read.csv("~/Downloads/Pooled_LICOR_R.csv",header=T)

head(data1)

# using sapply because as.numeric kept failing for whatever reason
?sapply

class(data1$Tleaf) #this needs to be numeric
lapply(data1, class) #drat, most are wrong

# fix 'em
data1$Photo <- sapply(data1$Photo, as.numeric) #fixing class for photosynthetic column
data1$Tleaf <- sapply(data1$Tleaf, as.numeric) #fixing class for tleaf
data1$Treat <- as.factor(data1$Treat) #fixing class for treatment
data1$PhiPS2 <- sapply(data1$PhiPS2, as.numeric) #fixing class 
data1$PhiCO2 <- sapply(data1$PhiCO2, as.numeric) #fixing class 
data1$Geno <- as.factor(data1$Geno) #fixing class for geno
## Grabbing the data that matters

# remember: (brackets = row and column -- in that order)

m82=data1[grepl("m82", data1$Geno),] #last comma tells it to take all columns 

controls=data1[grepl("control",data1$Treat),]

penn=data1[grepl("penn", data1$Geno),] 

# this comes in handy later - let's get the avg temps of leaves:

m82$avgTleaf <- round(m82$Tleaf)
penn$avgTleaf <- round(penn$Tleaf)
data1$avgTleaf <- round(data1$Tleaf)
#just checking
head(controls)

#Up first: box plot!

class(data1$Photo) #photo class needs to be numeric for box plot to work

ggplot(data1,aes(Treat,Photo))+geom_boxplot() #basic ; man, penn can almost compensate for m82 in drought and vice versa in waterlogging


## bar graph of treatment vs photosynth
ggplot(m82, aes(code, Photo, fill=Treat)) +
  theme_bw() +
  facet_wrap(~Treat)+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.6)) +
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.2) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("forestgreen", "firebrick3","dodgerblue3")) +
  ylab("Photosynthesis (A)") +
  xlab("Experimental Conditions") +
  theme(axis.title.y=element_text(size=18))+
  theme(axis.text.y=element_text(size=15))+
  theme(axis.title.x=element_text(size=18))+
  theme(axis.text.x=element_text(size=15))+
  theme(strip.text.x=element_text(size=15))+
  ggtitle("M82 Mean Photosynthesis by Treatment: Pooled")+
  theme(title=element_text(size=18))+
  ylim(0,18)
  
