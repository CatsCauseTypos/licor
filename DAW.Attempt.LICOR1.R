#!/usr/bin/perl 
use strict; use warnings;

################################################################
# Author:Donnelly West 
# Email: Donwest@UCDdavis.edu
# Date: 4-17-15
# Script Name:  LICOR Trail A 	
# Version: 1.0
#
# <brief description of script's function>
# Trying to find differences between treatment groups from experiment "Trial A" in GC418 

#######################################################################################
#Steven help: What do you want to look at?
#me: photosynthesis in treatments
#Him: stomatal conductance, leaf temp vs chamber, transpiration
#me: leaf temp is cool (lol)

#set working directory to file location (click on location in side window, then click on "more" set working directory)
#you can make a 'project' - upper right corner. it will make you a folder wherever and if you open it, it will automatically change your working directory to that folder - SO FANCY

data1=read.csv("daw-mar-3-2015.csv",header=T)
data2=read.csv("daw-mar12015-trial-a.csv")
#how do you want to break it up? species then control vs treatments
head(data1)
#brackets = row and column -- in that order
m82=data1[grepl("m82", data1$Geno),] #last comma tells it to take all columns 
penn=data1[grepl("pen", data1$Geno),] #remember, you can use partial names for it 
#say you wanted both... data_whatever = data1[grepl("m82|pen", data1$Geno),]
all=data1[grepl("m82|pen", data1$Geno),] #both species
controls=all[grepl("control",all$Treat),]
oops=data1[data1[,c("Geno","Treat")]=="oops",] #what if I wanted ALL the oops? I have "oops" in both genotype and treatment columns

#Up first: box plot!

ggplot(m82,aes(Treat,Photo))+geom_boxplot()

ggplot(penn,aes(Treat,Photo))+geom_boxplot()


#Steve's code to make bar graphs
plot.summary=function(x) {
  x=na.omit(x)
  y=mean(x)
  sem=sd(x)/sqrt(length(x))
  return(data.frame(
    y=y,
    ymin=y-sem,
    ymax=y+sem
  ))} #creates a function (from Julin)

)+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")
#gets tagged on the end of a ggplot if you want to make bar graphs!

ggplot(penn,aes(Treat,Photo,fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")

ggplot(m82,aes(Treat,Photo, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")

#to see them all:
ggplot(all,aes(Treat,Photo, fill=Geno))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")
#nope
#there's a way.....YAY
ggplot(all,aes(Treat,Photo, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)
#This splits them by genotype, but at least you can see the darn things

#scatter plot related to photosynthesis
head(all)
#PhiPS2 is quantum efficiency - photons received going into PHOTOSYS2; PhiCO2 mole of fixed CO2 per mole of photons

ggplot(all,aes(PhiPS2,PhiCO2, fill=Geno))+geom_point()+stat_smooth(method="lm") 
#linear, up angle== can stil utilize the photons coming in to fix carbon

#PARi (par in) vs photo will tell you light response curve which is how you justify the light intensity at which you measured (or it can be how)
#just checking on controls per species... dang. that's ugly.
ggplot(controls,aes(PhiPS2,PhiCO2, fill=Geno))+geom_point()+stat_smooth(method="lm") 

#bar graph for leaf temp
ggplot(all,aes(Treat,Tleaf, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)

#bar graph transpiration vs treatment
ggplot(all,aes(Treat,Trans, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)

#conductance
ggplot(all,aes(Treat,Cond, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)
#transpiration / conductance match (and probablly should)

#internal CO2 with treatment
ggplot(all,aes(Treat,Ci, fill=Treat))+stat_summary(fun.data=plot.summary,geom="bar")+stat_summary(fun.data=plot.summary,geom="errorbar")+facet_wrap(~Geno)

#steven says the M82 isn't surprising - it's still uptaking CO2, it's just stopped fixing. In fact, it ain't fixin' well in general.

#giant scary bunch of graphs

#photo vs transpiration
ggplot(all, aes(Photo, Trans))+geom_point()+facet_grid(Geno~Treat)+stat_smooth(method="loess", se=F) #can use loess for polynomial and lm for linear 

ggplot(all, aes(Photo, Tleaf))+geom_point()+facet_grid(Geno~Treat)+stat_smooth(method="lm")
