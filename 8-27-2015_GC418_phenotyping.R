#Date: 8-27-2015
#Project: Plant Cell Fate Plasticity; growth chamber experiments (GC418)
#Author: Donnelly West - DonWest@UCDavis.edu
#Version: 1.0 
#Goal: compare leaf number, flowering, axillary meristems

#Run this first or your error bars won't work:
#Julin's fancy shamancy code
## PHOTO
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
#now you have functions! WOOO!


data <- read.csv("~/Downloads/GC418_Exp1_R.csv")




head(data)



#checking the column classes
sapply(data, class)

#install & load needed libraries
library(lme4)
library(ggplot2)


#Scatter plot 
# scat.plot <- ggplot(hab2, aes(HAB.ab,hab.ad,colour=Parent) )
# scat.plot +geom_jitter()+
#   #scale_color_manual(name="Color Key", values=c("darkorange", "magenta","darkorchid3","deepskyblue2","yellow2","darkseagreen2"))+
#   geom_point(size=4)+
#   theme_bw()+
#   theme(legend.position = "bottom")+
#   theme(legend.direction="horizontal")+
#   ggtitle("Normalized Stomatal Density of S. habrochaites ILs")+
#   ylab("Normalized Adaxial Counts")+
#   xlab("Normalized Abaxial Counts")

#Bar graph showing the avg leaf #

qplot(x=data$num_leaf, y=data$bef_num_leaf, geom="point", colour=data$Condition)


bef.bar <- ggplot(data, aes(reorder(Condition, bef_num_leaf, mean), bef_num_leaf, )) #fill=Genotype))
bef.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
#  scale_fill_manual(name="Color Key", values = c("darkolivegreen1", "chocolate1","cyan","blueviolet"))+
  #theme(legend.position = c(.24,.9))+
  #theme(legend.direction="horizontal")+
  #theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Average # Leaves Before")+
  ylab("Leaves")+
  xlab("Condition")#+
  #coord_cartesian(ylim = c(0, 82))
# avg leaves after 

aftr.bar <- ggplot(data, aes(reorder(Condition, num_leaf, mean), num_leaf, )) #fill=Genotype))
aftr.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  #  scale_fill_manual(name="Color Key", values = c("darkolivegreen1", "chocolate1","cyan","blueviolet"))+
  #theme(legend.position = c(.24,.9))+
  #theme(legend.direction="horizontal")+
  #theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Average # Leaves After")+
  ylab("Leaves")+
  xlab("Condition")

head(data)
 #cant do it bc its not an integer
aftr_flwr.bar <- ggplot(data, aes(reorder(Condition, floral, mean), floral, )) 
aftr.bar+
  theme_bw()+
  stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
  stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
  #  scale_fill_manual(name="Color Key", values = c("darkolivegreen1", "chocolate1","cyan","blueviolet"))+
  #theme(legend.position = c(.24,.9))+
  #theme(legend.direction="horizontal")+
  #theme(axis.text.x=element_text(angle=-90,hjust=0))+
  ggtitle("Average # Leaves After")+
  ylab("Leaves")+
  xlab("Condition")#+






# #Adaxial
# ad.bar <- ggplot(adaxial, aes(reorder(IL,num.stomata, mean), num.stomata, fill=Parent))
# ad.bar+
#   theme_bw()+
#   stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
#   stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
#   scale_fill_manual(name="Color Key", values = c("orange", "dark blue","magenta"))+
#   theme(legend.position = c(.24,.9))+
#   theme(legend.direction="horizontal")+
#   theme(axis.text.x=element_text(angle=-90,hjust=0))+
#   ggtitle("Adaxial Stomatal Density")+
#   ylab("AdaxialStomata")+
#   xlab("Genotypes")+
#   coord_cartesian(ylim = c(0, 18))
# 
# 
# #But which ILs are actually, significantly different than parent 4024? 
# 
# #let's try making our own linear model
# #(PS- it will compare EVERYTHING by THE FIRST THING, so name your stuff appropriately)
# #adaxial
# ad.lm <- lm(num.stomata~IL,adaxial)
# sum.ad.lm <- as.data.frame(summary(ad.lm)$coefficients[,4])
# 
# #abaxial
# ab.lm <- lm(num.stomata~IL,abaxial)
# sum.ab.lm<- as.data.frame(summary(ab.lm)$coefficients[,4])
# 
# #the summary shows us 'what we expect' from looking at the bar graph of the stomatal counts (the ILs that look to have way more/less stomata than lyco parent are coming out as 'significantly different' than 4024)
# 
# #we still need to adjust for false discovery: adjusted p-value
# 
# ad.adj.p <- data.frame(value=p.adjust(summary(ad.lm)$coefficients[,4], method="fdr")) 
# ad.adj.p$IL=rownames(ad.adj.p) #this makes life easier later
# 
# #And again for abaxial
# ab.adj.p <- data.frame(value=p.adjust(summary(ab.lm)$coefficients[,4], method="fdr")) 
# ab.adj.p$IL=rownames(ab.adj.p) 
# 
# 
# #Let's pull out only the ILs with adjusted p values less than .05:
# 
# ad.sigf.p <- as.data.frame(ad.adj.p[which(ad.adj.p$value<.05),])
# write.csv(ad.sigf.p,"significant_adaxial_ILs_hab.csv") #making a file for prosperity
# 
# #again for abaxial
# ab.sigf.p <- as.data.frame(ab.adj.p[which(ab.adj.p$value<.05),])
# write.csv(ab.sigf.p,"significant_abaxial_ILs_hab.csv") 
# 
# #Bar graph with signifigance astrix 
# 
# #adaxial
# #sig: 13,17,18,22,27,32,33,35,43,44,45,49,53,54,58,62,65,66,67,68,94,99,05,08 
# #ad.sig<-
#   ad.bar+
#   theme_bw()+
#   stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
#   stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
#   scale_fill_manual(name="Color Key", values = c("darkorange", "dark blue","magenta"))+
#   theme(legend.position = c(.24,.9))+
#   theme(legend.direction="horizontal")+
#   theme(axis.text.x=element_text(angle=-90,hjust=0))+
#   ggtitle("hab Adaxial Stomatal Density")+
#   ylab("Adaxial Stomata")+
#   xlab("Genotypes")+
#   coord_cartesian(ylim = c(0, 20))#+
# #   annotate(geom="text", label="*",size=9.5,x=37,y=15)+ #3913
# #   annotate(geom="text", label="*",size=9.5,x=38,y=18)+ #3917
# #   annotate(geom="text", label="*",size=9.5,x=36,y=14.9)+ #3918
# #   annotate(geom="text", label="*",size=9.5,x=8,y=5)+ #3922
# #   annotate(geom="text", label="*",size=9.5,x=9,y=5.1)+ #3927
# #   annotate(geom="text", label="*",size=9.5,x=17,y=7)+ #3932
# #   annotate(geom="text", label="*",size=9.5,x=3,y=4.3)+ #3933
# #   annotate(geom="text", label="*",size=9.5,x=13,y=5.9)+ #3935
# #   annotate(geom="text", label="*",size=9.5,x=20,y=8.2)+ #3943
# #   annotate(geom="text", label="*",size=9.5,x=1,y=3.3)+ #3944
# #   annotate(geom="text", label="*",size=9.5,x=12,y=5.9)+ #3945
# #   annotate(geom="text", label="*",size=9.5,x=18,y=7.5)+ #3949
# #   annotate(geom="text", label="*",size=9.5,x=16,y=6.75)+ #3953
# #   annotate(geom="text", label="*",size=9.5,x=11,y=5.98)+ #3954
# #   annotate(geom="text", label="*",size=9.5,x=15,y=6.2)+ #3958
# #   annotate(geom="text", label="*",size=9.5,x=19,y=8)+ #3962
# #   annotate(geom="text", label="*",size=9.5,x=5,y=4.4)+ #3965
# #   annotate(geom="text", label="*",size=9.5,x=10,y=5.35)+ #3966
# #   annotate(geom="text", label="*",size=9.5,x=6,y=4.8)+ #3967
# #   annotate(geom="text", label="*",size=9.5,x=4,y=4.5)+ #3968
# #   annotate(geom="text", label="*",size=9.5,x=35,y=14.6)+ #3994
# #   annotate(geom="text", label="*",size=9.5,x=14,y=6)+ #3999
# #   annotate(geom="text", label="*",size=9.5,x=7,y=4.9)+ #4005
# #   annotate(geom="text", label="*",size=9.5,x=2,y=4.6) #4008
# 
# #abaxial
# #sig: 1777, 13,38,43,44,45,54,66,68,69,94,02,,04,05,10 
# #ab.sig<-
#   ab.bar+
#   theme_bw()+
#   stat_summary(fun.data=plot.summary, geom = "bar", aes(width=.8))+
#   stat_summary(fun.data=plot.summary, geom = "errorbar", width=.4)+
#   scale_fill_manual(name="Color Key", values = c("darkorange", "dark blue","magenta"))+
#   theme(legend.position = c(.24,.9))+
#   theme(legend.direction="horizontal")+
#   theme(axis.text.x=element_text(angle=-90,hjust=0))+
#   ggtitle("hab Abaxial Stomatal Density")+
#   ylab("Abaxial Stomata")+
#   xlab("Genotypes")+
#    coord_cartesian(ylim = c(0, 85))#+
# #   annotate(geom="text", label="*",size=10,x=36,y=69)+ #1777
# #   annotate(geom="text", label="*",size=10,x=28,y=48)+ #3913
# #   annotate(geom="text", label="*",size=10,x=35,y=65)+ #3938
# #   annotate(geom="text", label="*",size=10,x=27,y=47)+ #3943
# #   annotate(geom="text", label="*",size=10,x=31,y=49.9)+ #3944
# #   annotate(geom="text", label="*",size=10,x=30,y=48.3)+ #3945
# #   annotate(geom="text", label="*",size=10,x=24,y=45)+ #3954
# #   annotate(geom="text", label="*",size=10,x=34,y=55.8)+ #3966
# #     annotate(geom="text", label="*",size=10,x=37,y=74.5)+ #3968
# #     annotate(geom="text", label="*",size=10,x=32,y=51)+ #3969
# #     annotate(geom="text", label="*",size=10,x=26,y=47)+ #3994
# #     annotate(geom="text", label="*",size=10,x=25,y=45.5)+ #4002
# #     annotate(geom="text", label="*",size=10,x=33,y=55)+ #4004
# #     annotate(geom="text", label="*",size=10,x=38,y=81)+ #4005
# #     annotate(geom="text", label="*",size=10,x=29,y=50.5) #4010
# #     