## Set working directory to folder with the excel sheet
setwd("C:/Users/Pardis/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/2) AS16 + S26 experiments/Cultures on solid medium/Co-culture picture with bars (day 2)/Final_pyphe_analysis/pyphe_quant")

## Import data from Pyphe .csv file
library("readxl")
m <- as.data.frame(read.csv("day2-pheno-mono-exp--6.png.csv"))
c <- as.data.frame(read.csv("NewProject-Run-2-Plate-001-Expbe-bar-exp--6.png.csv"))

################################# MONOCULTURES #################################
## Extract data for each strain in monoculture
## Subset of dataframe is extracted and sorted by "column" numbering. 

#AS16
AS16_mono<-subset(m, m$column==1|m$column==7|m$column==13|m$column==19)
AS16_mono<-AS16_mono[order(AS16_mono$column),]

#S26
S26_mono<-subset(m, m$column==3|m$column==9|m$column==15|m$column==21)
S26_mono<-S26_mono[order(S26_mono$column),]

#mut
mut_mono<-subset(m, m$column==5|m$column==11|m$column==17|m$column==23)
mut_mono<-mut_mono[order(mut_mono$column),]

################################# CO-CULTURES #################################
## Using "row" and "column" information from dataframe to extract data 

## AS16 with S26
AS16_w_S26<-subset(c,c$row==2 & c$column==3|c$row==2 & c$column==9|c$row==2 & c$column==15|c$row==3 & c$column==14|c$row==3 & c$column==20
                   |c$row==5 & c$column==3|c$row==5 & c$column==9|c$row==5 & c$column==15|c$row==6 & c$column==8|c$row==6 & c$column==20|c$row==8 & c$column==9|c$row==8 & c$column==3)
nrow(AS16_w_S26)

## AS16 with mut
AS16_w_mut<-subset(c,c$row==2 & c$column==2|c$row==2 & c$column==8|c$row==2 & c$column==14|c$row==2 & c$column==20|c$row==3 & c$column==5|c$row==3 & c$column==11|c$row==3 & c$column==17|c$row==5 & c$column==2|c$row==5 & c$column==20|c$row==6 & c$column==5|c$row==6 & c$column==11|c$row==6 & c$column==17
                   |c$row==6 & c$column==23|c$row==8 & c$column==2|c$row==8 & c$column==8|c$row==8 & c$column==14|c$row==8 & c$column==20)

## AS16 with AS16
AS16_w_AS16 <-subset(c,c$row==1 & c$column==7|c$row==1 & c$column==8|c$row==1 & c$column==13|c$row==1 & c$column==14|c$row==1 & c$column==19|c$row==1 & c$column==20
                   |c$row==4 & c$column==2|c$row==4 & c$column==7|c$row==4 & c$column==8|c$row==4 & c$column==13|c$row==4 & c$column==14
                   |c$row==4 & c$column==19|c$row==4 & c$column==20|c$row==7 & c$column==2|c$row==7 & c$column==7|c$row==7 & c$column==8|c$row==7 & c$column==13|c$row==7 & c$column==14)

## S26 with AS16
S26_w_AS16 <- subset(c,c$row==2 & c$column==4|c$row==2 & c$column==10|c$row==2 & c$column==16|c$row==2 & c$column==22|c$row==3 & c$column==1|c$row==3 & c$column==7|c$row==3 & c$column==13|c$row==3 & c$column==19|c$row==5 & c$column==4
                    |c$row==5 & c$column==10|c$row==5 & c$column==16|c$row==6 & c$column==1|c$row==6 & c$column==7|c$row==6 & c$column==19|c$row==8 & c$column==10|c$row==8 & c$column==4)

## S26 with mut
S26_w_mut <- subset(c,c$row==2 & c$column==5|c$row==2 & c$column==11|c$row==2 & c$column==17|c$row==2 & c$column==23|c$row==3 & c$column==4|c$row==3 & c$column==10
                    |c$row==3 & c$column==16|c$row==3 & c$column==22|c$row==5 & c$column==5|c$row==5 & c$column==11|c$row==5 & c$column==17|c$row==5 & c$column==17|c$row==5 & c$column==23
                    |c$row==6 & c$column==4|c$row==6 & c$column==10|c$row==6 & c$column==16|c$row==6 & c$column==16|c$row==6 & c$column==22|c$row==8 & c$column==5|c$row==8 & c$column==11|c$row==8 & c$column==17|c$row==8 & c$column==23)

## S26 with S26 
S26_w_S26 <- subset(c,c$row==1 & c$column==3|c$row==1 & c$column==4||c$row==1 & c$column==9|c$row==1 & c$column==10|c$row==1 & c$column==15
                    |c$row==1 & c$column==16|c$row==1 & c$column==21|c$row==1 & c$column==22|c$row==4 & c$column==3|c$row==4 & c$column==4
                    |c$row==4 & c$column==9|c$row==4 & c$column==10|c$row==4 & c$column==15|c$row==4 & c$column==16|c$row==4 & c$column==21
                    |c$row==4 & c$column==22|c$row==7 & c$column==3|c$row==7 & c$column==4|c$row==7 & c$column==9|c$row==7 & c$column==10|c$row==7 & c$column==15
                    |c$row==7 & c$column==16|c$row==7 & c$column==21|c$row==7 & c$column==22)


## Mut with AS16
mut_w_AS16 <- subset(c,c$row==2 & c$column==1|c$row==2 & c$column==7|c$row==2 & c$column==13|c$row==2 & c$column==19|c$row==3 & c$column==6|c$row==3 & c$column==12|c$row==3 & c$column==18|c$row==5 & c$column==1|c$row==5 & c$column==19)

## Mut with S26
mut_w_S26 <- subset(c,c$row==2 & c$column==6|c$row==2 & c$column==12|c$row==2 & c$column==18|c$row==2 & c$column==24||c$row==3 & c$column==3|c$row==3 & c$column==9
                    |c$row==3 & c$column==15|c$row==3 & c$column==21|c$row==5 & c$column==6|c$row==5 & c$column==12|c$row==5 & c$column==18|c$row==5 & c$column==24|c$row==6 & c$column==3
                    |c$row==6 & c$column==9|c$row==6 & c$column==15|c$row==6 & c$column==21|c$row==8 & c$column==6|c$row==8 & c$column==12|c$row==8 & c$column==18|c$row==8 & c$column==24)

## Mut with mut
mut_w_mut <- subset(c,c$row==1 & c$column==5|c$row==1 & c$column==6|c$row==1 & c$column==11|c$row==1 & c$column==12|c$row==1 & c$column==17|c$row==1 & c$column==18|c$row==1 & c$column==23|c$row==1 & c$column==24
                    |c$row==4 & c$column==24|c$row==4 & c$column==24|c$row==7 & c$column==5|c$row==7 & c$column==6|c$row==7 & c$column==17|c$row==7 & c$column==18|c$row==7 & c$column==23|c$row==7 & c$column==27)


############################## For table in report ###################################
## Area
#AS16
mean(AS16_mono$area)
sd(AS16_mono$area)

mean(AS16_w_AS16$area)
sd(AS16_w_AS16$area)

mean(AS16_w_S26$area)
sd(AS16_w_S26$area)

mean(AS16_w_mut$area)
sd(AS16_w_mut$area)

#S26
mean(S26_mono$area)
sd(S26_mono$area)

mean(S26_w_AS16$area)
sd(S26_w_AS16$area)

mean(S26_w_S26$area)
sd(S26_w_S26$area)

mean(S26_w_mut$area)
sd(S26_w_mut$area)

#Mut
mean(mut_mono$area)
sd(mut_mono$area)

mean(mut_w_AS16$area)
sd(mut_w_AS16$area)

mean(mut_w_S26$area)
sd(mut_w_S26$area)

mean(mut_w_mut$area)
sd(mut_w_mut$area)

############################### one-way ANOVA ##################################

#### AS16 in different conditions

# Reorganize data in one vector.
AS16_area<-c(AS16_mono$area,AS16_w_AS16$area,AS16_w_S26$area,AS16_w_mut$area)

# Assign the strain it's co-cultured with to the corresponding to element in data vector.
treatm_AS16 <-factor(rep(c("Monoculture","With AS16","With S26", "With S26 mut"),times=c(nrow(AS16_mono),nrow(AS16_w_AS16),nrow(AS16_w_S26),nrow(AS16_w_mut))))

# Check if data is normal distributed
shapiro.test(AS16_mono$area)
shapiro.test(AS16_w_AS16$area)
shapiro.test(AS16_w_S26$area)
shapiro.test(AS16_w_mut$area)

# Visualize with qq plots:
par(mfrow=c(2,2))
qqnorm(AS16_mono$area,main= "AS16 in monoculture")
qqline(AS16_mono$area)

qqnorm(AS16_w_AS16$area, main= "AS16 with AS16")
qqline(AS16_w_AS16$area)

qqnorm(AS16_w_S26$area,main="AS16 with S26 WT")
qqline(AS16_w_S26$area)

qqnorm(AS16_w_mut$area,main="AS16 with S26 mut")
qqline(AS16_w_mut$area)


# Use Bartletts test for testing equal variance
bartlett.test(AS16_area,treatm_AS16)

# Put data in data frame for making boxplot
data_as16<-data.frame(name=treatm_AS16,value=AS16_area)


library(ggplot2)
library(ggeasy)
my_xlab <- paste(levels(data_as16$name),"\n(N=",table(data_as16$name),")",sep="")

ggplot(data_as16, aes(x=name, y=value)) + 
  geom_boxplot(color="black",fill=c("aquamarine4","darkseagreen4","darkseagreen3","darkseagreen1"))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("Area [pixels]")+
  xlab(" ")+
  geom_point() +  
  labs(title="Area of AS16 spots")+
  ggeasy::easy_center_title()+
  scale_x_discrete(labels=my_xlab)+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size=14))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ylim(700,4000)
  
  
# one-way anova
anova(lm(AS16_area~treatm_AS16))

# Post hoc analysis
# Calculate M
4*(4-1)/2
#corrected alpha
0.05/6

#H0: the means are equal
#df=n-k
n<-length(AS16_area)
k<-4
MSE<- 22981

tobs<-(mean(AS16_mono$area)-mean(AS16_w_AS16$area))/(sqrt(MSE*(1/length(AS16_mono$area)+1/length(AS16_w_AS16$area))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(AS16_mono$area)-mean(AS16_w_S26$area))/(sqrt(MSE*(1/length(AS16_mono$area)+1/length(AS16_w_S26$area))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(AS16_mono$area)-mean(AS16_w_mut$area))/(sqrt(MSE*(1/length(AS16_mono$area)+1/length(AS16_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(AS16_w_AS16$area)-mean(AS16_w_S26$area))/(sqrt(MSE*(1/length(AS16_w_AS16$area)+1/length(AS16_w_S26$area))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(AS16_w_AS16$area)-mean(AS16_w_mut$area))/(sqrt(MSE*(1/length(AS16_w_AS16$area)+1/length(AS16_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(AS16_w_S26$area)-mean(AS16_w_mut$area))/(sqrt(MSE*(1/length(AS16_w_S26$area)+1/length(AS16_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))


## Fold change
mean(AS16_mono$area)/mean(AS16_w_AS16$area)
mean(AS16_mono$area)/mean(AS16_w_S26$area)
mean(AS16_mono$area)/mean(AS16_w_mut$area)

mean(AS16_w_AS16$area)/mean(AS16_w_S26$area)
(mean(AS16_w_AS16$area)/mean(AS16_w_mut$area))^-1

(mean(AS16_w_S26$area)/mean(AS16_w_mut$area))^-1


############# S26 in different conditions
# Reorganize data in one vector.
S26_area<-c(S26_mono$area,S26_w_AS16$area,S26_w_S26$area,S26_w_mut$area)

# Assign the strain it's co-cultured with to the corresponding to element in data vector.
treatm_S26 <-factor(rep(c("Monoculture","With AS16","With S26", "With S26 mut"),times=c(nrow(S26_mono),nrow(S26_w_AS16),nrow(S26_w_S26),nrow(S26_w_mut))))

# Check if data is normal distributed
shapiro.test(S26_mono$area)
shapiro.test(S26_w_AS16$area)
shapiro.test(S26_w_S26$area)
shapiro.test(S26_w_mut$area)

# Visualize with qq plots:
par(mfrow=c(2,2))
qqnorm(S26_mono$area,main= "S26 in monoculture")
qqline(S26_mono$area)

qqnorm(S26_w_AS16$area, main= "S26 with AS16")
qqline(S26_w_AS16$area)

qqnorm(S26_w_S26$area,main="S26 with S26 WT")
qqline(S26_w_S26$area)

qqnorm(S26_w_mut$area,main="S26 with S26 mut")
qqline(S26_w_mut$area)

# Use Bartlettes test for testing equal variance
bartlett.test(S26_area,treatm_S26)

# Put data in data frame
data_S26<-data.frame(name=treatm_S26,value=S26_area)

my_xlab <- paste(levels(data_S26$name),"\n(N=",table(data_S26$name),")",sep="")

ggplot(data_S26, aes(x=name, y=value)) + 
  geom_boxplot(color="black",fill=c("darkorange4","darkorange3","darkorange","darkgoldenrod1"))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("Area [pixels]")+
  xlab(" ")+
  geom_point() +  
  labs(title="Area of S26 WT spots")+
  ggeasy::easy_center_title()+
  scale_x_discrete(labels=my_xlab)+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size=14))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ylim(700,4000)


# one-way anova
anova(lm(S26_area~treatm_S26))

# Post hoc analysis
# Calculate M
4*(4-1)/2
#corrected alpha
0.05/6

#H0: the means are equal
#df=n-k
n<-length(S26_area)
k<-4
MSE <-65161

tobs<-(mean(S26_mono$area)-mean(S26_w_AS16$area))/(sqrt(MSE*(1/length(S26_mono$area)+1/length(S26_w_AS16$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(S26_mono$area)-mean(S26_w_S26$area))/(sqrt(MSE*(1/length(S26_mono$area)+1/length(S26_w_S26$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(S26_mono$area)-mean(S26_w_mut$area))/(sqrt(MSE*(1/length(S26_mono$area)+1/length(S26_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))


tobs<-(mean(S26_w_AS16$area)-mean(S26_w_S26$area))/(sqrt(MSE*(1/length(S26_w_AS16$area)+1/length(S26_w_S26$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(S26_w_AS16$area)-mean(S26_w_mut$area))/(sqrt(MSE*(1/length(S26_w_AS16$area)+1/length(S26_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(S26_w_S26$area)-mean(S26_w_mut$area))/(sqrt(MSE*(1/length(S26_w_S26$area)+1/length(S26_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))

###### Fold-change
## Fold change
mean(S26_mono$area)/mean(S26_w_AS16$area)
mean(S26_mono$area)/mean(S26_w_S26$area)
mean(S26_mono$area)/mean(S26_w_mut$area)

mean(S26_w_AS16$area)/mean(S26_w_S26$area)
mean(S26_w_AS16$area)/mean(S26_w_mut$area)

(mean(S26_w_S26$area)/mean(S26_w_mut$area))^-1


############# Mutant in different conditions
# Reorganize data in one vector.
mut_area<-c(mut_mono$area,mut_w_AS16$area,mut_w_S26$area,mut_w_mut$area)

# Assign the strain it's co-cultured with to the corresponding to element in data vector.
treatm_mut <-factor(rep(c("Monoculture","With AS16","With S26", "With S26 mut"),times=c(nrow(mut_mono),nrow(mut_w_AS16),nrow(mut_w_S26),nrow(mut_w_mut))))

# Check if data is normal distributed
shapiro.test(mut_mono$area)
shapiro.test(mut_w_AS16$area)
shapiro.test(mut_w_S26$area)
shapiro.test(mut_w_mut$area)

# Visualize with qq plots:
par(mfrow=c(2,2))
qqnorm(mut_mono$area,main= expression(paste("S26",Delta, ~"in monoculture")))
qqline(mut_mono$area)

qqnorm(mut_w_AS16$area, main= expression(paste("S26",Delta,~"with AS16")))
qqline(mut_w_AS16$area)

qqnorm(mut_w_S26$area,main=expression(paste("S26",Delta,~"with S26 WT")))
qqline(mut_w_S26$area)

qqnorm(mut_w_mut$area,main=expression(paste("S26",Delta, ~"with S26",Delta)))
qqline(mut_w_mut$area)

# Use Bartlett's test for testing equal variance
bartlett.test(mut_area,treatm_mut)

# Put data in data frame
data_mut<-data.frame(name=treatm_mut,value=mut_area)

library(ggplot2)
library(ggeasy)
my_xlab <- paste(levels(data_mut$name),"\n(N=",table(data_mut$name),")",sep="")

ggplot(data_mut, aes(x=name, y=value)) + 
  geom_boxplot(color="black",fill=c("cornsilk4","cornsilk3","cornsilk2","cornsilk"))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("Area [pixels]")+
  xlab(" ")+
  geom_point() +  
  labs(title=expression(paste("Area of S26",Delta,~ "spots")))+
  ggeasy::easy_center_title()+
  scale_x_discrete(labels=my_xlab)+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size=14))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ylim(700,4000)


# one-way anova
anova(lm(mut_area~treatm_mut))

# Post hoc analysis
# Calculate M
4*(4-1)/2
#corrected alpha
0.05/6

#H0: the means are equal
#df=n-k
n<-length(mut_area)
k<-4
MSE <-237948 

tobs<-(mean(mut_mono$area)-mean(mut_w_AS16$area))/(sqrt(MSE*(1/length(mut_mono$area)+1/length(mut_w_AS16$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(mut_mono$area)-mean(mut_w_S26$area))/(sqrt(MSE*(1/length(mut_mono$area)+1/length(mut_w_S26$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(mut_mono$area)-mean(mut_w_mut$area))/(sqrt(MSE*(1/length(mut_mono$area)+1/length(mut_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))


tobs<-(mean(mut_w_AS16$area)-mean(mut_w_S26$area))/(sqrt(MSE*(1/length(mut_w_AS16$area)+1/length(mut_w_S26$area))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(mut_w_AS16$area)-mean(mut_w_mut$area))/(sqrt(MSE*(1/length(mut_w_AS16$area)+1/length(mut_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(mut_w_S26$area)-mean(mut_w_mut$area))/(sqrt(MSE*(1/length(mut_w_S26$area)+1/length(mut_w_mut$area))))
2*(1 - pt(abs(tobs),df=n-k))


## Fold change
mean(mut_mono$area)/mean(mut_w_AS16$area)
mean(mut_mono$area)/mean(mut_w_S26$area)
mean(mut_mono$area)/mean(mut_w_mut$area)

(mean(mut_w_AS16$area)/mean(mut_w_S26$area))^-1
(mean(mut_w_AS16$area)/mean(mut_w_mut$area))^-1

(mean(mut_w_S26$area)/mean(mut_w_mut$area))^-1


