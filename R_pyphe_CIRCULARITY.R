## Set working directory to folder with the excel sheet
setwd("C:/Users/Pardis/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/2) AS16 + S26 experiments/Cultures on solid medium/Co-culture picture with bars (day 2)/Final_pyphe_analysis/pyphe_quant")

## Import data from Pyphe .csv file
library("readxl")
m <- as.data.frame(read.csv("day2-pheno-mono-exp--6.png.csv"))
c <- as.data.frame(read.csv("NewProject-Run-2-Plate-001-Expbe-bar-exp--6.png.csv"))

################################# MONOCULTURES #################################
## Extract data for each strain
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

############################ Calculations ############################
####### Circularity

#AS16
mean(AS16_mono$circularity)
sd(AS16_mono$circularity)

mean(AS16_w_AS16$circularity)
sd(AS16_w_AS16$circularity)

mean(AS16_w_S26$circularity)
sd(AS16_w_S26$circularity)

mean(AS16_w_mut$circularity)
sd(AS16_w_mut$circularity)

#S26
mean(S26_mono$circularity)
sd(S26_mono$circularity)

mean(S26_w_AS16$circularity)
sd(S26_w_AS16$circularity)

mean(S26_w_S26$circularity)
sd(S26_w_S26$circularity)

mean(S26_w_mut$circularity)
sd(S26_w_mut$circularity)

#Mut
mean(mut_mono$circularity)
sd(mut_mono$circularity)

mean(mut_w_AS16$circularity)
sd(mut_w_AS16$circularity)

mean(mut_w_S26$circularity)
sd(mut_w_S26$circularity)

mean(mut_w_mut$circularity)
sd(mut_w_mut$circularity)
############################### one-way ANOVA ##################################

#### AS16 in different conditions

# Reorganize data in one vector.
AS16_circularity<-c(AS16_mono$circularity,AS16_w_AS16$circularity,AS16_w_S26$circularity,AS16_w_mut$circularity)

# Assign the strain it's co-cultured with to the corresponding element in data vector.
treatm_AS16 <-factor(rep(c("Monoculture","With AS16","With S26", "With S26 mut"),times=c(nrow(AS16_mono),nrow(AS16_w_AS16),nrow(AS16_w_S26),nrow(AS16_w_mut))))


# Check if data is normal distributed
shapiro.test(AS16_mono$circularity)
shapiro.test(AS16_w_AS16$circularity)
shapiro.test(AS16_w_S26$circularity)
shapiro.test(AS16_w_mut$circularity)

# Visualize with qq plots:
par(mfrow=c(2,2))
qqnorm(AS16_mono$circularity,main= "AS16 in monoculture")
qqline(AS16_mono$circularity)

qqnorm(AS16_w_AS16$circularity, main= "AS16 with AS16")
qqline(AS16_w_AS16$circularity)

qqnorm(AS16_w_S26$circularity,main="AS16 with S26 WT")
qqline(AS16_w_S26$circularity)

qqnorm(AS16_w_mut$circularity,main="AS16 with S26 mut")
qqline(AS16_w_mut$circularity)


# Use Bartlettes test for testing equal variance
bartlett.test(AS16_circularity,treatm_AS16)

# Put data in data frame
data_as16<-data.frame(name=treatm_AS16,value=AS16_circularity)


library(ggplot2)
library(ggeasy)
my_xlab <- paste(levels(data_as16$name),"\n(N=",table(data_as16$name),")",sep="")
ggplot(data_as16, aes(x=name, y=value)) + 
  geom_boxplot(color="black",fill=c("aquamarine4","darkseagreen4","darkseagreen3","darkseagreen1"))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("Circularity")+
  xlab(" ")+
  geom_point() +  
  labs(title="Circularity of AS16 spots")+
  ggeasy::easy_center_title()+
  scale_x_discrete(labels=my_xlab)+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size=14))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ylim(0.1,1)


# one-way anova
anova(lm(AS16_circularity~treatm_AS16))

# Post hoc analysis
# Calculate M
4*(4-1)/2
#corrected alpha
0.05/6

#H0: the means are equal
#df=n-k
n<-length(AS16_circularity)
k<-4
MSE<- 0.019943

tobs<-(mean(AS16_mono$circularity)-mean(AS16_w_AS16$circularity))/(sqrt(MSE*(1/length(AS16_mono$circularity)+1/length(AS16_w_AS16$circularity))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(AS16_mono$circularity)-mean(AS16_w_S26$circularity))/(sqrt(MSE*(1/length(AS16_mono$circularity)+1/length(AS16_w_S26$circularity))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(AS16_mono$circularity)-mean(AS16_w_mut$circularity))/(sqrt(MSE*(1/length(AS16_mono$circularity)+1/length(AS16_w_mut$circularity))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(AS16_w_AS16$circularity)-mean(AS16_w_S26$circularity))/(sqrt(MSE*(1/length(AS16_w_AS16$circularity)+1/length(AS16_w_S26$circularity))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(AS16_w_AS16$circularity)-mean(AS16_w_mut$circularity))/(sqrt(MSE*(1/length(AS16_w_AS16$circularity)+1/length(AS16_w_mut$circularity))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(AS16_w_S26$circularity)-mean(AS16_w_mut$circularity))/(sqrt(MSE*(1/length(AS16_w_S26$circularity)+1/length(AS16_w_mut$circularity))))
2*(1 - pt(abs(tobs),df=n-k))

## Fold change
mean(AS16_mono$circularity)/mean(AS16_w_AS16$circularity)
mean(AS16_mono$circularity)/mean(AS16_w_S26$circularity)
mean(AS16_mono$circularity)/mean(AS16_w_mut$circularity)

mean(AS16_w_AS16$circularity)/mean(AS16_w_S26$circularity)
(mean(AS16_w_AS16$circularity)/mean(AS16_w_mut$circularity))^-1

(mean(AS16_w_S26$circularity)/mean(AS16_w_mut$circularity))^-1



############# S26 in different conditions
# Reorganize data in one vector.
S26_circularity<-c(S26_mono$circularity,S26_w_AS16$circularity,S26_w_S26$circularity,S26_w_mut$circularity)

# Assign the strain it's co-cultured with to the corresponding to element in data vector.
treatm_S26 <-factor(rep(c("Monoculture","With AS16","With S26", "With S26 mut"),times=c(nrow(S26_mono),nrow(S26_w_AS16),nrow(S26_w_S26),nrow(S26_w_mut))))

# Check if data is normal distributed
shapiro.test(S26_mono$circularity)
shapiro.test(S26_w_AS16$circularity)
shapiro.test(S26_w_S26$circularity)
shapiro.test(S26_w_mut$circularity)

# Visualize with qq plots:
par(mfrow=c(2,2))
qqnorm(S26_mono$circularity,main= "S26 in monoculture")
qqline(S26_mono$circularity)

qqnorm(S26_w_AS16$circularity, main= "S26 with AS16")
qqline(S26_w_AS16$circularity)

qqnorm(S26_w_S26$circularity,main="S26 with S26 WT")
qqline(S26_w_S26$circularity)

qqnorm(S26_w_mut$circularity,main="S26 with S26 mut")
qqline(S26_w_mut$circularity)

# Use Bartlettes test for testing equal variance
bartlett.test(S26_circularity,treatm_S26)

# Put data in data frame
data_S26<-data.frame(name=treatm_S26,value=S26_circularity)


library(ggplot2)
library(ggeasy)
my_xlab <- paste(levels(data_S26$name),"\n(N=",table(data_S26$name),")",sep="")

ggplot(data_S26, aes(x=name, y=value)) + 
  geom_boxplot(color="black",fill=c("darkorange4","darkorange3","darkorange","darkgoldenrod1"))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("Circularity")+
  xlab(" ")+
  geom_point() +  
  labs(title="Circularity of S26 WT spots")+
  ggeasy::easy_center_title()+
  scale_x_discrete(labels=my_xlab)+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size=14))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ylim(0.1,1)


# one-way anova
anova(lm(S26_circularity~treatm_S26))

## Fold change
(mean(S26_mono$circularity)/mean(S26_w_AS16$circularity))^-1
(mean(S26_mono$circularity)/mean(S26_w_S26$circularity))^-1
mean(S26_mono$circularity)/mean(S26_w_mut$circularity)

mean(S26_w_AS16$circularity)/mean(S26_w_S26$circularity)
mean(S26_w_AS16$circularity)/mean(S26_w_mut$circularity)

mean(S26_w_S26$circularity)/mean(S26_w_mut$circularity)


############# Mutant in different conditions
# Reorganize data in one vector.
mut_circularity<-c(mut_mono$circularity,mut_w_AS16$circularity,mut_w_S26$circularity,mut_w_mut$circularity)

# Assign the strain it's co-cultured with to the corresponding to element in data vector.
treatm_mut <-factor(rep(c("Monoculture","With AS16","With S26", "With S26 mut"),times=c(nrow(mut_mono),nrow(mut_w_AS16),nrow(mut_w_S26),nrow(mut_w_mut))))

# Check if data is normal distributed
shapiro.test(mut_mono$circularity)
shapiro.test(mut_w_AS16$circularity)
shapiro.test(mut_w_S26$circularity)
shapiro.test(mut_w_mut$circularity)

# Visualize with qq plots:
par(mfrow=c(2,2))
qqnorm(mut_mono$circularity,main= expression(paste("S26",Delta, ~"in monoculture")))
qqline(mut_mono$circularity)

qqnorm(mut_w_AS16$circularity, main= expression(paste("S26",Delta,~"with AS16")))
qqline(mut_w_AS16$circularity)

qqnorm(mut_w_S26$circularity,main=expression(paste("S26",Delta,~"with S26 WT")))
qqline(mut_w_S26$circularity)


qqnorm(mut_w_mut$circularity,main=expression(paste("S26",Delta, ~"with S26",Delta)))
qqline(mut_w_mut$circularity)

# Use Bartlett's test for testing equal variance
bartlett.test(mut_circularity,treatm_mut)

# Put data in data frame
data_mut<-data.frame(name=treatm_mut,value=mut_circularity)


library(ggplot2)
library(ggeasy)
my_xlab <- paste(levels(data_mut$name),"\n(N=",table(data_mut$name),")",sep="")

ggplot(data_mut, aes(x=name, y=value)) + 
  geom_boxplot(color="black",fill=c("cornsilk4","cornsilk3","cornsilk2","cornsilk"))+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  ylab("circularity")+
  xlab(" ")+
  geom_point() +  
  labs(title=expression(paste("Circularity of S26",Delta,~ "spots")))+
  ggeasy::easy_center_title()+
  scale_x_discrete(labels=my_xlab)+
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size=14))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ylim(0.1,1)


# one-way anova
anova(lm(mut_circularity~treatm_mut))

# Post hoc analysis
# Calculate M
4*(4-1)/2
#corrected alpha
0.05/6

#H0: the means are equal
#df=n-k
n<-length(mut_circularity)
k<-4
MSE <-0.018193 

tobs<-(mean(mut_mono$circularity)-mean(mut_w_AS16$circularity))/(sqrt(MSE*(1/length(mut_mono$circularity)+1/length(mut_w_AS16$circularity))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(mut_mono$circularity)-mean(mut_w_S26$circularity))/(sqrt(MSE*(1/length(mut_mono$circularity)+1/length(mut_w_S26$circularity))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(mut_mono$circularity)-mean(mut_w_mut$circularity))/(sqrt(MSE*(1/length(mut_mono$circularity)+1/length(mut_w_mut$circularity))))
2*(1 - pt(abs(tobs),df=n-k))


tobs<-(mean(mut_w_AS16$circularity)-mean(mut_w_S26$circularity))/(sqrt(MSE*(1/length(mut_w_AS16$circularity)+1/length(mut_w_S26$circularity))))
2*(1 - pt(abs(tobs),df=n-k))
tobs<-(mean(mut_w_AS16$circularity)-mean(mut_w_mut$circularity))/(sqrt(MSE*(1/length(mut_w_AS16$circularity)+1/length(mut_w_mut$circularity))))
2*(1 - pt(abs(tobs),df=n-k))

tobs<-(mean(mut_w_S26$circularity)-mean(mut_w_mut$circularity))/(sqrt(MSE*(1/length(mut_w_S26$circularity)+1/length(mut_w_mut$circularity))))
2*(1 - pt(abs(tobs),df=n-k))


n<-length(mut_circularity)
k<-4
t.test(mut_mono$circularity,mut_w_AS16$circularity,df=n-k,var.equal = TRUE)
t.test(mut_mono$circularity,mut_w_S26$circularity,df=n-k,var.equal = TRUE)
t.test(mut_mono$circularity,mut_w_mut$circularity,df=n-k,var.equal = TRUE)

t.test(mut_w_AS16$circularity,mut_w_S26$circularity,df=n-k,var.equal = TRUE)
t.test(mut_w_AS16$circularity,mut_w_mut$circularity,df=n-k,var.equal = TRUE)

t.test(mut_w_S26$circularity,mut_w_mut$circularity,df=n-k,var.equal = TRUE)

(mean(mut_mono$circularity)/mean(mut_w_AS16$circularity))^-1
(mean(mut_mono$circularity)/mean(mut_w_S26$circularity))^-1
(mean(mut_mono$circularity)/mean(mut_w_mut$circularity))^-1

(mean(mut_w_AS16$circularity)/mean(mut_w_S26$circularity))^-1
(mean(mut_w_AS16$circularity)/mean(mut_w_mut$circularity))^-1

mean(mut_w_S26$circularity)/mean(mut_w_mut$circularity)

