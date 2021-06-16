###########################  Growth analysis using "growthcurver" ###########################
# Set working directory: go to "Session" --> "To source file location" --> copy paste the code that appears in the terminal into the script
setwd("C:/Users/Pardis/OneDrive - Danmarks Tekniske Universitet/DTU/Bachelorprojekt/2) AS16 + S26 experiments/Cultures in liquid medium")

### Read excel file
# Use "readxl" to read data in excel file
#install.packages("readxl")
library("readxl")

#Importing data for mono- and co-cultures
m <- as.data.frame(read_excel("Data_liquid_exp.xlsx", sheet="mono_R"))
c <- as.data.frame(read_excel("Data_liquid_exp.xlsx", sheet="co_R"))

######################## Data analysis with GrowthCurver ##################
#Fits the points to a logistic model
library("growthcurver")

#### One strain at a time

## AS16 monoculture (A)
m_AS16_A_fit <- SummarizeGrowth(m$time,m$AS16_A,bg_correct='none')
plot(m_AS16_A_fit, main="AS16 monoculture (A)",cex.main=1)
m_AS16_A_fit$model

# Trying own plot
eq1 <- function(t){8.500e+09/(1+(8.500e+09-1.360e+03)/1.360e+03*exp(-5.553e-01*t))}

plot(m$time,m$AS16_A,col='red',main="AS16 monoculture A")
lines(eq1(1:60),type='l')

## AS16 monoculture (B)
m_AS16_B_fit <- SummarizeGrowth(m$time,m$AS16_B,bg_correct='none')
plot(m_AS16_B_fit)
m_AS16_B_fit$model


## S26 monoculture (A)
m_S26_A_fit <- SummarizeGrowth(m$time,m$S26_A,bg_correct='none')
plot(m_S26_A_fit)
m_S26_A_fit$model

## S26 monoculture (B)
m_S26_B_fit <- SummarizeGrowth(m$time,m$S26_B,bg_correct='none')
plot(m_S26_B_fit)
m_S26_B_fit$model

## Mut monoculture (A)
m_mut_A_fit <- SummarizeGrowth(m$time,m$mut_A,bg_correct='none')
plot(m_mut_A_fit)
m_mut_A_fit$model

# Mut monoculture (B)
m_mut_B_fit <- SummarizeGrowth(m$time,m$mut_B,bg_correct='none')
plot(m_mut_B_fit)
m_mut_B_fit$model

######################## All strains at once ###########################
### Mono-cultures
all_m<-SummarizeGrowthByPlate(plate=m,bg_correct='none',plot_fit=FALSE,plot_file ="mono_cultures_plots1.pdf")
all_m

all_c<-SummarizeGrowthByPlate(plate=c,bg_correct='none',plot_fit=FALSE,plot_file ="co_cultures_plots1.pdf")
all_c

