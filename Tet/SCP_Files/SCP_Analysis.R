library(tidyverse)
library(ggplot2)
library(dplyr)

scp<-read.csv("SCP_Values_Final.csv", header=TRUE)

scp<-scp%>%
  drop_na(SCP_Value)%>%
  mutate(value=as.numeric(SCP_Value),Group=as.factor(Group))

#Analysis
results<-aov(value~Group, data=scp)
summary(results)
TukeyHSD(results)

