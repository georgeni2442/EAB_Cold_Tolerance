setwd("~/OneDrive/Documents/R/Tet/Emergence")

emerge<-read.csv("Emergence_Spreadsheet.csv")

edit<-emerge%>%
  group_by(Group,Treatment)%>%
  summarize(Tets=sum(Tetrastichus), Failed=sum(Tetrastichus.Failed.to.Eclose))

plot<-ggplot(data=edit, mapping=aes(x=Treatment, y=Tets))+geom_boxplot()
plot

plot2<-ggplot(data=emerge, mapping=aes(x=Treatment, y=Tetrastichus))+geom_boxplot()+facet_wrap(~Group)
plot2

plot3<-ggplot(data=emerge, mapping=aes(x=Treatment, y=Tetrastichus))+geom_boxplot()
plot3

ANOVA<-aov(data=edit, Tets~Treatment)
fail<-aov(data=edit, Failed~Treatment)
summary(ANOVA)
summary(fail)

hist<-ggplot(data=emerge, mapping=aes(x=emerge$Tetrastichus, color=Treatment))+geom_histogram(bins=30)
hist
