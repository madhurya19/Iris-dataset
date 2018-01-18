library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
## run these before plotting graphs

data_iris <- read.csv('iris.csv',header=T,stringsAsFactors = F)
summary(data_iris)
data_iris$species <- as.factor(data_iris$species)

plot1<-ggplot(data_iris,aes(x=petal.width,fill=species)) +
  geom_histogram(position="identity",alpha=0.4,binwidth = 0.05)

plot2<-ggplot(data_iris,aes(x=petal.length,fill=species)) +
  geom_histogram(position="identity",alpha=0.4,binwidth = 0.05)

plot3<-ggplot(data_iris,aes(x=sepal.length,fill=species)) +
  geom_histogram(position="identity",alpha=0.4,binwidth = 0.05)

plot4<-ggplot(data_iris,aes(x=sepal.width,fill=species)) +
  geom_histogram(position="identity",alpha=0.4,binwidth = 0.05)

grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)

## Clustering these Flowers

p1<-ggplot(data_iris,aes(x=sepal.length,y=sepal.width,shape=data_iris$species,colour=data_iris$species)) +
  geom_point() +
  scale_shape_manual(values=c(1,2,3)) +
  scale_colour_brewer(palette="set1")
p2<-ggplot(data_iris,aes(x=petal.length,y=petal.width,shape=data_iris$species,colour=data_iris$species)) +
  geom_point() +
  scale_shape_manual(values=c(1,2,3) +
  scale_colour_brewer(palette="set1"))
grid.arrange(p1,p2,ncol=1,nrow=2)

d<- data_iris %>% group_by(species) %>% summarise(pl=mean(petal.length),ps = sd(petal.length),n=n())

## Confidence Intervals

#We see from visualizing the data that the species virginica and versicolor seem to be pretty close to each other in some physical aspects. The data contains 150 samples , of which , each species comprise of 50 instances. Is this closeness true in nature?

#Before we conduct a t-test, we lay down a few assumptions about the data.
#<li> The samples that are collected are independent of each other within species</li>
# <li> The samples that are collected are independent of each other </li>
  
#As we have no information regarding the population parameters, we look into the t-distribution for answers. With 95% confidence, what can we say about the difference in the mean petal lengths of versicolor and virginica?

mean_diff <- 5.552-4.260
df <- 50-1
se <- sqrt(((0.4699110^2)/50)+((0.5518947^2)/50))
t <- qt(0.025,df=df)
t <- abs(t)
c_i <- c(mean_diff-(t*se),mean_diff+(t*se))
c_i

##With 95% confidence we can say that the difference between the mean petal lengths of virginica and versicolor lie between (1.086001,1.497999)

##P-values associated with hypothesis test-We present a null hypotheses that the mean difference between the petal lengths of virginica and versicolor is zero. What would be the p-value for this hypothesis?

# Hypothesis test
# H0 : 0
# HA : not = 0
t <- (mean_diff)-0
t <- t/se
p_value <- pt(t,df=49,lower.tail = F)*2
p_value

##The p-value is really really small (5.433453e-17). 
##This would tell us to reject the null hypothesis in favour of the alternate hypothesis that the means are indeed different.