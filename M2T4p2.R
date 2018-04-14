#Module2Task4
#created by Ros
#=========================Set Working Directory=======================================
setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M2T4")
#=================== Loading libraries====================================
library(arules)
library(arulesViz)
library(ggplot2)
#====================Data import==========================================
Mydata <- read.transactions("ElectronidexTransactions2017.csv",sep=",")
#====================vizualization==========================================
itemFrequencyPlot(Mydata, support = 0.10)#item with suppot >.7%
itemFrequencyPlot(Mydata,topN= 10)
itemFrequency(Mydata, type = "absolute",weighted = FALSE)
sort(itemFrequency(Mydata, type = "absolute",weighted = FALSE), decreasing = T)
itemFrequency(Mydata[,1:100]) 
summary(Mydata)
#====================model===============================================
#rules <- apriori(Mydata,appearance = list(rhs = "Backlit LED Gaming Keyboard", default = "lhs"), parameter = list(support=0.0007,confidence=0.2,minlen=1))
rules <- apriori(Mydata, parameter = list(support=0.007,confidence=0.30,minlen=1))
myrules <- rules[!is.redundant(rules)]
summary(myrules)
myrules
newrules =subset(rules, (lhs %in% c("iMac")))
newrules
#====================Model vizualization================================================
inspect(sort(myrules, by= "lift")[1:10])
plot(myrules[1:10], method="graph", control=list(type="lift"))
plot(myrules,measure=c("support","confidence"),shading="count" ,engine='interactive')
library("RColorBrewer")
plot(myrules,control=list(col=brewer.pal(11,"Spectral")),main="")

subrules2 = head(sort(myrules, by="count"), 30);

plot(myrules, method="graph");

plot(subrules2, method="graph", control=list(type="items"));

plot(myrules, method="paracoord");

plot(myrules, method="paracoord", control=list(reorder=TRUE),shading ="count")
