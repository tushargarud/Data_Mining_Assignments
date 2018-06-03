#http://www.salemmarafi.com/code/market-basket-analysis-with-r/

# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

setwd("C:/Academic/Data Mining/Assignments/Assignment_6/Dataset")

allData <- read.table("Amazon0312.txt")

sampleData <- allData[1:100,]

logicalSampleData <- sapply(sampleData,as.data.frame.logical)

rules <- apriori(logicalSampleData, parameter = list(supp = 1.0, conf = 1))
rules<-sort(rules, by="support", decreasing=TRUE)

inspect(rules[1:5])

#print(summary(txs))


#rules <- apriori(txs, parameter = list(supp = 0.001, conf = 0.8))

#inspect(rules[1:5])

# Load the data set
#data(Amazon0312)

# Create an item frequency plot for the top 20 items
#itemFrequencyPlot(txs,topN=20,type="absolute")

