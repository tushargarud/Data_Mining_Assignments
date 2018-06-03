#Frequent Itemset Mining Dataset Repository: https://www.r-bloggers.com/datasets-to-practice-your-data-mining/
#Retail dataset: http://fimi.ua.ac.be/data/
#DataSet info: http://fimi.ua.ac.be/data/retail.pdf

# Load the libraries
library(arules)
library(arulesViz)
library(datasets)

# Load the data set
#allData <- read.transactions("C:/Academic/Data Mining/Assignments/Assignment_6/Dataset/retail.dat")
allData<- read.transactions("C:/Academic/Data Mining/Assignments/Assignment_6/Dataset/retail.dat",rm.duplicates= TRUE, sep=" ");
sampleData <- allData[1:40,]

# Create an item frequency plot for the top 20 items
#itemFrequencyPlot(sampleData,topN=20,type="absolute")

# Get the rules
rules <- apriori(sampleData, parameter = list(supp = 0.1, conf = 0.8, maxlen = 5))

# Sort the rules by support in decreasing order
rules<-sort(rules, by="support", decreasing=TRUE)

# Show the top 5 rules, but only 4 digits
options(digits=4)
inspect(rules[1:5])
