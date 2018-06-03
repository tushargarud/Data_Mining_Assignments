#Webpage: http://www.philippe-fournier-viger.com/spmf/index.php?link=datasets.php
#DataSet: http://www.philippe-fournier-viger.com/spmf/datasets/foodmartFIM.txt

# Load the libraries
library(arules)
library(arulesViz)

# Load the metadata information
metadata <- read.csv("C:/Academic/Data Mining/Assignments/Assignment_6/all_files/metadata.csv")
colnames(metadata) <- c("id","name")

# Load the data set
sampleData<- read.transactions("C:/Academic/Data Mining/Assignments/Assignment_6/all_files/foodmart.dat", sep=" ");

# Convert data from id format to name format
sampleData@itemInfo$labels <- metadata$name[match(sampleData@itemInfo$labels,metadata$id)]

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(sampleData,topN=20,type="absolute")

# Get the rules
rules <- apriori(sampleData, parameter = list(supp = 0.0005, conf = 0.2, minlen=2))

# Sort the rules by support in decreasing order
rules<-sort(rules, by="support", decreasing=TRUE)

# Show generated rules
cat("\n\nGenerated rules:\n")
inspect(rules)

# Find redundant rules
rules_matrix <- is.subset(rules, rules)
rules_matrix[lower.tri(rules_matrix, diag=T)] <- NA
duplicates <- colSums(rules_matrix, na.rm=T) >= 1

# Show redundant rules
rules.redundant <- rules[duplicates]
cat("\n\nRedundant rules:\n")
inspect(rules.redundant)

# Show non-redundant rules
rules.uniq <- rules[!duplicates]
cat("\n\nNon-redundant rules:\n")
inspect(rules.uniq)

#Display the rules as graph
plot(rules,method="graph",interactive=TRUE)
