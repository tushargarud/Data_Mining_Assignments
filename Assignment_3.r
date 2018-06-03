input = c(13, 15,16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70) 
width <- 10
result <- vector(length= ceiling(max(input)/width))
for(i in 1:length(input))
{
  result[trunc((input[i]-1)/width)+1] = result[trunc((input[i]-1)/width)+1] + 1
}
for(i in 1:length(result))
{
  names(result)[i] <- paste(as.character(i*width-width+1),"-",as.character(i*width))
}
png(file = "histogram.png")
barplot(result, main="Histogram", xlab="Age",ylab="Count")
dev.off()