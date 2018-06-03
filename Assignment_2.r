# 2.2 Suppose that the data for analysis includes the attribute age. The age values for the data
# tuples are (in increasing order) 13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30,
# 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70.
# (a) What is the mean of the data? What is the median?
# (b) What is the mode of the data? Comment on the data’s modality (i.e., bimodal,
# trimodal, etc.).
# (c) What is the midrange of the data?
# (d) Can you find (roughly) the first quartile (Q1) and the third quartile (Q3) of the data?
# (e) Give the five-number summary of the data.
# (f) Show a boxplot of the data.
# (g) How is a quantile–quantile plot different from a quantile plot?

cat("\n\nAnswers for question 2.2:\n")

# Input
ages <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)

#Mean
result_mean=mean(ages)
cat("Mean is: ", result_mean, "\n")

#Median
result_median=median(ages)
cat("Median is: ", result_median, "\n")

#Mode
age_freq = vector()                                     #Create a vector containing frequency of every number
age_freq[1] = 1
for(i in 2:length(ages))
{
  if(ages[i] == ages[i-1])
  {
    age_freq[i] <- age_freq[i-1]+1
  }
  else
  {
    age_freq[i] <- 1
  }
}

highest_freq = max(age_freq)                            #Find maximum frequency
mode_indexes = which(age_freq %in% max(age_freq))       #Find positions of the maximum frequency in frequency vector
result.mode <- ages[mode_indexes]                       #Get values at those positions in ages vector as modes

if(length(result.mode)== 1) cat("Mode is: ", result.mode, "\n") else cat("Modes are: ", result.mode, "\n")


#Midrange
max_value <- max(ages)
min_value <- min(ages)
result_midrange <- (max_value + min_value) / 2
cat("Midrange is: ", result_midrange, "\n")

#Quartiles
result_q1 <- quantile(ages)[2]
cat("First quartile is: ", result_q1, "\n")

result_q3 <- quantile(ages)[4]
cat("Third quartile is: ", result_q3, "\n")

#Five number summary
cat("Five number summary is: ", quantile(ages), "\n")

#Boxplot
png(file = "boxplot.png")
boxplot(ages, xlab = "Ages", main = "Boxplot for ages", horizontal=TRUE, range=0)
dev.off()

#Note: In this boxplot, 70 is not considered as an outlier. To mark it as outlier in the boxplot, remove the range parameter from boxplot function call.



#2.3 Suppose that the values for a given set of data are grouped into intervals.
#    The intervals and corresponding frequencies are as follows:
  
#  age         frequency
#  1-5         200
#  6-15        450
#  16-20       300
#  21-50       1500
#  51-80       700
#  81-110      44

cat("\n\nAnswers for question 2.3:\n")

#Input
range_start <- c(1,6,16,21,51,81)
range_end <- c(5,15,20,50,80,110)
freq <- c(200,450,300,1500,700,44)

cumulative_freq = vector()               #Calculate cumulative frequency
cumulative_freq[1] <- freq[1]
for(i in 2:length(freq))
{
  cumulative_freq[i] <- cumulative_freq[i-1] + freq[i]
}

for(i in 1:length(cumulative_freq))      #Find median interval
{
  if( sum(freq)/2 <= cumulative_freq[i])
  {
    median_interval_index <- i
    break()
  }
}

L1 <- range_start[median_interval_index]                                               #Lower boundary of the median interval
N <- sum(freq)                                                                         #Number of values in the entire data set
lower_freq_sum <- cumulative_freq[median_interval_index-1]                             #Sum of the frequencies of all of the intervals that are lower than the median interval
median_interval_frequency <- freq[median_interval_index]                               #Frequency of the median interval
width <- range_end[median_interval_index] - range_start[median_interval_index] + 1     #Width of the median interval

approx_median = L1 + ( (N/2-lower_freq_sum) / median_interval_frequency ) * width      #Formula to calculate approximate median
cat("Approximate value of median is: ", approx_median, "\n")



#2.6 Suppose that the values for a given set of data are grouped into intervals. 
#    The intervals and corresponding frequencies are as follows:
#    age      frequency
#    1-5      200
#    6-15     450
#    16-20    300
#    21-50    1500
#    51-80    700
#    81-110   44
#Compute an approximate median value for the data

cat("\n\nAnswers for question 2.6:\n")

#Input
x <- c(22,1,42,10)
y <- c(20,0,36,8)

#Euclidean distance
temp_sum <- 0
for(i in 1:length(x))
{
  temp_sum <- temp_sum + (x[i] - y[i] ) ^ 2
}
euclidean_distance <- temp_sum ^ (1/2)
cat("Euclidean distance is: ", euclidean_distance, "\n")

#Manhattan distance
temp_sum <- 0
for(i in 1:length(x))
{
  temp_sum <- temp_sum + abs(x[i] - y[i])
}
manhattan_distance <- temp_sum
cat("Manhattan distance is: ", manhattan_distance, "\n")

#Minkowski distance
h <- 3
temp_sum <- 0
for(i in 1:length(x))
{
  temp_sum <- temp_sum + abs(x[i] - y[i] ) ^ h
}
minkowski_distance <- temp_sum ^ (1/h)
cat("Minkowski distance is: ", minkowski_distance, "\n")

#Supremum distance
supremum_distance <- 0
for(i in 1:length(x))
{
  if(abs(x[i] - y[i]) > supremum_distance)
  {
    supremum_distance = abs(x[i] - y[i])
  }
}
cat("Supremum distance is: ", supremum_distance, "\n")





#2.8 It is important to define or select similarity measures in data analysis. However, there
#    is no commonly accepted subjective similarity measure. Results can vary depending on
#    the similarity measures used. Nonetheless, seemingly different similarity measures may
#    be equivalent after some transformation.
#    Suppose we have the following 2-D data set:
#        A1   A2
#    x1  1.5  1.7
#    x2  2    1.9
#    x3  1.6  1.8
#    x4  1.2  1.5
#    x5  1.5  1.0
#    (a) Consider the data as 2-D data points. Given a new data point, x = (1.4,1.6) as a
#        query, rank the database points based on similarity with the query using Euclidean
#        distance, Manhattan distance, supremum distance, and cosine similarity.
#    (b) Normalize the data set to make the norm of each data point equal to 1. Use Euclidean
#        distance on the transformed data to rank the data points


#Input
x1 <- c(1.5, 1.7)
x2 <- c( 2 , 1.9)
x3 <- c(1.6, 1.8)
x4 <- c(1.2, 1.5)
x5 <- c(1.5, 1.0)
x  <- c(1.4, 1.6)

point_names <- c("x1","x2","x3","x4","x5")

cat("\n\nAnswers for question 2.8 (a):\n\n")

#Function to calculate Euclidean Distance
euclidean_distance <- function(vect1, vect2)
{
  temp_sum <- 0
  for(i in 1:length(vect1))
  {
    temp_sum <- temp_sum + (vect1[i] - vect2[i] ) ^ 2
  }
  distance <- temp_sum ^ (1/2)
  return(distance)
}

#Function to calculate Manhattan Distance
manhattan_distance <- function(vect1, vect2)
{
  distance <- 0
  for(i in 1:length(vect1))
  {
    distance <- distance + abs(vect1[i] - vect2[i])
  }
  return(distance)
}

#Function to calculate Supremum Distance
supremum_distance <- function(vect1, vect2)
{
  distance <- 0
  for(i in 1:length(vect1))
  {
    if(abs(vect1[i] - vect2[i]) > distance)
    {
      distance <- abs(vect1[i] - vect2[i])
    }
  }
  return(distance)
}

#Function to calculate Cosine Similarity
cosine_similarity <- function(vect1, vect2)
{
  numerator <- 0
  vect1_len_squ <- 0
  vect2_len_squ <- 0
  for(i in 1:length(vect1))
  {
    numerator = numerator + (vect1[i] * vect2[i])
    vect1_len_squ <-  vect1_len_squ + (vect1[i] * vect1[i])
    vect2_len_squ <-  vect2_len_squ + (vect2[i] * vect2[i])
  }
  distance <- numerator / ((vect1_len_squ^(1/2)) * (vect2_len_squ^(1/2)))
  return(distance)
}

#Function to print points in ranked order
print_closer_points <- function(distances, order)
{
  data_frame <- data.frame(distances,point_names)
  if(order=="asc")
  {
    sorted_points <- data_frame[with(data_frame, order(distances, point_names)),]
  }
  else
  {
    sorted_points <- data_frame[with(data_frame, order(-distances, point_names)),]
  }
  sorted_points <- rev(sorted_points)
  print(sorted_points,row.names=FALSE)
}

#Calculate the distances and display points in ranked order
dist_list <- c(euclidean_distance(x,x1),euclidean_distance(x,x2),euclidean_distance(x,x3),euclidean_distance(x,x4),euclidean_distance(x,x5))
cat("Ranked order using Euclidean distance:\n")
print_closer_points(dist_list,"asc")

dist_list <- c(manhattan_distance(x,x1),manhattan_distance(x,x2),manhattan_distance(x,x3),manhattan_distance(x,x4),manhattan_distance(x,x5))
cat("Ranked order using Manhattan distance:\n")
print_closer_points(dist_list,"asc")

dist_list <- c(supremum_distance(x,x1),supremum_distance(x,x2),supremum_distance(x,x3),supremum_distance(x,x4),supremum_distance(x,x5))
cat("Ranked order using Supremum distance:\n")
print_closer_points(dist_list,"asc")

dist_list <- c(cosine_similarity(x,x1),cosine_similarity(x,x2),cosine_similarity(x,x3),cosine_similarity(x,x4),cosine_similarity(x,x5))
cat("Ranked order using cosine similarity:\n")
print_closer_points(dist_list,"desc")


cat("\n\nAnswers for question 2.8 (b):\n")

#Function to normalize the data
normalize <- function(vect)
{
  denominator <- 0
  for(i in 1:length(vect))
  {
    denominator = denominator + (vect[i] * vect[i])
  }
  denominator = denominator ^ (1/2)
  result = vector()
  for(i in 1:length(vect))
  {
    result[i] = vect[i] / denominator
  }
  return(result)
}

#Normalize the input and display points in ranked order
n_x1 <- normalize(x1)
n_x2 <- normalize(x2)
n_x3 <- normalize(x3)
n_x4 <- normalize(x4)
n_x5 <- normalize(x5)
n_x <- normalize(x)

cat("\nNormalized data:\n")
cat("x1:    A1 =",n_x1[1],"  A2 =",n_x1[2],"\n")
cat("x2:    A1 =",n_x2[1],"  A2 =",n_x2[2],"\n")
cat("x3:    A1 =",n_x3[1],"  A2 =",n_x3[2],"\n")
cat("x4:    A1 =",n_x4[1],"   A2 =",n_x4[2],"\n")
cat("x5:    A1 =",n_x5[1],"  A2 =",n_x5[2],"\n")
cat("x :    A1 =",n_x[1],"  A2 =",n_x[2],"\n")

dist_list <- c(euclidean_distance(n_x,n_x1),euclidean_distance(n_x,n_x2),euclidean_distance(n_x,n_x3),euclidean_distance(n_x,n_x4),euclidean_distance(n_x,n_x5))
cat("\nRanked order using Euclidean distance:\n")
print_closer_points(dist_list,"asc")
