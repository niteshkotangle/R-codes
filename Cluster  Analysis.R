
########################################################
# Step 1 Import the data
########################################################
DT = read.csv("Mall_Visits.csv")

########################################################
# Step 2 Look at the first 5 rows of the data
########################################################

head(DT, 5)

#After looking at the data using head(DT, 5 )). Anything from the data would
#concern you regarding using the geometric distance measurement.

#While 6 of the "survey" data are on a similar scale, namely 1 7, there is one variable that is about 2 orders of magnitude larger: the Income variable. The
# Income variable will have disproportional impact on the similarity measure using geometric distance.

#To avoid such issues, one has to consider to scale or standardize the variables of interest. A common way of scaling data is to use z
# Look at the last 5 rows of the data
tail(DT, 5)

#Check regularities in the data
summary(DT)

########################################################
# Step 3 Scale the data. 
########################################################
# Note that the first column is ID number
# Scaling ID number does not make any sense

#Scaling by tapping into R s power programming feature. It can apply the same
ScaDT = apply(DT[, 2:9], 2, function(x) scale(x)) 

dim(DT)

summary(DT$Income)

#Try to standardize the "Fun" variable in R

MeanFN = mean(DT$Fun)

#There is one function scale() in R can scale the data for you. You can eitherscale the data one by one. 
SDFun = sd(DT$Fun)

ScaFn = (DT$Fun - MeanFN)/ SDFun

ScaFn
MeanFN
SDFun

# it may not be always necessary to scale the data. One downside of scaling data is the meaning can be lost. For instance, what does Income = -0.34 mean?
#Compared to MeanFun and SDFun 

CombineView = cbind(DT$Fun, ScaFun) 
CombineView

# Show the Euclidean distant for the first 10 customers

########################################################
# Step 4 we choose column 1:6 -- attitudinal questions for segmentation.
# Income and MallVisit will be used for profiling
########################################################

########################################################
# Step 5 we use Euclidean distance
########################################################

#We often use only a few of the data attributes for segmentation (the segmentation attributes ) and use some of the remaining ones
#(the profiling attributes ) only to profile the clusters. 
#we will use the 6 attitudinal questions (Fun, BadBudget , Eat, Buy,NotCare , Saving) for segmentation, and the remaining 2 (Income and MallVisits ) for profiling later.
#Just get a quick sense of the distance of the first 10 customers
EuclideanD <- dist(ScaDT[1:10, 1:6])
EuclideanD


# We cannot directly view EuclideanD by using View(EuclideanD)
# To view the results, we have to make it as a matrix.

EuclideanD = as.matrix(EuclideanD)
View(EuclideanD)

# Let's round up the numbers to the 10th digits
View( round(EuclideanD, 1) )


## We can define our own distance:
## The two variables are the same if the difference is less or equal to 2
# My_Distance_function<-function(x,y){sum(abs(x-y) > 2)}
# RawD = apply(ScaDT[1:10, ],1,function(i) 
#        apply(ScaDT[1:10, ],1,function(j) My_Distance_function(i,j) ))
# RawD = as.matrix(RawD)
# View( round(RawD, 1) )

########################################################
# Step 6: Cluster method
########################################################

## we first look at Hierarchical Cluster

# Euclidean distance again for all the customers
EuclideanD <- dist(ScaDT[, 1:6])

# hclust() function performs a hierarchical cluster anlaysis based on the distance

Hierarchical_Cluster <- hclust(EuclideanD)

# Plot the Dendrogram
#In Dendrogram, observations are "grouped together ", starting from pairs of individual observations which are the closest to each other, and merging smaller groups into larger ones
#depending on which groups are closest to each other. Eventually all our data are merged into one segment. The heights of the branches of the tree indicate how different the clusters

plot(Hierarchical_Cluster)

# save the Dendrogram plot

png(file="Dendrogram.png",width=700, height=500)

plot(Hierarchical_Cluster)

# Or plot the heatmap
#There is a quicker and fancier way to plot

heatmap(as.matrix(ScaDT[, 1:6]))

# save the plot

png(file="HeatMap_ClusterAnalysis.png", width=700, height=800)

heatmap(as.matrix(ScaDT[, 1:6]))

dev.off()

#From the dendrogram and heatmap, it is sensible to segment the customers into 3 groups. We assign the customers into the 3 segments and merge (be careful at this
HC_membership = as.vector(cutree(Hierarchical_Cluster, k = 3))
DT_HC = cbind(DT, HC_membership)
head(DT_HC)

########################################################
# Step 7: Profiling and interpretation
########################################################

#We can further explore the characteristics of the segments. First, we can have a quick look at the segment size by using

prop.table(table(DT_HC$HC_membership))
#As can be seen, the segment size for each group is 40 %, 30 % and 30

#We can compute the average of all the variables by groups . We can create a function to do it automatically for all the variables.
Mean_by_Group <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

Group_Character_HC = round( Mean_by_Group(DT_HC, DT_HC$HC_membership) )
View(Group_Character_HC)

write.csv(Group_Character_HC, "Group_Character_HC.csv", row.names = FALSE)


########################################################
# Step 6 revisited: K-means
########################################################
#kmeans is the most popular algorithm for cluster analysis due to its simplicity and efficiency. For large dataset (in terms of both rows and columns), K means
# clustering method is highly preferred to Hierarchical clustering method.

#To ensure each time you get the same initial "centroids", you can set up the random seed using set.seed (#),

set.seed(888)
kmeans_Cluster <- kmeans(ScaDT[, 1:6],centers= 3, iter.max=2000)
#Set the maximum rounds of iterations. 2000 should be sufficient.

Kmeans_membership = as.vector(kmeans_Cluster$cluster)
DT_Kmeans = cbind(DT, Kmeans_membership)
head(DT_Kmeans)

#Similar to HC method, we can explore the characteristics of the segments. First, we can have a quick look at the segment size by using
prop.table(table(DT_Kmeans$Kmeans_membership))

#As can be seen, the segment size for each group is 40%, 30% and 30%.

#We can compute the average of all the variables by groups

Group_Character_Kmeans = round( Mean_by_Group(DT_Kmeans, DT_Kmeans$Kmeans_membership) )
View(Group_Character_Kmeans)

