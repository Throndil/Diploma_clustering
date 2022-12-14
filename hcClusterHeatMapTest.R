install.packages("janitor")

library(janitor)
library(factoextra)
library(ggplot2) 
library(ape)
library(dplyr)
#warnings()

library(factoextra)
library(ggplot2)
library(ape)
theme_set(theme_minimal())
names <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
           "V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25",
           "V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39","V40",
           "V41","V42","V43","V44","V45","V46","V47","V48","V49","V50")

window_width = 5
step = 5

# Dataset v R 
mydata <- read.csv(file.choose(), header=T)
#mydata<-matrix(rnorm(11500),nrow = 11500,ncol = 50)
head(mydata,0)
column_names <- head(mydata,0)
#iris.labels <- iris$Species
#table(iris.labels)

# Data 
str(mydata)
#dim(mydata)

#m = matrix(1:8,nrow = 2)
#dim(m)
#m
#dim(m) <- c(4,2)
#m
#my_data_test = rep(head(mydata,0),5)

tvoricVectorov(window_width, step)

vectors_matrix <- matrix(unlist(vectors_list), ncol=step*50, nrow=num_rows, byrow=TRUE)
colnames(vectors_matrix) <- rep(names, times = step)
#write.csv(vectors_matrix, "vector_loop_list.csv", row.names = TRUE)
write.csv(vectors_matrix, "vector_loop_list_home_testing_7.csv", row.names = TRUE)



noise_matrix <- matrix(rnorm(num_rows*(step*50),0,0.5),nrow = num_rows,ncol = step*50)

# 50 stlpcov
#mydata_data = mydata[1:50]
#250 stlpcov
#mydata_data = vectors_matrix[1:50]
# Scale:
# Distancing

#mydata_data_std = mydata_data_std + noise_matrix

#plot(vectors_matrix[,26])
#mydata_data_std = scale(vectors_matrix)
mydata_data_std = vectors_matrix

mydata_data_std[is.nan(mydata_data_std)] <- rnorm((num_rows*(step*50)),0,0.5)


mydata_data_std <- janitor::remove_empty(mydata_data_std, which = "cols")
ncol(mydata_data_std)

plot(mydata_data_std[,26])

# Distance = factoextra funkcia - ?dist
# Ako default je euclidean
mydata.dist = dist(mydata_data_std)
which(is.nan(mydata.dist))

plot(mydata.dist)
#mydata.dist = dist(vectors_matrix)
#iris.dist = dist(iris_data_std)
#mydata_data_std = subset(mydata_data_std, select = -c(V26))

# Oproti k-means zhlukovaniu kde sme si museli definovat pocet zhlukov tu to nemusim
# To je z dovodu ze robime bud top down or bottom up pristup

# Hierarchical clustering
# ?hclust
hc.output_mydata <- hclust(mydata.dist, method = "complete")
# Pouzita bottom up pristup, cize mergujeme pozorovania
plot(hc.output_mydata)
which(is.nan(mydata.dist))
#test <- hclust(mydata.dist, method = "complete")

# Dendrogram
# https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
plot(hc.output_mydata)

# k hodnota predstavuje unikatnu hodnotu typov v tomto pripade to je typ kvetiny
# unique(iris$species)
rect.hclust(hc.output_mydata, k = 50, border = 2:5)

#plot(as.phylo(test), type = "fan")
#plot(test)
#nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")

#plot(test, nodePar = nodePar)

# Clusters
# ?cutree
mydata.clusters <- cutree(hc.output_mydata, k=50)

# Customized plot; remove labels
# Ukaz ktori stlpec je konstantny
#mydata_data_std[,apply(mydata_data_std, 2, var, na.rm=TRUE) != 0]
write.csv(mydata_data_std, "mydata_data_std.csv", row.names = TRUE)
which(apply(mydata_data_std, 2, var)==0)
mydata_data_std[ , which(apply(mydata_data_std, 1, var) != 0)]
summary(mydata_data_std)
sum(!is.finite(scale(mydata_data_std)))
#class(mydata_data_std)
sapply(mydata_data_std, class)
var(mydata_data_std[,42])
prcomp(mydata_data_std, scale = FALSE)

# Visualize
#rownames(iris_data_std) <- paste(iris$Species, 1:dim(iris)[1], sep = "_")
# Funkcia z factoextra
graph <- fviz_cluster(list(data=mydata_data_std, cluster = mydata.clusters), stand = FALSE)
graph

#table(iris.clusters, iris$Species)


tvoricVectorov <- function(window_width, step){
  
  counter = 1
  row_counter = 1
  row = 1
  num_rows = (nrow(mydata) - window_width) / step + 1
  vectors_list <- list()
  vectors <- matrix(ncol=step*50,nrow=num_rows)
  
  for(i in 1:num_rows){
    vector_test = mydata[row_counter,]
    #Este jeden for cyklus aby mohol prechadzat a appendovat nezavisle od zadaneho kroku?
    #Update 20.10, ano treba este jeden for cyklus kde bude appendovat tolko krat podla toho kolko
    #sa zadala velkost vektora
    counter <- counter + 1
    vector_test = append(vector_test, mydata[counter,])
    counter <- counter + 1
    vector_test = append(vector_test, mydata[counter,])
    counter <- counter + 1
    vector_test = append(vector_test, mydata[counter,])
    counter <- counter + 1
    vector_test = append(vector_test, mydata[counter,])
    vectors_list[[i]] <- vector_test 
    counter <- counter + 1
    #Pocitac riadkov, cize tu budem pripocitavat krok
    #Update 20.10, namiesto step bude asi ten window_width, teda keby bolo 1 tak sa posuvam
    #po jednom prvku, teda [1,2],[2,3],[3,4]
    row_counter <- row_counter + step
    #Posun riadku v subore nemenim, ostava rovnake
    #Update 20.10, treba pozriet aj toto ci je dobre
    row <- row + 1
    message("Processing image ", i, " of ", num_rows)
  }
}  