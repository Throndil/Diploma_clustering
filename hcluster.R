# https://www.youtube.com/watch?v=MAUs4484TG8&t=908s&ab_channel=SpencerPao

library(factoextra)

# Dataset v R o kvetinach
iris.labels <- iris$Species
table(iris.labels)

# Data 
str(iris)
# Prve styri stlpce
iris_data = iris[1:4]

# Scale:
# Distancing
iris_data_std = scale(iris_data)

# Distance = factoextra funkcia - ?dist
# Ako default je euclidean
iris.dist = dist(iris_data_std)

# Oproti k-means zhlukovaniu kde sme si museli definovat pocet zhlukov tu to nemusim
# To je z dovodu ze robime bud top down or bottom up pristup

# Hierarchical clustering
# ?hclust
hc.output_iris <- hclust(iris.dist, method = "complete")
# Pouzita bottom up pristup, cize mergujeme pozorovania
hc.output_iris


# Dendrogram
# https://stackoverflow.com/questions/23050928/error-in-plot-new-figure-margins-too-large-scatter-plot
plot(hc.output_iris)

# k hodnota predstavuje unikatnu hodnotu typov v tomto pripade to je typ kvetiny
# unique(iris$species)
rect.hclust(hc.output_iris, k = 3, border = 2:5)

# Clusters
# ?cutree
iris.clusters <- cutree(hc.output_iris, k=3)

# Visualize
rownames(iris_data_std) <- paste(iris$Species, 1:dim(iris)[1], sep = "_")
# Funkcia z factoextra
fviz_cluster(list(data=iris_data_std, cluster = iris.clusters))


table(iris.clusters, iris$Species)






