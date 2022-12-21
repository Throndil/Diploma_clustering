options(java.parameters = "- Xmx1024m")
library(janitor)
library(factoextra)
library(ggplot2) 
library(ape)
library(plyr)
library(dplyr)
library(openxlsx)
library(plotrix)
library(ggforce) 
library(xlsx)
library(Rtsne)
library(factoextra)
library(ggplot2)
library(ggpubr)
library(ape)
library(RColorBrewer)

names <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
           "V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25",
           "V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39","V40",
           "V41","V42","V43","V44","V45","V46","V47","V48","V49","V50")

sheet_names <- c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6","Cluster7","Cluster8","Cluster9","Cluster10")

window_width = 50                                        #Šírka okna
step = 10                                                #Posun
number_of_clusters = 10                                  #Počet zhlukov

original_heatmap <- read.csv(file.choose(), header=T)              #Načítanie teplotnej mapy z CSV súboru z programu p. Krnáča, mydata = teplotna_mapa

column_names <- head(original_heatmap,0)                           #Vlož názvy V1 až V50 ako názvy stĺpcov

counter = 1                                              #Celkové počítadlo
current_row_heatmap = 1                                          #Počítadlo riadkov
number_rows_after_vectoring = (nrow(original_heatmap) - window_width) / step + 1      #Celkový počet riadkov, výpočet je: (počet riadkov teplotnej mapy - zvolená šírka vektora) / posun
heatmap_vectors_list_base <- list()                                   #Vytvorenie listu do ktorého uložím vektory
for(i in 1:number_rows_after_vectoring){                                    #Prechádzam počtom riadkov
  if (window_width == 1) {                               #Ak používateľ zvolil šírku okna 1, znamená to že vektorová mapa bude rovnaká ako teplotná mapa
    heatmap_vectors_matrix_base <- original_heatmap
    break
  }
  if (window_width != 1) {
    heatmap_column = original_heatmap[current_row_heatmap,]                   #Pomocný vector do ktorého si vložím prvotný riadok a na neho nabaľujem ostatné
    counter <- current_row_heatmap + 1                           #Zvýšim počítadlo
    for (j in 1:window_width) {                          #Cyklus sa toľkokrát ako bola zadaná šírka okna
      if (j != window_width) {                           #Telo cyklu, pokým sa cyklus nerovná šírke okna tak nabaľujem riadky na pomocný vektor
        heatmap_column = append(heatmap_column, original_heatmap[counter,])
        counter <- counter + 1
      }
      if (j == window_width) {                           #Ak sme na poslednom kroku cyklu, už nepribaľujeme a pribalíme vektor do listu na pozíciu i
        heatmap_vectors_list_base[[i]] <- heatmap_column 
        current_row_heatmap <- current_row_heatmap + step                #Aktualizujem počítadlo riadkov
      }
    }
  }
  message("Processing image ", i, " of ", number_rows_after_vectoring)      #Pomocný výpis
  if (i == number_rows_after_vectoring) {
    if (window_width == 1) {                             #Ak je šírka okna 1, matica je vlastne samotné dáta
      heatmap_vectors_matrix_base <- original_heatmap
      colnames(heatmap_vectors_matrix_base) <- rep(names, times = ncol(original_heatmap))    #Nastavenie názvu stĺpcov pre vektorovú mapu
    }else{
      heatmap_vectors_matrix_base <- matrix(unlist(heatmap_vectors_list_base), ncol=window_width*ncol(original_heatmap), nrow=number_rows_after_vectoring, byrow=TRUE)
      colnames(heatmap_vectors_matrix_base) <- rep(names, times = ncol(original_heatmap))
    }
  }
}

heatmap_distance_matrix = dist(heatmap_vectors_matrix_base)                         #Vypočítanie vzdialeností matice
heatmap_hclust_output <- hclust(heatmap_distance_matrix, method = "complete") #Funkcia hclust do ktorej ako parameter posielame vzdialenosť matice (už vektorová mapa) a metódu "complete"
plot(heatmap_hclust_output)                                       #Dendrogram hclust
heatmap_clusters_base <- cutree(heatmap_hclust_output, k=number_of_clusters) #Zvolenie počtu zhlukov a orezanie
graph <- fviz_cluster(list(data=heatmap_vectors_matrix_base, cluster = heatmap_clusters_base), stand = FALSE, labelsize = 0) #Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou, ak chceme definovať osi použiť axes = c(x,y)
graph

### ODSTRANENIE ZHLUKOV VELKOSTI 1
removed_columns_first_reduction = 0
rows_in_cluster = 0
current_cluster = 1
row_index = 0
heatmap_vectors_list_after_first_reduction = heatmap_vectors_list_base
heatmap_clusters_after_first_reduction = heatmap_clusters_base
heatmap_vectors_matrix_after_first_reduction = heatmap_vectors_matrix_base
for (i in 1:number_of_clusters) {
  for (j in 1:length(heatmap_clusters_after_first_reduction)) {
    if (heatmap_clusters_after_first_reduction[j] == current_cluster) {          #Musi byt taketo pocitadlo zhluku inac by vratil anomali na indexe 1 pre kazdy zhluk
      rows_in_cluster <- rows_in_cluster + 1
      row_index = j
    }
    if (j == length(heatmap_clusters_after_first_reduction) && rows_in_cluster == 1) {
      message("Anomalia v zhluku: ", i, ", na indexe: ", row_index)
      heatmap_vectors_list_after_first_reduction[[row_index]] <- NULL
      heatmap_clusters_after_first_reduction <- heatmap_clusters_after_first_reduction[-c(row_index)]
      if (removed_columns_first_reduction == 0 || removed_columns_first_reduction < 0) {
        removed_columns_first_reduction <- heatmap_vectors_matrix_after_first_reduction[row_index,]
      }else{
        removed_columns_first_reduction <- rbind(removed_columns_first_reduction,heatmap_vectors_matrix_after_first_reduction[row_index,])
      }
      heatmap_vectors_matrix_after_first_reduction <- heatmap_vectors_matrix_after_first_reduction[-row_index,]
    }
  }
  rows_in_cluster = 0
  row_index = 0
  current_cluster <- current_cluster + 1
}

## DRUHE ZHLUKOVANIE
table(heatmap_clusters_base)
table(heatmap_clusters_after_first_reduction)
dim(heatmap_vectors_matrix_base)
dim(heatmap_vectors_matrix_after_first_reduction)
heatmap_distance_matrix_after_first_reduction = dist(heatmap_vectors_matrix_after_first_reduction)
heatmap_hclust_output_after_first_reduction <- hclust(heatmap_distance_matrix_after_first_reduction, method = "complete")
heatmap_clusters_after_first_reduction <- cutree(heatmap_hclust_output_after_first_reduction, k=number_of_clusters)
table(heatmap_clusters_after_first_reduction)
dim(heatmap_vectors_matrix_after_first_reduction)
graph <- fviz_cluster(list(data=heatmap_vectors_matrix_after_first_reduction, cluster = heatmap_clusters_after_first_reduction), stand = FALSE, labelsize = 0)
graph


###### UKLADANIE SUBOROV AKO TREBA BEZ RIADKOV STLPCOV, Z MATICE NA DATAFRAME
heatmap_clusters_dataframe = as.data.frame(heatmap_clusters_after_first_reduction)
names(heatmap_clusters_dataframe) <- NULL
write.csv(heatmap_clusters_dataframe, "pilot_project_10shift_50width_clustersv2.csv", row.names = FALSE, col.names = FALSE)

heatmap_vectors_dataframe = as.data.frame(heatmap_vectors_matrix_after_first_reduction)
names(heatmap_vectors_dataframe) <- NULL
write.csv(heatmap_vectors_dataframe, "pilot_project_10shift_50width_heatmap_vectorsv2.csv", row.names = FALSE,  col.names = FALSE)


m = nrow(heatmap_vectors_matrix_after_first_reduction)  
n = ncol(heatmap_vectors_matrix_after_first_reduction)
y = heatmap_vectors_matrix_after_first_reduction[1:m,1:n] #y obsahuje maticu dat
z <- heatmap_clusters_after_first_reduction[1:m]   #z obsahuje vektor zhlukov
clusters_length = length(heatmap_clusters_after_first_reduction)
clusters_list <- list()
radius_list <- list()
list_counter = 1
cdf_counter = 1
percent = .99
length_d = 0
radius_dataframe = data.frame()
clusters_dataframe = data.frame()

for (i in 1:number_of_clusters) {
  message("Processing image ", i, " of ", number_of_clusters)
  v = y[z == i,]
  if (is.null(nrow(v))) {
    d = sqrt(v^2)
    clusters_list[[i]] = d
    length_d = length(d)
    clusters_matrix = matrix(unlist(clusters_list[[i]]), ncol=length_d, nrow=1, byrow=TRUE)
    d_dataframe = as.data.frame(clusters_matrix)
    write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE,col.names = FALSE)
    CDF = ecdf(d)
    q = quantile(CDF,c(percent))
    radius_dataframe = rbind(radius_dataframe,q)
  }else{
    c = colMeans(v)
    u = t(v) - c
    d = sqrt(colSums(u^2))
    clusters_list[[i]] = d
    length_d = length(d)
    clusters_matrix = matrix(unlist(clusters_list[[i]]), ncol=length_d, nrow=1, byrow=TRUE)
    d_dataframe = as.data.frame(clusters_matrix)
    write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE,col.names = FALSE)
    CDF = ecdf(d)
    q = quantile(CDF,c(percent))
    radius_dataframe = rbind(radius_dataframe,q)
  }
  if (i == number_of_clusters) {
    write.xlsx(radius_dataframe,file = "pilot_project_radius.xlsx", sheetName = "Clusters_radius",row.names = FALSE,col.names = FALSE,append = TRUE)
  }
}


## POZERANIE PRE BODY MIMO POLOMERU ###### AUTOMATICKY POSUN INDEXOV ########
index_to_remove = 0
index_to_to_remove = 0
how_many_removed = 0
heatmap_clusters_after_second_reduction = heatmap_clusters_after_first_reduction
heatmap_vectors_matrix_after_second_reduction = heatmap_vectors_matrix_after_first_reduction
removed_columns_second_reduction = 0
for (j in 1:number_of_clusters) {
  message("Novy zhluk: ", j)
  for (i in 1:length(clusters_list[[j]])) {
    index_to_remove = which(heatmap_clusters_after_second_reduction %in% c(j))
    if (clusters_list[[j]][i] > radius_dataframe[j,]) {
      message("Zhluk: ", j, " ,bod: ",clusters_list[[j]][i], " ,je mimo polomeru zhluku: ", j, " ,ktori je: ", radius_dataframe[j,])
      message("Index v zhluku: ", i)
      index_to_to_remove = index_to_remove[i]
      message("Index v datach: ", index_to_remove[i])
      heatmap_clusters_after_second_reduction <- heatmap_clusters_after_second_reduction[-c(index_to_to_remove)]
      if (removed_columns_second_reduction == 0 || removed_columns_second_reduction < 0) {
        removed_columns_second_reduction <- heatmap_vectors_matrix_after_second_reduction[index_to_to_remove,]
      }else{
        removed_columns_second_reduction <- rbind(removed_columns_second_reduction,heatmap_vectors_matrix_after_second_reduction[index_to_to_remove,])
      }
      heatmap_vectors_matrix_after_second_reduction <- heatmap_vectors_matrix_after_second_reduction[-index_to_to_remove,]
      how_many_removed <- how_many_removed + 1
    }
  }
  if (j == number_of_clusters) {
    message("Pocet odstranenych bodov: ", how_many_removed)
  }
}

table(heatmap_clusters_after_first_reduction)
table(heatmap_clusters_after_second_reduction)
dim(heatmap_vectors_matrix_after_first_reduction)
dim(heatmap_vectors_matrix_after_second_reduction)

graph_first_reduction <- fviz_cluster(list(data=heatmap_vectors_matrix_after_first_reduction, cluster = heatmap_clusters_after_first_reduction), stand = FALSE)
graph_second_reduction <- fviz_cluster(list(data=heatmap_vectors_matrix_after_second_reduction, cluster = heatmap_clusters_after_second_reduction), stand = FALSE)
attach(mtcars)
par(mfrow=c(3,1))
plot(graph_first_reduction)
plot(graph_second_reduction)

heatmap_clusters_dataframe = as.data.frame(heatmap_clusters_after_second_reduction)
names(heatmap_clusters_dataframe) <- NULL
write.csv(heatmap_clusters_dataframe, "pilot_project_10shift_50width_cluster_second_reduction.csv", row.names = FALSE, col.names = FALSE)

heatmap_vectors_dataframe = as.data.frame(heatmap_vectors_matrix_after_second_reduction)
names(heatmap_vectors_dataframe) <- NULL
write.csv(heatmap_vectors_dataframe, "pilot_project_10shift_50width_heatmap_vectors_second_reduction.csv", row.names = FALSE,  col.names = FALSE)


# 
# tsne_model_1 = Rtsne(heatmap_vectors_matrix_after_second_reduction, check_duplicates=FALSE, pca=TRUE, perplexity=230, theta=0.0, dims=3, normalize = FALSE)
# d_tsne_1 = as.data.frame(tsne_model_1$Y)
# d_tsne_1_original=d_tsne_1
# fit_cluster_hierarchical=hclust(dist(d_tsne_1))
# d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=number_of_clusters))
# table(d_tsne_1_original$cl_hierarchical)


tsne_model_1 = Rtsne(heatmap_vectors_matrix_after_second_reduction, check_duplicates=FALSE, pca=FALSE, perplexity=220, theta=0.0, dims=3, normalize = FALSE, max_iter = 2000)
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1_original=d_tsne_1
fit_cluster_hierarchical=hclust(dist(d_tsne_1))
#d_tsne_1_original$cl_hierarchical = factor(fit_cluster_hierarchical)


tsne_model_1 = Rtsne(heatmap_vectors_matrix_after_second_reduction, check_duplicates=FALSE, pca=FALSE, perplexity=220, theta=0.0, dims=3, normalize = FALSE, max_iter = 2000)
dim(heatmap_vectors_matrix_after_second_reduction)
heatmap_vectors_matrix_after_second_reduction[1:3,1:4]
plot(heatmap_vectors_matrix_after_second_reduction[1,], type = "l")
#zobrazenie prvy riadok 1:50 stlpcov, po 1 riadok 51:100 stlpcov
matplot(cbind(heatmap_vectors_matrix_after_second_reduction[1,1:50], +
                heatmap_vectors_matrix_after_second_reduction[1,51:100]),
                  type = "l")

str(tsne_model_1)
d_tsne_1 = as.data.frame(tsne_model_1$Y)
d_tsne_1_original=d_tsne_1
plot(d_tsne_1_original)
fit_cluster_hierarchical=hclust(dist(d_tsne_1))
d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=10))
#d_tsne_1_original$cl_hierarchical = factor(heatmap_clusters_after_second_reduction)
plot(fit_cluster_hierarchical)

nblucst <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "wss")
plot(nblucst)
#nblucst <- fviz_nbclust(heatmap_vectors_matrix_after_second_reduction, FUN = hcut, method = "wss")
vector <- as.vector(nblucst$data)
plot(as.numeric(vector[,1])*as.numeric(vector[,2]), type = "l")

tsne11 <- d_tsne_1_original$cl_hierarchical
table(tsne11)

table(d_tsne_1_original$cl_hierarchical)
table(heatmap_clusters_after_second_reduction)

## https://stackoverflow.com/questions/40821591/how-to-print-the-optimal-number-of-clusters-using-fviz-nbclust
## http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determiningthe-optimal-number-of-clusters-3-must-know-methods/
nblucst_hclust <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "wss", print.summary = TRUE)
nblucst_hclust_data <- nblucst_hclust$data
#max_cluster_hclust <- as.numeric(nblucst_hclust_data$clusters[which.max(nblucst_hclust_data$y)])
#plot(max_cluster_hclust)
plot(nblucst_hclust)
delta_result = data.frame(col1 = numeric())
for (i in 1:length(nblucst_hclust_data$clusters)) {
  if (i > length(nblucst_hclust_data$clusters) - 2) {
    break
  }
  delta_i_1 = nblucst_hclust_data$y[i] - nblucst_hclust_data$y[i+1]
  delta_i_2 = nblucst_hclust_data$y[i] - nblucst_hclust_data$y[i+2]
  new_row <- c(delta_i_1/delta_i_2)
  delta_result[nrow(delta_result) + 1, ] <- new_row
}
##https://stackoverflow.com/questions/43662977/how-to-find-the-indexes-of-the-minimum-value-in-whole-dataframe
best_cluster = which(delta_result == min(delta_result, na.rm = TRUE))
plot(nblucst_hclust_data$y, type = "b")
abline(v = best_cluster, col = "blue", lwd = 2)

#ggplot(nblucst_hclust_data, aes(clusters, y, group = 1)) +  geom_line()


nbclust_kmeans <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = kmeans, method = "wss", print.summary = TRUE)
nbclust_kmeans_data <- nbclust_kmeans$data
max_cluster_kmeans <- as.numeric(nbclust_kmeans_data$clusters[which.max(nbclust_kmeans_data$y)])
#plot(max_cluster_kmeans)
plot(nbclust_kmeans_data$y, type = "b")



nbclust_pam <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::pam, method = "wss")
nbclust_pam_data <- nbclust_pam$data
#max_cluster_pam <- as.numeric(nbclust_pam_data$clusters[which.max(nbclust_pam_data$y)])
#plot(max_cluster_pam)
plot(nbclust_pam_data$y, type = "b")

nbclust_clara <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::clara, method = "wss")
nbclust_clara_data <- nbclust_clara$data
max_cluster_clara <- as.numeric(nbclust_clara_data$clusters[which.max(nbclust_clara_data$y)])
plot(max_cluster_clara)
plot(nbclust_clara)

nbclust_fanny <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::fanny, method = "wss")
nbclust_fanny_data <- nbclust_fanny$data
max_cluster_fanny <- as.numeric(nbclust_fanny_data$clusters[which.max(nbclust_fanny_data$y)])
plot(max_cluster_fanny)
plot(nbclust_fanny)

## SILHOUTTE
nblucst_hclust_silhoutte <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "silhouette", print.summary = TRUE)
nblucst_hclust_data_silhoutte <- nblucst_hclust_silhoutte$data
max_cluster_hclust_silhoutte <- as.numeric(nblucst_hclust_data_silhoutte$clusters[which.max(nblucst_hclust_data_silhoutte$y)])
plot(max_cluster_hclust_silhoutte)
plot(nblucst_hclust_data_silhoutte$y,type = "b")

nbclust_kmeans_silhoutte <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = kmeans, method = "silhouette", print.summary = TRUE)
nbclust_kmeans_data_silhoutte <- nbclust_kmeans_silhoutte$data
max_cluster_kmeans_silhoutte <- as.numeric(nbclust_kmeans_data_silhoutte$clusters[which.max(nbclust_kmeans_data_silhoutte$y)])
plot(max_cluster_kmeans_silhoutte)
y.vector <- as.vector(nbclust_kmeans_silhoutte$data$clusters)
points(nbclust_kmeans_silhoutte$data$clusters)
plot(nbclust_kmeans_silhoutte)

nbclust_pam_silhoutte <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::pam, method = "silhouette")
nbclust_pam_data_silhoutte <- nbclust_pam_silhoutte$data
max_cluster_pam_silhoutte <- as.numeric(nbclust_pam_data_silhoutte$clusters[which.max(nbclust_pam_data_silhoutte$y)])
plot(max_cluster_pam_silhoutte)
plot(nbclust_pam_silhoutte)

nbclust_clara_silhoutte <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::clara, method = "silhouette")
nbclust_clara_data_silhoutte <- nbclust_clara_silhoutte$data
max_cluster_clara_silhoutte <- as.numeric(nbclust_clara_data_silhoutte$clusters[which.max(nbclust_clara_data_silhoutte$y)])
plot(max_cluster_clara_silhoutte)
plot(nbclust_clara_silhoutte)

nbclust_fanny_silhoutte <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::fanny, method = "silhouette")
nbclust_fanny_data_silhoutte <- nbclust_fanny_silhoutte$data
max_cluster_fanny_silhoutte <- as.numeric(nbclust_fanny_data_silhoutte$clusters[which.max(nbclust_fanny_data_silhoutte$y)])
plot(max_cluster_fanny_silhoutte)
plot(nbclust_fanny_silhoutte)

## GAP_STAT
nblucst_hclust_gap_stat <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "gap_stat", print.summary = TRUE)
nblucst_hclust_data_gap_stat <- nblucst_hclust_gap_stat$data
#max_cluster_hclust_gap_stat <- as.numeric(nblucst_hclust_data_gap_stat$clusters[which.max(nblucst_hclust_data_gap_stat$y)])
plot(nblucst_hclust_gap_stat)
plot(nblucst_hclust_data_gap_stat$y,type = "b")

## https://www.biostars.org/p/320710/
MyKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=50))
nbclust_kmeans_gap_stat <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = MyKmeansFUN, method = "gap_stat", print.summary = TRUE)
nbclust_kmeans_data_gap_stat <- nbclust_kmeans_gap_stat$data
max_cluster_kmeans_gap_stat <- as.numeric(nbclust_kmeans_data_gap_stat$clusters[which.max(nbclust_kmeans_data_gap_stat$y)])
plot(max_cluster_kmeans_gap_stat)
plot(nbclust_kmeans_gap_stat)

nbclust_pam_gap_stat <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::pam, method = "gap_stat")
nbclust_pam_data_gap_stat <- nbclust_pam_gap_stat$data
max_cluster_pam_gap_stat <- as.numeric(nbclust_pam_data_gap_stat$clusters[which.max(nbclust_pam_data_gap_stat$y)])
plot(max_cluster_pam_gap_stat)
plot(nbclust_pam_gap_stat)

nbclust_clara_gap_stat <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::clara, method = "gap_stat")
nbclust_clara_data_gap_stat <- nbclust_clara_gap_stat$data
max_cluster_clara_gap_stat <- as.numeric(nbclust_clara_data_gap_stat$clusters[which.max(nbclust_clara_data_gap_stat$y)])
plot(max_cluster_clara_gap_stat)
plot(nbclust_clara_gap_stat)

nbclust_fanny_gap_stat<- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::fanny, method = "gap_stat")
nbclust_fanny_data_gap_stat <- nbclust_fanny_gap_stat$data
max_cluster_fanny_gap_stat <- as.numeric(nbclust_fanny_data_gap_stat$clusters[which.max(nbclust_fanny_data_gap_stat$y)])
plot(max_cluster_fanny_gap_stat)
plot(nbclust_fanny_gap_stat)

## MANHATTAN DISTANCE
manhattan_dist <- dist(d_tsne_1, method = "manhattan")
nblucst_hclust_wss_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "wss", print.summary = TRUE, diss = manhattan_dist)
nblucst_hclust_data_wss_manhattan <- nblucst_hclust_wss_manhattan$data
max_cluster_hclust_wss_manhattan <- as.numeric(nblucst_hclust_data_wss_manhattan$clusters[which.max(nblucst_hclust_data_wss_manhattan$y)])
plot(max_cluster_hclust_wss_manhattan)
plot(nblucst_hclust_data_wss_manhattan$y, type= "b")

delta_result = data.frame(col1 = numeric())
for (i in 1:length(nblucst_hclust_data_wss_manhattan$clusters)) {
  if (i > length(nblucst_hclust_data_wss_manhattan$clusters) - 2) {
    break
  }
  delta_i_1 = nblucst_hclust_data_wss_manhattan$y[i] - nblucst_hclust_data_wss_manhattan$y[i+1]
  delta_i_2 = nblucst_hclust_data_wss_manhattan$y[i] - nblucst_hclust_data_wss_manhattan$y[i+2]
  new_row <- c(delta_i_1/delta_i_2)
  delta_result[nrow(delta_result) + 1, ] <- new_row
}
##https://stackoverflow.com/questions/43662977/how-to-find-the-indexes-of-the-minimum-value-in-whole-dataframe
best_cluster = which(delta_result == min(delta_result, na.rm = TRUE))
plot(nblucst_hclust_data_wss_manhattan$y, type = "b")
abline(v = best_cluster, col = "blue", lwd = 2)

nbclust_kmeans_wss_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = kmeans, method = "wss", print.summary = TRUE, diss = manhattan_dist)
nbclust_kmeans_data_wss_manhattan <- nbclust_kmeans_wss_manhattan$data
max_cluster_kmeans_wss_manhattan <- as.numeric(nbclust_kmeans_data_wss_manhattan$clusters[which.max(nbclust_kmeans_data_wss_manhattan$y)])
plot(max_cluster_kmeans_wss_manhattan)
plot(nbclust_kmeans_wss_manhattan)

nbclust_pam_wss_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::pam, method = "wss", diss = manhattan_dist)
nbclust_pam_data_wss_manhattan <- nbclust_pam_wss_manhattan$data
max_cluster_pam_wss_manhattan <- as.numeric(nbclust_pam_data_wss_manhattan$clusters[which.max(nbclust_pam_data_wss_manhattan$y)])
plot(max_cluster_pam_wss_manhattan)
plot(nbclust_pam_wss_manhattan)

nbclust_clara_wss_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::clara, method = "wss", diss = manhattan_dist)
nbclust_clara_data_wss_manhattan <- nbclust_clara_wss_manhattan$data
max_cluster_clara_wss_manhattan <- as.numeric(nbclust_clara_data_wss_manhattan$clusters[which.max(nbclust_clara_data_wss_manhattan$y)])
plot(max_cluster_clara_wss_manhattan)
plot(nbclust_clara_wss_manhattan)

nbclust_fanny_wss_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::fanny, method = "wss", diss = manhattan_dist)
nbclust_fanny_data_wss_manhattan <- nbclust_fanny_wss_manhattan$data
max_cluster_fanny_wss_manhattan <- as.numeric(nbclust_fanny_data_wss_manhattan$clusters[which.max(nbclust_fanny_data_wss_manhattan$y)])
plot(max_cluster_fanny_wss_manhattan)
plot(nbclust_fanny_wss_manhattan)

## MANHATTAN DISTANCE - SILHOUETTE
manhattan_dist <- dist(d_tsne_1, method = "manhattan")
nblucst_hclust_silhouette_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "silhouette", print.summary = TRUE, diss = manhattan_dist)
nblucst_hclust_data_silhouette_manhattan <- nblucst_hclust_silhouette_manhattan$data
max_cluster_hclust_silhouette_manhattan <- as.numeric(nblucst_hclust_data_silhouette_manhattan$clusters[which.max(nblucst_hclust_data_silhouette_manhattan$y)])
plot(max_cluster_hclust_silhouette_manhattan)
plot(nblucst_hclust_silhouette_manhattan)

nbclust_kmeans_silhouette_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = kmeans, method = "silhouette", print.summary = TRUE, diss = manhattan_dist)
nbclust_kmeans_data_silhouette_manhattan <- nbclust_kmeans_silhouette_manhattan$data
max_cluster_kmeans_silhouette_manhattan <- as.numeric(nbclust_kmeans_data_silhouette_manhattan$clusters[which.max(nbclust_kmeans_data_silhouette_manhattan$y)])
plot(max_cluster_kmeans_silhouette_manhattan)
plot(nbclust_kmeans_silhouette_manhattan)

nbclust_pam_silhouette_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::pam, method = "silhouette", diss = manhattan_dist)
nbclust_pam_data_silhouette_manhattan <- nbclust_pam_silhouette_manhattan$data
max_cluster_pam_silhouette_manhattan <- as.numeric(nbclust_pam_data_silhouette_manhattan$clusters[which.max(nbclust_pam_data_silhouette_manhattan$y)])
plot(max_cluster_pam_silhouette_manhattan)
plot(nbclust_pam_silhouette_manhattan)

nbclust_clara_silhouette_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::clara, method = "silhouette", diss = manhattan_dist)
nbclust_clara_data_silhouette_manhattan <- nbclust_clara_silhouette_manhattan$data
max_cluster_clara_silhouette_manhattan <- as.numeric(nbclust_clara_data_silhouette_manhattan$clusters[which.max(nbclust_clara_data_silhouette_manhattan$y)])
plot(max_cluster_clara_silhouette_manhattan)
plot(nbclust_clara_silhouette_manhattan)

nbclust_fanny_silhouette_manhattan <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::fanny, method = "silhouette", diss = manhattan_dist)
nbclust_fanny_data_silhouette_manhattan <- nbclust_fanny_silhouette_manhattan$data
max_cluster_fanny_silhouette_manhattan <- as.numeric(nbclust_fanny_data_silhouette_manhattan$clusters[which.max(nbclust_fanny_data_silhouette_manhattan$y)])
plot(max_cluster_fanny_silhouette_manhattan)
plot(nbclust_fanny_silhouette_manhattan)

## https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d
d_tsne_1_original_center = colMeans(d_tsne_1_original)
d_tsne_1_original_covariance = cov(d_tsne_1_original)
mahalanobis_dist <- mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance)
## https://statisticsglobe.com/r-warning-items-to-replace-not-multiple-of-length
## WSS_MAHALANOBIS
nblucst_hclust_wss_mahal <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = hcut, method = "wss", print.summary = TRUE, diss = mahalanobis_dist)
nblucst_hclust_data_wss_mahal <- nblucst_hclust_wss_mahal$data
max_cluster_hclust_wss_mahal <- as.numeric(nblucst_hclust_data_wss_mahal$clusters[which.max(nblucst_hclust_data_wss_mahal$y)])
plot(max_cluster_hclust_wss_mahal)
plot(nblucst_hclust_data_wss_mahal$y, type = "b")

delta_result = data.frame(col1 = numeric())
for (i in 1:length(nblucst_hclust_data_wss_mahal$clusters)) {
  if (i > length(nblucst_hclust_data_wss_mahal$clusters) - 2) {
    break
  }
  delta_i_1 = nblucst_hclust_data_wss_mahal$y[i] - nblucst_hclust_data_wss_mahal$y[i+1]
  delta_i_2 = nblucst_hclust_data_wss_mahal$y[i] - nblucst_hclust_data_wss_mahal$y[i+2]
  new_row <- c(delta_i_1/delta_i_2)
  delta_result[nrow(delta_result) + 1, ] <- new_row
}
##https://stackoverflow.com/questions/43662977/how-to-find-the-indexes-of-the-minimum-value-in-whole-dataframe
best_cluster = which(delta_result == min(delta_result, na.rm = TRUE))
plot(nblucst_hclust_data_wss_mahal$y, type = "b")
abline(v = best_cluster, col = "blue", lwd = 2)

nbclust_kmeans_wss_mahal <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = kmeans, method = "wss", print.summary = TRUE, diss = mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance))
nbclust_kmeans_data_wss_mahal <- nbclust_kmeans_wss_mahal$data
max_cluster_kmeans_wss_mahal <- as.numeric(nbclust_kmeans_data_wss_mahal$clusters[which.max(nbclust_kmeans_data_wss_mahal$y)])
plot(max_cluster_kmeans_wss_mahal)
plot(nbclust_kmeans_wss_mahal)

nbclust_pam_wss_mahal <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::pam, method = "wss", diss = mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance))
nbclust_pam_data_wss_mahal <- nbclust_pam_wss_mahal$data
max_cluster_pam_wss_mahal <- as.numeric(nbclust_pam_data_wss_mahal$clusters[which.max(nbclust_pam_data_wss_mahal$y)])
plot(max_cluster_pam_wss_mahal)
plot(nbclust_pam_wss_mahal)

nbclust_clara_wss_mahal <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::clara, method = "wss", diss = mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance))
nbclust_clara_data_wss_mahal <- nbclust_clara_wss_mahal$data
max_cluster_clara_wss_mahal <- as.numeric(nbclust_clara_data_wss_mahal$clusters[which.max(nbclust_clara_data_wss_mahal$y)])
plot(max_cluster_clara_wss_mahal)
plot(nbclust_clara_wss_mahal)

nbclust_fanny_wss_mahal <- fviz_nbclust(d_tsne_1,k.max = 20, FUN = cluster::fanny, method = "wss", diss = mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance))
nbclust_fanny_data_wss_mahal <- nbclust_fanny_wss_mahal$data
max_cluster_fanny_wss_mahal <- as.numeric(nbclust_fanny_data_wss_mahal$clusters[which.max(nbclust_fanny_data_wss_mahal$y)])
plot(max_cluster_fanny_wss_mahal)
plot(nbclust_fanny_wss_mahal)

## SILHOUETTE_MAHALANOBIS
tsne_model_2 = Rtsne(heatmap_vectors_matrix_after_second_reduction, check_duplicates=FALSE, pca=FALSE, perplexity=220, theta=0.0, dims=2, normalize = FALSE, max_iter = 2000)
d_tsne_2 = as.data.frame(tsne_model_2$Y)
d_tsne_2_original=d_tsne_2

d_tsne_2_original_center = colMeans(d_tsne_2_original)
d_tsne_2_original_covariance = cov(d_tsne_2_original)
mahalanobis_dist_2 <- mahalanobis(x = d_tsne_2_original,center = d_tsne_2_original_center,cov = d_tsne_2_original_covariance)

nblucst_hclust_silhouette_mahal <- fviz_nbclust(d_tsne_2,k.max = 20, FUN = hcut, method = "silhouette", print.summary = TRUE, diss = mahalanobis_dist_2)
nblucst_hclust_data_silhouette_mahal <- nblucst_hclust_silhouette_mahal$data
max_cluster_hclust_silhouette_mahal <- as.numeric(nblucst_hclust_data_silhouette_mahal$clusters[which.max(nblucst_hclust_data_silhouette_mahal$y)])
plot(max_cluster_hclust_silhouette_mahal)
plot(nblucst_hclust_silhouette_mahal)

nbclust_kmeans_silhouette_mahal <- fviz_nbclust(d_tsne_2,k.max = 20, FUN = kmeans, method = "silhouette", print.summary = TRUE, diss = mahalanobis_dist_2)
nbclust_kmeans_data_silhouette_mahal <- nbclust_kmeans_silhouette_mahal$data
max_cluster_kmeans_silhouette_mahal <- as.numeric(nbclust_kmeans_data_silhouette_mahal$clusters[which.max(nbclust_kmeans_data_silhouette_mahal$y)])
plot(max_cluster_kmeans_silhouette_mahal)
plot(nbclust_kmeans_silhouette_mahal)

nbclust_pam_silhouette_mahal <- fviz_nbclust(d_tsne_2,k.max = 20, FUN = cluster::pam, method = "silhouette", diss = mahalanobis(x = d_tsne_2_original,center = d_tsne_2_original_center,cov = d_tsne_2_original_covariance))
nbclust_pam_data_silhouette_mahal <- nbclust_pam_silhouette_mahal$data
max_cluster_pam_silhouette_mahal <- as.numeric(nbclust_pam_data_silhouette_mahal$clusters[which.max(nbclust_pam_data_silhouette_mahal$y)])
plot(max_cluster_pam_silhouette_mahal)
plot(nbclust_pam_silhouette_mahal)

nbclust_clara_silhouette_mahal <- fviz_nbclust(d_tsne_2,k.max = 20, FUN = cluster::clara, method = "silhouette", diss = mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance))
nbclust_clara_silhouette_mahal <- nbclust_clara_silhouette_mahal$data
max_cluster_clara_silhouette_mahal <- as.numeric(nbclust_clara_data_silhouette_mahal$clusters[which.max(nbclust_clara_data_silhouette_mahal$y)])
plot(max_cluster_clara_silhouette_mahal)
plot(nbclust_clara_silhouette_mahal)

nbclust_fanny_silhouette_mahal <- fviz_nbclust(d_tsne_2,k.max = 20, FUN = cluster::fanny, method = "silhouette", diss = mahalanobis(x = d_tsne_1_original,center = d_tsne_1_original_center,cov = d_tsne_1_original_covariance))
nbclust_fanny_data_silhouette_mahal <- nbclust_fanny_silhouette_mahal$data
max_cluster_fanny_silhouette_mahal <- as.numeric(nbclust_fanny_data_silhouette_mahal$clusters[which.max(nbclust_fanny_data_silhouette_mahal$y)])
plot(max_cluster_fanny_silhouette_mahal)
plot(nbclust_fanny_silhouette_mahal)

#tsne_results <- Rtsne(heatmap_vectors_matrix_after_second_reduction, perplexity=30, check_duplicates = FALSE)
#plot(tsne_results$Y, col = "blue", pch = 19, cex = 1.5)
#plot(tsne_results$Y, col = "black", bg= heatmap_clusters_after_second_reduction$louvain, pch = 21, cex = 1)

# plot_cluster=function(data, var_cluster, palette)  
# {
#   ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
#     geom_point(size=1.2) +
#     guides(colour=guide_legend(override.aes=list(size=10))) +
#     xlab("") + ylab("") +
#     ggtitle("") +
#     theme_light(base_size=20) +
#     theme(axis.text.x=element_blank(),
#           axis.text.y=element_blank(),
#           legend.direction = "horizontal", 
#           legend.position = "bottom",
#           legend.box = "horizontal") + 
#     scale_colour_brewer(palette = palette) 
# }
# 
# plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Set1")
# plot_h
########### https://www.r-bloggers.com/2017/03/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/

# plot_cluster=function(data, var_cluster,palette)  
# {
#   ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
#     geom_point(size=1.2) +
#     guides(colour=guide_legend(override.aes=list(size=10))) +
#     xlab("") + ylab("") +
#     ggtitle("") +
#     theme_light(base_size=10) +
#     theme(axis.text.x=element_blank(),
#           axis.text.y=element_blank(),
#           legend.direction = "horizontal", 
#           legend.position = "bottom",
#           legend.box = "horizontal")+ 
#     scale_colour_brewer(palette = palette) 
# 
# }
# 
# plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Spectral")
# plot_h





# plot_cluster=function(data, var_cluster,palette)  
# {
#   ggplot(data, aes_string(x="V1", y="V2", color=var_cluster)) +
#     geom_point(size=1.2) +
#     # geom_mark_ellipse(data = d_tsne_1_original %>% 
#     #                     filter(d_tsne_1_original$cl_hierarchical == 2),
#     #                   expand = unit(0.5, "mm")) +
#     geom_mark_ellipse(data = d_tsne_1_original %>% 
#                         filter(d_tsne_1_original$cl_hierarchical == 9),
#                       expand = unit(0.99, "mm")) +
#     guides(colour=guide_legend(override.aes=list(size=10))) +
#     xlab("") + ylab("") +
#     ggtitle("") +
#     theme_light(base_size=10) +
#     theme(axis.text.x=element_blank(),
#           axis.text.y=element_blank(),
#           legend.direction = "horizontal", 
#           legend.position = "bottom",
#           legend.box = "horizontal")+ 
#     scale_colour_brewer(palette = palette) 
#   
# }
# 
#   
# ### https://www.google.com/search?q=r+brewer+pal&oq=r+brewer+pal&aqs=chrome..69i57j69i64l3.3343j0j7&sourceid=chrome&ie=UTF-8#imgrc=o2H_xJhgKgHvxM
# plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Spectral")
# plot_h



#install.packages("plot3D")
library(plot3D)
#plot3d(d_tsne_1)

plot_cluster=function(data, var_cluster,palette)  
{
  ggplot(data, aes_string(x="V1", y="V2",z="V3",color=var_cluster)) +
    geom_point(size=2) +
    # geom_mark_ellipse(data = d_tsne_1_original %>% 
    #                     filter(d_tsne_1_original$cl_hierarchical == 2),
    #                   expand = unit(0.5, "mm")) +
    geom_mark_ellipse(data = d_tsne_1_original %>% 
                        filter(d_tsne_1_original$cl_hierarchical == 1),
                      #fill = "red",
                      expand = unit(1, "mm")) +
    guides(colour=guide_legend(override.aes=list(size=10))) +
    xlab("") + ylab("") +
    ggtitle("") +
    theme_light(base_size=10) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal")+ 
    scale_colour_brewer(palette = palette) 
  
}

plot_h=plot_cluster(d_tsne_1_original, "cl_hierarchical", "Set3")
plot_h
abline(v = 0)
####################################################################################
##################          TESTING           ######################################
####################################################################################


#plot(colSums(mydata))                                     #Zobrazenie sčítaných stĺpcov dát
#plot(mydata[,1])                                          #Zobrazenie prvého stĺpcu dát


y <- matrix(c(1,3, 1,2, 2,1, 3,1),nrow = 4,byrow = TRUE)
z <- c(1, 1, 2, 2)
z
str(y)
y
#v obsahuje riadky ktore patria do zhluku 2
v <- y[z == 2,]
v
dim(v)
c <- colMeans(v)
length(c)
plot(c)
points(v[1,],col='red')
t(v)
c
u <- t(v) - c
u
#d je vzdialenosti pre zhluk 2
d <- sqrt(colSums(u^2))
d
plot(d)
#vykreslenie kde sa ciaru stretnu pre 99% bodov
CDF = ecdf(d)
plot(CDF)
percent <- .99
abline(h=percent)
abline(v=quantile(CDF,c(percent)))
#polomer zhlukov aby obsahoval .99% bodov
CDF <- ecdf(d)
quantile(CDF,c(.5,.75,.90,.99))

quantile_plot <- quantile(CDF,c(.5,.75,.90,.99))
quantile_plot
plot(quantile_plot)


data_num <- as.data.frame(apply(testing, 2, as.numeric)) 


#cdf_tst = ecdf(clusters_list[[10]])
cdf_test2 = testing[1:38,]
cdf_test3 = as.numeric(cdf_test2)
cdf_test = as.list(cdf_test2)
cdf_test_cluster = ecdf(cdf_test)
quantile_plot_test <- quantile(cdf_tst,c(.5,.75,.90,.99))
quantile_plot_test
plot(quantile_plot)

## 24_11_projekt ##

m <- nrow(heatmap_vectors_matrix_base)
n <- ncol(heatmap_vectors_matrix_base)

#y obsahuje maticu dat
y <- heatmap_vectors_matrix_base[1:m,1:n]

#z obsahuje vektor zhlukov
z <- heatmap_clusters_base[1:m]
clusters_length = length(heatmap_clusters_base)
clusters_list <- list()
radius_list <- list()
list_counter = 1
cdf_counter = 1
percent <- .99

radius_dataframe = data.frame()
clusters_dataframe = data.frame()

for (i in 1:number_of_clusters) {
  message("Processing image ", i, " of ", number_of_clusters)
  v <- y[z == i,]
  c <- colMeans(v)
  u <- t(v) - c
  d <- sqrt(colSums(u^2))
  clusters_list[[i]] <- d
  clusters_matrix = matrix(unlist(clusters_list), ncol=window_width*col(original_heatmap), nrow=nrow(v), byrow=TRUE)
  d_dataframe = as.data.frame(clusters_matrix)
  write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
  CDF <- ecdf(d)
  q <- quantile(CDF,c(percent))
  radius_dataframe <- rbind(radius_dataframe,q)
}

write.xlsx(radius_dataframe,file = "pilot_project_radius.xlsx", sheetName = "Clusters_radius",row.names = FALSE,append = TRUE)

testing <- 0
testing <- read.xlsx("pilot_project_distance_matrix.xlsx", sheetName="Cluster1")
dim(y[z==1,])
testing <- read.xlsx("pilot_project_radius.xlsx", sheetName= "Clusters_radius")

testing_dataframe_to_matrix = data.matrix(testing)

testing_CDF = ecdf(testing_dataframe_to_matrix)

testing_quantile_plot <- quantile(testing_CDF,c(percent))
plot(testing_quantile_plot)

quantile(testing_CDF,c(percent))

plot(testing_CDF)
abline(h=percent,lty='dotted', col='red')
abline(v=quantile(testing_CDF,c(percent)),lty='dotted', col='red')


v = y[z == 2,]
vv = y[z == 10,]
c = colMeans(vv)
u = t(vv) - c
d = sqrt(colSums(u^2))


plot(v)
plot(y[322,])
nrow(v)