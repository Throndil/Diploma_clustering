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
heatmap_clusters_dataframe = as.data.frame(heatmap_vectors_matrix_after_first_reduction)
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

# heatmap_clusters_dataframe = as.data.frame(heatmap_vectors_matrix_after_first_reduction)
# names(heatmap_clusters_dataframe) <- NULL
# write.csv(heatmap_clusters_dataframe, "pilot_project_10shift_50width_clustersv2.csv", row.names = FALSE, col.names = FALSE)
# 
# heatmap_vectors_dataframe = as.data.frame(heatmap_vectors_matrix_after_first_reduction)
# names(heatmap_vectors_dataframe) <- NULL
# write.csv(heatmap_vectors_dataframe, "pilot_project_10shift_50width_heatmap_vectorsv2.csv", row.names = FALSE,  col.names = FALSE)
# 
# 
# m = nrow(heatmap_vectors_matrix_after_first_reduction)  
# n = ncol(heatmap_vectors_matrix_after_first_reduction)
# y = heatmap_vectors_matrix_after_first_reduction[1:m,1:n] #y obsahuje maticu dat
# z <- heatmap_clusters_after_first_reduction[1:m]   #z obsahuje vektor zhlukov
# clusters_length = length(heatmap_clusters_after_first_reduction)
# clusters_list <- list()
# radius_list <- list()
# list_counter = 1
# cdf_counter = 1
# percent = .99
# length_d = 0
# radius_dataframe = data.frame()
# clusters_dataframe = data.frame()
# 
# for (i in 1:number_of_clusters) {
#   message("Processing image ", i, " of ", number_of_clusters)
#   v = y[z == i,]
#   if (is.null(nrow(v))) {
#     d = sqrt(v^2)
#     clusters_list[[i]] = d
#     length_d = length(d)
#     clusters_matrix = matrix(unlist(clusters_list[[i]]), ncol=length_d, nrow=1, byrow=TRUE)
#     d_dataframe = as.data.frame(clusters_matrix)
#     write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE,col.names = FALSE)
#     CDF = ecdf(d)
#     q = quantile(CDF,c(percent))
#     radius_dataframe = rbind(radius_dataframe,q)
#   }else{
#     c = colMeans(v)
#     u = t(v) - c
#     d = sqrt(colSums(u^2))
#     clusters_list[[i]] = d
#     length_d = length(d)
#     clusters_matrix = matrix(unlist(clusters_list[[i]]), ncol=length_d, nrow=1, byrow=TRUE)
#     d_dataframe = as.data.frame(clusters_matrix)
#     write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE,col.names = FALSE)
#     CDF = ecdf(d)
#     q = quantile(CDF,c(percent))
#     radius_dataframe = rbind(radius_dataframe,q)
#   }
#   if (i == number_of_clusters) {
#     write.xlsx(radius_dataframe,file = "pilot_project_radius.xlsx", sheetName = "Clusters_radius",row.names = FALSE,col.names = FALSE,append = TRUE)
#   }
# }


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