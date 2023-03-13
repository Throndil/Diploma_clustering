options(java.parameters = "- Xmx1024m")
library(janitor)
library(ggplot2) 
library(factoextra)
library(ape)
library(plyr)
library(dplyr)
library(openxlsx)
library(plotrix)
library(ggforce) 
library(xlsx)
library(Rtsne)
library(factoextra)
library(ggpubr)
library(ape)
library(RColorBrewer)
library(functional)
library(gsubfn)
library(readxl)
library(plotly)
library(htmlwidgets)
library(purrr)
library(V8)
library(rjson)
#install.packages("V8")
#install.packages("htmlwidgets")
#install.packages("plotly")
#install.packages("dplyr")
#library(tidyverse)
working_directory <- getwd()
setwd(getwd())

attach(mtcars)
par(mfrow=c(2,1))
dev.off()

names <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
           "V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25",
           "V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39","V40",
           "V41","V42","V43","V44","V45","V46","V47","V48","V49","V50")

sheet_names <- c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6","Cluster7","Cluster8","Cluster9","Cluster10")
distance_names <- c("euclidean","maximum","manhattan","canberra")
titles <- c("Hierarchical","K-means","K-medoids","Clara","Fanny")
subtitles <- c("Euclidean","Maximum","Manhattan","Canberra","Mahalanobis")
ylabel <- c("Total Within Sum of Square","Average silhouette width", "Gap statistic (K)")
methods <- c("wss","silhouette","gap_stat")

set_window_width = 50                                        #Šírka okna
set_step = 10                                                #Posun
set_number_of_clusters = 10                                  #Počet zhlukov
#set_original_heatmap <- read.csv(file.choose(), header=T, sep = "\t")              #Načítanie teplotnej mapy z CSV súboru z programu p. Krnáča, mydata = teplotna_mapa
set_original_heatmap <- read.csv(file.choose(), header=T)
column_names <- head(set_original_heatmap,0)                           #Vlož názvy V1 až V50 ako názvy stĺpcov


create_heatmap_vectors <- function(data,clusters,step,window){
  counter = 1                                              #Celkové počítadlo
  current_row_heatmap = 1                                          #Počítadlo riadkov
  number_rows_after_vectoring = (nrow(data) - window) / step + 1      #Celkový počet riadkov, výpočet je: (počet riadkov teplotnej mapy - zvolená šírka vektora) / posun
  heatmap_vectors_list_base <- list()                                   #Vytvorenie listu do ktorého uložím vektory
  for(i in 1:number_rows_after_vectoring){                                    #Prechádzam počtom riadkov
    if (window == 1) {                               #Ak používateľ zvolil šírku okna 1, znamená to že vektorová mapa bude rovnaká ako teplotná mapa
      heatmap_vectors_matrix_base <- data.class()
      break
    }
    if (window != 1) {
      heatmap_column = data[current_row_heatmap,]                   #Pomocný vector do ktorého si vložím prvotný riadok a na neho nabaľujem ostatné
      counter <- current_row_heatmap + 1                           #Zvýšim počítadlo
      for (j in 1:window) {                          #Cyklus sa toľkokrát ako bola zadaná šírka okna
        if (j != window) {                           #Telo cyklu, pokým sa cyklus nerovná šírke okna tak nabaľujem riadky na pomocný vektor
          heatmap_column = append(heatmap_column, data[counter,])
          counter <- counter + 1
        }
        if (j == window) {                           #Ak sme na poslednom kroku cyklu, už nepribaľujeme a pribalíme vektor do listu na pozíciu i
          heatmap_vectors_list_base[[i]] <- heatmap_column 
          current_row_heatmap <- current_row_heatmap + step                #Aktualizujem počítadlo riadkov
        }
      }
    }
    message("Processing image ", i, " of ", number_rows_after_vectoring)      #Pomocný výpis
    if (i == number_rows_after_vectoring) {
      if (window == 1) {                             #Ak je šírka okna 1, matica je vlastne samotné dáta
        heatmap_vectors_matrix_base <- data
        colnames(heatmap_vectors_matrix_base) <- rep(names, times = ncol(data))    #Nastavenie názvu stĺpcov pre vektorovú mapu 
        heatmap_vectors_matrix_base <<- heatmap_vectors_matrix_base                #Assign ako globalnu premennu
        heatmap_vectors_list_base <<- heatmap_vectors_list_base
        number_rows_after_vectoring <<- number_rows_after_vectoring
      }else{
        heatmap_vectors_matrix_base <- matrix(unlist(heatmap_vectors_list_base), ncol=window*ncol(data), nrow=number_rows_after_vectoring, byrow=TRUE)
        colnames(heatmap_vectors_matrix_base) <- rep(names, times = ncol(data))
        heatmap_vectors_matrix_base <<- heatmap_vectors_matrix_base
        heatmap_vectors_list_base <<- heatmap_vectors_list_base
        number_rows_after_vectoring <<- number_rows_after_vectoring
      }
    }
  }
}

create_heatmap_vectors(set_original_heatmap,set_number_of_clusters,set_step,set_window_width)

create_first_clustering <- function(data,method,clusters){
  message("Calculating distance matrix.")
  heatmap_distance_matrix = dist(data)                         #Vypočítanie vzdialeností matice
  message("Finished distance matrix, calculating hclust.")
  heatmap_hclust_output <- hclust(heatmap_distance_matrix, method = method) #Funkcia hclust do ktorej ako parameter posielame vzdialenosť matice (už vektorová mapa) a metódu "complete"
  message("Finished hclust, calculating cutree.")
  #plot(heatmap_hclust_output)                                       #Dendrogram hclust
  heatmap_clusters_base <- cutree(heatmap_hclust_output, k=clusters) #Zvolenie počtu zhlukov a orezanie
  message("Finished cutree.")
  #graph <- fviz_cluster(list(data=heatmap_vectors_matrix_base, cluster = heatmap_clusters_base), stand = FALSE, labelsize = 0) #Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou, ak chceme definovať osi použiť axes = c(x,y)
  #graph 
  heatmap_distance_matrix <<- heatmap_distance_matrix
  heatmap_hclust_output <<- heatmap_hclust_output
  heatmap_clusters_base <<- heatmap_clusters_base
}

create_first_clustering(heatmap_vectors_matrix_base,"complete",set_number_of_clusters)

graph_first_reduction = fviz_cluster(list(data=heatmap_vectors_matrix_base, cluster = heatmap_clusters_base), stand = FALSE,
                                     ggtheme = theme_minimal(),
                                     alpha = 0.2)

ggplotly(graph_first_reduction)

first_reduction <- function(heatmap_list,heatmap_clusters,heatmap_vectors,clusters){
  ### ODSTRANENIE ZHLUKOV VELKOSTI 1
  counter_anomaly = 1
  min_index = 0
  max_index = 0
  removed_columns_first_reduction = 0
  rows_in_cluster = 0
  current_cluster = 1
  row_index = 0
  heatmap_vectors_list_after_first_reduction = heatmap_list
  heatmap_clusters_after_first_reduction = heatmap_clusters
  heatmap_vectors_matrix_after_first_reduction = heatmap_vectors
  anomalous_columns_first_reduction <- list()
  for (i in 1:clusters) {
    for (j in 1:length(heatmap_clusters_after_first_reduction)) {
      if (heatmap_clusters_after_first_reduction[j] == current_cluster) {          #Musi byt taketo pocitadlo zhluku inac by vratil anomali na indexe 1 pre kazdy zhluk
        rows_in_cluster <- rows_in_cluster + 1
        row_index = j
      }
      if (j == length(heatmap_clusters_after_first_reduction) && rows_in_cluster == 1) {
        message("Anomaly in cluster: ", i, ", at index: ", row_index)
        heatmap_vectors_list_after_first_reduction[[row_index]] <- NULL
        heatmap_clusters_after_first_reduction <- heatmap_clusters_after_first_reduction[-c(row_index)]
        min_index <- (j - 1) * set_step + 1 # x = (a - 1) * p + 1, a - aktualny index, p - posun
        max_index <- min_index + (set_window_width - 1) # y = x + (w - 1), x - min_index, w - sirka okna
        anomalous_columns_first_reduction[[counter_anomaly]] <- set_original_heatmap[min_index:max_index,]
        counter_anomaly <- counter_anomaly + 1
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
  anomalous_columns_first_reduction <<- anomalous_columns_first_reduction
  heatmap_vectors_list_after_first_reduction <<- heatmap_vectors_list_after_first_reduction
  heatmap_clusters_after_first_reduction <<- heatmap_clusters_after_first_reduction
  heatmap_vectors_matrix_after_first_reduction <<- heatmap_vectors_matrix_after_first_reduction
  removed_columns_first_reduction <<- removed_columns_first_reduction
}

first_reduction(heatmap_vectors_list_base,heatmap_clusters_base,heatmap_vectors_matrix_base,set_number_of_clusters)

create_second_clustering <- function(data,method,clusters){
  ## DRUHE ZHLUKOVANIE
  message("Calculating distance matrix.")
  heatmap_distance_matrix_after_first_reduction = dist(data)
  message("Finished distance matrix, calculating hclust.")
  heatmap_hclust_output_after_first_reduction <- hclust(heatmap_distance_matrix_after_first_reduction, method = method)
  message("Finished hclust, calculating cutree.")
  heatmap_clusters_after_first_reduction <- cutree(heatmap_hclust_output_after_first_reduction, k=clusters)
  message("Finished cutree.")
  #graph <- fviz_cluster(list(data=heatmap_vectors_matrix_after_first_reduction, cluster = heatmap_clusters_after_first_reduction), stand = FALSE, labelsize = 0)
  #graph
  heatmap_distance_matrix_after_first_reduction <<- heatmap_distance_matrix_after_first_reduction
  heatmap_hclust_output_after_first_reduction <<- heatmap_hclust_output_after_first_reduction
  heatmap_clusters_after_first_reduction <<- heatmap_clusters_after_first_reduction
}

create_second_clustering(heatmap_vectors_matrix_after_first_reduction,"complete",set_number_of_clusters)

graph_second_clustering = fviz_cluster(list(data=heatmap_vectors_matrix_after_first_reduction, cluster = heatmap_clusters_after_first_reduction), stand = FALSE,
                                     ggtheme = theme_minimal(),
                                     alpha = 0.2)

ggplotly(graph_first_reduction)


first_reduction_save_file <- function(clusters_filename,vectors_filename){
  ###### UKLADANIE SUBOROV AKO TREBA BEZ RIADKOV STLPCOV, Z MATICE NA DATAFRAME
  heatmap_clusters_dataframe = as.data.frame(heatmap_clusters_after_first_reduction)
  names(heatmap_clusters_dataframe) <- NULL
  if (file.exists(clusters_filename)) {
    file.remove(clusters_filename)
    write.csv(heatmap_clusters_dataframe, clusters_filename, row.names = FALSE, col.names = FALSE)
  }else{
    write.csv(heatmap_clusters_dataframe, clusters_filename, row.names = FALSE, col.names = FALSE)
  }
  
  heatmap_vectors_dataframe = as.data.frame(heatmap_vectors_matrix_after_first_reduction)
  names(heatmap_vectors_dataframe) <- NULL
  if (file.exists(vectors_filename)) {
    file.remove(vectors_filename)
    write.csv(heatmap_vectors_dataframe,vectors_filename , row.names = FALSE,  col.names = FALSE)
  }else{
    write.csv(heatmap_vectors_dataframe,vectors_filename , row.names = FALSE,  col.names = FALSE)
  }
}

first_reduction_save_file("pilot_project_10shift_50width_cluster_first_reduction.csv","pilot_project_10shift_50width_heatmap_vectors_first_reduction.csv")


calculate_radius <- function(data,clusters_data,clusters,quantile_percentage,distance_filename,radius_filename){
  m = nrow(data)  
  n = ncol(data)
  y = data[1:m,1:n] #y obsahuje maticu dat
  z <- clusters_data[1:m]   #z obsahuje vektor zhlukov
  clusters_length = length(clusters_data)
  clusters_list <- list()
  radius_list <- list()
  list_counter = 1
  cdf_counter = 1
  length_d = 0
  radius_dataframe = data.frame()
  clusters_dataframe = data.frame()
  
  for (i in 1:clusters) {
    message("Processing image ", i, " of ", clusters)
    v = y[z == i,]
    if (is.null(nrow(v))) {
      d = sqrt(v^2)
      clusters_list[[i]] = d
      length_d = length(d)
      clusters_matrix = matrix(unlist(clusters_list[[i]]), ncol=length_d, nrow=1, byrow=TRUE)
      d_dataframe = as.data.frame(clusters_matrix)
      write.xlsx2(d_dataframe, file=distance_filename, sheetName=sheet_names[i], append=TRUE, row.names=FALSE,col.names = FALSE)
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
      write.xlsx2(d_dataframe, file=distance_filename, sheetName=sheet_names[i], append=TRUE, row.names=FALSE,col.names = FALSE)
      CDF = ecdf(d)
      q = quantile(CDF,c(quantile_percentage))
      radius_dataframe = rbind(radius_dataframe,q)
    }
    if (i == clusters) {
      if (file.exists(radius_filename)) {
        file.remove(radius_filename)
        write.xlsx(radius_dataframe,file = radius_filename, sheetName = "Clusters_radius",row.names = FALSE,col.names = FALSE,append = TRUE)
        radius_dataframe <<- radius_dataframe
      }else{
        write.xlsx(radius_dataframe,file = radius_filename, sheetName = "Clusters_radius",row.names = FALSE,col.names = FALSE,append = TRUE)
        radius_dataframe <<- radius_dataframe
      }
    }
  }
  clusters_list <<- clusters_list
}

calculate_radius(heatmap_vectors_matrix_after_first_reduction,heatmap_clusters_after_first_reduction,set_number_of_clusters,.99,"pilot_project_distance_matrix.xlsx","pilot_project_radius.xlsx")


find_points_outside <- function(data,clusters_data,clusters){
  counter_anomaly = 1
  index_to_remove = 0
  i = 1
  index_to_to_remove = 0
  how_many_removed = 0
  heatmap_clusters_after_second_reduction = heatmap_clusters_after_first_reduction
  heatmap_vectors_matrix_after_second_reduction = heatmap_vectors_matrix_after_first_reduction
  removed_columns_second_reduction = data.frame()
  anomalous_columns_second_reduction <- list()
  for (j in 1:set_number_of_clusters) {
    message("Novy zhluk: ", j)
    outer_bound = length(clusters_list[[j]])
    while (i <= outer_bound) 
    {
      index_to_remove = which(heatmap_clusters_after_second_reduction %in% c(j))
      if (clusters_list[[j]][i] > radius_dataframe[j,]) {
        message("Zhluk: ", j, " ,bod: ",clusters_list[[j]][i], " ,je mimo polomeru zhluku: ", j, " ,ktori je: ", radius_dataframe[j,])
        message("Index v zhluku: ", i)
        index_to_to_remove = index_to_remove[i]
        message("Index v datach: ", index_to_remove[i])
        heatmap_clusters_after_second_reduction <- heatmap_clusters_after_second_reduction[-c(index_to_to_remove)]
        removed_columns_second_reduction <- rbind(removed_columns_second_reduction,heatmap_vectors_matrix_after_second_reduction[index_to_to_remove,])
        heatmap_vectors_matrix_after_second_reduction <- heatmap_vectors_matrix_after_second_reduction[-index_to_to_remove,]
        how_many_removed <- how_many_removed + 1
        
        temporary_vector = unlist(clusters_list[[j]])
        temporary_vector = temporary_vector[-i]
        clusters_list[[j]] <- temporary_vector 
        outer_bound = outer_bound - 1
        
        min_index <- (j - 1) * set_step + 1 # x = (a - 1) * p + 1, a - aktualny index, p - posun
        max_index <- min_index + (set_window_width - 1) # y = x + (w - 1), x - min_index, w - sirka okna
        anomalous_columns_second_reduction[[counter_anomaly]] <- set_original_heatmap[min_index:max_index,]
        counter_anomaly <- counter_anomaly + 1
      }
      if (i == outer_bound) {
        i = 1
        break
      }
      i = i + 1
    }
  }
    if (j == set_number_of_clusters) {
      message("Pocet odstranenych bodov: ", how_many_removed)
      heatmap_vectors_matrix_after_second_reduction <<- heatmap_vectors_matrix_after_second_reduction
      heatmap_clusters_after_second_reduction <<- heatmap_clusters_after_second_reduction
      removed_columns_second_reduction <<- removed_columns_second_reduction
      anomalous_columns_second_reduction <<- anomalous_columns_second_reduction
    }
}


find_points_outside(heatmap_vectors_matrix_after_first_reduction,heatmap_clusters_after_first_reduction,set_number_of_clusters)
table(heatmap_clusters_after_second_reduction)

graph_first_reduction = fviz_cluster(list(data=heatmap_vectors_matrix_after_first_reduction, cluster = heatmap_clusters_after_first_reduction), stand = FALSE,
                                      ggtheme = theme_minimal(),
                                      alpha = 0.2)



graph_second_reduction <- fviz_cluster(list(data=heatmap_vectors_matrix_after_second_reduction, cluster = heatmap_clusters_after_second_reduction), stand = FALSE)
#dataframe_first_reduction <- as.data.frame(graph_first_reduction)
##graph_first_reduction %>%
##  ggplotly() %>%
##  highlight(
##    on = "plotly_click", 
##    selectize = TRUE, 
##    dynamic = TRUE, 
##    persistent = TRUE
##  )
graph_first_reduction
graph_first_reduction %>%
  ggplotly() %>%
  onRender("
    function(el) { 
      el.on('plotly_hover', function(d) { 
        console.log('Hover: ', d); 
      });
      el.on('plotly_click', function(d) { 
        console.log('Click: ', d);
      });
      el.on('plotly_selected', function(d) { 
        console.log('Select: ', d); 
      });
    }
  ")

ggplotly(graph_first_reduction) %>%
  onRender("
    function(el) { 
      el.on('plotly_click', function(d) { 
        console.log('Click: ', d.points[0]);
      });
    }
  ")


ctx <- v8()
ctx$assign("graph_first_reduction",graph_first_reduction$data)

ctx$get("graph_first_reduction")
#ctx$console()


result <- capture.output()

graph_first_reduction %>%
  onRender(readLines("js/annotate.js"))
  
matrix_as_dataframe = as.data.frame(heatmap_vectors_matrix_after_first_reduction)


testing_custom_data = graph_first_reduction$data
testing_custom_data = map2(graph_first_reduction$data$name, graph_first_reduction$data$cluster, ~.x, ~.y)  
testing_custom_data_list = as.list(testing_custom_data)
aa <- t(heatmap_clusters_after_first_reduction)




testing_graph <- graph_first_reduction$data %>%
  group_by(graph_first_reduction$data$cluster) %>%
  highlight_key(~graph_first_reduction$data$name) %>%
  plot_ly(x = ~graph_first_reduction$data$x, y = ~graph_first_reduction$data$y, hoverinfo = "name", type = "scatter", mode = "markers+text") %>%
  add_markers(
    text = graph_first_reduction$data$name,
    customdata = graph_first_reduction$data$name,
  )%>%
  add_lines(customdata = ~map2(matrix_as_dataframe, aa, ~list(.x, .y))) %>%
  highlight("plotly_hover")

onRender(testing_graph,readLines("js/annotate.js"))





  

#plot_ly(graph_first_reduction)
dev.off()
attach(mtcars)
par(mfrow=c(2,1))
plot(graph_first_reduction)
plot(graph_second_reduction)

second_reduction_save_file <- function(clusters_filename,vectors_filename){
  ###### UKLADANIE SUBOROV AKO TREBA BEZ RIADKOV STLPCOV, Z MATICE NA DATAFRAME
  heatmap_clusters_dataframe = as.data.frame(heatmap_clusters_after_second_reduction)
  names(heatmap_clusters_dataframe) <- NULL
  if (file.exists(clusters_filename)) {
    file.remove(clusters_filename)
    write.csv(heatmap_clusters_dataframe, clusters_filename, row.names = FALSE, col.names = FALSE)
  }else{
    write.csv(heatmap_clusters_dataframe, clusters_filename, row.names = FALSE, col.names = FALSE)
  }
  
  heatmap_vectors_dataframe = as.data.frame(heatmap_vectors_matrix_after_second_reduction)
  names(heatmap_vectors_dataframe) <- NULL
  if (file.exists(vectors_filename)) {
    file.remove(vectors_filename)
    write.csv(heatmap_vectors_dataframe,vectors_filename , row.names = FALSE,  col.names = FALSE)
  }else{
    write.csv(heatmap_vectors_dataframe,vectors_filename , row.names = FALSE,  col.names = FALSE)
  }
}

second_reduction_save_file("pilot_project_10shift_50width_cluster_second_reduction.csv","pilot_project_10shift_50width_heatmap_vectors_second_reduction.csv")


create_tsne_model <- function(){
  tsne_model_1 = Rtsne(heatmap_vectors_matrix_after_second_reduction, check_duplicates=FALSE, pca=FALSE, perplexity=220, theta=0.0, dims=3, normalize = FALSE, max_iter = 2000)
  d_tsne_1 = as.data.frame(tsne_model_1$Y)
  d_tsne_1_original=d_tsne_1
  fit_cluster_hierarchical=hclust(dist(d_tsne_1))
  d_tsne_1_original$cl_hierarchical = factor(cutree(fit_cluster_hierarchical, k=10))
  
  tsne_model_1 <<- tsne_model_1
  d_tsne_1_original <<- d_tsne_1_original
  fit_cluster_hierarchical <<- fit_cluster_hierarchical
  d_tsne_1 <<- d_tsne_1
}

create_tsne_model()

plot(table(d_tsne_1_original$cl_hierarchical), title("TSNE"),ylab = "Number of observations", xlab = "Clusters")
plot(table(heatmap_clusters_after_second_reduction), title("Clustering"),ylab = "Number of observations", xlab = "Clusters")

dev.off()
attach(mtcars)
par(mfrow=c(2,1))
plot(table(d_tsne_1_original$cl_hierarchical))
plot(table(heatmap_clusters_after_second_reduction))
plot(fit_cluster_hierarchical)
plot(heatmap_hclust_output_after_first_reduction)

### PRE DOKUMENTACIU 

heatmap_vectors_matrix_after_second_reduction[1:3,1:4]
plot(heatmap_vectors_matrix_after_second_reduction[1,], type = "l")
#zobrazenie prvy riadok 1:50 stlpcov, po 1 riadok 51:100 stlpcov
matplot(cbind(heatmap_vectors_matrix_after_second_reduction[1,1:50], +
                heatmap_vectors_matrix_after_second_reduction[1,51:100]),
                  type = "l")

names(d_tsne_1_original) <- NULL
colnames(d_tsne_1_original) <- NULL

run <- function() {
  eval(parse(text = rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())$text))
}

show_clusters=function(){
  message("Zhluky po prvom zhlukovaní.")
  table(heatmap_clusters_base)
  message("Zhluky po druhom zhlukovaní.")
  table(heatmap_clusters_after_first_reduction)
  message("Zhluky po treťom zhlukovaní.")
  table(heatmap_clusters_after_second_reduction)
}
show_clusters()

graph <- fviz_cluster(list(data=heatmap_vectors_matrix_base, cluster = heatmap_clusters_base), stand = FALSE, labelsize = 0) #Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou, ak chceme definovať osi použiť axes = c(x,y)
graph 
graph2 <- fviz_cluster(list(data=heatmap_vectors_matrix_after_first_reduction, cluster = heatmap_clusters_after_first_reduction), stand = FALSE, labelsize = 0) #Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou, ak chceme definovať osi použiť axes = c(x,y)
graph2 
graph3 <- fviz_cluster(list(data=heatmap_vectors_matrix_after_second_reduction, cluster = heatmap_clusters_after_second_reduction), stand = FALSE, labelsize = 0) #Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou, ak chceme definovať osi použiť axes = c(x,y)
graph3 
## https://stackoverflow.com/questions/40821591/how-to-print-the-optimal-number-of-clusters-using-fviz-nbclust
## http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determiningthe-optimal-number-of-clusters-3-must-know-methods/

nbclust_calculation=function(data,kmax,funct,method,distance)  
{
  if (distance == 1) {
    d_tsne_matrix = as.matrix(d_tsne_1_original[1:3])
    d_tsne_1_center = colMeans(d_tsne_matrix)
    d_tsne_1_covariance = cov(d_tsne_matrix)
    mahalanobis_dist <- mahalanobis(x = d_tsne_matrix,center = d_tsne_1_center,cov = d_tsne_1_covariance)
    fviz_nbclust(d_tsne_1,k.max = kmax, FUN = funct, method = method, print.summary = TRUE, diss = mahalanobis_dist)
  }else{
  dist = dist(data,distance)
  fviz_nbclust(data,k.max = kmax, FUN = funct, method = method, print.summary = TRUE, diss = dist)
  }
}

best_number_of_clusters=function(data,ylab,title,subtitle)  
{
  delta_result = data.frame(col1 = numeric())
  for (i in 1:length(data$clusters)) {
    data_i_0 = data$y[i]
    data_i_1 = data$y[i+1]
    data_i_2 = data$y[i+2]
    delta_i_1 = data_i_0 - data_i_1
    delta_i_2 = data_i_0 - data_i_2
    new_row <- c(delta_i_1/delta_i_2)
    delta_result[nrow(delta_result) + 1, ] <- new_row
  }
  ##https://stackoverflow.com/questions/28560188/find-the-index-of-minimum-non-negative-value-in-r
  best_cluster = which(delta_result==min(delta_result[delta_result>0], na.rm = TRUE))
  yvalue = data$y[best_cluster]
  plot(data$y, type = "b",  xlab = "Number of clusters k", ylab = ylab)
  title(main = title,cex.main = 1.5)
  mysubtitle = subtitle
  mtext(side = 3, line = 0.25, at = 14, adj = 0, mysubtitle)
  abline(v = best_cluster,h = yvalue, col = "blue", lwd = 1,lty = "1342")
}
dev.off()
attach(mtcars)
par(mfrow=c(2,3))

nbclust_wss_hcut <- function(){
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss","euclidean")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Euclidean")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss","maximum")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Maximum")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss","manhattan")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Manhattan")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss","canberra")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Canberra")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss","minkowski")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Minkowski")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss", 1)
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Mahalanobis")

}
nbclust_wss = nbclust_calculation_test(d_tsne_1, 30, hcut, "wss","euclidean")
nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Euclidean")
nbclust_wss_hcut()

nbclust_wss_kmeans <- function(){
  dev.off()
  attach(mtcars)
  par(mfrow=c(2,3))
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, kmeans, "wss","euclidean")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-means","Euclidean")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, kmeans, "wss","maximum")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-means","Maximum")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, kmeans, "wss","manhattan")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-means","Manhattan")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, kmeans, "wss","canberra")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-means","Canberra")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, kmeans, "wss","minkowski")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-means","Minkowski")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, kmeans, "wss", 1)
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-means","Mahalanobis")
}

nbclust_wss_kmeans()


nbclust_wss_pam <- function(){
  dev.off()
  attach(mtcars)
  par(mfrow=c(2,3))
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::pam, "wss","euclidean")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-medoids","Euclidean")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::pam, "wss","maximum")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-medoids","Maximum")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::pam, "wss","manhattan")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-medoids","Manhattan")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::pam, "wss","canberra")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-medoids","Canberra")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::pam, "wss","minkowski")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-medoids","Minkowski")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::pam, "wss", 1)
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","K-medoids","Mahalanobis")
}

nbclust_wss_pam()


nbclust_wss_clara <- function(){
  dev.off()
  attach(mtcars)
  par(mfrow=c(2,3))
  #https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Clustering/CLARA
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::clara, "wss","euclidean")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Clara","Euclidean")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::clara, "wss","maximum")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Clara","Maximum")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::clara, "wss","manhattan")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Clara","Manhattan")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::clara, "wss","canberra")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Clara","Canberra")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::clara, "wss","minkowski")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Clara","Minkowski")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::clara, "wss", 1)
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Clara","Mahalanobis")
}

nbclust_wss_clara()

nbclust_wss_fanny <- function(){
  dev.off()
  attach(mtcars)
  par(mfrow=c(2,3))
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::fanny, "wss","euclidean")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Fanny","Euclidean")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::fanny, "wss","maximum")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Fanny","Maximum")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::fanny, "wss","manhattan")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Fanny","Manhattan")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::fanny, "wss","canberra")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Fanny","Canberra")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::fanny, "wss","minkowski")
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Fanny","Minkowski")
  nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, cluster::fanny, "wss", 1)
  nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Fanny","Mahalanobis")
  
}

nbclust_wss_fanny()



nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, hcut, "silhouette","euclidean")
nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Euclidean")
nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, hcut, "silhouette","maximum")
nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Maximum")
nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, hcut, "silhouette","manhattan")
nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Manhattan")
nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, hcut, "silhouette","canberra")
nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Canberra")
nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, hcut, "silhouette","minkowski")
nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Minkowski")
#nbclust_wss = nbclust_calculation_test(d_tsne_1, 20, hcut, "silhouette", 1)
#nbclust_best_wss = best_number_of_clusters_test(nbclust_wss$data,"Total Within Sum of Square","Hierarchical","Mahalanobis")




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
########### https://www.r-bloggers.com/2017/03/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/
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
##################          SHINY          #########################################
####################################################################################


library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(RColorBrewer)
load('ezData.rdata')

ui <- fluidPage(
  plotlyOutput("scatterplot"),
  plotlyOutput("heatmap", height = 1000),
  dataTableOutput("datatable")
  # tableOutput("datatable")
)

server <- function(input, output, session) {
  
  trunc_x <- trunc(graph_first_reduction$data$x*10^0)/10^0
  trunc_y <- trunc(graph_first_reduction$data$y*10^0)/10^0
  heatmap_clicked = 0
  mapColor <- brewer.pal(n = 9,name = 'OrRd')
  
  kontrola_dat <- function(data) {  #funkcia y krnacovho programu, neviem ci treba
    data[is.na(data)] = "";
    return(data);
  }
  
  odstran_id_stlpce <- function(data) {   
    
    data <- data[,-c(1,2,3)]
    
    return(data);
  }
  
  # for maintaining the state of drill-down variables
  scatterplot <- reactiveVal()
  heatmap <- reactiveVal()
  
  # when clicking on a category, 
  observeEvent(event_data("plotly_click", source = "scatterplot"), {
    scatterplot(event_data("plotly_click", source = "scatterplot")$x)
    heatmap(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "heatmap"), {
    heatmap(
      event_data("plotly_click", source = "heatmap")$x
    )
  })
  
  
  output$scatterplot <- renderPlotly({
    scatterplot <- plot_ly(data = graph_first_reduction$data, x = ~graph_first_reduction$data$x, y = ~graph_first_reduction$data$y, 
                           type = "scatter", mode = "markers", source = "scatterplot", color = ~graph_first_reduction$data$cluster,hoverinfo = 'text',
                           text = ~paste(graph_first_reduction$data$name, graph_first_reduction$data$cluster)) %>% add_text(textposition = "top right") %>%
      layout(xaxis = list(title = "X"), yaxis = list(title = "Y"))
  })
  
  output$heatmap <- renderPlotly({
    req(scatterplot)
    d = event_data("plotly_click", source = "scatterplot")
    if (is.null(d)) return(NULL)
    d_x <- d$x
    d_y <- d$y
    d_x <- trunc(d_x*10^0)/10^0
    d_y <- trunc(d_y*10^0)/10^0
    point <<- which(trunc_x == d_x & trunc_y == d_y)
    heatmap_data = 0
    as.matrix(heatmap_data)
    min_index <<- (point - 1) * set_step + 1
    max_index <<- min_index + (set_window_width - 1)
    heatmap_data <<- t(set_original_heatmap[min_index:max_index,])
    #heatmap_clicked <- heatmaply(heatmap_data, source = "heatmap", plot_method = c("plotly"))
    # plot_ly(z = heatmap_data, type = "heatmap")
    #plot_ly(x = rownames(heatmap_data), y = colnames(heatmap_data),z = heatmap_data, type = "heatmap")
    
    # heatmap_clicked <<- plot_ly(
    #  z = t(as.matrix(set_original_heatmap[min_index:max_index,])), type = "heatmap", source = "heatmap",colorscale = mapColor) %>%
    #    layout(xaxis = list(dtick = 1,tick0 = 1,tickmode = "linear",title = "Časové okno",range = c(1, 50)), yaxis = list(dtick = 1,tickmode = "linear", title = "Kategória",range = c(1, 50)))
    
    heatmap_clicked <<- plot_ly(
      z = t(as.matrix(set_original_heatmap[min_index:max_index,])), 
      type = "heatmap",
      source = "heatmap",
      colorscale = mapColor,
      x = seq(1, ncol(set_original_heatmap[min_index:max_index,])), 
      y = seq(1, nrow(set_original_heatmap[min_index:max_index,])),) %>%
      layout(
        xaxis = list(
          tickvals = 1:50,
          dtick = 1,
          title = "Časové okno",
          range = c(1, 50) 
        ),
        yaxis = list(
          tickvals = 1:50,
          dtick = 1, 
          title = "Kategória", 
          range = c(1, 50)
        )
      )
    
    
    heatmap_clicked <<- heatmap_clicked
  })
  
  
  output$datatable <- renderDataTable({
    d_heatmap = event_data("plotly_click", source = "heatmap")
    if (is.null(heatmap())) return(NULL)
    d_heatmap$point <- point
    
    kateg <- d_heatmap$y
    sirka <- 2
    posun <- 0.5
    
    #t0 = ezData$frame.time_relative[1] + posun;  #zaciatok, cize 0 + posun
    t0 = (min_index + d_heatmap$x) * posun - posun
    #t1 = t0 + sirka;       # zaciatok + sirka
    t1 = t0 + sirka / set_window_width    
    #message(kateg)
    # message(d_heatmap$y)
    # message(d_heatmap$x)
    # message(t0)
    # message(t1)
    
    
    # ak by som chcel celu mapu tak nedavat / set_window_width
    # a nedavat v subset(kategorie == kateg) ale len subset(ezData)
    # ak by som chcel riadok, subset nechat tak a v t1 nedelit / set_window_width
    # ak by som chcel stlpec nie subset(ezData,kategorie == kateg), ale subset(ezData) a t1 nemenit
    
    data1 <- subset(ezData,kategorie == kateg)
    
    ind <- which(data1$frame.time_relative >= t0 & data1$frame.time_relative <= t1);  #indexy paketov vo vybranom casovom okne
    #message(ind)
    
    vysledne_data <- data1[ind,];  # vybranie paketov, ktore sa nachadzaju len v casovom okne
    # data <- kontrola_dat(data)   
    vysledne_data <- odstran_id_stlpce(vysledne_data)  # odstranenie 3 stlpcov:  frame.number, frame.time, frame.time_relative, aby som sa zbavil 
    vysledne_data <- plyr::count(vysledne_data)
    # unikatnych hodnot a mohol scitavat rovnake pakety
    
    #vysledne_data <- plyr::count(data)  # spocitanie rovnakych paketov dokopy, vo premennej freq je ich pocet
    
    
    #vysledne_data_skratene <- subset(vysledne_data,kategorie == kateg)  # vybranie konkretnej kategorie, takze toto su uz vysledne hodnoty
    #message(vysledne_data_skratene)
    
    
    #d_heatmap
    #heatmap_data
    
  })
  
  # output$datatable <- renderTable({
  #   d = event_data("plotly_click", source = "heatmap")
  #  if (is.null(heatmap())) return(NULL)
  #    d$x <- d$x + 1
  #   heatmap_data[,d$x]4
  
  # })
}

shinyApp(ui, server)


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




## RLANG FIX
remove.packages("rlang")
install.packages("rlang")
