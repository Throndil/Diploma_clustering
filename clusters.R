options(java.parameters = "- Xmx1024m")
library(janitor)
library(factoextra)
library(ggplot2) 
library(ape)
library(plyr)
library(dplyr)
library(openxlsx)
#install.packages("xlsx")
library(xlsx)
#warnings()

library(factoextra)
library(ggplot2)
library(ape)
#theme_set(theme_minimal())
names <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
           "V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25",
           "V26","V27","V28","V29","V30","V31","V32","V33","V34","V35","V36","V37","V38","V39","V40",
           "V41","V42","V43","V44","V45","V46","V47","V48","V49","V50")

sheet_names <- c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6","Cluster7","Cluster8","Cluster9","Cluster10")

window_width = 50
step = 10
number_of_clusters = 10
#Načítanie teplotnej mapy z CSV súboru z programu p. Krnáča
mydata <- read.csv(file.choose(), header=T) #mydata = teplotna_mapa
#Vlož názvy V1 až V50 ako názvy stĺpcov
head(mydata,0)
column_names <- head(mydata,0)

#Celkové počítadlo
counter = 1
#Počítadlo riadkov
row_counter = 1
#Celkový počet riadkov, výpočet je: (počet riadkov teplotnej mapy - zvolená šírka vektora) / posun
num_rows = (nrow(mydata) - window_width) / step + 1 
#Vytvorenie listu do ktorého uložím vektory
vectors_list <- list()
#?? je to tu potreba?
#vectors <- matrix(ncol=window_width*50,nrow=num_rows)

#Prechádzam počtom riadkov
for(i in 1:num_rows){
  #Ak používateľ zvolil šírku okna 1, znamená to že vektorová mapa bude rovnaká ako teplotná mapa
  if (window_width == 1) {
    vectors_matrix <- mydata
    break
  }
  #Ak používateľ zvolil šírku okna inú ako 1 pokračuje cyklus
  if (window_width != 1) {
    #Pomocný vector do ktorého si vložím prvotný riadok a na neho nabaľujem ostatné
    vector_test = mydata[row_counter,]
    #Zvýšim počítadlo
    counter <- row_counter + 1
    #Cyklus sa toľkokrát ako bola zadaná šírka okna
    for (j in 1:window_width) {
      #Telo cyklu, pokým sa cyklus nerovná šírke okna tak nabaľujem riadky na pomocný vektor
      if (j != window_width) {
        vector_test = append(vector_test, mydata[counter,])
        counter <- counter + 1
      }
      #Ak sme na poslednom kroku cyklu, už nepribaľujeme a pribalíme vektor do listu na pozíciu i
      if (j == window_width) {
        vectors_list[[i]] <- vector_test 
        #Aktualizujem počítadlo riadkov
        row_counter <- row_counter + step
        
      }
    }
  }
  #Pomocný výpis
  message("Processing image ", i, " of ", num_rows)
}

#POZOR TREBA POZRIET CYKLUS A TOTO ABY SA TO PORIESILO, NIEJE DOBRE AJ KED JE TO SPROSTOST TREBA OPRAVIT

#Ak je šírka okna 1, matica je vlastne 
if (window_width == 1) {
  vectors_matrix <- mydata
}else{
  vectors_matrix <- matrix(unlist(vectors_list), ncol=window_width*ncol(mydata), nrow=num_rows, byrow=TRUE)
}
#Nastavenie názvu stĺpcov pre vektorovú mapu
colnames(vectors_matrix) <- rep(names, times = ncol(mydata))


mydata_data_std = vectors_matrix


plot(colSums(mydata))
plot(mydata[,1])
#PCA
#Vypočítanie vzdialeností matice
mydata.dist = dist(mydata_data_std)
#Funkcia hclust do ktorej ako parameter posielame vzdialenosť matice (už vektorová mapa) a metódu "complete"
hc.output_mydata <- hclust(mydata.dist, method = "complete")
#Dendrogram hclust
plot(hc.output_mydata)
#Zvolenie počtu zhlukov a orezanie
mydata.clusters <- cutree(hc.output_mydata, k=number_of_clusters)
table(mydata.clusters)
length(mydata.clusters)

#Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou
graph <- fviz_cluster(list(data=mydata_data_std, cluster = mydata.clusters), stand = FALSE, labelsize = 0)
#graph <- fviz_cluster(list(data=mydata_data_std, cluster = mydata.clusters), stand = FALSE, axes = c(1, 3))
graph


#najdi a odstran anomalie (zhluky velkosti 1)
table(mydata.clusters)
rows_in_cluster = 0
clusters_counter = 1
row_index = 0
for (i in 1:number_of_clusters) {
  for (j in 1:length(mydata.clusters)) {
    if (mydata.clusters[j] == clusters_counter) {
      rows_in_cluster <- rows_in_cluster + 1
      row_index = j
    }
    if (j == length(mydata.clusters) && rows_in_cluster == 1) {
      message("Anomalia v zhluku: ", i, ", na indexe: ", row_index)
    }
  }
  rows_in_cluster = 0
  row_index = 0
  clusters_counter <- clusters_counter + 1
}
### ON JE NUTENY UROBIT 10 ZHLUKOV TAKZE POSLEDNE 3 SU ROVNAKE



#Zápis zhlukov do CSV, teda do akého zhluku patril ten príslušný vektor
#write.csv(mydata.clusters, "homeTraffic_2shift_5width_clusters.csv", row.names = TRUE)
write.csv(mydata.clusters, "pilot_project_10shift_50width_clusters.csv", row.names = TRUE)
#write.csv(vectors_matrix, "homeTraffic_2shift_5width_vectors.csv", row.names = TRUE)
write.csv(vectors_matrix, "pilot_project_10shift_50width_heatmap_vectors.csv", row.names = TRUE)




typeof(mydata.clusters)
class(mydata.clusters)
str(mydata.clusters)
typeof(mydata.dist)
class(mydata.dist)
str(mydata.dist)




#str(vectors_matrix)
m <- nrow(vectors_matrix)
n <- ncol(vectors_matrix)
#y obsahuje maticu dat

y <- vectors_matrix[1:m,1:n]

#z obsahuje vektor zhlukov
z <- mydata.clusters[1:m]
clusters_length = length(mydata.clusters)
clusters_list <- list()
radius_list <- list()
list_counter = 1
cdf_counter = 1
percent <- .99

radius_dataframe = data.frame()
clusters_dataframe = data.frame()

#ITERATION 1
#for (i in 1:number_of_clusters) {
#message("Processing image ", i, " of ", number_of_clusters)
#v <- y[z == i,]
#c <- colMeans(v)
#u <- t(v) - c
#d <- sqrt(colSums(u^2))
#clusters_list[[list_counter]] <- d
#list_counter <- list_counter + 1
#CDF = ecdf(d)
#q <- quantile(CDF,c(percent))
#radius_list[[cdf_counter]] <- q
#cdf_counter <- cdf_counter + 1
#}
#ITERATION 2

for (i in 1:number_of_clusters) {
  message("Processing image ", i, " of ", number_of_clusters)
  v <- y[z == i,]
  if (is.null(nrow(v))) {
    #c <- colMeans(v)
    #u <- t(v) - c
    d <- sqrt(v^2)
    # clusters_list[[list_counter]] <- d
    # list_counter <- list_counter + 1
    #clusters_dataframe <- rbind(clusters_dataframe,d)
    clusters_list[[i]] <- d
    clusters_matrix = matrix(unlist(clusters_list), ncol=window_width*col(mydata), nrow=1, byrow=TRUE)
    d_dataframe = as.data.frame(clusters_matrix)
    #write.xlsx2(d_dataframe, file="test_xlsx.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
    write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
    #write.xlsx2(d_dataframe, file="home_traffic_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
    CDF <- ecdf(d)
    q <- quantile(CDF,c(percent))
    radius_dataframe <- rbind(radius_dataframe,q)
    # radius_list[[cdf_counter]] <- q
    # cdf_counter <- cdf_counter + 1
  }else{
    c <- colMeans(v)
    u <- t(v) - c
    d <- sqrt(colSums(u^2))
    # clusters_list[[list_counter]] <- d
    # list_counter <- list_counter + 1
    #clusters_dataframe <- rbind(clusters_dataframe,d)
    clusters_list[[i]] <- d
    clusters_matrix = matrix(unlist(clusters_list), ncol=window_width*col(mydata), nrow=nrow(v), byrow=TRUE)
    d_dataframe = as.data.frame(clusters_matrix)
    #write.xlsx2(d_dataframe, file="test_xlsx.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
    write.xlsx2(d_dataframe, file="pilot_project_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
    #write.xlsx2(d_dataframe, file="home_traffic_distance_matrix.xlsx", sheetName=sheet_names[i], append=TRUE, row.names=FALSE)
    CDF <- ecdf(d)
    q <- quantile(CDF,c(percent))
    radius_dataframe <- rbind(radius_dataframe,q)
    # radius_list[[cdf_counter]] <- q
    # cdf_counter <- cdf_counter + 1
  }
}
v <- y[z == 8,]
plot(v)
plot(y[322,])
nrow(v)
write.xlsx(radius_dataframe,file = "pilot_project_radius.xlsx", sheetName = "Clusters_radius",row.names = FALSE,append = TRUE)
#write.xlsx(radius_dataframe,file = "home_traffic_radius.xlsx", sheetName = "Clusters_radius",row.names = FALSE,append = TRUE)



#clusters_output <- capture.output(clusters_list)
#radius_output <- capture.output(radius_list)
#cat(clusters_output, file = "test_clusters_list.csv", sep = "\t", append = TRUE)
#write.csv(clusters_output, "test_clusters_list.csv")
#cat(clusters_output, file = "test_radius_list.csv", sep = "\t", append = TRUE)
#capture.output(clusters_list, file = "test_clusters_list.csv")
#capture.output(radius_list, file = "test_radius_list.csv")
#write.csv(clusters_matrix, "test_clusters_list.csv", row.names = TRUE)

#df <- ldply(clusters_output, data.frame)
#df2 <- ldply(radius_output, data.frame)
#write.csv(df, "test_clusters_list.csv", row.names = FALSE)
#write.csv(df2, "test_radius_list.csv", row.names = FALSE)
testing <- 0
testing <- read.table("test_clusters_list.csv")
testing <- read.csv("test_clusters_list.csv")

#write.xlsx(v, file="test_xlsx.xlsx", sheetName="sheet1", append=TRUE, row.names=FALSE)
#write.xlsx(vv, file="test_xlsx.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

######OPAKUJE HODNOTY ABY VYPLNIL 672

clusters_dataframe = data.frame()

v <- y[z == 1,]
c <- colMeans(v)
u <- t(v) - c
d <- sqrt(colSums(u^2))
#d_dataframe = as.data.frame(d)
#clusters_dataframe <- rbind(clusters_dataframe,d, fill=TRUE)
#merge.data.frame(clusters_dataframe,d_dataframe)
clusters_list[[1]] <- d
clusters_matrix = matrix(unlist(clusters_list), ncol=window_width*col(mydata), nrow=nrow(v), byrow=TRUE)
d_dataframe = as.data.frame(clusters_list)
write.xlsx(d_dataframe, file="test_xlsx.xlsx", sheetName="sheet1", append=TRUE, rowNames=FALSE)


test_matrix_dataframe = 0
test_matrix_dataframe <- read.xlsx2(file="test_xlsx.xlsx",sheetName="Cluster1")
test_matrix_dataframe <- read.xlsx2(file="test_radius_list.xlsx",sheetName="Clusters_radius")
##dorobit poistku aby ak je dimenzia 1x2500 aby to nezhodilo na colMeans() pretoze ta funkcia potrebuje rozmer aspon 2x2

vv <- y[z == 2,]
c <- colMeans(vv)
u <- t(vv) - c
d <- sqrt(colSums(u^2))
#d_dataframe = as.data.frame(d)
clusters_dataframe <- rbind(clusters_dataframe,d, fill=TRUE)

t(d_dataframe)

z == 1
#v obsahuje riadky ktore patria do zhluku 1
X <- y[z == 1,]
str(X)
#v <- y[z == 2,]
#v
#dim(v)


#radius_matrix <- matrix(nrow = 1,ncol = ncol(clusters_matrix))
#for (i in 1:ncol(clusters_matrix)) {
#  x = sum(clusters_matrix[,i])/nrow(clusters_matrix)
#  radius_matrix[,i] <- x
#}
#c obsahuje centrum pre maticu V ktora sa sklada z riadkov zhluku 1
#centra pre stlpce x1 az x1850
c <- colMeans(v)
#length(c)
#plot(c)
#plot(c)
#points(v[1,],col='red')

#radius_matrix_transposed <- t.default(radius_matrix)
#ncol(radius_matrix)
#ncol(clusters_matrix)
#clusters_matrix_test = clusters_matrix
#clusters_matrix_test2 <- rbind(v,c)
#clusters_distance = dist(clusters_matrix_test2,method = "euclidean")

#t(v)
#c

u <- t(v) - c
#u
#d je vzdialenosti pre zhluk 1
d <- sqrt(colSums(u^2))
#d
plot(d)

#clusters_distance
#plot(clusters_distance)

#polomer zhlukov aby obsahoval .99% bodov
CDF = ecdf(d)
CDF(1000)
plot(CDF)
percent <- .99
abline(h=percent)
abline(v=quantile(CDF,c(percent)))

CDF <- ecdf(d)
quantile(CDF,c(.5,.75,.90,.99))

quantile_plot <- quantile(CDF,c(.5,.75,.90,.99))
quantile_plot
plot(quantile_plot)
#abline(a=0,b=1,lty="dotted")
plot(CDF,quantile_plot)
#curve
#ggplot2::aes(CDF,quantile_plot)


#plot(ecdf(clusters_distance),quantile(CDF,c(.5,.75,.90)))
#points(quantile_plot,col='red', pch=13)


##TESTING 
y <- matrix(c(1,3, 1,2, 2,1, 3,1),nrow = 4,byrow = TRUE)
z <- c(1, 1, 2, 2)
z
str(y)
y
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
#d je vzdialenosti pre zhluk 1
d <- sqrt(colSums(u^2))
d
plot(d)
CDF = ecdf(d)
plot(CDF)
percent <- .99
abline(h=percent)
abline(v=quantile(CDF,c(percent)))
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

m <- nrow(vectors_matrix)
n <- ncol(vectors_matrix)

#y obsahuje maticu dat
y <- vectors_matrix[1:m,1:n]

#z obsahuje vektor zhlukov
z <- mydata.clusters[1:m]
clusters_length = length(mydata.clusters)
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
  clusters_matrix = matrix(unlist(clusters_list), ncol=window_width*col(mydata), nrow=nrow(v), byrow=TRUE)
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




