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

window_width = 50
step = 10
#Načítanie teplotnej mapy z CSV súboru z programu p. Krnáča
mydata <- read.csv2(file.choose(), header=T, sep = " ") #mydata = teplotna_mapa
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
  vectors_matrix <- matrix(unlist(vectors_list), ncol=window_width*50, nrow=num_rows, byrow=TRUE)
}
#Nastavenie názvu stĺpcov pre vektorovú mapu
colnames(vectors_matrix) <- rep(names, times = window_width)

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
mydata.clusters <- cutree(hc.output_mydata, k=10)
length(mydata.clusters)
#Zápis zhlukov do CSV, teda do akého zhluku patril ten príslušný vektor
write.csv(mydata.clusters, "clusters_Moloch-180418-07-anonymized.csv", row.names = TRUE)
write.csv(vectors_matrix, "heatMap_vectors_Moloch-180418-07-anonymized.csv", row.names = TRUE)
#Vykreslenie zhlukov z vektorovej mapy, zhlukov a s vypnutou štandardizáciou
graph <- fviz_cluster(list(data=mydata_data_std, cluster = mydata.clusters), stand = FALSE)
graph <- fviz_cluster(list(data=mydata_data_std, cluster = mydata.clusters), stand = FALSE, axes = c(1, 3))
graph


typeof(mydata.clusters)
class(mydata.clusters)
str(mydata.clusters)
typeof(mydata.dist)
class(mydata.dist)
str(mydata.dist)




cluster = 1

list_counter = 1
clusters_list <- list()

for (i in 1:nrow(vectors_matrix)) {
  if (mydata.clusters[i] == cluster) {
    clusters_list[[list_counter]] <- vectors_matrix[i,]
    list_counter <- list_counter + 1
  }
}
#print(count_clusters)
clusters_matrix <- matrix(unlist(clusters_list), ncol=window_width*50, nrow=count_clusters, byrow=TRUE)

#str(vectors_matrix)
m <- nrow(vectors_matrix)
n <- ncol(vectors_matrix)
#y obsahuje maticu dat
y <- vectors_matrix[1:m,1:n]

#y <- matrix(c(1,3, 1,2, 2,1, 3,1),nrow = 4,byrow = TRUE)
#str(y)
#y
#z obsahuje vektor zhlukov
z <- mydata.clusters[1:m]
#z <- c(1, 1, 2, 2)
#z
z == 1
#v obsahuje riadky ktore patria do zhluku 1
v <- y[z == 1,]
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

t(v)
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
