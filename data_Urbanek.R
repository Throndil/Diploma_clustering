load('ezData.rdata')

kontrola_dat <- function(data) {  #funkcia y krnacovho programu, neviem ci treba
  data[is.na(data)] = "";
  return(data);
}

odstran_id_stlpce <- function(data) {   
  a <- vector();
  
  for (i in seq(1:ncol(data))) {
    if (length(unique(data[,i])) >= nrow(data)/100*60) {
      a <-append(a, i);
    } 
  }
  
  data <- data[,-a];
  sprintf("Vraciam upravené dáta.");
  
  return(data);
}

kateg <- 27
sirka <- 2;
posun <- 0.5

t0 = ezData$frame.time_relative[1] + posun;  #zaciatok, cize 0 + posun
t1 = t0 + sirka;       # zaciatok + sirka

ind <- which(ezData$frame.time_relative >= t0 & ezData$frame.time_relative < t1);  #indexy paketov vo vybranom casovom okne
#print(ind)
#tail(ind)

data <- ezData[ind,];  # vybranie paketov, ktore sa nachadzaju len v casovom okne
data <- kontrola_dat(data)   
data <- odstran_id_stlpce(data)  # odstranenie 3 stlpcov:  frame.number, frame.time, frame.time_relative, aby som sa zbavil 
                                                            # unikatnych hodnot a mohol scitavat rovnake pakety

vysledne_data <- plyr::count(data);  # spocitanie rovnakych paketov dokopy, vo premennej freq je ich pocet


vysledne_data_skratene <- subset(vysledne_data,kategorie == kateg)  # vybranie konkretnej kategorie, takze toto su uz vysledne hodnoty
