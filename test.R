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


numeric = as.numeric(rownames(heatmap_data))
mapColor <- brewer.pal(n = 9,name = 'OrRd')
heatmap_clicked <<- plot_ly(
  z = t(as.matrix(set_original_heatmap[1:49,])), type = "heatmap", source = "heatmap",colorscale = mapColor) %>%
  layout(xaxis = list(dtick = 1,tickmode = "linear",title = "Časové okno",range = c(1, 50)), yaxis = list(dtick = 1,tickmode = "linear", title = "Kategória",range = c(1, 50)))

