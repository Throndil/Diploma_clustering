library(shiny)
library(plotly)
library(heatmaply)
library(dplyr)

ui <- fluidPage(
  selectInput(
    "y", "Choose a variable", 
    choices = graph_first_reduction$data$name
  ),
  plotlyOutput("scatterplot"),
  plotlyOutput("heatmap_clicked"),
  # verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  verbatimTextOutput("click_heatmap")
)

server <- function(input, output, session) {
  trunc_x <- trunc(graph_first_reduction$data$x*10^0)/10^0
  trunc_y <- trunc(graph_first_reduction$data$y*10^0)/10^0
  heatmap_clicked = 0
  
  scatterplot <- reactiveVal()
  heatmap_clicked <- reactiveVal()
  heatmap_table <- reactiveVal()
  
  
  observeEvent(event_data("plotly_click", source = "scatterplot"), {
    scatterplot(event_data("plotly_click", source = "scatterplot")$x)
    heatmap_clicked(NULL)
    heatmap_table(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "heatmap_clicked"), {
    heatmap_clicked(
      event_data("plotly_click", source = "heatmap_clicked")$x
    )
    heatmap_table(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "heatmap_table"), {
    heatmap_table(event_data("plotly_click", source = "heatmap_table")$x)
  })
  
  
  
  output$scatterplot <- renderPlotly({
    
    ## p <- plot_ly(graph_first_reduction$data, source = "scatter_plot")
    
    ## req(graph_first_reduction$data)
    scatterplot <- plot_ly(data = graph_first_reduction$data, x = ~graph_first_reduction$data$x, y = ~graph_first_reduction$data$y, 
                           type = "scatter", mode = "markers", source = "scatterplot", color = ~graph_first_reduction$data$cluster,hoverinfo = 'text',
                           text = ~paste(graph_first_reduction$data$name)) %>% add_text(textposition = "top right")
    ## text = ~paste('</br> Cluster: ', graph_first_reduction$data$cluster,
    ##                '</br> Name: ', graph_first_reduction$data$name))
    ##  clickData <- event_data("plotly_click", source = "scatterplot")
    ##  if (is.null(clickData)) return(NULL)
  })
  
  output$heatmap_clicked <- renderPlotly({
    req(scatterplot)
    d = event_data("plotly_click", source = "scatterplot")
    if (is.null(d)) return(NULL)
    d_x <- d$x
    d_y <- d$y
    d_x <- trunc(d_x*10^0)/10^0
    d_y <- trunc(d_y*10^0)/10^0
    point = which(trunc_x == d_x & trunc_y == d_y)
    heatmap_data = 0
    as.matrix(heatmap_data)
    min_index = (point - 1) * set_step + 1
    max_index = min_index + (set_window_width - 1)
    heatmap_data = set_original_heatmap[min_index:max_index,]
    #heatmap_clicked <- heatmaply(heatmap_data, source = "heatmap", plot_method = c("plotly"))
    # plot_ly(z = heatmap_data, type = "heatmap")
    #plot_ly(x = rownames(heatmap_data), y = colnames(heatmap_data),z = heatmap_data, type = "heatmap")
    heatmap_clicked <- plot_ly(
      z = as.matrix(set_original_heatmap[min_index:max_index,]), type = "heatmap", source = "heatmap_clicked")
    
  })
  
}

shinyApp(ui, server)

point = 150
min_index = (point - 1) * set_step + 1
max_index = min_index + (set_window_width - 1)
heatmap_data = set_original_heatmap[min_index:max_index,]
heatmap_clicked <- heatmaply(heatmap_data, source = "heatmap", plot_method = c("plotly"))
heatmap_clicked

colnames(heatmap_data) <- NULL
heatmap_data
heatmap_data <- unname(heatmap_data)
heatmap_data = as.data.frame(heatmap_data)
plot_test <- plot_ly(z = heatmap_data, type = "heatmap")
plot_test


m <- matrix(rnorm(250), nrow = 50, ncol = 50)
heatmap_data <- as.matrix(heatmap_data)
fig <- plot_ly(
  z = heatmap_data, type = "heatmap")

fig










trunc_x = trunc(graph_first_reduction$data$x*10^0)/10^0
trunc_y = trunc(graph_first_reduction$data$y*10^0)/10^0
test = graph_first_reduction$data$x

trunc_x=0
trunc_y=0

c = which(test_x == -595 & test_y == -110)
c 


aa = c(1,2,4,6,-8.5,10,12,14)
aaa = which(aa == -8.5 )
aaa



test = match(c(graph_first_reduction$data$x[-943.4686],graph_first_reduction$data$y[-213.9568]),graph_first_reduction$data)
test

p <- heatmaply(heatmap_data)
p
fig <- plot_ly(x = rownames(heatmap_data), y = colnames(heatmap_data),z = heatmap_data, type = "heatmap")
fig
heatmap_data
a = t(heatmap_clusters_after_first_reduction)
a = as.data.frame(heatmap_clusters_after_first_reduction)
str(a)