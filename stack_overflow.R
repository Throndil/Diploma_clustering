library(shiny)
library(plotly)
library(heatmaply)
library(dplyr)

ui <- fluidPage(
  plotlyOutput("scatterplot"),
  plotlyOutput("heatmap_clicked"),
  verbatimTextOutput("click"),
  verbatimTextOutput("click_heatmap")
)

server <- function(input, output, session) {
  trunc_x <- trunc(graph_first_reduction$data$x*10^0)/10^0
  trunc_y <- trunc(graph_first_reduction$data$y*10^0)/10^0
  heatmap_clicked_data <- reactiveVal(NULL)
  
  scatterplot <- reactiveVal()
  heatmap_clicked <- reactiveVal()
  
  observeEvent(event_data("plotly_click", source = "scatterplot"), {
    scatterplot(event_data("plotly_click", source = "scatterplot")$x)
    heatmap_clicked(NULL)
  })
  
  observeEvent(event_data("plotly_click", source = "scatterplot"), {
    clicked_x <- event_data("plotly_click", source = "scatterplot")$x
    clicked_y <- event_data("plotly_click", source = "scatterplot")$y
    clicked_x <- trunc(clicked_x*10^0)/10^0
    clicked_y <- trunc(clicked_y*10^0)/10^0
    point <- which(trunc_x == clicked_x & trunc_y == clicked_y)
    min_index <- (point - 1) * set_step + 1
    max_index <- min_index + (set_window_width - 1)
    heatmap_data <- set_original_heatmap[min_index:max_index,]
    heatmap_clicked_data(list(x = clicked_x, data = heatmap_data))
  })
  
  output$scatterplot <- renderPlotly({
    scatterplot <- plot_ly(
      data = graph_first_reduction$data,
      x = ~graph_first_reduction$data$x,
      y = ~graph_first_reduction$data$y, 
      type = "scatter",
      mode = "markers",
      source = "scatterplot",
      color = ~graph_first_reduction$data$cluster,
      hoverinfo = 'text',
      text = ~paste(graph_first_reduction$data$name)) %>%
      add_text(textposition = "top right")
    
    # Register the 'plotly_click' event for the 'scatterplot' source
    event_register(scatterplot, 'plotly_click')
    
    scatterplot
  })
  
  output$heatmap_clicked <- renderPlotly({
    req(heatmap_clicked_data())
    heatmap_data <- heatmap_clicked_data()$data
    heatmap_plot <- plot_ly(
      z = as.matrix(heatmap_data),
      type = "heatmap",
      source = "heatmap_clicked"
    )
    
    # Register the 'plotly_click' event for the 'heatmap_clicked' source
    event_register(heatmap_plot, 'plotly_click')
    
    heatmap_plot
  })
  
  output$click_heatmap <- renderPrint({
    req(heatmap_clicked)
    d_heatmap = event_data("plotly_click", source = "heatmap_clicked")
    if (is.null(d_heatmap)) return(NULL)
    d_heatmap
    
  })
  
  
}

shinyApp(ui, server)
