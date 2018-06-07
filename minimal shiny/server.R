library(ggplot2)

server <- function(input, output) {
    shiny_data = read.csv('shiny.csv')
    
    data_chart <- reactive({
      id_select = input$id
      device_select = input$device
      
      filter_rows = (shiny_data$id == id_select) & (shiny_data$device == device_select)
      shiny_data[filter_rows, ]
    })
    
    output$plot <- renderPlot({
      p = ggplot2::ggplot(data = data_chart(), ggplot2::aes(x = time, y = X_original)) + 
          ggplot2::geom_line() + 
          ggplot2::theme_bw() +
          ggplot2::geom_point(data = subset(data_chart(), abs(S_transform) > 0), color = "red",
                              ggplot2::aes(size = S_transform^2))
      
      p
      
    })
}