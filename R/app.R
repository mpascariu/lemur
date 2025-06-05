library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    ".       header   header  ",
    "sidebar bluePlot bluePlot",
    ".       table    plotly  ",
    "area5   table    plotly  "
  ),
  row_sizes = c(
    "100px",
    "0.87fr",
    "0.42fr",
    "1.71fr"
  ),
  col_sizes = c(
    "235px",
    "0.59fr",
    "1.41fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      sliderInput(
        inputId = "bins",
        label = "Number of Bins",
        min = 12,
        max = 100,
        value = 30,
        width = "100%"
      ),
      numericInput(
        inputId = "numRows",
        label = "Number of table rows",
        value = 10,
        min = 1,
        step = 1,
        width = "100%"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Geysers!",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    card_header("Table"),
    card_body()
  ),
  grid_card_plot(area = "bluePlot"),
  grid_card(
    area = "plotly",
    card_header("Interactive Plot"),
    card_body(
      plotlyOutput(
        outputId = "distPlot",
        width = "100%",
        height = "100%"
      )
    )
  ),
  grid_card(
    area = "area5",
    card_body(
      markdown(
        mds = c(
          "hello _world_"
        )
      )
    )
  )
)


server <- function(input, output) {
   
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  
}

shinyApp(ui, server)
  

