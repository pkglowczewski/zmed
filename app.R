library(shiny)
library(DT)
library(openxlsx)
library(leaflet)
library(RColorBrewer)
library(plotly)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Map", fluid = TRUE,
             plotOutput("plot1",
                        click = "plot_click",
                        dblclick = "plot_dblclick",
                        hover = "plot_hover",
                        brush = "plot_brush"
             ),
             verbatimTextOutput("info"),
             h2("Read CSV from url"),
             DTOutput("csv")
    ),
    tabPanel("plot", fluid = TRUE,
             leafletOutput("mymap"),
             p(),
             actionButton("recalc", "New points")
             )
    
  )
)

server <- function(input, output){
  
  df <- read.xlsx("https://github.com/pkglowczewski/zmed/raw/master/ANTYSMOG.xlsx",sheet=1)
  
  output$plot1 <- renderPlot({
    plot(df$pm25_avg, df$pm25_avg)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
  output$csv <- renderDataTable({
    df
  })
  
  result <- df %>% select('school_latitude','school_longitude')
  points <- eventReactive(input$recalc, {
    cbind(as.double(df$school_longitude), as.double(df$school_latitude))
  }, ignoreNULL = FALSE)
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(data = points())
  })
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click
    print(p[3])
    print(p[4])
  })
}


shinyApp(ui = ui, server = server)