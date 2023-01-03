library(shiny)
library(DT)
library(openxlsx)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(ECharts2Shiny)
df <-
  read.xlsx("https://github.com/pkglowczewski/zmed/raw/master/osoby.xlsx",
            sheet = 1)
ui <- fluidPage(navbarPage(
  "ZMED - PKG & MT",
  tabPanel(
    "Tabela",
    h2("Read CSV from url"),
    DTOutput("csv")
  ),
  tabPanel(
    "Mapa",
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "New points")
  ),
  tabPanel(
    "Wykresy",
    plotOutput(
      "plot1",
      click = "plot_click",
      dblclick = "plot_dblclick",
      hover = "plot_hover",
      brush = "plot_brush"
    ),
    verbatimTextOutput("info"),
    selectInput("Ind","Dane 1",choices = names(df),selected = names(df)[6]),
    selectInput('Dep','Dane 2',choices = names(df),selected = names(df)[7]),
    plotOutput("BoxPlot"),
    plotOutput('Hist'),
    selectInput('woj','Województwo',choices = df$WOJEWÓDZTWO,selected = df$WOJEWÓDZTWO[1]),
    loadEChartsLibrary(),
    tags$div(id="test", style="width:50%;height:400px;"),
    deliverChart(div_id = "test")
  )
))
server <- function(input, output) {
  
  result <- df %>% select('wszyscy', 'mezczyzni_zameldowane')
  output$plot1 <- renderPlot({
    plot(df$wszyscy, df$mezczyzni_zameldowane)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if (is.null(e))
        return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if (is.null(e))
        return("NULL\n")
      paste0(
        "xmin=",
        round(e$xmin, 1),
        " xmax=",
        round(e$xmax, 1),
        " ymin=",
        round(e$ymin, 1),
        " ymax=",
        round(e$ymax, 1)
      )
    }
    
    paste0(
      "click: ",
      xy_str(input$plot_click),
      "dblclick: ",
      xy_str(input$plot_dblclick),
      "hover: ",
      xy_str(input$plot_hover),
      "brush: ",
      xy_range_str(input$plot_brush)
    )
  })
  
  output$csv <- renderDataTable({
    df
  })
  
  points <- eventReactive(input$recalc, {
    cbind(as.double(df$school_longitude),
          as.double(df$school_latitude))
  }, ignoreNULL = FALSE)
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data = points())
  })
  observeEvent(input$mymap_marker_click, {
    p <- input$mymap_marker_click
    print(p[3])
    print(p[4])
    
  })
  
  output$plot <- renderPlot({
    ggplot(df, aes(humidity_avg, pressure_avg)) + geom_point()
  }, res = 96)
  
  output$data <- renderTable({
    brushedPoints(df, input$plot_brush)
  })
  
  data1 <- reactive({
    input$Ind
  })
  data2 <- reactive({
    input$Dep
  })
  
  data3 <- reactive({
   input$woj
  })
  output$BoxPlot <- renderPlot({
    boxplot(get(data2()) ~ get(data1()) , data=df)
  })
  
  output$Hist <- renderPlot({
    req(data1())
    hist(df[[data1()]])
  }) 
  
  dataTest <-   df[df$WOJEWÓDZTWO == "DOLNOŚLĄSKIE",]
  print(sum(dataTest$kobiety_zameldowane, na.rm=TRUE))
  dat <- c(rep("Type-A", as.integer(sum(dataTest$kobiety_zameldowane, na.rm=TRUE))),
           rep("Type-B", as.integer(sum(dataTest$mezczyzni_zameldowane, na.rm=TRUE))),
           rep("Type-C", as.integer(sum(dataTest$powyzej_18, na.rm=TRUE))))
  renderPieChart(div_id = "test",
                 data = dat)
  }


shinyApp(ui = ui, server = server)