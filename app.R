library(shiny)
library(openxlsx)
library(RColorBrewer)
library(plotly)
library(DT)
df <-
  read.xlsx("https://github.com/pkglowczewski/zmed/raw/master/osoby_pl.xlsx",
            sheet = 1)
ui <-navbarPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  "ZMED - PKG & MT",
  tabPanel("Tabela",
           h2("Dane tabelaryczne"),
           DTOutput("csv")),
  navbarMenu("Wykresy",
            tabPanel("WYKRES LEJKOWY",
                     plotlyOutput(outputId = "funnelChart", height = '800px', width = 'auto')),
            tabPanel("WYKRES KOŁOWY",
                     selectizeInput(
                       inputId = "woje", 
                       label = "Wybierz województwo", 
                       choices = unique(df$WOJEWÓDZTWO), 
                       selected = "POMORSKIE",
                       multiple = TRUE
                     ),
                     plotlyOutput("pieChart", height = '800px', width = 'auto')),
            tabPanel("WYKRES SŁUPKOWY PIONOWY",
                     selectizeInput(
                       inputId = "voivodeship", 
                       label = "Wybierz województwo", 
                       choices = unique(df$WOJEWÓDZTWO), 
                       selected = "POMORSKIE",
                       multiple = TRUE
                     ),
                     selectInput(
                       "dataBarChart",
                       "Dane wejściowe",
                       choices = names(df),
                       selected = names(df)[1]
                     ),
                     plotlyOutput(outputId = "barChart", height = '800px', width = 'auto')),
            tabPanel("WYKRES SŁUPKOWY POZIOMY",
                     plotlyOutput("barChart2", height = '800px', width = 'auto')),
            tabPanel("WYKRES BĄBELKOWY",
                     plotlyOutput("bubble", height = '800px', width = 'auto')),
             )
  
)
server <- function(input, output) {
  groupDataPlot1<- df%>%
    group_by(WOJEWÓDZTWO)%>%
    summarise(WSZYSCY = sum(WSZYSCY),KOBIETY_ZAMELDOWANE= sum(KOBIETY_ZAMELDOWANE), MEZCZYZNI_ZAMELDOWANE= sum(MEZCZYZNI_ZAMELDOWANE))
  sortedGroupDataPlot1<-groupDataPlot1[order(groupDataPlot1$WSZYSCY,decreasing=TRUE),]
  
  output$funnelChart <- renderPlotly({
    plot_ly(
        type = "funnel",
        orientation = "h",
        name = 'KOBIETY ZAMELDOWANE',
        y = sortedGroupDataPlot1$WOJEWÓDZTWO,
        x = sortedGroupDataPlot1$KOBIETY_ZAMELDOWANE,
        textposition = "inside",
        textinfo = "value+percent total")%>%
      add_trace(
        type = "funnel",
        name = 'MĘŻCZYŹNI ZAMELDOWANI',
        orientation = "h",
        y = sortedGroupDataPlot1$WOJEWÓDZTWO,
        x = sortedGroupDataPlot1$MEZCZYZNI_ZAMELDOWANE,
        textposition = "inside",
        textinfo = "value+percent total") %>%
      layout(yaxis = list(categoryarray = c(sortedGroupDataPlot1$WOJEWÓDZTWO)))
    })
  
  output$csv <- renderDataTable({
    df
  })
  
  output$data <- renderTable({
    brushedPoints(df, input$plot_brush)
  })
  
  output$pieChart<-renderPlotly({
    dataForPie <- df%>%
      filter(WOJEWÓDZTWO %in% input$woje) %>%
      group_by(WOJEWÓDZTWO)%>%
      summarise(WSZYSCY = sum(WSZYSCY))
    dataFramePie <- data.frame("WOJEWÓDZTWO"=dataForPie$WOJEWÓDZTWO, dataForPie)
    data <- dataFramePie[,c('WOJEWÓDZTWO', 'WSZYSCY')]
    plot_ly(data, labels = ~WOJEWÓDZTWO, values = ~WSZYSCY, type = 'pie')  %>% 
      layout(title = 'Liczba ludzi w danym województwie',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  
  thematic::thematic_shiny()
  
  output$plot <- renderPlot({
    ggplot(df, aes(df$WOJEWÓDZTWO, df$POWYZEJ_18)) +
      geom_point() +
      geom_smooth()+
      xlab('Województwo')+
      ylab('Wszyscy')
  }, res = 96,)
  
  fromInput <- reactive({
    input$dataBarChart  })
  output$barChart <- renderPlotly({
    plot_ly(df, x = ~WOJEWÓDZTWO, y = ~get(fromInput())) %>%
      filter(WOJEWÓDZTWO %in% input$voivodeship) %>%
      group_by(WOJEWÓDZTWO) %>%
      add_bars()%>%
      layout(xaxis = list(autotypenumbers = 'strict', title = 'WOJEWÓDZTWO'),
             yaxis = list(title = ''),
             plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
  })
  output$barChart2 <- renderPlotly({
    plot_ly(df, x = ~POWYZEJ_18, y = ~WOJEWÓDZTWO, type = 'bar', orientation = 'h',
    marker = list(color = 'rgba(246, 78, 139, 1.0)',
                  line = list(color = 'rgba(246, 78, 139, 1.0)',
                              width = 2)),name = 'Powyżej 18')%>%
    add_trace(df,x = ~PONIZEJ_18,y = ~WOJEWÓDZTWO, name = 'Poniżej 18',
                  marker = list(color = 'rgba(77, 71, 130, 1.0)',
                                line = list(color = 'rgba(58, 71, 80, 1.0)',
                                            width = 2)))%>%
      add_trace(df,x = ~WSZYSCY,y = ~WOJEWÓDZTWO, name = 'Wszyscy',
                marker = list(color = 'rgba(158, 171, 80, 1.0)',
                              line = list(color = 'rgba(158, 171, 80, 1.0)',
                                          width = 2)))%>%
      layout(xaxis = list(autotypenumbers = 'strict', title = 'Porównanie poniżej i powyżej 18'),
             yaxis = list(title = ''),
             plot_bgcolor='#e5ecf6',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'))
    
    })
  dataForBubble<- df%>%
    group_by(WOJEWÓDZTWO)%>%
    summarise(PONIZEJ_18 = (sum(PONIZEJ_18)/10000),KOBIETY_PONIZEJ_18 = sum(KOBIETY_PONIZEJ_18),MEZCZYZNI_PONIZEJ_18 = sum(MEZCZYZNI_PONIZEJ_18))
  output$bubble <- renderPlotly({
    plot_ly(dataForBubble, x = ~KOBIETY_PONIZEJ_18, y = ~MEZCZYZNI_PONIZEJ_18, text = ~WOJEWÓDZTWO, type = 'scatter', mode = 'markers',
            marker = list(size = ~PONIZEJ_18, opacity = 0.8,color = 'rgb(255, 65, 54)'))%>% 
      layout(title = 'Ludzie poniżej 18 roku życia wg województw',
             xaxis = list(showgrid = FALSE),
             yaxis = list(showgrid = FALSE))
  })
}


shinyApp(ui = ui, server = server)