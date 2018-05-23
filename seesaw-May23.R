library(shinydashboard)
library(data.table)
library(highcharter)
library(shinythemes)
library(reshape2)
library(ggplot2)
library(shinyjs)
library(plotly)
library(scales)
library(shiny)
library(readr)
library(dplyr)
library(plyr)
library(DT)
library(V8)
#cat("\014")

#sample <- fread("~/Documents/- tools/- r/seesaw/data/sample.csv")
#facebk <- fread("~/Documents/- tools/- r/seesaw/data/facebook.csv")
#twittr <- fread("~/Documents/- tools/- r/seesaw/data/twitter.csv")
#adwrds <- fread("~/Documents/- tools/- r/seesaw/data/adwords.csv")
sample <- fread("~/Downloads/sample.csv")
#setnames(sample, old = c('Interest Group','Amount Spent'), new = c('Interest Group','AmountSpent'))
sample$Date <- as.Date(as.character(sample$Date),"%Y-%m-%d")
h <-c("League of Conservation Voters","Planned Parenthood","Progress Michigan")

sample$Theme <- paste0("<a href='",sample$Theme,"' target='_blank'>",sample$Theme,"</a>")

jsCode <- "shinyjs.dateRefresh = function(params){
for(i=0;i<document.querySelectorAll('.input-daterange').length;i++){
document.querySelectorAll('.input-daterange')[i].querySelectorAll('.input-sm')[0].value = params[0][0];
document.querySelectorAll('.input-daterange')[i].querySelectorAll('.input-sm')[1].value = params[0][1];
console.log(params[0][0]);
console.log(params[0][1]);
}}"
#setInterval(function(){var a = window.pageYOffset;window.scrollTo(0,a+75);},25);


header <- fluidRow(
  tags$h1("MICHIGAN GUBERNATORIAL ELECTION 2018"),
  tags$img(src = 'https://watermelonrouge.github.io/seesaw/michigan2.png', class = 'map'),
  tags$h5('Powered by Alchemy', class = 'power'),
  tags$img(src = 'https://watermelonrouge.github.io/seesaw/alchemy.png', class = 'alchemy')
)

interest_groups <- c('The League of Conservation Voters',
                     'Progress Michigan',
                     'Planned Parenthood')

themes <- c('Healthcare',
            'Hold Government Officials Accountable',
            'Infrastructure',
            'Guns',
            'Keep Lakes Clean',
            'Our Freedoms',
            'Education',
            'Criminal Justice Reform',
            'Women’s Rights',
            'Marijuana Legalization',
            'Economy & Jobs',
            'Clean Water / Flint')

summary_view <- tabPanel(
  h[1],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Media & Demographic Performance Summary"),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3("Media Summary Table")
    ),
    fluidRow(
      DT::dataTableOutput("table")
    ),
    fluidRow(
      tags$h3("Media Summary Chart")
    ),
    fluidRow(
      highchartOutput("LCV_Rate_Chart")
    ),
    fluidRow(
      tags$h3("Performance Summary by Interest Group")
    ),
    fluidRow(
      column(3,
             tags$img(class='interest', src = 'https://watermelonrouge.github.io/seesaw/lcv.png'),
             tags$img(class='interest', src = 'https://watermelonrouge.github.io/seesaw/pm.png'),
             tags$img(class='interest', src = 'https://watermelonrouge.github.io/seesaw/pp.png')
      ),
      column(3
      ),
      column(3
      )
    ),
    fluidRow(
      tags$h3("Cumulative Spend")
    ),
    fluidRow(
      tags$h3("Channel Performance Summary")
    ),
    fluidRow(
      tags$h3("Website Performance Summary")
    )
  )
)

media_view <- tabPanel(
  h[2],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Media Performance Detail"),
      tags$div(class='below_title')
    ),
    fluidRow(tags$h3("Section 1")),
    fluidRow(tags$h3("Section 2")),
    fluidRow(tags$h3("Section 3"))
  )
)

web_view <- tabPanel(
  h[3],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Interest Group Website Performance Detail"),
      tags$div(class='below_title')
    ),
    fluidRow(
      tags$h3("Website Traffic")
    ),
    fluidRow(
      tags$h3("Website Engagement")
    ),
    fluidRow(
      tags$h3("Popular Content")
    )
  )
)

sentiment_view <- tabPanel(
  h[4],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Audience Sentiment Analysis"),
      tags$div(class='below_title')
    ),
    fluidRow(tags$h3("Section 1")),
    fluidRow(tags$h3("Section 2")),
    fluidRow(tags$h3("Section 3"))
  )
)

geo_view <- tabPanel(
  h[5],
  mainPanel(
    header,
    fluidRow(
      tags$h2("Location-Specific Analysis"),
      tags$div(class='below_title')
    ),
    fluidRow(tags$h3("Section A")),
    fluidRow(tags$h3("Section B")),
    fluidRow(tags$h3("Section C"))
  )
)

testarea <- tabPanel("TEST AREA",
   mainPanel(
     header,
     fluidRow(
       tags$h2("A tab to just try things out!"),
       tags$div(class='below_title')
     ),
     fluidRow(
       tags$h3("Plotly Test - Channel response and all filters, by Date")
     ),
     fluidRow(
       plotlyOutput("test1")
     ),
     fluidRow(
       tags$h3("Highctarts test - stacked bar (not currently working!)")
     ),
     fluidRow(
       highchartOutput("test_chart1")       
     ),
     fluidRow(
       tags$h3("Highcharts Test 2")
     ),
     fluidRow(
       highchartOutput("test2")
     )
   )
)


ui <- fluidPage(
  titlePanel("Michigan: Powered by Alchemy"),
  #useShinyjs(),
  #extendShinyjs(text = jsCode),
  theme = "style.css",
  column(3,
      tags$div(class='cbx interest',checkboxInput('interestCheck','Show',TRUE)),
      selectInput("interest","Interest Group:",c("All Interest Groups" = "all_groups",interest_groups)),
      tags$div(class='cbx theme',checkboxInput('themeCheck','Show',TRUE)),
      selectInput("theme","Theme:",c("All Themes" = "all_themes",themes)),
      tags$div(class='cbx channel',checkboxInput('channelCheck','Show',FALSE)),
      selectInput("channel", "Performance Channel:",c("All Channels"="all_channels","Facebook","Twitter","Adwords","YouTube","Website Performance")),
      tags$div(class='cbx date',checkboxInput('dateCheck','Show',FALSE)),
      dateRangeInput('dateRange', label = 'Select Date Range:',start = min(sample$Date), end = max(sample$Date))
  ),
  column(9,
    navbarPage("",position = "fixed-top",
      summary_view,
      media_view,
      web_view,
      sentiment_view,
      geo_view,
      testarea
    )
  )
)

colorChoices <- list(
  'rgb(124, 181, 236)',
  'rgb(224, 157, 157)',
  'rgb(124, 236, 181)',
  'rgb(255, 245, 165)'
)

####FUNCTION TO MAKE PLOTLY BAR CHARTS WITH DYNAMIC INPUT
makePlotlyChart <- function(dt,dims,metrics,dates,channel,interest,theme) {
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if (channel != "all_channels"){s <- s[s$Channel == channel,]}
    if (interest != "all_groups"){s <- s[s$`Interest Group` == interest,]}
    if (theme != "all_themes"){s <- s[s$Theme == theme,]}
    s <- s[,c(dims,metrics),with=FALSE]
    s <- s[,j=list(Reach = sum(Reach)),by=dims]
    s <- dcast(s, as.formula(paste("y~", paste(grep("^x", colnames(s), value = T) ), fun=sum)))
    p <- plot_ly(s, x = ~s[,1], y = ~s[,2], type = 'bar', name = colnames(s)[2], marker = list(color = 'rgb(124, 181, 236)')) %>%
    layout(p, yaxis = list(title = 'Reach'), barmode = 'stack')
    if(ncol(s) > 2){p <- add_trace(p, y = ~s[,3], name = colnames(s)[3], marker = list(color = 'rgb(224, 157, 157)'))}
    if(ncol(s) > 3){p <- add_trace(p, y = ~s[,4], name = colnames(s)[4], marker = list(color = 'rgb(124, 236, 181)'))}
    if(ncol(s) > 4){p <- add_trace(p, y = ~s[,5], name = colnames(s)[5], marker = list(color = 'rgb(255, 245, 165)'))}
    if(ncol(s) > 5){p <- add_trace(p, y = ~s[,6], name = colnames(s)[6], marker = list(color = 'rgb(0, 245, 165)'))}
    p
}

makeHighChart <- function(){
    #add function here
}

####FUNCTION TO MAKE TABLES WITH DYNAMIC INPUT
makeTable <- function(dt,dims,metrics,dates,channel,interest,theme,checks){
  DT::datatable({
    s <- dt[(dt$Date >= dates[1]) & (dt$Date <= dates[2]),]
    if (channel != "all_channels"){s <- s[s$Channel == channel,]}
    if (interest != "all_groups"){s <- s[s$`Interest Group` == interest,]}
    if (theme != "all_themes"){s <- s[s$Theme == theme,]}
    if(checks[1]==FALSE){dims <- dims[!dims %in% 'Theme']}
    if(checks[2]==FALSE){dims <- dims[!dims %in% 'Channel']}
    if(checks[3]==FALSE){dims <- dims[!dims %in% 'Date']}
    if(checks[4]==FALSE){dims <- dims[!dims %in% 'Interest Group']}
    s <- s[,c(dims,metrics),with=FALSE]  #[,j=list(Reach=sum(Reach)),by=dims]
    s <- s[,lapply(.SD,sum),by=dims]
    s$`Amount Spent` <- dollar_format()(s$`Amount Spent`)
    s
  },escape=FALSE)  
}

server <- function(input, output){
  output$table <- DT::renderDataTable({
    checks <- c(input$themeCheck,input$channelCheck,input$dateCheck,input$interestCheck)
    makeTable(sample,c('Date','Interest Group','Theme','Channel'),c('Amount Spent','Reach','Impressions','Media Engagements','Website Engagements'),input$dateRange,input$channel,input$interest,input$theme,checks)
  })
  output$chart1 <- renderPlotly(
    makePlotlyChart(sample,c('Date','Interest Group'),c('Reach'),input$dateRange,input$channel,input$interest,input$theme)
  )
  
  output$LCV_Rate_Chart <- renderHighchart(LCV_Rate_Chart)
  
  observeEvent(input$dateRange1, {
    #js$dateRefresh(input$dateRange1)
  })
  observeEvent(input$dateRange2, {
    #js$dateRefresh(input$dateRange1)
  })
  output$test_chart1 <- renderHighchart({
    group1 <- sample[,j=list(`Amount Spent`=sum(`Amount Spent`)), by = c('Date','Channel')]
    group1 <- group1[(group1$Date >= input$dateRange[1]) & (group1$Date <= input$dateRange[2]),]
    #hc <- group1 %>%
    hc <- hchart(group1, "column", hcaes(x = 'Date', y = `Amount Spent`)) %>%
      hc_plotOptions(series = list(stacking = "normal"))
    hc
  })
  
  
  
  #..........TEST CHARTS..........#
  
  #output$test1 <- renderPlotly({
  #  makePlotlyChart(sample,input$dateRange,input$channel,input$interest,input$theme)
  #})
  output$test2 <- renderHighchart({
    s <- sample[,c('Date','Reach'),with=FALSE]
    s <- s[,lapply(.SD,sum),by='Date']
    hchart(s[(s$Date >= input$dateRange[1]) & (s$Date <= input$dateRange[2]),], "column", hcaes(x = Date, y = Reach))
  })
}

shinyApp(ui = ui, server = server)