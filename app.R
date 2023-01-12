library(shiny)
library(DT)
library(shinythemes)
library(ggcorrplot)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(rsconnect)
library(plyr)
library(skimr)
library(corrplot)



data <- read.csv("data/dataMinutes3MonthswithTime.csv")
data2 <- read.csv("data/dataMinutes3MonthswithoutTime.csv")
data3 <- read.csv("data/dataMinutes1MonthwithoutTime.csv")
data4 <- read.csv("data/dataHours1MonthwithoutTime.csv")


ui <- fluidPage( theme = shinytheme("cerulean"),
                 titlePanel("Pumpade too analuus", windowTitle = "Pumpade too analuus"),
                 helpText("Maksim Dmitrijev, Gleb Volodin. Andmete visualiseerimise projekt."),
                 br(),
                 
                 tabsetPanel(
                   tabPanel("Sissejuhatus",
                            sidebarPanel( img(src = "pilt.jpg", height = 500, width = 500)),
                            mainPanel(br(),tags$h3(htmlOutput("sissejuhatus")), br(), htmlOutput("text"), br(), htmlOutput("text2"), br(), htmlOutput("text3"), br(), tags$h3(htmlOutput("eesmargid")), br(), htmlOutput("text4"), br(), htmlOutput("text5"), br(), htmlOutput("text6"),br(), htmlOutput("text7"))
                            
                   ),
                   
                   tabPanel("Andmed",
                            
                            sidebarLayout(
                              sidebarPanel(tags$h4("Tunnused:"),br(), 
                                           tags$b("2FI1204")," - Oodatava amortisatsiooni ületamine",br(),
                                           tags$b("2PI1211")," - Oodatava eluea ületamine",br(),
                                           tags$b("2FIC1201")," - Planeeritud töötundide hälve",br(),
                                           tags$b("2K-167A")," - Keskmise kulutuse ületamine",br(),
                                           tags$b("2K-167")," - Keskmise toojõukulutuse ületamine",br(),
                                           tags$b("2K-169")," - Keskmisest efektiivsusest hälve",br(),
                                           tags$b("2PI1301")," - Mahakandmise staatus",br(),
                                           tags$b("2PI1212")," - ",br(),
                                           tags$b("2K175")," - ",br(),
                                           tags$b("2FI1203")," - ",br(),
                                           tags$b("2LIAS3250")," - ",br(),
                              
                              
                              radioButtons("dataChoice", "Data Set", choices = c("3 kuu andmed ajaga" = "data/dataMinutes3MonthswithTime.csv", "3 kuu andmed ajata" = "data/dataMinutes3MonthswithoutTime.csv", "1 kuu andmed ajata" = "data/dataMinutes1MonthwithoutTime.csv", "1 kuu tunnised andmed ajata" = "data/dataHours1MonthwithoutTime.csv"))),
                              
                              
                              mainPanel(br(),tags$h3(htmlOutput("ttitle")), DTOutput("tabel"))
                              
                              
                              
                            )),
                   
                   
                 
                   tabPanel("Kirjeldav analuus",
                            sidebarLayout(
                              sidebarPanel(selectInput("kirVariableX",
                                                       label = h4("Tunnus X:"),
                                                       choices = list("X2FI1204",
                                                                      "X2PI1211",
                                                                      "X2FIC1201",
                                                                      "X2K.167A",
                                                                      "X2K.167",
                                                                      "X2K.169",
                                                                      "X2PI1301",
                                                                      "X2PI1212",
                                                                      "X2K175",
                                                                      "X2FI1203",
                                                                      "X2LIAS3250"),
                                                       selected = "X2FI1204"),
                                           selectInput("kirVariableY",
                                                       label = h4("Tunnus Y: (Boxplot)"),
                                                       choices = list("X2FI1204",
                                                                      "X2PI1211",
                                                                      "X2FIC1201",
                                                                      "X2K.167A",
                                                                      "X2K.167",
                                                                      "X2K.169",
                                                                      "X2PI1301",
                                                                      "X2PI1212",
                                                                      "X2K175",
                                                                      "X2FI1203",
                                                                      "X2LIAS3250"),
                                                       selected = "X2FI1204")),
                              
                              
                              
                              mainPanel(br(),tags$h3(htmlOutput("chartitle")),htmlOutput("char"),br(),tags$h3(htmlOutput("histtitle")), plotOutput("histogramm"),br(),tags$h3(htmlOutput("boxtitle")), plotOutput("boxplot"),br(),htmlOutput("mean"))
                            ) 
                   ),
                   
                   tabPanel("Korrelatsioonimaatriks",
                            sidebarLayout(
                              sidebarPanel(selectInput("var1",
                                                       label = h4("Kuvata matriks ruutudena v6i ringidena:"),
                                                       choices = list("square",
                                                                      "circle"),
                                                       selected = "square"),
                                           
                                         
                                           
                                           radioButtons("var2", label = h4("Numbrite kuvamine"),
                                                        choices = list("TRUE" = TRUE, "FALSE" = FALSE), 
                                                        selected = FALSE),
                                           
                                           checkboxGroupInput("xvariablecorr",
                                                              label = h4("Tunnused:"),
                                                              choices = list("X2FI1204",
                                                                             "X2PI1211",
                                                                             "X2FIC1201",
                                                                             "X2K.167A",
                                                                             "X2K.167",
                                                                             "X2K.169",
                                                                             "X2PI1301",
                                                                             "X2PI1212",
                                                                             "X2K175",
                                                                             "X2FI1203",
                                                                             "X2LIAS3250"),
                                                              selected = "X2FI1204"),
                                           
                                           
                              ),
                              mainPanel(br(),tags$h3(htmlOutput("corrtitle")),plotOutput("corrplot"), htmlOutput("corrtext"))
                            ) 
                   ),
                   
                   
                   tabPanel("Lineaarne regressioon",
                            sidebarLayout(
                              sidebarPanel(checkboxGroupInput("xvariable",
                                                              label = h4("Tunnused X:"),
                                                              choices = list("X2FI1204",
                                                                             "X2PI1211",
                                                                             "X2FIC1201",
                                                                             "X2K.167A",
                                                                             "X2K.167",
                                                                             "mk$X2K.169",
                                                                             "2PI1301",
                                                                             "2PI1212",
                                                                             "2K175",
                                                                             "2FI1203",
                                                                             "2LIAS3250"),
                                                              selected = "X2FI1204"),
                                           
                                           selectInput("yvariable",
                                                       label = h4("Tunnused Y:"),
                                                       choices = list("X2FI1204",
                                                                      "X2PI1211",
                                                                      "X2FIC1201",
                                                                      "X2K.167A",
                                                                      "X2K.167",
                                                                      "X2K.169",
                                                                      "X2PI1301",
                                                                      "X2PI1212",
                                                                      "X2K175",
                                                                      "X2FI1203",
                                                                      "X2LIAS3250"),
                                                       selected = "X2FI1204"),
                                           
                                           
                              ),
                              
                              
                              mainPanel(verbatimTextOutput('lmSummary'),plotOutput("diagnosticPlot"))
                              
                              
                              
                              #mainPanel( #DTOutput("tb1"), 
                                #fluidRow(column(6, verbatimTextOutput('lmSummary')) , column(6, plotOutput('diagnosticPlot')))
                              #)
                            ) 
            
                   ),
                 )
)
                   
                   
                   
              
                   


server <- function(input, output) {
  output$ttitle <- renderText({"Andmed:"})
  #output$tabel <-renderDT(data3, options = list(searching = FALSE,ordering=F, pageLength =
   #                                            10,scrollX = TRUE)) 
  lmData = read.csv("data/dataHours1MonthwithoutTime.csv")
  myData = reactive({
    read.csv(input$dataChoice)
  })
  
  output$tabel = renderDT(myData(), options = list(searching = FALSE,ordering=F, pageLength =10,scrollX = TRUE)) 
  
 
  
  
  
  output$sissejuhatus <- renderText({"Sissejuhatus:"})
  output$text <- renderText({"Ettev6tes on olemas suur hulk erinevaid pumbad. Tanapaeval, seoes elektrienergia hinnaga on suur vajadus teha madalamaks ettev6tete kulud. Sellejaoks on tarvis labiviia katsed, et naha kas ja kuidas pumbade too reziimid mojuvad tootmist."})
  output$text2 <- renderText({"Selleks et katsete labiviimine oleks kergem ja insenerid saaksid aru millised vaartused on omavahel seotud ja kuidas need on seotud oli loodud antud rakendus."})
  output$text3 <- renderText({"Projektis oli kasutatud andmestik, mis on parit otse ettevotest ja andurite naitudest. GitHub: "})
  output$eesmargid <- renderText({"Projekti eesmargid:"})
  output$text4 <- renderText({"1. Anda ulevaade andmestikust."})
  output$text5 <- renderText({"2. Teostada andmete esialgse visuaalse analuusi soltuvuste defineerimiseks."})
  output$text6 <- renderText({"3. Aru saada kuidas pumpade erinevad naiturid omavahel seotud on"})
  output$text7 <- renderText({"4. Kursuses 'Andmete visualiseerimine' omandatud meetodite ja teadmiste rakendamine."})

  output$corrtitle <- renderText({"Tunnuste korrelatsioonimaatriks:"})
  output$corrplot <- renderPlot({data<-switch(input$var1,input$var2)
  output$corrtext <- renderText({""})
  
  #corrplot(cor(Filter(is.numeric, data4)),method='number')
  ggcorrplot(cor(data4[,input$xvariablecorr]), method = input$var1, lab = input$var2)
  
  }) 
  
  

  
  
  
  
  
  output$chartitle <- renderText({"Arvkarakteristikud:"})
  output$char <- renderText(kable_styling(kable(summary(data4))))
  output$histtitle <- renderText({"Histogramm:"})
  output$histogramm <- renderPlot({data<-switch(input$kirVariableX)
  ggplot(data4, aes_string(x = input$kirVariableX)) + geom_histogram(binwidth = 0.1, fill = "#EC7063", color = "black")}) 
  output$boxtitle <- renderText({"Boxplot:"})
  output$boxplot <- renderPlot({data<-switch(input$kirVariableX)
  #boxplot(data, horizontal =1, col="#90CAF9", xlab=input$var3) +
  #geom_boxplot()+
  ggplot(data4, aes_string(x = input$kirVariableX, y =input$kirVariableY)) + geom_boxplot(fill = "#00A5E3")
  }) 
  
  
 
  
  lmModel <- reactive({
    req(data4,input$xvariable,input$yvariable)
    x <- as.numeric(data4[[as.name(input$xvariable)]])
    y <- as.numeric(data4[[as.name(input$yvariable)]])
    current_formula <- paste0(input$yvariable, " ~ ", paste0(input$xvariable, collapse = " + "))
    current_formula <- as.formula(current_formula)
    model <- lm(current_formula, data = data4, na.action=na.exclude)
    return(model)
  })
  
  
  
  
  
  
  
  output$lmSummary <- renderPrint({
    req(lmModel())
    summary(lmModel())
  })
  
  output$diagnosticPlot <- renderPlot({
    req(lmModel())
    par(mfrow = c(2,2))
    plot(lmModel())
  })
  
  
}
shinyApp(ui = ui, server = server)