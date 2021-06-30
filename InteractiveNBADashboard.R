#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(ggvis)
library(plotly)
library(tidyverse)
basicstats <- read.csv("/Users/WebbWilliams/Downloads/NBABasicAndAdvStats2021.csv")
basicstats <- basicstats[,-1]
basicstats[,2] <- as.factor(basicstats[,2])
basicstats[,4] <- as.factor(basicstats[,4])
basicstats[,4] <- as.factor(basicstats[,51])
basicstats[,4] <- as.factor(basicstats[,52])


colnames(basicstats)[c(1:52)] <- c("Player","Pos","Age","TM","G","GS","MP","FG","FGA","FG%","3P",
                                   "3PA","3P%","2P","2PA","2P%","eFG%","FT","FTA","FT%","ORB","DRB",
                                   "TRB","AST","STL","BLK","TOV","PF","PTS","MPTOT","PER","TS%","3PAr","FTr",
                                   "ORB%","DRB%","TRB%", "AST%","STL%","BLK%","TOV%","USG%","OWS","DWS","WS","WS/48",
                                   "OBPM","DBPM","BPM","VORP","AllStar","AllNBA")

ui <- dashboardPage(skin = "red",
    dashboardHeader(title = "Interactive Basketball Dataset"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("The Dataset", tabName = "Dataset", icon = icon("th")),
            menuItem("Histogram", tabName = "Histogram", icon = icon("boxes")),
            menuItem("Scatter Plot", tabName = "ScatterPlot", icon = icon("calculator")),
            menuItem("3D Scatter Plot", tabName = "3DScatterPlot", icon = icon("cubes")),
            menuItem("Boxplot", tabName = "Boxplot", icon = icon("box"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Dataset",
                    fluidPage(
                        uiOutput("text"),
                        DT::dataTableOutput("mytable")
                    )),
            tabItem(tabName = "Histogram",
                    fluidPage(
                        uiOutput("histogramtab"),
                        plotlyOutput("myPlot"),
                        selectInput("histChoice",
                                    label = "Choose a statistic for the histogram",
                                    choices = colnames(basicstats),
                                    selected = "PTS"),
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 25)
                        ,
                        
                    )),
            tabItem(tabName = "ScatterPlot",
                    fluidPage(
                        uiOutput("scattertab"),
                        plotlyOutput("myScatter"),
                            selectInput("choiceX",
                                        label = "Choose a statistic for X axis",
                                        choices = colnames(basicstats),
                                        selected = "PTS")
                    
                    ,
                            selectInput("choiceY",
                                        label = "Choose a statistic for Y axis",
                                        choices = colnames(basicstats),
                                        selected = "AST")
                    ,
                    
                            selectInput("choiceCol",
                                        label = "Choose a statistic to color by",
                                        choices = colnames(basicstats[,c(2:4,51,52)]),
                                        selected = "Pos")
                        
                    ,
                    )),
            tabItem(tabName = "3DScatterPlot",
                    fluidPage(
                        uiOutput("scattertab3d"),
                        plotlyOutput("my3DScatter"),
                            selectInput("choiceX3D",
                                        label = "Choose a statistic for X axis",
                                        choices = colnames(basicstats),
                                        selected = "PTS")
                        ,
                            selectInput("choiceY3D",
                                        label = "Choose a statistic for Y axis",
                                        choices = colnames(basicstats),
                                        selected = "AST")
                        ,
                            selectInput("choiceZ3D",
                                        label = "Choose a statistic for Z axis",
                                        choices = colnames(basicstats),
                                        selected = "TRB")
                        ,
                            selectInput("choiceCol3D",
                                        label = "Choose a statistic to color by",
                                        choices = colnames(basicstats[,c(2:4,51,52)]),
                                        selected = "Pos")
                        ,
                    )),
            tabItem(tabName = "Boxplot",
                    fluidPage(
                        uiOutput("boxplottab"),
                        plotlyOutput("myBoxplot"),
                            selectInput("choiceBox",
                                        label = "Choose a statistic for the Boxplot",
                                        choices = colnames(basicstats),
                                        selected = "PTS")
                        
                    ))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$mytable <- DT::renderDataTable({
        basicstats
    },
    options = list(
        autoWidth = TRUE,
        scrollX = TRUE
    ))
    
    url4 <- a("Basketball Reference Link", href = "https://www.basketball-reference.com/leagues/NBA_2021_per_game.html")
    output$text <- renderUI({
        tagList("Thank you for using my interactive NBA web app! The tabs below will allow you
                                  to explore the regular season stats from the 19-20 season and create either a
                                  histogram, scatter plot, 3Dscatter plot, or boxplot of any of those statsAfter clicking on any of the tabs, simply select which inputs you would like
                                  to graph. If you have any questions about how to interpret the graph you created, 
                                  there is an educational hyperlink at the top of each tab. If you have any further questions about the app, they can be emailed to williamsjw@smu.edu
                and the dataset can be found here:", url4)
    })

    output$myPlot <- renderPlotly({ #referring input p in ui.r as input$p     
        fighist <- plot_ly(x = basicstats[,input$histChoice],type = "histogram",xaxis = input$bins,nbinsx = input$bins)
        fighist <- fighist %>% layout(
            title = "Histogram",
            scene = list(xaxis = list(title = input$histChoice))
        )
    })
    
    url <- a("Khan Academy Histogram Video", href = "https://www.khanacademy.org/math/cc-sixth-grade-math/cc-6th-data-statistics/histograms/v/interpreting-histograms")
    output$histogramtab <- renderUI({
        tagList("Want to learn how to interpret your histogram? Click the following link:", url)
    })
    
    output$myScatter <- renderPlotly({
        figxy <- plot_ly(x = basicstats[,input$choiceX],y = basicstats[,input$choiceY],color = basicstats[,input$choiceCol],type = "scatter",mode = "markers")
        figxy <- figxy %>% layout(
            title = "Scatter Plot",
            scene = list(
                xaxis = list(title = input$choiceX),
                yaxis = list(title = input$choiceY)
            )
        )
        figxy <- figxy %>% add_trace(
            text = paste("Player:", basicstats$Player,
                         "<br>X:", basicstats[,input$choiceX],
                         "<br>Y:", basicstats[,input$choiceY]),
            hoverinfo = 'text',
            showlegend = F
        )
    })
    
    url1 <- a("How to interpret scatter plot", href = "https://www.dummies.com/education/math/statistics/how-to-interpret-a-scatterplot/")
    output$scattertab <- renderUI({
        tagList("Want to learn how to interpret your Scatter Plot? Click the following link:", url1)
    })
    
    output$my3DScatter <- renderPlotly({
        fig3d <- plot_ly(x = basicstats[,input$choiceX3D],y = basicstats[,input$choiceY3D],z = basicstats[,input$choiceZ3D], color = basicstats[,input$choiceCol3D])
        fig3d <- fig3d %>% layout(
            title = "3D Scatter Plot",
            scene = list(
                xaxis = list(title = input$choiceX3D),
                yaxis = list(title = input$choiceY3D),
                zaxis = list(title = input$choiceZ3D)
            )
        )
        fig3d <- fig3d %>% add_trace(
            text = paste("Player:", basicstats$Player,
                         "<br>X:", basicstats[,input$choiceX3D],
                         "<br>Y:", basicstats[,input$choiceY3D],
                         "<br>Z:", basicstats[,input$choiceZ3D]),
            hoverinfo = 'text',
            showlegend = F
        )
    })
    
    url2 <- a("How to interpret 3D scatter plot", href = "https://support.minitab.com/en-us/minitab/19/help-and-how-to/graphs/3d-scatterplot/interpret-the-results/key-results/")
    output$scattertab3d <- renderUI({
        tagList("Want to learn how to interpret your 3D Scatter Plot? Click the following link:", url2)
    })
    
    output$myBoxplot <- renderPlotly({
        figbox <- plot_ly(y = basicstats[,input$choiceBox],type = "box",quartilemethod = "linear")
        figbox <- figbox %>% layout(
            title = "Boxplot",
            scene = list(xaxis = list(title = input$choiceBox))
        )
    })
    
    url3 <- a("How to interpret a boxplot", href = "https://www.khanacademy.org/math/cc-sixth-grade-math/cc-6th-data-statistics/cc-6th-box-whisker-plots/v/interpreting-box-plots")
    output$boxplottab <- renderUI({
        tagList("Want to learn how to interpret your Boxplot? Click the following link:", url3)
    })
    
    }


# Run the application 
shinyApp(ui = ui, server = server)


