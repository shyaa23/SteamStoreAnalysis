# Global
library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(broom)
library(shinyglide)
library(shinyjs)
library(shinythemes)
library(flexdashboard)
library(shinyWidgets)
library(rvest)
library(robotstxt)
library(tm)
library(wordcloud)
library(XML)
library(RCurl)
library(rsconnect)



#for word cloud genres
game= read_csv("data/steam.csv")
games = game%>%
    mutate(total_rating = positive_ratings+negative_ratings, p_positive = positive_ratings/total_rating*100)%>%
    filter(total_rating>10000)%>%
    head(100)%>%
    select(appid, name, genres)%>%
    separate_rows(genres, sep=";", convert = TRUE)%>%
    group_by(appid)%>%
    mutate(genres=paste(unique(genres),collapse=", "))

tags = game%>%
    mutate(total_rating = positive_ratings+negative_ratings, p_positive = positive_ratings/total_rating*100)%>%
    filter(total_rating>10000)%>%
    head(100)%>%
    select(appid, name, steamspy_tags)%>%
    separate_rows(steamspy_tags, sep=";", convert = TRUE)%>%
    group_by(appid)%>%
    mutate(steamspy_tags=paste(unique(steamspy_tags),collapse=", "))%>%
    unique()

#general data
steam = read_csv("data/steam_final.csv")
topTV = read.csv("data/topTV.csv", header=TRUE)
topTVgenre = topTV%>%
    select(title,genre)
topTVkeyword = topTV%>%
    select(title,keywords)

steam<-steam%>%
    mutate(total_rating = positive_ratings+negative_ratings, p_positive = positive_ratings/total_rating*100, date = date(release_date), year = year(release_date), month= month(release_date))%>%
    mutate(english=as.factor(english),
           Action=as.factor(Action),
           Adventure=as.factor(Adventure),
           Indie=as.factor(Indie),
           Casual=as.factor(Casual),
           Other=as.factor(Other),
           Singleplayer=as.factor(Singleplayer),
           Multiplayer=as.factor(Multiplayer),
           Sports=as.factor(Sports),
           RPG=as.factor(RPG),
           Racing=as.factor(Racing),
           Massively_multiplayer=as.factor(Massively_multiplayer),
           Simulation=as.factor(Simulation),
           price = as.numeric(price)
    )


# Define UI for application 
ui <- fluidPage(theme = shinytheme("united"),
                useShinyjs(),
                dashboardPage(
                    #UI Header
                    dashboardHeader(title = "Steam Analyzer"),
                    #UI Sidebar
                    dashboardSidebar(sidebarMenu(
                        menuItem("Explore Steam", icon = icon("th"), tabName = "overview"),
                        menuItem("What's Popular", tabName = "popular", icon = icon("fire"),
                                 badgeLabel = "HOT", badgeColor = "red"),
                        menuItem("Build-a-Game", tabName = "model", icon = icon("dashboard"))
                    )
                    ),
                    #UI Body
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = "overview",
                                    fluidRow(
                                        infoBox("Number of Variables", width = 6, paste(ncol(steam)), icon = icon("columns")),
                                        infoBox("Number of Games", width = 6, paste(nrow(steam)), icon = icon("table"))),
                                    fluidRow(
                                        box(title = "Explore The Leaders", collapsible = TRUE, collapsed =TRUE, width = 12,
                                            tabBox( width = 12, tabPanel("Top Developers", 
                                                                         sidebarLayout(
                                                                             sidebarPanel(
                                                                                 radioButtons("topDev", "Check out the best developers", choices =
                                                                                                  c("Top 3" = "3",
                                                                                                    "Top 5" = "5",
                                                                                                    "Top 10" = "10",
                                                                                                    "Top 15" = "15"), selected = "3")
                                                                             ),
                                                                             mainPanel( 
                                                                                 plotOutput("developers")
                                                                             )
                                                                         )
                                            ),
                                            
                                            tabPanel("Top Publishers", 
                                                     sidebarLayout(
                                                         sidebarPanel(
                                                             radioButtons("topPub", "Check out the best publishers", choices =
                                                                              c("Top 3" = "3",
                                                                                "Top 5" = "5",
                                                                                "Top 10" = "10",
                                                                                "Top 15" = "15"), selected = "3")
                                                         ),
                                                         mainPanel( 
                                                             plotOutput("publishers")
                                                         )
                                                     )
                                            ),
                                            tabPanel("Top Games", 
                                                     sidebarLayout(
                                                         sidebarPanel(
                                                             radioButtons("topGame", "Check out the best games today", choices =
                                                                              c("Top 3" = "3",
                                                                                "Top 5" = "5",
                                                                                "Top 10" = "10",
                                                                                "Top 15" = "15"), selected = "3")
                                                         ),
                                                         mainPanel( 
                                                             plotOutput("games")
                                                         )
                                                     )
                                            ))
                                            
                                        ), 
                                        box(title = "Explore The Distributions", collapsible = TRUE, width = 12, collapsed = TRUE,
                                            tabBox(width = 12, tabPanel("Achievements", 
                                                                        sidebarLayout(
                                                                            sidebarPanel(
                                                                                radioButtons("achievements", "Filter games by number of total reviews:", choices =
                                                                                                 c("All Games" = "0",
                                                                                                   "100+ Reviews" = "100",
                                                                                                   "1,000+ Reviews" = "1000",
                                                                                                   "10,000+ Reviews" = "10000"), selected = "0")
                                                                            ),
                                                                            mainPanel( 
                                                                                plotOutput("achievement")
                                                                            )
                                                                            
                                                                        )
                                            ),tabPanel("Price", 
                                                       sidebarLayout(
                                                           sidebarPanel(
                                                               radioButtons("price", "Filter games by number of total reviews:", choices =
                                                                                c("All Games" = "0",
                                                                                  "100+ Reviews" = "100",
                                                                                  "1,000+ Reviews" = "1000",
                                                                                  "10,000+ Reviews" = "10000"), selected = "0")
                                                           ),
                                                           mainPanel( 
                                                               plotOutput("price")
                                                           )
                                                           
                                                       )
                                            ),tabPanel("Positive Rating", 
                                                       sidebarLayout(
                                                           sidebarPanel(
                                                               radioButtons("rate", "Filter games by number of total reviews:", choices =
                                                                                c("All Games" = "0",
                                                                                  "100+ Reviews" = "100",
                                                                                  "1,000+ Reviews" = "1000",
                                                                                  "10,000+ Reviews" = "10000"), selected = "0")
                                                           ),
                                                           mainPanel( 
                                                               plotOutput("rate")
                                                           )
                                                           
                                                       )
                                            )
                                            
                                            
                                            ))),
                                    fluidRow(
                                        box(title = "Explore Games Over Time",width = 12, collapsible = TRUE, collapsed = TRUE,
                                            tabPanel("Time series", 
                                                     sidebarLayout(
                                                         sidebarPanel(
                                                             
                                                             radioButtons("col","Time series for:",
                                                                          choices = c("Games by year",
                                                                                      "Games by month",
                                                                                      "Compare playtime and price per year"),
                                                                          selected = "Games by year"),
                                                             
                                                             conditionalPanel(
                                                                 condition = "input.col == 'Games by year'",
                                                                 sliderInput("slider_year", "Year:", 
                                                                             min=1997,
                                                                             max=2019,
                                                                             value=c(2013,2018),
                                                                             sep = '',
                                                                             animate=TRUE
                                                                 )),
                                                             conditionalPanel(
                                                                 condition = "input.col == 'Games by month'",
                                                                 sliderInput("slider_month", "Month:", 
                                                                             min=1,
                                                                             max=12,
                                                                             value=c(1,12),
                                                                             sep = '',
                                                                             animate=TRUE
                                                                 ))
                                                         ),
                                                         
                                                         mainPanel( 
                                                             #textOutput("t")
                                                             #tableOutput('ta')
                                                             conditionalPanel(
                                                                 condition = "input.col == 'Games by year'",
                                                                 plotOutput("timeseries_year")
                                                             ),
                                                             conditionalPanel(
                                                                 condition = "input.col == 'Games by month'",
                                                                 plotOutput("timeseries_month")
                                                             ),
                                                             conditionalPanel(
                                                                 condition = "input.col == 'Compare playtime and price per year'",
                                                                 plotOutput("timeseries_compare")
                                                             )
                                                         )
                                                     ))
                                            
                                        )
                                    ),
                                    fluidRow(
                                        box(title = "Top Games",width = 12, collapsible = TRUE, collapsed = TRUE,
                                            infoBoxOutput("SingleplayerBox"),
                                            infoBoxOutput("MultiplayerBox"),
                                            infoBoxOutput("MassivelyMultiplayerBox"),
                                            infoBoxOutput("nonEnglish"),
                                            infoBoxOutput("BestBox"),
                                            infoBoxOutput("LastBox")
                                        )
                                    )
                            ),
                            #TAB2               
                            tabItem(tabName = "popular",
                                    fluidRow(
                                        box(title = "Top 100 TV Genres", width = 6, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                                            plotOutput("tvGenre")),
                                        box(title = "Top 100 Game Genres", width = 6, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                                            plotOutput("gameGenre"))
                                        
                                    ),
                                    fluidRow(
                                        box(title = "Top 100 TV Keywords", width = 6, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "info",
                                            plotOutput("tvKey")),
                                        box(title = "Top 100 Game Keywords", width = 6, collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "info",
                                            plotOutput("gameKey"))
                                        
                                    )
                            ),
                            #TAB3
                            tabItem(tabName = "model",
                                    titlePanel("Predictive Analytics on Steams data"),
                                    
                                    tabsetPanel(
                                        tabPanel("Train Model",
                                                 sidebarLayout(
                                                     sidebarPanel(
                                                         uiOutput('predictors'),
                                                         # actionButton('reset','Reset'),
                                                         checkboxInput('model','View summary of the Model')
                                                     ),
                                                     
                                                     mainPanel(
                                                         tableOutput('details'),
                                                         conditionalPanel(
                                                             condition = 'input.model==true',
                                                             h4("Summary"),
                                                             verbatimTextOutput("model")
                                                         )
                                                     )
                                                 )
                                        ), tabPanel("Predict",
                                                    sidebarLayout(
                                                        sidebarPanel(
                                                            #uiOutput("levels1")
                                                            fluidRow(
                                                                column(3,selectInput("Action",
                                                                                     label = "Action",
                                                                                     choices = sort(unique(steam$Action)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Adventure",
                                                                                     label = "Adventure",
                                                                                     choices = sort(unique(steam$Adventure)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Casual",
                                                                                     label = "Casual",
                                                                                     choices = sort(unique(steam$Casual)),
                                                                                     selected = NULL))
                                                            ),
                                                            fluidRow(
                                                                column(3,selectInput("Indie",
                                                                                     label = "Indie",
                                                                                     choices = sort(unique(steam$Indie)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Racing",
                                                                                     label = "Racing",
                                                                                     choices = sort(unique(steam$Racing)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("RPG",
                                                                                     label = "RPG",
                                                                                     choices = sort(unique(steam$RPG)),
                                                                                     selected = NULL))
                                                            ),
                                                            fluidRow(
                                                                column(3,selectInput("Simulation",
                                                                                     label = "Simulation",
                                                                                     choices = sort(unique(steam$Simulation)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Sports",
                                                                                     label = "Sports",
                                                                                     choices = sort(unique(steam$Sports)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Other",
                                                                                     label = "Other",
                                                                                     choices = sort(unique(steam$Other)),
                                                                                     selected = NULL))
                                                            ),
                                                            fluidRow(
                                                                column(3,selectInput("Singleplayer",
                                                                                     label = "SinglePlayer",
                                                                                     choices = sort(unique(steam$Singleplayer)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Multiplayer",
                                                                                     label = "Multiplayer",
                                                                                     choices = sort(unique(steam$Multiplayer)),
                                                                                     selected = NULL)),
                                                                column(3,selectInput("Massively_multiplayer",
                                                                                     label = "Massively_multiplayer",
                                                                                     choices = sort(unique(steam$Massively_multiplayer)),
                                                                                     selected = NULL))
                                                            ),
                                                            sliderInput("dollar", label="Price", min = 0, max = 100, value =5),
                                                            actionButton("do", "Predict")
                                                            
                                                        ),
                                                        
                                                        mainPanel(
                                                            h4("Prediction"),
                                                            h5("Model is trained on following variables:"),
                                                            verbatimTextOutput("selected_var"),
                                                            h5('The game would have following postive review percent based on the given input features.'),
                                                            uiOutput('gameDesc'),
                                                            uiOutput(outputId = 'Pred'),
                                                            gaugeOutput("gauge")
                                                            
                                                            
                                                        )
                                                    )
                                        )  
                                        
                                        
                                    )
                                    
                            )
                        )
                    )
                )
)


# Define server logic 
server <- function(input, output) {
    
    observeEvent(input$reset, {
        reset("x")
    })
    
    output$developers <- renderPlot({
        x =as.numeric(input$topDev)
        top = steam%>%
            count(developer)%>%
            arrange(desc(n))%>%
            mutate(id = row_number())%>%
            head(x)
        
        ggplot(top, aes(x=reorder(developer,id), n)) +
            geom_bar(stat = "identity", fill = "#00008b") + labs(y = "Count", x = "Developer Name") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ), text = element_text(size=15)) 
    })
    
    output$publishers <- renderPlot({
        x =as.numeric(input$topPub)
        top = steam%>%
            count(publisher)%>%
            arrange(desc(n))%>%
            mutate(id = row_number())%>%
            head(x)
        
        ggplot(top, aes(x=reorder(publisher,id), n)) +
            geom_bar(stat = "identity", fill = "#00008b") + labs(y = "Count", x = "Publisher Name") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ), text = element_text(size=15)) 
    })
    
    output$games <- renderPlot({
        x =as.numeric(input$topGame)
        top = steam%>%
            filter(total_rating >1000)%>%
            arrange(desc(p_positive))%>%
            mutate(id = row_number(), name = ifelse(name=="東方天空璋 ～ Hidden Star in Four Seasons.", "Hidden Star in Four Seasons", name))%>%
            head(x)
        
        ggplot(top, aes(x=reorder(name,id), p_positive)) +
            geom_bar(stat = "identity", fill = "#00008b") + labs(y = "Percent Positive Reviews", x = "Game Name") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ), text = element_text(size=15)) 
    })
    
    
    output$achievement <- renderPlot({
        x =as.numeric(input$achievements)
        achievement = steam%>%
            filter(total_rating >x)
        
        ggplot(achievement, aes(x = achievements)) +
            geom_histogram(binwidth = 10, fill = "#00008b") + labs(y = "Number of Games", x = "Number of Achievements") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ), text = element_text(size=15)) 
        
    })
    
    output$price <- renderPlot({
        x =as.numeric(input$price)
        achievement = steam%>%
            filter(total_rating >x)
        
        ggplot(achievement, aes(x = price)) +
            geom_histogram(binwidth = 2, fill = "#00008b") + labs(y = "Number of Games", x = "Price of Game ($)") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ), text = element_text(size=15)) 
        
    })
    
    
    output$rate <- renderPlot({
        x =as.numeric(input$rate)
        achievement = steam%>%
            filter(total_rating >x)
        
        ggplot(achievement, aes(x = p_positive)) +
            geom_histogram(binwidth = 2, fill = "#00008b") + labs(y = "Number of Games", x = "Percent Positive Reviews") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ), text = element_text(size=15)) 
        
    })
    
    output$SingleplayerBox <- renderInfoBox({
        
        top = steam%>%
            filter(Singleplayer==1)%>%
            filter(total_rating>1000)%>%
            head(1)
        
        infoBox(
            "Top Singleplayer", HTML("<h5>", paste(top$name,br(), "Developer:", top$developer, br(),"Publisher:", top$publisher, br(), "Price:", top$price ), "<h5>"), icon = icon("user"),
            color = "green"
        )
    })
    
    output$nonEnglish <- renderInfoBox({
        
        top = steam%>%
            filter(english==0)%>%
            filter(total_rating>1000)%>%
            arrange(desc(p_positive))%>%
            head(1)%>%
            mutate(name = ifelse(name=="東方天空璋 ～ Hidden Star in Four Seasons.", "Hidden Star in Four Seasons", name))
        
        infoBox(
            "Top Non-English", HTML("<h5>", paste(top$name,br(), "Developer:", top$developer, br(),"Publisher:", top$publisher, br(), "Price:", top$price ), "<h5>"), icon = icon("globe-asia"),
            color = "blue"
        )
    })
    
    output$MultiplayerBox <- renderInfoBox({
        
        top = steam%>%
            filter(Multiplayer==1)%>%
            filter(total_rating>1000)%>%
            head(1)
        
        infoBox(
            "Top Multiplayer", HTML("<h5>", paste(top$name,br(), "Developer:", top$developer, br(),"Publisher:", top$publisher, br(), "Price:", top$price ), "<h5>"), icon = icon("user-friends"),
            color = "navy"
        )
    })
    
    
    output$MassivelyMultiplayerBox <- renderInfoBox({
        
        top = steam%>%
            filter(Massively_multiplayer==1)%>%
            filter(total_rating>1000)%>%
            head(1)
        
        infoBox(
            "Top Massively Multiplayer", HTML("<h5>", paste(top$name,br(), "Developer:", top$developer, br(),"Publisher:", top$publisher, br(), "Price:", top$price ), "<h5>"), icon = icon("users"),
            color = "red"
        )
    })
    
    output$BestBox <- renderInfoBox({
        
        top = steam%>%
            mutate(year = year(date))%>%
            filter(year==2019)%>%
            filter(total_rating>1000)%>%
            head(1)
        
        infoBox(
            "Top Game of 2019", HTML("<h5>", paste(top$name,br(), "Developer:", top$developer, br(),"Publisher:", top$publisher, br(), "Price:", top$price ), "<h5>"), icon = icon("star"),
            color = "orange"
        )
    })
    
    output$LastBox <- renderInfoBox({
        
        top = steam%>%
            mutate(year = year(date))%>%
            filter(year>=2010)%>%
            filter(total_rating>1000)%>%
            head(1)
        
        infoBox(
            "Top Game of the Decade", HTML("<h5>", paste(top$name,br(), "Developer:", top$developer, br(),"Publisher:", top$publisher, br(), "Price:", top$price ), "<h5>"), icon = icon("award"),
            color = "fuchsia"
        )
    })
    
    #MODEL
    df <- steam%>%
        filter(total_rating>10000)%>%
        filter(english==1)
    
    
    y = df$p_positive
    
    
    xcol <- reactive({
        xcol <- input$x
        xf <- as.data.frame(df[,xcol])
        names(xf) <- xcol
        xf
    })
    
    output$predictors <- renderUI({
        predictor_data <- df %>%
            select(-c(X1,name,appid,release_date,developer,publisher,platforms, english, required_age,
                      steamspy_tags,owners,positive_ratings,negative_ratings, achievements,
                      total_rating,p_positive,average_playtime, median_playtime, date, month, year))
        x <- names(predictor_data)
        selectInput("x",
                    label    = "Choose Predictor Varaible(s)",
                    selected = "Action",
                    choices  = x,
                    multiple = TRUE)
        
        
    })
    
    output$model <- renderPrint({
        cat('Linear regression is performed on the formula:','p_positve','~',
            paste(names(xcol()),collapse='+') )
        fit <- lm(y~., data=xcol())
        summary(fit)
        
    })
    
    output$details <- renderTable({
        fit <- lm(y~., data=xcol())
        intercept =tidy(fit)$estimate[1]
        term = tidy(fit)$term[-1]
        #Get the values of slope from summary of linear model
        slope = paste(term,":",tidy(fit)$estimate[-1],collapse=",")
        #Get the pvalues from summary of linear model
        p_value = glance(fit)$p.value
        #Get the R sqaured from summary of linear model
        rsquare = glance(fit)$r.squared
        #Get the adjusted R square value from summary of linear model
        adjrsquare = glance(fit)$adj.r.squared
        #Rounding the adjusted and R square values to 3 digits
        adjrsquare=round(adjrsquare,4)
        rsquare=round(rsquare,4)
        
        #Just for the information to keep on the table which formula had been used in model creation for different subset
        model_formula = paste(paste('p_positve',"~", paste(names(xcol()), collapse=" + ")))
        
        #Create a dataframe that contains all the required information of every possible subset and its model
        df <- data.frame(model_formula,intercept,slope,
                         p_value,rsquare,adjrsquare) 
        df
    })
    
    observeEvent(input$do, {
        price=input$dollar
        Action=input$Action
        Adventure=input$Adventure
        Indie=input$Indie
        Casual=input$Casual
        Simulation=input$Simulation
        Sports=input$Sports
        RPG = input$RPG
        Racing = input$Racing
        Massively_multiplayer=input$Massively_multiplayer
        Other=input$Other
        Singleplayer=input$Singleplayer
        Multiplayer=input$Multiplayer
        t = data.frame(price, Action,
                       Indie,Casual,Adventure,Other,
                       Singleplayer,Multiplayer, Simulation, RPG, Racing, Sports, Massively_multiplayer)
        model <- lm(y~., data=xcol())
        
        output$gameDesc<-renderUI({
            desc = "You've selected a "
            
            if(!is.null(price)){
                desc=paste0(desc, "$", input$dollar, ",")
            }
            if(Singleplayer==1){
                desc=paste(desc, "Single Player,")
            }
            if(Multiplayer==1){
                desc=paste(desc, "Multiplayer,")
            }
            if(Massively_multiplayer==1){
                desc=paste(desc, "Massively Multiplayer,")
            }
            if(Indie==1){
                desc=paste(desc, "indie,")
            }
            if(Action==1){
                desc=paste(desc, "action,")
            }
            if(Adventure==1){
                desc=paste(desc, "adventure,")
            }
            if(Casual==1){
                desc=paste(desc, "casual,")
            }
            if(Simulation==1){
                desc=paste(desc, "simulation,")
            }
            if(Sports==1){
                desc=paste(desc, "sports,")
            }
            if(RPG==1){
                desc=paste(desc, "RPG,")
            }
            if(Racing==1){
                desc=paste(desc, "Racing")
            }
            paste(desc, "game.")
        })
        
        output$Pred<- renderUI({
            paste0("This game will have ", round(predict(model,t),digits = 0), "% positive reviews!")
        })
        
        output$gauge <- renderGauge({ 
            gauge(round(as.numeric(paste(predict(model,t))), digits=0), min = 0, max = 100, symbol = '%', gaugeSectors(
                success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
            ))
        })
        
    })
    
    output$selected_var <- renderText({
        paste(input$x,collapse =", ")
        
    })
    
    
    output$tvGenre <- renderPlot({
        wordcloud(unlist(topTV$genre),
                  colors=brewer.pal(8, "Dark2"))
    })  
    
    output$gameGenre <- renderPlot({
        top = steam%>%
            filter(total_rating>10000)%>%
            head(100)
        wordcloud(unlist(games$genres),
                  colors=brewer.pal(8, "Dark2"))
    }) 
    
    
    output$tvKey <- renderPlot({
        wordcloud(unlist(topTV$keywords), max.words = 13,
                  colors=brewer.pal(8, "Set2"))
    })  
    
    output$gameKey <- renderPlot({
        top = steam%>%
            filter(total_rating>10000)%>%
            head(100)
        wordcloud(unlist(tags$steamspy_tags), max.words = 13,
                  colors=brewer.pal(8, "Set2"))
    }) 
    
    dateRangeInput<-reactive({
        dataset <- subset(steam, year >= input$slider_year[1] & year <= input$slider_year[2])
        dataset
    })
    
    output$timeseries_year<- renderPlot({
        year1 <- dateRangeInput() %>%
            group_by(year)%>%
            summarize(count=n()) %>%
            arrange(desc(count))
        
        ggplot(year1, aes(x=as.factor(year), count)) +
            geom_bar(stat = "identity", fill = "#00008b") + labs(y = "Number of games", x = "Year") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ))
    })
    
    
    dateRangeInput2<-reactive({
        dataset <- subset(steam, month >= input$slider_month[1] & month <= input$slider_month[2])
        dataset
    })
    
    output$timeseries_month<- renderPlot({
        month1 <- dateRangeInput2() %>%
            mutate(month_name=month.abb[month])%>%
            select(month,month_name)%>%
            group_by(month_name)%>%
            summarize(count=n()) %>%
            arrange(desc(count))
        
        ggplot(month1, aes(x=as.factor(month_name), count)) +
            geom_bar(stat = "identity", fill = "#00008b") + 
            scale_x_discrete(limits = month.abb)+
            labs(y = "Number of games", x = "Month") + theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1 ))
    })
    
    output$timeseries_compare<- renderPlot({
        year_price_play <- steam %>%
            select(price,year,median_playtime)%>%
            group_by(year)%>%
            summarize(price=mean(price),playtime=mean(median_playtime))
        
        ggplot(year_price_play, aes(x=year)) +
            geom_line(aes(y=playtime,color='playtime')) +
            geom_line(aes(y=price*100,color='price')) + 
            scale_y_continuous(sec.axis = sec_axis(~.*1/100, name = "Price"))+ 
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 45, hjust = 1 ))+
            theme(legend.position = "top")+
            labs(colour="")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
