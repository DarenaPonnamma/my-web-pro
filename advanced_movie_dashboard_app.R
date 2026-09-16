
# Advanced Movie Analytics Dashboard (app.R)

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)

set.seed(123)

movies <- data.frame(
  Title = paste("Movie", 1:100),
  Language = sample(c("English","Hindi","Kannada","Tamil","Telugu","Malayalam","Korean","Japanese"),100,TRUE),
  Genre = sample(c("Action","Drama","Comedy","Thriller","Sci-Fi","Romance"),100,TRUE),
  Year = sample(1980:2025,100,TRUE),
  Rating = round(runif(100,6,9.5),1),
  Votes = sample(10000:5000000,100,TRUE),
  stringsAsFactors = FALSE
)

ui <- dashboardPage(
 dashboardHeader(title="Movie Analytics Dashboard"),
 dashboardSidebar(
   textInput("search","Search Movie"),
   selectInput("language","Language",c("All",sort(unique(movies$Language)))),
   selectInput("genre","Genre",c("All",sort(unique(movies$Genre)))),
   sliderInput("rating","Rating",0,10,c(6,10)),
   sliderInput("year","Year",min(movies$Year),max(movies$Year),
               c(min(movies$Year),max(movies$Year)))
 ),
 dashboardBody(
   fluidRow(
     valueBoxOutput("moviesBox",3),
     valueBoxOutput("ratingBox",3),
     valueBoxOutput("votesBox",3),
     valueBoxOutput("langBox",3)
   ),
   fluidRow(
     box(width=6,plotlyOutput("genrePlot")),
     box(width=6,plotlyOutput("languagePlot"))
   ),
   fluidRow(
     box(width=6,plotlyOutput("topMovies")),
     box(width=6,plotlyOutput("ratingDist"))
   ),
   fluidRow(
     box(width=12,plotlyOutput("yearTrend"))
   ),
   fluidRow(
     box(width=12,DTOutput("movieTable"))
   ),
   fluidRow(
     box(width=12,downloadButton("downloadData","Download Filtered Data"))
   )
 )
)

server <- function(input,output){

 filtered <- reactive({
   df <- movies

   if(input$language!="All")
     df <- df[df$Language==input$language,]

   if(input$genre!="All")
     df <- df[df$Genre==input$genre,]

   df <- df[df$Rating >= input$rating[1] & df$Rating <= input$rating[2],]
   df <- df[df$Year >= input$year[1] & df$Year <= input$year[2],]

   if(nchar(input$search)>0)
     df <- df[grepl(input$search,df$Title,ignore.case=TRUE),]

   df
 })

 output$moviesBox <- renderValueBox(
   valueBox(nrow(filtered()),"Movies")
 )

 output$ratingBox <- renderValueBox(
   valueBox(round(mean(filtered()$Rating),2),"Average Rating")
 )

 output$votesBox <- renderValueBox(
   valueBox(format(sum(filtered()$Votes),big.mark=","),"Total Votes")
 )

 output$langBox <- renderValueBox(
   valueBox(length(unique(filtered()$Language)),"Languages")
 )

 output$genrePlot <- renderPlotly({
   p <- ggplot(filtered(),aes(Genre))+geom_bar()
   ggplotly(p)
 })

 output$languagePlot <- renderPlotly({
   p <- ggplot(filtered(),aes(Language))+geom_bar()
   ggplotly(p)
 })

 output$topMovies <- renderPlotly({
   top <- filtered() |> arrange(desc(Rating)) |> head(15)
   p <- ggplot(top,aes(reorder(Title,Rating),Rating))+geom_col()+coord_flip()
   ggplotly(p)
 })

 output$ratingDist <- renderPlotly({
   p <- ggplot(filtered(),aes(Rating))+geom_histogram(bins=10)
   ggplotly(p)
 })

 output$yearTrend <- renderPlotly({
   yr <- filtered() |> group_by(Year) |> summarise(Movies=n())
   p <- ggplot(yr,aes(Year,Movies))+geom_line()+geom_point()
   ggplotly(p)
 })

 output$movieTable <- renderDT({
   datatable(filtered(),options=list(pageLength=15))
 })

 output$downloadData <- downloadHandler(
     filename=function(){"filtered_movies.csv"},
     content=function(file){
       write.csv(filtered(),file,row.names=FALSE)
     }
   )
}

shinyApp(ui,server)
