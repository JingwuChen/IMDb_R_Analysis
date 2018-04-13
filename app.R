require(shiny)
require(ggplot2)

ui<-fluidPage(
  
  #  Application title
  headerPanel("IDMb数据爬虫分析演示"),
  
  # Sidebar with sliders that demonstrate various available options
  
  sidebarPanel(
    sliderInput("integer", "Number of films including:", 
                min=50, max=1000, value=100),
    br(),
    radioButtons("isFacet","电影时长 vs 评分分面",list("是"="yes","否"="no"
      
    )),
    # set a date range for you to choose 
    dateRangeInput("dates", label = "电影日期范围",start = "2015-01-01",end = Sys.Date(),language="zh-CN")
   
  
 ),
  # Show a plot summarizing the values entered
  mainPanel(
    #verbatimTextOutput("value"), 
    h3(strong("电影时长 vs 评分"),align = "center"), 
    
    plotOutput("run_time_ratingPlot") ,
    h3(strong("电影时长柱状图"),align = "center"), 
    plotOutput("run_time_barPlot")
    
  )
)



server<-function(input, output) {
  source("get_data.R",local = T)
  
  
  
  # Reactive expression to compose a data frame containing all of the values
  movie_df <- reactive({
    
    # Compose data frame
    dates<-input$dates
    dates<-paste(dates,collapse = ",")
    url<-sprintf("http://www.imdb.com/search/title?count=%d&release_date=%s&title_type=feature",input$integer,dates)
    data<-get_response(url)
    #data_chosen<-data[grep("Drama",input$genre),]
  })
  isFacet<-reactive({
    #facet<-switch(input$isFacet,"yes"="是",
                  #"no"="否","是")
    facet<-input$isFacet
  })
  
  # Show the values using an HTML plot
  #output$value <- renderPrint({dateRange() })
  
  output$run_time_ratingPlot <- reactivePlot(function() {
    p<-ggplot(data=movie_df(),aes(x=run_time,y=rating))+
                     xlab("电影时长（分钟）")+ylab("评分")
    if(isFacet()=="yes"){
    p<-p+geom_point(aes(size=votes))
    p<-p+facet_grid(.~genre)
    }
    else {
      p<-p+geom_point(aes(size=votes,color=genre))
    }
    print(p)
  })
  
  output$run_time_barPlot <- reactivePlot(function() {
   p<-qplot(data =movie_df(),run_time,fill = genre,bins = 30,xlab = "电影时长（分钟）",
            ylab = "频数") 
    
    print(p)
  })
  }

shinyApp(ui=ui,server = server)
