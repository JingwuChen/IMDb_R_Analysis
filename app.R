require(shiny)
require(ggplot2)

ui<-fluidPage(
  
  #  Application title
  titlePanel("IDMb数据爬虫分析演示"),
  
  # Sidebar with sliders that demonstrate various available options
  fluidRow(
    column(4,
  wellPanel(
    sliderInput("integer", "数据集包含的电影数(从IMDb排名第一倒着数):", 
                min=50, max=5000, value=100),
    br(),
    radioButtons("isFacet","Fig.1 电影时长 vs 评分分面",list("否"="no","是"="yes"
      
    )),
    br(),
    # set a date range for you to choose 
    dateRangeInput("dates", label = "电影发行日期范围",start = "2015-01-01",end = Sys.Date(),language="zh-CN")
   
  
 )),
 
  # Show a plot summarizing the values entered
  column(4,
    h3(strong("Fig.1 电影时长 vs 评分"),align = "center"), 
    
    plotOutput("run_time_ratingPlot")),
 column(4,
    h3(strong("Fig.2 电影时长柱状图"),align = "center"), 
    plotOutput("run_time_barPlot")
    
  )
  ),
 hr(),
 fluidRow(
   column(4,
  h3(strong("Fig.4 投票 vs 评分"),align = "center"),
   plotOutput("vote_rating")),
   column(8,
          h3(strong("Fig.5 投票直方图"),align = "center"),
          plotOutput("voteHistogram")
          )
   
 ),
 
 hr(),
 fluidRow(
   h3(strong("Fig.6 评分直方图"),align = "center"),
   
   plotOutput("ratingHistogram")
 ),
 hr(),
 fluidRow(
   column(4,
          h3(strong("Fig.7 票房 vs 评分"),align = "center"),
          plotOutput("box_rating")),
   column(8,
          h3(strong("Fig.8 票房直方图"),align = "center"),
          plotOutput("boxHistogram")
   )
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
      p<-p+geom_point(aes(size=votes,color=genre))+geom_smooth(method = "lm")
    }
    print(p)
  })
  
  output$run_time_barPlot <- reactivePlot(function() {
   p<-qplot(data =movie_df(),run_time,fill = genre,bins = 30,xlab = "电影时长（分钟）",
            ylab = "频数") 
    
    print(p)
  })
  output$ratingHistogram<-reactivePlot(function() {
    p1<-ggplot(data=movie_df(),aes(rating))+geom_histogram(bins=10,aes(fill=genre))+
      facet_wrap(~year,ncol = 6)+xlab("评分")+ylab("频数")
    print(p1)
  })
  output$vote_rating<-reactivePlot(function() {
    p2<-ggplot(data = movie_df(),aes(x=votes,y = rating))+geom_point(size=5,aes(color=genre))+
      geom_smooth(method = "lm")+xlab("投票")+ylab("评分")
    print(p2)
  })
  output$voteHistogram<-reactivePlot(function (){
    p3<-ggplot(data=movie_df(),aes(votes))+geom_histogram(bins=10,aes(fill=genre))+
      facet_wrap(~year,ncol = 6)+xlab("投票")+ylab("频数")
    print(p3)
  })
  output$box_rating<-reactivePlot(function() {
    p2<-ggplot(data = movie_df(),aes(x=gross_box,y = rating))+geom_point(size=5,aes(color=genre))+
      geom_smooth(method = "lm")+xlab("北美票房(万美元）")+ylab("评分")
    print(p2)
  })
  output$boxHistogram<-reactivePlot(function (){
    p3<-ggplot(data=movie_df(),aes(gross_box))+geom_histogram(bins=10,aes(fill=genre))+
      facet_wrap(~year,ncol = 6)+xlab("北美票房（万美元）")+ylab("频数")
    print(p3)
  })
  }

shinyApp(ui=ui,server = server)
