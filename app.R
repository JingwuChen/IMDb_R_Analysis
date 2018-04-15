require(shiny)
require(ggplot2)
require(psych)
ui<-fluidPage(
  
  #  Application title
  titlePanel("IDMb数据分析"),
  fluidRow(
    
    pre(
      h2("1.电影时长和评分"),
    div("这是我想要的")
    )
    
    
  ),
  br(),
  hr(),
  fluidRow(
    column(4,
           h3(strong("Table1 电影数据中的描述性统计"),align = "center"), 
           
           verbatimTextOutput("summary")
           ),
    column(8,
           h3(strong("Table2 电影数据根据年份分组描述性统计"),align = "center"), 
           
           verbatimTextOutput("summary_by_year")
    )
  ),
  hr(),
  #use the fluidPage to build a bootstrap layout
  fluidRow(
    column(4,
  wellPanel(
    sliderInput("integer", "数据集包含的电影数(从IMDb排名第一倒着数):", 
                min=50, max=5000, value=100),
    br(),
    #set a option for user to choose whether to display facet
    radioButtons("isFacet","Fig.1 电影时长 vs 评分分面",list("否"="no","是"="yes"
      
    )),
    br(),
    # set a date range for you to choose 
    dateRangeInput("dates", label = "电影发行日期范围",start = "2015-01-01",end = Sys.Date(),language="zh-CN")
   
  
 )),
 
  # Show a series of plots summarizing the values entered
  column(4,
    h3(strong("Fig.1 电影时长 vs 评分"),align = "center"), 
    
    plotOutput("run_time_ratingPlot")),
 column(4,
    h3(strong("Fig.2 电影时长柱状图"),align = "center"), 
    plotOutput("run_time_Histogram")
    
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
  #get the web scarpping function 
  source("get_data.R",local = T)
  
  
  
  # Reactive expression to compose a data frame containing all of the values
  movie_df <- reactive({
    
    # Compose url string,and get the web scrapping data
    dates<-input$dates
    dates<-paste(dates,collapse = ",")
    url<-sprintf("http://www.imdb.com/search/title?count=%d&release_date=%s&title_type=feature",input$integer,dates)
    data<-get_response(url)
  })
  isFacet<-reactive({
    #facet<-switch(input$isFacet,"yes"="是",
                  #"no"="否","是")
    facet<-input$isFacet
  })
  #compose the total summary
  var<-c("run_time","rating","votes","gross_box")
  output$summary<-renderPrint(
  
   psych::describe(movie_df()[var])
  )
  #compose the summary by year
  output$summary_by_year<-renderPrint(
    
    psych::describe.by(movie_df()[var],movie_df()$year)
  )
  
  #compose the run_time vs rating plot 
  output$run_time_ratingPlot <- reactivePlot(function() {
    #build a linear regression model
    model1<-lm(run_time~rating,data = movie_df())
    l <- list(r2 = format(summary(model1)$r.squared, digits = 4),
              p_value = format(summary(model1)$coefficients[2,4], digits = 4))
    #create the R2 and p_value expression
    eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p_value, l)
    
  
    p<-ggplot(data=movie_df(),aes(x=run_time,y=rating))+
                     xlab("电影时长（分钟）")+ylab("评分")
    p<-p + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    if(isFacet()=="yes"){
    p<-p+geom_point(aes(size=votes))
    p<-p+facet_grid(.~genre)
    }
    else {
      p<-p+geom_point(aes(size=votes,color=genre))+geom_smooth(method = "lm")
      #get the R2 and p_value to show at certain spot on the plot
      p<-p+geom_text(aes(x = 150, y = 6, label = as.character(as.expression(eq))), parse = TRUE)
    }
    print(p)
  })
  #compose the run_time histogram
  output$run_time_Histogram <- reactivePlot(function() {
   p<-qplot(data =movie_df(),run_time,fill = genre,bins = 30,xlab = "电影时长（分钟）",
            ylab = "频数") 
   p<-p + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    
    print(p)
  })
  #compose the rating histogram
  output$ratingHistogram<-reactivePlot(function() {
    p1<-ggplot(data=movie_df(),aes(rating))+geom_histogram(bins=10,aes(fill=genre))+
      facet_wrap(~year,ncol = 6)+xlab("评分")+ylab("频数")
    p1<-p1 + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    print(p1)
  })
  #compose the votes vs rating scatter plot
  output$vote_rating<-reactivePlot(function() {
    #build a linear regression model
    model2<-lm(votes~rating,data = movie_df())
    l <- list(r2 = format(summary(model2)$r.squared, digits = 4),
              p_value = format(summary(model2)$coefficients[2,4], digits = 4))
    eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p_value, l)
    
    p2<-ggplot(data = movie_df(),aes(x=votes,y = rating))+geom_point(size=5,aes(color=genre))+
      geom_smooth(method = "lm")+xlab("投票")+ylab("评分")
    p2<-p2+geom_text(aes(x = 4e+05, y = 6, label = as.character(as.expression(eq))), parse = TRUE)
    p2<-p2 + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    print(p2)
  })
  #compose the vote histogram
  output$voteHistogram<-reactivePlot(function (){
    p3<-ggplot(data=movie_df(),aes(votes))+geom_histogram(bins=10,aes(fill=genre))+
      facet_wrap(~year,ncol = 6)+xlab("投票")+ylab("频数")
    p3<-p3 + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    print(p3)
  })
  #compose the gross_box vs rating scatter plot with lm model
  output$box_rating<-reactivePlot(function() {
    #build a linear regression model
    model3<-lm(gross_box~rating,data = movie_df())
    l <- list(r2 = format(summary(model3)$r.squared, digits = 4),
              p_value = format(summary(model3)$coefficients[2,4], digits = 4))
    eq <- substitute(italic(R)^2~"="~r2~","~italic(P)~"="~p_value, l)
    
    p2<-ggplot(data = movie_df(),aes(x=gross_box,y = rating))+geom_point(size=5,aes(color=genre))+
      geom_smooth(method = "lm")+xlab("北美票房(万美元）")+ylab("评分")
    p2<-p2+geom_text(aes(x = 500, y = 6, label = as.character(as.expression(eq))), parse = TRUE)
    p2<-p2 + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    print(p2)
  })
  #compose the gross_box histogram with year facet
  output$boxHistogram<-reactivePlot(function (){
    p3<-ggplot(data=movie_df(),aes(gross_box))+geom_histogram(bins=10,aes(fill=genre))+
      facet_wrap(~year,ncol = 6)+xlab("北美票房（万美元）")+ylab("频数")
    p3<-p3 + theme(axis.title=element_text(size=14),text=element_text(family="SimHei"))
    print(p3)
  })
  }

shinyApp(ui=ui,server = server)
