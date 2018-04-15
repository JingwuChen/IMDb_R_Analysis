require(shiny)
require(ggplot2)
require(psych)
ui<-fluidPage(
  
  #  Application title
  titlePanel("IDMb数据分析"),
  fluidRow(
    
   div(
     h2("本次分析数据为自1990-01-01至今IMDb排名前3000的电影"),
     hr(),
     div(h3("1.电影时长和评分"),
         div("电影时长平均110.30分钟，上下浮动19分钟，
             而最短的电影为一部日本电影",span("秒速五厘米",style="color:green"),"才63分钟，是一部写实爱情动画电影。最长的电影247分钟，
             是由鬼才导演昆汀·塔伦蒂诺导演的电影",span("杀死比尔整个血腥事件",style="color:green"),"，这是一部惊悚电影，这么冗长的电影正常来讲应该很难取悦观众，
             但是本片将《杀死比尔》一、二两部串联在一起，并无传言所说的删减片段和内容增加，但在配乐和音效方面做了改动，
             使整个复杂离奇的故事更加的连贯和生动。所以最后观众反响确都不错，IMDb达到了8.8，豆瓣评分都有8.7。"),
         div("其实电影时长这些年有所变短，90年代的115分钟左右，降至2016年以来的电影时长107分钟左右了，17年电影平均只有105分钟了，
             可能这跟现代人的快餐文化有关，没有耐心，喜欢快速获取信息。"),
         div("正常来说剧情类、传记类电影较长，动作类电影基本集中在120分钟"),
         div("电影评分平均6.57，上下1分浮动，由于这些电影是1990年以来IMDb以来排名前3000的电影，所以排名较高也正常，
             最高评分的是",span("忍者蝙蝠侠",style="color:green"),"，达到9.6分，是华纳DC联合日本神风动画制作的动画电影。"),
         div("电影时长和评分之间关系不大，相关系数才0.17。这也很容易理解，电影的好坏本来就跟时长无关。")),
     hr(),
     div(h3("2.电影评分和投票"),
         div("电影投票数平均138211票，上下在82089票浮动，
             最惨的是2017年的",span("波西杰克逊与巨神的诅咒(Percy Jackson and the Titan's Curse)",style="color:green"),"只有5票，是根据书籍改编的，可能因为故事比较老套，
             演员不知名，宣传较少，所以无人问津。最高投票数的是1994年的经典电影",span("肖申克的救赎",style="color:green"),"，有1939813票，IMDB评分达到9.3分，
             列为美国电影协会世纪百部佳片中的72名。其实豆瓣评分中肖申克的救赎也有1014946票，评分9.6分，相当惊人。"),
         div("从投票直方图(Fig.5)来看16年以来投票数暴涨，互联网化造成网络活跃度高，但其实高投票数的电影反而是90年代的电影。"),
         div("评分和投票数关系不大，相关系数才0.25。但其实投票数还是能影响到评分的，有些得分很高的一些电影投票数并不高，
              就像前面IMDb得分最高的忍者蝙蝠侠，才222票，几乎可以断定都是一些小团体书迷或团体爱好者给评分的。
             高的投票数群体很大，众口难调，倾向不可能都一致，肯定评分有高有低，自然会冲淡一部分高分。")),
     hr(),
     div(h3("3.电影评分和投票"),
         div("IMDB的票房统计不是特别完善,比如最热的头号玩家北美票房超过9648万美元，但IMDb上才103.4万美元。所以参考价值不大。
             但是还是可以看出动作类的票房普遍是最高的。")),style="padding:10%;backgroud-color:#DDDDDD"
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
