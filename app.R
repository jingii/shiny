source("help_plot.R", encoding = "UTF-8")
# setwd("D:/Sophmore2/stat app/project")
library(shiny)
library(tidyverse)
library(lubridate)
library(jiebaR)
library(jiebaRD)
library(wordcloud2)
library(ggplot2)
library(reshape2)

namelist = c("不选定关键词","肖鹤云/白敬亭", "李诗情/赵今麦","导演/孙墨龙/刘洪源/算","司机/王兴德","陶映红/锅姨","卢迪","张警官","杜局","江警官","王萌萌","人性","循环","烂尾","剧情")
shapelist = c("circle", "cardioid", "triangle-forward", "triangle", "pentagon", "star")
colorlist = c("random-dark", "random-light")
textlist = c("剧评标题","剧评内容","小说文本","分集剧情文本")
fontlist = c("黑体", "楷体", "微软雅黑", "宋体", "华文新魏")
starchoose = c("不选定星级", "四星/五星(好评)", "一星/二星(差评)")

# load and process data
data <- read_csv("./data/data_with_senti.csv")
data$star <- suppressWarnings(as.integer(data$star))
data$star <- suppressWarnings(data$star / 10)
data$up_per <-  suppressWarnings(data$up / (data$up + data$down))
data$down_per <-  suppressWarnings(data$down / (data$up + data$down))
data$date <- as.Date(data$time) # 剧评发表日期
data$month <- month(data$time)  # 剧评发表月份
# 剧评发表时间段
for(i in 1:nrow(data)){  
  hour <- hour(data[i, 'time'][[1]])
  if(hour < 9)
    data[i, 'hour'] <- paste0('0', as.character(hour), ':00-0', as.character(hour+1), ':00')
  else if(hour == 9)
    data[i, 'hour'] <- '09:00-10:00'
  else
    data[i, 'hour'] <- paste0(as.character(hour), ':00-', as.character(hour+1), ':00')
}
plot_star <- dplyr::filter(data, !is.na(star))
novel <- readLines("./data/novel.txt", encoding = "UTF-8")
juqing <- readLines("./data/juqing.txt", encoding = "UTF-8")
novel_allsenti <- read_csv("./data/novel_overall_senti.csv")
juqing_allsenti <- read_csv("./data/juqing_overall_senti.csv")
novel_person_senti <- read_csv("./data/novel_person_senti.csv")
juqing_person_senti <- read_csv("./data/juqing_person_senti.csv")



tab1 <- tabPanel(h6("词云图"),
                 sidebarLayout(
                   sidebarPanel(p("展示剧评、小说和分集剧情的文本生成的",strong("词云图"),"。",class="text-primary"),
                                p("其中剧评可以选定",strong("关键词"),"，用涉及关键词的文本生成词云图，小说和分集剧情可以选择",strong("章节或剧集范围"),"。",class="text-muted"),
                                p("注意：词云图不显示可能是因为字体大小设置过大。选中个别关键词可能导致相关词过少，无法生成词云图。",class="text-danger"),
                                sliderInput("size", "字体大小",value = 0.5, max = 1,min = 0, step = 0.1),
                                selectInput("shape", "词云图形状", shapelist),
                                selectInput("color", "词云图颜色", colorlist),
                                selectInput("font", "词云图字体", fontlist)
                                
                     
                   ),
                   mainPanel("词云图",
                             tabsetPanel(
                               tabPanel("剧评标题",
                                        selectInput("keyword1", "关键词", namelist),
                                        wordcloud2Output("wordcloud1")
                               ),
                               tabPanel("剧评内容",
                                        selectInput("keyword2", "关键词", namelist),
                                        wordcloud2Output("wordcloud2")
                               ),
                               tabPanel("小说文本",
                                        sliderInput("chapterrange", "章节范围", value = c(1,64), max = 64, min = 1, step = 1),
                                        wordcloud2Output("wordcloud3")
                               ),
                               tabPanel("分集剧情文本",
                                        sliderInput("videorange", "剧集范围", value = c(1,15), max = 15, min = 1, step = 1),
                                        wordcloud2Output("wordcloud4")
                               )
                             )
                               
                             
                             )
                   
                 )
)
datalist <- c("data_with_senti.csv","novel_overall_senti.csv","juqing_overall_senti.csv","novel_person_senti.csv","juqing_person_senti.csv")
tab2 <- tabPanel(h6("数据"),
                 tabsetPanel(
                   tabPanel("剧评数据",
                            downloadButton("download1","下载数据"),
                            dataTableOutput("data1")
                            ),
                   tabPanel("小说整体情感",
                            downloadButton("download2","下载数据"),
                            dataTableOutput("data2")
                   ),
                   tabPanel("分集剧情整体情感",
                            downloadButton("download3","下载数据"),
                            dataTableOutput("data3")
                   ),
                   tabPanel("小说人物情感",
                            downloadButton("download4","下载数据"),
                            dataTableOutput("data4")
                   ),
                   tabPanel("分集剧情人物情感",
                              downloadButton("download5","下载数据"),
                              dataTableOutput("data5")
                   )
                 )
)

tab3 <- tabPanel(h6("词频柱状图"),
                 sidebarLayout(
                   sidebarPanel(p("展示剧评、小说和分集剧情的",strong("文本高频词"),"。",class="text-primary"),
                                p("其中剧评可以选定",strong("关键词"),"和",strong("评星高低"),"，小说和分集剧情可以选择",strong("章节"),"或",strong("剧集范围"),"。",class="text-muted")
                   ),
                   mainPanel("词频柱状图",
                             tabsetPanel(
                               tabPanel("剧评标题",
                                        fluidRow(
                                          column(6,
                                                 selectInput("bar_keyword1", "关键词", namelist)
                                                 ),
                                          column(6,
                                                 selectInput("bar_star1", "评论评星", starchoose)
                                                 )
                                        ),
                                        fluidRow(
                                          column(10,
                                                 plotOutput("bar1")
                                                 )
                                        )
                               ),
                               tabPanel("剧评内容",
                                        fluidRow(
                                          column(6,
                                                 selectInput("bar_keyword2", "关键词", namelist)
                                          ),
                                          column(6,
                                                 selectInput("bar_star2", "评论评星", starchoose)
                                          )
                                        ),
                                        fluidRow(
                                          column(10,
                                                 plotOutput("bar2")
                                          )
                                        )
                               ),
                               tabPanel("小说文本",
                                        sliderInput("bar_chapterrange", "章节范围", value = c(1,64), max = 64, min = 1, step = 1),
                                        fluidRow(
                                          column(10,
                                                 plotOutput("bar3")
                                                 )
                                        )
                                        
                                        
                               ),
                               tabPanel("分集剧情文本",
                                        sliderInput("bar_videorange", "剧集范围", value = c(1,15), max = 15, min = 1, step = 1),
                                        fluidRow(
                                          column(10,
                                                 plotOutput("bar4")
                                          )
                                        )   
                               )
                             )
                   )
                 )
)

star_typelist = c("饼图", "柱状图", "牛眼图", "玫瑰图")
count_typelist = c("堆叠柱状图","柱状图","折线图","光滑曲线图")
tab4 <- tabPanel(h6("剧评分析"),
                 sidebarLayout(sidebarPanel(p("展示剧评中",strong("非文本的数据"),"。",class="text-primary"),
                                            p("可以自定义日期范围，从而可以看出数据随电视剧播出后不同时期的情况。",class="text-muted"),
                                            dateRangeInput("daterange1", "日期范围", start = "2021-12-05", end = "2022-05-21")
                               ),
                               mainPanel("剧评分析",
                                         tabsetPanel(
                                           tabPanel("发表时间段",
                                                    plotOutput("hourcount")
                                                    ),
                                           tabPanel("评星整体分布",
                                                    selectInput("star_plottype", "图像类型", star_typelist),
                                                    plotOutput("star_overall")
                                                    ),
                                           tabPanel("相对up和down",
                                                    plotOutput("udplot")
                                                    ),
                                           tabPanel("评论数",
                                                    selectInput("count_plottype","图像类型",count_typelist),
                                                    plotOutput("count_with_star"))
                                         )
                            ) 
                 ),                 
)

senti_textlist1 <- c("剧评内容(分星级)","剧评内容","小说文本","分集剧情文本")
senti_textlist2 <- c("小说文本","分集剧情文本")
tab5 <- tabPanel(h6("情感分析"),
                 sidebarLayout(sidebarPanel(p("展示剧评、小说和分集剧情相关的",strong("情感分析"),"。",class="text-primary"),
                                            p("利用python中的",code("SnowNLP"), "库，计算每条剧评的正向情感概率和小说、分集剧情中每句话的正向情感概率。",class="text-muted"),
                                            p("描述人物情感的方法为找出涉及人物所在句子，用这些句子统计。",class="text-muted")
                                            ),
                               mainPanel("情感分析",
                                         tabsetPanel(
                                           tabPanel("整体情感分布",
                                                       selectInput("choose_text1","选择文本",senti_textlist1),
                                                       plotOutput("senti_overall")
                                                       ),
                                           tabPanel("人物情感分布",
                                                    fluidRow(
                                                      column(6,
                                                             selectInput("choose_text2","选择文本",senti_textlist2)
                                                             ),
                                                      column(6,
                                                             sliderInput("senti_chapterrange", "章节/剧集范围", value = c(1,64), max = 64, min = 1, step = 1)
                                                             )
                                                    ),
                                                       plotOutput("senti_person")
                                                       ),
                                           tabPanel("剧评情感随时间变化",
                                                       dateRangeInput("daterange2", "日期范围", start = "2021-12-05", end = "2022-05-21"),
                                                       plotOutput("senti_date")
                                                       ),
                                           tabPanel("小说/分集剧情中情感随情节发展变化",
                                                    selectInput("choose_text3","选择文本",senti_textlist2),
                                                    imageOutput("photo")
                                                       )
                                         )
                                    )
                    ),         
)
  
tab6 <- tabPanel(h6("关于网站"),
                 fluidRow(h3("关于网站")),
                 fluidRow(
                          column(7,
                                 br(),
                                 p("《开端》是由东阳正午阳光影视有限公司出品，孙墨龙、刘洪源、算共同执导，由白敬亭、赵今麦领衔主演，刘奕君特别出演，刘涛友情出演，黄觉、刘丹等主演的时间循环短剧。于2022年1月11日在腾讯视频独播。"),
                                 br(),
                                 p("该剧改编自祈祷君的同名小说，讲述了游戏架构师“肖鹤云”和在校大学生“李诗情”遭遇公交车爆炸后死而复生，在时间循环中并肩作战，努力阻止爆炸、寻找真相的故事。"),
                                 br(),
                                 p("本网站用于展示与《开端》相关的数据分析，包括电视剧剧评、小说原文和电视剧分集剧情。")
                                 ),
                          column(5,
                                 imageOutput("kaiduan")
                                 )
                   
                 ),
                 fluidRow(h3("参考来源")),
                 br(),
                 p(a("开端（2022年白敬亭、赵今麦主演的电视剧）_百度百科",href="https://baike.baidu.com/item/%E5%BC%80%E7%AB%AF/56787751")),
                 p(a("开端 (豆瓣)", href="https://movie.douban.com/subject/35332289/")),
                 p(a("开端全文阅读_笔下文学", href="https://m.bxwxorg.com/read/189539/")),
                 fluidRow(h3("作者")),
                 br(),
                 p("Jingi"),
                 p("Contact: jingi@mail.ustc.edu.cn ")
)

tab7 <- tabPanel(h6("人物出场频率"),
                 sidebarLayout(
                   sidebarPanel(p("展示剧评、小说和分集剧情中",strong("各个人物的出场频率"),"。",class="text-primary")      
                   ),
                   mainPanel("矩阵树图",
                             tabsetPanel(
                               tabPanel("剧评标题",
                                        fluidRow(
                                          column(10,
                                                 imageOutput("treemap1")
                                          )
                                        )
                               ),
                               tabPanel("剧评内容",
                                        fluidRow(
                                          column(10,
                                                 imageOutput("treemap2")
                                          )
                                        )
                               ),
                               tabPanel("小说文本",
                                        fluidRow(
                                          column(10,
                                                 imageOutput("treemap3")
                                          )
                                        )
                               ),
                               tabPanel("分集剧情文本",
                                        fluidRow(
                                          column(10,
                                                 imageOutput("treemap4")
                                          )
                                        ) 
                               )
                             )
                   )
                 )
)

ui <- bootstrapPage(
  theme = bslib::bs_theme(bootswatch = "yeti"),
  navbarPage(h6("《开端》数据分析"),
             tab1,
             tab3,
             tab7,
             tab4,
             tab5,
             tab2,
             tab6
  )
)

server <- function(input, output, session) {
  output$data1 <- renderDataTable(data, options = list(pageLength = 5))
    
  output$data2 <- renderDataTable(novel_allsenti, options = list(pageLength = 5))
  
  output$data3 <- renderDataTable(juqing_allsenti, options = list(pageLength = 5))

  output$data4 <- renderDataTable(novel_person_senti, options = list(pageLength = 5))

  output$data5 <- renderDataTable(juqing_person_senti, options = list(pageLength = 5))
  
  
  output$download1 <- downloadHandler(
    filename = "data_with_senti.csv" ,
    content = function(file){
        vroom::vroom_write(data, file)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = "novel_overall_senti.csv" ,
    content = function(file){
      vroom::vroom_write(novel_allsenti, file)
    }
  )
  
  output$download3 <- downloadHandler(
    filename = "juqing_overall_senti.csv" ,
    content = function(file){
      vroom::vroom_write(juqing_allsenti, file)
    }
  )
  
  output$download4 <- downloadHandler(
    filename = "novel_person_senti.csv" ,
    content = function(file){
      vroom::vroom_write(novel_person_senti, file)
    }
  )
  
  output$download5 <- downloadHandler(
    filename = "juqing_person_senti.csv" ,
    content = function(file){
      vroom::vroom_write(juqing_person_senti, file)
    }
  )
  
  output$wordcloud1 <- renderWordcloud2({
    if(input$keyword1=="不选定关键词"){
      keyword = c()
    }
    else if(input$keyword1=="肖鹤云/白敬亭"){
      keyword = c("肖鹤云", "白敬亭","男主")
    }
    else if(input$keyword1=="李诗情/赵今麦"){
      keyword = c("李诗情", "赵今麦","女主")
    }
    else if(input$keyword1=="导演/孙墨龙/刘洪源/算"){
      keyword = c("导演","孙墨龙","刘洪源","算")
    }
    else if(input$keyword1=="司机/王兴德"){
      keyword = c("司机", "王兴德","黄觉")
    }
    else if(input$keyword1=="陶映红/锅姨"){
      keyword = c("陶映红", "锅姨","刘丹")
    }
    else if(input$keyword1=="卢迪"){
      keyword = c("卢迪", "猫之使徒","被光选中的人","哮喘征服者","卢笛","曾柯琅")
    }
    else if(input$keyword1=="张警官"){
      keyword = c("张成", "刘奕君","张警官")
    }
    else if(input$keyword1=="杜局"){
      keyword = c("杜警官", "杜局","刘涛")
    }
    else if(input$keyword1=="江警官"){
      keyword = c("江警官", "江枫","小江")
    }
    else{
      keyword = c(input$keyword1)
    }
    plot_wordcloud(data, comment = FALSE, input$font, keyword, input$shape, input$size, input$color)
  })
  
  output$wordcloud2 <- renderWordcloud2({
    if(input$keyword2=="不选定关键词"){
      keyword = c()
    }
    else if(input$keyword2=="肖鹤云/白敬亭"){
      keyword = c("肖鹤云", "白敬亭","男主")
    }
    else if(input$keyword2=="李诗情/赵今麦"){
      keyword = c("李诗情", "赵今麦","女主")
    }
    else if(input$keyword2=="导演/孙墨龙/刘洪源/算"){
      keyword = c("导演","孙墨龙","刘洪源","算")
    }
    else if(input$keyword2=="司机/王兴德"){
      keyword = c("司机", "王兴德","黄觉")
    }
    else if(input$keyword2=="陶映红/锅姨"){
      keyword = c("陶映红", "锅姨","刘丹")
    }
    else if(input$keyword2=="卢迪"){
      keyword = c("卢迪", "猫之使徒","被光选中的人","哮喘征服者","卢笛","曾柯琅")
    }
    else if(input$keyword2=="张警官"){
      keyword = c("张成", "刘奕君","张警官")
    }
    else if(input$keyword2=="杜局"){
      keyword = c("杜警官", "杜局","刘涛")
    }
    else if(input$keyword2=="江警官"){
      keyword = c("江警官", "江枫","小江")
    }
    else{
      keyword = c(input$keyword2)
    }
    plot_wordcloud(data, comment = TRUE, input$font, keyword, input$shape, input$size, input$color)
  })
  
  output$wordcloud3 <- renderWordcloud2({
    from_chapter <- input$chapterrange[1]
    to_chapter <- input$chapterrange[2]
    plot_text_wordcloud(novel, novel = TRUE, from_chapter, to_chapter, input$font, input$shape, input$size, input$color)
  })
  
  output$wordcloud4 <- renderWordcloud2({
    from_chapter <- input$videorange[1]
    to_chapter <- input$videorange[2]
    plot_text_wordcloud(juqing, novel = FALSE, from_chapter, to_chapter, input$font, input$shape, input$size, input$color)
  })
  
  output$bar1 <- renderPlot({
    if(input$bar_keyword1=="不选定关键词"){
      keyword = c()
    }
    else if(input$bar_keyword1=="肖鹤云/白敬亭"){
      keyword = c("肖鹤云", "白敬亭","男主")
    }
    else if(input$bar_keyword1=="李诗情/赵今麦"){
      keyword = c("李诗情", "赵今麦","女主")
    }
    else if(input$bar_keyword1=="导演/孙墨龙/刘洪源/算"){
      keyword = c("导演","孙墨龙","刘洪源","算")
    }
    else if(input$bar_keyword1=="司机/王兴德"){
      keyword = c("司机", "王兴德","黄觉")
    }
    else if(input$bar_keyword1=="陶映红/锅姨"){
      keyword = c("陶映红", "锅姨","刘丹")
    }
    else if(input$bar_keyword1=="卢迪"){
      keyword = c("卢迪", "猫之使徒","被光选中的人","哮喘征服者","卢笛","曾柯琅")
    }
    else if(input$bar_keyword1=="张警官"){
      keyword = c("张成", "刘奕君","张警官")
    }
    else if(input$bar_keyword1=="杜局"){
      keyword = c("杜警官", "杜局","刘涛")
    }
    else if(input$bar_keyword1=="江警官"){
      keyword = c("江警官", "江枫","小江")
    }
    else{
      keyword = c(input$bar_keyword1)
    }
    if(input$bar_star1 == "不选定星级")
      plot_keywc_bar10(data, comment = FALSE, keyword)
    
    else if(input$bar_star1 == "四星/五星(好评)")
      plot_hp_bar10(data, comment = FALSE)
    
    else if(input$bar_star1 == "一星/二星(差评)"){
      plot_cp_bar10(data, comment = FALSE)
    }
  })

  output$bar2 <- renderPlot({
    if(input$bar_keyword2=="不选定关键词"){
      keyword = c()
    }
    else if(input$bar_keyword2=="肖鹤云/白敬亭"){
      keyword = c("肖鹤云", "白敬亭","男主")
    }
    else if(input$bar_keyword2=="李诗情/赵今麦"){
      keyword = c("李诗情", "赵今麦","女主")
    }
    else if(input$bar_keyword2=="导演/孙墨龙/刘洪源/算"){
      keyword = c("导演","孙墨龙","刘洪源","算")
    }
    else if(input$bar_keyword2=="司机/王兴德"){
      keyword = c("司机", "王兴德","黄觉")
    }
    else if(input$bar_keyword2=="陶映红/锅姨"){
      keyword = c("陶映红", "锅姨","刘丹")
    }
    else if(input$bar_keyword2=="卢迪"){
      keyword = c("卢迪", "猫之使徒","被光选中的人","哮喘征服者","卢笛","曾柯琅")
    }
    else if(input$bar_keyword2=="张警官"){
      keyword = c("张成", "刘奕君","张警官")
    }
    else if(input$bar_keyword2=="杜局"){
      keyword = c("杜警官", "杜局","刘涛")
    }
    else if(input$bar_keyword2=="江警官"){
      keyword = c("江警官", "江枫","小江")
    }
    else{
      keyword = c(input$bar_keyword2)
    }
    if(input$bar_star2 == "不选定星级"){
      plot_keywc_bar10(data, comment = TRUE, keyword)
    }
    else if(input$bar_star2 == "四星/五星(好评)"){
      plot_hp_bar10(data, comment = TRUE)
    }
    else if(input$bar_star2 == "一星/二星(差评)"){
      plot_cp_bar10(data, comment = TRUE)
    }
  })
  
  output$bar3 <- renderPlot({
    from_chapter <- input$bar_chapterrange[1]
    to_chapter <- input$bar_chapterrange[2]
    plot_text_bar10(novel, novel = TRUE, from_chapter, to_chapter)
  })
  
  output$bar4 <- renderPlot({
    from_chapter <- input$bar_videorange[1]
    to_chapter <- input$bar_videorange[2]
    plot_text_bar10(juqing, novel = FALSE, from_chapter, to_chapter)
  })
  
  output$hourcount <- renderPlot({
    plot_hour_count(data, input$daterange1[1], input$daterange1[2])
  })
  
  output$star_overall <- renderPlot({
    if(input$star_plottype == "饼图"){
      type = "pie"
    }
    else if(input$star_plottype == "柱状图"){
      type = "bar"
    }
    else if(input$star_plottype == "牛眼图"){
      type = "bullseye"
    }
    else if(input$star_plottype == "玫瑰图"){
      type = "rose"
    }
    plot_starcount_overall(plot_star, type, input$daterange1[1], input$daterange1[2])
  })
  
  output$udplot <- renderPlot({
    plot_ud_point(data, input$daterange1[1], input$daterange1[2])
  })
  
  output$count_with_star <- renderPlot({
    if(input$count_plottype == "堆叠柱状图"){
      type = "stackbar"
    }
    else if(input$count_plottype == "柱状图"){
      type = "dodgebar"
    }
    else if(input$count_plottype == "折线图"){
      type = "line"
    }
    else if(input$count_plottype == "光滑曲线图"){
      type = "smooth"
    }
    plot_starcount_date(plot_star, type, input$daterange1[1], input$daterange1[2])
  })
  
  output$senti_overall <- renderPlot({
    if(input$choose_text1 == "剧评内容"){
      plot_senti_overall(data)
    }
    else if(input$choose_text1 == "剧评内容(分星级)"){
      plot_senti_star_density(data)
    }
    else if(input$choose_text1 == "小说文本"){
      ggplot(novel_allsenti, aes(x = sentiment)) + geom_histogram(aes(y = ..density..), color = "pink", fill = "white") + geom_density(alpha = 0.3, color = "black", fill = "pink") + xlab("正向情感概率(小说 整体)")
    }
    else if(input$choose_text1 == "分集剧情文本"){
      ggplot(juqing_allsenti, aes(x = sentiment)) + geom_histogram(aes(y = ..density..), color = "pink", fill = "white") + geom_density(alpha = 0.3, color = "black", fill = "pink") + xlab("正向情感概率(分集剧情 整体)")
    }
  })
  
  output$senti_person <- renderPlot({
    if(input$choose_text2 == "小说文本"){
      plot_box_senti(novel_person_senti, TRUE, input$senti_chapterrange[1], input$senti_chapterrange[2])
    }
    else if(input$choose_text2 == "分集剧情文本"){
      plot_box_senti(juqing_person_senti, FALSE, input$senti_chapterrange[1], input$senti_chapterrange[2])
    }
  })
  
  output$senti_date <- renderPlot({
    plot_senti_date(data, input$daterange2[1], input$daterange2[2])
  })
  
  output$photo <- renderImage({
    if(input$choose_text3 == "小说文本"){
      list(
        src = file.path("./image/Sentiment of novel.png"),
        width = 600,
        height = 400
        )
    }
    else if(input$choose_text3 == "分集剧情文本"){
      list(
        src = file.path("./image/Sentiment of juqing.png"),
        width = 600,
        height = 400
      )
    }
  })
  
  output$kaiduan <- renderImage({
    list(
      src = file.path("./image/kaiduan.webp"),
      contentType = "image/webp",
      width = 400,
      height = 600
    )
  })
  
  output$treemap1 <- renderImage({
    list(
      src = file.path("./image/treemap_comment.png"),
      width = 600,
      height = 400
    )
  })
  
  output$treemap2 <- renderImage({
    list(
      src = file.path("./image/treemap_title.png"),
      width = 600,
      height = 400
    )
  })
  
  output$treemap3 <- renderImage({
    list(
      src = file.path("./image/treemap_novel.png"),
      width = 600,
      height = 400
    )
  })
  
  output$treemap4 <- renderImage({
    list(
      src = file.path("./image/treemap_juqing.png"),
      width = 600,
      height = 400
    )
  })
  
  
}

shinyApp(ui, server)
