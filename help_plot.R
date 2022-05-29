library(jiebaR)
library(jiebaRD)
library(wordcloud2)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(reshape2)
stoppath = "./stopwords.dat"  
dictpath = "./add_dict.dat"  
cutter <- worker(bylines = TRUE, stop_word = stoppath, user = dictpath)
# 剧评 词云图
plot_wordcloud <- function(data, comment = TRUE, font, keyword, shape, size, color){
  key_num = length(keyword)
  if(comment == FALSE){
    text <- cutter[data$title]
  }
  else{
    text <- cutter[data$comment]
  }
  if(key_num > 0){
    pattern = paste(keyword, collapse = "|")
    text <- text[grep(pattern = pattern, text)]
  }
  cleared <- lapply(text, as.matrix)
  cleared <- as.data.frame(do.call(rbind, cleared), stringsAsFactors = FALSE)
  freq <- as.data.frame(table(cleared), stringsAsFactors = FALSE)
  freq <- freq[order(-freq[,2]),]
  if(key_num == 0)
    top100 <- freq[2:101,]
  else{
    top100 <- freq[2:101,]
  }
  if(comment == FALSE){
    wordcloud2(top100, size = size, fontFamily = font, shape = shape, color = color)
  }
  else{
    wordcloud2(top100, size = size, fontFamily = font, shape = shape, color = color)
  }
}
# 小说和分集剧情 词云图
plot_text_wordcloud <- function(text, novel = TRUE, from_chapter, to_chapter, font, shape, size, color){
  if(novel == TRUE){
    index = grep(paste0("第","\\d+","章"), text)
    index <- c(index, length(text) + 1)
    if(from_chapter < 1)
      from_chapter = 1
    if(to_chapter > 64)
      to_chapter = 64
  }
  else{
    index = grep(paste0("第","\\d+","集"),text)
    index <- c(index, length(text) + 1)
    if(from_chapter < 1)
      from_chapter = 1
    if(to_chapter > 15)
      to_chapter = 15
  }
  from = index[from_chapter]
  to = index[to_chapter+1] - 1
  text <- text[from:to]
  text <- cutter[text]
  cleared <- lapply(text, as.matrix)
  cleared <- as.data.frame(do.call(rbind, cleared), stringsAsFactors = FALSE)
  freq <- as.data.frame(table(cleared), stringsAsFactors = FALSE)
  freq <- freq[order(-freq[,2]),]
  top100 <- freq[1:100,]
  wordcloud2(top100, fontFamily = font, shape = shape, color = color, size = size)
}
# 剧评 词频柱状图
plot_keywc_bar10 <- function(data, comment = TRUE, keyword){
  key_num <- length(keyword)
  if(comment == FALSE){
    text <- cutter[data$title]
  }
  else{
    text <- cutter[data$comment]
  }
  if(key_num > 0){
    pattern = paste(keyword, collapse = "|")
    text <- text[grep(pattern = pattern, text)]
  }
  cleared <- lapply(text, as.matrix)
  cleared <- as.data.frame(do.call(rbind, cleared), stringsAsFactors = FALSE)
  freq <- as.data.frame(table(cleared), stringsAsFactors = FALSE)
  freq <- freq[order(-freq[,2]),]
  plot <- freq[1:10,]
  plot <- plot[plot[,"Freq"]>1,] # 去除只出现过一次的词
  plot$cleared <- factor(plot$cleared, levels = as.vector(plot[,1]))
  if(comment == FALSE){
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none")+ ggtitle("剧评标题高频词")
  }
  else{
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none")+ ggtitle("剧评内容高频词")
  }
}
# 剧评 词频柱状图 好评
plot_hp_bar10 <- function(data, comment = TRUE){
  select_data <- subset(data, star == 4 | star == 5)
  if(comment == FALSE){
    text <- cutter[select_data$title]
  }
  else{
    text <- cutter[select_data$comment]
  }
  cleared <- lapply(text, as.matrix)
  cleared <- as.data.frame(do.call(rbind, cleared), stringsAsFactors = FALSE)
  freq <- as.data.frame(table(cleared), stringsAsFactors = FALSE)
  freq <- freq[order(-freq[,2]),]
  plot <- freq[1:10,]
  plot <- plot[plot[,"Freq"]>1,]
  plot$cleared <- factor(plot$cleared, levels = as.vector(plot[,1]))
  if(comment == FALSE){
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none") + ggtitle("高评分 剧评标题高频词")
  }
  else{
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none")+ ggtitle("高评分 剧评内容高频词")
  }
}
# 剧评 词频柱状图 差评
plot_cp_bar10 <- function(data, comment = TRUE){
  select_data <- subset(data, star == 1 | star == 2)
  if(comment == FALSE){
    text <- cutter[select_data$title]
  }
  else{
    text <- cutter[select_data$comment]
  }
  cleared <- lapply(text, as.matrix)
  cleared <- as.data.frame(do.call(rbind, cleared), stringsAsFactors = FALSE)
  freq <- as.data.frame(table(cleared), stringsAsFactors = FALSE)
  freq <- freq[order(-freq[,2]),]
  plot <- freq[1:10,]
  plot <- plot[plot[,"Freq"]>1,]
  plot$cleared <- factor(plot$cleared, levels = as.vector(plot[,1]))
  if(comment == FALSE){
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none") + ggtitle("低评分 剧评标题高频词")
  }
  else{
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none")+ ggtitle("低评分 剧评内容高频词")
  }
}

plot_text_bar10 <- function(text, novel = TRUE, from_chapter, to_chapter){
  if(novel == TRUE){
    index = grep(paste0("第","\\d+","章"), text)
    index <- c(index, length(text) + 1)
    if(from_chapter < 1)
      from_chapter = 1
    if(to_chapter > 64)
      to_chapter = 64
  }
  else{
    index = grep(paste0("第","\\d+","集"), text)
    index <- c(index, length(text) + 1)
    if(from_chapter < 1)
      from_chapter = 1
    if(to_chapter > 15)
      to_chapter = 15
  }
  from = index[from_chapter]
  to = index[to_chapter+1] - 1
  text <- text[from:to]
  text <- cutter[text]
  cleared <- lapply(text, as.matrix)
  cleared <- as.data.frame(do.call(rbind, cleared), stringsAsFactors = FALSE)
  freq <- as.data.frame(table(cleared), stringsAsFactors = FALSE)
  freq <- freq[order(-freq[,2]),]
  plot <- freq[1:10,]
  plot <- plot[plot[,"Freq"]>1,]
  plot$cleared <- factor(plot$cleared, levels = as.vector(plot[,1]))
  if(novel == TRUE){
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none")+ ggtitle(paste0("小说高频词(", as.character(from_chapter),"章-",as.character(to_chapter),"章)"))
  }
  else{
    ggplot(data = plot, mapping = aes(x = cleared, y = Freq, fill = cleared, group = factor(1))) + geom_bar(stat = "identity") + labs(x = "高频词", y = "频数") + guides(fill = "none")+ ggtitle(paste0("剧情介绍高频词(", as.character(from_chapter),"集-",as.character(to_chapter),"集)"))
  }
}
# 评论发表时间段
plot_hour_count <- function(data, from_date, to_date){
  if(as.Date(from_date) < as.Date("2021-12-05"))
    from_date <- "2021-12-05"
  if(as.Date(to_date) > as.Date("2022-05-21"))
    to_date <- "2022-05-21"
  select_data <- subset(data, date >= from_date & date <= to_date)
  ggplot(select_data, aes(x = hour, fill = hour, color = hour)) + 
    geom_bar() + 
    xlab("时刻") + ylab("评论数") +
    theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) +
    ggtitle(paste0(from_date, "-", to_date))
}
# star overall
plot_starcount_overall <- function(data, type, from_date, to_date){
  if(as.Date(from_date) < as.Date("2021-12-05"))
    from_date <- "2021-12-05"
  if(as.Date(to_date) > as.Date("2022-05-21"))
    to_date <- "2022-05-21"
  select_data <- subset(data, date >= from_date & date <= to_date)
  select_data$star <- factor(select_data$star, levels = 1:5)
  if(type == "bar"){
    ggplot(data = select_data, aes(x = star,fill = star)) + geom_bar() + xlab("评星") + ylab("评论数") + guides(fill = "none") + ggtitle(paste0(from_date, "-", to_date))
  }
  else if(type == "pie"){
    ggplot(select_data, aes(x = "", fill = star))+geom_bar() + coord_polar(theta = "y", start = 0) + xlab("") + ylab("评星占比") + theme(panel.grid.major = element_blank()) + ggtitle(paste0(from_date, "-", to_date))
  }
  else if(type == "bullseye"){
    ggplot(select_data, aes(x = star, fill = star))+geom_bar() + coord_polar(theta = "y", start = 0)+ xlab("") + ylab("评星占比") + theme(panel.grid.major = element_blank()) + ggtitle(paste0(from_date, "-", to_date))
  }
  else if(type == "rose"){
    ggplot(data = select_data, aes(x = date,fill = star)) + geom_bar(position = "stack") + coord_polar() + xlab("日期") + ylab("") + scale_x_date(date_breaks = "2 days") + ggtitle(paste0(from_date, "-", to_date))
  }
}
# up_per down_per
plot_ud_point <- function(data, from_date, to_date){
  select_data <- subset(data, date >= from_date & date <= to_date)
  plot_ud <- melt(select_data[, c("date", "up_per", "down_per")], id = "date")
  colnames(plot_ud) <- c("date", "type", "value")
  ggplot(plot_ud, aes(x = date, y = value, group = type, color = type)) + geom_point() + xlab("日期") + ylab("") + theme(legend.title = element_blank())
}

plot_starcount_date <- function(data, type, from_date, to_date){
  if(as.Date(from_date) < as.Date("2021-12-05"))
    from_date <- "2021-12-05"
  if(as.Date(to_date) > as.Date("2022-05-21"))
    to_date <- "2022-05-21"
  select_data <- subset(data, date >= from_date & date <= to_date)
  select_data$star <- as.character(select_data$star)
  seg <- as.integer(difftime(as.Date(to_date), as.Date(from_date), units = "days") / 16)
  breaks <- ifelse(seg < 2, "1 day", paste0(as.character(seg), " days"))
  if(type == "stackbar"){
    # 堆叠柱状图 starcount&date
    ggplot(select_data, aes(x = date, fill = star)) + 
      geom_bar(position = "stack") + 
      scale_x_date(date_breaks = breaks) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
      xlab("日期") + ylab("评论数") + ggtitle(paste0(from_date, "-", to_date))
  }
  else if(type == "dodgebar"){
    ggplot(select_data, aes(x = date, color = star,fill = star)) + 
      geom_bar(position = "dodge") + 
      scale_x_date(date_breaks = breaks) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
      xlab("日期") + ylab("评论数")  + ggtitle(paste0(from_date, "-", to_date))
  }
  else if(type == "line"){
    melt <- as.data.frame(table(select_data$date, select_data$star))
    colnames(melt) <- c("date", "star", "count")
    melt$date <- as.Date(melt$date)
    ggplot(melt, aes(x = date, y = count, group = star, color = star, fill = star)) +
      geom_line() + 
      scale_x_date(date_breaks = breaks) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
      xlab("日期") + ylab("评论数") + ggtitle(paste0(from_date, "-", to_date))
  }
  else if(type == "smooth"){
    melt <- as.data.frame(table(select_data$date, select_data$star))
    colnames(melt) <- c("date", "star", "count")
    melt$date <- as.Date(melt$date)
    ggplot(melt, aes(x = date, y = count, group = star, color = star, fill = star)) +
      geom_smooth(formula = y ~ x, method = "loess") + 
      scale_x_date(date_breaks = breaks) + 
      theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5)) + 
      xlab("日期") + ylab("评论数") + ggtitle(paste0(from_date, "-", to_date))
  }
}
# 剧评 情感
plot_senti_overall <- function(data){
  data <- subset(data, !is.na(star))
  data$star <- as.character(data$star)
  ggplot(data, aes(x = sentiment)) + geom_histogram(aes(y = ..density..), color = "pink", fill = "white") + geom_density(alpha = 0.3, color = "black", fill = "pink") + xlab("正向情感概率(整体)")
}
plot_senti_star_density <- function(data){
  data <- subset(data, !is.na(star))
  data$star <- as.character(data$star)
  ggplot(data, aes(x = sentiment, color = star, fill = star)) +geom_density(alpha = 0.3) + xlab("正向情感概率")
}
# 小说和剧评人物箱线图
plot_box_senti <- function(data, novel = TRUE, from_chapter, to_chapter){
  
  if(novel == TRUE){
    if(from_chapter < 1)
      from_chapter = 1
    if(to_chapter > 64)
      to_chapter = 64
    select_data <- subset(data, chapter >= from_chapter & chapter <= to_chapter)
    ggplot(select_data, aes(x = person, y = sentiment, fill = person)) + 
      geom_boxplot() + 
      xlab("人物") + ylab("正向情感概率") +
      scale_fill_discrete(name = "人物") +
      stat_summary(fun = mean, geom = "point", shape = 5,size = 3) +
      ggtitle(paste0("小说(第", as.character(from_chapter), "章-第", as.character(to_chapter), "章)"))
  }
  else{
    if(from_chapter < 1)
      from_chapter = 1
    if(to_chapter > 15)
      to_chapter = 15
    select_data <- subset(data, chapter >= from_chapter & chapter <= to_chapter)
    ggplot(select_data, aes(x = person, y = sentiment, fill = person)) +
      geom_boxplot() +
      xlab("人物") + ylab("正向情感概率") +
      scale_fill_discrete(name = "人物") +
      stat_summary(fun = mean, geom = "point", shape = 5,size = 3) +
      ggtitle(paste0("分集剧情(第", as.character(from_chapter), "集-第", as.character(to_chapter), "集)"))
  }
}
# 剧评 情感随日期变化
plot_senti_date <- function(data, from_date, to_date){
  if(as.Date(from_date) < as.Date("2021-12-05"))
    from_date <- "2021-12-05"
  if(as.Date(to_date) > as.Date("2022-05-21"))
    to_date <- "2022-05-21"
  seg <- as.integer(difftime(as.Date(to_date), as.Date(from_date), units = "days") / 16)
  breaks <- ifelse(seg < 2, "1 day", paste0(as.character(seg), " days"))
  select_data <- subset(data, date >= from_date & date <= to_date & !is.na(star))
  select_data <- aggregate(select_data[,"sentiment"], by = select_data[,"date"], mean)
  ggplot(select_data, aes(x = date, y = sentiment)) +
    geom_line(color = "deepskyblue") + 
    scale_x_date(date_breaks = breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
    xlab("日期") + ylab("正向情感概率") 
}