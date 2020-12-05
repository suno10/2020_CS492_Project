###############-------------------------------------------###############
###############            20170459 & 20170616            ###############
###############                                           ###############
###############               CS492 Project               ###############
###############-------------------------------------------###############

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(wordcloud)

# Please download KRvideos file first.
KRvideos <- read_csv("Downloads/KRvideos.csv")
View(KRvideos)
colnames(KRvideos)
summary(KRvideos)

# percentage of null values
sum(is.na(KRvideos)) / dim(KRvideos)[1] / dim(KRvideos)[2] * 100

###############     1.  Pre-processing     ##############

KRvideos$trending_date <- ydm(KRvideos$trending_date)
KRvideos$publish_date <- substr(KRvideos$publish_time, 1, 10)
KRvideos$publish_time <- substr(KRvideos$publish_time, 12, 19)

KRvideos$category_id <- as.factor(KRvideos$category_id)
KRvideos$trending_date <- as.factor(KRvideos$trending_date)

KRvideos <- within(KRvideos, rm(thumbnail_link, video_error_or_removed, video_id))
KRvideos <- arrange(KRvideos, desc(views))

attach(KRvideos)

##########     2.  Detailed Analysis on Entertainment category     ##########
TagsWithPublishtime<-data.frame(tags[category_id==24],publish_date[category_id==24])
TagsWithPublishtime$tags<-str_replace_all(TagsWithPublishtime$tags,'\\\"',"")
TagsWithPublishtime$tags<-strsplit(TagsWithPublishtime$tags, fixed=TRUE, split="|")
TagsWithPublishtime$publish_year <- year(TagsWithPublishtime$publish_date)
TagsWithPublishtime$publish_month <- month(TagsWithPublishtime$publish_date)
TagsWithPublishtime <- TagsWithPublishtime[,c("tags","publish_year","publish_month")]

noun_201711 <- c()
noun_201712 <- c()
noun_201801 <- c()
noun_201802 <- c()
noun_201803 <- c()
noun_201804 <- c()
noun_201805 <- c()
noun_201806 <- c()

for(i in 1:length(TagsWithPublishtime$tags)){
  if ((TagsWithPublishtime$publish_year[[i]]==2017) && (TagsWithPublishtime$publish_month[[i]]==11)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201711 <- c(noun_201711, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2017) && (TagsWithPublishtime$publish_month[[i]]==12)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201712 <- c(noun_201712, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2018) && (TagsWithPublishtime$publish_month[[i]]==1)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201801 <- c(noun_201801, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2018) && (TagsWithPublishtime$publish_month[[i]]==2)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201802 <- c(noun_201802, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2018) && (TagsWithPublishtime$publish_month[[i]]==3)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201803 <- c(noun_201803, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2018) && (TagsWithPublishtime$publish_month[[i]]==4)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201804 <- c(noun_201804, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2018) && (TagsWithPublishtime$publish_month[[i]]==5)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201805 <- c(noun_201805, txt)
    }
    next
  }
  else if ((TagsWithPublishtime$publish_year[[i]]==2018) && (TagsWithPublishtime$publish_month[[i]]==6)){
    txt = TagsWithPublishtime$tags[[i]] %>% tolower()
    if (txt[1] != "[none]"){ 
      noun_201806 <- c(noun_201806, txt)
    }
    next
  }
}
  
df_noun_201711 <- as.data.frame(table(noun_201711))
df_noun_201711 <- filter(df_noun_201711, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201712 <- as.data.frame(table(noun_201712))
df_noun_201712 <- filter(df_noun_201712, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201801 <- as.data.frame(table(noun_201801))
df_noun_201801 <- filter(df_noun_201801, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201802 <- as.data.frame(table(noun_201802))
df_noun_201802 <- filter(df_noun_201802, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201803 <- as.data.frame(table(noun_201803))
df_noun_201803 <- filter(df_noun_201803, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201804 <- as.data.frame(table(noun_201804))
df_noun_201804 <- filter(df_noun_201804, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201805 <- as.data.frame(table(noun_201805))
df_noun_201805 <- filter(df_noun_201805, Freq >= 2) %>% arrange(desc(Freq))
df_noun_201806 <- as.data.frame(table(noun_201806))
df_noun_201806 <- filter(df_noun_201806, Freq >= 2) %>% arrange(desc(Freq))

wordcloud(words=df_noun_201711$noun_201711, freq=df_noun_201711$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201712$noun_201712, freq=df_noun_201712$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201801$noun_201801, freq=df_noun_201801$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201802$noun_201802, freq=df_noun_201802$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201803$noun_201803, freq=df_noun_201803$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201804$noun_201804, freq=df_noun_201804$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201805$noun_201805, freq=df_noun_201805$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
wordcloud(words=df_noun_201806$noun_201806, freq=df_noun_201806$Freq, max.words=100, random.order = F, colors=brewer.pal(8, "Dark2"), family="AppleGothic", rot.per=.1, scale=c(2, 0.01))
  

