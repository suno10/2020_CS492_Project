
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
library(nortest)

KRvideos <- read_csv("Downloads/archive/KRvideos.csv")
View(KRvideos)
colnames(KRvideos)
summary(KRvideos)

# percentage of null values
sum(is.na(KRvideos)) / dim(KRvideos)[1] / dim(KRvideos)[2] * 100

###############     1.  Exploration and Pre-processing     ##############

# 0. Pre-processing
KRvideos$trending_date <- ydm(KRvideos$trending_date)
KRvideos$publish_date <- substr(KRvideos$publish_time, 1, 10)
KRvideos$publish_time <- substr(KRvideos$publish_time, 12, 19)

KRvideos$category_id <- as.factor(KRvideos$category_id)
KRvideos$trending_date <- as.factor(KRvideos$trending_date)

KRvideos <- within(KRvideos, rm(thumbnail_link, video_error_or_removed, video_id))
KRvideos <- arrange(KRvideos, desc(views))

attach(KRvideos)

# 1. trending date
levels(trending_date)

cnt_date <- KRvideos %>% group_by(trending_date) %>% summarise(n=n())
barplot(cnt_date$n, names.arg=levels(trending_date), xlab='date(YY.MM.DD)', ylab='count',
        cex.axis = 0.8, cex.names = 0.8, col=5)

boxplot(cnt_date$n, col='white', cex.axis=0.8,xlab='Count of the Videos')
mtext(side=1, line=1, 'Median=165', cex=0.9)

summary(cnt_date)

# 2. category_id
levels(category_id)
pie(summary(category_id), main='Proportions of each category')

cnt_id <- KRvideos %>% group_by(category_id) %>% summarise(n=n())

ggplot(data=cnt_id, aes(as.factor(category_id), n)) +
  geom_bar(stat='identity', fill=1:17) +
  theme_bw() + labs(x='Category id', y='Count of videos') +
  theme(axis.title=element_text(size=9))

# 3. Tag
tags <- tags[!is.na(tags)]
tags <- strsplit(tags, fixed=TRUE, split="\"")

noun=c()
# !!!!!! WARNING: below command takes very long time !!!!!!
for(i in 1:length(tags)){
  txt = tags[[i]]
  txt[1] <- gsub('|', "", txt[1], fixed=TRUE)
  txt <- txt[txt != '|'] %>% tolower()
  noun=c(noun, txt)
}

rm(txt, i)
df_noun <- as.data.frame(table(noun))
df_noun <- filter(df_noun, Freq >= 2) %>% arrange(desc(Freq))
df_noun <- df_noun[-1,]

set.seed(20170459)
wordcloud(words=df_noun$noun, freq=df_noun$Freq, max.words=100,
          random.order = F, colors=brewer.pal(8, "Dark2"),
          family="AppleGothic", rot.per=.1, scale=c(3, 0.4))

# 4. Top 10 Videos
unique(KRvideos[,'title']) %>% head(10)

# 5. Correlation between Publish date and Trending date
period <- interval(publish_date, trending_date) %>% as.period() %>% day()
summary(period)
period_df <- as.data.frame(table(period))

filter(period_df, Freq >= 10) %>%
  ggplot(aes(period, Freq)) +
  geom_bar(stat='identity', fill=1:21) +
  theme_bw() + labs(x='Period(Day)', y='Count of videos') +
  theme(axis.title=element_text(size=9))

# 6. Likes & Dislikes
cor.test(likes, dislikes)
pairs(KRvideos[,8:9])

lm_likes <- lm(likes~dislikes)
summary(lm_likes)
plot(dislikes, likes, pch=21, bg='black')
lines(dislikes, lm_likes$fitted.values, col='red')

plot(dislikes, likes, pch=21, bg='blue',cex=0.7, log = "x")

# 7. Allow comment & Rating
table(comments_disabled, ratings_disabled)

###############      2.  Analysis of ranking factors      ###############

# 1. length of the title
title_df <- data.frame(views=views, title=title, length=nchar(title))
title_df <- within(title_df, {
  length_range = character(0)
  length_range[length<10] = "0-10"
  length_range[length>=10 & length<20] = "10-20"
  length_range[length>=20 & length<30] = "20-30"
  length_range[length>=30 & length<40] = "30-40"
  length_range[length>=40 & length<50] = "40-50"
  length_range[length>=50 & length<60] = "50-60"
  length_range[length>=60 & length<70] = "60-70"
  length_range[length>=70 & length<80] = "70-80"
  length_range[length>=80 & length<90] = "80-90"
  length_range[length>=90] = "90-100"
  
  length_range=factor(length_range, level=c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60",
                                            "60-70", "70-80", "80-90", "90-100"))
 })

table(title_df$length_range)

by(title_df$views, title_df$length_range, cvm.test)
    # it doesn't follow normal distribution, so we can't execute ANOVA test

kruskal.test(views~length_range, data=title_df)
    # p < 0.05 (A very small p-value)
    # We conclude that length of the title has a significant effect on view counts.

#test.set <- title_df %>% filter(views > 10000000)
test.set <- title_df %>% arrange(desc(views)) %>% head(500)
table(test.set$length_range)
boxplot(views~length_range, data=test.set, main='Top 500 videos',
        xlab='length_range of the title', ylab='view counts')

title_length_effect <- title_df %>% group_by(length_range) %>% summarise(views = mean(views))
title_length_effect$views <- round(title_length_effect$views/1000)
barplot(title_length_effect$views, names=title_length_effect$length_range,
        xlab='Length of the title', ylab='Avg number of views (thousand)')

# 2. Category
test.set <- filter(KRvideos, category_id != 44)
    # there are only 2 videos whose category ID is 44
    # for more accurate test, we eliminated these 2 videos in testset
by(test.set$views, test.set$category_id, cvm.test)
    # it doesn't follow normal distribution, so we can't execute ANOVA test

kruskal.test(views~category_id, data=test.set)
    # p < 0.05 (A very small p-value)
    # We conclude that category has a significant effect on view counts.

category_df <- KRvideos %>% group_by(category_id) %>%
  summarise(views=mean(views)/1000) %>%
  arrange(desc(views))

barplot(category_df$views, names=category_df$category_id,
        xlab='Category ID', ylab='Avg number of views (thousand)', col=1:17)
table(category_id)

# 3. Allow comment & Rating
high_view_videos <- filter(KRvideos, views > 2000000) %>%
  group_by(comments_disabled, ratings_disabled)

high_view_videos %>% summarise(views=mean(views))
boxplot(high_view_videos$views~high_view_videos$comments_disabled+high_view_videos$ratings_disabled)

# 4. Common words in title

# 5. Common tags / number of the tags

# 6. Common words in descriptions

# 7. Comments/ratings/subscribes in descriptions

###############     3.  Case study for trending video     ###############
