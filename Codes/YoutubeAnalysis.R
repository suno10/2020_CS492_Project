
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
library(cluster)
library(NbClust)

# Please download KRvideos file first.
KRvideos <- read_csv("Downloads/KRvideos.csv")
View(KRvideos)
colnames(KRvideos)
summary(KRvideos)

# percentage of null values
sum(is.na(KRvideos)) / dim(KRvideos)[1] / dim(KRvideos)[2] * 100

###############     1.  Exploration and Pre-processing     ###############

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
tags <- strsplit(tags, fixed=TRUE, split="\"")

noun <- c()
num_tag <- c()

# !!!!!! WARNING: below command takes very long time !!!!!!
for(i in 1:length(tags)){
  txt = tags[[i]]
  
  if (txt[1] == "[none]") {
    num_tag=c(num_tag, 0)
    next
  }
  txt[1] <- gsub('|', "", txt[1], fixed=TRUE)
  txt <- txt[txt != '|'] %>% tolower()
  num_tag=c(num_tag, length(txt))
  noun=c(noun, txt)
}

rm(txt, i)
df_noun <- as.data.frame(table(noun))
df_noun <- filter(df_noun, Freq >= 2) %>% arrange(desc(Freq))
head(df_noun)

set.seed(492)
wordcloud(words=df_noun$noun, freq=df_noun$Freq, max.words=100,
          random.order = F, colors=brewer.pal(8, "Dark2"),
          family="AppleGothic", rot.per=.1, scale=c(3, 0.4))

summary(num_tag)
boxplot(num_tag, col='yellow', outline=FALSE, main='Number of tags')

# 4. Top 10 Videos
head(KRvideos[,'title'], 10)
head(unique(KRvideos[,'title']), 10)

# 5. Correlation between Publish date and Trending date
period <- interval(publish_date, trending_date) %>% as.period() %>% day()
summary(period)
period_df <- as.data.frame(table(period))

filter(period_df, Freq >= 10) %>%
  ggplot(aes(period, Freq)) +
  geom_bar(stat='identity', fill=1:21) +
  theme_bw() + labs(x='Period(Day)', y='Count of videos') +
  theme(axis.title=element_text(size=9))

# 6. Comments and Ratings (Likes & Dislikes)
cor.test(likes, dislikes)
pairs(KRvideos[,8:9])

poly_model <- lm(likes~poly(dislikes, 3, raw=T))
x <- with(KRvideos, seq(min(dislikes), max(dislikes)))
y <- predict(poly_model, newdata = data.frame(dislikes=x))

plot(dislikes, likes)
lines(x, y, col='red', type='l')

plot(log(dislikes), likes, pch=21, bg='blue')
lines(log(x), y, col='red')

# 7. Publish_time
time_hour=as.numeric(substr(publish_time,1,2))
ggplot(data=KRvideos,aes(time_hour))+geom_bar(fill= 1:24)+
  labs(x='Time (hour)', y='Count of videos')+
  theme(axis.title=element_text(size=9))
sort(table(time_hour),decreasing=T)[1:6]

time_group=3*floor(time_hour/3)
time_group1=paste(time_group>10,time_group-12*(time_group>10),'~',time_group+3-12*(time_group>10),'??')
time_group2=gsub("TRUE","????",time_group1)
time_group3=gsub("FALSE","????",time_group2)

ggplot(KRvideos, aes(category_id, fill=time_group3))+
  theme(legend.title=element_blank())+ geom_bar(position='fill')+labs(y='',x='Category ID') 

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
high_view_videos <- filter(KRvideos, views > 2000000)
allow_comment_rating <- group_by(high_view_videos, comments_disabled, ratings_disabled)

allow_comment_rating %>% summarise(views=mean(views))
boxplot(allow_comment_rating$views~allow_comment_rating$comments_disabled+allow_comment_rating$ratings_disabled)

# 4. Number of the tags
tag_df <- data.frame(views=views, tags=num_tag)
tag_df <- within(tag_df, {
  num_tag = character(0)
  num_tag[tags<5] = "0-5"
  num_tag[tags>=5 & tags<10] = "5-10"
  num_tag[tags>=10 & tags<20] = "10-20"
  num_tag[tags>=20 & tags<30] = "20-30"
  num_tag[tags>=30 & tags<50] = "30-50"
  num_tag[tags>=50 & tags<80] = "50-80"
  num_tag[tags>=80 & tags<100] = "80-100"
  num_tag[tags>=100] = "100~"
  
  num_tag=factor(num_tag, level=c("0-5", "5-10", "10-20", "20-30", "30-50", "50-80",
                                  "80-100", "100~"))
})

levels(tag_df$num_tag)
summary(tag_df$num_tag)

by(tag_df$views, tag_df$num_tag, cvm.test)
    # it doesn't follow normal distribution, so we can't execute ANOVA test

kruskal.test(views~num_tag, data=tag_df)
    # p < 0.05 (A very small p-value)
    # We conclude that number of the tag has a significant effect on view counts.

tag_df_avg <- tag_df %>% group_by(num_tag) %>%
  summarise(mean_views=mean(views)/1000)

barplot(tag_df_avg$mean_views, names=tag_df_avg$num_tag,
        xlab='Number of the tags', ylab='Avg number of views (thousand)', col=1:10)

# 5. Common words in tags
tags1 <- KRvideos$tags
df_noun1<-as.vector(df_noun[,1])

l=list()
for (i in 1:10) l<-append(l,rep(F,length(tags)))
for (i in 1:100){
  for(j in 1:10) l[[j]]=l[[j]]|str_detect(tags1,df_noun1[i+100*(j-1)])
}

l1=list()
for (i in 1:10) l1=append(l1,mean(views[l[[i]]][1:100]))
l1<-unlist(l1)
barplot(l1, ylab = "Number of views", col=2:11,
        xlab="Contains most used tags <--------> Contains less used tags (Unit: 100Tags)")

c<-rep(F,length(tags))
for (i in 1:1000) c=c|str_detect(tags1,df_noun1[i])
tags1000_views = data.frame(True=log(views[c][1:100]),False=log(views[!c][1:100]))
boxplot(tags1000_views,xlab="Contains Top 1000 most used tags", ylab="ln(views)", col=c("green","yellow"))
summary(views[c][1:100])
summary(views[!c][1:100])

##################        3.  Clustering        ##################

temp <- KRvideos
temp$num_tags <- tag_df$tags
temp$title_length <- title_df$length
temp$is_pm <- time_hour >= 12 # when the video published? (a.m. or p.m.)

clst <- rbind(head(temp, 100), tail(temp, 100), temp[17231:17330,])

common_tag=c()

for(i in 1:nrow(clst)){
  for (j in 1:100) {
    detected <- FALSE
    if (str_detect(clst$tags[i], as.character(df_noun[j,1]))) {
      detected = TRUE; break
    }
  }
  common_tag=c(common_tag, detected)
}

colnames(clst)

scaled_clst <- clst[,c(4,7,8,15,16,17)] %>% as.data.frame()
scaled_clst$views <- scale(scaled_clst$views)
scaled_clst$likes <- scale(scaled_clst$likes)
scaled_clst$common_tag <- common_tag # if the video has one of the top 100 tag the value of 'common tag' is 1, else 0

pamx <- pam(scaled_clst, 3)
summary(pamx)
plot(pamx)

res_clst <- data.frame(scaled_clst, cluster=pamx$clustering)

res_clst %>%
  select(views, num_tags, title_length, cluster, is_pm, common_tag,likes) %>%
  group_by(cluster) %>%
  summarise(mean_views=mean(views, na.rm = TRUE), mean_numTag=mean(num_tags),
            mean_titleLength=mean(title_length),
            is_pm=mean(is_pm), common_tag=mean(common_tag),
            mean_likes=mean(likes))

clst1 <- res_clst %>% filter(cluster==1) %>% group_by(category_id) %>%
  summarise(n=n()) %>% arrange(desc(n))
clst2 <- res_clst %>% filter(cluster==2) %>% group_by(category_id) %>%
  summarise(n=n()) %>% arrange(desc(n))
clst3 <- res_clst %>% filter(cluster==3) %>% group_by(category_id) %>%
  summarise(n=n()) %>% arrange(desc(n))

par(mfrow=c(3,1))
barplot(clst1$n, names.arg = clst1$category_id, col=1:9,
        main="middle-view-group")
barplot(clst2$n, names.arg = clst2$category_id, col=1:9,
        main="high-view-group")
barplot(clst3$n, names.arg = clst3$category_id, col=1:9,
        main="low-view-group")

par(mfrow=c(2,2),mar=c(2,4,1,1))
boxplot(views~cluster, data=res_clst, col=2:4)
boxplot(num_tags~cluster, data=res_clst, col=2:4)
boxplot(title_length~cluster, data=res_clst, col=2:4)
boxplot(likes~cluster, data=res_clst, col=2:4)
par(mfrow=c(1,1))
