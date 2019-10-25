tweets<-read.csv("C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/HW/HW2/trump_data.csv",stringsAsFactors=F,header=F)

#data preparation and cleaning
names(tweets)[1]="source"
names(tweets)[2]="time_posted"
names(tweets)[3]="text"
tweets$id<-seq(1,1240,1)
View(tweets)
tweets<-as.data.frame(tweets)
summary(tweets)

sum(tweets$source == "Trump")
sum(tweets$source == "Staff")
#Transfer the format of time 
tweets$time_posted<-as.character(tweets$time_posted)
tweets$time_posted<-ymd_hms(tweets$time_posted)
tweets$hour<-hour(tweets$time_posted)

#Exploratory analysis to examine which features to use
#Text analysis of individual words
reg<-"([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words<-tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

#Detect the most frequently used words
tweet_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

#Detecting quotation frequency
quoted<-tweets %>%
  count(source,quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) 
print(quoted)
tweets %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')
#From the figure, we can clearly see that tweets from Trump has a significantly higher change of starting with a quotation mark while those from Staff barely show the pattern. 

#Difference in sharing links or pictures in tweets
tweet_picture_counts <- tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))

ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets share links or pictures')
#From the figure, we can clearly see that tweets from Staff has a significantly higher change of sharing links or pictures while those from Trump barely show the pattern. 

#Calculate ratio 
spr <- tweet_picture_counts %>%
  spread(source, n) %>%
  mutate(funs(. / sum(.)), "Trump", "Staff")

rr <- spr$Staff[2] / spr$Trump[2]
#Tweets from Staff are 37.6 times as likely to contain either a picture or a link as those from Trump.

#From explortory analysis above, I determine to use 5 features as below.
#Generate features for Naive Bayes Classifier
#Hashtags
tweets$hashtag<-grepl(pattern="#",tweets$text)
#Punctuation
tweets$punctuation<-grepl(pattern="!",tweets$text)
sum(tweets$punctuation==T)
#Quotation
tweets$quotation<-grepl(pattern='^"', tweets$text)
sum(tweets$quotation==T)
#Picture or link
tweets$pic<-ifelse(str_detect(tweets$text, "t.co"),TRUE,FALSE)
#time period
tweets$period<-ifelse(tweets$hour<=19&tweets$hour>10,TRUE,FALSE)

tweets<-read.csv("C:/Chanel/NYU/Messy Data and Machine Learning, Section 001 Resources/HW/HW2/trump_data.csv",stringsAsFactors=F,header=F)
names(tweets)[1]="source"
names(tweets)[2]="time_posted"
names(tweets)[3]="text"
tweets$id<-seq(1,1240,1)
#View(tweets)
#tweets<-as.data.frame(tweets)
summary(tweets)
sum(tweets$source == "Trump")
sum(tweets$source == "Staff")
tweets$time_posted<-as.character(tweets$time_posted)
tweets$time_posted<-ymd_hms(tweets$time_posted)
tweets$hour<-hour(tweets$time_posted)
tweets$hashtag<-grepl(pattern="#",tweets$text)
tweets$punctuation<-grepl(pattern="!",tweets$text)
tweets$quotation<-grepl(pattern='^"', tweets$text)
tweets$pic<-ifelse(str_detect(tweets$text, "t.co"),TRUE,FALSE)
tweets$period<-ifelse(tweets$hour<=19&tweets$hour>10,TRUE,FALSE)
df<-data.frame(tweets$source,tweets$hashtag,tweets$punctuation,tweets$quotation,tweets$pic,tweets$period)
df<-rename(df,source=c("tweets.source"))
df<-rename(df,hashtag=c("tweets.hashtag"))
df<-rename(df,punctuation=c("tweets.punctuation"))
df<-rename(df,quotation=c("tweets.quotation"))
df<-rename(df,pic=c("tweets.pic"))
df<-rename(df,period=c("tweets.period"))
#View(df)
df$source<-as.factor(df$source)
df$hashtag<-as.factor(df$hashtag)
df$source<-as.factor(df$source)
df$punctuation<-as.factor(df$punctuation)
df$quotation<-as.factor(df$quotation)
df$pic<-as.factor(df$pic)
df$period<-as.factor(df$period)
str(df)

#Split training set and test set
set.seed(3221)
sample<-sample(1:nrow(df),nrow(df)*0.8)
train<-df[sample,]
test<-df[-sample,]
dim(test)
train<-as.data.frame(train)
test<-as.data.frame(test)

#Fit Naive Bayes classifier model on training set
attach(df)
fit_Bayes=NaiveBayes(source~hashtag+punctuation+quotation+period+pic,data=train)
#fit_Bayes[1:length(fit_Bayes)]

plot(fit_Bayes) 

#Predict on test set
pre_Bayes=predict(fit_Bayes,newdata=test)
names(pre_Bayes)

#Error rate
table(test$source,pre_Bayes$class)
error_Bayes=sum(as.numeric(as.numeric(pre_Bayes$class)!=as.numeric(test$source)))/nrow(test)
error_Bayes