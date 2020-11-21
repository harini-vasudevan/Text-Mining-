library(dslabs)
library(lubridate)
library(stringr)
library(tidyverse)
options(digits = 3)    # 3 significant digits
dates <- c("09-01-02", "01-12-07", "02-03-04")
ymd(dates)
data(brexit_polls)
brexit_polls%>% head()
polls <- brexit_polls%>% filter(month(startdate)==04)
sum(str_detect(brexit_polls$startdate,"\\d\\d\\d\\d-04-\\d\\d"))

sum(month(brexit_polls$startdate) == 4) #another way
length(polls)
round_polls <- round_date(brexit_polls$enddate,unit="week")
sum(str_detect(round_polls, "2016-06-12"))
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12") #another way
weekdays <- weekdays(brexit_polls$enddate)
sum(str_detect(weekdays, "Sunday"))
sum(str_detect(weekdays, "Tuesday"))
sum(str_detect(weekdays, "Wednesday"))
sum(str_detect(weekdays, "Thursday"))
sum(str_detect(weekdays, "Friday"))
sum(str_detect(weekdays, "Saturday"))

#or
table(weekdays(brexit_polls$enddate))
data(movielens)
head(movielens)
movielens %>% head()
# table(movielens$year)
# hms(as.character(movielens$datetime))
# hour(movielens$datetime)
# separate(data = movielens, col = datetime, into = c("date", "time"),by= " ")
# movielens <- movielens %>% mutate(str_split(movielens$datetime, " "))%>% setNames(c(date,time))
# table
dates <-as_datetime(movielens$timestamp)
review_by_year<- table(year(dates))
review_by_year
names(which.max(review_by_year))
review_by_hour <- table(hour(dates))
review_by_hour 
names(which.max(review_by_hour))
#text minng sentiment analysis
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata
#Use str_detect() to find the ID of the novel Pride and Prejudice.How many different ID numbers are returned?
gutenberg_metadata %>% filter(str_detect(title, "Pride and Prejudice"))%>% summarise(n = n_distinct(gutenberg_id))
gutenberg_works(title=="Pride and Prejudice",languages = "en", only_languages = TRUE,distinct
=TRUE) 
#How many words are present in the book?
book <- gutenberg_download(1342)
words<- book %>% unnest_tokens(word, text)
nrow(words)
#Remove stop words from the words object. Recall that stop words are defined in the stop_words data frame from the tidytext package.
words<- book %>% unnest_tokens(word, text)%>% filter(!word %in% stop_words$word)
nrow(words)
#After removing stop words, detect and then filter out any token that contains a digit from words.

#How many words remain?
sum(str_detect(words$word, "\\d+"))
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)
#Analyze the most frequent words in the novel after removing stop words and tokens with digits.

#How many words appear more than 100 times in the book?
words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()
words %>% count(word) %>% top_n(37180, n) %>% mutate(word = reorder(word, n)) %>% arrange(desc(n))%>% count(n>100)
#What is the most common word in the book?

words %>% count(word) %>% top_n(37180, n) %>% mutate(word = reorder(word, n)) %>% arrange(desc(n))
afinn <- get_sentiments("afinn")%>%count(sentiments)
afinn
sentiments
afinn_sentiments <- inner_join(words,afinn,by="word")
afinn_sentiments%>%count(word, sentiment, sort = TRUE)
nrow(afinn_sentiments)

#Positive and negative words in each lexicon
sentiments
afinn<- get_sentiments("afinn") 
afinn%>%inner_join(get_sentiments("afinn"))%>%summarise(sentiment = sum(value))%>%mutate(method = "AFINN")
afinn_sentiments <- inner_join(words,afinn,by="word")
afinn_sentiments
afinn_sentiments%>% filter(value >0)
#3414/6065


afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)
mean(afinn_sentiments$value > 0)
sum(afinn_sentiments$value == 4)
