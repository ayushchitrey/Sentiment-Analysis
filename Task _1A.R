# Task-1A Extract reviews of any product from ecommerce website like Snapdeal and Amazon

#installing packages
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

#Library
library(rvest)
library(XML)
library(magrittr)


#################AMAZON Review: Canon EOS 80D 24.2MP Digital SLR Camera (Black) + EF-S 18-55mm STM Lens Kit + Memory card
aurl <- "https://www.amazon.in/Canon-24-2MP-Digital-Camera-18-55mm/product-reviews/B01BUYJXMA/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumbe"

amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep = "")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}

write.table(amazon_reviews,"Canon_EOS_80D_24.2MP.txt")
getwd()


############################SNAPDEAL: bhawna collection Loard Shiv Trishul Damru Locket With Puchmukhi Rudraksha Mala Gold-plated Brass, Wood


surl_1 <- "https://www.snapdeal.com/product/bhawna-collection-loard-shiv-trishul/672311651336/reviews?page="
surl_2 <- "&sortBy=HELPFUL#defRevPDP"
snapdeal_reviews <- NULL
for (i in 1:21){
  surl <- read_html(as.character(paste(surl_1,surl_2,sep=as.character(i))))
  srev <- surl %>%
    html_nodes("#defaultReviewsCard p") %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"bhawna_collection_Loard_Shiv_Trishul.txt")
getwd()


######## Sentiment Analysis ##########
# 1B : Perform Sentiment Analysis prepare word Cloud for Unigram and Bigram

txt <- amazon_reviews

str(txt)
length(txt)
View(txt)

install.packages("tm")
library(tm)

# Convert the character data to corpus type
x <- Corpus(VectorSource(txt))

inspect(x[1])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))
?tm_map

# Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

inspect(x1[5])
x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

# To remove sparse entries upon a specific value
corpus.dtm.frequent <- removeSparseTerms(tdm, 0.99) 
?removeSparseTerms

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[1])

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 65)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

##### Word cloud #####
install.packages("wordcloud")
library(wordcloud)

wordcloud(words = names(w_sub), freq = w_sub)

w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
head(w_sub1)

wordcloud(words = names(w_sub1), freq = w_sub1) # all words are considered

# better visualization
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors=rainbow(30), scale = c(2,0.5), rot.per = 0.4)
windows()
wordcloud(words = names(w_sub1), freq = w_sub1, random.order=F, colors= rainbow(30),scale=c(3,0.5),rot.per=0.3)
?wordcloud

windowsFonts(JP1 = windowsFont("MS Gothic"))
par(family = "JP1")
wordcloud(x1, scale= c(2,0.5))
?windowsFonts

#### Bigram ####
#install.packages("rJava")
#install.packages("RWeka")
#Sys.setenv('JAVA_HOME'="C:/Program Files/Java/jdk-11.0.1/")

# Before installing java and changing JAVA_Home getting this error so installed 
#error: JAVA_HOME cannot be determined from the Registry

library(RWeka)
library(wordcloud)

minfreq_bigram <- 2
bitoken <- NGramTokenizer(x1, Weka_control(min = 2, max = 2))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq, decreasing = TRUE), ]

wordcloud(sort_two$bitoken, sort_two$Freq, random.order = F, scale = c(2, 0.35), min.freq = minfreq_bigram, colors = brewer.pal(8, "Dark2"), max.words = 150)


#####################################
# lOADING Positive and Negative words  
pos.words <- readLines(file.choose())	# read-in positive-words.txt
neg.words <- readLines(file.choose()) 	# read-in negative-words.txt

stopwdrds <-  readLines(file.choose())

### Positive word cloud ###
pos.matches <- match(names(w_sub1), pos.words)
pos.matches <- !is.na(pos.matches)
freq_pos <- w_sub1[pos.matches]
names <- names(freq_pos)
windows()
wordcloud(names, freq_pos, scale=c(4,1), colors = brewer.pal(8,"Dark2"))


### Matching Negative words ###
neg.matches <- match(names(w_sub1), neg.words)
neg.matches <- !is.na(neg.matches)
freq_neg <- w_sub1[neg.matches]
names <- names(freq_neg)
windows()
wordcloud(names, freq_neg, scale=c(4,.5), colors = brewer.pal(8, "Dark2"))
