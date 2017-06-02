# Sentiment analysis using Postive and Negative Words
# Positive and Negative word list extracted from 
# http://ptrckprry.com/course/ssd/data/positive-words.txt
# http://ptrckprry.com/course/ssd/data/negative-words.txt
rm(list=ls())
setwd("E:\\ISB\\Residency\\2\\TABA\\Text_Analysis_Code")

library(stringr)
library(gsubfn)
library(text2vec)
library(tm)
library(wordcloud)
library(ggplot2)

PositiveWords = read.delim("Positive-words.txt",header = FALSE)
PositiveWords <- unlist(lapply(PositiveWords, function(x) { str_split(x, "\n") }))
NegativeWords = read.delim("Negative-words.txt",header = FALSE)
NegativeWords <- unlist(lapply(NegativeWords, function(x) { str_split(x, "\n") }))

#nrow(PositiveWords)
#nrow(NegativeWords)

#samsungreview = readLines("samsungs7reviewsamazonin.txt")
#samsungreview = as.data.frame(samsungreview)

calcScore = function(Reviewtext)
{ 
  #convert to lower
  Reviewtext = tolower(Reviewtext)
  
  #remove punctuation
  Reviewtext = gsub("[[:punct:]]", " ", Reviewtext)
  
  # remove control characters
  Reviewtext = gsub("[[:cntrl:]]", " ", Reviewtext)
  
  # remove numbers
  Reviewtext = gsub('\\d+', '', Reviewtext)
  
  WordList = str_split(Reviewtext,"\\s+")
  WordList = unlist(WordList)
  
  PositiveCnt = match(WordList,PositiveWords)
  PositiveCnt = !is.na(PositiveCnt)
  NegativeCnt = match(WordList,NegativeWords)
  NegativeCnt = !is.na(NegativeCnt)
  
  score = sum(PositiveCnt) - sum(NegativeCnt)
  
  return(score)
}


buildSentiGraph = function(textdf,GraphTitle)
{
  
  ScoreDF = NULL
  for(i in 1:nrow(textdf))
  {
    df = NULL
    score = calcScore(textdf[i,1])
    Trend = ifelse(score >0,"positive",ifelse(score < 0,"negative",ifelse(score==0,"Neutral",0)))
    
    df = data.frame(Review = textdf[i,1], Score = score, SentiTrend = Trend)
    ScoreDF = rbind(ScoreDF,df)
  }
  
  qplot(factor(SentiTrend), data=ScoreDF, geom="bar", fill=factor(SentiTrend))+xlab("Trend Categories") + ylab("Frequency") + ggtitle(GraphTitle)

}

my_tokenizer = function(textx, ReturnType = "Token")
{
  
  #----------------------------------------------------------------------#
  #                           TEXT CLEANING
  #----------------------------------------------------------------------#
  
  #Remove all emojis
  textx = str_replace_all(textx,"(\\\\x[a-z][0-9])","") %>% 
    str_replace_all("(\\\\x[0-9][a-z])","") %>% 
    str_replace_all("(\\\\x[a-z][a-z])","") %>% 
    str_replace_all("(\\\\x[0-9][0-9])","")
  
  textx = str_replace_all(textx,'http\\S+\\s*'," ") #Remove all URLs
  
  textx = str_replace_all(textx,"@\\w+|#\\w+"," ") #Remove all Hashtags and Users
  
  textx = str_replace_all(textx,"[^a-zA-Z\\s]+"," ")
  
  textx = removeNumbers(textx)
  
  textx = tolower(textx)
  
  textx = gsub("^b"," ",textx)
  
  textx = stripWhitespace(textx)                     # removing white space
  
  textx = str_replace_all(textx,"^\\s+|\\s+$", "")   # remove leading and trailing white space
  
  #----------------------------------------------------------------------#
  #                           CLEANING STOP WORDS
  #----------------------------------------------------------------------#
  
  # Read Stopwords list
  stpw1 = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt')# stopwords list
  stpw2 = tm::stopwords('english')      # tm package stop word list; tokenizer package has the same name function, hence 'tm::'
  comn  = unique(c(stpw1, stpw2,"rt","amp"))         # Union of two list
  stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation
  
  textx  =  removeWords(textx,stopwords)            # removing stopwords created above
  textx  =  stripWhitespace(textx)                  # removing white space
  
  #----------------------------------------------------------------------#
  #                           TEXT TOKENIZING
  #----------------------------------------------------------------------#
  
  textx = str_replace_all(textx,"^\\s+|\\s+$", "")   # remove leading and trailing white space
  TextTokens = unlist(str_split(textx,"\\s"))
  
  TokenList = NULL
  
  for(i in 1:length(TextTokens))
  {
    
    TokenList = append(TokenList, TextTokens[i])
  }
  
  TokenList = TokenList[TokenList != ""]
  
  if(ReturnType == "Token")
  {
    to_return = TokenList
  }
  else
  {
    to_return = textx
  }
  
  return(to_return)
  
}



DataWordCloud = function(xText,title)
{
  Tokens = my_tokenizer(xText)
  TokenCnt = table(Tokens)
  TokenCnt = TokenCnt[order (TokenCnt,decreasing = T)]
  
  TopWords = TokenCnt[1:5]
  
  wordcloud(names(TokenCnt), TokenCnt,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            20,                     # min.freq of words to consider
            1000,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = title)
  
}

#--------------------------------------------------------------------------------
# GST - Twitter Data
#--------------------------------------------------------------------------------

TwitterData = read.csv("GST_TwitterData.csv",header = FALSE)
TwitterText = as.data.frame(TwitterData$V2)

buildSentiGraph(TwitterText,"GST - Twitter Sentiment")

DataWordCloud(TwitterData$V2,"Twitter GST")

#--------------------------------------------------------------------------------
# GST - Review Data
#--------------------------------------------------------------------------------
GSTReviewData = readLines("GST_Review_Data.txt")
GSTReviewData = as.data.frame(GSTReviewData)

buildSentiGraph(GSTReviewData,"GST - Review Data")

#--------------------------------------------------------------------------------
# NDTV on GST - Review Data
#--------------------------------------------------------------------------------
GSTReviewDataM = readLines("NDTV_GST.txt")
GSTReviewDataM = as.data.frame(GSTReviewDataM)

buildSentiGraph(GSTReviewDataM,"GST - Media Data")

fulldata=paste0(TwitterData,GSTReviewData,GSTReviewDataM)
fulldata = as.data.frame(fulldata)

buildSentiGraph(fulldata,"GST - Media Data")

DataWordCloud(fulldata$fulldata,"Twitter GST")
  
  
