rm(list=ls())
#setwd("E:/ISB/Residency/2/TABA/Assignment/IndividualAssignment1")
library(stringr)
library(gsubfn)
library(text2vec)
library(tm)
library(wordcloud)
library(ggplot2)

TopUsersTags = function(InputDF,topN=5)
{
  TweetDF = InputDF
  
  #Reading just the tweeter text
  TweetText = TweetDF$text
  
  users = unlist(strapplyc(TweetText,"@\\w+"))      #Extracting all the users from the text
  hash_Tags = unlist(strapplyc(TweetText,"#\\w+"))  #Extracting all the hash tags from the text
  
  usertbl = table(users)
  hashtagtbl = table(hash_Tags)
  
  # Extracting the top 5 Users and Hashtags
  Top5Users = usertbl[order(usertbl, decreasing = T)][1:topN]
  Top5Hashtag = hashtagtbl[order(hashtagtbl, decreasing = T)][1:topN]

  to_return = list(Top5Users = Top5Users, Top5Hashtag=Top5Hashtag)
  
  return(to_return)
  
}

#---------------------------------------------------------------------------------------------#
# Toeknizer Function             
#---------------------------------------------------------------------------------------------#

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


#----------------------------------------------------------------------#
#                     BUILDING THE WORD CLOUD
#----------------------------------------------------------------------#

TweetWordCloud = function(TweetText)
{
  Tokens = my_tokenizer(TweetText)
  TokenCnt = table(Tokens)
  TokenCnt = TokenCnt[order (TokenCnt,decreasing = T)]
  
  TopWords = TokenCnt[1:5]
  
  wordcloud(names(TokenCnt), TokenCnt,     # words, their freqs 
            scale = c(3.5, 0.5),     # range of word sizes
            20,                     # min.freq of words to consider
            1000,       # max #words
            colors = brewer.pal(8, "Dark2"))    # Plot results in a word cloud 
  title(sub = "IBM Tweeter")
  
}
   

#----------------------------------------------------------------------#
#           BUILDING THE BAR CHART FOR THE WORD FREQUENCY
#----------------------------------------------------------------------#

TweetBarChart = function(TweetText)
{
  Tokens = my_tokenizer(TweetText)
  TokenCnt = table(Tokens)
  TokenCnt = TokenCnt[order (TokenCnt,decreasing = T)]
  
  Tokendf = as.data.frame(TokenCnt[order (TokenCnt,decreasing = T)][1:15],stringsAsFactors = F)
  
  ggplot(Tokendf, aes(x = Tokendf$Tokens, y = Tokendf$Freq)) + 
    geom_bar(stat = "identity", fill = "Blue") +
    geom_text(aes(label = Tokendf$Freq), vjust= -0.20) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

#----------------------------------------------------------------------#
#                      UNIGRAMS AND BIGRAMS
#----------------------------------------------------------------------#
GetNGrams = function(TweetText,NGram=2, TopN=5)
{
  library(tidytext)
  library(tibble)
  StrText = my_tokenizer(TweetText,ReturnType = "Text")
  StrTextdf = as.data.frame(StrText)
  
  tok_fun = word_tokenizer  # using word & not space tokenizers
  
  it_0 = itoken( StrText,
                 tokenizer = tok_fun,
                 ids = 1:length(StrText),
                 progressbar = T)
  
  NGram = as.numeric(NGram)
  
  vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
                            ngram = c(NGram, NGram), stopwords = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt'))
  #vocab$vocab[order(vocab$vocab$terms_counts, decreasing = T),]
  
  TopBigrams = vocab$vocab[order(vocab$vocab$terms_counts, decreasing = T),][1:TopN]
  TopBigrams
  
  #vocab = create_vocabulary(it_0,    #  func collects unique terms & corresponding statistics
  #ngram = c(1L, 1L), stopwords = readLines('https://raw.githubusercontent.com/sudhir-voleti/basic-text-analysis-shinyapp/master/data/stopwords.txt'))
  #vocab$vocab[order(vocab$vocab$terms_counts, decreasing = T),]

}

#----------------------------------------------------------------------#
#                          unnest - Tokenizer/Bi-gram
#----------------------------------------------------------------------#

hb_GetNGrams = function(TweetText,NGram=2, TopN=20)
{
  library(dplyr)
  StrText = my_tokenizer(TweetText,ReturnType = "Text")
  StrTextdf = data_frame(StrText)
  unnest_tokens = unnest_tokens(StrTextdf,word_output, StrText,token = "words") 
  
  NGram = as.numeric(NGram)
  
  #counts & sorts no. of occurrences of each item in 'word' column 
  bigram_tokens = unnest_tokens(StrTextdf,word_output, StrText,token = "ngrams",n=NGram) %>% count(word_output, sort = TRUE) %>% rename(WordCnt = n)
  
  bigram_tokens_df = as.data.frame(bigram_tokens)
  
  top20 = head(bigram_tokens_df,TopN)
  
  ggplot(top20, aes(x = top20$word_output, y = top20$WordCnt)) + 
    geom_bar(stat = "identity", fill = "Blue") +
    geom_text(aes(label = top20$WordCnt), vjust= -0.20) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}




