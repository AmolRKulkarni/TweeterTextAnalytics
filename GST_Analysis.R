setwd("E:\\ISB\\Residency\\2\\TABA\\Text_Analysis_Code")

library(twitteR)
library(base64enc)

consumerkey = "xPv3WRCkcSzrn731PD3it2CfU"
consumersecret = "O83ZjK2EMbU5bJyfIuJ9GXr8d4f9hxZfZKrkum6ACVMNL0N3ej"
accesstoken = "706574951-BGp91dsC2BdLogrwbx9hHZ4NN7RCAqMymBjdLjfD"
accesstokensecret = "DZy8oC096eS6VPBe0SwOXUFiKdZTuLwMRgtODG8tpZV9a"

setup_twitter_oauth(consumerkey,consumersecret,accesstoken,accesstokensecret)

hashtaglist = c("#gst")
TwitterDF = NULL

for (ht in hashtaglist)
{
  df = NULL
  df = twListToDF(searchTwitter(ht,n=2000))  
  TwitterDF = rbind(TwitterDF,df)
}

TwitterDF$text = iconv(TwitterDF$text, "latin1", "ASCII", sub=" ")
TwitterDF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", TwitterDF$text) 
TwitterDF$text = gsub("@\\w+", "", TwitterDF$text) 
TwitterDF$text = gsub("[[:punct:]]", " ", TwitterDF$text) 
TwitterDF$text = gsub("[[:digit:]]", " ", TwitterDF$text) 
TwitterDF$text = gsub("http\\w+", " ", TwitterDF$text) 
TwitterDF$text = gsub("\n", " ", TwitterDF$text)  
TwitterDF$text = gsub("[ \t]{2,}", " ", TwitterDF$text) 
TwitterDF$text = gsub("[^[:alnum:]///' ]", " ", TwitterDF$text)
TwitterDF$text = gsub("^\\s+|\\s+$", " ", TwitterDF$text)

unique(TwitterDF$text)


write.csv(unique(TwitterDF$text),"GST_TwitterData.csv")
