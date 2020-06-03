#Load required libraries
install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
library("tm")
library("twitteR")
library("ROAuth")
library("stringr")

# Download the file and store in your working directory
download.file(url= "http://curl.haxx.se/ca/cacert.pem", destfile= "cacert.pem")

#Insert your consumerKey and consumerSecret below
credentials <- OAuthFactory$new(consumerKey= Passing appropriate credentials,
                                consumerSecret= Passing appropriate credentials
                                oauthKey = Passing appropriate credentials,
                                oauthSecret Passing appropriate credentials,
                                requestURL='https://api.twitter.com/oauth/request_token',
                                accessURL='https://api.twitter.com/oauth/access_token',
                                authURL='https://api.twitter.com/oauth/authorize')

#Register Twitter Authentication
setup_twitter_oauth(credentials$consumerKey, credentials$consumerSecret, credentials$oauthKey, credentials$oauthSecret)

#extracting the tweets and storing it in a list
x <- userTimeline("AECOM",n=1500,includeRts = TRUE,excludeReplies=FALSE)

#converting list to Data Frame
aecom<- twListToDF(x)
write.xlsx(aecom, "C:/Users/prathamesh/Desktop/Aecom.xlsx")

primera <- read.csv(file.choose(), header=T, stringsAsFactors = FALSE)
View(primera)

#Cleaning text
iconv(primera$Tweets, from="UTF-8", to="ASCII", sub="")
primera$Tweets=str_replace_all(primera$Tweets,"[^[:graph:]]", " ")
primera$Tweets <- gsub("http\\w+", "", primera$Tweets)
primera$Tweets <- gsub("https://w+", "", primera$Tweets)
primera$Tweets <- gsub("@\\w+", "", primera$Tweets)
primera$Tweets <- gsub('[[:punct:]]', '', primera$Tweets)
primera$Tweets <- gsub('[[:cntrl:]]', '', primera$Tweets)
primera$Tweets <- gsub("[[:digit:]]", "", primera$Tweets)
primera$Tweets <- gsub("[ \t]{2,}", "", primera$Tweets)
primera$Tweets <- gsub("^\\s+|\\s+$", "", primera$Tweets)


str(primera)
View(primera)

VectorSource(primera$Tweets)

#Corpus
corpusprimera = Corpus(VectorSource(primera$Tweets))
corpusprimera

#Corpus-- list of words--collection of text documents
#list where all the words are there
#list of text-> list of words
#Vector Source--vectorization
#excel->vector
primeradtm=DocumentTermMatrix(corpusprimera)
primeradtm
View(primeradtm)
#List of words- -- convert list into matrix. each words becomes 1 or 0
#in comparison with tweet text
dim(primeradtm)
#number of rows and number of cols(words)
#Dimension before removing sparse terms

dim(primeradtm)
primeradtm = removeSparseTerms(primeradtm, 0.99)
primeradtm
#Whichever words is utilized less it removes them
#% of how much we want to remove sparse term
#Dimension after removing sparse terms
dim(primeradtm)
primerawordvec = as.data.frame(as.matrix(primeradtm))
# Add sentiment variable
primerawordvec$Category <- NA
primerawordvec$Category = factor(primera$Category)
View(primerawordvec)
table(primerawordvec$Category)
levels(primerawordvec$Category)

dim(primerawordvec)
rownames(primerawordvec)
primerawordvec<-as.data.frame(primerawordvec)
rownames((primerawordvec[primerawordvec$Category=="Sharing articles , Demos & learnings",][1:1524]), decreasing = T)

Miscellaneous<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Miscellaneous",][1:158]), decreasing = T))
Miscellaneous$Frequency<-Miscellaneous$`sort(colSums(primerawordvec[primerawordvec$Category == "Miscellaneous", ][1:158]), decreasing = T)`
Miscellaneous$`sort(colSums(primerawordvec[primerawordvec$Category == "Miscellaneous", ][1:158]), decreasing = T)`<-NULL
Miscellaneous$Words<-rownames(Miscellaneous)
View(Miscellaneous)
write.csv(Miscellaneous,"Miscellaneous.csv")


Articles_Learnings<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Articles/Learnings",][1:158]), decreasing = T))
Articles_Learnings$Frequency<-Articles_Learnings$`sort(colSums(primerawordvec[primerawordvec$Category == "Articles/Learnings", ][1:158]), decreasing = T)`
Articles_Learnings$`sort(colSums(primerawordvec[primerawordvec$Category == "Articles/Learnings", ][1:158]), decreasing = T)`<-NULL
Articles_Learnings$Words<-rownames(Articles_Learnings)
View(Articles_Learnings)
write.csv(Articles_Learnings,"Articles_Learnings.csv")

Retweets<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Retweets",][1:158]), decreasing = T))
Retweets$Frequency<-Retweets$`sort(colSums(primerawordvec[primerawordvec$Category == "Retweets", ][1:158]), decreasing = T)`
Retweets$`sort(colSums(primerawordvec[primerawordvec$Category == "Retweets", ][1:158]), decreasing = T)`<-NULL
Retweets$Words<-rownames(Retweets)
View(Retweets)
write.csv(Retweets,"Retweets.csv")

Client_Partner_Meeting<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Client/Partner Meeting",][1:158]), decreasing = T))
Client_Partner_Meeting$Frequency<-Client_Partner_Meeting$`sort(colSums(primerawordvec[primerawordvec$Category == "Client/Partner Meeting", ][1:158]), decreasing = T)`
Client_Partner_Meeting$`sort(colSums(primerawordvec[primerawordvec$Category == "Client/Partner Meeting", ][1:158]), decreasing = T)`<-NULL
Client_Partner_Meeting$Words<-rownames(Client_Partner_Meeting)
View(Client_Partner_Meeting)
write.csv(Client_Partner_Meeting,"Client_Partner_Meeting.csv")

Conference_Events_Webinars<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Conference/Events/Webinars",][1:158]), decreasing = T))
Conference_Events_Webinars$Frequency<-Conference_Events_Webinars$`sort(colSums(primerawordvec[primerawordvec$Category == "Conference/Events/Webinars", ][1:158]), decreasing = T)`
Conference_Events_Webinars$`sort(colSums(primerawordvec[primerawordvec$Category == "Conference/Events/Webinars", ][1:158]), decreasing = T)`<-NULL
Conference_Events_Webinars$Words<-rownames(Conference_Events_Webinars)
View(Conference_Events_Webinars)
write.csv(Conference_Events_Webinars,"Conference_Events_Webinars.csv")

Project_Updates<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Project Updates",][1:158]), decreasing = T))
Project_Updates$Frequency<-Project_Updates$`sort(colSums(primerawordvec[primerawordvec$Category == "Project Updates", ][1:158]), decreasing = T)`
Project_Updates$`sort(colSums(primerawordvec[primerawordvec$Category == "Project Updates", ][1:158]), decreasing = T)`<-NULL
Project_Updates$Words<-rownames(Project_Updates)
View(Project_Updates)
write.csv(Project_Updates,"Project_Updates.csv")

Job_Related<-as.data.frame(sort(colSums(primerawordvec[primerawordvec$Category=="Job Related",][1:158]), decreasing = T))
Job_Related$Frequency<-Job_Related$`sort(colSums(primerawordvec[primerawordvec$Category == "Job Related", ][1:158]), decreasing = T)`
Job_Related$`sort(colSums(primerawordvec[primerawordvec$Category == "Job Related", ][1:158]), decreasing = T)`<-NULL
Job_Related$Words<-rownames(Job_Related)
View(Job_Related)
write.csv(Job_Related,"Job_Related.csv")