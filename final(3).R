setwd("E:/×ÀÃæÎÄ¼þ/zuoye/r/myself/finalproject")
load("BAN432_exam.Rda")

#packages
require(tm)
require(dplyr)
require(slam)
require(tibble)
require(pdftools)
require(rvest)
require(topicmodels)
require(wordcloud)
require(SentimentAnalysis)

##############################
#                            #
# Identify successful ICOs   #
#                            #
##############################

# Firstly find out those tokens listed anyway
listed.token <- merge(x = ICO, 
                      y = coinmarketcap, 
                      by = "token", 
                      all = F)

# Secondly look up the list date for those tokens
Sys.setlocale("LC_TIME","C")
for (i in 1:nrow(listed.token)){
  sourcedata <- readLines(listed.token[i,10])
  targetline <- sourcedata[grep("</tbody>", sourcedata)-8]
  listdate <- gsub("<.+?>", "", targetline)
  listdate <- gsub("[[:space:]]|[[:punct:]]", "", listdate)
  listdate <- as.character(as.Date(listdate, format = "%b%d%Y"))
  listed.token$date.list[i] <- listdate
}

# Thirdly filter out thoese company listed within 60 days (success)
# calculate the date difference
for(i in 1:nrow(listed.token)){
  listed.token$differce[i]<-as.Date(listed.token[i,11])-listed.token[i,3]}

# Remove those firms whose difference is greater than 60
require(dplyr)
success.firm <- listed.token %>% 
  filter(differce <= 60 & differce >= 0)

# Also make a tibble for the unsuccessful ICOs
unsuccess.firm <- ICO %>%
  filter(!token %in% success.firm$token)

##############################
#                            #
#     Sentiment Analysis     #     
#                            #
##############################
# Filter out those forum text about successful ICOs
# and also not published by issuer
success.forum <- forum %>%
  filter(token %in% success.firm$token) %>%
  filter(ICO.issuer == "FALSE")

# Make a new column for the ICO start date
for(i in 1:nrow(success.forum)){
  match.line <- success.firm[match(success.forum$token[i],success.firm$token),]
  success.forum$date.start[i] <- as.character(match.line$date.start)
}

# Make a new column for the date difference between text time and ICO start
success.forum$time.diff <- as.Date(success.forum$date.start)-as.Date(success.forum$date)
# Filter out those text posted before ICO start
success.forum <- success.forum %>%
  filter(success.forum$time.diff > 0)

success.forum$text <- iconv(success.forum$text, to = "UTF-8", sub = "")

bp <- quantile(success.forum$time.diff, 
               c(0,0.2,0.4,0.6,0.8,1), 
               na.rm = T)
success.forum$time.diff.quantile <- cut(as.numeric(success.forum$time.diff),
                                        breaks = bp,
                                        include.lowest = T)
success.forum$sentimentLM <- analyzeSentiment(success.forum$text)$SentimentLM
success.forum$sentimentGI <- analyzeSentiment(success.forum$text)$SentimentGI

plot(x = success.forum$user.status,
     y = success.forum$sentimentGI)

plot(x = success.forum$time.diff.quantile,
     y = success.forum$sentimentGI)

sentiment.score.1 <- aggregate(success.forum$sentimentGI,
                               by = list(success.forum$user.status),
                               mean)
plot(sentiment.score.1)

####### Do the same procedure for thoese unsuccessful firms ###########

unsuccess.forum <- forum %>%
  filter(token %in% unsuccess.firm$token) %>%
  filter(ICO.issuer == "FALSE")

for(i in 1:nrow(unsuccess.forum)){
  match.line <- unsuccess.firm[match(unsuccess.forum$token[i],unsuccess.firm$token),]
  unsuccess.forum$date.start[i] <- as.character(match.line$date.start)
}
unsuccess.forum$time.diff <- as.Date(unsuccess.forum$date.start)-as.Date(unsuccess.forum$date)
unsuccess.forum <- unsuccess.forum %>%
  filter(unsuccess.forum$time.diff > 0)

unsuccess.forum$sentimentLM <- analyzeSentiment(unsuccess.forum$text)$SentimentLM

unsuccess.forum$sentimentGI <- analyzeSentiment(unsuccess.forum$text)$SentimentGI

bp2 <- quantile(unsuccess.forum$time.diff, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)

unsuccess.forum$time.diff.quantile <- cut(as.numeric(unsuccess.forum$time.diff),
                                          breaks = bp2,
                                          include.lowest = T)
plot(x = unsuccess.forum$user.status,
     y = unsuccess.forum$sentimentGI)

plot(x = unsuccess.forum$time.diff.quantile,
     y = unsuccess.forum$sentimentGI)

# Comparison between success ones and unsuccess ones
summary(unsuccess.forum$sentimentGI, na.rm = T)
summary(success.forum$sentimentGI, na.rm = T)
sd(unsuccess.forum$sentimentGI, na.rm = T)
sd(success.forum$sentimentGI, na.rm = T)

######## See if narrow the user status ################
success.forum.2 <- forum %>%
  filter(token %in% success.firm$token) %>%
  filter(ICO.issuer == "FALSE") %>%
  filter(user.status >= 5)
for(i in 1:nrow(success.forum.2)){
  match.line <- success.firm[match(success.forum.2$token[i],success.firm$token),]
  success.forum.2$date.start[i] <- as.character(match.line$date.start)
}

success.forum.2$time.diff <- as.Date(success.forum.2$date.start)-as.Date(success.forum.2$date)
success.forum.2 <- success.forum.2 %>%
  filter(success.forum.2$time.diff > 0)

success.forum.2$text <- iconv(success.forum.2$text, to = "UTF-8", sub = "")
success.forum.2$sentimentGI <- analyzeSentiment(success.forum.2$text)$SentimentGI

## unsuccessful ICOs narrow to status4
unsuccess.forum.2 <- forum %>%
  filter(token %in% unsuccess.firm$token) %>%
  filter(ICO.issuer == "FALSE") %>%
  filter(user.status >= 5)

for(i in 1:nrow(unsuccess.forum.2)){
  match.line <- unsuccess.firm[match(unsuccess.forum.2$token[i],unsuccess.firm$token),]
  unsuccess.forum.2$date.start[i] <- as.character(match.line$date.start)
}
unsuccess.forum.2$time.diff <- as.Date(unsuccess.forum.2$date.start)-as.Date(unsuccess.forum.2$date)
unsuccess.forum.2 <- unsuccess.forum.2 %>%
  filter(unsuccess.forum.2$time.diff > 0)

unsuccess.forum.2$sentimentGI <- analyzeSentiment(unsuccess.forum.2$text)$SentimentGI

# calculate summary statistics
t.test(success.forum.2$sentimentGI, unsuccess.forum.2$sentimentGI)
summary(unsuccess.forum.2$sentimentGI, na.rm = T)
summary(success.forum.2$sentimentGI, na.rm = T)
sd(unsuccess.forum.2$sentimentGI, na.rm = T)
sd(success.forum.2$sentimentGI, na.rm = T)

##############################
#                            #
#         Passion            #
#                            #
##############################

token.freq <- as.tibble(table(forum$token))
token.freq <- token.freq[order(-token.freq$n),]
bp3 <- quantile(token.freq$n, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)
token.freq$n.quantile <- cut(token.freq$n,
                             breaks = bp3,
                             include.lowest = T)
bp3
table(token.freq$n.quantile)
success.firm.token <- paste(success.firm$token, collapse = " ")
token.freq$success <- as.numeric(0)
for(i in 1:nrow(token.freq)){
  token.freq$success[i] <- token.freq$success[i] + grepl(token.freq$Var1[i], success.firm.token, ignore.case = T)
}

aggregate(token.freq$success, 
          by = list(token.freq$n.quantile), 
          sum)

success.token.freq <- token.freq %>%
  filter(token.freq$success == 1)

unsuccess.token.freq <- token.freq %>%
  filter(token.freq$success == 0)

t.test(success.token.freq$n, unsuccess.token.freq$n, alternative = "greater")
sd(success.token.freq$n)
sd(unsuccess.token.freq$n)

##############################
#                            #
# Amount of Tokens for Sale  #
#                            #
##############################
summary(success.firm$coins.for.sale.nr)
summary(unsuccess.firm$coins.for.sale.nr)
summary(ICO$coins.for.sale.nr)

bp4 <- quantile(success.firm$coins.for.sale.nr, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)
bp5 <- quantile(unsuccess.firm$coins.for.sale.nr, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)

bp6 <- quantile(ICO$coins.for.sale.nr, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)

mean <- 1688000000
std <- 7430073798
n <- 52
mean + qt(0.025, n-1, low = T)*std/sqrt(n)
mean - qt(0.025, n-1, low = T)*std/sqrt(n)

t.test(as.numeric(success.firm$coins.for.sale.nr), as.numeric(unsuccess.firm$coins.for.sale.nr))

##############################
#                            #
#         KWIC               #
#                            #
##############################

head(text.success)
text.success <- tolower(paste(success.forum.2$text, sep = " "))
text.success <- gsub("[[:punct:]]|[[:digit:]]", "", text.success)
text.success <- strsplit(text.success, " ", fixed = T) 
text.success <- unlist(text.success)
text.success <- text.success[text.success != ""]
text.success <- as.tibble(text.success)
text.success <- text.success %>%
  filter(!text.success$value %in% stopwords())


keywords <- c("^scam.*?|^con.*?|^mlm.*?|^fraud.*?|^trick.*?|^deceiv.*?|^lie.*?|^swindl.*?|^cheat.*?")


keywords.nr <- sum(grepl(keywords, text.success$value))
keywords.ratio <- keywords.nr/(nrow(text.success)*0.001)


####### Do the same thing to unsuccess ICO's texts on forum

text.unsuccess <- tolower(paste(unsuccess.forum.2$text, sep = " "))
text.unsuccess <- gsub("[[:punct:]]|[[:digit:]]", "", text.unsuccess)
text.unsuccess <- strsplit(text.unsuccess, " ", fixed = T) 
text.unsuccess <- unlist(text.unsuccess)
text.unsuccess <- text.unsuccess[text.unsuccess != ""]
text.unsuccess <- as.tibble(text.unsuccess)
text.unsuccess <- text.unsuccess %>%
  filter(!text.unsuccess$value %in% stopwords())

keywords.nr.2 <- sum(grepl(keywords, text.unsuccess$value))
keywords.ratio.2 <- keywords.nr.2/(nrow(text.unsuccess)*0.001)

##############################
#                            #
#   Team members size        #
#                            #
##############################
success.firm.team <- success.firm %>%
  filter(team.members != "")
success.firm.team$team.size <- 1
for(i in 1:nrow(success.firm.team)){
  success.firm.team$team.size[i] <- 
    success.firm.team$team.size[i]+
    length(unlist(regmatches(success.firm.team$team.members[i], 
                             gregexpr("\\|", success.firm.team$team.members[i])))) 
  
}
success.firm.team$coin.per.member <- 
  as.numeric(success.firm.team$coins.for.sale.nr)/success.firm.team$team.size

##### Do the same procedure for the unsuccess ICOs ####

unsuccess.firm.team <- unsuccess.firm %>%
  filter(team.members != "")
unsuccess.firm.team$team.size <- 1
for(i in 1:nrow(unsuccess.firm.team)){
  unsuccess.firm.team$team.size[i] <- 
    unsuccess.firm.team$team.size[i]+
    length(unlist(regmatches(unsuccess.firm.team$team.members[i], 
                             gregexpr("\\|", unsuccess.firm.team$team.members[i])))) 
  
}
unsuccess.firm.team$coin.per.member <- 
  as.numeric(unsuccess.firm.team$coins.for.sale.nr)/unsuccess.firm.team$team.size

##### Comparison #######

bp7 <- quantile(success.firm.team$team.size, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)
bp8 <- quantile(unsuccess.firm.team$team.size, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)
t.test(success.firm.team$team.size, unsuccess.firm.team$team.size)
sd(success.firm.team$team.size)
sd(unsuccess.firm.team$team.size)
summary(success.firm.team$team.size)
summary(unsuccess.firm.team$team.size)

success.firm.team.2 <- success.firm.team %>%
  filter(team.size < 10)
success.firm.team.3 <- success.firm.team %>%
  filter(team.size >= 10)
summary(as.numeric(success.firm.team$coins.for.sale.nr))
summary(as.numeric(success.firm.team.2$coins.for.sale.nr))
summary(as.numeric(success.firm.team.3$coins.for.sale.nr))


bp9 <- quantile(success.firm.team$coin.per.member, 
                c(0,0.2,0.4,0.6,0.8,1), 
                na.rm = T)

bp10 <- quantile(unsuccess.firm.team$coin.per.member, 
                 c(0,0.2,0.4,0.6,0.8,1), 
                 na.rm = T)
t.test(success.firm.team$coin.per.member, unsuccess.firm.team$coin.per.member)
sd(success.firm.team$coin.per.member)
sd(unsuccess.firm.team$coin.per.member)


##############################
#                            #
#    Download Whitepaper     #
#   and analyze readability  #
#                            #
##############################

# read files names of provided whitepapers into vector
files<-list.files("whitepaper",full.names = T)

# get token name part from filenames
token.name<- character()
for(i in 1:length(files)){
  token.name[i]<-substr(files[i],12,nchar(files[i])-4)
  }   
                                       
# find whitepaper of which project we haven't been provided
need.firm<- ICO %>%
              filter(! token %in% token.name)

# download pdf for token without whitepaper
# forming url to get source code
name<-gsub(" ","",tolower(need.firm$firm.name))
name<-gsub(" ","_",name)
dir.create("pdfdownload/",showWarnings = F)

for(j in 1:length(name)){
  
  #1.form the url of project
  source.code<-readLines(paste0("https://icosbull.com/eng/ico/",name[j],"/whitepaper"))
  
  # find the line contain the index number of the token
  line<-grep("whitepapers",source.code)
  
  # find the first one code in source code which contains the index of token
  index.sentence<-source.code[line[1]]
  # remove all the \t
  index<-gsub("\t","",index.sentence)
  # get the index
  index<-substr(index,20,23)

  #2.pdf download
  download.file(url=paste0("https://icosbull.com/whitepapers/",index,"/",name[j],"_whitepaper.pdf"),
                destfile =paste0("pdf2/",need.firm$token[j],".pdf"),
                mode="wb")
  print(j)
}
#here we download whitepaper into other document
#then we paste them into document "whitepaper"

# remove firms whose whitepaper still can't be found
ICO<-ICO %>%
        filter(token %in% token.name)

# calculate summary statistics of whitepaper

all.text<-character()
for(i in 1:nrow(ICO)){
  success.text[i]<-paste(pdf_text(paste0("whitepaper/",
                                         ICO$token[i],".pdf"),
                                  opw="",
                                  upw=""),collapse = " ")
  print(i)}
all.text.1<-iconv(all.text,to="UTF-8",sub="")
# make document-term-matrix
all.corpus <- Corpus(VectorSource(all.text.1))
all.dtm <- DocumentTermMatrix(all.corpus,
                            control = list( 
                              removePunctuation = T,
                              stopwords = T,
                              stemming = F,
                              removeNumbers = T,
                              wordLengths = c(4, 20),
                              bounds = list(global = c(3,30))))
all.term.freq<-tibble(term=all.dtm$dimnames$Terms,
                    freq = as.vector(all.dtm[1,]))
all.term.freq %>% arrange(-freq)
all.term.freq$syllable<-syllable_counts_data$syllables[match(all.term.freq$term,
                                                           syllable_counts_data$word)]
# most frequent complex terms
all.term.freq%>% filter(!is.na(syllable)&syllable>4)%>%
  arrange(-freq)   
# we find that complex words are not complex for investors
# define yules.k function
yules.k <- function(input) {
  m1 <- length(input); temp <- table(table(input))
  m2 <- sum("*"(temp, as.numeric(names(temp))^2))
  return(10000*(m2-m1) / (m1^2))
}

for(i in 1:nrow(ICO)){
  # make words
  all.doc<-strsplit(all.text.1[i], "\\s+")[[1]]
  # clean a bit
  all.doc <- tolower(all.doc)
  all.doc.stemmed <- stemDocument(all.doc)
  # individual statistics
  ICO$nr.words[i]<-length(all.doc)
  ICO$nr.sentence[i]<-sum(grepl("(\\.|\\?|\\!|;|:)$",all.doc))
  ICO$words.per.sentence[i] <- as.numeric(ICO$nr.words[i])/as.numeric(ICO$nr.sentence[i])
  ICO$obj.size[i] <- as.numeric(object.size(all.text.1[i]))
  ICO$nr.verbs[i] <- sum(all.doc %in% tolower(verbs))
  # readability measures
  ICO$length[i] <- log(ICO$nr.words[i]+1) #delete infinitive or do it later as in slides
  ICO$YULES.K[i] <- yules.k(all.doc.stemmed)
  ICO$FICHTNERS.C[i] <- ICO$nr.verbs[i]/as.numeric(ICO$nr.sentence[i])*
  ICO$nr.words[i]/as.numeric(ICO$nr.sentence[i])
  print(i)
}

# calculate statistics of document size
summary(ICO$obj.size)
sd(ICO$obj.size)

# calcute statistics of other variables
# seperate information of success firms and fail firms first
ICO$nr.words<-as.numeric(ICO$nr.words)         
ICO$nr.sentence<-as.numeric(ICO$nr.sentence)   
final.ICO<-ICO %>%                                 
  filter(nr.words > 300)                               
                  
summary(final.ICO$nr.words)

# seperate ICO from success and fail to calculate them seperately
success.ICO<-final.ICO %>%                                 
  filter(nr.words > 300)%>%                                
  filter(token %in% success.firm$token) 
fail.ICO<-final.ICO %>%                                    
  filter(nr.words > 300)%>%                                
  filter(!token %in% success.firm$token)  
summary(success.ICO$nr.verbs)

##############################
#                            #
#    Adivisor Board          #
#                            #
##############################

# create three lists to store information of famous advisor
avg.rate<-list()
name<-list()
done.ICO<-list()
# scrape information of advisors
for(i in 1:4){
  
  famous.advisor<-html(paste0("https://icoholder.com/en/ico-advisors?sort=icoscount&direction=desc&page=",1))
  # get average project rate
  avg.rate[[i]]<-famous.advisor %>%
    html_nodes(".vertical-listing-td-member-rank span") %>%
    html_text() %>%
    as.numeric()
  
  # get name of famous advisors
  name[[i]]<-famous.advisor %>%
    html_nodes(".vertical-listing-td-member-name span") %>%
    html_text() 
  # get ICO they've participated in 
  done.ICO[[i]]<- famous.advisor %>%
    html_nodes(".vertical-listing-td-icos span") %>%
    html_text() %>%
    as.numeric() %>%
    na.omit()
}
advisor<-as.tibble(cbind(unlist(name),
                         unlist(avg.rate),
                         unlist(done.ICO)))
names(advisor)<-c("name","avg project rate","ICOs","projects raised")              
save(advisor,file="advisor.Rda")

# calculate frequency of name of famous advisors appeare in success firm
for(i in 1:nrow(success.ICO)){
  # get name of advisor member
  member<-strsplit(success.ICO$advisor.members[i],"|",fixed=T)
  # calculate frequency
  fre<- sum(advisor$name %in% member[[1]])
  success.ICO$nr.advisor[i]<-fre
}
# calculate frequency of name of famous advisors appeare in fail firm
for(i in 1:nrow(fail.ICO)){
  # get name of advisor member
  member<-strsplit(fail.ICO$advisor.members[i],"|",fixed=T)
  # calculate frequency
  fre<- sum(advisor$name %in% member[[1]])
  fail.ICO$nr.advisor[i]<-fre
}
# create frequency table for fail and success firm seperately
success.fre<-table(success.ICO$nr.advisor)
fail.fre<-table(fail.ICO$nr.advisor)

# do chi-square test to check if work statistically
x<-matrix(c(50-36,265-225,36,225),ncol=2,byrow=T)
chisq.test(x)

##############################
#                            #
#    Document Clustering     #
#        LDA model           #
#                            #
##############################

# create character vector
final.text<-character()
# store whitepaper contents to the vector we created
for(i in 1:nrow(final.ICO)){
  final.text[i]<-paste(pdf_text(paste0("whitepaper/",
                                     final.ICO$token[i],".pdf"),
                              opw="",
                              upw=""),collapse = " ")
  print(i)}

# make document-term-matrix
final.corpus <- Corpus(VectorSource(final.text.1))
final.dtm <- DocumentTermMatrix(final.corpus,
                          control = list( 
                            removePunctuation = T,
                            stopwords = T,
                            stemming = F,
                            removeNumbers = T,
                            wordLengths = c(4, 20),
                            bounds = list(global = c(3,30))))
# estimate topic model
final.topic <- LDA(final.dtm,  # document term matrix
                 k = 25, # specifify number of topics
                 method = "Gibbs",
                 control = list(
                   seed = 1234, # eases replication
                   burnin = 100,  # how often sampled before estimation recorded
                   iter = 100,  # number of iterations
                   keep = 1,    #  # saves logLiklihood of all iterations #saves additional data per iteration (such as logLik)
                   save = F,    
                   verbose = 10  # report progress
                 ))
# Term distribution for each topic
beta <- exp(final.topic@beta) # log of probability reported
dim(beta)
# inspect the most common terms of topic 1
head(final.topic@terms[order(beta[1,], decreasing = T)], 10)
# Topic distribution for each document
gamma <- final.topic@gamma
dim(gamma)
colnames(gamma)<-1:25

# match topic to each firm
for(i in 1:nrow(final.ICO)){
  calculate.ICO$topic[i]<-match(max(gamma[i,]),gamma[i,])
}
# calculate fre table of topic for success firm
success.ICO<-merge(success.ICO,final.ICO,by="token")
s.topic.fre<-table(success.ICO$topic)
fail.ICO<-merge(fail.ICO,final.ICO,by="token")
f.topic.fre<-table(fail.ICO$topic)
# wordcloud with term distribution for topic 1

terms.top.40 <- head(final.topic@terms[order(beta[2,], decreasing = T)], 40)
prob.top.40 <- head(sort(beta[2,], decreasing = T), 40)
wordcloud(words = terms.top.40,
          freq = prob.top.40,
          random.order = F,
          scale = c(4, 1))
# here we find topics categorized by LDA are very hard to define

##############################
#                            #
#    Document Clustering     #
#        K-means             #
#                            #
##############################

clusters <- kmeans(as.matrix(final.dtm), centers = 25)

# inspect cluster assignments
final.ICO$industry.kmeans<-clusters$cluster
success.ICO<-merge(success.ICO,final.ICO,by="token")
fail.ICO<-merge(fail.ICO,final.ICO,by="token")
# inspect how many tokens in each cluster
kmeans.distri<-table(fail.ICO$industry.kmeans)
# check wcss
clusters$tot.withinss


