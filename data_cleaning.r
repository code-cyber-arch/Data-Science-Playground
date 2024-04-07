library(tidyverse)

# -------------------------------------------------------------------------------------------------------
# Step 1 - Reading the dataset
# -------------------------------------------------------------------------------------------------------

setwd("C:/Users/amirasabdu/Desktop/finalCapston")
reddit <- read.csv(file = "RS_2017-09_filtered70.csv")
reddit_df <- reddit
glimpse(reddit_df)
str(reddit_df)

# -------------------------------------------------------------------------------------------------------
# Step 2 - Removing an index variable which is the first column
# -------------------------------------------------------------------------------------------------------

reddit_df <- reddit_df[,-1]

# -------------------------------------------------------------------------------------------------------
# Step 3 - Identifying and removing columns with all NA values
# -------------------------------------------------------------------------------------------------------

#Identifying columns with all NA values
which(colSums(is.na(reddit_df)) == nrow(reddit_df))
# Removing columns with all NA values
reddit_df <- reddit_df[,-which(colSums(is.na(reddit_df)) == nrow(reddit_df))]

# -------------------------------------------------------------------------------------------------------
# Step 4 - Identifying and removing columns with all one value
# -------------------------------------------------------------------------------------------------------

# Identifying columns with all one values
allOne <- c()
for (n in 1:ncol(reddit_df)){
  if (n_distinct(reddit_df[,n]) == 1){
    allOne <- c(allOne ,n)
  }
}
names(reddit_df[, allOne])
# Removing columns with all one values
reddit_df <- reddit_df[, -c(allOne)]

# -------------------------------------------------------------------------------------------------------
# Step 5 - Date and time
# -------------------------------------------------------------------------------------------------------
library(lubridate)

# convert created_utc to a week day
created_day <- reddit_df$created_utc
created_day <- as.POSIXct(created_day, origin="1970-01-01")
created_day <- as.Date(created_day, abbreviate = T)
created_day <- weekdays(created_day, abbreviate = T)
created_day <- as.factor(created_day)
reddit_df <- reddit_df[, -which(names(reddit_df)%in%c("created_utc"))]
reddit_df$created_day <- created_day

# convert retrieved_on to a week day
retrieved_day <- reddit_df$retrieved_on
retrieved_day <- as.POSIXct(retrieved_day, origin="1970-01-01")
retrieved_day <- as.Date(retrieved_day, abbreviate = T)
retrieved_day <- weekdays(retrieved_day, abbreviate = T)
retrieved_day <- as.factor(retrieved_day)
reddit_df <- reddit_df[, -which(names(reddit_df)%in%c("retrieved_on"))]
reddit_df$retrieved_day <- retrieved_day

# Frequency Table for created_day
table(reddit_df$created_day)
# Frequency Table for retrieved_day
table(reddit_df$retrieved_day)

# -------------------------------------------------------------------------------------------------------
# Step 6 - Text processing of the titles
# -------------------------------------------------------------------------------------------------------

library(tm)

titles <- reddit_df$title
titles <- Corpus(VectorSource(titles))
titles <- tm_map(titles, content_transformer(tolower))      # Step 6.1 - Convert all letters to lowercase
titles <- tm_map(titles, removeNumbers)                     # Step 6.2 - Remove numbers
titles <- tm_map(titles, removeWords, stopwords("english")) # Step 6.3 - Remove stop words
titles <- tm_map(titles, removePunctuation)                 # Step 6.4 - Remove punctuation
titles <- tm_map(titles, stripWhitespace)                   # Step 6.5 - Eliminate extra white space

library(SnowballC)
titles <- tm_map(titles, stemDocument)                      # Step 6.6 - Reduce all word to thier stem words


# Step 6.7 - Represent the data as incidence matrix
dtm <- TermDocumentMatrix(titles)
dtm$v<-rep(1,length(dtm$v))

# Step 6.8 - Words with at least 500
MySet <- findFreqTerms(dtm, 500)
dtm <- dtm[Terms(dtm)%in%MySet,]

# Step 6.9 - Adding the incidence matrix to th reddit_df
m <- as.matrix(dtm)
tm <- t(m)
tm <- as.data.frame(tm)

reddit_df <- cbind(reddit_df, tm)
reddit_df <- reddit_df[, -which(names(reddit_df)%in%c("title"))]

# Saving List of words that appear at least 500 posts
write.csv(MySet, file = "list_words.csv")

# -------------------------------------------------------------------------------------------------------
# Step 7 - Recording factors - one level occurs less than 30 times
# -------------------------------------------------------------------------------------------------------

# Identify all factors where at least one level occurs less than 30 times 
one_lessthan30 <- c()
for (n in 1:ncol(reddit_df) - length(MySet)){
  if (is.factor(reddit_df[,n])){
    for (x in 1:nlevels(reddit_df[,n])){
      if (table(factor(reddit_df[,n]))[x] < 30){
        one_lessthan30 <- c(one_lessthan30, n)
        break
      }
    }
  }
}
names(reddit_df[,one_lessthan30])


library(forcats)

for (n in 1:ncol(reddit_df) - length(MySet)){
  if (is.factor(reddit_df[,n])){
    reddit_df[, n] <- fct_lump_min(reddit_df[, n], 31)
  }
}

# -------------------------------------------------------------------------------------------------------
# Step 8 - Recording Factors - more than 100 levels
# -------------------------------------------------------------------------------------------------------

# Identify factors with more than 100 levels and amalgamate the remainder to the "other" category
above100 <- c()
for (n in 1:ncol(reddit_df) - length(MySet)){
  if (is.factor(reddit_df[,n]) && nlevels(reddit_df[,n]) > 100){
    reddit_df[, n] <- fct_lump(reddit_df[,n], 100)
    above100 <- c(above100, n)
  }
}
names(reddit_df[,above100])

# -------------------------------------------------------------------------------------------------------
# Step 9 - Recording Factors - factor where one level has fewer than 30 occurrences
# -------------------------------------------------------------------------------------------------------

# Identifying variables with binary factor where one level has fewer than 30 occurrences
lessthan30 <- c()
for (n in 1:ncol(reddit_df) - length(MySet)){
  if (is.factor(reddit_df[,n])){
    for (x in 1:nlevels(reddit_df[,n])){
      if (table(factor(reddit_df[,n]))[x] < 30){
        lessthan30 <- c(lessthan30, n)
      }
    }
  }
}
names(reddit_df[, lessthan30])
# Removing variables with binary factor where one level has fewer than 30 occurrences
reddit_df <- reddit_df[, -c(lessthan30)]

# -------------------------------------------------------------------------------------------------------
# Step 10 - Identifying and removing any columns that contain missing values
# -------------------------------------------------------------------------------------------------------

which(colSums(is.na(reddit_df)) > 0)
reddit_df <- reddit_df[, -which(colSums(is.na(reddit_df)) > 0)]

# -------------------------------------------------------------------------------------------------------
# Step 11 - Final checking
# -------------------------------------------------------------------------------------------------------

# Final Checking 
str(reddit_df)

# Identifying and removing all one values after lumping factor variables
allOne2 <- c()
for (n in 1:ncol(reddit_df) - length(MySet)){
  if (is.factor(reddit_df[,n]) && nlevels(reddit_df[,n]) == 1){
    allOne2 <- c(allOne2, n)
  }
}
names(reddit_df[,allOne2])
reddit_df <- reddit_df[ , -c(allOne2)]

# Final Checking and recording each remaining variables
str(reddit_df)

# -------------------------------------------------------------------------------------------------------
# Step 12 - Data transformation
# -------------------------------------------------------------------------------------------------------

# Identifying numerical variables
numVar <- c()
for (n in 1:(ncol(reddit_df) - length(MySet))){
  if (is.numeric(reddit_df[,n])){
    numVar <- c(numVar, n)
  }
}
names(reddit_df[, numVar])

# install.packages("patchwork")
library(patchwork)

# Plot score variable vs gilded variable
ggplot()+
  stat_bin_hex(aes(reddit_df$score, reddit_df$gilded)) + 
ggplot()+
  stat_bin_hex(aes(log(reddit_df$score+1), log(reddit_df$gilded+1))) +
ggplot()+
  stat_bin_hex(aes(reddit_df$score, log(reddit_df$gilded+1))) +
ggplot()+ 
  stat_bin_hex(aes(log(reddit_df$score+1), reddit_df$gilded))

# Plot score variable vs num_comments variable
ggplot()+
  stat_bin_hex(aes(reddit_df$score, reddit_df$num_comments)) +
ggplot()+
  stat_bin_hex(aes(log(reddit_df$score+1), log(reddit_df$num_comments+1))) +
ggplot()+
  stat_bin_hex(aes(reddit_df$score, log(reddit_df$num_comments+1))) + 
ggplot()+ 
  stat_bin_hex(aes(log(reddit_df$score+1), reddit_df$num_comments))

# Plot score variable vs num_crossposts variable
ggplot()+
  stat_bin_hex(aes(reddit_df$score, reddit_df$num_crossposts)) +
ggplot()+
  stat_bin_hex(aes(log(reddit_df$score+1), log(reddit_df$num_crossposts+1))) +
ggplot()+
  stat_bin_hex(aes(reddit_df$score, log(reddit_df$num_crossposts+1))) + 
ggplot()+ 
  stat_bin_hex(aes(log(reddit_df$score+1), reddit_df$num_crossposts))

# Transform score and num_comments variables
reddit_df$score <- log(1+reddit_df$score)
reddit_df$num_comments <- log(1+reddit_df$num_comments)

# Saving cleaned and transformed data
save(reddit_df, file = "reddit_df.RData")

# -------------------------------------------------------------------------------------------------------
# Step 13 - Assumptions and summary
# -------------------------------------------------------------------------------------------------------

# Regression model
reddit_df.lm <- lm(score~., data=reddit_df)
broom::glance(reddit_df.lm)

# Diagnostic plots for the model
plot(reddit_df.lm, which = 1) # Linearity plot
plot(reddit_df.lm, which = 2) # Normality plot
plot(reddit_df.lm, which = 3) # Constant spread plot

# -------------------------------------------------------------------------------------------------------
# Step 14 - Aliased factors
# -------------------------------------------------------------------------------------------------------

# Identify Aliased factors and save as data frame for inspection of each factor variable
names(coef(reddit_df.lm))[is.na(coef(reddit_df.lm))]
aliased_factors <- as.data.frame(names(coef(reddit_df.lm))[is.na(coef(reddit_df.lm))])
# Saving aliased factors
write.csv(aliased_factors, file = "aliased_factors.csv")

# Aliased factors
aliased_names <- c("author_flair_text", "crosspost_parent_list", "disable_comments",
                    "domain", "href_url", "parent_whitelist_status", "preview", "promoted",
                    "secure_media_embed", "subreddit", "subreddit_id", "whitelist_status")

# Removing Aliased factors
reddit_df <- select(reddit_df, -all_of(aliased_names))
#Update regression model
reddit_df.lm <- update(reddit_df.lm)
# Checking for Aliased factors
names(coef(reddit_df.lm))[is.na(coef(reddit_df.lm))]
# Checking updated regression model
broom::glance(reddit_df.lm)

# -------------------------------------------------------------------------------------------------------
# Step 15 - Simplification of the model
# -------------------------------------------------------------------------------------------------------
# saving data frame before removing the variables as backup data frame
reddit_df_b <- reddit_df

# removing author_cakeday
reddit_df <- reddit_df_b[, -which(names(reddit_df_b)%in%c("author_cakeday"))]
# update model
reddit_df.lm <- update(reddit_df.lm)
broom::glance(reddit_df.lm)

# removing contest_mode
reddit_df <- reddit_df_b[, -which(names(reddit_df_b)%in%c("contest_mode"))]
# update model
reddit_df.lm <- update(reddit_df.lm)
broom::glance(reddit_df.lm)

# removing both author_cakeday and contest_mode
reddit_df <- reddit_df_b[, -which(names(reddit_df_b)%in%c("author_cakeday", "contest_mode"))]
# update model
reddit_df.lm <- update(reddit_df.lm)
broom::glance(reddit_df.lm)

# -------------------------------------------------------------------------------------------------------
# Step 16 - Prediction
# -------------------------------------------------------------------------------------------------------

# Step 16.1 -  Predicting with 95% confidence interval for the 1400 record
pred1 <- predict(reddit_df.lm, newdata = reddit_df[1400, ], 
                 interval = "confidence", level = 0.95)
# Step 16.2 - 
pred1 <- exp(pred1)-1
pred1

# Step 16.3 - Altering the 1400 record
reddit_df[1400, "author"] <- "Other"
reddit_df[1400, "num_crossposts"] <- 7

# Repeating step 16.1 and 16.2
pred2 <- predict(reddit_df.lm, newdata = reddit_df[1400, ],
                 interval = "confidence", level = 0.95)
pred2 <- exp(pred2)-1
pred2

# -------------------------------------------------------------------------------------------------------
