#Final Project - Scratch Space

library(dplyr);library(lubridate);library(text2vec)

# Helper Functions --------------------------------------------------------
read.csv0 = function(x) {read.csv(x, stringsAsFactors=FALSE)}

elite.yrs_processor = function(entry){
  if(grepl("None", entry)){
    return(0)
  }else{
    entry = gsub("\\[", "", entry)
    entry = gsub("\\]", "", entry)
    entry = gsub("'", "", entry)
    split = strsplit(entry, split=",") %>% 
      unlist() 
    return(length(split))
  }
}

`%in.fast%` <- function(x, table) {
  stopifnot(require(fastmatch))
  fmatch(x, table, nomatch = 0L) > 0L
}


hours_processor = function(vector){
  each = function(entry){
    entry = gsub("\\[", "", entry)
    entry = gsub("\\]", "", entry)
    entry = gsub("'", "", entry)
    split = strsplit(entry, split=",") %>% 
      unlist() 
    return(split)
  }
  sapply(vector, each)
}

attributes_processor = function(vector){
  each = function(entry){
    entry = tolower(entry)
    regex_es = c("\\[", "\\]", "\\{", "\\}", "'" ,
                 "\"", "(ambience)", "(businessparking)")
    for(regex in regex_es){
      entry = gsub(regex, "", entry)
    }
    split = strsplit(entry, split=",") %>% 
      unlist() 
    split = trimws(split, which = c("both"))
    return(split)
  }
  sapply(vector, each) 
}
# Load Data --------------------------------------------------------
review_train.df = read.csv0("review_train.csv")
review_test.df = read.csv0("review_test.csv")

business_train.df = read.csv0("business_train.csv")
business_test.df = read.csv0("business_test.csv")

user.df = read.csv0("user.csv") %>% select(-name, -friends, type) %>% 
  mutate(yelping_since=as.Date(yelping_since)) %>% 
  mutate(active.days = (today() - yelping_since) %>% as.numeric) %>% 
  mutate(elite.yrs = sapply(elite, elite.yrs_processor)) %>% 
  select(-yelping_since, -type, -elite)%>% 
  mutate(useful_user = useful, cool_user=useful,
         funny_user = funny) %>% 
  select(-useful, -cool,-funny)

stop.words = read.csv0("stop-word-list.csv")
stop.words = stop.words$word %>% trimws(which="both")
stop.words = c(stop.words, "don", "don_t", "t", "cv")
# NLP for reviews --------------------------------------------------------
it.train = itoken(review_train.df$text, 
                  preprocessor = tolower,
                  tokenizer =  word_tokenizer, 
                  ids = review_train.df$review_id)
it.test = itoken(review_test.df$text,
                 preprocessor = tolower,
                 tokenizer =  word_tokenizer,
                 ids = review_test.df$review_id)

vocab = create_vocabulary(it.train, ngram = c(1L, 2L), stopwords = stop.words)
pruned.vocab = prune_vocabulary(vocab, term_count_min = 11646) #appears in 10% of data

vectorizer = vocab_vectorizer(pruned.vocab)


data_train = create_dtm(it.train, vectorizer)
data_test = create_dtm(it.test, vectorizer)

dim(data_train); dim(data_test)
data_train = data_train %>% as.matrix %>%  as.data.frame
data_test = data_test %>% as.matrix %>% as.data.frame
data_train$review_id = rownames(data_train)
data_test$review_id = rownames(data_test)

# NLP for business --------------------------------------------------------

it.train2 = itoken(business_train.df$attributes, 
                   preprocessor = tolower,
                   tokenizer =  attributes_processor, 
                   ids = business_train.df$business_id)
it.test2 = itoken(business_test.df$attributes,
                  preprocessor = tolower,
                  tokenizer =  attributes_processor,
                  ids = business_test.df$business_id)

vocab2 = create_vocabulary(it.train2, ngram = c(1L, 1L))
pruned.vocab2 = prune_vocabulary(vocab2, term_count_min = 250) #appears in 10% of data

vectorizer2 = vocab_vectorizer(pruned.vocab2)

data_train2 = create_dtm(it.train2, vectorizer2)
data_test2 = create_dtm(it.test2, vectorizer2)

dim(data_train2); dim(data_test2)
data_train2 = data_train2 %>% as.matrix %>%  as.data.frame
data_test2 = data_test2 %>% as.matrix %>% as.data.frame
data_train2$business_id = rownames(data_train2)
data_test2$business_id = rownames(data_test2)

it.train3 = itoken(business_train.df$hours, 
                   preprocessor = tolower,
                   tokenizer =  hours_processor, 
                   ids = business_train.df$business_id)
it.test3 = itoken(business_test.df$hours,
                  preprocessor = tolower,
                  tokenizer =  hours_processor,
                  ids = business_test.df$business_id)

vocab3 = create_vocabulary(it.train3)
pruned.vocab3 = prune_vocabulary(vocab3, term_count_min = 100)

vectorizer3 = vocab_vectorizer(pruned.vocab3)

data_train3 = create_dtm(it.train3, vectorizer3)
data_test3 = create_dtm(it.test3, vectorizer3)

dim(data_train3); dim(data_test3)
data_train3 = data_train3 %>% as.matrix %>%  as.data.frame
data_test3 = data_test3 %>% as.matrix %>% as.data.frame
data_train3$business_id = rownames(data_train3)
data_test3$business_id = rownames(data_test3)

business_train_prepped.df = business_train.df %>% 
  select(stars, business_id, review_count, longitude, latitude, state, postal_code) %>% 
  left_join(data_train2, by="business_id")%>% 
  left_join(data_train3, by="business_id")

business_test_prepped.df = business_test.df %>% 
  select(business_id, review_count, longitude, latitude, state, postal_code) %>% 
  left_join(data_test2, by="business_id")%>% 
  left_join(data_test3, by="business_id")


# Combining the data  --------------------------------------------------------
train.DF = review_train.df %>% 
  select(-type, -X, -stars) %>% 
  select(user_id, review_id, business_id, 
         useful_review = useful, cool_review=cool, 
         funny_review=funny) %>%
  left_join(business_train_prepped.df, by="business_id") %>% 
  mutate(y.stars=stars) %>% 
  select(-stars) %>%
  left_join(user.df, by="user_id") %>% 
  left_join(data_train, by="review_id") 

order = colnames(train.DF); order=c("y.stars", order[order !="y.stars"])
train.DF = train.DF[c(order)] %>% 
  select(-state)

dim(train.DF)
saveRDS(train.DF, "training_set.RDS")

test.DF = review_test.df %>% 
  select(-type, -X) %>% 
  select(user_id, review_id, business_id, 
         useful_review = useful, cool_review=cool, 
         funny_review=funny) %>% 
  left_join(user.df, by="user_id") %>% 
  left_join(data_test, by="review_id")%>% 
  left_join(business_test_prepped.df , by="business_id")%>% 
  select(-state) 

saveRDS(test.DF, "testing_set.RDS")
