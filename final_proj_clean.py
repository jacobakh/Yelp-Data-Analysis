
# coding: utf-8

# In[2]:

import pandas
import nltk
import string
import matplotlib.pyplot as plt #note this last import statement. Why might we use "import as"?

#read in our data
df = pandas.read_csv("../data/yelp_academic_dataset_review_train.csv", sep = ',', encoding = 'utf-8', index_col=0)
#df


# In[3]:

df['text'] = df['text'].str.lower()
df['text'] = df['text'].apply(lambda s: s.translate(str.maketrans("","", string.punctuation))) #remove punctuation
df['text'] = df['text'].apply(lambda x: ''.join([i for i in x if not i.isdigit()])) #remove numbers


# In[4]:

df['tokens'] = df['text'].str.split()


# In[1]:

from nltk.corpus import stopwords
#df['tokens'] = [word for word in df['tokens'] if word not in stopwords.words('english')]


# In[16]:

print(stopwords.words('english'))


# In[2]:

cachedStopWords = stopwords.words("english")
cachedStopWords


# In[10]:

import csv
import os

def WriteListToCSV(csv_file,csv_columns,data_list):
    try:
        with open(csv_file, 'w') as csvfile:
            writer = csv.writer(csvfile, dialect='excel', quoting=csv.QUOTE_NONNUMERIC)
            writer.writerow(csv_columns)
            for data in data_list:
                writer.writerow(data)
    except IOError as (errno, strerror):
            print("I/O error({0}): {1}".format(errno, strerror))    
    return              

csv_columns = ['words']
csv_data_list = cachedStopWords
currentPath = os.getcwd()
csv_file = currentPath + "../stopwords.csv"


WriteListToCSV(csv_file,csv_columns,csv_data_list)


# In[11]:

target = open(cachedStopWords,"wb")
writer = csv.writer("../stopwords.csv", dialect ='excel')


# In[4]:

import csv
cachedStopWords = open(..., 'wb')
wr = csv.writer(cachedStopWords, quoting=csv.QUOTE_ALL)
wr.writerow(cachedStopWords)
#cachedStopWords.to_csv("stopwords.csv")


# In[6]:

df['tokens_nostop'] = df['tokens'].apply(lambda x: [word for word in x if word not in cachedStopWords])


# In[8]:

df


# In[7]:

df['tokens_clean'] = df['tokens_nostop'].apply(lambda x: [" ".join(x)])
#df['tokens_clean'] = df['tokens_nostop'].apply(lambda x: [str(x)])
#" ".join([str(item) for var in data for item in var])


# In[ ]:

df.to_csv('cleaned_review_data.csv')
#df = pandas.read_csv('../data/cleaned_review_data.csv')
df = pandas.from_csv('../data/cleaned_review_data.csv', encoding = 'utf-8')


# In[13]:

import csv
df = csv.reader('../data/cleaned_review_data.csv')


# In[ ]:

df2 = pandas.DataFrame()
# r.to_csv("data.csv")
# grouped.to_csv("nodupes_alllyrics_tokenized.csv")
df2.from_csv("../data/cleaned_review_data.csv")
df2


# In[77]:

#df = pandas.read_csv("../data/cleaned_review_data.csv", sep = ',', encoding = 'utf-8', index_col=0)
df


# In[29]:

from sklearn.feature_extraction.text import CountVectorizer
countvec = CountVectorizer()
sklearn_dtm = countvec.fit_transform(df.text)


# In[ ]:

#import the function
from sklearn.feature_extraction.text import TfidfVectorizer
tfidfvec = TfidfVectorizer()


# In[91]:

#df_subset = df(index_col)
#df_subset = df[0:4999]
#df_subset = df_subset.reset_index(drop = True)
#df_subset.drop_duplicates(inplace=True)
#df_subset.rename(columns = {'': 'index'}
#df_subset = df_subset.reindex(index=None, columns=1)
#df_subset.columns = ['index', 'funny', 'user_id', 'review_id', 'text', 'business_id',
                    # 'stars', 'date', 'useful', 'type', 'cool', 'tokens', 'tokens_nostop',
                     #'tokens_clean']
df_subset


# In[94]:

#create the dtm, but with cells weigthed by the tf-idf score.
dtm_tfidf_df = pandas.DataFrame(tfidfvec.fit_transform(df_subset.tokens_clean).toarray(), columns=tfidfvec.get_feature_names())
print(dtm_tfidf_df.max().sort_values(ascending=False)[:100])


# In[ ]:

all_text = df['text'].apply(lambda x: [" ".join(x)])
all_text


# In[8]:

df['tokens_clean'] = df.tokens_clean.astype(str).str.replace('\[|\]|\'', '')


# In[10]:

list = df['tokens_clean']
list


# In[98]:

top100 = pandas.Series(' '.join(df['tokens_clean']).lower().split()).value_counts()[:100]
for x in top100:
    print("{0}".format(*x))


# In[99]:

pandas.Series(' '.join(df['tokens_clean']).lower().split()).value_counts()[:100]


# In[44]:

print(sorted([(w, words.count(w)) for w in set(all_tokens)], key = lambda x:x[1], reverse=True)[:10])

