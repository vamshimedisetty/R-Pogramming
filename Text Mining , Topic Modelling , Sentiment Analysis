# Code to install the packages

install.packages('tm')
install.packages('dplyr')
install.packages('qdap')
install.packages('topicmodels')
install.packages('ggplot2')
install.packages('knitr')

#______ To install and load Rgraphviz_______##
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")                      

## Please press 'a' when Update statement pops up
## Please press 'Yes' - to use a personal library.

##__________________________________________##

install.packages('wordcloud')


# Code to load the packages

library(tm)
library(qdap)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(knitr)


# Define the path to the text files


filepath <- file.path("C:", "Users/vamsh/Desktop/R_Project_1/", "arabian_nights") # Path to the local directory

filepath                                                                          # To check the contents of variable 'filepath'

length(dir(filepath))                                                             # To check the number of files in the directory

dir(filepath)                                                                     # Displays the set of files

docs = Corpus(DirSource(filepath))                                                # create a Corpus with all the files
  
                                                                                  # A Corpus is a data structure which contains text as documents.
                                                                                  # This is extensively used in Text Mining.

docs                                                                              # verify that all files have been loaded into corpus


inspect(docs[1:2])                                                                # To check details about first two documents in corpus

getTransformations()                                                              # To get a list of all the data cleaning functions available

docs = tm_map(docs, removePunctuation)                                            # To remove punctuations on the text data

docs = tm_map(docs, tolower)                                                      # To convert all the text data into lower case

docs = tm_map(docs, removeNumbers)                                                # To remove all the numerical data present in text


##____________  Removing Stop Words ________________##

# Stop words are the frequently used words, which don't add value.

# Argument Smart is inbuilt, which gives a set of pre-defined stop words. 

# We have also added few stop words of our choice.

stop_word = c(stopwords("SMART"),"thee", "thy","thou","ought", "said", "will","and","hath","made","till","replied") 

length(stop_word)                                  # To check the list of stop words

docs = tm_map(docs, removeWords,stop_word)         # This removes the stop words listed above from the Corpus     


##________________________________________##                                                                                                                                                          

docs = tm_map(docs, stripWhitespace)     # To remove the white spaces

docs = tm_map(docs, stemDocument)        # This stems the document


pdocs = tm_map(docs, PlainTextDocument)  # Converts the document into plain text document

docterms = TermDocumentMatrix(pdocs)     # This creates a term document matrix

findFreqTerms(docterms, 1000)            # Find terms which are repeated atleast 1000 times 


# Plot for Word frequency

termFrequency <- rowSums(as.matrix(docterms))                   # The row sums in a matrix gives number of times a word is used            

termFrequency <- subset(termFrequency, termFrequency>=1500)     # Subset of all words with frequency >= 1500

df <- data.frame(term=names(termFrequency), freq=termFrequency) # Take these entries into a dataframe


# Plot a bar chart with X-axis as 'terms' and y-terms as 'frequency'
# Also add X-label as 'Term' and Y-label as 'Ocuurence count'
# Heights of bar chart represent values in the data


ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Term") + ylab("Occurence count")


## Removing Sparse Terms

sparse_doc = removeSparseTerms(docterms, 0.1) # Remove the sparse terms from tdm

inspect(sparse_doc)                           # To get details on the sparse terms

findFreqTerms(sparse_doc, 500) # Check the frequent terms in spares_doc


## Plotting the word cloud of the frequently occuring words

m <- as.matrix(docterms)       # docterms is represented as matrix                

word.freq <- sort(rowSums(m),decreasing = T) # Word frequency is calculated in decreaing order

wordcloud (words = names(word.freq), freq = word.freq , min.freq = 500, 
           random.order = T,colors = brewer.pal(6,"Dark2"))      # Plot the word frequency as a word cloud in random order using random colours



## Finding Associations 

findAssocs(sparse_doc, "king", 0.2)    # Find associations with the word King with 20% correlation
findAssocs(sparse_doc, "hundred", 0.1)
findAssocs(sparse_doc, "allah", 0.1)


# Choose top 10 words with minimum occuring frequency of 400
# With 10% correlation

plot(sparse_doc, term=findFreqTerms(spares_doc, 400)[1:10], corThreshold = 0.1) 

sde = as.matrix(sparse_doc)          # Represent sparsed document as matrix
dm = dist(scale(sde))                # Calculate distance in matrix
fit = hclust(dm, method = "ward.D" ) # Represent distance parameter for cluster                                                            
plot(fit)


## Topic Modelling and Clustering

# LDA provides k different categories of topics, each category contains many subtopics


termdocs = DocumentTermMatrix(pdocs) # Convert plain text into dtm
topics   = LDA(termdocs,k=2)         # Find topics with two categories


# Obtain the terms from the topics
terms(topics,10)     



#______________________________________________________       Future Scope      ____________________________________________________________________#


# Polarity function taken from qdap package takes in plain text document containing text as an argument. We are considering just one document here 
# Output consists of the count of total words, average polarity , sd of the polarity and standard mean of the polarity
# Polarity is a way to calculate sentiment. Positive polarity indicates Positive (good) sentiment
# Negative polarity indicates Negative (bad) sentiment
# Polarity function uses in-built lexicon to assign each word a good / bad value

polarity(pdocs[1])
#polarity(pdocs[1])$group

#polarity()$all gives us a list of all the positive words and the negative words

sentiment = polarity(pdocs[1])$all
#sentiment

#lists all the  positive words in the document
sentiment$pos.words

#lists all the negative words in the document
sentiment$neg.words

#_______________________________________________________________________________________________________________________________________________________#

