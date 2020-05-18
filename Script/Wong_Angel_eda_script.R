# Angel Wong 
# EDA Script 
# Project 1 
# HGEN611 


#YOU HAVE TO LOAD ALL KERAS AND RETICULATE FIRST AND RUN USE_PYTHON 
library(keras)
library(reticulate)
use_python("/Library/Frameworks/Python.framework/Versions/3.7/bin/python3")
library(tidyr)
library(dplyr)
library(readr)
library(bpa)
library(skimr)
library(Ecdat)
library(knitr)
library(ggplot2)
library(stringr)
library(knitr)
library(gridExtra)


###############################################################################################

# Section 1: Cleaning, loading and editing data

###############################################################################################
#read in the original dataset 
dataset1<-read_csv("~/Downloads/az_products_partial.csv")

#read in the prediction values from sentiment analysis 
predictions<- read_csv("~/Downloads/predictions_nonbinary.csv")




#adjusting the data by taking the original dataset and selecting columns 
#arid, arin, rating, asin, price, review count, title and primary category 
#then getting the distinct arin because there can be only one arin for each review
#then grouping by the product and then getting the distinct arid because only one review
#for each reveiwer for each product. Then ungrouped so I can filter out all 
#products that does not have an ID and ensuring that ratings are 1,2,3,4, or 5.
#Also removed all the $ from price and ',' and added on a column for the prediction
#values that were from the sentiment analysis. Then filtered any products that 
#did not have a primary category

adjusted_data<-dataset1 %>% 
  select("arid","arin","rating","asin","price",
         "review_count","title","primary_category") %>% 
  distinct(arin,.keep_all = TRUE) %>%
  group_by(asin) %>% 
  distinct(arid, .keep_all=TRUE) %>% 
  ungroup(asin) %>% 
  filter(!is.na(asin), rating %in% c(1,2,3,4,5)) %>% 
  mutate(price=str_replace_all(price,"\\$|\\,",""),
         rating=as.factor(rating)) %>% 
  cbind(predictions) %>% 
  filter(!is.na(primary_category))

str(adjusted_data)
glimpse(adjusted_data)

nrow(dataset1 %>%
  distinct(arin))

###############################################################################################

# Section 2: Detailed Cleaning and manipulation and graphs

###############################################################################################

#distribution of rating, we can see that it is more skewed left, more ratings that are 5 stars 
adjusted_data %>% 
  ggplot(aes(rating))+
  geom_bar() + 
  ggtitle("Price vs Rating") +
  theme_bw() +
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
  

   
#creating pricing table where more certain products are removed if they are a range
#selected asin, rating, price and primary category and remove products that have a 
#range for price or does not have a price and mutate price to be numerical. 
pricing<-adjusted_data %>% 
  select("asin","rating", "price","primary_category") %>% 
  distinct(asin, .keep_all = TRUE) %>% 
  filter(!is.na(price), !str_detect(price, '-')) %>% 
  mutate(price=as.numeric(as.character(price)))


###############################################################################################
# Section 2.1: Graphs for pricing and rating 
###############################################################################################

######Figure 1########

#How does price impact rating? Does lower priced items have higher rating?
#shows lower priced items had more higher ratings 
#pricing graphed with histogram and facet_wrap with rating and fill with rating 

#we can see that there is a trend across all of the distributions that this a right skewed
#We see an that most of the count for the price is on the right, while there are prices that 
#are high that tips extends to the left


pricing %>% 
  ggplot(aes(price)) + 
  geom_histogram() + 
  facet_wrap(~rating, scales="free") +
  ggtitle("Price vs Rating") +
  theme_bw() +
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


######Table 1########
###THIS IS USED! 
#summary of price grouped by rating 
price_table<- pricing %>% 
  select("rating","price") %>% 
  group_by(rating) %>% 
  dplyr::summarise(total=n(),
            mean= round(mean(price),2),
            median=round(median(price),2),
            sd= round(sd(price),2),
            min=round(min(price),2),
            max=round(max(price),2)) %>% 
  ungroup() %>% 
  kable()
#pretty table
kable()

######Figure 2########

###bar graph of the average price and rating
pricing %>% 
  group_by(rating) %>% 
  dplyr::mutate(average_price=mean(price)) %>% 
  distinct(rating, .keep_all=TRUE) %>% 
  ggplot(aes(rating, average_price)) +
  geom_bar(stat="identity")+
  ggtitle("Average Price across Ratings") +
  theme_bw() +
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


######Figure 3########

##bar graph of the average price and rating with fill as primary category and facet_wraping
pricing %>% 
  group_by(rating,primary_category) %>% 
  dplyr::mutate(average_price=mean(price)) %>% 
  distinct(primary_category, .keep_all=TRUE) %>% 
  ggplot(aes(rating,average_price)) + 
  geom_bar(aes(fill=rating),stat="identity")+
  ggtitle("Average Price grouped by Rating and Departments") +
  facet_wrap(~primary_category, scales="free_y") +
  theme_bw() +
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

ggsave("Figure3_RatingxPrice.jpeg",plot=last_plot())




###############################################################################################
# Section 2.2: Graphs for review count and rating 
###############################################################################################

#rev_count includes proportions, it deletes all duplicate products based on asin.
#makes review_count a numeric and groups by primary category to calcuate proportion
#of reviews for each department 
rev_count<-adjusted_data %>% 
  select("asin","rating","review_count","primary_category") %>% 
  distinct(asin, .keep_all = TRUE) %>%
  mutate(review_count=as.numeric(as.character(review_count))) %>% 
  group_by(primary_category) %>% 
  mutate(prop_review= review_count/sum(review_count), 
         num=sum(review_count)) %>% 
  ungroup()


######Figure 4########

#boxplot of review_count and rating, with out the outliers and zoomed in to see the
#median better
p1<-rev_count %>% 
  ggplot(aes(rating, review_count)) +
  geom_boxplot() +
  facet_grid(.~rating, scale="free") + 
  theme_bw()+ 
  ggtitle("Review Count vs Rating")+
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
p2<-rev_count %>% 
  ggplot(aes(rating, review_count)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(.~rating, scale="free") + 
  theme_bw()+ 
  coord_cartesian( ylim = c(0, 400))+
  ggtitle("Review Count vs Rating")+
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

grid.arrange(p1,p2)

######Figure 5########

#boxplot of proportion and rating, with out the outliers and zoomed in to see the
#median better
rev_count %>%   
  ggplot(aes(rating, prop_review)) +
  geom_boxplot(outlier.shape=NA)  +
  facet_grid(.~rating, scale="free") +
  coord_cartesian( ylim = c(0, 0.004))+
  theme_bw()+ 
  ggtitle("Review Count Proportions vs Rating")+
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


#can say, while looking at the boxplots, we can see that there were outliers
# which is why for the boxplot images, we zoomed in on it. This makes sense
#because there is very few review_counts for a product that exceeds a couple hundred

######Figure 6########

#bar of proportion and rating with primary category as fill and facet_wrapped
rev_count %>%   
  ggplot(aes(rating, prop_review, fill=primary_category)) +
  geom_bar(stat="identity")  +
  facet_wrap(.~primary_category) +
  theme_bw()+ 
  ggtitle("Review Count Proportions vs Rating with Categorization")+
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


######Figure 7########

#price, review_count and rating as a line graph (not the greatest one)
adjusted_data %>% 
  select("asin","rating", "price","prop_review") %>% 
  distinct(asin, .keep_all = TRUE) %>% 
  filter(!is.na(price), !str_detect(price, '-')) %>% 
  dplyr::mutate(price=as.numeric(as.character(price)), 
         review_count=as.numeric(as.character(prop_review)),
         rating= as.factor(as.character(rating))) %>% 
  ggplot(aes(review_count, price)) +
  geom_line(aes(color=rating)) +
  facet_grid(.~rating, scale="free") + 
  theme_bw()+ 
  ggtitle("Lower price and increased review count results in increased rating")+
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))





###############################################################################################
# Section 2.3: Graphs for sentiment title reviews and rating 
###############################################################################################



######Figure 8########
#bar graph of rating and sentiment of title. Used facet_warp to split based on rating
#also filled color as rating too.
adjusted_data %>% 
  select("rating","primary_category","predictions") %>% 
  ggplot(aes(predictions)) +
  geom_bar(aes(fill=rating)) +
  facet_wrap(~rating, scale="free") + 
  theme_bw()+ 
  ggtitle("Positive titles for reviews has more five stars")+
  theme(plot.title = element_text(size=22),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


######Table 3########
#table of total amounts of positive and negative titles for each department
sentiment_table<-adjusted_data %>% 
  select("rating","primary_category","predictions") %>% 
  group_by(primary_category,predictions) %>% 
  dplyr::summarise(total_num=n()) %>% 
  ungroup() %>% 
  spread(predictions,total_num) %>% 
  dplyr::arrange(desc(positive)) %>% 
  kable()



######Table 4########
#table of proportion of positive and negative titles for each department grouped by
#rating as well
sentiment_table_detailed<-adjusted_data %>% 
  select("rating","primary_category","predictions") %>% 
  group_by(primary_category,predictions,rating) %>% 
  dplyr::summarise(total_num=n()) %>% 
  ungroup() %>% 
  spread(predictions,total_num) %>% 
  group_by(primary_category, rating) %>% 
  dplyr::summarize(prop_pos=positive/(negative+positive),
            prop_neg=negative/(negative+positive)) %>% 
  ungroup() %>% 
  gather("prop",n,3:4) %>% 
  spread(rating,n)


kable(sentiment_table_detailed) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))


######################################################################################################################

#     Section 3: Sentiment Analysis 

######################################################################################################################


#script edited from :
#https://jjallaire.github.io/deep-learning-with-r-notebooks/notebooks/6.1-using-word-embeddings.nb.html 


#takes in the csv file, which is amazon_titles that was prased and edited in python
#amazon.py script
train<- read_csv("~/Downloads/amazon_titles copy.csv")


#naming the headers for the training dataset 
names(train)<-c("label","title")
train<- train %>% 
  distinct(title, .keep_all=TRUE)

test<- adjusted_data$title

#getting 9000 reviews randomly from the training set 
train_sample<-sample_n(train, 9000)

#split into the training title and the sentiment 
train_title<-train_sample$title
train_label<-train_sample$label


#setting max words for tokenizer 
max_words<-10000

#setting max length of title 
maxlen<-100

#create your tokenizer and then fit your tokenizer with the title 
tokenizer <- text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(train_title)

#change your title to sequences 
sequences <- texts_to_sequences(tokenizer, train_title)

#get the word_index, how many times each word appears 
word_index = tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")

#pad your data so the sequences are all the same dimensions 
data <- pad_sequences(sequences, maxlen = maxlen)


labels <- as.array(train_label)



# Need to convert categorical data to numerical data
library(superml)
convert_to_numeric<-LabelEncoder$new()
y_train_num <-convert_to_numeric$fit_transform(train_label)



# Converts vector to a matrix 
y_train_matrix = to_categorical(y_train_num)


# A dictionary of words and IDs
word_index<-tokenizer$word_index

# Map to word embedding

embedding_dim <- 100
embedding_matrix <- array(0, c(max_words, embedding_dim)) # 100 is the shape of the input model (this may be changed if glove is changed)

#import glove in and read glove and map the embeddings to your words 
glove_dir = '~/Downloads/glove.6B'
lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt"))
embeddings_index <- new.env(hash = TRUE, parent = emptyenv())

for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}
for (word in names(word_index)) {
  index <- word_index[[word]]
  if (index < max_words) {
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector))
      # Words not found in the embedding index will be all zeros.
      embedding_matrix[index+1,] <- embedding_vector
  }
}



#create your model, this is a Bi-LSTM, we have the embedding layer which is with gloves input and dropout to make sure 
#we do not over train 
#we use sigmoid to classify, which puts it between 0-1 for predicition values (neg, pos)

model2 <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words, 
                output_dim = embedding_dim, 
                input_length = maxlen) %>% 
  bidirectional(layer_lstm(units = 64)) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 2, activation = 'sigmoid')
summary(model2)

#grabs the embeddings and sets the weights 
get_layer(model2, index = 1) %>% 
  set_weights(list(embedding_matrix)) %>% 
  freeze_weights()

#complite your model
model2 %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

#we are training model here 
model2 %>% fit(
  data, y_train_matrix,
  batch_size = 64,
  epochs = 30
)

#this is the test set, or in this case the titles from the amazon reviews 
test_sequence<-texts_to_sequences(tokenizer,test)
test_sequence_pad<-pad_sequences(test_sequence,maxlen = maxlen)

#get the prediction of the classes thus 0 or 1 
predictions<- model2 %>% predict_classes(test_sequence_pad,verbose=1)

#put as a dataframe instead of a matrix 
predictions<-as.data.frame(predictions)
#save as csv 
write.csv(predictions,"~/Downloads/predictions_nonbinary.csv", row.names = FALSE)
#transform back into text and put as dataframe 
predictions<-apply(predictions,1, function(x) convert_to_numeric$inverse_transform(x))
predictions<-as.data.frame(predictions)

#can do the same for model.predict and get the actual prediction probability values 
# write.csv(predictions,"~/Downloads/predictions.csv", row.names = FALSE)
# predictions<- read_csv("~/Downloads/predictions.csv")

