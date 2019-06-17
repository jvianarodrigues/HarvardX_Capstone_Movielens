# Module 9 - Movielens capstone project
# version 5.1S
# Author: Jo Rodrigues
# Date: May 2019


# SECTION 1: EXTRACTING AND SETTING UP THE REQUIRED DATASETS ###############################
# Create edx set, validation set, and submission file

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# set up for creating the dataset from a local copy of the movielens source files
setwd("Y:\\JRodrigues\\Knowledge\\Data Science\\Harvardx\\Course examples\\Module 9 - capstone")
ratings <- read.table(text = gsub("::", "\t", readLines("ratings.dat")), col.names = c("userId", "movieId", "rating", "timestamp"))
str(ratings)
movies <- str_split_fixed(readLines("movies.dat"), "\\::", 3)

# toggled off download from grouplens.org - defaulting to local copy 
#dl <- tempfile()
#download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))
#movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
# run a very small sample to test
sample_set_divisor <- 1
movielens <- sample_n(movielens, size = floor(nrow(movielens)/sample_set_divisor), replace = FALSE)
#
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)

# SECTION 2: DATA PREPARATION ###########################################################

#test for complete cases in edx
sum(complete.cases(edx)) == nrow(edx)

#add no of ratings for a movie as a field
edx <- edx %>% group_by(movieId) %>% mutate(n_ratings = n()) %>% ungroup()
#edx_sample <- edx_sample %>% group_by(movieId) %>% mutate(n_ratings = n()) %>% ungroup()

#append matrix of unique movie genres to edx, i.e. split concatenated genres into distinct genres
n_distinct(edx$genres)
edx_single_genres <- edx %>% select(genres) %>% separate_rows(genres, sep ="\\|")
head(edx_single_genres)
edx_single_genres %>% summarize(n = n())
n_distinct(edx_single_genres)
genres_distinct <- distinct(edx_single_genres)
dim(genres_distinct)

for (i in 1:nrow(genres_distinct)){
  temp_vector <- str_detect(edx$genres, as.character(genres_distinct[i, ]))
  edx <- cbind(edx, temp_vector)
  colnames(edx)[colnames(edx) == "temp_vector"] <- paste("Genre", as.character(i), genres_distinct[i, ])
}
str(edx)

#create a subset of edx for speed of testing
#10% of full size
#set.seed(1)
#sample_index <- createDataPartition(edx$rating, times = 1, p = 0.1, list = FALSE)
#edx_sample <- edx[sample_index, ]

# Make sure userId and movieId in validation set are also in edx set
#validation_sample <- edx[-sample_index, ] %>% 
#  semi_join(edx, by = "movieId") %>%
#  semi_join(edx, by = "userId")


# SECTION 3: EXPLORATORY DATA ANALYSIS ####################################################
#exploratory data analysis to understand 

#display overall distributions of key parameters in the edx dataset
summary(edx)

#specifically, plot the ratings histogram
edx %>% ggplot(aes(rating)) + geom_histogram(color = "blue4", fill = "cornflowerblue", binwidth = 0.5) + ggtitle("Histogram of ratings")

#test for number of unique movies and users
edx %>% summarize(n_movies = n_distinct(movieId), 
                  n_users = n_distinct(userId))

#test for % of overall edx dataset with 100 or more entries for both movies and users
edx %>% 
  group_by(movieId) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  group_by(userId) %>%
  filter(n() >=100) %>%
  ungroup() %>%
  nrow() / nrow(edx)

#test for number of distinct movieId's and userId's in overall edx dataset with 100 or more entries
edx %>% 
  group_by(movieId) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  group_by(userId) %>%
  filter(n() >=100) %>%
  ungroup() %>%
  summarize(n_movies = n_distinct(movieId), 
            n_users = n_distinct(userId))
  
# calculate the mean, sd and median number of ratings per userId
edx %>% 
  group_by(userId) %>% 
  mutate(no_of_ratings_per_user = n()) %>% 
  summarize(mean_u = mean(no_of_ratings_per_user)) %>%
  summarize(mean = mean(mean_u), sd = sd(mean_u), median = median(mean_u))

# plot the distribution of the number of ratings per userId
edx %>% 
  group_by(userId) %>% 
  mutate(no_of_ratings_per_user = n()) %>% 
  ggplot(aes(no_of_ratings_per_user)) + geom_histogram(binwidth = 100, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of number of ratings per user")

# calculate the mean, sd and median number of ratings per movieId
edx %>% 
  group_by(movieId) %>% 
  mutate(no_of_ratings_per_movie = n()) %>% 
  summarize(mean_m = mean(no_of_ratings_per_movie)) %>%
  summarize(mean = mean(mean_m), sd = sd(mean_m), median = median(mean_m)) 

# plot the distribution of the number of ratings per movieId
edx %>% 
  group_by(movieId) %>% 
  mutate(no_of_ratings_per_movie = n()) %>% 
  ggplot(aes(no_of_ratings_per_movie)) + geom_histogram(binwidth = 1000, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of number of ratings per movie")

# plot the distribution of the number of ratings per movieId on a log10 x-scale
edx %>% 
  group_by(movieId) %>%
  summarize(n_rating = n()) %>%
  ggplot(aes(n_rating)) + 
      geom_histogram(color = "blue4", fill = "cornflowerblue") + 
      scale_x_log10() + 
      xlab("log of number of movies") + 
      ggtitle("Histogram of number of ratings per movie")

#display the top 10 movies by number of ratings, and the associated mean rating for those movies
edx %>% 
  group_by(movieId, title) %>%
  summarize(n_rating = n(), mean_rating = mean(rating)) %>%
  arrange(desc(n_rating)) %>% 
  head(n = 10)

# test for correlations - no_of_ratings vs. mean rating per movieId
cor_rating_nratings <- 
  edx %>% 
  group_by(movieId) %>%
  filter(n() > 100) %>%
  summarize(mean_rating = mean(rating), n_ratings = n()) %>%
  ungroup() %>%
  cor()
cor_rating_nratings

# plot of graph to visualise the strength of correlation btw no_of_ratings and the overall rating by movie
edx %>% 
  group_by(movieId) %>%
  filter(n() > 100) %>%
  summarize(mean_rating = mean(rating), n_ratings = n()) %>%
  ungroup() %>%
  ggplot(aes(n_ratings, mean_rating)) + 
  geom_point(colour = "blue4", alpha = 0.5) +
  geom_smooth(method = "lm", show.legend = TRUE) + 
  ggtitle("Mean of ratings vs. number of ratings", 
          subtitle= paste("Filtered for no. of ratings > 100, R^2 = ", 
                      round(cor_rating_nratings[3, 2], 3)))

# SECTION 4: TESTING PREDICTION MODELS ###############################################################

#set up train and test data partitions - toggled off testing for a subset of data
set.seed(1)
#previously used: test_index_sample <- createDataPartition(y = edx_sample$rating, times = 1, p = 0.2, list = FALSE)
train_set_sample <- edx #previously used: edx_sample[-test_index_sample, ]
test_set_sample <- validation  #previously used: edx_sample[test_index_sample, ]

test_set_sample <- test_set_sample %>% 
  semi_join(train_set_sample, by = "movieId") %>%
  semi_join(train_set_sample, by = "userId")

#set up the RMSE function

RMSE <- function(actual_ratings, pred_ratings){
  sqrt(mean((actual_ratings - pred_ratings)^2))
}

#Test model 1 - mean rating for all
mean_rating_sample <- mean(train_set_sample$rating)
mean_rating_sample
rmse1_naive <- RMSE(test_set_sample$rating, mean_rating_sample)
test_results <- data_frame(method = "Global mean", RMSE = rmse1_naive)

#Test model 2 - movie effect

bias_movies <- train_set_sample %>% 
  group_by(movieId) %>% 
  summarize(movie_i = mean(rating - mean_rating_sample))

bias_movies %>% ggplot(aes(movie_i)) + geom_histogram(binwidth = 0.25, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of movie effects")

pred_ratings <- mean_rating_sample + test_set_sample %>% 
  left_join(bias_movies, by='movieId') %>%
  pull(movie_i)

rmse2_movie_bias <- RMSE(pred_ratings, test_set_sample$rating)
test_results <- bind_rows(test_results,
                          data_frame(method="Add Movie bias",  
                                     RMSE = rmse2_movie_bias))

#Test model 3 - add user effect to mean + movie effect
bias_users <- train_set_sample %>% 
  left_join(bias_movies, by='movieId') %>%
  group_by(userId) %>%
  summarize(user_i = mean(rating - mean_rating_sample - movie_i))

bias_users %>% ggplot(aes(user_i)) + geom_histogram(binwidth = 0.25, color = "blue4", fill = "cornflowerblue") + ggtitle("Histogram of user effects")

pred_ratings <- test_set_sample %>% 
  left_join(bias_movies, by='movieId') %>%
  left_join(bias_users, by='userId') %>%
  mutate(pred = mean_rating_sample + movie_i + user_i) %>%
  pull(pred)

rmse3_user_bias <- RMSE(pred_ratings, test_set_sample$rating)
test_results <- bind_rows(test_results,
                          data_frame(method="Add Movie and User Bias",  
                                     RMSE = rmse3_user_bias))

#Test model 4 - regularised movie and user effects

# 4.1. solve for minimum lambda as applied to both movie and user effect
lambdas_movie <- seq(3.5, 6, 0.25)
lambdas_user <- seq(4.5, 6, 0.25)
rmses <- matrix(nrow = length(lambdas_movie), ncol = length(lambdas_user))

for (l_m in 1:length(lambdas_movie)){
  for (l_u in 1:length(lambdas_user)){
  
mu <- mean(train_set_sample$rating)
  
  b_movie_i <- train_set_sample %>% 
    group_by(movieId) %>%
    summarize(b_movie_i = sum(rating - mu)/(n()+lambdas_movie[l_m]))
  
  b_user_i <- train_set_sample %>% 
    left_join(b_movie_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_user_i = sum(rating - b_movie_i - mu)/(n()+lambdas_user[l_u]))
  
  pred_ratings <- 
    test_set_sample %>% 
    left_join(b_movie_i, by = "movieId") %>%
    left_join(b_user_i, by = "userId") %>%
    mutate(pred = mu + b_movie_i + b_user_i) %>%
    pull(pred)
  
  rmses[l_m,l_u] <- RMSE(pred_ratings, test_set_sample$rating)
  }
}  
colnames(rmses) <- lambdas_user
rownames(rmses) <- lambdas_movie
which.min(rmses)
as.vector(rmses)[which.min(rmses)]
knitr::knit_print(round(rmses, 5))
lambdas_movie_optimal <- lambdas_movie[ifelse(which.min(rmses) %% length(lambdas_movie) == 0, 
                                              length(lambdas_movie), 
                                              which.min(rmses) %% length(lambdas_movie))] 
lambdas_user_optimal <- lambdas_user[ceiling(which.min(rmses)/length(lambdas_movie))]

#4.2. calculate the RMSE off of the optimal lambdas for rmse4_reg_movie_user 

mu <- mean(train_set_sample$rating)

b_movie_i <- train_set_sample %>% 
  group_by(movieId) %>%
  summarize(b_movie_i = sum(rating - mu)/(n()+lambdas_movie_optimal))

b_user_i <- train_set_sample %>% 
  left_join(b_movie_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_user_i = sum(rating - b_movie_i - mu)/(n()+lambdas_user_optimal))

pred_ratings <- 
  test_set_sample %>% 
  left_join(b_movie_i, by = "movieId") %>%
  left_join(b_user_i, by = "userId") %>%
  mutate(pred = mu + b_movie_i + b_user_i) %>%
  pull(pred)

rmse4_reg_movie_user <- RMSE(pred_ratings, test_set_sample$rating)

#plot the regularised movie bias vs. the original movie bias to visualise the damping effects of the regularisation
b_movie_i <- train_set_sample %>% 
  group_by(movieId) %>%
  summarize(b_movie_i = sum(rating - mu)/(n()+lambdas_movie_optimal), 
            n = n())

data_frame(non_reg = bias_movies$movie_i, 
           regularlized = b_movie_i$b_movie_i, 
           n = b_movie_i$n) %>%
  ggplot(aes(non_reg, regularlized, size=log10(n))) + 
  geom_point(shape=1, alpha=0.1, color = "blue4") +
  ggtitle("Original movie bias vs. regularised")


#plot of regularised user bias
b_user_i <- train_set_sample %>% 
  left_join(b_movie_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_user_i = sum(rating - b_movie_i - mu)/(n()+lambdas_user_optimal),
            n = n())

summary(b_user_i)
data_frame(non_reg = bias_users$user_i, 
           regularlized = b_user_i$b_user_i, 
           n = b_user_i$n) %>%
  ggplot(aes(non_reg, regularlized, size=log10(n))) + 
  geom_point(shape=1, alpha=0.2, color = "blue4") +
  ggtitle("Original user bias vs. regularised")


test_results <- bind_rows(test_results,
                          data_frame(method="Regularized Movie and User Bias",  
                                     RMSE = rmse4_reg_movie_user))

#plot the final dataframe with the summary of all results across the 4 models

as.data.frame(test_results)
