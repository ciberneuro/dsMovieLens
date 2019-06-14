###################################
# Installing and loading libraries
###################################

# Note: Make sure you have properly installed and loaded the libraries before continuing

install.packages("data.table")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies=TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies=TRUE)
library(data.table)

###################################
# Create edx set and validation set
###################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

#Note: the commented text is the original ratings reading command.
#       Like many others, I had issued and had to replace it
#ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                      col.names = c("userId", "movieId", "rating", "timestamp"))
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)


###################################
# Analysis
###################################

#Note: the following lines are the code required to solve the MovieLens dataset Quiz

#Q1
print(paste("edx dataset has", dim(edx)[1], "rows and", dim(edx)[2], "columns."))

#Q2
print(paste(sum(edx$rating==0), "movies were given a rating of 0."))
print(paste(sum(edx$rating==3), "movies were given a rating of 3."))

#Q3
print(paste("There are", length(unique(edx$movieId)), "unique movies in the edx dataset"))

#Q4
print(paste("There are", length(unique(edx$userId)), "unique users in the edx dataset"))

#Q5
categories <- c("Drama", "Comedy", "Thriller", "Romance")
for (cat in categories) {
  print(paste("There are", sum(grepl(cat, edx$genres, fixed=TRUE)), 
              "movies in the", cat, "category."))
}

#Q6
edx %>% group_by(title) %>%
  summarize(n = n()) %>% arrange(desc(n)) %>% top_n(5)

#Q7
edx %>% group_by(rating) %>%
  summarize(n = n()) %>% arrange(desc(n))

#Q8
edx %>% mutate(startype = ifelse(rating %% 1 > 0, "Half-star", "Full-star")) %>%
  group_by(startype) %>%
  summarize(n = n()) %>% arrange(desc(n))

###################################
# Getting the lambda
###################################

#Note: This code runs through a series of lambdas to find the optimal value
#       the values in the lambda vector have been altered after the first run

lambdas <- c( seq(0, 5, 1), seq(0, 2, 0.25) )

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx$rating))
})

qplot(lambdas, rmses)
print(paste("Lambda", lambdas[which.min(rmses)], "offers the lowest RMSE."))

###################################
# Excecution
###################################

# Here we calculate the RMSE against the validation set 

lambda <- 0.5
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

validation_RMSE <- RMSE(predicted_ratings, validation$rating)
print(paste("Our algorithm has a RMSE of", validation_RMSE))
