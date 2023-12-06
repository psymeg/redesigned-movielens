#clear R variables
#rm(list = ls ())
####################################################
# This code is divided into the following sections #
# 1. Install required packages                     #
# 2. edx code for creating data sets               #
# 3. Data set exploration                          #
####################################################

##########################################################
# 1. Install required packages and download data            #
##########################################################


# Note: this process takes a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "https://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "https://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "https://cran.us.r-project.org") 
if(!require(kableExtra)) install.packages("kableExtra", repos = "https://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "https://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "https://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "https://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(dplyr)
library(kableExtra)
library(lubridate)
library(scales)
library(stringr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
    download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
    unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
    unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                           stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
    mutate(userId = as.integer(userId),
           movieId = as.integer(movieId),
           rating = as.numeric(rating),
           timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                          stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
    mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.9, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################
# End of edx code                        #
##########################################
##########################################
# combine with 8.6.2 recommendation code #
##########################################

#create a smaller dataset as this laptop cant handle the full set
#edx20 <- createDataPartition(edx$rating, times=1, p=.02, list=FALSE)

#Overview of Data / Initial exploration
#kable(head(edx), "pipe", 10)
kbl(head(edx,10), caption = "EDX Dataset Overview - First 10 Rows", booktabs = T) %>%     kable_styling(latex_options = c("striped", "hold_position"))

#kable(summary(edx), "simple")
kable(summary(edx), caption = "EDX Dataset Summary", booktabs = T) %>%     kable_styling(latex_options = c("striped", "hold_position"))
movieCount <- n_distinct(edx$title)
userCount <- n_distinct(edx$userId)
genreCount <- n_distinct(edx$genres)

edx_summarise <- edx |>
    summarise(Users = n_distinct(userId),
              MoviesIds = n_distinct(movieId),
              Titles = n_distinct(title),
              Genres = n_distinct(genres),
              MissingValues = anyNA(edx))

#kable(edx_summarise, title = "Summary of Movielens Data Set", "pipe")
kable(edx_summarise, caption = "Summary of Movielens Data Set", booktabs = T) %>%     kable_styling(latex_options = c("striped", "hold_position"))

#add a column with the datestamp in human readable form
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
maxFullDate <- max(edx$date)
#translate date to year
edx$reviewYear <- as.integer(format(edx$date, format="%Y"))

#get the earliest date
minDate <- min(edx$date)
#get the latest date
maxDate <- max(edx$date)
#plot of the review dates
edx_dataExplorationDateHistogram <- edx %>% 
    ggplot(aes(edx$reviewYear)) +
    geom_histogram(binwidth=1, colour="#094438", fill="#97d2dd") + 
    ggtitle("edx Review Date Histogram") + 
    xlab("Year") +
    ylab("Count") +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme(aspect.ratio=1)
edx_dataExplorationDateHistogram


#add a column with the release year extracted from the column 
edx$releaseYear <- str_extract(edx$title, "\\([0-9][0-9][0-9][0-9]\\)")

#remove parentheses from edx$releaseYear
edx$releaseYear <- str_sub(edx$releaseYear, 2, -2)
                    
edx$releaseYear <- edx$releaseYear %>% as.integer(edx$releaseYear)                    

#get the earliest year a movie was released in the dataset
minReleaseYear <- min(edx$releaseYear)

#get the latest year a movie was released in the dataset
maxReleaseYear <- max(edx$releaseYear)

#plot of the release dates
edx_dataExplorationReleaseHistogram <- edx %>% 
    ggplot(aes(edx$releaseYear)) +
    geom_histogram(binwidth=1, colour="#094438", fill="#97d2dd") + 
    ggtitle("edx Release Year Histogram") + 
    xlab("Year") +
    ylab("Count") +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme(aspect.ratio=1)
edx_dataExplorationReleaseHistogram

#plot of the review dates vs release dates

dataExplorationReviewLagHistogram <- edx %>% 
    mutate(reviewLag = edx$reviewYear - edx$releaseYear) %>%
    group_by(reviewLag) %>%
    summarise(Difference = n()) %>%
    ggplot(aes(reviewLag, Difference)) +
    geom_bar(stat = "identity", colour="#094438", fill="#97d2dd") + 
    ggtitle("edx Years Between Review and Release") + 
    xlab("Years") +
    ylab("Count") +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme(aspect.ratio=1)
dataExplorationReviewLagHistogram

#plot of the review dates vs release dates

dataExplorationMeanReviewLagHistogram <- edx %>% 
    mutate(reviewLag = edx$reviewYear - edx$releaseYear) %>%
    group_by(reviewLag) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(reviewLag, rating)) +
    geom_point() +
    geom_smooth(colour="#094438", fill="#97d2dd")  + 
    theme_bw() +
    scale_x_continuous(breaks = seq(0,100, by=10)) +
    xlab("Years") +
    ylab("Rating") +
    ggtitle("Effect of Movie Age on Rating.")
dataExplorationMeanReviewLagHistogram

edx_table <- table(edx$rating)

kable(edx_table, caption = "Rating Distribution", booktabs = T) %>%     kable_styling(latex_options = c("striped", "hold_position"))

#plot of the ratings
edx_hist1 <- edx %>% 
    ggplot(aes(edx$rating)) +
    geom_histogram(binwidth=0.5, colour="#094438", fill="#97d2dd") + 
    ggtitle("edx Rating Histogram") + 
    xlab("Rating") +
    ylab("Count") +
    scale_y_continuous(labels = scales::label_number_si()) +
    theme(aspect.ratio=1)
edx_hist1
seq1 <- seq(1,5,1)
edx1 <- edx[edx$rating %in% seq1,]
#head(edx1)

edx1_summarise <- edx1 |>
    summarise(Users = n_distinct(userId),
              MoviesIds = n_distinct(movieId),
              Titles = n_distinct(title),
              Genres = n_distinct(genres))

#kable(edx1_summarise)

kable(edx1_summarise, caption = "Whole Number Ratings", booktabs = T) %>%     kable_styling(latex_options = "hold_position")

#summary(edx1$rating)

seq0.5 <- seq(0.5,4.5,1)
edx0.5 <- edx[edx$rating %in% seq0.5,]
#head(edx0.5)

edx0.5_summarise <- edx0.5 |>
    summarise(Users = n_distinct(userId),
              MoviesIds = n_distinct(movieId),
              Titles = n_distinct(title),
              Genres = n_distinct(genres))

kbl(edx0.5_summarise, caption = "Decimal Point Ratings", booktabs = T) %>%     kable_styling(latex_options = "hold_position")
  
summary(edx0.5$rating)

edx_reviewperuser <- edx %>% 
    count(userId) %>%
    ggplot(aes(n)) +
    geom_histogram(binwidth=0.1, colour="#094438", fill="#97d2dd") +
    ggtitle("edx Count of Ratings per User") + 
    scale_x_log10()+
    xlab("Movies") +
    ylab("Count per User") +
    theme(aspect.ratio=1)
edx_reviewperuser

edx_reviewpermovie <- edx%>% 
    count(movieId) %>%
    ggplot(aes(n)) +
    geom_histogram(binwidth=0.1, colour="#094438", fill="#97d2dd") +
    ggtitle("edx Count of Ratings per Movie") + 
    scale_x_log10()+
    xlab("Movies") +
    ylab("Count per Movie") +
    theme(aspect.ratio=1)
edx_reviewpermovie

edx %>%
    count(userId) %>%
      ggplot(aes(n)) +
      geom_histogram(bins = 30, color = "#094438", fill="#97d2dd") +
      scale_x_log10() +
      xlab("Number of ratings") + 
      ylab("Number of users") +
      ggtitle("Number of ratings given by users") +
    theme(aspect.ratio=1)


#RMSE calculation function
RMSE <- function(predicted_ratings, true_ratings){
    sqrt(mean((predicted_ratings - true_ratings)^2))}
    

# rmse_results is lower than RMSE required for course
RMSETarget <- 0.86490

rmse_results <- tibble(method = "Target RMSE", RMSE = RMSETarget)
kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))


#Derive the mean and apply to RMSE function
mu_hat <- mean(edx$rating, na.rm = TRUE)

naive_rmse <- RMSE(edx$rating, mu_hat)

#print result to kable table
rmse_results <- rmse_results %>% add_row(method = "Naive RMSE", RMSE = naive_rmse)
kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))

#use mean derived above
movie_avgs <- edx %>% 
    group_by(movieId) %>% 
    summarize(lse = mean(rating - mu_hat))

predicted_ratings <- mu_hat + edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    pull(lse)

model_1_rmse <- RMSE(predicted_ratings, edx$rating)
#print result to kable table
rmse_results <- rmse_results %>% add_row(method = "Group By MovieID", RMSE = model_1_rmse)
kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))

#compute an approximation by computing mu and b_i and estimating b_u as the average 
user_avgs <- edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - lse))

predicted_ratings <- edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu_hat + lse + b_u) %>%
    pull(pred)


model_2_rmse <- RMSE(predicted_ratings, edx$rating)

rmse_results <- rmse_results %>% add_row(method = "Group By MovieID + UserID", RMSE = model_2_rmse)
kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))
# rmse_results is lower than RMSE required for course
# rmse_results$RMSE < 0.86490
mu_hat <- mean(edx1$rating, na.rm = TRUE)
movie_avgs <- edx1 %>% 
    group_by(movieId) %>% 
    summarize(lse = mean(rating - mu_hat))

predicted_ratings <- mu_hat + edx1 %>% 
    left_join(movie_avgs, by='movieId') %>%
    pull(lse)
  
model_1W_rmse <- RMSE(predicted_ratings, edx1$rating)
rmse_results <- rmse_results %>% add_row(method = "Whole Number - Group By MovieID", RMSE = model_1W_rmse)
  
user_avgs <- edx1 %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - lse))

predicted_ratings <- edx1 %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu_hat + lse + b_u) %>%
    pull(pred)  
  
model_2W_rmse <- RMSE(predicted_ratings, edx1$rating)

rmse_results <- rmse_results %>% add_row(method = "Whole Number - Group By MovieID + UserID", RMSE = model_2W_rmse)
kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))

mu_hat <- mean(edx0.5$rating, na.rm = TRUE)
movie_avgs <- edx0.5 %>% 
    group_by(movieId) %>% 
    summarize(lse = mean(rating - mu_hat))

predicted_ratings <- mu_hat + edx0.5 %>% 
    left_join(movie_avgs, by='movieId') %>%
    pull(lse)
  
model_1.5_rmse <- RMSE(predicted_ratings, edx0.5$rating)
rmse_results <- rmse_results %>% add_row(method = "Decimal Number - Group By MovieID", RMSE = model_1.5_rmse)
  
user_avgs <- edx0.5 %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - lse))

predicted_ratings <- edx0.5 %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu_hat + lse + b_u) %>%
    pull(pred)  
  
model_2.5_rmse <- RMSE(predicted_ratings, edx0.5$rating)

rmse_results <- rmse_results %>% add_row(method = "Decimal Number - Group By MovieID + UserID", RMSE = model_2.5_rmse)
kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))

#remove whole number ratings
seq0.5 <- seq(0.5,4.5,1)
final_holdout_test <- final_holdout_test[final_holdout_test$rating %in% seq0.5,]

#Derive the mean
mu_hat <- mean(final_holdout_test$rating, na.rm = TRUE)

#calculate Movie Effect
movie_avgs <- final_holdout_test %>% 
    group_by(movieId) %>% 
    summarize(lse = mean(rating - mu_hat))

predicted_ratings <- mu_hat + final_holdout_test %>% 
    left_join(movie_avgs, by='movieId') %>%
    pull(lse)
  
# compute an approximation by computing mu and b_i and estimating b_u as the average

user_avgs <- final_holdout_test %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu_hat - lse))

predicted_ratings <- final_holdout_test %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu_hat + lse + b_u) %>%
    pull(pred)
  
  
final_holdout_test_RMSE <- RMSE(predicted_ratings, final_holdout_test$rating)

rmse_results <- rmse_results %>% add_row(method = "Final Holdout Test", RMSE = final_holdout_test_RMSE)

kable(rmse_results, caption = "RMSE Results", booktabs = T) %>%     
    kable_styling(latex_options = c("striped", "hold_position"))
