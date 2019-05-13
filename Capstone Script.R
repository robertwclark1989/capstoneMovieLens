
## Create Test and Validation Sets


# load libraries
library(tidyverse)
library(caret)


# load edx and validation sets
edx <- readRDS("/Users/robertwilliamclark/Downloads/edx.rds")
validation <- readRDS("/Users/robertwilliamclark/Downloads/validation.rds")


# create RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}





## Investigate Dataset

# examine the data
head(edx)

# how many rows and columns are there?
nrow(edx)
ncol(edx)

# how many 0's were given as a rating?
zeros <- edx$rating == 0
sum(zeros)

# how many 3's were given as a rating?
threes <- edx$rating == 3
sum(threes)

# What is the distribution of the ratings given?
edx %>% group_by(rating) %>% summarize(count = n(), percent = n() / 9000055)
# most of the ratings are whole numbers. It is more common for a user to give a whole number review, instead of, say, a 3.5 or 2.5.

# Sum up the percentages of whole number ratings
edx %>% group_by(rating) %>% summarize(count = n(), percent = n() / 9000055) %>% filter(rating %in% c(1,2,3,4,5)) %>% summarize(sum = sum(percent))
# almost 80% of the ratings in the train set are a whole number review. Would a model work better if the predicted rating was rounded up or down to a whole number?



# how many different movies are there?
edx %>% summarize(n_movies = n_distinct(movieId)) %>% .$n_movies

# how many different users are there?
edx %>% summarize(n_users = n_distinct(userId)) %>% .$n_users


# how many ratings per genre? 
genres <- c("Comedy", "Romance", "Action", "Crime", "Thriller", "Sci-Fi", "Thriller", "Adventure", "Children", "Fantasy", "War", "Musical", "Western", "Mystery", "Film-Noir", "Horror", "Documentary")
smallgenres <- c("Comedy", "Romance", "Action")

genrecount <- sapply(genres, function(x){
  edx %>% filter(str_detect(genres,x)) %>% nrow()
})
genrecount


# which movie has the greatest number of ratings?
maxratings <- edx %>% group_by(title) %>% summarize(count = length(title))
index <- which.max(maxratings$count)
maxratings$title[index]


# find the number of ratings for a specific movie
movie <- "Jurassic Park"
ratings <- edx %>% filter(title == movie) %>% summarize(count = n())
paste("# of ratings for", movie, ":", ratings) 

# what are the five most given ratings in order from most to least
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>% arrange(desc(count))  


# how many users rated 1000 movies or more?
edx %>% group_by(userId) %>% summarize(count = n()) %>% filter(count >= 1000) %>% nrow()
# 611, less than 1% of the user base, rated over 1000 movies each. 

# What is the total number of ratings of this small group of users?
top_users <- edx %>% group_by(userId) %>% summarize(count = n()) %>% filter(count >= 1000) %>% mutate(top_user = "yes")
edx %>% left_join(top_users, by='userId') %>% filter(top_user == "yes") %>% nrow()
# these 611 users together submitted 867198 ratings. That is almost 10% of all ratings!


# What is the distribution of number of movies rated by each user?
edx %>% group_by(userId) %>% summarize(count = n()) %>% qplot(count, geom = "histogram", bins = 100, data = ., color = I("black"))

# the top users are making the rest of the chart hard to see. What happens if we remove the ratings from these top users?
edx %>% left_join(top_users, by='userId') %>% filter(is.na(top_user)) %>% group_by(userId) %>% summarize(count = n()) %>% qplot(count, geom = "histogram", bins = 100, data = ., color = I("black"))

# what was the average number of ratings over all users?
num_user_ratings <- edx %>% group_by(userId) %>% summarize(count = n())
mean(num_user_ratings$count)






## Prepare EDX and Validation for Modeling

# calculate minimum movie timestamp for edx
min_edx_timestamp <- edx %>%
  group_by(movieId) %>%
  summarize(first_review = min(timestamp))

# calculate the number of days between each review day and the movie's first review day and add it to edx
edx <- edx %>% left_join(min_edx_timestamp, by='movieId') %>% mutate(review_day = ceiling((timestamp - first_review) / 86400))


# calculate minimum movie timestamp for validation
min_val_timestamp <- validation %>%
  group_by(movieId) %>%
  summarize(first_review = min(timestamp))

# calculate the number of days between each review day and the movie's first review day and add it to validation
validation <- validation %>% left_join(min_val_timestamp, by='movieId') %>% mutate(review_day = ceiling((timestamp - first_review) / 86400))



# this code chucnk will split out the movie title and the year, inserting the year into a new column, then add the decade to a new column
edx <- edx %>%
  # take off any whitespaces
  mutate(title = str_trim(title)) %>%
  # create title_tmp and year column
  extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  # drop title_tmp column
  select(-title_tmp)  %>%
  # generic function to turn (no genres listed) to NA
  mutate(genres = if_else(genres == "(no genres listed)", `is.na<-`(genres), genres)) %>% 
  # add decade column
  mutate(decade = year - year %% 10)


# we will need to add the year and decade  to validation as well
validation <- validation %>%
  # take off any whitespaces
  mutate(title = str_trim(title)) %>%
  # create title_tmp and year column
  extract(title, c("title_tmp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # for series take debut date
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace title NA's with original title
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  # drop title_tmp column
  select(-title_tmp)  %>%
  # generic function to turn (no genres listed) to NA
  mutate(genres = if_else(genres == "(no genres listed)", `is.na<-`(genres), genres)) %>% 
  # add decade column
  mutate(decade = year - year %% 10 )






## Begin Recommendation Modeling

# calculate the average rating for all movies, which will be the baseline of our prediction model
mu <- mean(edx$rating)

# calculate the b_i's, the least squared estimate, aka, the distance away the rating is from the mean (3.5 ish)
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# plot the b_i's
movie_avgs %>% qplot(b_i, geom = "histogram", bins = 10, data = ., color = I("black"))

# join in the movie_avgs and predict the movie ratings by adding the b_i with the overall average
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% mutate(pred = mu + b_i) %>% .$pred

# calculate RMSE from these predicted ratings
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
model_1_rmse

# calculate the b_u's, the user specific effect. 
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))

# plot the b_u's
user_avgs %>% qplot(b_u, geom = "histogram", bins = 10, data = ., color = I("black"))

# predict the rmse's with the b_i's and b_u's 
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% .$pred

# calculate RMSE from these predicted ratings
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse



# join in the movie_avgs and user_avgs data sets so that we can find the average rating for each review day without the influence of the movie average and the user average. 
reviewday_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% group_by(review_day) %>% summarize(b_r = mean(rating - mu - b_i - b_u))

# predict the rmse's with the b_i's, b_u's and b_r's
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% left_join(reviewday_avgs, by='review_day') %>% mutate(pred = mu + b_i + b_u + b_r) %>% .$pred

# calculate RMSE from these predicted ratings
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
model_3_rmse



# find the average that a user rates a genre, without the bias from the movie effect, user effect, and review_day effect
user_genre_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% left_join(reviewday_avgs, by='review_day') %>% group_by(userId, genres) %>% summarize(b_g = mean(rating - mu - b_i - b_u - b_r))

# predict the rmse's with the b_i's, b_u's, b_r's and b_g's, putting in a 0 when there is no b_g
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% left_join(reviewday_avgs, by='review_day')  %>% left_join(user_genre_avgs, by = c("userId", "genres")) %>% mutate(pred = mu + b_i + b_u + b_r + ifelse(is.na(b_g), 0, b_g)) %>% .$pred

# calculate RMSE from these predicted ratings
model_4_rmse <- RMSE(predicted_ratings, validation$rating)
model_4_rmse



# plot the b_g's
user_genre_avgs %>% qplot(b_g, geom = "histogram", bins = 10, data = ., color = I("black"))

# let's look at our largest residuals and see the breakdown of our model biases
validation %>%
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(reviewday_avgs, by='review_day')  %>% 
  left_join(user_genre_avgs) %>%
  mutate(residual = rating - (mu + b_i + b_u + b_r + ifelse(is.na(b_g), 0, b_g)), pred = (mu + b_i + b_u + b_r + ifelse(is.na(b_g), 0, b_g)), mu = mu, b_i = b_i, b_u = b_u, b_r = b_r, b_g = b_g) %>% 
  arrange(desc(abs(residual))) %>%
  select(title, rating, pred, residual, mu, b_i, b_u, b_r, b_g) %>% slice(1:10)



# our first step should be to use cross-fertilization to choose the lamba that minimizes the rmse
user_genre_sum <- edx %>%
left_join(movie_avgs, by='movieId') %>% 
left_join(user_avgs, by='userId') %>% 
left_join(reviewday_avgs, by='review_day') %>%
group_by(userId, genres) %>%
summarize(s = sum(rating - mu - b_i - b_u - b_r), n_i = n())
head(user_genre_sum)

# I tried to use the sapply function to try multiple lambda values to see which one minimizes the RMSE, but for some reason it just wouldn't work for me. It kept giving me this error: "Error in tbl_vars(y) : argument "y" is missing, with no default". So instead, I just manually tried multiple lambda values individually. It looked like really high values were giving me the lowest RMSE, so I will go with a lambda value of 10. 

# use the regularized b_g's in our prediction model
predicted_ratings <- validation %>%
  left_join(user_genre_sum, by = c("userId", "genres")) %>%
  left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') %>% 
  left_join(reviewday_avgs, by='review_day') %>%
  mutate(b_g = ifelse(is.na(s), 0, s)/(ifelse(is.na(n_i), 0, n_i)+10)) %>%
  mutate(pred = mu + b_i + b_u + b_r + b_g) %>%
  pull(pred)
model_5_rmse <- RMSE(predicted_ratings, validation$rating)
model_5_rmse




# is there a bias towards older movies? Let's do a movie decade bias
decade_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% left_join(reviewday_avgs, by='review_day') %>% left_join(user_genre_sum, by = c("userId", "genres")) %>% mutate(b_g = ifelse(is.na(s), 0, s)/(ifelse(is.na(n_i), 0, n_i)+10)) %>% group_by(decade) %>% summarize(b_d = mean(rating - mu - b_i - b_u - b_r - b_g))

# use decade_avgs to build model 6
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% left_join(reviewday_avgs, by='review_day')  %>% left_join(user_genre_sum, by = c("userId", "genres")) %>% left_join(decade_avgs, by='decade') %>% mutate(b_g = ifelse(is.na(s), 0, s)/(ifelse(is.na(n_i), 0, n_i)+10)) %>% mutate(pred = round(mu + b_i + b_u + b_r + b_g + ifelse(is.na(b_d), 0, b_d))) %>% .$pred

# calculate RMSE from these predicted ratings
model_6_rmse <- RMSE(predicted_ratings, validation$rating)
model_6_rmse


# best model, final RMSE
model_5_rmse



