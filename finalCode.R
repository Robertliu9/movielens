################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Download File
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Create datasets
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Set seed for R versions above 3.5
set.seed(1, sample.kind="Rounding")

# Validation set will be 10% of MovieLens data
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

# Remove unused datasets
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# All libraries required in the rest of the code is located here
if(!require(pbapply)) install.packages("pbapply", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repo = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("tidyverse", repo = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("tidyverse", repo = "http://cran.us.r-project.org")

# Take a preview of the dataset
glimpse(edx)
glimpse(validation)

# Count of distinct users who revied movies
n_distinct(edx$userId)

# Count of distinct movies
edx %>% 
  filter(!is.na(movieId)) %>%
  summarise(uni = n_distinct(movieId))

# Average movie rating
mean(edx$rating)

# Extract genres of movies into a data frame
genresCountTable <- edx %>% tidyr::separate_rows(genres, sep = "\\|") %>%
  dplyr::group_by(genres) %>%
  dplyr::summarize(count = n(), averageRating = mean(rating)) %>%
  plyr::arrange(desc(count))

# Look at genres table
genresCountTable %>% knitr::kable()

# Get count of top 10 most reviewed movies, arranged in descending order
edx %>% group_by(title) %>%
  summarise(count = n(), averageRating = mean(rating)) %>%
  arrange(desc(count)) %>%
  slice(1:10) %>%
  knitr::kable()

# Plot number of times a rating was given in the edx dataset
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_col() +
  labs(title = "Count of Ratings vs Movie Rating",
       x = "Movie Rating",
       y = "Count")

# Plot number of ratings a user
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 20, colour = "white") +
  scale_x_log10(breaks = c(10,25,50,100,250,500,1000,2500,5000,10000)) +
  labs(title = "Number of ratings by users",
       x = "Log10 of Number of Ratings",
       y = "Count of Users")

# Look for missing values
summary(edx)
summary(validation)

# Transform the timestamp value into the useable POSIXct format
edx <- edx %>%
  mutate(timestamp = as_datetime(timestamp))
summary(edx$timestamp)

# Also transform the validation set
validation <- validation %>%
  mutate(timestamp = as_datetime(timestamp))

# Extract year of movie from title from edx and validation sets
edx <- edx %>%
  mutate(year = as.numeric(substr(title, 
                                  nchar(title) - 4,
                                  nchar(title) - 1)))

validation <- validation %>%
  mutate(year = as.numeric(substr(title, 
                                  nchar(title) - 4,
                                  nchar(title) - 1)))

head(edx$year)

# Show the first and latest 10 years of movie releases by the number of reviews
edx %>%
  select(movieId, year) %>%
  group_by(year) %>%
  summarise(countOfReviews = n()) %>%
  arrange(desc(year)) %>%
  slice(1:10) %>% knitr::kable()

edx %>%
  select(movieId, year) %>%
  group_by(year) %>%
  summarise(countOfReviews = n()) %>%
  arrange(year) %>%
  slice(1:10) %>% knitr::kable()

# Graph showing movie reviews by year of movie release
edx %>%
  select(movieId, year) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line() +
  ggtitle("Number of movie reviews by year of release") +
  xlab("Year of movie release") +
  ylab("Reviews")

# Add variable for number of years between release of movie and year of review
edx <- edx %>%
  mutate(reviewLag = year(timestamp) - year)

# Look at new variable
summary(edx$reviewLag)

# Look at negative numbers
edx %>%
  select(title, timestamp, year, reviewLag) %>%
  filter(reviewLag < 0) %>%
  arrange(reviewLag) %>%
  slice(1:10)

# Replace negative numbers with NAs
edx <- edx %>%
  mutate(reviewLag = ifelse(reviewLag < 0, "NA", reviewLag))

# Plot Trends
edx %>% 
  group_by(reviewLag) %>%
  filter(reviewLag != "NA") %>%
  summarise(mean = mean(rating), sqrtOfCount = sqrt(n())) %>%
  ggplot(aes(x = as.numeric(reviewLag), y = mean)) +
  geom_point(aes(size = sqrtOfCount)) +
  ggtitle("Average rating of movies by years between movie release and review") +
  xlab("Years between movie release and review") +
  ylab("Average Rating")

# Convert edx set to numeric and process validation set for modelling, ignore errors for NA's introduced
edx <- edx %>%
  mutate(reviewLag = as.numeric(reviewLag))

validation <- validation %>%
  mutate(reviewLag = as.numeric(ifelse(year(timestamp) - year < 0,
                                       "NA",
                                       year(timestamp) - year)))

# Create a variable in dataframe for every genre, then match for movie
edxGenres <- edx %>% 
  mutate(noGenre = ifelse(str_detect(genres, "no genres listed"), 1, 0),
         Action = ifelse(str_detect(genres, "Action"), 1, 0),
         Adventure = ifelse(str_detect(genres, "Adventure"), 1, 0),
         Animation = ifelse(str_detect(genres, "Animation"), 1, 0),
         Children = ifelse(str_detect(genres, "Children"), 1, 0),
         Comedy = ifelse(str_detect(genres, "Comedy"), 1, 0),
         Crime = ifelse(str_detect(genres, "Crime"), 1, 0),
         Documentary = ifelse(str_detect(genres, "Documentary"), 1, 0),
         Drama = ifelse(str_detect(genres, "Drama"), 1, 0),
         Fantasy = ifelse(str_detect(genres, "Fantasy"), 1, 0),
         FilmNoir = ifelse(str_detect(genres, "Film-Noir"), 1, 0),
         Horror = ifelse(str_detect(genres, "Horror"), 1, 0),
         IMAX = ifelse(str_detect(genres, "IMAX"), 1, 0),
         Musical = ifelse(str_detect(genres, "Musical"), 1, 0),
         Mystery = ifelse(str_detect(genres, "Mystery"), 1, 0),
         Romance = ifelse(str_detect(genres, "Romance"), 1, 0),
         SciFi = ifelse(str_detect(genres, "Sci-Fi"), 1, 0),
         Thriller = ifelse(str_detect(genres, "Thriller"), 1, 0),
         War = ifelse(str_detect(genres, "War"), 1, 0),
         Western = ifelse(str_detect(genres, "Western"), 1, 0))

# Get sum of all genres
colSums(edxGenres[,9:28])

# Check sum of all genres in new df against edx set
genresCountTable

# Add binary genre variables to edx set
edx <- edxGenres

# Complete same steps for the Validation set, creating a validation set with the same predictors
validation <- validation %>% mutate(noGenre = ifelse(str_detect(genres, "no genres listed"), 1, 0),
                                    Action = ifelse(str_detect(genres, "Action"), 1, 0),
                                    Adventure = ifelse(str_detect(genres, "Adventure"), 1, 0),
                                    Animation = ifelse(str_detect(genres, "Animation"), 1, 0),
                                    Children = ifelse(str_detect(genres, "Children"), 1, 0),
                                    Comedy = ifelse(str_detect(genres, "Comedy"), 1, 0),
                                    Crime = ifelse(str_detect(genres, "Crime"), 1, 0),
                                    Documentary = ifelse(str_detect(genres, "Documentary"), 1, 0),
                                    Drama = ifelse(str_detect(genres, "Drama"), 1, 0),
                                    Fantasy = ifelse(str_detect(genres, "Fantasy"), 1, 0),
                                    FilmNoir = ifelse(str_detect(genres, "Film-Noir"), 1, 0),
                                    Horror = ifelse(str_detect(genres, "Horror"), 1, 0),
                                    IMAX = ifelse(str_detect(genres, "IMAX"), 1, 0),
                                    Musical = ifelse(str_detect(genres, "Musical"), 1, 0),
                                    Mystery = ifelse(str_detect(genres, "Mystery"), 1, 0),
                                    Romance = ifelse(str_detect(genres, "Romance"), 1, 0),
                                    SciFi = ifelse(str_detect(genres, "Sci-Fi"), 1, 0),
                                    Thriller = ifelse(str_detect(genres, "Thriller"), 1, 0),
                                    War = ifelse(str_detect(genres, "War"), 1, 0),
                                    Western = ifelse(str_detect(genres, "Western"), 1, 0))

# Clean up environment
rm(edxGenres)

# Average rating by genre in order
genresCountTable %>%
  ggplot(aes(x = reorder(genres, -averageRating), y = averageRating)) +
  geom_point(stat = "identity", aes(size = sqrt(count))) +
  labs(title = "Average rating by genre",
       x = "Genres",
       y = "Average Rating") +
  scale_y_continuous(limits = c(0, 5)) +
  theme(axis.text.x = element_text(angle = 90))

# RMSE function
RMSE <- function(rating, prediction){
  sqrt(mean((rating - prediction)^2))
}

# Set seed for R versions above 3.5
set.seed(17, sample.kind="Rounding")

# Create testing & training set
index <- createDataPartition(y = edx$rating, 
                             times = 1,
                             p = 0.1,
                             list = F)
training <- edx[-index,]
testing <- edx[index,]

# Remove users & movies in test set that do not appear in training set
testing <- testing %>%
  semi_join(training, by = "movieId") %>%
  semi_join(training, by = "userId")

# MODEL 1 - Naive model; Baseline model using total average ratings to predict movie ratings
# Calculate average
average <- mean(training$rating)
average

# Create table to compare RMSEs of different models and add naive mode RMSE
rmseResults <- data_frame(method = "Naive Model", RMSE = RMSE(testing$rating, average))
rmseResults %>% knitr::kable()

# MODEL 2 - Movie Average; Model that adjusts prediction by quantifying average rating of the movie
# Work out residual of movie ratings vs total average
movieAvgs <- training %>%
  group_by(movieId) %>%
  summarise(movieRsd = mean(rating - average))

# Plot of movie residuals vs count of movies
movieAvgs %>% ggplot(aes(movieRsd)) +
  geom_histogram(bins = 20,
                 alpha = .7,
                 col = "black") +
  labs(title = "Count of movie rating residuals against total average",
       x = "Movie Residuals",
       y = "Count")

# Calculate predictions of model
movieAvgsPred <- average + testing %>%
  left_join(movieAvgs, by = 'movieId') %>%
  pull(movieRsd)

# Calculate RMSE of new model, add value to table & compare
rmseResults <- bind_rows(rmseResults,
                         data_frame(method = "Movie Residuals Model", 
                                    RMSE = RMSE(testing$rating, movieAvgsPred)))
rmseResults %>% knitr::kable()

# MODEL 3 - Users Average; Model takes into account users average ratings
# Work out user review averages against total average and the movie residual
userAvgs <- training %>%
  left_join(movieAvgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarise(userRsd = mean(rating - average - movieRsd))

# Build predictions of model
userMoviePred <- testing %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  mutate(pred = average + movieRsd + userRsd) %>%
  pull(pred)

# Calculate RMSE of new model, add value to table & compare
rmseResults <- bind_rows(rmseResults, data_frame(method = "User & Movie Residuals Model", 
                                                 RMSE = RMSE(testing$rating, userMoviePred)))
rmseResults %>% knitr::kable()

# Plot of user residuals vs count of movies
userAvgs %>% ggplot(aes(userRsd)) +
  geom_histogram(bins = 20, alpha = .7, col = "black") +
  labs(title = "Count of user rating residuals against total average",
       x = "User Residuals", y = "Count")

## MODEL 4 - Movie & User & ReviewLag effects
# Work out review time lag averages against total average and the movie & user residual
reviewLagAvgs <- training %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  group_by(reviewLag) %>%
  summarise(reviewLagRsd = mean(rating - average - movieRsd - userRsd))

# Build predictions of model
userMoviePred <- testing %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  left_join(reviewLagAvgs, by = 'reviewLag') %>%
  mutate(pred = average + movieRsd + userRsd + reviewLagRsd) %>%
  pull(pred)

# Calculate RMSE of new model, add value to table & compare
rmseResults <- bind_rows(rmseResults, data_frame(method = "User & Movie & Review Lag Residuals Model", 
                                                 RMSE = RMSE(testing$rating, userMoviePred)))
rmseResults %>% knitr::kable()

# Plot of review lag residuals vs count of movies
reviewLagAvgs %>% ggplot(aes(reviewLagRsd)) +
  geom_histogram(bins = 20, alpha = .7, col = "black") +
  labs(title = "Count of review lag residuals (year) against total average",
       x = "Review Lag Residuals",
       y = "Count review lag years")

## MODEL 5- Movie & User & Review Lag & Genre effects
# Create dataframe to calculate and hold residual for first genre
genreResids <- training %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  left_join(reviewLagAvgs, by = 'reviewLag') %>%
  filter(noGenre == 1) %>%
  summarise(Residual = mean(rating - average - movieRsd - userRsd - reviewLagRsd))

# Loop through, calculate and store residuals for the rest of the genres
for (col in c(10:28)) {
  
  genreResid <- training %>%
    left_join(movieAvgs, by = 'movieId') %>%
    left_join(userAvgs, by = 'userId') %>%
    left_join(reviewLagAvgs, by = 'reviewLag') %>%
    filter(.[[col]] == 1) %>%
    summarise(Residual = mean(rating - average - movieRsd - userRsd - reviewLagRsd))
  
  genreResids <- rbind(genreResids, genreResid)
}

# Take genre residuals and add them to predictions if a movie falls within a genre
GenrePredictions <- testing %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  left_join(reviewLagAvgs, by = 'reviewLag') %>%
  mutate(pred = average + movieRsd + userRsd +
           if_else(noGenre == 1, genreResids$Residual[1], 0) +
           if_else(Action == 1, genreResids$Residual[2], 0) +
           if_else(Adventure == 1, genreResids$Residual[3], 0) +
           if_else(Animation == 1, genreResids$Residual[4], 0) +
           if_else(Children == 1, genreResids$Residual[5], 0) +
           if_else(Comedy == 1, genreResids$Residual[6], 0) +
           if_else(Crime == 1, genreResids$Residual[7], 0) +
           if_else(Documentary == 1, genreResids$Residual[8], 0) +
           if_else(Drama == 1, genreResids$Residual[9], 0) +
           if_else(Fantasy == 1, genreResids$Residual[10], 0) +
           if_else(FilmNoir == 1, genreResids$Residual[11], 0) +
           if_else(Horror == 1, genreResids$Residual[12], 0) +
           if_else(IMAX == 1, genreResids$Residual[13], 0) +
           if_else(Musical == 1, genreResids$Residual[14], 0) +
           if_else(Mystery == 1, genreResids$Residual[15], 0) +
           if_else(Romance == 1, genreResids$Residual[16], 0) +
           if_else(SciFi == 1, genreResids$Residual[17], 0) +
           if_else(Thriller == 1, genreResids$Residual[18], 0) +
           if_else(War == 1, genreResids$Residual[19], 0) +
           if_else(Western == 1, genreResids$Residual[20], 0)) %>%
  pull(pred)

# Calculate RMSE of new model, add value to table & compare
rmseResults <- bind_rows(rmseResults, data_frame(method = "User & Movie & Review Lag & Genres Residuals Model", 
                                                 RMSE = RMSE(testing$rating, GenrePredictions)))
rmseResults %>% knitr::kable()

# MODEL 6 - Regularisation; Model penalises large estimates from small sample sizes
# Sequence of lambdas to test
lambdas <- seq(0, 10, .25)

# Run sequence through model and calculate all their RMSEs, with progress bar
lambdaRMSEs <- pbsapply(lambdas, function(l){
  
  averageRating <- mean(training$rating)
  
  movieAvgs <- training %>%
    group_by(movieId) %>%
    summarise(movieRsds = sum(rating - average) / (n() + l))
  
  userAvgs <- training %>%
    left_join(movieAvgs, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(userRsds = sum(rating - movieRsds - average) / (n() + l))
  
  reviewLagAvgs <- training %>%
    left_join(movieAvgs, by = 'movieId') %>%
    left_join(userAvgs, by = 'userId') %>%
    group_by(reviewLag) %>%
    summarise(reviewLagRsds = sum(rating - average - movieRsds - userRsds) / (n() + l))
  
  predictions <- testing %>%
    left_join(movieAvgs, by = 'movieId') %>%
    left_join(userAvgs, by = 'userId') %>%
    left_join(reviewLagAvgs, by = 'reviewLag') %>%
    mutate(pred = average + movieRsds + userRsds + reviewLagRsds) %>%
    pull(pred)
  
  return(RMSE(testing$rating, predictions))
})

# Plot of lambdas & select lambda which gives lowest RMSE
qplot(lambdas, lambdaRMSEs)
lambda <- lambdas[which.min(lambdaRMSEs)]
lambda

# Work out new residuals with generated lambda
movieAvgs <- training %>%
  group_by(movieId) %>%
  summarise(movieRsds = sum(rating - average) / (n() + lambda))

userAvgs <- training %>%
  left_join(movieAvgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarise(userRsds = sum(rating - movieRsds - average) / (n() + lambda))

reviewLagAvgs <- training %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  group_by(reviewLag) %>%
  summarise(reviewLagRsd = sum(rating - average - movieRsds - userRsds) / (n() + lambda))

regPred <- testing %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  left_join(reviewLagAvgs, by = 'reviewLag') %>%
  mutate(pred = average + movieRsds + userRsds + reviewLagRsd) %>%
  pull(pred)

# Calculate RMSE of new model, add value to table & compare
rmseResults <- bind_rows(rmseResults,
                         data_frame(method = "User & Movie & Review Lag Regularised Model", 
                                    RMSE = RMSE(testing$rating, regPred)))
rmseResults %>% knitr::kable()

# MODEL 7 - Matrix Factorisation; 
# Clear unused memory
invisible(gc())

# Create a matrix copy to disk of training and testing set where we only take userID, movieID & ratings in correct format for recosystem
train2 <- training %>%
  select(userId, movieId, rating) %>%
  as.matrix() %>%
  write.table(file = "train2.txt", sep = " ", row.names = F, col.names = F)

test2 <- testing %>%
  select(userId, movieId, rating) %>%
  as.matrix() %>%
  write.table(file = "test2.txt", sep = " ", row.names = F, col.names = F)

# Build reco object used to construct recommender model and conduct prediction.
mfModel <- Reco()

# Load data in format reco requires
set.seed(17, sample.kind = "Rounding")
trainSet <- data_file(paste0(getwd(), "/train2.txt"), index1 = FALSE)
testSet <- data_file(paste0(getwd(), "/test2.txt"), index1 = FALSE)

# Tune RECO parameters, pararmeters optimised to lower computation time
opts <- mfModel$tune(trainSet, 
                     opts = list(dim = c(1:15), # Number of latent factors
                                 lrate = c(0.1, 0.2), # Learning rate, step size of gradient decent
                                 costp_l1 = 0, 
                                 costq_l1 = 0,
                                 nthread = 4,
                                 niter = 10,
                                 verbose = FALSE))

# Train RECO model
mfModel$train(trainSet, opts = c(opts$min, nthread = 4, niter = 30))

# Create Predictions
predFile = tempfile()
mfModel$predict(testSet, out_file(predFile))

# Create prediction data and real review results data set
scoresReal <- read.table("test2.txt", header = FALSE, sep = " ")$V3
scoresPred <- scan(predFile)

# Find RMSE
rmseResults <- bind_rows(rmseResults, data_frame(method = "Matrix Factorisation with Gradient Descent", 
                                                 RMSE = RMSE(scoresReal, scoresPred)))
rmseResults %>% knitr::kable()

# FINAL MODEL 1 - Regularisation;
# Sequence of lambdas to test
lambdas <- seq(3, 8, .1)

# Run sequence through model and calculate all their RMSEs, with progress bar
lambdaRMSEs <- pbsapply(lambdas, function(l){
  
  averageRating <- mean(edx$rating)
  
  movieAvgs <- edx %>%
    group_by(movieId) %>%
    summarise(movieRsds = sum(rating - average) / (n() + l))
  
  userAvgs <- edx %>%
    left_join(movieAvgs, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(userRsds = sum(rating - movieRsds - average) / (n() + l))
  
  reviewLagAvgs <- edx %>%
    left_join(movieAvgs, by = 'movieId') %>%
    left_join(userAvgs, by = 'userId') %>%
    group_by(reviewLag) %>%
    summarise(reviewLagRsds = sum(rating - average - movieRsds - userRsds) / (n() + l))
  
  predictions <- validation %>%
    left_join(movieAvgs, by = 'movieId') %>%
    left_join(userAvgs, by = 'userId') %>%
    left_join(reviewLagAvgs, by = 'reviewLag') %>%
    mutate(pred = average + movieRsds + userRsds + reviewLagRsds) %>%
    pull(pred)
  
  return(RMSE(validation$rating, predictions))
})

# Plot of lambdas & select lambda which gives lowest RMSE
qplot(lambdas, lambdaRMSEs)
lambda <- lambdas[which.min(lambdaRMSEs)]
lambda

# Work out new residuals with generated lambda
movieAvgs <- edx %>%
  group_by(movieId) %>%
  summarise(movieRsds = sum(rating - average) / (n() + lambda))

userAvgs <- edx %>%
  left_join(movieAvgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarise(userRsds = sum(rating - movieRsds - average) / (n() + lambda))

reviewLagAvgs <- edx %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  group_by(reviewLag) %>%
  summarise(reviewLagRsd = sum(rating - average - movieRsds - userRsds) / (n() + lambda))

finalRegPred <- validation %>%
  left_join(movieAvgs, by = 'movieId') %>%
  left_join(userAvgs, by = 'userId') %>%
  left_join(reviewLagAvgs, by = 'reviewLag') %>%
  mutate(pred = average + movieRsds + userRsds + reviewLagRsd) %>%
  pull(pred)

# Calculate RMSE of new model, add value to table & compare
rmseResults <- bind_rows(rmseResults,
                         data_frame(method = "Edx Set User & Movie & Review Lag Regularised Model on Validation Data", 
                                    RMSE = RMSE(validation$rating, finalRegPred)))
rmseResults %>% knitr::kable()

# FINAL MODEL 2 - Matrix Factorisation;
# Clear unused memory
invisible(gc())

# Create a matrix copy to disk of training and testing set where we only take userID, movieID & ratings in correct format for recosystem
edxTrain <- edx %>%
  select(userId, movieId, rating) %>%
  as.matrix() %>%
  write.table(file = "edx.txt", sep = " ", row.names = F, col.names = F)

valid <- validation %>%
  select(userId, movieId, rating) %>%
  as.matrix() %>%
  write.table(file = "valid.txt", sep = " ", row.names = F, col.names = F)

# Build reco object used to construct recommender model and conduct prediction.
mfModel <- Reco()

# Load data in format reco requires
set.seed(17, sample.kind = "Rounding")
edxSet <- data_file(paste0(getwd(), "/edx.txt"), index1 = FALSE)
validSet <- data_file(paste0(getwd(), "/valid.txt"), index1 = FALSE)

# Tune RECO parameters, pararmeters optimised to lower computation time
opts <- mfModel$tune(edxSet, 
                     opts = list(dim = c(1:15), # Number of latent factors
                                 lrate = c(0.1, 0.2), # Learning rate, step size of gradient decent
                                 costp_l1 = 0, 
                                 costq_l1 = 0,
                                 nthread = 4,
                                 niter = 10,
                                 nfold = 10, # Use 10 fold cross validation
                                 verbose = FALSE))

# Train RECO model
mfModel$train(edxSet, opts = c(opts$min, nthread = 4, niter = 30))

# Predictions
predFile = tempfile()
mfModel$predict(validSet, out_file(predFile))

# Create prediction data and real review results data set
scoresReal <- read.table("valid.txt", header = FALSE, sep = " ")$V3
scoresPred <- scan(predFile)

# Calculate RMSE
rmseResults <- bind_rows(rmseResults,
                         data_frame(method = "Edx Set Matrix Factorisation with Gradient Descent on Validation Data", 
                                    RMSE = RMSE(scoresReal, scoresPred)))
rmseResults %>% knitr::kable()