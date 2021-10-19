########################################################################################################
#
#                                       EDX PROJECT: MOVILENS RECOMMENDATION SYSTEM
#
#
# ######################################################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RcolorBrewer", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(Matrix)
library(caret)
library(data.table)
library(lubridate)
library(RColorBrewer)
library(recommenderlab)


# #######################################################################################################
# 
#  CREATE EDX SET and VALIDATION SET final hold-out test set)
# 
########################################################################################################

# Download the MovieLens 10M dataset from http://files.grouplens.org/datasets/movielens/ml-10m.zip
# includes ratings.dat and movies.dat

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# each line of ratings.dat represents one rating in the form UserID::MovieID::Rating::Timestamp
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# each line of movies.dat represents one movie in the form MovieID::Title::Genres
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

# add column names
colnames(movies) <- c("movieId", "title", "genres")

# store information in movies dataframe. using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# join the two tables by movieID, but only keep rows that have a rating (left_join)
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

# remove tmp and files that are no longer needed
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#####################################################################################################
# 
# DATA EXPLORATION AND VISUALIZATION
#
#####################################################################################################

edx %>% as_tibble() # view structure of the edx dataset and key attributes

edx %>% summarize (ratingst=nrow(.),     # determine number of ratings
                   users= n_distinct(userId),  # number of unique users that provided a rating
                   movies=n_distinct(movieId),  # number of unique movies that were rated
                   genres=n_distinct(genres))  # number of unique genres including combinations


#  Relationship between MOVIES and RATINGS
#----------------------------------------------------------------------------------------------------

# Explore if some movies are rated more often than others
movie_counts<- edx %>%
  group_by(movieId) %>%
  summarize(ratings_count=n()) %>%
  ggplot(aes(ratings_count)) +
  geom_histogram(bins = 30, color = "black") +
  theme_gray (base_size= 20)+
  ggtitle("Number of Ratings per Movie") + xlab("number of ratings") + ylab("count of movies")

movie_counts
ggsave("figures/movie_counts.png", movie_counts, dpi=300)

# Determine if average ratings are uniform or if some movies have higher averages than others.
movie_averages <- edx %>%
  group_by(movieId) %>%
  summarize(rating_avg=mean(rating)) %>%
  ggplot(aes(rating_avg)) +
  theme_gray(base_size= 20)+
  geom_histogram(bins = 30, color = "black") +
  ggtitle("Average Rating per Movie") + xlab("average rating") + ylab("count of movies")

movie_averages
ggsave("figures/movie_averages.png", movie_averages, dpi=300)


#  Relationship between USERS and RATINGS
#---------------------------------------------------------------------------------------------------

# Determine if some users rate more movies than others
user_counts <- edx %>%
  group_by(userId) %>%
  summarize(ratings_count=n()) %>%
  ggplot(aes(ratings_count)) +
  geom_histogram(bins = 30, color = "black") +
  theme_gray (base_size= 20)+
  ggtitle("Number of Ratings per User") + xlab("number of ratings") + ylab("count of users")

user_counts
ggsave("figures/user_counts.png", user_counts, dpi=300)

# Explore is some users are more generous with their ratings than others 
user_averages <- edx %>%
  group_by(userId) %>%
  summarize(rating_avg=mean(rating)) %>%
  ggplot(aes(rating_avg)) +
  geom_histogram(bins = 30, color = "black") +
  theme_gray (base_size= 20)+
  ggtitle("Average Rating per User") + xlab("average rating") + ylab("count of users")

user_averages
ggsave("figures/user_averages.png", user_averages, dpi=300)

# Relationship between GENRES and RATINGS
#-----------------------------------------------------------------------------------------------------
# Evaluate if some genres get more ratings than others

# Separate first (main) genre into genre1, and keep the rest under genre2
edx<- edx %>%
  separate(genres, sep = "\\|",into=c("genre1", "genre2"), extra="merge", fill="right",remove=FALSE)

genre_counts <- edx %>%
  group_by(genre1) %>%
  summarize(ratings_count=n()) %>%
  mutate(genre1 = reorder(genre1, ratings_count)) %>%
  ggplot(aes(ratings_count, genre1)) +
  geom_bar(stat="identity") +
  theme_gray (base_size= 20)+
  ggtitle("Number of Ratings per Genre") + xlab("number of ratings") + ylab("movie genres")

genre_counts
ggsave("figures/genre_counts.png", genre_counts, dpi=300)


# Visualize with boxplot if some genres elicit higher averages
genre_averages <- edx %>%
  group_by(genre1) %>%
  filter(n()>100)  %>%
  summarize(n=n(), avg=mean(rating), se=sd(rating)/sqrt(n())) %>%  # mean  &standard error per genre
  mutate(genre1 = reorder(genre1, avg)) %>%
  ggplot(aes(x = genre1, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  coord_flip()+
  theme_gray (base_size= 20) +
  ggtitle("Average Rating by Genre") + xlab("movie genres") + ylab("average rating")

genre_averages
ggsave("figures/genre_averages.png", genre_averages, dpi=300)


# Relationship between TIME and RATINGS
#-------------------------------------------------------------------------------------------------------
# Convert timestamp of rating into date, week, and day of week and add to edx
edx <- mutate(edx, date = as_datetime(timestamp),
              week=round_date(date, "week"),
              weekday = weekdays(as_datetime(timestamp)))

# Visualize week over week changes in ratings
time_week<- edx %>%
  group_by(week) %>%
  summarize(rating_avg=mean(rating)) %>%
  ggplot(aes(week, rating_avg)) + geom_point()+ geom_smooth()+
  theme_gray (base_size= 20)+
  ggtitle("Average Rating Week over Week") +
  xlab("week year") + ylab("average rating")

time_week
ggsave("figures/time_week.png", time_week, dpi=300)


# Visualized with boxplot changes in rating due to day of the week.
time_weekday <- edx %>%
  group_by(weekday) %>%
  summarize(n=n(), avg=mean(rating), se=sd(rating)/sqrt(n())) %>%  # mean and standard error
  mutate(weekday = reorder(weekday, avg)) %>%
  ggplot(aes(x = weekday, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  coord_flip() +
  theme_gray (base_size= 20)+
  ggtitle("Average Rating by Day of the Week") +
  xlab("week day") + ylab("average rating")

time_weekday

ggsave("figures/time_weekday.png", time_weekday, dpi=300)

save(edx, file="rdas/edx.rda")  # save edx for loading into rmarkdown


# GROUP PATTERNS
#----------------------------------------------------------------------------------------------------------
# Determine if there are groups of movies or groups of users with similar ratings patterns.

# Use subset of data consisting of the top 50 movies 
top <- edx %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%   # top 50 movies
  pull(movieId)

# consider only users that have rated at least 25 of the top movies
small_edx <- edx %>%
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%  
  ungroup() %>%
  select(title, userId, rating) %>% spread(userId, rating)

row_names <- str_remove(small_edx$title, ": Episode") %>% str_trunc(20)

# convert to matrix
small_edx<- as.matrix(sapply(small_edx[ ,-1], as.numeric))

# normalize by removing the average from columns and rows
small_edx <- sweep(small_edx, 2, colMeans(small_edx, na.rm = TRUE))
small_edx <- sweep(small_edx, 1, rowMeans(small_edx, na.rm = TRUE))

# add titles as row names
rownames(small_edx) <- row_names


# Clusters of Movies with similar ratings
#-----------------------------------------------------

# calculate the distance between movies based on ratings
d<- dist(small_edx)

#cluster movies into groups by proximity
h<- hclust(d)

# summarize clustering information with a dendrogram
plot(h, cex = 0.65, main = "", xlab = "")  

# cut the tree into 10 groups
groups <- cutree(h, k = 10)  # generate  6 groups

# display groups to identify trends
names(groups)[groups==2]  # comedies
names(groups)[groups==3]  # family movies
names(groups)[groups==4]  # crime & drama
names(groups)[groups==5]  # blockbusters
names(groups)[groups==9]  # romantic

save(d, file="rdas/distance.rda")  # save for loading into markdown

# Clusters of Users with similar rating patters
#----------------------------------------------
# For our evaluation, only include users with high variability in their ratings
# for whom movies are not all the same.

ordercols<- order(matrixStats::colSds(small_edx, na.rm = TRUE), decreasing = TRUE)[1:25]
x_small<- small_edx[,ordercols]

h2 <- dist(t(x_small)) %>% hclust()   # cluster users in groups with similar rating patterns
save(h2, file="rdas/h2.rda")  # save for loading into markdown

# plot results using a dendrogram
cluster_users <- plot(h2, cex = 0.65, main = "", xlab = "")

#create 5 groups
groups <- cutree(h2, k = 5) 

# display users with similar ratings in first group
users_group1<- names(groups)[groups==1]   


# Visualize cluster of movies and users with a heatmap
# -------------------------------------------------------------
# display heatmap on screen
heatmap(x_small,
        col = colorRampPalette(brewer.pal(8,"Blues"))(4))

#save heatmap to file for display in markdown
png('figures/heatmap.png') 
heatmap(x_small,
        col = colorRampPalette(brewer.pal(9,"Blues"))(4),
        keep.dendro=FALSE)

legend(x = "topleft",     # include legend
       legend = c("not rated", "low rating", "medium rating", "high rating"),
       cex = 1.0, fill = colorRampPalette(brewer.pal(8, "Blues"))(4))
dev.off()


#########################################################################################################
# 
#
#  BUILDING THE ALGORITHM - MODELING FOR EFFECTS / BIASES
#
#
#########################################################################################################


# DIVIDING EDX into TRAINING and TESTING sets
# ------------------------------------------------------------------------------------------------------
# 
# Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in the training set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set set back into the training set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# remove temp and files that are no longer needed
rm(temp, test_index, removed)


#  BASELINE RATING: Average
#----------------------------------------------------------------------------------------------------------
# Start with a basic model that predicts the same value mu for all unknown ratings: Yui= mu + eui
# with mu the average of known ratings and eui the random error that explains variations

mu<- mean(train_set$rating)  # average of all ratings across all movies and users.

RMSE<- sqrt(mean((test_set$rating - mu)^2))   # calculate RMSE of predicting mu for unknown ratings

# #create dataframe to store RMSE results
results<- data.frame(MODEL= character(9), RMSE=numeric(9))
results$MODEL[1]<-"Average"
results$RMSE[1] <- round(RMSE,5)


#  ADJUST for MOVIE EFFECTS
#--------------------------------------------------------------------------------------------------------
#  Augment model to adjust for the variability bi in movie-to-movie ratings: Yui= mu + bi + eui
# calculate the bi(s) as the average variation in rating by movie from the average mu

bis<- train_set %>%   
  group_by(movieId) %>%
  summarize(bi = mean(rating- mu))

# make predictions for unknown ratings (test_set) using: Yui = mu + bi
predictions<- mu + test_set %>%
  left_join(bis, by="movieId") %>%
  pull(bi)

RMSE<- sqrt(mean((test_set$rating - predictions)^2))  # evaluate RMSE of the predictions

results$MODEL[2]<-"Movies Effects"
results$RMSE[2] <- round(RMSE,5)


# ADJUST for USER EFFECT
#--------------------------------------------------------------------------------------------------------
# Add adjustments for the variability bu in users ratings:  Yui= mu + bi + bu + eui
# calculate bu(s) as average difference in user's ratings after removing movie effects

bus<- train_set %>%    
  left_join(bis, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = mean(rating- mu- bi))

# make predictions for unknown ratings (test_set) using: Yui = mu + bi +  bu
predictions<- test_set %>%    #
  left_join(bis, by="movieId") %>%
  left_join(bus, by="userId") %>%
  summarize(pred= mu + bi+ bu) %>%
  pull(pred)

RMSE<- sqrt(mean((test_set$rating - predictions)^2))  # calculate RMSE

results$MODEL[3]<-"Movies + Users Effects"
results$RMSE[3] <- round(RMSE,5)

# REGULARIZATION OF MOVIE & USER EFFECTS
#---------------------------------------------------------------------------------------------------------
# Improve model by "penalizing" large estimates formed using movies or users with small number of ratings
# Select the regularization parameter lambda that optimizes RMSE for both movie & user effects:

lambdas <- seq(0, 6, 0.05)    # use cross-validation

rmses <- sapply(lambdas, function(l){
  mu<-mean(train_set$rating)
  
  reg_bis <- train_set%>%
    group_by(movieId) %>%
    summarize(reg_bi = sum(rating - mu)/(n()+l))
  
  reg_bus <- train_set %>%
    left_join(reg_bis, by="movieId") %>%
    group_by(userId) %>%
    summarize(reg_bu = sum(rating - reg_bi - mu)/(n()+l))
  
  predictions <-test_set %>%
    left_join(reg_bis, by = "movieId") %>%
    left_join(reg_bus, by = "userId") %>%
    mutate(pred = mu + reg_bi + reg_bu) %>%
    pull(pred)
  
  return(sqrt(mean((test_set$rating - predictions)^2))) })

# plot the regularization parameter lambda vs RMSE
qplot(lambdas, rmses)

# select the lambda that minimizes the RMSE error
lambda<- lambdas[which.min(rmses)]

# The RMSE is minimized by using the optimal lambda
RMSE<-min(rmses)

#results[4, ]<-c("EFFECTS: Reg Movies + Reg Users", round(RMSE,5))  # store results
results$MODEL[4]<-"Reg Movies + Reg users Effects"
results$RMSE[4] <- round(RMSE,5)

save(lambdas, file="rdas/lambdas.rda")
save(rmses, file="rdas/rmses_l.rda")

# re-calculate reg_bis and reg_bus with the optimized lambda
reg_bis<- train_set %>%
  group_by(movieId) %>%
  summarise(reg_bi =  sum(rating - mu)/(n()+lambda))

reg_bus <- train_set %>%
  left_join(reg_bis, by="movieId") %>% group_by(userId) %>%
  summarize(reg_bu = sum(rating - reg_bi - mu)/(n()+lambda))


# ADJUST for GENRE EFFECT
#---------------------------------------------------------------------------------------------------------
# Augment model to adjust for variability in genre to genre gui ratings:   Yui= mu + reg_bi + reg_bu + gui+ eui,

# gui(s) are the average difference in ratings by genre after removing regularized movie & user effects
guis<- train_set %>%   
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  group_by(genres) %>%
  summarise(gui = mean(rating- mu- reg_bi- reg_bu))

# make predictions for unknown ratings in the test_set using: Yui= mu + reg_bi + reg_bu + gui
predictions<- test_set %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  left_join(guis, by="genres") %>%
  summarize(pred= mu + reg_bi+ reg_bu + gui) %>%
  pull(pred)

RMSE<- sqrt(mean((test_set$rating - predictions)^2))  # evaluate the RMSE of the predictions

results$MODEL[5]<-"Reg Movies + Reg Users + Genres Effects"
results$RMSE[5] <- round(RMSE,5)

# ADJUST for TIME EFFECT
#------------------------------------------------------------------------------------------------------------
# Adjust for variability of ratings by time  dui:  Yui = mu + reg_bi + reg_bu  + gui + dui + eui

# dui(s) are the average difference in ratings by time, after removing user, movie and genre effects.
duis<- train_set %>%  
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  left_join(guis, by="genres")  %>%
  group_by(week) %>%
  summarise(dui = mean(rating- mu- reg_bi- reg_bu- gui))

# make predictions for unknown ratings using: Yui = mu + reg_bi + reg_bu  + gui + dui
predictions<- test_set %>%
  mutate(date = as_datetime(timestamp),
         week=round_date(date, "week")) %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  left_join(guis, by="genres") %>%
  left_join(duis, by="week") %>%
  summarize(pred= mu + reg_bi+ reg_bu + gui + dui) %>%
  pull(pred)

RMSE<- sqrt(mean((test_set$rating - predictions)^2))  # evaluate the RMSE of the predictions

results$MODEL[6]<-"Reg Movies + Reg Users + Genres + Time Effects"
results$RMSE[6] <- round(RMSE,5)

###################################################################
#
# EFFECTS MODEL VALIDATION TEST
#
##################################################################

# Use full edx set and recalculate parameters- reg_bis, reg_bus, guis and duis- with the optimal lambda

mu<- mean(edx$rating)

reg_bis<- edx %>%
  group_by(movieId) %>%
  summarise(reg_bi =  sum(rating - mu)/(n()+lambda))

reg_bus <- edx %>%
  left_join(reg_bis, by="movieId") %>% 
  group_by(userId) %>%
  summarize(reg_bu = sum(rating - reg_bi - mu)/(n()+lambda))

guis<- edx %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  group_by(genres) %>%
  summarise(gui = mean(rating- mu- reg_bi- reg_bu))

duis<- edx %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  left_join(guis, by="genres")  %>%
  group_by(week) %>%
  summarise(dui = mean(rating- mu- reg_bi- reg_bu- gui))

# make predictions on the validation set using the final model: Yui = mu + reg_bi + reg_bu  + gui + dui
predictions<- validation %>%
  mutate(date = as_datetime(timestamp),
         week=round_date(date, "week")) %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  left_join(guis, by="genres") %>%
  left_join(duis, by="week") %>%
  summarize(pred= mu + reg_bi+ reg_bu + gui + dui ) %>%
  pull(pred)


RMSE<- sqrt(mean((validation$rating - predictions)^2))  # evaluate the RMSE of the predictions
results$MODEL[8]<-"VALIDATION test: All Effects model"
results$RMSE[8] <- round(RMSE,5)


########################################################################################################################
#
#  MODELING WITH RECOMMMENDERLAB
#
########################################################################################################################
# We have observed that there are groups of users and movies with similar rating patterns

# Use Recommenderlab implementation of matrix factorization/ SVD for predicting ratings based on proximity of users and movies 
# see:  https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

# transform edx dataframe into a sparse matrix which is more efficient storing sparse data, with NAs represented as dots
edx_matrix <- sparseMatrix(i = edx$userId,
                           j = edx$movieId,
                           x = edx$rating)

#remove empty rows and columns and add names
edx_matrix<- edx_matrix[rowSums(edx_matrix!=0) != 0, ]
edx_matrix<- edx_matrix[ ,colSums(edx_matrix!=0) != 0 ]

dimnames(edx_matrix) = list(unique(edx$userId), unique(edx$movieId))

# convert sparse matrix to realRatingMatrix as required by Recommenderlab
edx_realMatrix <- as(edx_matrix, "realRatingMatrix")

# display residuals first 10 users and movies in the edx realRatingsMatrix
getRatingMatrix(edx_realMatrix[1:10,1:10])  

# view 100 x 100 image
image(edx_realMatrix[1:100, 1:100],
      main = "Ratings for 100 users x 100 movies", xlab="Movies", ylab="Users")

#   Train a Model using Recommenderlab SVD
#---------------------------------------------------------------

# To improve predictions and speed the performance of Recommenderlab, 
# we train our algorithm with "frequent" users that have evaluated at least 50 movies 

frequent_users <- edx_realMatrix[rowCounts(edx_realMatrix) >= 50 ]

set.seed(123, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed()`

# use Recommenderlab evaluationScheme function to partition data into a training, test and error sets. 
frequent_users_split <- evaluationScheme(frequent_users, method="split", train= 0.9, given=30, goodRating=5)
train_set<- getData(frequent_users_split, "train")  
test_set <- getData(frequent_users_split, "known")  
error_set <- getData(frequent_users_split, "unknown")  

# view list of recommender methods
list_models<-recommenderRegistry$get_entries(dataType = "realRatingMatrix")$SVD_realRatingMatrix

# Train a model using recommenderlab SVD implementation. Tune for the k-neighbor parameter
k_tune <- seq(10,40, 5)

rmses <- sapply(k_tune, function(k){
  model_svd <- Recommender(train_set, method="SVD", param=list(k=k))
  predictions <- predict(model_svd, test_set, type="ratings" )
  RMSE<- calcPredictionAccuracy(predictions, error_set)["RMSE"]
  return(RMSE)
})

# plot results of tuning for k
qplot(k_tune, rmses)

# select the lambda that minimizes the RMSE error
k_opt<- k_tune[which.min(rmses)]

RMSE<- min(rmses)


########################################################################################################################
#
#  COMBINED MODEL: EFFECTS + RECOMMENDERLAB
#
########################################################################################################################

# Let's see if we can improve estimates by fitting the model: Yui = mu + reg_bi + reg_bu+ gui + dui + rui + eui
# with rui the residuals after adjusting for all previous effects:  res= rating- (mu + reg_bi + reg_bu  + gui + dui) 
# we estimate those residuals using the RecommenderLab SVD implementation

mu<- mean(edx$rating)

residual <- edx %>%  
  mutate(date = as_datetime(timestamp),week=round_date(date, "week")) %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by= "userId") %>%
  left_join(guis, by="genres") %>%
  left_join(duis, by="week") %>%
  summarize(res= rating - (mu + reg_bi + reg_bu + gui + dui)) %>%
  pull(res)

# transform dataframe into a sparse matrix which is more efficient storing sparse data, with NAs represented as dots
edx_matrix <- sparseMatrix(i = edx$userId,
                           j = edx$movieId,
                           x = residual)

#remove empty rows and columns and add names
edx_matrix<- edx_matrix[rowSums(edx_matrix!=0) != 0, ]
edx_matrix<- edx_matrix[ ,colSums(edx_matrix!=0) != 0 ]

dimnames(edx_matrix) = list(unique(edx$userId), unique(edx$movieId))

# convert sparse matrix to realRatingMatrix as required by Recommenderlab
edx_realMatrix <- as(edx_matrix, "realRatingMatrix")

save(edx_realMatrix, file="rdas/edx_realMatrix.rda")

# view 100 x 100 image
image(edx_realMatrix[1:100, 1:100],
      main = "Residuals for 100 users x 100 movies", xlab="Movies", ylab="Users")


#   Train a Model using Recommenderlab to estimate residuals
#---------------------------------------------------------------
# we train our algorithm with "frequent" users that have evaluated at least 50 movies 

frequent_users <- edx_realMatrix[rowCounts(edx_realMatrix) >= 50 ]

set.seed(123, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed()`

# use Recommenderlab evaluationScheme function to partition data into a training, test and error sets. 
frequent_users_split <- evaluationScheme(frequent_users, method="split", train= 0.9, given=30, goodRating=1.5)
train_set<- getData(frequent_users_split, "train")  
test_set <- getData(frequent_users_split, "known")  
error_set <- getData(frequent_users_split, "unknown")  

# Train a model using recommenderlab SVD implementation. Tune for the k-neighbor parameter
k_tune <- seq(30,45, 1)

rmses <- sapply(k_tune, function(k){
  model_svd <- Recommender(train_set, method="SVD", param=list(k=k))
  predictions <- predict(model_svd, test_set, type="ratings" )
  RMSE<- calcPredictionAccuracy(predictions, error_set)["RMSE"]
  return(RMSE)
})

# plot results of tuning for k
qplot(k_tune, rmses)

# select the lambda that minimizes the RMSE error
k_opt<- k_tune[which.min(rmses)]

RMSE<- min(rmses)

save(k_tune, file="rdas/k_tune.rda")
save(rmses, file="rdas/rmses_k.rda")

results$MODEL[7]<-"Recommenderlab SVD + Effects"
results$RMSE[7] <- round(RMSE,5)

###########################################################
# 
#  RECOMMENDERLAB + EFFECTS VALIDATION TEST
#
############################################################
# fit model for frequent users
model_svd<- Recommender(frequent_users, method="SVD",param=list(k=k_opt) )

# define function that makes Recommenderlab predictions and returns as dataframe.
recommender<- function(group, rec_type){
  preds_realMatrix <- predict(model_svd,  group, type=rec_type )
  preds_matrix <- getRatingMatrix(preds_realMatrix)
  
  userId<-as.integer(rownames(preds_matrix))
  movieId<- as.integer(colnames(preds_matrix))
  residual<- as.vector(preds_matrix)
  
  preds<- data.frame(userId, movieId, residual) 
  
  # only keep predictions in validation set
  preds<- validation %>% inner_join(preds, by=c("movieId", "userId")) %>% 
    select (userId, movieId, residual)
  
  return(preds)
}


# Estimate residuals for frequent users using Recommenderlab 
#--------------------------------------------------------------------
nrows<- nrow(frequent_users)
s<- 5000
index1<- seq(1,nrows, s)
index2<- c(seq(s,nrows,s),nrows)

# split matrix of users into smaller chunks for processing by Recommenderlab
frequent_users_groups<- mapply (function(i,j) frequent_users[i:j,], index1, index2 )

recommender_res<- lapply(frequent_users_groups, rec_type="ratingMatrix", recommender)

# bind Recommenderlab results after processing
res_freq_users <- do.call(rbind, lapply (recommender_res, as.data.frame))

gc()


# Estimate residuals for infrequent users using Recommenderlab predict()
#--------------------------------------------------------------------
infrequent_users <- edx_realMatrix[rowCounts(edx_realMatrix) <50, ]

# break realRatingMatrix for infrequent users into smaller chunks for processing by Recommenderlab predict()
nrows<- nrow(infrequent_users)
s<- 5000
index1<- seq(1,nrows, s)
index2<- c(seq(s,nrows,s),nrows)

# split data into smaller chunks for processing by Recommenderlab
infrequent_users_groups<- mapply (function(i,j) infrequent_users[i:j,], index1, index2 )

recommender_res<- lapply(infrequent_users_groups, rec_type="ratings", recommender)

# bind Recommenderlab results after processing
res_infreq_users <- do.call(rbind, lapply (recommender_res, as.data.frame))

# Combine all residuals
res<- rbind (res_freq_users, res_infreq_users)

# make predictions on the validation set using the final model: Yui = mu + reg_bi + reg_bu  + gui + dui + residual
predictions<- validation %>%
  mutate(date = as_datetime(timestamp),
         week=round_date(date, "week")) %>%
  left_join(reg_bis, by="movieId") %>%
  left_join(reg_bus, by="userId") %>%
  left_join(guis, by="genres") %>%
  left_join(duis, by="week") %>%
  left_join(res, by=c("movieId", "userId")) %>%
  mutate (pred= mu + reg_bi + reg_bu + gui + dui + residual) %>%
  pull(pred)

RMSE<- sqrt(mean((validation$rating - predictions)^2))  # evaluate the RMSE of the predictions

results$MODEL[9]<-"VALIDATION test: Recommenderlab SVD + Effects"
results$RMSE[9] <- round(RMSE,5)

save(results, file="rdas/results.rda")
