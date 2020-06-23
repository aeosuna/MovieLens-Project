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

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

#Dataset Exploratory Analysis----
library(lubridate)
library(tidyverse)
install.packages("kableExtra")
library(kableExtra)
library(caret)
head(edx) %>% kable() %>% kable_styling()

data.frame(Var= c("userId", "timestamp","movieId","title",
                  "genres", "rating"),
           class= c(class(edx$userId), class(edx$timestamp),
                    class(edx$movieId), class(edx$title),
                    class(edx$genres), class(edx$rating))) %>% 
  kable() %>% kable_styling()

N_rows_edx<- nrow(edx)
N_col_edx<- ncol(edx)
N_rows_validation<- nrow(validation)
N_col_validation<- ncol(validation)
data.frame(set= c("edx", "validation"), N_rows=c(N_rows_edx, N_rows_validation), N_columns= c(N_col_edx, N_col_validation)) %>% kable() %>% kable_styling()

N_zeros<- edx %>% filter(rating==0) %>% summarize(n=n())
N_3<- edx %>% filter(rating== 3) %>% summarize(n=n())

N_different_movies<- n_distinct(edx$movieId)
N_different_users<- n_distinct(edx$userId)
paste("Our data set contains", N_different_movies, "movies","&",N_different_users, " users")

N_drama<- edx %>% filter(str_detect(genres, "Drama")) %>% nrow()
N_comedy<- edx %>% filter(str_detect(genres, "Comedy")) %>% nrow()
N_thriller<- edx %>% filter(str_detect(genres, "Thriller")) %>% nrow()
N_romance<- edx %>% filter(str_detect(genres, "Romance")) %>% nrow()
print(data.frame(Genre= c("Drama", "Comedy", "Thriller", "Romance"), N=c(N_drama, N_comedy, N_thriller, N_romance)))

top_10<-edx %>% group_by(title) %>% summarize(n=n(), .groups= "drop") %>% top_n(10, n) %>% arrange(desc(n)) #movies with the greatest number of ratings

top_5<-edx %>% group_by(rating)%>% summarize(n=n(),.groups= "drop") %>% top_n(5, n) %>% arrange(desc(n)) #the five most given ratings

summary_ratings<-edx %>% group_by(rating)%>% summarize(n=n(),.groups= "drop") %>% arrange(desc(n)) #half star rating are less common than the whole star ratings

#some ratings are more frequent than others
ratings<- data.frame(ratings=as.factor(edx$rating)) %>% 
  group_by(ratings) %>% 
  summarize(count=n(), .groups="drop") 

ratings$ratings<- factor(ratings$ratings, levels= ratings$ratings[order(-ratings$count)])

ggplot(ratings, aes(x= ratings, y=count, fill= ratings))+ 
  theme_bw()+
  geom_bar(stat= "identity")+
  scale_fill_brewer(palette = "RdGy")+
  ggtitle("Ratings Count")

#some movies get rated more often than others
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 20, color = "black") + 
  scale_x_log10() +
  ggtitle("Movies")

#A second observation is that some users rate more movies than others
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 20, fill = "grey", color= "black") + 
  scale_x_log10() +
  ggtitle("Users")

#a third obseravtion is the some genres combinations or categories are more rated than others
edx %>% 
  group_by(userId) %>% 
  filter(n()>=500) %>% #lets filter for computational time purposes
  separate_rows(genres, sep= "\\|") %>% 
  group_by(genres) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(factor(genres, levels= genres[order(-n)]), n))+
  geom_bar(stat= "identity")+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  ggtitle("Genres Count")+
  xlab("genres")

#this trend shows how people used to rate the movies higher back in the 1990s
edx %>% 
  rename(date= timestamp) %>% 
  mutate(date= as_datetime(date), year= year(date)) %>% 
  group_by(year) %>% summarize(rating_mean=mean(rating), .groups="drop") %>% 
  ggplot(aes(year, rating_mean))+
  geom_point()+
  geom_smooth()

##Dataset Pre-processing----

#extracting released year from the title to create a new feature 
new_edx<-edx %>% mutate(year_released= str_extract(title,("\\(\\d{4}\\)"))) %>% #extracting the year in ()
  mutate(year_released= str_extract(year_released, "\\d+")) %>% 
  rename(date= timestamp) %>% 
  mutate(date= as_datetime(date)) %>% #convert the timestap to a date everyone can read
  mutate(date= round_date(date, unit= "month"))

#It is also necessary to do it to the validation set
validation<-validation %>% mutate(year_released= str_extract(title,("\\(\\d{4}\\)"))) %>% #we do the same for the validation set
  mutate(year_released= str_extract(year_released, "\\d+")) %>%
  rename(date= timestamp) %>% 
  mutate(date= as_datetime(date)) %>% #do the same for the validation set
  mutate(date= round_date(date, unit= "month"))

#Movie's year released shows variation as well
new_edx %>% 
  group_by(year_released) %>% 
  summarize(n=n(),.groups="drop") %>%  #count how many ratings per year_released
  filter(year_released>=1960)%>% 
  mutate(year_released= as.factor(year_released)) %>% 
  ggplot(aes(x=year_released,y=n, fil= year_released)) +
  geom_bar(stat= "identity")+
  theme(axis.text.x = element_text(angle=90, hjust = 1))+
  ggtitle("Year Released")


#the average rating for the most recent movies has decreased over the years
new_edx %>% 
  group_by(year_released) %>% 
  summarize(mean=mean(rating), .groups= "drop") %>% #calculate mean per year_released feature
  filter(year_released >=1960) %>% 
  ggplot(aes(year_released,mean))+
  geom_point(size=2.5, color= "red")+
  theme(axis.text.x= element_text(angle=90, hjust=1))

#Modeling approaches:----
#Naive Mean-Baseline----
avg_rating<- mean(new_edx$rating) #we calculate the mean of the training set

#this function calculates the RMSE between the validation ratings and the predicted values
RMSE<- function(true_ratings, predicted_rating){
  sqrt(mean((true_ratings-predicted_rating)^2))
}

baseline_rmse<-RMSE(validation$rating, avg_rating) #the simples approach, in which we predict all ratings will be equal to the mean

#Regularization:

#Regularized Movie effect model----

lambdas<- seq(0,15,0.25) #set a vector of different lambda to calculate best penalty for movie effect 

#apply the vector of lambdas to a function that will calculate the rmses
rmses_me<- sapply(lambdas, function(lambda){
  m_i<- new_edx %>% 
    group_by(movieId) %>% 
    summarize(m_i= sum(rating-avg_rating)/(n()+ lambda), .groups= "drop") #calculate the movie effect and penalize the groups with small number of ocurrences
  
  predicted_ratings<-validation %>% #use the test set to predict movie ratings by users
    left_join(m_i, by= "movieId") %>% 
    mutate(prediction= avg_rating+ m_i) %>% 
    .$prediction #extract predictions
  return(RMSE(validation$rating, predicted_ratings)) #call the RMSE function developed before
})

plot(lambdas, rmses_me) #plot penalties vs rmses
optlm<- lambdas[which.min(rmses_me)] #extracts optimal penalty for movie effect
paste("The best achieved RMSE is ", round(rmses_me[which.min(rmses_me)],5), "with a penalty of ", optlm)


#Regularized Movie effect + User effect---- 

#apply the vector of lambdas to a function that will calculate the rmses
rmses_2<- sapply(lambdas, function(lambda){

m_i<- new_edx %>% 
    group_by(movieId) %>% 
    summarize(m_i= sum(rating-avg_rating)/(n()+ lambda), .groups= "drop") #calculate the movie effect and penalize the groups with small number of ocurrences

u_u <- new_edx %>% 
    left_join(m_i, by= "movieId") %>% 
    group_by(userId) %>% 
    summarize(u_u= sum(rating- m_i- avg_rating)/(n()+lambda), .groups="drop") #calculate the user effect and penalize the groups with small number of ocurrences

predicted_ratings<-validation %>% 
    left_join(m_i, by= "movieId") %>% 
    left_join(u_u, by="userId") %>% 
    mutate(prediction= avg_rating+ m_i+ u_u) %>% 
    .$prediction
  
return(RMSE(validation$rating, predicted_ratings))
})

plot(lambdas, rmses_2) #it plots penalties vs rmses
optl2<- lambdas[which.min(rmses_2)] #extracts optimal penalty for movie + user effect
paste("The best achieved RMSE is ", round(rmses_2[which.min(rmses_2)],5), "with a penalty of ", optl2)


#Regularized Movie effect + User effect + released_year effect----

#apply the vector of lambdas to a function that will calculate the rmses
rmses_3<- sapply(lambdas, function(lambda){
  
m_i<- new_edx %>% 
    group_by(movieId) %>% 
    summarize(m_i= sum(rating-avg_rating)/(n()+ lambda), .groups="drop") 
#calculate the movie effect and penalize the groups with small number of ocurrences

u_u <- new_edx %>% 
    left_join(m_i, by= "movieId") %>% 
    group_by(userId) %>% 
    summarize(u_u= sum(rating- m_i- avg_rating)/(n()+lambda), .groups= "drop") 
#calculate the user effect and penalize the groups with small number of ocurrences

r_j<- new_edx %>% 
    left_join(m_i, by= "movieId") %>% 
    left_join(u_u, by= "userId") %>% 
    group_by(year_released) %>% 
    summarize(r_j= sum(rating-m_i-u_u-avg_rating)/ (n()+ lambda), .groups= "drop") 
#calculate the year effect and penalize the groups with small number of ocurrences
  
predicted_ratings<-validation %>% 
    left_join(m_i, by= "movieId") %>% 
    left_join(u_u, by="userId") %>% 
    left_join(r_j, by= "year_released") %>% 
    mutate(prediction= avg_rating+ m_i+ u_u+ r_j) %>% 
    .$prediction
  
return(RMSE(validation$rating, predicted_ratings))
})


plot(lambdas, rmses_3) #it plots penalties vs rmses
optl3<- lambdas[which.min(rmses_3)] 
#extracts optimal penalty for movie + user effect + year_released effect
paste("The best achieved RMSE is ", round(rmses_3[which.min(rmses_3)],5), "with a penalty of ", optl3) #we print the rmse results 

#Adding regularized genre effect---- 

#in this case we will a more defined subset of lambdas for greater precision
l<- seq(4,5.5,0.10) #sets a vector of penalties for last model

#apply the vector of lambdas to a function that will calculate the rmses
calculate_rmses<- sapply(l, function(lambda){
  
m_i<- new_edx %>% 
    group_by(movieId) %>% 
    summarize(m_i= sum(rating-avg_rating)/(n()+ lambda),.groups="drop") 
#calculate the movie effect and penalize the groups with small number of ocurrences
  
u_u <- new_edx %>% 
    left_join(m_i, by= "movieId") %>% 
    group_by(userId) %>% 
    summarize(u_u= sum(rating- m_i- avg_rating)/(n()+lambda),.groups="drop") 
#calculate the user effect and penalize the groups with small number of ocurrences


g_g<- new_edx %>% 
  left_join(m_i, by= "movieId") %>% 
  left_join(u_u, by= "userId") %>% 
  group_by(genres) %>% 
  summarize(g_g= sum(rating-m_i-u_u-avg_rating)/(n()+lambda),.groups="drop") 
#calculate the genre effect and penalize the groups with small number of ocurrence

r_j<- new_edx %>% 
  left_join(m_i, by= "movieId") %>% 
  left_join(u_u, by= "userId") %>% 
  left_join(g_g, by= "genres") %>% 
  group_by(year_released) %>% 
  summarize(r_j= sum(rating-m_i-g_g-u_u-avg_rating)/ (n()+ lambda),.groups="drop") 
#calculate the year effect and penalize the groups with small number of ocurrences

predicted_ratings<-validation %>% 
  left_join(m_i, by= "movieId") %>% 
  left_join(u_u, by="userId") %>% 
  left_join(g_g, by= "genres") %>% 
  left_join(r_j, by= "year_released") %>% 
  mutate(prediction= avg_rating+ m_i+g_g+ u_u+ r_j) %>% 
  .$prediction

return(RMSE(validation$rating, predicted_ratings))
})


data.frame(l=l, RMSE=calculate_rmses) %>% 
  ggplot(aes(l, RMSE)) + 
  geom_point() #it graphs the curve between penalty and rmse to visualize the minimum
optimal_lambda<-l[which.min(calculate_rmses)] #extracts optimal penalty for latest model
paste("The best achieved RMSE is ", round(calculate_rmses[which.min(calculate_rmses)],5), 
      "with a penalty of ", optimal_lambda) #prints the RMSE solution


#this code will create a results table that includes the name of the approach, the penalty (if any) and the rmse calculated
results<- data.frame(Model=c("Baseline Naive-mean Model","Regularized Movie effect", "Regularized Movie + User effect", 
                             "Regularized Movie + User + Year effect", "Regularized Movie+ User+ Year+ Genre"),
                     penalty= c(0,lambdas[which.min(rmses_me)],lambdas[which.min(rmses_2)],
                                lambdas[which.min(rmses_3)],l[which.min(calculate_rmses)]), 
                     RMSE= c(baseline_rmse,rmses_me[which.min(rmses_me)],
                     rmses_2[which.min(rmses_2)],
                     rmses_3[which.min(rmses_3)],
                     calculate_rmses[which.min(calculate_rmses)]))

#we round the rmse for easier visualization and understanding
results %>% mutate(RMSE= round(RMSE,5)) %>% kable %>% kable_styling()                

