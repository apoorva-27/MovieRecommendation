library(corrplot)
library(Matrix)
library(recommenderlab)
library(quanteda)
library(reshape2)

movies <-read.csv("movies.csv", header = TRUE, sep = ",")
ratings <- read.csv("ratings.csv",header = TRUE, sep=",")

summary(movies)

summary(ratings)

genre_table=matrix(0,nrow(movies)+1,18)

genre_table[1,]=c("Action", "Adventure", "Animation", "Children","Comedy", "Crime",
                         "Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery",
                         "Romance", "Sci-Fi", "Thriller", "War", "Western")

#generating a genre table
for (i in 1:nrow(movies)) {
  list_of_genres=strsplit(as.character((movies[i,])$genres),'\\|')
  
  for (j in 1:length(list_of_genres[[1]])) {
    print (list_of_genres[[1]][j])
    insert_j=which(genre_table[1,]==list_of_genres[[1]][j])
    print (insert_j)
    genre_table[i+1,insert_j]=1
  }
}

g=genre_table[1,]
y=data.frame(genre_table)
colnames(y)=c(g)
genre_table=y[-1,]

genre_table[,c('movieId')]=movies$movieId
genre_table[,c('title')]=movies$title

#generate a consolidated table of movieId, ratings and genres
z=merge(genre_table,ratings,by='movieId')

d=sapply(z,is.factor)
z[d]=lapply(z[d],function(x) as.numeric(as.character(x)))

correlation_plot=cor(z)
corrplot(correlation_plot)

ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) 

#sparse matrix 
smatrix = as(ratingmat,"realRatingMatrix")
smatrix <- similarity(smatrix,method='cosine',c(1))

ratings_vector <- as.vector(smatrix@data)

table(ratings_vector)
qplot(ratings_vector)
plot(factor(ratings_vector[ratings_vector!=0]))

#user based collaborative  filtering model
recom_model <- Recommender(smatrix,method='UBCF',param=list(method='cosine',nn=30))

model_details<- getModel(recom_model)

recom1 <- predict(recom_model, 
                 smatrix[2], 
                 n=10)

#convert output to list
recom_list <- as(recom1, 
                 "list") 

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}

recom_result
