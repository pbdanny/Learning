## 1. Import data
# 1.1 Import movies & genres data (movies, genres)
movies <- read.csv("movies.csv")
str(movies)
# 1.2 Import movies rating data (users, movies, rating)
ratings <- read.csv("ratings.csv")
str(ratings)

## 2. Data preprocessing
# 2.1 Convert movies & genre data into a feature matrix
install.packages("data.table")
library(data.table)
genres <- as.data.frame(movies$genres, stringsAsFactors = FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1],"[|]",type.convert = TRUE), stringsAsFactors = FALSE)
colnames(genres2) <- 1:7
# 2.2 view the data
View(genres2)
# 2.3 Create genre feature matrix
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")
genre_matrix <- matrix(0,9126,18) #empty matrix 9126 = 9125 movies + 1 (genre list), 18 = number of features
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

# 2.4 Set genre binary value for each movie
#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

# 2.5 convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {  
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

## 3. Convert rating from tabular format into matrix
# 3.1 Change to sentiment rating
binaryratings <- ratings
binaryratings$rating <- ifelse(binaryratings$rating > 3, 1, -1)

# 3.2 install and load package reshape2
install.packages("reshape2")
library(reshape2)

# 3.3 spread the data into matrix of (movieId x userId) with values "rating"
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)
for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
#remove movieIds col. Rows are movieIds, cols are userIds
binaryratings2 = binaryratings2[,-1] 

# 3.4 Remove unrated data
#Remove rows that are not rated from movies 
movieIds <- unique(movies$movieId)
datasetmovieIds <- unique(movies$movieId)
ratingmovieIds <- unique(ratings$movieId)
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

## 4. Calculate the dot product
# 4.1 Calculate dot product for User Profiles
result = matrix(0,18,671)
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c]))
  }
}
result <- ifelse(result < 0, 0, 1)
# 0 = dislike, 1 = like

# 4.2 View the results
colnames(result) <- colnames(binaryratings2)
rownames(result) <- colnames(genre_matrix3)
View(result)

## 5. Recommendation
# 5.1 Set the user profile we want to extract
result2 <- result[,1] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) 

# 5.2 Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:9067]))

# 5.3 Get the top 3 rank similar recommended movies
rows <- which(rank(sim_results, ties.method='min') <= 3)
result2
movies2[rows,]


