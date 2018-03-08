## Slide: 47 Create rating matrix Rows = userId, Columns = movieId
library(reshape2)
rating_matrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
rating_matrix <- as.matrix(rating_matrix[,-1]) #remove userIds

## Slide 49: Use recommenderlab to create user-based recommender engine
library(recommenderlab)
# 3.1 Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(rating_matrix, "realRatingMatrix")

## Slide 50: Create recommender
e <- evaluationScheme(ratingmat, method = "split", train = 0.9, k = 1, given = 15)
r <- Recommender(getData(e, "train"), "UBCF")

## Slide 51: Make recommendation
p <- predict(r, getData(e, "known"), type="topNList")
p.list <- getList(p)
movies[as.integer(p.list[[2]]),]

## Slide 53: Prediction accuracy
p1 <- predict(r, getData(e, "known"), type="ratings")
calcPredictionAccuracy(p1, getData(e, "unknown"), given=15, goodRating=5) 

## Slide 62: Creating IBCF
ratingmat_ib <- as(rating_matrix[,1:500], "realRatingMatrix")
r <- Recommender(ratingmat_ib, "IBCF")

## Slide 63: Predicting with IBCF
p <- predict(r, ratingmat_ib[1], type="topNList")
p.list <- getList(p)
movies[as.integer(p.list[[1]]),]
