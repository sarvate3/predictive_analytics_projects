## function for evaluating euclidian distance between two observations in feature space

euclid_dist <- function(a, b) {
  
  a_vec <- as.numeric(a)
  b_vec <- as.numeric(b)
  
  distance <- sqrt(abs(sum(a_vec ** 2) - sum(b_vec ** 2)))
  
  return(distance)
}

## function for determining majority classification of K nearest neighbors of a test observation

kNN <- function(training_set, test_obs, K) {
  
  matched = TRUE
  
  while(matched)
  {
    
    # evaluate and assign distance to test observation for each training point
  
    difference <- NULL
  
    rows_training <- nrow(training_set)
  
    for (i in 1:rows_training){
      difference <- c(difference, euclid_dist(training_set[i,],test_obs))
    }

    
    training_set_aug <- cbind(training_set, difference)
  
    training_set_aug_sort <- training_set_aug[order(training_set_aug$difference),]
  
    # tabulate and count results for most common classification for nearest K training points
  
    categories <- as.data.frame(table(training_set_aug_sort[1:K,5]))
  
    # determine whether any of the non-zero categories are tied in terms of frequency
    # if there are ties, decrement K by 1 and repeat kNN calculation
    
    cat_freq <- categories$Freq
    
    cat_freq_nonzero <- subset(cat_freq, cat_freq != 0)
  
    matched <- any(duplicated(cat_freq_nonzero))
  
    K = K - 1
  }
  
  # Extract category corresponding to maximum count in table
  
  categories_max <- as.character(categories[which.max(categories$Freq),1])
  
  
  return(categories_max)
}

## split iris dataset into training and test component by random selection

K <- 10 # number of "nearest neighbors" used to predict classification
prop_training <- 0.65    # proportion of dataset used for training

rows_training <- as.integer(prop_training * nrow(iris))

# randomly sample and select rows corresponding to training and test data set

training_index <- sample(1:nrow(iris), rows_training, replace<-FALSE)

test_index <- -(training_index)

iris_train <- iris[training_index,]
iris_test <- iris[test_index,]

## run test data set through kNN function and determine match rate (accuracy)

# add additional column to test data set for prediction outcome

iris_test_outcome <- cbind(iris_test, "prediction"=rep(0,nrow(iris_test)))

for (i in 1:nrow(iris_test)) {
  iris_test_outcome[i,6] <- kNN(iris_train, iris_test[i,], K)  
}

# tabulate portions of correct vs. incorrect predictions

prediction_matrix <- as.matrix(table(iris_test_outcome[,5:6]))

print(prediction_matrix)

diag_sum = sum(diag(prediction_matrix))

accuracy = diag_sum / (diag_sum + ((sum(prediction_matrix) - diag_sum)/2))

print(accuracy)




