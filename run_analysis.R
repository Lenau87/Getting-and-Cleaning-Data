testX <- read.table("test/X_test.txt")
testy <- read.table("test/y_test.txt")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
labels <- read.table("activity_labels.txt")

y <- factor(rbind(testy,y_train)[,1]) # appends the rows of y_train on testy and converts y to an array of factors
X <- rbind(testX,X_train) # appends the rows of X_train on testX
subject <- rbind(subject_train,subject_test)

levels(y) <- levels(labels[,2])[c(labels[,2])] # changes the order of the labels and assigns it to the levels of y

feat <- read.table("features.txt") # loads the names of the features
names(X) <- feat[,2] # Sets the names of X to the name of the features from feature.txt
feat1 <- feat[grepl("mean()",feat[,2],fixed = TRUE),2] # gets the features with mean() in them
feat2 <- feat[grepl("std",feat[,2]),2] # gets the features with std in them
X <- X[,feat[,2] %in% feat1 | feat[,2] %in% feat2] # gets the values for features with mean() and std in them

tables <- vector(mode = "list", length = nrow(unique(subject)))
names(tables) <- unique(subject)
for (i in unique(subject[,1])){
  tables[[i]] <- as.table(matrix(rep(0,length(names(X))*length(levels(y))),ncol = 6,byrow = TRUE)) # creats a table for every subject
  names(tables)[i] <- i
  colnames(tables[[i]]) <- levels(y)
  rownames(tables[[i]]) <- names(X)
  for (j in names(X)){
    for (k in levels(y)){
      X1 <- X[subject == i & y == k, names(X) == j]
      tables[[i]][j,k] <- mean(X1)
    }
  }
}

