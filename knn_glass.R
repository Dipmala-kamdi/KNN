glass <- read.csv(file.choose())
View(glass)
head(glass)

#90% of databas 
ran <- sample(1:nrow(glass), 0.9 * nrow(glass)) 

nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

glass_norm <- as.data.frame(lapply(glass[,c(1,2,3,4,5,6,7,8,9)], nor))

summary(glass_norm)


##extract training set
glass_train <- glass_norm[ran,] 

##extract testing set
glass_test <- glass_norm[-ran,]

target_category <- glass[ran,10]
test_category <- glass[-ran,10]
View(test_category)

library(class)

glass_knn <- knn(glass_train,glass_test,cl=target_category,k=25)


tab <- table(glass_knn,test_category)


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


acc <- (sum(diag(tab)) / length(target_category)) * 100
print(acc)


#############################################

i=1                        
k.optm=1                    

for (i in 1:28){ 
  knn.mod <-  knn(glass_train,glass_test,cl=target_category,k=i)
  k.optm[i] <- 100 * sum(test_category == knn.mod)/NROW(test_category)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

pred<- knn(glass_train,glass_test,cl=target_category,k=25)
