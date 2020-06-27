zoo <- read.csv(file.choose())
View(zoo)
head(zoo)

zoo <- data.frame(zoo)

summary(zoo)

types <- table(zoo$type)
zoo_target <- zoo[, 18]
zoo_key <- zoo[, 1]
zoo$animal <- NULL

types
names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
types

names(zoo) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
              "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
              "fins", "legs", "tail", "domestic", "size", "type")
str(zoo)

library(class)

sam_ple <- sample(1:nrow(zoo), 0.9 * nrow(zoo)) 

nor_1 <-function(x) { (x -min(x))/(max(x)-min(x))   }

zoo_norm <- as.data.frame(lapply(zoo[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)], nor_1))

summary(zoo_norm)


##extract training set
zoo_train <- zoo_norm[sam_ple,] 

##extract testing set
zoo_test <- zoo_norm[-sam_ple,]

target_category <- zoo[sam_ple,10]
test_category <- zoo[-sam_ple,10]
View(test_category)

i=1                        
k.optm=1                    

for (i in 1:28){ 
  knn.mod <-  knn(zoo_train,zoo_test,cl=target_category,k=i)
  k.optm[i] <- 100 * sum(test_category == knn.mod)/NROW(test_category)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

pred<- knn(zoo_train,zoo_test,cl=target_category,k=5)
pred

