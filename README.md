#kaggle.titanic.competition
#All code, with minor changes, from David Linger's Intro to Data Science
#with R

#Read in Titanic data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test vector for combining with train
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# Change data types where needed
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)

# Look at gross survival rates
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

# load ggplot2
library(ggplot2)

# Hypothesis - rich people survived at higher rates
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("TotalCount") +
  labs(fill = "Survived")

# Examine first few names as character
head(as.character(train$Name))

# How many unique names are there across test and train
length(unique(as.character(data.combined$Name)))

# Determine duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Look at duplicate name records
data.combined[which(data.combined$Name %in% dup.names),]

# Load stringer library
library(stringr)

# Is there any correlation between "Miss" and other variables
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]

# Hypothesis - "Mrs." correlates with older age
mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrs[1:5,]

# Look at first five male records
mr <- data.combined[which(data.combined$Sex == "male"),]
mr[1:5,]

# Expand on survival in terms of pClass by adding title variable

# Create utility function for title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss", Name)) > 0) {
    return("Miss")
  } else if (length(grep("Master", Name)) > 0) {
    return("Master")
  } else if (length(grep("Mrs", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)

# Plot the first 891 rows (because only have survived in train dataset)

ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Passenger Class") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Distribution of males to females
table(data.combined$Sex)

#Graph survival by sex and class
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  ggtitle("Survival by Class and Sex") +
  facet_wrap(~Pclass) +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Drilling in on age
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#Graph on sex, age, and class
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  geom_bar(width = 2) +
  ggtitle("Survival by Age, Sex, and Class") +
  facet_wrap(~Sex + Pclass) +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

#More detail on boys
boys <- data.combined[which(data.combined$Title == "Master"),]
summary(boys$Age)

#More detail on people with title "Miss"
misses <- data.combined[which(data.combined$Title == "Miss"),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar(width = 2) +
  ggtitle("People with 'Miss' title by Age and Class") +
  xlab("Age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

#Looking at the sibling spouse variable
summary(data.combined$SibSp)

#Can we treat SibSp as a factor? (there are only 7 options) and visualize
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#What are titles labeled "Other"
table(data.combined$Title)
other <- data.combined[which(data.combined$Title == "Other"),]
other[1:31,]

#Look at Parch values as factor and plot
length(unique(data.combined$Parch))

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Family size feature
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$FamilySize <- as.factor(temp.SibSp + temp.Parch + 1)

#Visual survival by family size
ggplot(data.combined[1:891,], aes(x = FamilySize, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Family Size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Ticket column
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

Ticket.firstchar <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket,1,1))
unique(Ticket.firstchar)

data.combined$Ticket.firstchar <- as.factor(Ticket.firstchar)

#Plot on first character of ticket
ggplot(data.combined[1:891,], aes(x = Ticket.firstchar, fill = Survived)) +
  geom_bar() +
  ggtitle("Survival by Ticket Code") +
  xlab("First Ticket Character") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#Plot on first character of ticket and class
ggplot(data.combined[1:891,], aes(x = Ticket.firstchar, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival by Ticket Code & Class") +
  xlab("First Ticket Character") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Plot on first character of ticket, class, and title
ggplot(data.combined[1:891,], aes(x = Ticket.firstchar, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Survival by Ticket Code, Class, & Title") +
  xlab("First Ticket Character") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

#Fares paid and their role
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#Because there are too many values, use fare as number and use histogram to plot
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Survival by Fare") +
  xlab("Fare Paid") +
  ylab("Total Count") +
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Survival by Fare") +
  xlab("Fare Paid") +
  ylab("Total Count") +
  ylim(0,80)

#Look at cabin data
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

Cabin.firstchar <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.firstchar)
levels(Cabin.firstchar)

data.combined$Cabin.firstchar <- Cabin.firstchar

ggplot(data.combined[1:891,], aes(x = Cabin.firstchar, fill = Survived)) +
  geom_bar() +
  ggtitle("Survival by Cabin First Character") +
  xlab("Deck Level") +
  ylab("Total Count")

ggplot(data.combined[1:891,], aes(x = Cabin.firstchar, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival by Cabin First Character & Class") +
  xlab("Class") +
  ylab("Total Count") +
  ylim(0,500)

ggplot(data.combined[1:891,], aes(x = Cabin.firstchar, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Survival by Cabin First Character, Class, & Title") +
  xlab("Title") +
  ylab("Total Count") +
  ylim(0,300)
#Warning - I am not getting the same result for third class males

#Do multiple cabins figure in
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Survival by Multiple Cabins") +
  xlab("Multiple Cabins") +
  ylab("Total Count") +
  ylim(0,300)

#Does where you boarded affect survivability
str(data.combined$Embarked)
length(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Survival by Place of Embarkation") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300)

#Start exploratory modeling
library(randomForest)

#Train random forest with pclass and title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#Train random forest with pclass, title, and sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

#Train random forest with pclass, title, and parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#Train random forest with pclass, title, sibsp, and parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

#Train random forest with pclass, title, and familysize
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "FamilySize")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#Train random forest with pclass, title, sibsp, and familysize
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "FamilySize")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

#Train random forest with pclass, title, parch, and familysize
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "FamilySize")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

#Create initial Kaggle submission
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "FamilySize")]

rf.5.pred <- predict(rf.5, test.submit.df)
table(rf.5.pred)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.pred)
View(submit.df)

write.csv(submit.df, "KaggleTitanicCrossValidation1.csv", row.names = FALSE)

#Cross validation work
library(caret)
library(doSNOW)

#Trying 10 fold CV repeated 10 times, stratified cross validation
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

#Check stratification
table(rf.label)
342 / 549

#Choosing the 33rd fold at random and comparing the the 62% overal survival rate
table(rf.label[cv.10.folds[[33]]])
308 / 494

#Use caret's trainControl object
ctrl.1 <- trainControl(method = "repeatedcv", number = 100, repeats = 10,
                       index = cv.10.folds)

#Set up doSNOW for multi-core training
cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)

#Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

#Check results
rf.5.cv.1

#Because results are projecting a lower error rate than the competition
#score, repeat using 5 fold cross-validation instead of 10
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

#Check results
rf.5.cv.2

#Because results are projecting a lower error rate than the competition
#score, repeat again using 3 fold cross-validation instead of 5
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type="SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

#Check results
rf.5.cv.3

#Using a single decision tree instead of a random forest to get a better
#feel for the individual features
library(rpart)
library(rpart.plot)

#Use 3-fold CV ten times, create a function for it
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return(rpart.cv)
}

#Grab features we like best
features <- c("Pclass", "Title", "FamilySize")
rpart.train.1 <- data.combined[1:891, features]

#Run the single tree cross-validation by calling the rpart.cv function
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#Plot the single tree
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#Additional feature engineering to get rid of overfitting
table(data.combined$Title)

#Drill in on Name attribute to work with title, specifically trying to
#find out about those 31 "Other" titles
name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]
data.combined$LastName <- last.names

name.splits <- str_split(sapply(name.splits, "[", 2), " ")
Titles <- sapply(name.splits, "[", 2)
unique(Titles)

#What is up with a title of "the"
data.combined[which(Titles == "the"),]

#Add additional gender depth to previously "other" titles
#Note, moving Mlle, etc. into Miss, etc.
Titles[Titles %in% c("Dona.", "the")] <- "Lady."
Titles[Titles %in% c("Mlle.", "Ms.")] <- "Miss."
Titles[Titles == "Mme."] <- "Mrs."
Titles[Titles %in% c("Don.", "Jonkheer.")] <- "Sir."
Titles[Titles %in% c("Col.", "Capt.", "Major.")] <- "Officer."
table(Titles)

#Make new title a factor in data.combined
data.combined$new.title <- as.factor(Titles)

#Plot new version of titles
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rates by New Title")

#Collapse certain titles into Mrs and Mr to avoid overfitting
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Officer.")
data.combined$new.title[indexes] <- "Mr."

#Visualize collapsed new.titles values
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survival Rates by New Title")

#Run new single tree CV for collapsed titles
features <- c("Pclass", "new.title", "FamilySize")
rpart.train.2 <- data.combined[1:891, features]

rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

#See new individual tree for collapsed titles
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

#See a small increase in accuracy but are not capturing the issue that
#more first class males survive than third class males, initial tree
#branch is purely predicated on being male, drill in and fix
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

#Why one female in "Mr."?
first.mr.df[first.mr.df$Sex == "female",]

#Fix mis-classified female in Mr. (Dr. Leader)
indexes <- which(data.combined$new.title == "Mr." & data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#Are there other mis-classes?
length(which(data.combined$sex == "female" & (data.combined$new.title == "Master." |
                  data.combined$new.title == "Mr.")))
length(which(data.combined$sex == "male" & 
               (data.combined$new.title == "Mrs." |
                  data.combined$new.title == "Miss.")))

#Refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

#Which males survived in first class?
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

#Look at a few high fare tickets, using ticket number based on observation
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

#Look into issue that there may be ticket relationships not shown by
#SibSp or Parch
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by fare")

# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

#Refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

#visualize new features involving fare and party size
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare")

#Hypothesis - ticket party size is highly correlated with avg.fare
summary(data.combined$avg.fare)

#What is the missing value?
data.combined[is.na(data.combined$avg.fare),]

# Get records for similar passengers and summarize avg.fares
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & FamilySize == 1 &
                                       Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

#Normalize the data with caret
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
# Hypothesis refuted again, but substantial increase to me

# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "FamilySize", "new.title", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results
rpart.3.cv.1 <- rpart.cv(94621, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)
# Result, still have not found a way to distinguish that 1st class males
# are more likely to survive than other males

#Report new scores to see how well we predicted, 80%
test.submit.df <- data.combined[892:1309, features]
rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)
write.csv(submit.df, file = "TitanicResultsTest2.csv", row.names = FALSE)

#Expand from single tree back to random forest with new features, 
features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]

rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)
write.csv(submit.df, file = "TitanicResultsTest3.csv", row.names = FALSE)

#Further improving the model using mutual information to determine
#where get things wrong most often, likely in adult males where you
#do not have good features
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Title[1:891])
mutinformation(rf.label, data.combined$FamilySize[1:891])
mutinformation(rf.label, data.combined$Ticket.firstchar[1:891])
mutinformation(rf.label, data.combined$Cabin.multiple[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))

#Use the tsne algorithm for visual work on improvement
library(Rtsne)

most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "None")
set.seed(984357)
tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2], 
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title Other than 'Mr.'")

# To get a baseline, let's use conditional mutual information on the tsne X and
# Y features for females and boys in 1st and 2nd class. The intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above.
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))


# As one more comparison, we can leverage conditional mutual information using
# the top two features used in our tree plot - new.title and pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "Pclass")])


# OK, now let's take a look at adult males since our model has the biggest 
# potential upside for improving (i.e., the tree predicts incorrectly for 86
# adult males). Let's visualize with tsne.
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "None")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2], 
                 color = misters$survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for new.title of 'Mr.'")


# Now conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# Idea - How about creating tsne features for all of the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2], 
                 color = data.combined$survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for all Training Data")

# Now conditional mutual information for tsne features for all training
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# Add the tsne features to our data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]
