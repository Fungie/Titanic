
# Libraries ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(data.table)
library(mice)
library(randomForest)
library(gridExtra)
library(stringr)


# Reading data ------------------------------------------------------------
# setting working directory
setwd("~/Documents/Titanic")

# reading training and test data
train <- as.tbl(fread(input = "./data/train.csv", na.strings = c("", " ")))
test <- as.tbl(fread(input = "./data/test.csv", na.strings = c("", " ")))
comb <- bind_rows("train" = train, "test" = test, .id = "originates")

# removing train and test
rm(list = c("train", "test"))
gc()
  

# Basic EDA ---------------------------------------------------------------
str(comb)
summary(comb)

# dropping PassengerID
#comb$PassengerId <- NULL

# Checking how many uniques of each column
sapply(comb, function (x) {length(unique(x))})

# Number of NA's in each column
sapply(comb, function(x) {sum(is.na(x))})


# Extracting titles from names ------------------------------------------------------------------
comb$Title <- gsub('(.*, )|(\\..*)', '', comb$Name)
table(comb$Title)

# reducing number of titles
military_titles <- c("Major", "Col", "Capt")
unusual_titles <- c("Don", "Rev", "Sir", "Mme", "Lady", "Mlle", "the Countess", "Jonkheer", "Dona")

comb$Title <- ifelse(comb$Title %in% military_titles, "Military",
                         ifelse(comb$Title %in% unusual_titles, "Unusual Title", comb$Title))

ggplot(data = comb, aes(x = Title, fill = Title)) + 
  geom_bar() +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Titles of Titanic's Passengers") +
  theme(plot.title = element_text(hjust = 0.5))


# Age ---------------------------------------------------------------------
# ____ Cleaining up Age Na's ----------------------------------------------
# Make variables factors into factors
factor_vars <- c("Pclass", "Sex", "Ticket", "Cabin", "Embarked", "Title")

comb[factor_vars] <- lapply(comb[factor_vars], as.factor)

mice_imputation_df <- comb[, !colnames(comb) %in% c('Name','Ticket','Cabin','Family','Surname','Survived', 'originates')]

mice_imputation <- mice(data = mice_imputation_df, method='rf') 

output <- complete(mice_imputation)


# ____ Visualising --------------------------------------------------------

a <- bind_rows("Original" = comb, "Imputed" = output, .id = "Is_imputed")

ggplot(a, aes(x = Age, fill = Is_imputed)) + 
  geom_histogram(binwidth = 2, alpha = 0.5, position = "identity", color = "black") +
  ggtitle("Age Distribution") + 
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  theme(legend.title=element_blank())
  


# ____ Imputing ages into comb --------------------------------------------
comb$Age <- output$Age


# Embarked removing NA's --------------------------------------------------
#comb$Embarked <- as.factor(ifelse(is.na(comb$Embarked), "C", comb$Embarked))

# Class -------------------------------------------------------------------
comb$Sex <- ifelse(comb$Age < 16, "child", as.character(comb$Sex))

class_stats <- comb %>% group_by(Pclass) %>% 
  summarise(number = n(), 
            av_fare = mean(Fare, na.rm = T),
            num_titles = length(unique(Title)),
            num_male = sum(Sex == "male"),
            num_female = sum(Sex == "female"),
            num_children = sum(Sex == "child"),
            num_Cherbourg = sum(na.omit(Embarked) == "C"),
           num_Queenstown = sum(na.omit(Embarked) == "Q"),
            num_Southampton = sum(na.omit(Embarked) == "S"))

a <- ggplot(data = class_stats, aes(x = Pclass, y = number)) + 
  geom_bar(stat = "identity", fill = "#ed1010", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Passengers by Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

b <- ggplot(data = class_stats, aes(x = Pclass, y = av_fare)) + 
  geom_bar(stat = "identity", fill = "#ed6110", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Average Fare by Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

c <- ggplot(data = class_stats, aes(x = Pclass, y = num_titles)) + 
  geom_bar(stat = "identity", fill = "#dba62b", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Titles by Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

d <- ggplot(data = class_stats, aes(x = Pclass, y = num_male)) + 
  geom_bar(stat = "identity", fill = "#edd710", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Men by Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

e <- ggplot(data = class_stats, aes(x = Pclass, y = num_female)) + 
  geom_bar(stat = "identity", fill = "#10ed77", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Women by Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

f <- ggplot(data = class_stats, aes(x = Pclass, y = num_children)) + 
  geom_bar(stat = "identity", fill = "#10edcc", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Children by Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

g <- ggplot(data = class_stats, aes(x = Pclass, y = num_Cherbourg)) + 
  geom_bar(stat = "identity", fill = "#10cced", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Embarked from Cherbourg") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

h <- ggplot(data = class_stats, aes(x = Pclass, y = num_Queenstown)) + 
  geom_bar(stat = "identity", fill = "#1056ed", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Embarked from Queenstown") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

i <- ggplot(data = class_stats, aes(x = Pclass, y = num_Southampton)) + 
  geom_bar(stat = "identity", fill = "#ed10c4", color = "black") +
  guides(fill = FALSE) +
  theme_bw() +
  ggtitle("Embarked from Southampton") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Class") + ylab("Number")

grid.arrange(a, b, c, d, e, f, g, h, i, ncol = 3, nrow = 3)




# Cabin -------------------------------------------------------------------
# getting the level the person was staying on from Cabin
comb$Cabin<- substr(comb$Cabin,1,1)
comb <- rename(.data = comb, Level = Cabin)
comb$Level <- addNA(comb$Level)


# Family size -------------------------------------------------------------

comb <- comb %>% mutate(Family_size = 1 + SibSp + Parch) %>% select(-SibSp, -Parch)

ggplot(comb, aes(x = Family_size, fill = Pclass)) + 
  geom_histogram(binwidth = 1, alpha = 0.5, color = "black") +
  ggtitle("Family Size Distribution") + 
  theme_bw() +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) 
  #xlim(1, 11)
  #theme(legend.title=element_blank())

comb$Family_size <- as.factor(ifelse(comb$Family_size == 1, "single",
                                     ifelse(comb$Family_size > 1 & comb$Family_size <= 4, "small family",
                                            "large family")))






# Fare --------------------------------------------------------------------
filter(comb, is.na(Fare))
comb$Fare <- ifelse(is.na(comb$Fare), 13.30, comb$Fare)

# Preparing data for model ------------------------------------------------
sapply(comb, class)
factor_vars <- c("Sex", "Level", "Survived")

comb[factor_vars] <- lapply(comb[factor_vars], as.factor)


# Splitting up to train and test ------------------------------------------

train <- comb %>% filter(originates == "train")
test <- comb %>% filter(originates == "test")



# Random Forest Model -----------------------------------------------------
set.seed(1246)

# ____ Formula ------------------------------------------------------------
rf_formula <- as.formula(Survived ~ Pclass +
                                    Sex +
                                    Age +
                                    Fare +
                                    Level +
                                    Title +
                                    Family_size)

# ____ Model --------------------------------------------------------------
rf_model <- randomForest(rf_formula,
                         data = train)

# ____ Plotting error -----------------------------------------------------
plot(rf_model)
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


# ____ Importance ---------------------------------------------------------
rf_importance <- as.data.frame(importance(rf_model))
rf_importance$variables <- row.names(rf_importance)
rf_importance <- rf_importance %>% select(variables, MeanDecreaseGini) %>% arrange(desc(MeanDecreaseGini))
row.names(rf_importance) <- NULL

ggplot(data = rf_importance, aes(x = reorder(variables, MeanDecreaseGini),  y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "#4f7bbc", color = "black") +
  coord_flip() +
  xlab("Variables") + ylab("Gini") +
  ggtitle("Important Variables") +
  theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +  
  theme_bw()


# ____ Predict ------------------------------------------------------------
test$Survived <- NULL
  
p <-  predict(rf_model, newdata = test)
