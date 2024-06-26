#Loading my environment
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library("readxl")
Navco =read_excel ("C:/Users/aleta/Documents/ASU/NAVCO2-1_ForPublication (1).xls")
FandL = read_excel("C:/Users/aleta/Documents/ASU/f&l.xlsx")

#print out the size of the data set
cat("Number of rows:", nrow(Navco), "/n")
cat("Number of columnss:", ncol(Navco), "/n")
cat("Number of rows:", nrow(FandL), "/n")
cat("Number of columnss:", ncol(FandL), "/n")

#Print out column name and type of data
str(Navco)
str(FandL)

#Check on categorical vs numeric variables in Navco
column_types <- sapply(Navco, class)

# Extract categorical variables (factors and characters)
categorical_variables <- names(column_types[column_types %in% c("factor", "character")])

# Extract numerical variables
numerical_variables <- names(column_types[column_types == "numeric"])

# Print the lists of categorical and numerical variables
cat("Categorical variables:\n")
cat(categorical_variables, sep = ", ")

cat("\n\nNumerical variables:\n")
cat(numerical_variables, sep = ", ")

#Check on categorical vs numeric variables in FandL
column_types <- sapply(FandL, class)

# Extract categorical variables (factors and characters)
categorical_variables <- names(column_types[column_types %in% c("factor", "character")])

# Extract numerical variables
numerical_variables <- names(column_types[column_types == "numeric"])

# Print the lists of categorical and numerical variables
cat("Categorical variables:\n")
cat(categorical_variables, sep = ", ")

cat("\n\nNumerical variables:\n")
cat(numerical_variables, sep = ", ")

#Remove some years so they match
FandLyear <-unique(FandL$year)
print(FandLyear)
Navcoyear <-unique(Navco$year)
print(Navcoyear)

FLYear <- subset(FandL, as.numeric(year) >= 1945 & as.numeric(year) <= 1991)
FLYear2 <-unique(FLYear$year)
print(FLYear2)

#Rename so variables match
names(FLYear)[names(FLYear) == "ccode"] <- "loc_cow"

#subset data to those of interest
subsetFL<- FLYear[, c("loc_cow", "year", "durest", "ethwar", "pop", "ethfrac", "relfrac")]
print(subsetFL)

missing_values <- is.na (Navco)
missing_count <- colSums(is.na(Navco))

#Merge code from Namig
navco_fl_left <- merge(Navco, subsetFL, by = c("loc_cow", "year"), all.x = TRUE)

variables<-colnames(navco_fl_left)
print(variables)

#Create some charts to better see the DF
hist(navco_fl_left$prim_meth, main = "Primary Method", col = "gray", border = "black")

hist(navco_fl_left$ethfrac, 
     main = "Histogram of Ethnic Div",
     xlab = "Values",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")


hist(navco_fl_left$relfrac, 
     main = "Histogram of Religious Div",
     xlab = "Values",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")

hist(navco_fl_left$gdpen, 
     main = "Histogram of GDP",
     xlab = "Values",
     ylab = "Frequency",
     col = "salmon",
     border = "black")


hist(navco_fl_left$pop, 
     main = "Histogram of Population",
     xlab = "Values",
     ylab = "Frequency",
     col = "salmon",
     border = "black")

hist(navco_fl_left$lpop, 
     main = "Histogram of Population",
     xlab = "Values",
     ylab = "Frequency",
     col = "salmon",
     border = "black")

str(navco_fl_left)


library(ggplot2)

install.packages("gplots")
library(gplots)
# Create a heatmap
subsetN <- navco_fl_left[, c("durest", "ethwar", "pop", "ethfrac", "relfrac", "prim_meth", "camp_size_cat", "camp_goals", "div_gender", "div_age", "div_class", "div_urban_rural", "div_ideology", "div_party", "div_regional", "div_ethnicity", "div_religion", "repression", "camp_support", "ingo_support", "dias_support")]

library(gplots)
cleanedsub <- na.omit (subsetN)

data(cleanedsub)
x <- as.matrix(cleanedsub)

heatmap.2(x,Rowv=TRUE, Colv=FALSE, scale="column", trace="none", col=redgreen, xlab="sample", ylab="phylum", margins=c(10,15))

png(filename="output/phylum.png", width=1200, height=800)
heatmap.2(x,Rowv=TRUE, Colv=FALSE, scale="column", trace="none", 
          col=redgreen, xlab="diet type", ylab="phylum", 
          ColSideColors=sidebarcolors, margins=c(15,15)) 
dev.off()
class(cleanedsub)         
str(subsetN)

# Create a heatmap using ggplot2 with missing values removed
ggplot(data, aes(x = variable1, y = variable2, fill = variable3)) +
  geom_tile(data = na.omit(data)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()

missing_count <- colSums(is.na(cleanedsub))
sorted_missing <- sort(missing_count, decreasing = TRUE)
print(sorted_missing)

#logistic regression
model <- glm(prim_meth ~  durest + ethwar + ethfrac + relfrac + camp_size_cat + camp_goals + div_gender + div_age + div_class + div_urban_rural + div_ideology + div_party + div_regional + div_ethnicity + div_religion + repression + camp_support + ingo_support + dias_support, data = cleanedsub, family = binomial)

# Summary of the model
summary(model)

#check for outliers
boxplot(cleanedsub)

#Basic predictive model using lm
set.seed(123)
index <- sample(nrow(cleanedsub), 0.7 * nrow(cleanedsub))

# Split data into train and test sets
train_data <- cleanedsub[index, ]
test_data <- cleanedsub[-index, ]

model <- lm(prim_meth~ durest + ethwar + ethfrac + relfrac + camp_size_cat + camp_goals + div_gender + div_age + div_class + div_urban_rural + div_ideology + div_party + div_regional + div_ethnicity + div_religion + repression + camp_support + ingo_support + dias_support,data=train_data)
summary(model)

predictions <- predict(model, newdata = test_data)
actuals <-test_data$prim_meth
mse <-mean((predictions - actuals)^2)
rmse <- sqrt(mse)

#checking that an error was false
any(is.na(test_data$Y))
any(is.na(predictions))

library(ggplot2)

ggplot(navco_fl_left, aes(x = year, y = prim_meth)) +
  geom_line() +
  labs(x = "Year", y = "Conflict type", title = "Line Plot by Year")

# Bar plot
ggplot(navco_fl_left, aes(x = year, y = prim_meth)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Conflict count", title = "Bar Plot by Year")

#By type of conflict
counts <- tapply(navco_fl_left$prim_meth, navco_fl_left$year, sum)
barplot(counts, names.arg = unique(navco_fl_left$year),
        xlab = "Year", ylab = "Type of Conflict", 
        main = "Count of Conflict Type by Year", col = "skyblue")


counts <- tapply(navco_fl_left$prim_meth, navco_fl_left$year, table)

counts_df <- as.data.frame(do.call(rbind, counts))

counts_df$year <- as.numeric(names(counts))

# Plot of conflict type by year
barplot(as.matrix(counts_df[,-ncol(counts_df)]), beside = TRUE, legend.text = TRUE, col = c("blue", "red"),
        xlab = "Year", ylab = "Count", main = "Counts of Binary Variable by Year")


counts <- data.frame(year = unique(navco_fl_left$year),
                     count = tapply(navco_fl_left$prim_meth, navco_fl_left$year, sum))


# RandomForest check
library(randomForest)

rf_model <- randomForest(prim_meth ~ durest + ethwar + ethfrac + relfrac + camp_size_cat + camp_goals + div_gender + div_age + div_class + div_urban_rural + div_ideology + div_party + div_regional + div_ethnicity + div_religion + repression + camp_support + ingo_support + dias_support, data = cleanedsub, ntree = 100, importance = TRUE)
predictions <- predict(rf_model, newdata = test_data)
confusion_matrix <- table(true_labels, predictions)
accuracy <- mean(predictions == true_labels)

# Predictions on new data
new_data <- cleanedsub(X1 = c(1, 2, 3), X2 = c(4, 5, 6))
predicted_probs <- predict(model, newdata = new_data, type = "response")



#another try at getting a visual on conflict over time
plotdata_long <- filter(navco_fl_left,
                        prim_meth == "0s" &
                          year %in% c(1945, 1990)) %>%
  select(prim_meth, year)

# convert data to wide format
plotdata_wide <- pivot_wider(plotdata_long, 
                             names_from = year, 
                             values_from = prim_meth)
names(plotdata_wide) <- c("country", "y1952", "y2007")

# create dumbbell plot
ggplot(plotdata_wide, aes(y = country,
                          x = y1952,
                          xend = y2007)) +  
  geom_dumbbell()


