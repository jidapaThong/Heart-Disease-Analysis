library(tidyr)
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)


# ---- Section 3 - Data Preparation ----
# this section's purpose is to prepare the data by 
# structuring and organizing the data that are going
# to be used in analyzing process
data <- read.csv('heart.csv')

colnames(data)[which(names(data) == "target")] <- "hd"

data$sex <- ifelse(test = data$sex == 1, yes = "M", no = "F")

data$hd <- ifelse(test = data$hd == 1, yes = "Y", no = "N")

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)
data$hd <- as.factor(data$hd)


# ---- Section 4 - Data Exploration and Visualization ----
#
data_ncol <- ncol(data)
data_names <- names(data)
data_nrow <- nrow(data)
data_summary <- summary(data)
data_str <- str(data)
data_ncolmis <- colSums(is.na(data))

hd_sex <- xtabs(~ hd + sex, data = data)
hd_cp <- xtabs(~ hd + cp, data = data)
hd_fbs <- xtabs(~ hd + fbs, data = data)
hd_restecg <- xtabs(~ hd + restecg, data = data)
hd_exang <- xtabs(~ hd + exang, data = data)
hd_slope <- xtabs(~ hd + slope, data = data)
hd_ca <- xtabs(~ hd + ca, data = data)
hd_thal <- xtabs(~ hd + thal, data = data)

chisq.test(hd_sex, simulate.p.value = TRUE)
chisq.test(hd_cp, simulate.p.value = TRUE)
chisq.test(hd_fbs, simulate.p.value = TRUE)
chisq.test(hd_restecg, simulate.p.value = TRUE)
chisq.test(hd_exang, simulate.p.value = TRUE)
chisq.test(hd_slope, simulate.p.value = TRUE)
chisq.test(hd_ca, simulate.p.value = TRUE)
chisq.test(hd_thal, simulate.p.value = TRUE)

ggplot(data, aes(x = sex, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = cp, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = fbs, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = restecg, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = exang, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = slope, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = ca, fill = hd)) + geom_bar(position = "fill")
ggplot(data, aes(x = thal, fill = hd)) + geom_bar(position = "fill")
ggplot(data) + geom_histogram(mapping = aes(x = age, fill = hd))
ggplot(data) + geom_histogram(mapping = aes(x = trestbps, fill = hd))
ggplot(data) + geom_histogram(mapping = aes(x = chol, fill = hd))
ggplot(data) + geom_histogram(mapping = aes(x = thalach, fill = hd))
ggplot(data) + geom_histogram(mapping = aes(x = oldpeak, fill = hd))




# ---- Section 6 - Model Implementation ----
#
sample_size <- floor(0.7 * nrow(data))
set.seed(3448)
train_index <- sample(seq_len(nrow(data)), size = sample_size)
data_training <- data[train_index, ]
data_testing <- data[-train_index, ]

model <- glm(hd ~ ., data = data_training, family = "binomial")
model_summary <- summary(model)


# ---- Section 7 - Evaluation ----
#
response <- predict(model, data_testing)

response_factor <- as.factor(ifelse(test = response > 0.6, yes = "Y", no = "N"))
confusionMatrix(response_factor, data_testing$hd, mode = "prec_recall", positive = "Y")


ncol(data)
names(data)
nrow(data)
summary(data)
str(data)
colSums(is.na(data))

install.packages('corrplot')
library(corrplot)
corrplot(cor(data), 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)