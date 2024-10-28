library(readxl)
library(dplyr)

data <- read_excel("Clean Data AI vs Real News.xlsx")

correct_answers <- c("AI Generated", "Real News", "Real News", "AI Generated", 
                     "AI Generated", "Real News", "AI Generated", "Real News", 
                     "AI Generated", "Real News")

data <- data %>%
  mutate(Article1_Correct = ifelse(Article1_Identification == correct_answers[1], 1, 0),
         Article2_Correct = ifelse(Article2_Identification == correct_answers[2], 1, 0),
         Article3_Correct = ifelse(Article3_Identification == correct_answers[3], 1, 0),
         Article4_Correct = ifelse(Article4_Identification == correct_answers[4], 1, 0),
         Article5_Correct = ifelse(Article5_Identification == correct_answers[5], 1, 0),
         Article6_Correct = ifelse(Article6_Identification == correct_answers[6], 1, 0),
         Article7_Correct = ifelse(Article7_Identification == correct_answers[7], 1, 0),
         Article8_Correct = ifelse(Article8_Identification == correct_answers[8], 1, 0),
         Article9_Correct = ifelse(Article9_Identification == correct_answers[9], 1, 0),
         Article10_Correct = ifelse(Article10_Identification == correct_answers[10], 1, 0))

data <- data %>%
  rowwise() %>%
  mutate(Overall_Accuracy = mean(c(Article1_Correct, Article2_Correct, Article3_Correct,
                                   Article4_Correct, Article5_Correct, Article6_Correct,
                                   Article7_Correct, Article8_Correct, Article9_Correct,
                                   Article10_Correct)))

data$News_Frequency <- as.numeric(as.factor(data$News_Frequency))  # Convert to numeric

correlation_result <- cor.test(data$News_Frequency, data$Overall_Accuracy, method = "pearson")

cat("Pearson's Correlation Test Results between News Frequency and Overall Accuracy:\n")
print(correlation_result)
