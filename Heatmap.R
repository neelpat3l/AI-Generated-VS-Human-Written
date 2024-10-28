library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

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

heatmap_data <- data %>%
  mutate(Overall_Correct = rowMeans(select(., Article1_Correct:Article10_Correct))) %>%
  group_by(Age, Education, News_Frequency) %>%
  summarize(Average_Accuracy = mean(Overall_Correct))

ggplot(heatmap_data, aes(x = Age, y = Education, fill = Average_Accuracy)) +
  geom_tile() +
  facet_wrap(~ News_Frequency, ncol = 2) + 
  scale_fill_gradient(low = "white", high = "red") + 
  labs(title = "Heatmap of Accuracy by Age, Education, and Media Exposure",
       x = "Age Group", y = "Education Level", fill = "Avg Accuracy") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 