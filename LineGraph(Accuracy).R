ibrary(readxl)
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

accuracy_per_article <- data %>%
  summarize(Article1 = mean(Article1_Correct),
            Article2 = mean(Article2_Correct),
            Article3 = mean(Article3_Correct),
            Article4 = mean(Article4_Correct),
            Article5 = mean(Article5_Correct),
            Article6 = mean(Article6_Correct),
            Article7 = mean(Article7_Correct),
            Article8 = mean(Article8_Correct),
            Article9 = mean(Article9_Correct),
            Article10 = mean(Article10_Correct))

accuracy_long <- accuracy_per_article %>%
  gather(key = "Article", value = "Accuracy")

accuracy_long <- accuracy_long %>%
  mutate(Article_Number = as.numeric(gsub("Article", "", Article)))

ggplot(accuracy_long, aes(x = Article_Number, y = Accuracy * 100, group = 1)) +
  geom_line(color = "Green", size = 1) +  
  geom_point(color = "darkblue", size = 2) +  
  geom_text(aes(label = sprintf("%.1f%%", Accuracy * 100)), vjust = -1.2, size = 3.5, colour = "Purple") +
  labs(title = "Accuracy Over 10 Articles",
       x = "Article Number", y = "Accuracy (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = 1:10)  
