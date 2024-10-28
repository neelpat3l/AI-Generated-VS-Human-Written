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


education_accuracy <- data %>%
  gather(key = "Article", value = "Correct", Article1_Correct:Article10_Correct) %>%
  group_by(Education) %>%
  summarize(Correct = sum(Correct), Incorrect = n() - sum(Correct)) %>%
  gather(key = "Identification", value = "Count", Correct, Incorrect)


ggplot(education_accuracy, aes(x = Education, y = Count, fill = Identification)) +
  geom_bar(stat = "identity", width = 0.7) +  # Narrower bars
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +  # Adding count labels
  labs(title = "Accuracy by Education Level (Correct vs Incorrect)",
       x = "Education Level", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Correct" = "green", "Incorrect" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

