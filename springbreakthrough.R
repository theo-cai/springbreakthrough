library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
#install.packages("ggstats")
library(ggstats)

#Overall, how satisfied were you with your Spring Breakthrough experience? 
student %>%
  ggplot(aes(x = Q1)) +
  geom_bar(stat = "count") +
  ggtitle("How satisfied were you with your Spring Breakthrough experience?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size= 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

#Would you recommend Spring Breakthrough to a Duke student similar to yourself?
student %>%
  group_by(Q2) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(Q2, -count))) + 
  geom_bar(stat = "count") +
  ggtitle("Would you recommend Spring Breakthrough to a Duke student similar to yourself?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 18, face = "bold",),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))

#How important are the following aspects of your Spring Breakthrough program to you? 
Q3 <- student %>%
  mutate(across(3:8, ~ factor(.x, levels = c("Not at all important",
                                              "Slightly important",
                                              "Moderately important",
                                              "Very important",
                                              "Extremely important"))))


gglikert(Q3, labels_size = 6, include = Q3_1:Q3_8, sort = "ascending",
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q3_1 = "Program topic and content",
           Q3_2 = "Interaction with program leaders",
           Q3_3 = "Interaction with peers in the program",
           Q3_4 = "POE transcript notation",
           Q3_5 = "Field trip opportunities",
           Q3_6 = "Learning new things",
           Q3_7 = "Group meals",
           Q3_8 = "Conference flex food points"
         )) +
  ggtitle("How important are the following aspects of your Spring Breakthrough program to you?") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20))

#Please rate the following statements about how Spring Breakthrough might have 
#influenced your intellectual experience at Duke.
Q4 <- student %>%
  filter(Q4_1 != "") %>%
  filter(Q4_2 != "") %>%
  filter(Q4_3 != "") %>%
  filter(Q4_4 != "") %>%
  filter(Q4_5 != "") %>%
  mutate(across(9:13, ~ factor(.x, levels = c("Not at all",
                                             "Slightly",
                                             "Moderately",
                                             "Greatly"))))

gglikert(Q4, labels_size = 6, include = Q4_1:Q4_5, sort = "ascending",
         sort_method = "mean", y_label_wrap = 25,
         variable_labels = c(
           Q4_1 = "Influenced my ultimate course of study at Duke",
           Q4_2 = "Influenced my career path",
           Q4_3 = "Led me to engage in new areas of study",
           Q4_4 = "Made me feel more enthusiastic about my academic work at Duke",
           Q4_5 = "Contributed to my sense of belonging at Duke"
         )) +
  ggtitle("Rate the statement: My Spring Breakthrough experience...") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 20))


##LEADS
#Do you think Spring Breakthrough adds something valuable to the Duke undergraduate experience?
leads %>%
  ggplot(aes(x = Q2)) +
  geom_bar(stat = "count") +
  ggtitle("Do you think Spring Breakthrough adds something valuable to the Duke undergraduate experience?") +
  xlab("") + ylab("Number of Responses") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 18))
