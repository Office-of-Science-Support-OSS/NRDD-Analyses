# R Studio Version 1.4.1717

# Date: 08/03/2022
# Contact: nrdd.admin@noaa.gov or ishrat.jabin@noaa.gov for questions/concerns

# This R code will perform supervised classification on the data using 
# Multinomial Naive Bayes Algorithm. 

# READ ME:
# 1. Ensure that all libraries are installed/loaded before running code. 
#    Different versions of R may not be compatible with the R packages.
# 2. Select an xlsx file (A pop-up display will ask you to select a file from 
#    your directory) that contains the NRDD Projects with their
#    associated labels. Each R&D project should be pre-labeled into a topic area in 
#    order to run supervised classification. 
# 3. A sample dummy .xlsx file is provided in the Github repo. The file contains
#    the Project NRDD ID, Title, Description, Benefits, Outcome, RL, Label, LO.
# 4. Go ahead an fill the dummy data with your real data.


###############################################################################
# Libraries ---------------------------------------------------------------

library(xml2)
library(rvest)
library(janitor)
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(naivebayes)


# Select HTML Data File, then load table ---------------------------------------

# Select Projects that are labeled as the topic 'Heat'
Projects <- read_xlsx(file.choose(), sheet = 1) %>%
  as_tibble()

# Projects by Labs/Offices -----------------------------------------------------
topic <-  Projects %>%
  count(Label)

# Factor Classes
# 2 Levels ---2 classes!
Projects$Label <- factor(Projects$Label)

# Shuffle the dataset in random order and get CLASS ----------------------------
# get random selection of 80% of the data for training
set.seed(1)
rows <- sample(nrow(Projects))
Projects <- Projects[rows, ]

# Term Frequencies  ------------------------------------------------------------
# doc --> Proj ID
# corpus --> Label
# We are classifying by topic, therefore our doc id is 'Label'

# combine title and description terms to become one doc per project ID
# then count how many times a word appears PER Label

# Column 1 is NRDD Project ID
# Column 2 is Project title
# Column 3 is LO (Line Office)
# Column 4 is Project Description
# Column 5 is Project Benefits
# Column 6 is Project Outcome
# Column 7 is Current RL
# Column 8 is Label

title <- Projects %>%
  select(c(1,2,8)) %>%
  unnest_tokens(word, Project.Title)

description <- Projects %>%
  select(c(1,4,8)) %>%
  unnest_tokens(word, Project.Description)

benefits <- Projects %>%
  select(c(1,5,8)) %>%
  unnest_tokens(word, Project.Benefits)

outcome <- Projects %>%
  select(c(1,6,8)) %>%
  unnest_tokens(word, Project.Outcome)

df <- bind_rows(title, description)
df <- bind_rows(df, benefits)

df <- bind_rows(df, outcome) %>%
  arrange(Label) %>%
  count(Label, NRDD.Project.ID, word, sort = TRUE)


# PLOT -------------------------------------------------------------------------
# Visualize a set of LOs with their top 5 words

# df %>%
#   group_by(Label) %>%
#   arrange(desc(n), .by_group = TRUE) %>%
#   mutate(word = factor(word, levels = rev(unique(word)))) %>%
#   slice(1:10) %>%
#   ungroup() %>%
#   mutate(word = reorder_within(word, n, Label)) %>%
#   ggplot(aes(word,n, fill = Label)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "Top words by Label", title = "Top 5 Terms ") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   facet_wrap(~Label, ncol = 2, scales = "free") +
#   coord_flip() +
#   scale_x_reordered()

# NAIVE BAYES ------------------------------------------------------------------

# cast document term matrix or document feature matrix 
dtm_df <- df %>%
  cast_dtm(NRDD.Project.ID, word, n)

# split into 80 and 20 
row <- round(.8*nrow(Projects), digits = 0)

# split into training and test sets
dtm_train <- dtm_df[1:(row), ]
dtm_test  <- dtm_df[nrow(Projects)-row:nrow(Projects), ]

id <- as.data.frame(as.numeric(dtm_train[["dimnames"]][["Docs"]]))
train_class <- id %>%
  left_join(Projects, by = c('as.numeric(dtm_train[["dimnames"]][["Docs"]])' = 'NRDD.Project.ID')) %>%
  select(c(1,8))
# Column 8 is the Label column
train_class$Label <- factor(train_class$Label)

id <- as.data.frame(as.numeric(dtm_test[["dimnames"]][["Docs"]]))
test_class <- id %>%
  left_join(Projects, by = c('as.numeric(dtm_test[["dimnames"]][["Docs"]])' = 'NRDD.Project.ID')) %>%
  select(c(1,8))
# Column 8 is the Label column
test_class$Label <- factor(test_class$Label)

# Train the Model --------------------------
Matrix_train <- as.matrix(dtm_train)
mod <- multinomial_naive_bayes(x = Matrix_train, y = train_class$Label, laplace = 1)

summary(mod)

# Prediction ---------------------------------
Matrix_test <- as.matrix(dtm_test)
y_pred <- predict(mod, newdata = Matrix_test, type = "class") # head(mnb %class% M)

# Confusion Matrix
cm <- as.data.frame(table(test_class$Label, y_pred)) %>%
  mutate(isdiag = (Var1 == y_pred)) %>%
  arrange(isdiag)

# PLOT CONFUSION MATRIX --------------------------------------------------------

#autoplot(cm2, type = "heatmap") + scale_fill_distiller(palette = 'Spectral')

# Edit Text Font
# Run this section of code only once and then comment out.
font_add_google("Alegreya Sans", "aleg")
head(font_families_google())
showtext_auto()

# Heatmap Plot
dev.new()

ggplot(cm, aes(y = Var1, x = y_pred, fill = Freq)) + 
  geom_tile(colour = "white") +
  geom_tile(size = 1, colour = cm$isdiag) +
  labs(y = 'Observed', x = "Prediction", title = "Confusion Matrix: Predicting Topic using Project Description") +
  geom_text(aes(label=Freq)) +
  scale_fill_gradientn(colours = colorspace::heat_hcl(7)) +
  theme(text = element_text(size = 16, family = 'aleg')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

