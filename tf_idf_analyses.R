# R Studio Version 1.4.1717

# Date: 08/03/2022
# Contact: nrdd.admin@noaa.gov or ishrat.jabin@noaa.gov for questions/concerns

# This R code will perform basic tf-idf analysis by calculating the 
# tf-idf of each term with respect to its document and corpus identifier.

# In this analysis each project is a document. Each Line Office (lo) is a corpus 
# of several documents.

# You may change the document identifier and corpus as desired.
# For example, you can combine several program projects into a single document.
# or you may change the corpus to a lab or topic. 

# STOP words were NOT removed. You may include a line of code to filter out
# STOP words before performing tf-idf to improve results.

# READ ME:
# 1. Ensure that all libraries are installed/loaded before running code. 
#    Different versions of R may not be compatible with the R packages.
# 2. Export Project Information Table from QueryBuilder in HTML Table Format
# 3. Load HTML Table file in Lines 41-43. A pop-up display will ask you to
#    select a file from your directory.

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

# Select HTML Data File, then load table ---------------------------------------

htm_tbl <- read_html(file.choose()) # Select HTML file
Projects <- as.data.frame(html_table(htm_tbl, fill=TRUE)) #via HTML
Projects <- Projects %>% clean_names() %>% as_tibble()

# Define your corpus. In this case, the projects from each lo will be
# assigned as a corpus (collection of texts). Therefore, lo will be the 
# corpus identifier

fmc_count <- Projects %>%
  count(lo)

# remove the projects that do not have LO/Office assigned to them.
Projects <- Projects %>%
  filter(lo != '')

# Start formatting the data for tf-idf analysis --------------------------------

# Here we are unnesting the words from the sentences within the titles and 
# descriptions of each project.

title <- Projects %>%
  select(nrdd_project_id, project_title, lo) %>%
  unnest_tokens(word, project_title)

description <- Projects %>%
  select(nrdd_project_id, project_description, lo) %>%
  unnest_tokens(word, project_description)

# combine title and description terms into one variable
# then count how many times a word appears PER document (Project ID)
# each Project is its own document within a group of documents (corpus)

df_words <- bind_rows(title, description) %>%
  arrange(nrdd_project_id) %>%
  count(lo, word, sort = TRUE)

# perform bind_tf_idf() on document terms.
df_words <- df_words %>%
  bind_tf_idf(word, lo, n) %>%     # find tf and idf
  mutate(lo = factor(lo, levels = unique(lo)))

## PLOT ------------------------------------------------------------------------

dev.new()
df_words %>%
  group_by(lo) %>%
  arrange(desc(tf_idf), .by_group = TRUE) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  slice(1:10) %>% # select top 10 terms
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, lo)) %>%
  ggplot(aes(word, tf_idf, fill = lo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf", title = "Top 10 tf-idf Terms ") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~lo, ncol = 4, scales = "free") +
  coord_flip() +
  scale_x_reordered()
