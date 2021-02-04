library(tidyverse)

questions <- read.csv(here::here("data", "post_dspg", "questions.csv"))
answers <- read.csv(here::here("data", "post_dspg", "answers.csv"))
responses <- read.csv(here::here("data", "post_dspg", "responses.csv"))
nrow(responses) # 14,505,786

s32wq2 <- responses %>%
  filter(question_id == "S032W.Q2")
nrow(s32wq2) # 4793

s32nq2 <- responses %>%
  filter(question_id == "S032N.Q2")
nrow(s32nq2)

levels(as.factor(tmp$label))


