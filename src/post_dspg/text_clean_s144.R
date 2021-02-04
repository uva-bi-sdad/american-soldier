library(tidyverse)
library(tidytext)
library(stringr)

s144 <- read.csv(here::here("data", "post_dspg", "Survey_144_export.csv"))

# unite S144.Q85.F and S144.Q85.C
s144 <- s144 %>% 
  unite(q85, S144.Q85.F, S144.Q85.C, Unnamed..6, sep = " ", na.rm = TRUE)


## handling unclear tags ##
# remove [unclear][/unclear] with no meaningful filler or with question mark
unclear.rm <- "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]"
# how many rows have at least one unclear tag that matches this regex pattern?
# sum(str_detect(s144$q85, unclear.rm))
# 386
s144 <- s144 %>%
  mutate(q85 = str_replace_all(q85, unclear.rm, ""))

# remove the unclear tags, but keep the words inside
unclear.keep <- "\\[unclear\\]|\\[\\/unclear\\]"
# how many rows still have unclear tags in some capacity?
# sum(str_detect(s144$q85, unclear.keep))
# 384
s144 <- s144 %>%
  mutate(q85 = str_replace_all(q85, unclear.keep, ""))


## handling paragraph tags ##
paragraph.rm <- "\\[paragraph\\]|\\[\\/paragraph\\]"
# sum(str_detect(s144$q85, paragraph.rm))
# 434
s144 <- s144 %>%
  mutate(q85 = str_replace_all(q85, paragraph.rm, ""))


## handling deletion tags ##
# remove [deletion][/deletion] and anything inside the tag
delete.rm <- "(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"
delete.rm2 <- "\\[deletion\\]|\\[\\/deletion\\]"
s144 <- s144 %>% 
  mutate(q85 = str_replace_all(q85, delete.rm, ""), 
         q85 = str_replace_all(q85, delete.rm2, ""))

# convert to lower case
s144 <- s144 %>%
  mutate(q85 = tolower(q85))

# remove punctuation
# remove extra spacing
# remove newlines
s144 <- s144 %>%
  mutate(q85 = str_replace_all(q85, "[^\\w\\s]", "")) %>% # remove punc
  mutate(q85 = str_replace_all(q85, "[\r\n]", " ")) %>% # remove newlines and carriage returns
  mutate(q85 = str_replace_all(q85, "\\s\\s+", " "))# remove extra spacing

s144 <- s144 %>%
  rename(id = X)
  
write.csv(s144, here::here("data", "post_dspg", "s144_clean_v1.csv"))
