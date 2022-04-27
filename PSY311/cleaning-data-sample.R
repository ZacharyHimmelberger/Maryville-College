# PSY311
# Cleaning Data Sample

# load libraries
library(tidyverse)

# load data
raw_df <- read.csv("https://raw.githubusercontent.com/ZacharyHimmelberger/Maryville-College/main/PSY311/sample-data.csv")

# examine data
glimpse(raw_df)
head(raw_df)
colnames(raw_df) # useful for copying column names during next step

# clean data
df <- raw_df %>%
  # subset and rename columns
  select(
    gender = What.is.your.gender.,
    age = What.is.your.age.,
    lonely = I.feel.lonely.,
    sleep = I.have.trouble.sleeping.or.I.sleep.too.much.,
    struggle = I.will.always.struggle.in.my.life.,
    happy = I.feel.happy.most.of.the.time.
  ) %>%
  # recode variables
  mutate_at(
    .vars = c("lonely", "sleep", "struggle"),
    .funs = recode,
    "strongly agree" = 3,
    "slightly agree" = 2,
    "slightly disagree" = 1,
    "strongly disagree" = 0
  ) %>%
  mutate_at(
    .vars = c("happy"), # negatively coded
    .funs = recode,
    "strongly agree" = 0,
    "slightly agree" = 1,
    "slightly disagree" = 2,
    "strongly disagree" = 3
  ) %>%
  # calculate composite score
  mutate(
    depression = lonely + sleep + struggle + happy
  )
  
# descriptive statistics
descriptive_stats <- df %>%
  group_by(gender) %>% 
  summarise(
    depression_mean = mean(depression),
    depression_sd = sd(depression)
  )
  
# graphs
ggplot(data = df, mapping = aes(x = depression)) +
  geom_dotplot(binwidth = 1, fill = "orange3") +
  scale_x_continuous(name = "Depression",
                     limits = c(0, 12),
                     breaks = seq(0, 12, by = 1))

ggplot(data = df, mapping = aes(x = gender, y = depression)) +
  geom_boxplot(fill = "darkgreen") +
  scale_x_discrete(name = "Gender",
                   labels = c("Female", "Male", "Non-Binary"))

ggplot(data = df, mapping = aes(x = age, y = depression)) +
  geom_point(shape = 2, size = 2) +
  geom_smooth(method = "lm", 
              color = "black", 
              size = .5,
              alpha = .2) +
  scale_x_continuous(name = "Age") +
  scale_y_continuous(name = "Depression",
                     limits = c(0, 12),
                     breaks = seq(0, 12, by = 2)) + 
  theme_classic()
