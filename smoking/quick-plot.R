library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

smoking_raw <- read_csv2("https://github.com/datawookie/data-diaspora/raw/master/smoking-prevalence-lancet.csv") %>%
  filter(country != "Global")

change <- smoking_raw %>%
  select(country, ends_with("1990_2015")) %>%
  gather(gender, rate, -country) %>%
  separate(
    rate, "rate", " ", extra = "drop"
  ) %>%
  mutate(
    gender = sub(".*_(female|male).*", "\\1_rate", gender),
    rate = as.numeric(rate) / 100
  ) %>%
  spread(gender, rate)

prevalence <- smoking_raw %>%
  select(country, ends_with("prevalence_2015")) %>%
  gather(gender, prevalence, -country) %>%
  separate(
    prevalence, "prevalence", " ", extra = "drop"
  ) %>%
  mutate(
    gender = sub("^(female|male).*", "\\1_2015", gender),
    prevalence = as.numeric(prevalence) / 100
  ) %>%
  spread(gender, prevalence)

smoking <- prevalence %>% inner_join(change)

rm(change, prevalence)

smoking <- smoking %>%
  mutate(
    # Work backwards to 1990.
    male_1990 = male_2015 / (1 + male_rate)^25,
    female_1990 = female_2015 / (1 + female_rate)^25,
    # Total change.
    change_male = male_2015 - male_1990,
    change_female = female_2015 - female_1990
  )

plot_1 <- ggplot(smoking, aes(x = change_male, y = change_female)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  geom_point()

plot_2 <- ggplot(smoking, aes(x = change_male, y = change_female, label = country)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  geom_point() +
  theme_minimal() + 
  geom_text_repel(data = subset(smoking, change_male > 0.065 | change_male < -0.15 | change_female >= 0.035 | change_female < -0.10 ),
    segment.size  = 0.1,
    segment.color = "grey50") +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) +
  labs(title = "Change in Men's and Women's Smoking Prevalence from 1990 to 2015", subtitle = "Inspired from Twitter and The Economist", caption = "Data from The Lancet") + labs(x = "Change in Men's Prevalence", y = "Change in Women's Prevalence") + theme(legend.position = "none") + annotate("text", x = 0.065, y = 0.04, label = "Both up")  + annotate("text", x = -0.065, y = -0.11, label = "Both down") + annotate("text", x = 0.05, y = -0.11, label = "Men up, Women down") +  annotate("text", x = -0.05, y = 0.06, label = "Men down, Women up")


