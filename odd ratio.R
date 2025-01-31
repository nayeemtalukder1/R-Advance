library(tidyverse)
theme_set(theme_bw())

ts <- carData::TitanicSurvival

set.seed(123)
d <- rbind(
  ts %>% filter(sex == "female" & survived == "yes") %>%
    sample_n(70),
  ts %>% filter(sex == "female" & survived == "no") %>%
    sample_n(30),
  ts %>% filter(sex == "male" & survived == "yes") %>%
    sample_n(20),
  ts %>% filter(sex == "male" & survived == "no") %>%
    sample_n(80)
)

xtabs(data = d, ~ survived + sex)

m <- glm(survived ~ sex, d, family = binomial())

library(emmeans)

emmeans(m, ~ sex, type = "response")

# Visualize probabilities
s_d <- tibble(
  id = seq(1:10),
  survival_probability = c(0.50, 0.67, 0.75, 0.80, 0.83, 0.86, 0.88, 0.89, 0.90, 0.91),
  death_probability = 1 - survival_probability
)

ggplot(s_d) +
  geom_point(aes(id, survival_probability)) +
  geom_line(aes(id, survival_probability), color = "green") +
  geom_point(aes(id, death_probability)) +
  geom_line(aes(id, death_probability), color = "red") +
  ylab("Probability of survival") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  geom_vline(xintercept = 4, linetype = 2)

ggplot(s_d) +
  geom_point(aes(id - 1, survival_probability)) +
  geom_line(aes(id - 1, survival_probability), color = "green") +
  geom_point(aes(id + 1, death_probability)) +
  geom_line(aes(id + 1, death_probability), color = "red") +
  ylab("Probability") +
  xlab("")

# Non-linear nature of probabilities
m_a <- glm(survived ~ sex * age, d, family = binomial())

emmeans(m_a, ~ sex | age, type = "response",
        at = list(age = c(30, 60)))

library(sjPlot)  # Extra video on my channel
plot_model(m_a, type = "eff", terms = c("age", "sex"))


# Odds and odds-ratios
s_d <- tibble(
  id = c(seq(11, 1, -1), seq(12, 21)),
  happening = c(rep(1, 11), rep(10, 10)),
  NOT_happening = c(seq(1, 10), 1000, c(seq(9, 2, by = -1), 0.01, 0.001))
) %>%
  unite("odds_are_ratios", happening:NOT_happening, sep = "/", remove = F) %>%
  mutate(
    odds = round(happening / NOT_happening, 3)
  ) %>%
  select(id, happening, NOT_happening, everything())
install.packages("flextable")
library(flextable)

s_d %>% flextable::regulartable()
