# ---- relay-races
library(tidyverse)
library(relayraces)
data("Sprints")


recode_race <- function(frame) {
  race_levels <- c("4x100m", "100m", "400m")
  race_labels <- c("4 x 100m\nrelay", "100m x 4", "400m")
  race_map <- tibble(
    Race = race_levels,
    RaceLabel = factor(race_levels, levels = race_levels, labels = race_labels)
  )
  left_join(frame, race_map)
}


Sprints100m <- Sprints %>%
  filter(Race == "100m") %>%
  bind_rows(
    `1` = .,
    `2` = .,
    `3` = .,
    `4` = .,
    .id = "Leg"
  ) %>%
  mutate(Leg = as.integer(Leg),
         FirstLeg = (Leg == 1))

SprintRecords <- Sprints %>%
  filter(Race != "100m") %>%
  mutate(FirstLeg = TRUE) %>%
  bind_rows(Sprints100m) %>%
  filter(Rank == 1, Gender == "M") %>%
  recode_race()


Labels <- SprintRecords %>%
  group_by(Race) %>%
  summarize(Time = sum(Time)) %>%
  recode_race()

ggplot(SprintRecords) +
  aes(RaceLabel, Time) +
  geom_bar(aes(color = FirstLeg, fill = FirstLeg), stat = "identity", position = "stack") +
  geom_text(aes(y = Time, label = Time),
            data = Labels, nudge_y = 2, size = 5) +
  labs(x = "", y = "Time (s)") +
  scale_color_manual(values = c("gray", "gray")) +
  scale_fill_manual(values = c(NA, "gray")) +
  coord_flip(ylim = c(0, 49), expand = FALSE) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())
