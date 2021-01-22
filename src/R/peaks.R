# ---- peaks
library(tidyverse)
library(plotly)
library(magrittr)
library(gridExtra)

library(totems)
library(peaks)
library(crotchet)

library(RColorBrewer)

base_theme <- theme_minimal()

theme_colors <- brewer.pal(4, "Set2")
names(theme_colors) <- c("green", "orange", "blue", "pink")
get_theme_color_values <- function(names) theme_colors[names] %>% unname()

scale_color_strategy <- scale_color_manual(
  "strategy",
  labels = c("diachronic", "synchronic"),
  values = get_theme_color_values(c("blue", "green"))
)
scale_fill_strategy <- scale_fill_manual(
  "strategy",
  labels = c("diachronic", "synchronic"),
  values = get_theme_color_values(c("blue", "green"))
)

scale_color_team_label <- scale_color_manual(
  "skill diversity",
  labels = c("4 complementary", "3", "2", "1", "0 congruent"),
  values = brewer.pal(7, "BuGn")[7:3]
)
scale_alpha_team <- scale_alpha_discrete(
  "Skill diversity",
  labels = c("4 complementary", "3", "2", "1", "0 congruent"),
  range = c(1.0, 0.3)
)

scale_y_fitness_pct <- scale_y_continuous("Score", labels = scales::percent)
scale_x_strategy_rev <- scale_x_discrete("strategy", labels = c("synchronic", "diachronic"))


# Vision of 1 gives a visible range of 3 (from -1 to 1)
visible_range <- function(vision) -vision:vision

# Vision in two dimensions is a search area
calculate_player_search_area <- function(vision_x, vision_y) {
  expand.grid(x = visible_range(vision_x),
              y = visible_range(vision_y)) %>%
    mutate(vision_x = vision_x, vision_y = vision_y) %>%
    select(vision_x, vision_y, x, y)
}

calculate_team_search_area <- function(p1_vision_x, p1_vision_y,
                                       p2_vision_x, p2_vision_y) {
  expand.grid(
    p1_x = visible_range(p1_vision_x),
    p1_y = visible_range(p1_vision_y),
    p2_x = visible_range(p2_vision_x),
    p2_y = visible_range(p2_vision_y)
  ) %>% mutate(
    p1_vision_x = p1_vision_x,
    p1_vision_y = p1_vision_y,
    p2_vision_x = p2_vision_x,
    p2_vision_y = p2_vision_y,
    x = p1_x + p2_x,
    y = p1_y + p2_y
  ) %>%
    select(p1_vision_x, p1_vision_y, p2_vision_x, p2_vision_y, x, y)
}

# * ability-as-vision ----
data("differing_skills")
teams <- differing_skills %>%
  get_team_info() %>%
  gather(dimension, value, -c(team, player, team_id)) %>%
  recode_team() %>%
  select(-team)

legend_text <- data_frame(
  team_label_rev = factor(0, levels = 0:4),
  player = 1,
  value = c(0.1, 0.1),
  dimension = c("vision_x", "vision_y"),
  label = c("vision x", "vision y")
)

dodge_width <- position_dodge(width = 0.9)

tradeoffs_plot <- ggplot() +
  aes(x = player, y = value, group = dimension) +
  geom_bar(aes(fill = team_label_rev, alpha = dimension),
           stat = "identity", position = dodge_width) +
  facet_wrap("team_label_rev", nrow = 1) +
  geom_text(aes(label = label), data = legend_text, position = dodge_width,
            angle = 90, vjust = 0.75, hjust = 0) +
  scale_x_continuous("generation", breaks = c(1, 2)) +
  scale_y_continuous("vision", breaks = 1:10, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(values = c(0.9, 0.5)) +
  coord_cartesian(ylim = c(0, 9)) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

gg_two_dimensions <- (tradeoffs_plot %+% filter(teams, player == 1)) +
  scale_x_continuous("", labels = NULL) +
  theme(strip.text = element_blank()) +
  coord_cartesian(xlim = c(0, 2))

gg_differing_skills <- (tradeoffs_plot %+% teams)

# * search-areas ----

# Vision of 1 gives a visible range of 3 (from -1 to 1)
visible_range <- function(vision) vision * 2 + 1

# Vision in two dimensions is a search area
calculate_search_area <- function(vision_x, vision_y) {
  search_area <- visible_range(vision_x) * visible_range(vision_y)
  return(search_area)
}

search_areas <- data_frame(
  vision_x = 5:1,
  vision_y = 5:9,
  search_area = calculate_search_area(vision_x, vision_y)
)

diachronic_team_search_areas <- search_areas %>%
  mutate(
    strategy = "diachronic",
    team_label_rev = factor(0:4)
  )

synchronic_team_search_areas <- data_frame(
  vision_x = 10, vision_y = 10,
  search_area = calculate_search_area(vision_x, vision_y),
  strategy = "synchronic",
  agg_func = "sum"
)

search_areas_by_strategy <- bind_rows(diachronic_team_search_areas,
                                      synchronic_team_search_areas) %>%
  mutate(vision_x_range = visible_range(vision_x),
         vision_y_range = visible_range(vision_y))

gg_search_areas <- ggplot() +
  aes(x = 0, y = 0, width = vision_y_range, height = vision_x_range,
      fill = team_label_rev) +
  geom_tile() +
  scale_x_continuous("vision x", labels = NULL) +
  scale_y_continuous("vision y", labels = NULL) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  facet_wrap("team_label_rev", nrow = 1, scales = "fixed") +
  theme(strip.text = element_blank())

gg_search_rects <- (gg_search_areas %+% filter(search_areas_by_strategy, strategy == "diachronic"))


# * search-areas-by-strategy ----
search_areas_by_strategy %<>%
  arrange(vision_y) %>%
  mutate(
    visions = paste0("(", vision_x, ",", vision_y, ")"),
    visions_f = factor(visions, levels = visions)
  )

gg_search_areas_by_strategy <- ggplot(search_areas_by_strategy) +
  aes(visions_f, search_area, fill = visions_f) +
  geom_bar(stat = "identity") +
  annotate("text", x = 6, y = 10, label = "all synchronic teams",
           hjust = -0.1, angle = 90) +
  scale_x_discrete("(vision x, vision y)") +
  scale_y_continuous("search area") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  base_theme


exp1 <- list()



# * differing-skills ----
data("differing_skills")

differing_skills %<>%
  recode_fitness_as_pct() %>%
  recode_team() %>%
  extract_position() %>%
  filter(strategy == "diachronic")

recode_team_label <- function(frame) {
  team_label_levels <- c("e", "d", "c", "b", "a")
  poly_contr <- as.data.frame(contr.poly(team_label_levels))
  names(poly_contr) <- c("team_label.L", "team_label.Q", "team_label.C", "team_label.4")
  poly_contr$team_label <- factor(team_label_levels, levels = team_label_levels)
  
  if(missing(frame)) return(poly_contr)
  left_join(frame, poly_contr)
}

recode_time <- function(frame) {
  time_map <- data_frame(time = c(49, 99), time_c = c(-0.5, 0.5))
  if(missing(frame)) return(time_map)
  left_join(frame, time_map)
}

differing_skills %<>%
  recode_team_label() %>%
  recode_time()

gen1_mod <- lm(fitness_pct ~ team_label.L + team_label.Q + team_label.C + team_label.4,
               data = filter(differing_skills, time_c == -0.5))
exp1$gen1 <- report_lm_mod(gen1_mod, "team_label.L")

gen2_mod <- lm(fitness_pct ~ team_label.L + team_label.Q + team_label.C + team_label.4,
               data = filter(differing_skills, time_c == 0.5))
exp1$gen2 <- report_lm_mod(gen2_mod, "team_label.L")

crossover_mod <- lm(fitness_pct ~ (team_label.L + team_label.Q + team_label.C + team_label.4) * time_c,
                    data = filter(differing_skills, time_c %in% c(-0.5, 0.5)))
exp1$crossover <- report_lm_mod(crossover_mod, "team_label.L:time_c")

# label first and second gen
differing_skills$generation <- ifelse(differing_skills$time < 49, "gen1", "gen2")

gg_differing_skills_timeline <- ggplot(differing_skills) +
  aes(I(time+1), fitness_pct, alpha = team_label, color = generation, group = team_label) +
  geom_line(stat = "summary", fun.y = "mean", size = 1.2) +
  geom_vline(xintercept = 50, linetype = 2, color = "gray") +
  scale_x_continuous("Time") +
  scale_y_fitness_pct +
  scale_color_manual(values = get_theme_color_values(c("blue", "green"))) +
  scale_alpha_team +
  guides(color = "none",
         alpha = guide_legend(order = 2)) +
  theme(legend.position = c(0.78, 0.26), legend.key.size = unit(0.9, "lines"))

max_fitness <- differing_skills %>%
  group_by(sim_id, strategy, team_label) %>%
  summarize(fitness_pct = max(fitness_pct)) %>%
  peaks::recode_strategy()

dodge_width <- 0.9
team_dodge <- position_dodge(width = dodge_width)
sim_dodge <- position_jitterdodge(dodge.width = dodge_width, jitter.width = 0.4)

gg_differing_skills_final_fitness <- ggplot(max_fitness) +
  aes(x = strategy, fitness_pct, alpha = team_label) +
  scale_y_fitness_pct +
  # geom_point(aes(color = strategy), position = sim_dodge) +
  geom_bar(aes(fill = strategy),
           stat = "summary", fun.y = "mean", position = team_dodge) +
  scale_alpha_team +
  scale_fill_strategy +
  guides(fill = "none") +
  theme(panel.grid.major.x = element_blank())

gg_differing_skills_walk <- ggplot(filter(differing_skills, exp_id == 1)) +
  aes(pos_x, pos_y, group = sim_id, color = generation) +
  geom_path(alpha = 0.2) +
  annotate("point", x = 0, y = 0, shape = 4) +
  annotate("point", x = -139, y = -139, shape = 1) +
  coord_equal() +
  facet_wrap("team_label_rev", nrow = 1) +
  scale_x_continuous("", labels = NULL) +
  scale_y_continuous("", labels = NULL) +
  scale_color_manual(values = get_theme_color_values(c("blue", "green"))) +
  theme(legend.position = "none")

# ---- simple-hill
lattice::trellis.par.set("axis.line",list(col=NA, lty=1, lwd=1))
limits <- seq(-100, 100, by = 10)
z <- expand.grid(x = limits, y = limits) %>%
  mutate(score = -x^2 - y^2)
simple_hill <- lattice::wireframe(
  score ~ x * y, data = z,
  xlab = list(label="X", cex=1.2),
  ylab = list(label="Y", cex=1.2),
  zlab = list(label="Score", cex=1.2)
)
simple_hill

# ---- ability-as-vision
grid.arrange(
  gg_two_dimensions + ggtitle("A. Vision in two dimensions"),
  gg_search_rects + ggtitle("B. Search areas"),
  gg_differing_skills + ggtitle("C. Two person teams"),
  ncol = 1,
  heights = c(0.375, 0.25, 0.375)
)

# ---- differing-skills
gg_differing_skills_timeline
