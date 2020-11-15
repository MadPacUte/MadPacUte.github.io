

# this is trying out the fflr package

library(fflr)
library(ffplot)
library(tidyverse)
library(plotly)
library(tidytext)
library(flexdashboard)

theme_set(theme_light())

options(lid = 85055)

rosters <- 
  team_roster(week = ffl_week(-1)) %>%
  bind_rows()

my_roster <- rosters %>% filter(team == "Mad")

roster_score(my_roster)

# my_best <- best_roster(my_roster)

rosters %>% 
  bind_rows() %>% 
  group_by(team) %>% 
  filter(!status == "A") %>% 
  count(status, sort = T) 




# fflr::nfl_players %>% 
#   ggplot(aes(weight, height, color = pos)) +
#   geom_jitter() +
#   facet_wrap(~pos, scales = "free")


teams <- 
  league_teams()[, -4]


scores <- 
  match_scores() %>% 
  group_by(abbrev) %>% 
  mutate(score_cum_sum = cumsum(score),
         avg_score = mean(score),
         max_score = max(score),
         min_score = min(score),
         range_score = max_score - min_score) %>% 
  ungroup()

data <- scores

# league_standings(85055)

pal <- c(
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
  "#A6761D", "#666666", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF"
)

weekly_scores <- function(data) {
  data <- data[!is.na(data$score), ]
  data$week <- forcats::fct_rev(data$week)
  data <- dplyr::group_by(data, .data$abbrev)
  data <- dplyr::mutate(data, cum = cumsum(.data$score))
  ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = stats::reorder(.data$abbrev, .data$score),
      y = .data$score,
      fill = .data$week
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      nudge_y = -10,
      ggplot2::aes_string(
        label = "score",
        y = "cum"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      breaks = seq(0, 1e5, by = 25)
    ) +
    ggplot2::scale_fill_manual(
      values = pal,
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = "Fantasy Team Points by Week",
      x = "Team",
      y = "Points",
      fill = "Week"
    )
}

ggplotly(
  weekly_scores(scores)
)


ggplotly(
  scores %>% 
    mutate(abbrev = fct_reorder(abbrev, score),
           week = fct_rev(week)) %>% 
    ggplot(aes(score, abbrev, fill = week)) +
    geom_col() +
    scale_fill_manual(values = pal)
)


power_rankings <- 
  scores %>% 
  group_by(abbrev, week) %>% 
  count(power) %>% 
  ungroup() %>% 
  group_by(abbrev) %>% 
  mutate(avg = round(mean(power), 1),
         abbrev = fct_reorder(abbrev, avg)) %>% 
  ungroup() %>% 
  arrange(desc(avg))

power_rankings_chart <- 
    power_rankings %>% 
      ggplot(aes(week, power, color = abbrev, group = abbrev,
                 text = paste("Name: ", abbrev, "\n",
                              "Week: ", week, "\n",
                              "Power: ", power, "\n",
                              "Avg Power: ", avg, "\n",
                              sep = ""))) +
      geom_line(size = 2) +
      geom_hline(data = power_rankings, aes(yintercept = avg), linetype = "dashed") +
      facet_wrap(~fct_reorder(abbrev, -avg)) +
      theme(legend.position = "none") +
      labs(x = "",
           y = "")



power_rankings %>% 
  ggplot(aes(week, power, color = abbrev)) +
  geom_line(aes(group = abbrev), size = 1) +
  geom_point(size = 3)


tie_fighter_chart <-
  scores %>% 
  select(abbrev, avg_score:range_score) %>% 
  distinct(., .keep_all = T) %>% 
  mutate(abbrev = toupper(abbrev),
         avg_score = round(avg_score, 2),
         abbrev = fct_reorder(abbrev, avg_score))%>% 
  rename(name = abbrev) %>%
  ggplot(aes(avg_score, name,
             text = paste("Min Score: ", min_score, "\n",
                                                    "Avg Score: ", avg_score, "\n",
                                                    "Max Score: ", max_score, "\n",
                                                    sep = ""))) +
  geom_point(aes(avg_score), size = 2) +
  geom_errorbarh(aes(xmin = min_score, xmax = max_score)) +
  scale_x_continuous(breaks = seq(0, 220, by = 50)) +
  expand_limits(x = 0) +
  labs(title = "",
       y = "",
       x = "Average Score")


