

# this is trying out the fflr package

library(fflr)
library(ffplot)
library(tidyverse)
library(plotly)
library(tidytext)
library(flexdashboard)
library(ggalt)

theme_set(theme_light())

options(lid = 85055)


abbrev_to_name <-
  tibble::tribble(
            ~full_name, ~abbrev,
        "Casey Walter",    "KC",
        "Cesar Vieira",    "CV",
       "Damien Thomas",    "D$ ",
          "Dan Fillat",    "LM",
       "Dominic Krohn",   "DOM",
        "Edwin Castro",  "EDWN",
      "Jordan Proctor",   "JJP",
        "Jose Zavalza",   "JOE",
          "Marc Krohn",  "MELO",
    "Michael Gonzalez",  "MIKE",
      "Randall Madsen",   "MAD",
         "Tate Graham",   "MIP"
    )






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
  ungroup() %>% 
  mutate(abbrev = toupper(abbrev)) %>% 
  left_join(abbrev_to_name) %>% 
  select(-abbrev) %>%
  rename(abbrev = full_name)

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
  arrange(desc(avg)) %>% 
  # mutate(abbrev = toupper(abbrev)) %>%
  left_join(abbrev_to_name, by = c("abbrev" = "full_name")) %>% 
  select(-abbrev.y) 
  # rename(abbrev = full_name) 

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



weekly_historical <- 
  fflr::week_scores(old = T) %>% 
  bind_rows(week_scores(old = F)) %>%
  select(-home) %>%
  rename(name = abbrev) %>%
  relocate(name) %>%
  janitor::clean_names(., "upper_camel") %>%
  arrange(Year, Week)


yearly_historical <- 
  fflr::score_summary(old = T) %>% 
  bind_rows(score_summary(old = F)) %>% 
  # filter(year == 2019) %>% 
  select(name = abbrev, year, final, wins, losses) %>% 
  filter(!year == 2020) %>%
  mutate(name = toupper(name)) %>% 
  group_by(name) %>% 
  mutate(win_total = sum(wins),
         loss_total = sum(losses),
         win_percentage = round(win_total / (win_total + loss_total), 2)) %>% 
  ungroup() %>% 
  arrange(final) %>%
  janitor::clean_names(., "upper_camel") %>%
  arrange(Year, Final)





yearly_historical %>% 
  select(Name, WinTotal:WinPercentage) %>% 
  distinct(Name, .keep_all = T) %>% 
  left_join(power_rankings %>% 
               select(Name = abbrev) %>% 
               distinct())

power_rankings %>% 
  select(Name = abbrev) %>% 
  distinct() %>% 
  left_join(yearly_historical %>% 
              select(Name, WinTotal:WinPercentage) %>% 
              distinct(Name, .keep_all = T))








standings <-
fflr::score_summary() %>% 
  arrange(current) %>% 
  select(name = abbrev, draft:seed, back:streak) %>% 
  mutate(name = toupper(as.character(name))) %>% 
  left_join(abbrev_to_name, by = c("name" = "abbrev")) %>% 
  select(-name) %>%
  rename(name = full_name) %>% 
  mutate(name = toupper(as.factor(name)),
         name = fct_reorder(name, -draft),
         color = case_when(current > draft ~ "43b049",
                           current < draft ~ "d02682",
                           TRUE ~ "80a2ad")) %>% 
  mutate(pos_o_neg = as.character(if_else(draft - current >= 0, 1, 0)),
         draft = draft * -1,
         current = current * -1)


dumbbell_chart <-
  standings %>% 
  arrange(desc(draft)) %>% 
    # mutate(draft = draft * -1,
    #        current = current * -1)  %>% 
  ggplot(aes(x = current, xend = draft, y = name, group = name)) +
    ggalt::geom_dumbbell(color="#a3c4dc", 
                  colour_xend = "#d02682",
                 size=4) +
    # scale_color_manual(values = c("grey" = "grey", "red" = "red", "green" = "green")) +
    labs(x = "",
         y = "")

  
slope_chart <-
standings %>% 
  ggplot() + 
    geom_segment(aes(x=1, xend=2, y=draft, yend=current, col=pos_o_neg), size=2, show.legend=F) +
    geom_vline(xintercept=1, linetype="dashed", size=.1) + 
    geom_vline(xintercept=2, linetype="dashed", size=.1) +
    scale_color_manual(values = c("1"="#00ba38", "0"="#f8766d")) + # color of lines
    labs(x = "", 
         y = "",
         title = "Draft Day vs. Current Standings") + # Axis labels
    ylim(-12, 0) +
    xlim(0.7, 2.3) +
    # geom_text(label = paste(standings$name, abs(standings$draft), sep = ", "), y = standings$draft, x=rep(1, NROW(standings)), hjust=-0.1, size=3.5) + #left
    geom_text(label = paste(standings$name, abs(standings$draft), sep = ", "), y = standings$draft, x=rep(1, NROW(standings)), hjust=1.1, size=1.9) + #left
    # geom_text(label = paste(standings$name, abs(standings$current), sep = ", "), y = standings$current, x=rep(2, NROW(standings)), hjust=1.1, size=3.5) + #right
    geom_text(label = paste(standings$name, abs(standings$current), sep = ", "), y = standings$current, x=rep(2, NROW(standings)), hjust=-0.1, size=1.9) + #right
    geom_text(label="Draft", x=1, y=0.3*(max(standings$draft, standings$current)), hjust=1.1, size=4) + #draft label
    geom_text(label="Current", x=2, y=0.3*(max(standings$draft, standings$current)), hjust=-0.1, size=4) + #current label 
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))

  
    
  