# load libraries
library(tidyverse)
library(scales)
library(prismatic)
library(cowplot)

# source helper functions
source("nba_scrape.R")
source("court.R")
source("hex_charts.R")

generate_hex_chart <- function(hex_shots, court_theme = court_themes$dark, type = sym(c("bounded_fg_diff", "bounded_freq_diff", "bounded_fg_pct", "bounded_points_per_shot")), use_short_three = F, logo, title, subtitle) {
  if (type == "bounded_fg_diff") {
    limits <- c(-.15, .15)
    breaks <- seq(-.15, .15, .03)
    labels <- c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%")
    legend_title <- "FG% vs. League Average"
  } else if (type == "bounded_freq_diff") {
    limits <- c(-.075, .075)
    breaks <- seq(-.075, .075, .025)
    labels <- c("-7.5%", "-5%", "-2.5%", "0%", "2.5%", "+5%", "+7.5%")
    legend_title <- "FGA Frequency vs. League Average"
  } else if (type == "bounded_fg_pct") {
    limits <- c(0.2, 0.7)
    breaks <- seq(0.2, 0.7, 0.1)
    labels <- c("-20%", "30%", "40%", "50%", "60%", "70%")
    legend_title <- "FG%"
  } else if (type == "bounded_points_per_shot") {
    limits <- c(0.5, 1.5)
    breaks <- seq(0.5, 1.5, 0.2)
    labels <- seq(0.5, 1.5, 0.2)
    legend_title <- "Points Per Shot"
  }
  
  p <- plot_court(court_theme = court_theme, use_short_three = use_short_three) +
    geom_polygon(
      data = hex_shots,
      aes(x = adj_x, y = adj_y, group = hexbin_id, 
          fill = !!type, 
          color = after_scale(clr_darken(fill, .333))),
      size = .25) + 
    scale_x_continuous(limits = c(0, 50)) + 
    scale_y_continuous(limits = c(0, 40)) +
    scale_fill_distiller(direction = -1, palette = "RdBu", 
                         limits = limits,
                         breaks = breaks,
                         labels = labels,
                         legend_title) +
    guides(fill = guide_legend(
      label.position = 'bottom', 
      title.position = 'top', 
      keywidth = .45,
      keyheight = .15, 
      default.unit = "inch", 
      title.hjust = .5,
      title.vjust = -0.5,
      label.vjust = 3,
      nrow = 1))  +
    theme(text = element_text(size = 14), 
          legend.spacing.x = unit(0, 'cm'), 
          legend.title = element_text(size = 8, face = "italic"), 
          legend.text = element_text(size = rel(0.45)), 
          legend.margin = margin(-5, 0, -1, 0),
          legend.position = 'bottom',
          legend.box.margin = margin(-30, 0, 15, 0), 
          plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = -.5), 
          plot.caption = element_text(face = "italic", size = 8), 
          plot.margin = margin(0, -5, 0, -5, "cm")) +
    labs(title = title,
         subtitle = subtitle)
  
  if (length(logo) == 1) {
    ggdraw(p) + 
      cowplot::draw_image(logo, y = 0.3, scale = 0.2) +
      theme(plot.background = element_rect(fill = court_theme$court, color = NA))
  } else {
    ggdraw(p) + 
      cowplot::draw_image(logo[1], x = -0.1, y = 0.3, scale = 0.2) +
      cowplot::draw_image(logo[2], x = 0.1, y = 0.3, scale = 0.2) +
      theme(plot.background = element_rect(fill = court_theme$court, color = NA))
  }
}

brooklyn_shots <- get_data(player_id = 0, team_id = find_team_id_by_name("Nets"),
                           opponent_id = 0, seasons = "2019-20", season_type = "Regular Season",
                           binwidths = c(1.5, 1.5), min_radius_factor = 0.15,
                           bubble = F)

raptors_def <- get_data(player_id = 0, team_id = 0, opponent_id = find_team_id_by_name("Raptors"), 
                        seasons = "2019-20", season_type = "Regular Season",
                        binwidths = c(1.5, 1.5), min_radius_factor = 0.15,
                        bubble = F)

bucks_def <- get_data(player_id = 0, team_id = 0, opponent_id = find_team_id_by_name("Bucks"), 
                        seasons = "2019-20", season_type = "Regular Season",
                        binwidths = c(1.5, 1.5), min_radius_factor = 0.15,
                        bubble = F)

generate_hex_chart(hex_shots = brooklyn_shots, 
                   court_theme = court_themes$dark,
                   type = sym("bounded_fg_diff"), 
                   use_short_three = F,
                   logo = team_photo_url("Nets"),
                   title = "Where are the Bubble Nets shooting better?",
                   subtitle = "As compared to the pre-bubble Nets | Thru 6 bubble games | data from stats.nba.com")

generate_hex_chart(hex_shots = raptors_def,
                   type = sym("bounded_fg_diff"),
                   logo = c(team_photo_url("Nets"), team_photo_url("Raptors")),
                   title = "Where do opponents score on the Raptors?",
                   subtitle = "Compared to league averages | All Raptors defensive FGA allowed thru 8/11/20 | data from stats.nba.com")

generate_hex_chart(hex_shots = bucks_def,
                   type = sym("bounded_freq_diff"),
                   logo = team_photo_url("Nets"),
                   title = "What are the Bucks strengths defensively",
                   subtitle = "")

