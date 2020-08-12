# load libraries
library(tidyverse)
library(scales)
library(prismatic)
library(cowplot)

zion_photo <- player_photo_url(find_player_by_name("Zion Williamson") %>% 
                                 pull(person_id))

zion_photo_college <- "https://saintlyreport.com/wp-content/uploads/2019/05/Zion-Williamson-HS.png"
pelicans_logo <- "https://www.nba.com/pelicans/sites/pelicans/files/1415_nop_hdrlogo2.png?"
duke_logo <- "https://a.espncdn.com/i/teamlogos/ncaa/500/150.png"

zion_pro <- get_data(find_player_by_name("Zion Williamson") %>% 
                       pull(person_id),
                     team_id = 0, opponent_id = 0,
                     "2019-20", "Regular Season")

duke_pbp <- ncaahoopR::get_pbp("Duke", season = "2018-19")

zion_college <- duke_pbp %>% 
  filter(shooter == "Zion Williamson", free_throw == F) %>% 
  clean_game_data() %>% 
  calculate_hexbins_from_shots(., binwidths = c(1.5, 1.5), min_radius_factor = .2, fg_diff_limits = c(-0.15, 0.15), fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) %>% 
  .[[1]]

generate_hex_chart <- function(hex_shots, player, season, player_photo, team_logo, court_theme = "dark", use_short_three = F) {
  if (court_theme == "dark") {
    p <- plot_court(court_themes$dark, use_short_three)
  } else {
    p <- plot_court(court_themes$light, use_short_three)
  }
  p <- p +
    geom_polygon(
      data = hex_shots,
      aes(x = adj_x, y = adj_y, group = hexbin_id, 
          fill = bounded_points_per_shot, 
          color = after_scale(clr_darken(fill, .333))),
      size = .25) + 
    scale_x_continuous(limits = c(0, 50)) + 
    scale_y_continuous(limits = c(0, 40)) +
    scale_fill_distiller(direction = -1, palette = "RdBu", "Points Per Shot") +
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
    labs(title = player,
         subtitle = paste0(season, " Season"))
  
  ggdraw(p) + 
    cowplot::draw_image(player_photo, y = 0.25, scale = 0.2) +
    cowplot::draw_image(team_logo, x = 0.05, y = 0.3, scale = 0.05) +
    theme(plot.background = element_rect(fill = "#000004", color = NA))
} 

college_plot <- generate_hex_chart(zion_college, player = "Zion Williamson", season = "2018-19", 
                                   player_photo = zion_photo_college, team_logo = duke_logo,
                                   court_theme = "dark", use_short_three = T)

pro_plot <- generate_hex_chart(zion_pro, player = "Zion Williamson", season = "2019-20", 
                               player_photo = zion_photo, team_logo = pelicans_logo,
                               court_theme = "dark", use_short_three = F)

cowplot::plot_grid(college_plot, pro_plot, rel_widths = c(0.5, 0.5))

