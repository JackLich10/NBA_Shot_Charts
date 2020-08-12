# load libraries
library(tidyverse)
library(hexbin)
library(scales)

hex_bounds <- function(x, binwidth) {
  c(plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6)
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarise(hex_attempts = n(),
              hex_pct = mean(shot_made_numeric),
              hex_points_scored = sum(shot_made_numeric * shot_value),
              hex_points_per_shot = mean(shot_made_numeric * shot_value)) 
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, shot_zone_range, shot_zone_area) %>%
    summarise(attempts = n()) %>%
    ungroup() %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, shot_zone_range, shot_zone_area)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    data.frame(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

calculate_hexbins_from_shots = function(shots, league_averages = NULL, binwidths, min_radius_factor, fg_diff_limits, fg_freq_limits, fg_pct_limits, pps_limits) {
  if (nrow(shots) == 0) {
    return(list())
  }
  
  if (is.null(league_averages)) {
    shots <- shots %>%
      mutate(shot_value = case_when(
        Location %in% c("LT 45", "LT CNR", "RT 45", "RT CNR", "TOK 3") ~ 3,
        TRUE ~ 2),
        shot_zone_area = Location)
  }
  
  grouped_shots = group_by(shots, shot_zone_range, shot_zone_area)
  
  zone_stats = grouped_shots %>%
    summarise(zone_attempts = n(),
              zone_pct = mean(shot_made_numeric),
              zone_points_scored = sum(shot_made_numeric * shot_value),
              zone_points_per_shot = mean(shot_made_numeric * shot_value)) %>% 
    ungroup() %>% 
    mutate(zone_freq = zone_attempts/sum(zone_attempts))
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  join_keys = c("shot_zone_area", "shot_zone_range")
  
  if (is.null(league_averages)) {
    hex_data = hex_data %>%
      inner_join(zone_stats, by = join_keys)
  } else {
    league_zone_stats <- league_averages %>% 
      group_by(shot_zone_range, shot_zone_area) %>% 
      summarise(fgm = sum(fgm),
                fga = sum(fga)) %>% 
      ungroup() %>% 
      mutate(league_pct = fgm/fga,
             league_freq = fga/sum(fga))
    
    hex_data = hex_data %>%
      inner_join(zone_stats, by = join_keys) %>%
      inner_join(league_zone_stats, by = join_keys) %>% 
      mutate(bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
             bounded_freq_diff = pmin(pmax(zone_freq - league_freq, fg_freq_limits[1]), fg_freq_limits[2]))
  }
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))

  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_freq_limits = fg_freq_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
}
