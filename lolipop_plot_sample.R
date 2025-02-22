library(ggplot2)
library(tidyverse)

# Create lists of first and last names for baseball-style names
first_names <- c("Jake", "Max", "Justin", "Clayton", "Gerrit", "Zack", "Noah", 
                 "Chris", "Luis", "Blake", "Trevor", "Shane", "Shohei", "Marcus",
                 "Brandon", "Tyler", "Dylan", "Walker", "Logan", "Kyle")

last_names <- c("Verlander", "Scherzer", "deGrom", "Kershaw", "Cole", "Wheeler",
                "Syndergaard", "Sale", "Castillo", "Snell", "Bauer", "Bieber",
                "Ohtani", "Stroman", "Woodruff", "Glasnow", "Cease", "Buehler",
                "Webb", "Wright")

set.seed(123)  # for reproducibility
pitcher_names <- paste(
  sample(first_names, 100, replace = TRUE),
  sample(last_names, 100, replace = TRUE)
)

# Create the dataset with random pitcher names
pitcher <- data.frame(
  player = pitcher_names,
  strikes = sample(0:4, 100, replace = TRUE),
  balls = sample(0:4, 100, replace = TRUE),
  pitch_count = sample(0:4, 100, replace = TRUE),
  pitch_type = sample(0:4, 100, replace = TRUE),
  pitch_speed = sample(0:4, 100, replace = TRUE),
  pitch_spin = sample(0:4, 100, replace = TRUE),
  pitch_location = sample(0:4, 100, replace = TRUE),
  pitch_release = sample(0:4, 100, replace = TRUE),
  pitch_arm_slot = sample(0:4, 100, replace = TRUE),
  pitch_movement = sample(0:4, 100, replace = TRUE),
  pitch_break = sample(0:4, 100, replace = TRUE),
  pitch_control = sample(0:4, 100, replace = TRUE),
  pitch_command = sample(0:4, 100, replace = TRUE)
)

# Categories remain the same
categories <- data.frame(
  variable = names(pitcher)[-1],  # exclude player column
  category = c(
    "Count",        # strikes, balls, pitch_count
    "Count",
    "Count",
    "Count",         # pitch_type
    "Location",     # pitch_location, pitch_release
    "Location", 
    "Location",     # pitch_location, pitch_release
    "Location",
    "Mechanics",    # pitch_arm_slot, pitch_movement
    "Mechanics",
    "Command",      # pitch_break, pitch_control, pitch_command
    "Command",
    "Command"
  )
)


top_5_pitchers <- pitcher %>%
  pivot_longer(-player, 
               names_to = "variable",
               values_to = "value") %>%
  group_by(player) %>%
  summarize(mean_val = mean(value)) %>%
  arrange(desc(mean_val)) %>%
  head(4) %>%
  pull(player)

# # Then create the full dataset with just those pitchers
# pitcher_long <- pitcher %>%
#   pivot_longer(-player, 
#                names_to = "variable",
#                values_to = "value") %>%
#   left_join(categories, by = "variable") %>%
#   filter(player %in% top_5_pitchers) %>% 
#   mutate(variable = factor(variable, 
#                            levels = categories$variable[order(categories$category)]))
  

# First create the main dataset with plus scale
pitcher_long <- pitcher %>%
  pivot_longer(-player, 
               names_to = "variable",
               values_to = "value") %>%
  left_join(categories, by = "variable") %>%
  filter(player %in% top_5_pitchers) %>%
  group_by(variable) %>%
  mutate(var_mean = mean(value),
         plus_value = 100 * (value / var_mean)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, 
                           levels = categories$variable[order(categories$category)]))

# Calculate the break positions correctly
category_breaks <- categories %>%
  arrange(category) %>%
  group_by(category) %>%
  summarize(n = n()) %>%
  mutate(break_position = cumsum(n)) %>%
  slice(1:(n()-1))  # Remove the last break position

# Create the plot
ggplot(pitcher_long, aes(x = variable, y = plus_value, color = category)) +
  # Add category break lines
  geom_vline(xintercept = category_breaks$break_position + 0.5,
             color = "gray70", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = variable, yend = 100),
               size = 5, alpha = 0.4) +
  geom_point(size = 5, alpha = 0.95) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~player, scales = "free_y") +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    title = "Pitcher Metrics by Category (Plus Scale)",
    subtitle = "100 = League Average, Each Point = 1% Above/Below Average",
    x = "Variables",
    y = "Plus Value",
    color = "Category"
  )


ggplot(pitcher_long, aes(x = variable, y = plus_value, color = category)) +
  # Add category break lines
  geom_vline(xintercept = category_breaks$break_position + 0.5,
             color = "gray70", linetype = "dashed", size = 0.5) +
  geom_segment(aes(xend = variable, yend = 100),
               size = 5, alpha = 0.4) +
  geom_point(size = 5, alpha = 0.95) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  coord_flip() +
  theme_minimal() +
  facet_grid(category ~ player, scales = "free_y", space = "free_y", switch = "y") +
  theme(
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10),
    strip.text.y = element_text(angle = 0),
    strip.placement = "outside",
    # This makes the player names large and bold
    strip.text.x = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "Pitcher Metrics by Category (Plus Scale)",
    subtitle = "100 = League Average, Each Point = 1% Above/Below Average",
    x = "Variables",
    y = "Plus Value",
    color = "Category"
  )


## Lets create an individual plot 


# First create the base dataset with plus scale
pitcher_long <- pitcher %>%
  pivot_longer(-player, 
               names_to = "variable",
               values_to = "value") %>%
  left_join(categories, by = "variable") %>%
  filter(player %in% top_5_pitchers) %>%
  group_by(variable) %>%
  mutate(var_mean = mean(value),
         plus_value = 100 * (value / var_mean)) %>%
  ungroup() %>%
  mutate(variable = factor(variable, 
                           levels = categories$variable[order(categories$category)]))

# Calculate the break positions
category_breaks <- categories %>%
  arrange(category) %>%
  group_by(category) %>%
  summarize(n = n()) %>%
  mutate(break_position = cumsum(n)) %>%
  slice(1:(n()-1))

# Function to create a plot for a single player
# Function to create a plot for a single player with value labels
create_player_plot <- function(player_name, data) {
  player_data <- data %>% filter(player == player_name)
  
  ggplot(player_data, aes(x = variable, y = plus_value, color = category)) +
    geom_vline(xintercept = category_breaks$break_position + 0.5,
               color = "gray70", linetype = "dashed", size = 0.5) +
    geom_segment(aes(xend = variable, yend = 100),
                 size = 8, alpha = 0.4) +
    geom_point(size = 8, alpha = 0.95) +
    # Add text labels showing the actual values
    geom_text(aes(label = round(value, 1)), 
              vjust = 0.5,  # Adjust vertical position above the point
              size = 3,      # Adjust text size as needed
              color = "white") +  # Set labels color for better visibility
    geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
    #scale_color_brewer(palette = "Set1") +
    scale_color_viridis_d() +
    scale_y_continuous(breaks = seq(0, 200, 25)) +
    coord_flip() +
    theme_minimal() +
    facet_grid(category ~ ., scales = "free_y", space = "free_y", switch = "y") +
    theme(
      legend.position = "right",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(size = 10),
      strip.text.y = element_text(angle = 0),
      strip.placement = "outside",
      plot.title = element_text(size = 16, face = "bold"),
      # Add some extra margin at the top to accommodate labels
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
    ) +
    labs(
      title = player_name,
      subtitle = "100 = League Average, Each Point = 1% Above/Below Average\nActual values shown above each point",
      x = "Variables",
      y = "Plus Value",
      color = "Category"
    )
}

# Create list of plots
player_plots <- list()
for(player in top_5_pitchers) {
  player_plots[[player]] <- create_player_plot(player, pitcher_long)
}

for(player in names(player_plots)) {
  grid.arrange(grobs = list(player_plots[[player]]))
}
# # If you want to save the plots
# library(ggplot2)
# for(player in names(player_plots)) {
#   ggsave(
#     filename = paste0("player_plot_", gsub(" ", "_", player), ".png"),
#     plot = player_plots[[player]],
#     width = 10,
#     height = 8
#   )
# }

# Alternatively, if you want to display them all at once in a grid
library(gridExtra)
grid.arrange(grobs = player_plots, ncol = 2)  # Adjust ncol as needed




# =============================================================================


# =============================================================================

# Production-ready pitcher analysis visualization
# Version: 1.0.0
# Last updated: 2024-02-21

library(tidyverse)
library(viridis)
library(scales)
library(glue)

#' Create enhanced categories with meaningful order
#' @return data.frame with ordered categories
create_category_mapping <- function() {
  data.frame(
    variable = names(pitcher)[-1],
    category = factor(c(
      "Pre-Pitch",    # mechanics, arm slot
      "Pre-Pitch", 
      "Execution",    # speed, spin, movement
      "Execution",
      "Execution",
      "Location",     # release, location
      "Location",
      "Outcome",      # type, control, command
      "Outcome",
      "Outcome",
      "Game State",   # count, balls, strikes
      "Game State",
      "Game State"
    ), levels = c("Pre-Pitch", "Execution", "Location", "Outcome", "Game State"))
  )
}

#' Process raw pitcher data into analysis-ready format
#' @param pitcher_data Raw pitcher data
#' @param categories Category mapping
#' @param selected_pitchers Vector of pitcher names to include
#' @return Processed data frame with calculated metrics
prepare_pitcher_data <- function(pitcher_data, categories, selected_pitchers) {
  # Validate inputs
  if (!all(selected_pitchers %in% pitcher_data$player)) {
    stop("Some selected pitchers not found in dataset")
  }
  
  pitcher_data %>%
    pivot_longer(-player, 
                 names_to = "variable",
                 values_to = "value") %>%
    left_join(categories, by = "variable") %>%
    filter(player %in% selected_pitchers) %>%
    group_by(variable) %>%
    mutate(
      # Use robust statistics
      var_median = median(value),
      var_mad = mad(value),
      plus_value = 100 * (value / var_median),
      std_error = var_mad / sqrt(n()),
      z_score = scale(value)
    ) %>%
    ungroup() %>%
    # Remove invalid values
    filter(!is.na(plus_value), is.finite(plus_value)) %>%
    mutate(variable = factor(variable, 
                             levels = categories$variable[order(categories$category)]))
}

#' Calculate summary statistics for all players
#' @param data Processed pitcher data
#' @return Summary statistics for each player
calculate_player_summaries <- function(data) {
  data %>%
    group_by(player) %>%
    summarize(
      n_observations = n(),
      mean_performance = mean(plus_value, trim = 0.05),
      median_performance = median(plus_value),
      variability = mad(plus_value),
      top_metric = variable[which.max(plus_value)],
      top_value = max(plus_value),
      bottom_metric = variable[which.min(plus_value)],
      bottom_value = min(plus_value),
      .groups = "drop"
    )
}

#' Create enhanced visualization theme
#' @param base_size Base font size
#' @return ggplot theme object
create_enhanced_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Typography
      plot.title = element_text(
        size = rel(1.6), 
        face = "bold",
        margin = margin(b = base_size)
      ),
      plot.subtitle = element_text(
        size = rel(1.1),
        color = "gray30",
        margin = margin(b = base_size)
      ),
      plot.caption = element_text(
        size = rel(0.8),
        color = "gray30",
        margin = margin(t = base_size)
      ),
      # Axis formatting
      axis.title = element_text(size = rel(1), face = "bold"),
      axis.text = element_text(size = rel(0.9)),
      # Strip (facet) formatting
      strip.text.y = element_text(
        angle = 0,
        hjust = 0,
        size = rel(1),
        face = "bold"
      ),
      strip.placement = "outside",
      panel.spacing.y = unit(1.5, "lines"),
      # Simplified grid
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray95"),
      # Legend styling
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = base_size),
      legend.title = element_text(face = "bold")
    )
}

#' Create enhanced player performance plot
#' @param player_name Player to visualize
#' @param data Processed pitcher data
#' @param summaries Player summary statistics
#' @param save_plot Whether to save the plot
#' @param output_format Output format (png or svg)
#' @param width Custom width (optional)
#' @param height Custom height (optional)
#' @param base_size Base font size
#' @param color_palette Viridis palette option
#' @return ggplot object
create_enhanced_player_plot <- function(
    player_name,
    data,
    summaries,
    save_plot = FALSE,
    output_format = "png",
    width = NULL,
    height = NULL,
    base_size = 12,
    color_palette = "turbo"
) {
  # Validate inputs
  if (!player_name %in% unique(data$player)) {
    stop("Player not found in dataset")
  }
  
  # Filter data for player
  player_data <- data %>% 
    filter(player == player_name)
  
  # Get player summary
  player_summary <- summaries %>%
    filter(player == player_name)
  
  # Create plot
  p <- ggplot(
    player_data,
    aes(x = variable, y = plus_value, color = category, shape = category)
  ) +
    # Context layers
    annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = 90, ymax = 110,
      fill = "gray95", alpha = 0.5
    ) +
    # Add reference lines individually
    geom_hline(yintercept = 80, linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = 100, linetype = "solid", color = "gray40") +
    geom_hline(yintercept = 120, linetype = "dashed", color = "gray60") +
    # Performance metrics
    geom_linerange(
      aes(ymin = plus_value - std_error,
          ymax = plus_value + std_error),
      alpha = 0.3
    ) +
    geom_segment(
      aes(xend = variable, yend = 100),
      size = rel(5),
      alpha = 0.4
    ) +
    geom_point(
      size = rel(5),
      alpha = 0.95
    ) +
    # Scales
    scale_color_viridis_d(option = color_palette) +
    scale_shape_manual(values = c(15:19)) +
    scale_y_continuous(
      breaks = seq(0, 200, 20),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    # Coordinates and faceting
    coord_flip() +
    facet_grid(
      category ~ .,
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    # Theme and labels
    create_enhanced_theme(base_size) +
    labs(
      title = player_name,
      subtitle = glue(
        "Performance Metrics Relative to League Average\n",
        "Based on {player_summary$n_observations} observations"
      ),
      caption = glue(
        "Data as of: {format(Sys.Date(), '%B %d, %Y')}\n",
        "90-110% represents average range\n",
        "Metrics calculated using robust statistics (median/MAD)"
      ),
      x = NULL,
      y = "Performance Index",
      color = "Category",
      shape = "Category"
    )
  
  # Add performance annotations
  p <- p + annotate(
    "text",
    x = -Inf,
    y = max(player_data$plus_value) * 1.1,
    label = glue(
      "Strongest: {player_summary$top_metric} ({round(player_summary$top_value)}%)\n",
      "Weakest: {player_summary$bottom_metric} ({round(player_summary$bottom_value)}%)"
    ),
    hjust = 0,
    size = rel(2.5),
    color = "gray30"
  )
  
  # Save plot if requested
  if (save_plot) {
    # Calculate dimensions if not provided
    if (is.null(width)) {
      width <- max(nchar(as.character(player_data$variable))) * 10 + 400
    }
    if (is.null(height)) {
      height <- length(unique(player_data$variable)) * 30 + 200
    }
    
    filename <- glue(
      "player_analysis_{gsub(' ', '_', player_name)}.{output_format}"
    )
    
    ggsave(
      filename = filename,
      plot = p,
      width = width / 72,
      height = height / 72,
      dpi = 300,
      bg = "white"
    )
    
    
  }
  
  return(p)
}

# Main execution
main <- function() {
  # Create category mapping
  categories <- create_category_mapping()
  
  # Process data
  pitcher_long <- prepare_pitcher_data(pitcher, categories, top_5_pitchers)
  
  # Calculate summaries
  player_summaries <- calculate_player_summaries(pitcher_long)
  
  # Create plots and print each one
  player_plots <- list()
  for(player in top_5_pitchers) {
    plot <- create_enhanced_player_plot(
      player_name = player,
      data = pitcher_long,
      summaries = player_summaries,
      save_plot = TRUE,
      output_format = "svg",
      base_size = 14
    )
    player_plots[[player]] <- plot
    print(plot)  # This will display each plot individually
  }
  
  return(player_plots)
}

# Execute main function
plots <- main()
