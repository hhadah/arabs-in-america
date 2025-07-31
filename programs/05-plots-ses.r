###################################
# Plotting SES Trends by Generation
# First version: Nov 6, 2024
# Edited: Nov 6th. 2024
###################################

CPS <- fread(file.path(dropbox_dir,"CPS_SES_VAR.csv.gz"))

# Define 4-year intervals
CPS[, year_interval := floor((year - 1994) / 4) * 4 + 1994]

# Calculate mean SES by year and group for all generations
ses_by_group <- CPS[(FirstGen == 1 | FirstGen_Asian == 1 | FirstGen_Arab == 1 | 
                    SecondGen == 1 | SecondGen_Asian == 1 | SecondGen_Arab == 1 | 
                    ThirdGen == 1 | ThirdGen_Asian == 1 | ThirdGen_Arab == 1 |
                    FourthGen_White == 1 | BlackChild == 1) & age <= 17,
                   .(mean_ses_lw     = mean(ses_lw_std, na.rm = TRUE),
                     mean_ses_pca    = mean(SES_PCA, na.rm = TRUE),
                     mean_ses        = mean(SES, na.rm = TRUE),
                     mean_ses_faminc = mean(SES_faminc, na.rm = TRUE),
                     se_ses_lw          = sd(ses_lw_std, na.rm = TRUE) / sqrt(.N),
                     se_ses_pca          = sd(SES_PCA, na.rm = TRUE) / sqrt(.N),
                     se_ses          = sd(SES, na.rm = TRUE) / sqrt(.N),
                     se_ses_faminc          = sd(SES_faminc, na.rm = TRUE) / sqrt(.N),
                     n = .N),
                   by = .(year_interval,
                         group = case_when(
                           FirstGen == 1 ~ "Hispanic First Gen",
                           FirstGen_Asian == 1 ~ "Asian First Gen",
                           FirstGen_Arab == 1 ~ "Arab First Gen",
                           SecondGen == 1 ~ "Hispanic Second Gen",
                           SecondGen_Asian == 1 ~ "Asian Second Gen",
                           SecondGen_Arab == 1 ~ "Arab Second Gen",
                           ThirdGen == 1 ~ "Hispanic Third Gen",
                           ThirdGen_Asian == 1 ~ "Asian Third Gen",
                           ThirdGen_Arab == 1 ~ "Arab Third Gen",
                           FourthGen_White == 1 ~ "Fourth Gen+ White",
                           BlackChild == 1 ~ "Black"
                         ))]

# Filter ses_by_group to include only data from 2000 onwards
# ses_by_group <- ses_by_group[year_interval >= 2000 & year_interval <= 2023]
recessions <- data.frame(
    start = c(2001 - 0.25,  2007,        2020 - 0.25),
    end   = c(2001 + 0.25,  2009,        2020 + 0.25)
  )
# Create plot for First Generation only with direct labels
first_gen_plot <- function(data) {
  # Filter data for first generation and reference group
  plot_data <- data[group %in% c("Hispanic First Gen", "Asian First Gen", 
                                "Arab First Gen", "Fourth Gen+ White", "Black")]
  
  # Base plot
  p <- ggplot(plot_data, aes(x = year_interval, y = mean_ses_lw, 
                        color = group, 
                        shape = group,
                        linetype = group)) +
    # Add recession shading
    geom_rect(
      data = recessions,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey70", alpha = 0.3,
      inherit.aes = FALSE
    ) +
    # Add "Recessions" text label to the middle recession
    annotate("text", x = 2008, y = max(plot_data$mean_ses_lw) + 0.05, label = "Recessions", 
             hjust = 0.5, size = 8, fontface = "bold", color = "gray30") +
    # Main plot elements
    geom_ribbon(aes(ymin = mean_ses_lw - 1.96*se_ses_lw, 
                    ymax = mean_ses_lw + 1.96*se_ses_lw,
                    fill = group), 
                alpha = 0.1) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # Styling
    scale_color_manual(name = "Group",
      values = c(
        "Hispanic First Gen" = "#D55E00",      # Orange-red
        "Asian First Gen" = "#0072B2",         # Deep blue
        "Arab First Gen" = "#CC79A7",          # Pink
        "Fourth Gen+ White" = "#000000",       # Black
        "Black" = "#009E73"                    # Teal
    )) +
    scale_fill_manual(values = c(
      "Hispanic First Gen" = "#D55E00",
      "Asian First Gen" = "#0072B2",
      "Arab First Gen" = "#CC79A7",
      "Fourth Gen+ White" = "#000000",
      "Black" = "#009E73"
    )) +
    scale_shape_manual(name = "Group",
      values = c(
        "Hispanic First Gen" = 16,    # Circle
        "Asian First Gen" = 17,       # Triangle
        "Arab First Gen" = 15,        # Square
        "Fourth Gen+ White" = 18,     # Diamond
        "Black" = 19                  # Circle with plus
    )) +
    scale_linetype_manual(name = "Group",
      values = c(
        "Hispanic First Gen" = "solid",
        "Asian First Gen" = "solid",
        "Arab First Gen" = "solid",
        "Fourth Gen+ White" = "dashed",
        "Black" = "solid"
    )) +
    labs(title = "First Generation Immigrant SES Trends",
         subtitle = "Compared to Fourth Generation+ White",
         x = "Year",
         y = "Mean SES Score") +
    theme_customs() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  # Add direct labels at the end of the lines
  # Requires directlabels package
  # library(directlabels)
  # final_p <- direct.label(p, method = "last.points")
  
  # Alternative: use the ggrepel package for better label placement
  # library(ggrepel)
  # Get data points at the most recent year
  last_year <- max(plot_data$year_interval)
  label_data <- plot_data[plot_data$year_interval == last_year, ]
  
  final_p <- p + 
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = group, color = group),
      fontface = "bold",
      size = 14,
      nudge_x = 1,
      direction = "y",
      segment.size = 0.5,
      segment.color = "gray50"
    )
  
  return(final_p)
}

# Create and display the plot
first_gen_ses <- first_gen_plot(ses_by_group)
print(first_gen_ses)
ggsave(paste0(figures_wd,"/04-SES-firstgens.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/04-SES-firstgens.png"), width = 10, height = 6, units = "in")

# Calculate summary statistics for first generation
summary_stats_first <- ses_by_group[group %in% c("Hispanic First Gen", 
                                                "Asian First Gen", 
                                                "Arab First Gen", 
                                                "Fourth Gen+ White",
                                                "Black"),
                                  .(mean_ses_lw = round(mean(mean_ses_lw, na.rm = TRUE), 2),
                                    Min_SES = round(min(mean_ses_lw, na.rm = TRUE), 2),
                                    Max_SES = round(max(mean_ses_lw, na.rm = TRUE), 2),
                                    Avg_Sample_Size = round(mean(n, na.rm = TRUE), 0)),
                                  by = group]

# Calculate gaps relative to Fourth Gen+ White
gaps_first <- ses_by_group[group %in% c("Hispanic First Gen", 
                                       "Asian First Gen", 
                                       "Arab First Gen", 
                                       "Fourth Gen+ White",
                                       "Black")]

# Create wide format
gaps_wide <- dcast(gaps_first, year_interval ~ group, value.var = "mean_ses_lw")

# Calculate gaps
gaps_wide[, ':=' (
  Hispanic_Gap = `Hispanic First Gen` - `Fourth Gen+ White`,
  Asian_Gap = `Asian First Gen` - `Fourth Gen+ White`,
  Arab_Gap = `Arab First Gen` - `Fourth Gen+ White`,
  Black_Gap = `Black` - `Fourth Gen+ White`
)]

# Calculate mean gaps
mean_gaps <- gaps_wide[, .(
  Mean_Hispanic_Gap = mean(Hispanic_Gap, na.rm = TRUE),
  Mean_Asian_Gap = mean(Asian_Gap, na.rm = TRUE),
  Mean_Arab_Gap = mean(Arab_Gap, na.rm = TRUE),
  Mean_Black_Gap = mean(Black_Gap, na.rm = TRUE)
)]

# Print summary statistics
print("Summary Statistics:")
print(summary_stats_first)

print("\nMean SES Gaps relative to Fourth Gen+ White:")
print(round(mean_gaps, 2))

# Create plot for Second Generation only with fixed legend
# Create plot for Second Generation only with direct labels
second_gen_plot <- function(data) {
  # Filter data for second generation and reference group
  plot_data <- data[group %in% c("Hispanic Second Gen", "Asian Second Gen", 
                                "Arab Second Gen", "Fourth Gen+ White", "Black")]
  
  # Base plot
  p <- ggplot(plot_data, aes(x = year_interval, y = mean_ses_lw, 
                        color = group, 
                        shape = group,
                        linetype = group)) +
    # Add recession shading
    geom_rect(
      data = recessions,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey70", alpha = 0.3,
      inherit.aes = FALSE
    ) +
    # Add "Recessions" text label to the middle recession
    annotate("text", x = 2008, y = max(plot_data$mean_ses_lw) + 0.05, label = "Recessions", 
             hjust = 0.5, size = 8, fontface = "bold", color = "gray30") +
    # Main plot elements
    geom_ribbon(aes(ymin = mean_ses_lw - 1.96*se_ses_lw, 
                    ymax = mean_ses_lw + 1.96*se_ses_lw,
                    fill = group), 
                alpha = 0.1) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # Styling
    scale_color_manual(name = "Group",
      values = c(
        "Hispanic Second Gen" = "#D55E00",     # Orange-red
        "Asian Second Gen" = "#0072B2",        # Deep blue
        "Arab Second Gen" = "#CC79A7",         # Pink
        "Fourth Gen+ White" = "#000000",       # Black
        "Black" = "#009E73"                    # Teal
    )) +
    scale_fill_manual(values = c(
      "Hispanic Second Gen" = "#D55E00",
      "Asian Second Gen" = "#0072B2",
      "Arab Second Gen" = "#CC79A7",
      "Fourth Gen+ White" = "#000000",
      "Black" = "#009E73"
    )) +
    scale_shape_manual(name = "Group",
      values = c(
        "Hispanic Second Gen" = 16,    # Circle
        "Asian Second Gen" = 17,       # Triangle
        "Arab Second Gen" = 15,        # Square
        "Fourth Gen+ White" = 18,      # Diamond
        "Black" = 19                   # Circle with plus
    )) +
    scale_linetype_manual(name = "Group",
      values = c(
        "Hispanic Second Gen" = "solid",
        "Asian Second Gen" = "solid",
        "Arab Second Gen" = "solid",
        "Fourth Gen+ White" = "dashed",
        "Black" = "solid"
    )) +
    labs(title = "Second Generation Immigrant SES Trends",
         subtitle = "Compared to Fourth Generation+ White",
         x = "Year",
         y = "Mean SES Score") +
    theme_customs() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  # Get data points at the most recent year for labels
  last_year <- max(plot_data$year_interval)
  label_data <- plot_data[plot_data$year_interval == last_year, ]
  
  final_p <- p + 
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = group, color = group),
      fontface = "bold",
      size = 14,
      nudge_x = 1,
      direction = "y",
      segment.size = 0.5,
      segment.color = "gray50"
    )
  
  return(final_p)
}

# Create and display the plot
second_gen_ses <- second_gen_plot(ses_by_group)
print(second_gen_ses)
ggsave(paste0(figures_wd,"/05-SES-secondgens.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/05-SES-secondgens.png"), width = 10, height = 6, units = "in")

# Calculate summary statistics for second generation
summary_stats_second <- ses_by_group[group %in% c("Hispanic Second Gen", 
                                                 "Asian Second Gen", 
                                                 "Arab Second Gen", 
                                                 "Fourth Gen+ White",
                                                 "Black"),
                                   .(mean_ses_lw = round(mean(mean_ses_lw, na.rm = TRUE), 2),
                                     Min_SES = round(min(mean_ses_lw, na.rm = TRUE), 2),
                                     Max_SES = round(max(mean_ses_lw, na.rm = TRUE), 2),
                                     Avg_Sample_Size = round(mean(n, na.rm = TRUE), 0)),
                                   by = group]

# Calculate gaps for second generation
gaps_second <- ses_by_group[group %in% c("Hispanic Second Gen", 
                                        "Asian Second Gen", 
                                        "Arab Second Gen", 
                                        "Fourth Gen+ White",
                                        "Black")]

# Create wide format
gaps_wide_second <- dcast(gaps_second, year_interval ~ group, value.var = "mean_ses_lw")

# Calculate gaps
gaps_wide_second[, ':=' (
  Hispanic_Gap = `Hispanic Second Gen` - `Fourth Gen+ White`,
  Asian_Gap = `Asian Second Gen` - `Fourth Gen+ White`,
  Arab_Gap = `Arab Second Gen` - `Fourth Gen+ White`,
  Black_Gap = `Black` - `Fourth Gen+ White`
)]

# Calculate mean gaps
mean_gaps_second <- gaps_wide_second[, .(
  Mean_Hispanic_Gap = mean(Hispanic_Gap, na.rm = TRUE),
  Mean_Asian_Gap = mean(Asian_Gap, na.rm = TRUE),
  Mean_Arab_Gap = mean(Arab_Gap, na.rm = TRUE),
  Mean_Black_Gap = mean(Black_Gap, na.rm = TRUE)
)]

# Print summary statistics
print("Summary Statistics:")
print(summary_stats_second)

print("\nMean SES Gaps relative to Fourth Gen+ White:")
print(round(mean_gaps_second, 2))

# Create plot for Third Generation only with fixed legend
third_gen_plot <- function(data) {
  # Filter data for third generation and reference group
  plot_data <- data[group %in% c("Hispanic Third Gen", "Asian Third Gen", 
                               "Arab Third Gen", "Fourth Gen+ White", "Black")]
  
  # Base plot
  p <- ggplot(plot_data, aes(x = year_interval, y = mean_ses_lw, 
                        color = group, 
                        shape = group,
                        linetype = group)) +
    # Add recession shading
    geom_rect(
      data = recessions,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey70", alpha = 0.3,
      inherit.aes = FALSE
    ) +
    # Add "Recessions" text label to the middle recession
    annotate("text", x = 2008, y = max(plot_data$mean_ses_lw) + 0.05, label = "Recessions", 
             hjust = 0.5, size = 8, fontface = "bold", color = "gray30") +
    # Main plot elements
    geom_ribbon(aes(ymin = mean_ses_lw - 1.96*se_ses_lw, 
                    ymax = mean_ses_lw + 1.96*se_ses_lw,
                    fill = group), 
                alpha = 0.1) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # Styling
    scale_color_manual(name = "Group",
      values = c(
        "Hispanic Third Gen" = "#D55E00",     # Orange-red
        "Asian Third Gen" = "#0072B2",        # Deep blue
        "Arab Third Gen" = "#CC79A7",         # Pink
        "Fourth Gen+ White" = "#000000",      # Black
        "Black" = "#009E73"                   # Teal
    )) +
    scale_fill_manual(values = c(
      "Hispanic Third Gen" = "#D55E00",
      "Asian Third Gen" = "#0072B2",
      "Arab Third Gen" = "#CC79A7",
      "Fourth Gen+ White" = "#000000",
      "Black" = "#009E73"
    )) +
    scale_shape_manual(name = "Group",
      values = c(
        "Hispanic Third Gen" = 16,    # Circle
        "Asian Third Gen" = 17,       # Triangle
        "Arab Third Gen" = 15,        # Square
        "Fourth Gen+ White" = 18,     # Diamond
        "Black" = 19                  # Circle with plus
    )) +
    scale_linetype_manual(name = "Group",
      values = c(
        "Hispanic Third Gen" = "solid",
        "Asian Third Gen" = "solid",
        "Arab Third Gen" = "solid",
        "Fourth Gen+ White" = "dashed",
        "Black" = "solid"
    )) +
    labs(title = "Third Generation Immigrant SES Trends",
         subtitle = "Compared to Fourth Generation+ White",
         x = "Year",
         y = "Mean SES Score") +
    theme_customs() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  # Get data points at the most recent year for labels
  last_year <- max(plot_data$year_interval)
  label_data <- plot_data[plot_data$year_interval == last_year, ]
  
  final_p <- p + 
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = group, color = group),
      fontface = "bold",
      size = 14,
      nudge_x = 1,
      direction = "y",
      segment.size = 0.5,
      segment.color = "gray50"
    )
  
  return(final_p)
}


# Create and display the plot
third_gen_ses <- third_gen_plot(ses_by_group)
print(third_gen_ses)
ggsave(paste0(figures_wd,"/06-SES-thirdgens.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/06-SES-thirdgens.png"), width = 10, height = 6, units = "in")

# Calculate summary statistics for third generation
summary_stats_third <- ses_by_group[group %in% c("Hispanic Third Gen", 
                                                "Asian Third Gen", 
                                                "Arab Third Gen", 
                                                "Fourth Gen+ White",
                                                "Black"),
                                  .(mean_ses_lw = round(mean(mean_ses_lw, na.rm = TRUE), 2),
                                    Min_SES = round(min(mean_ses_lw, na.rm = TRUE), 2),
                                    Max_SES = round(max(mean_ses_lw, na.rm = TRUE), 2),
                                    Avg_Sample_Size = round(mean(n, na.rm = TRUE), 0)),
                                  by = group]

# Calculate gaps for third generation
gaps_third <- ses_by_group[group %in% c("Hispanic Third Gen", 
                                       "Asian Third Gen", 
                                       "Arab Third Gen", 
                                       "Fourth Gen+ White",
                                       "Black")]

# Create wide format
gaps_wide_third <- dcast(gaps_third, year_interval ~ group, value.var = "mean_ses_lw")

# Calculate gaps
gaps_wide_third[, ':=' (
  Hispanic_Gap = `Hispanic Third Gen` - `Fourth Gen+ White`,
  Asian_Gap = `Asian Third Gen` - `Fourth Gen+ White`,
  Arab_Gap = `Arab Third Gen` - `Fourth Gen+ White`,
  Black_Gap = `Black` - `Fourth Gen+ White`
)]

# Calculate mean gaps
mean_gaps_third <- gaps_wide_third[, .(
  Mean_Hispanic_Gap = mean(Hispanic_Gap, na.rm = TRUE),
  Mean_Asian_Gap = mean(Asian_Gap, na.rm = TRUE),
  Mean_Arab_Gap = mean(Arab_Gap, na.rm = TRUE),
  Mean_Black_Gap = mean(Black_Gap, na.rm = TRUE)
)]

# Print summary statistics
print("Summary Statistics:")
print(summary_stats_third)

print("\nMean SES Gaps relative to Fourth Gen+ White:")
print(round(mean_gaps_third, 2))


# Calculate mean SES by year and racial/ethnic group using direct classifications
ses_by_demo <- CPS[, 
  .(mean_ses_lw     = mean(ses_lw_std, na.rm = TRUE),
    mean_ses_pca    = mean(SES_PCA, na.rm = TRUE),
    mean_ses        = mean(SES, na.rm = TRUE),
    mean_ses_faminc = mean(SES_faminc, na.rm = TRUE),
    se_ses = sd(ses_lw_std, na.rm = TRUE) / sqrt(.N),
    se_ses_faminc = sd(SES_faminc, na.rm = TRUE) / sqrt(.N),
    n = .N),
  by = .(year_interval,
         group = case_when(
           Hispanic == 1 ~ "Hispanic",
           Black == 1 ~ "Black",
           White == 1 ~ "White",
           Arab == 1 ~ "Arab",
           Asian == 1 ~ "Asian",
           TRUE ~ NA_character_
         ))]

# Remove NA groups
ses_by_demo <- ses_by_demo[!is.na(group)]

# Create plot function with direct labels
demographic_ses_plot <- function(data) {
  # Base plot
  p <- ggplot(data, aes(x = year_interval, y = mean_ses_faminc, 
                   color = group, 
                   shape = group,
                   linetype = group)) +
    # Add recession shading
    geom_rect(
      data = recessions,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey70", alpha = 0.3,
      inherit.aes = FALSE
    ) +
    # Add "Recessions" text label to the middle recession
    annotate("text", x = 2008, y = 0.4, label = "Recessions", 
             hjust = 0.5, size = 8, fontface = "bold", color = "gray30") +
    # Main plot elements
    geom_ribbon(aes(ymin = mean_ses_faminc - 1.96*se_ses_faminc, 
                    ymax = mean_ses_faminc + 1.96*se_ses_faminc,
                    fill = group), 
                alpha = 0.1) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    # Styling
    scale_color_manual(name = "Race/Ethnicity",
      values = c(
        "Hispanic" = "#D55E00",    # Orange-red
        "Arab" = "#CC79A7",        # Pink
        "White" = "#000000",       # Black
        "Black" = "#009E73",        # Teal
        "Asian" = "#0072B2"        # Deep blue
    )) +
    scale_fill_manual(values = c(
        "Hispanic" = "#D55E00",
        "Arab" = "#CC79A7",
        "White" = "#000000",
        "Black" = "#009E73",
        "Asian" = "#0072B2"
    )) +
    scale_shape_manual(name = "Race/Ethnicity",
      values = c(
        "Hispanic" = 16,    # Circle
        "Arab" = 17,        # Triangle
        "White" = 18,       # Diamond
        "Black" = 15,        # Square
        "Asian" = 19        # Circle with plus
    )) +
    scale_linetype_manual(name = "Race/Ethnicity",
      values = c(
        "Hispanic" = "solid",
        "Arab" = "solid",
        "White" = "dashed",
        "Black" = "solid",
        "Asian" = "solid"
    )) +
    labs(title = "Socioeconomic Status Trends by Race/Ethnicity",
         x = "Year",
         y = "Mean SES Score") +
    theme_customs() +
    theme(legend.position = "none",  # Remove legend since we'll use direct labels
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  # Get data points at the most recent year for labels
  last_year <- max(data$year_interval)
  label_data <- data[data$year_interval == last_year, ]
  
  # Add direct labels with ggrepel
  final_p <- p + 
    ggrepel::geom_text_repel(
      data = label_data,
      aes(label = group, color = group),
      fontface = "bold",
      size = 14,
      nudge_x = 1,
      direction = "y",
      segment.size = 0.5,
      segment.color = "gray50",
      box.padding = 0.5
    )
  
  return(final_p)
}

# Create and display the plot
demo_plot <- demographic_ses_plot(ses_by_demo)
print(demo_plot)

ggsave(paste0(figures_wd,"/07-SES-selfreport-race-ethn.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/07-SES-selfreport-race-ethn.png"), width = 10, height = 6, units = "in")
