###################################
# Plotting SES Trends by Generation
# First version: Nov 6, 2024
# Edited: Nov 6, 2024
###################################

CPS <- fread(file.path(dropbox_dir,"CPS_SES_VAR.csv.gz"))

############################################################
# Calculate Lorenz curves for each group
############################################################

lorenz_by_group <- CPS[
  (FirstGen == 1 | FirstGen_Asian == 1 | FirstGen_Arab == 1 | 
   SecondGen == 1 | SecondGen_Asian == 1 | SecondGen_Arab == 1 | 
   ThirdGen == 1 | ThirdGen_Asian == 1 | ThirdGen_Arab == 1 |
   FourthGen_White == 1 | BlackChild == 1) & age <= 17,
  {
    ses_lw_clean <- na.omit(ses_lw_std)
    l <- Lc(ses_lw_clean)
    data.table(p = l$p, L = l$L)
  },
  by = .(group = case_when(
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
  ))
]

############################################################
# Extract White curve and merge
############################################################

white_curve <- lorenz_by_group[group == "Fourth Gen+ White", .(p, L_white = L)]
merged_data <- merge(lorenz_by_group, white_curve, by = "p", all.x = TRUE)

############################################################
# Function to plot a single group vs White with zoom
############################################################

plot_single_group_vs_white <- function(data, group_name) {
  
  # Filter for just the chosen group + White
  plot_data <- data[group %in% c(group_name, "Fourth Gen+ White")]
  
  # Remove rows with NA
  plot_data <- plot_data[!is.na(L) & !is.na(L_white)]
  
  # Set a color map so White is black and the group has a distinct color
  color_map <- c(
    "Hispanic First Gen" = "#D55E00",
    "Asian First Gen" = "#0072B2",
    "Arab First Gen" = "#CC79A7",
    "Hispanic Second Gen" = "#D55E00",
    "Asian Second Gen" = "#0072B2",
    "Arab Second Gen" = "#CC79A7",
    "Hispanic Third Gen" = "#D55E00",
    "Asian Third Gen" = "#0072B2",
    "Arab Third Gen" = "#CC79A7",
    "Black" = "#009E73",
    "Fourth Gen+ White" = "#000000"
  )
  
  # Plot with zoom on the lower part of the curve
  gg <- ggplot(plot_data, aes(x = L_white, y = L, color = group)) +
    # For the chosen group
    geom_line(data = subset(plot_data, group == group_name), size = 1.2) +
    # Equality line
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    # White line (coincides with equality line if identical)
    geom_line(data = subset(plot_data, group == "Fourth Gen+ White"),
              aes(x = L_white, y = L_white), color = "black", size = 1.2) +
    scale_color_manual(values = color_map) +
    coord_cartesian(xlim = c(0, 0.5), ylim = c(0, 0.5)) +
    labs(
      title = paste(group_name, "vs White SES Distribution (Zoomed)"),
      subtitle = "If identical to White, line would lie on y=x",
      x = "Cumulative SES share (White)",
      y = "Cumulative SES share (Group)"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  # Save the plot
  file_name <- paste0(gsub(" ", "_", tolower(group_name)), "_vs_white_zoomed.png")
  ggsave(file_name, gg, width = 8, height = 6, dpi = 300)
  
  return(gg)
}
############################################################
# List of groups to plot individually vs White
############################################################

groups_to_plot <- c(
  "Hispanic First Gen", "Asian First Gen", "Arab First Gen",
  "Hispanic Second Gen", "Asian Second Gen", "Arab Second Gen",
  "Hispanic Third Gen", "Asian Third Gen", "Arab Third Gen", 
  "Black"
)

plot_single_group_vs_white(merged_data, groups_to_plot[1])
plot_single_group_vs_white(merged_data, groups_to_plot[2])
plot_single_group_vs_white(merged_data, groups_to_plot[3])
plot_single_group_vs_white(merged_data, groups_to_plot[4])
plot_single_group_vs_white(merged_data, groups_to_plot[5])
plot_single_group_vs_white(merged_data, groups_to_plot[6])
plot_single_group_vs_white(merged_data, groups_to_plot[7])
plot_single_group_vs_white(merged_data, groups_to_plot[8])
plot_single_group_vs_white(merged_data, groups_to_plot[9])
plot_single_group_vs_white(merged_data, groups_to_plot[10])


############################################################
# Calculate Lorenz curves for each group for the entire period
############################################################

lorenz_by_group <- CPS[
  (FirstGen == 1 | FirstGen_Asian == 1 | FirstGen_Arab == 1 | 
   SecondGen == 1 | SecondGen_Asian == 1 | SecondGen_Arab == 1 | 
   ThirdGen == 1 | ThirdGen_Asian == 1 | ThirdGen_Arab == 1 |
   FourthGen_White == 1 | BlackChild == 1) & age <= 17,
  {
    ses_lw_clean <- na.omit(ses_lw_std)
    l <- Lc(ses_lw_clean)
    data.table(p = l$p, L = l$L)
  },
  by = .(group = case_when(
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
  ))
]

############################################################
# Function to plot Lorenz curves using White as baseline
############################################################

create_lorenz_plot_white_baseline <- function(data, gen_groups, title_subtitle) {
  
  # Reference group: White
  white_data <- data[group == "Fourth Gen+ White"]
  
  # Minority groups to compare against White
  plot_groups <- c(gen_groups, "Fourth Gen+ White")
  plot_data <- data[group %in% plot_groups]
  
  # Plot
  ggplot() +
    # Plot White line first as baseline
    geom_line(data = white_data, aes(x = p, y = L),
              color = "black", linetype = "solid", size = 1.5) +
    # Plot other groups on top
    geom_line(data = plot_data[group != "Fourth Gen+ White"], 
              aes(x = p, y = L, color = group, linetype = group, size = group), alpha = 0.9) +
    
    # Scales and aesthetics
    scale_color_manual(name = "Group",
                       values = c(
                         "Hispanic First Gen" = "#D55E00",
                         "Asian First Gen" = "#0072B2",
                         "Arab First Gen" = "#CC79A7",
                         "Hispanic Second Gen" = "#D55E00",
                         "Asian Second Gen" = "#0072B2",
                         "Arab Second Gen" = "#CC79A7",
                         "Hispanic Third Gen" = "#D55E00",
                         "Asian Third Gen" = "#0072B2",
                         "Arab Third Gen" = "#CC79A7"
                       )) +
    scale_linetype_manual(name = "Group",
                          values = c(
                            "Hispanic First Gen" = "solid",
                            "Asian First Gen" = "solid",
                            "Arab First Gen" = "solid",
                            "Hispanic Second Gen" = "solid",
                            "Asian Second Gen" = "solid",
                            "Arab Second Gen" = "solid",
                            "Hispanic Third Gen" = "solid",
                            "Asian Third Gen" = "solid",
                            "Arab Third Gen" = "solid"
                          )) +
    scale_size_manual(name = "Group",
                      values = c(
                        "Hispanic First Gen" = 1,
                        "Asian First Gen" = 1,
                        "Arab First Gen" = 1,
                        "Hispanic Second Gen" = 1,
                        "Asian Second Gen" = 1,
                        "Arab Second Gen" = 1,
                        "Hispanic Third Gen" = 1,
                        "Asian Third Gen" = 1,
                        "Arab Third Gen" = 1
                      )) +
    # More axis breaks for visibility
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    
    labs(
      title = title_subtitle$title,
      subtitle = title_subtitle$subtitle,
      x = "Cumulative share of population",
      y = "Cumulative share of SES"
    ) +
    theme_bw() +
    # Remove or comment out if you don't have theme_customs
    # theme_customs() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.margin = margin()
    ) +
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 3)),
           linetype = guide_legend(nrow = 1, override.aes = list(size = 3)))
}

############################################################
# First Generation Lorenz Plot (White as baseline)
############################################################

first_gen_lorenz_plot_white <- function(data) {
  gen_groups <- c("Hispanic First Gen", "Asian First Gen", "Arab First Gen")
  title_subtitle <- list(
    title = "First Generation Lorenz Curves (Whole Period)",
    subtitle = "Compared to Fourth Gen+ White"
  )
  p <- create_lorenz_plot_white_baseline(data, gen_groups, title_subtitle)
  ggsave("first_gen_vs_white_lorenz_curve.png", p, width = 8, height = 6, dpi = 300)
  p
}

############################################################
# Second Generation Lorenz Plot (White as baseline)
############################################################

second_gen_lorenz_plot_white <- function(data) {
  gen_groups <- c("Hispanic Second Gen", "Asian Second Gen", "Arab Second Gen")
  title_subtitle <- list(
    title = "Second Generation Lorenz Curves (Whole Period)",
    subtitle = "Compared to Fourth Gen+ White"
  )
  p <- create_lorenz_plot_white_baseline(data, gen_groups, title_subtitle)
  ggsave("second_gen_vs_white_lorenz_curve.png", p, width = 8, height = 6, dpi = 300)
  p
}

############################################################
# Third Generation Lorenz Plot (White as baseline)
############################################################

third_gen_lorenz_plot_white <- function(data) {
  gen_groups <- c("Hispanic Third Gen", "Asian Third Gen", "Arab Third Gen")
  title_subtitle <- list(
    title = "Third Generation Lorenz Curves (Whole Period)",
    subtitle = "Compared to Fourth Gen+ White"
  )
  p <- create_lorenz_plot_white_baseline(data, gen_groups, title_subtitle)
  ggsave("third_gen_vs_white_lorenz_curve.png", p, width = 8, height = 6, dpi = 300)
  p
}

############################################################
# Example usage (uncomment and run if lorenz_by_group is ready)
############################################################
# first_gen_lorenz_plot_white(lorenz_by_group)
# second_gen_lorenz_plot_white(lorenz_by_group)
# third_gen_lorenz_plot_white(lorenz_by_group)
