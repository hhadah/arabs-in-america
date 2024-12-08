###################################
# Plotting SES Trends by Generation
# First version: Nov 6, 2024
# Edited: Nov 6, 2024
###################################

CPS <- fread(file.path(dropbox_dir,"CPS_SES_VAR.csv.gz"))


############################################################
# Calculate Lorenz curves for each group for the entire period
############################################################

lorenz_by_group <- CPS[
  (FirstGen == 1 | FirstGen_Asian == 1 | FirstGen_Arab == 1 | 
   SecondGen == 1 | SecondGen_Asian == 1 | SecondGen_Arab == 1 | 
   ThirdGen == 1 | ThirdGen_Asian == 1 | ThirdGen_Arab == 1 |
   FourthGen_White == 1 | BlackChild == 1) & age <= 17,
  {
    ses_lw_clean <- na.omit(ses_lw)
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
# Define a function to create a Lorenz curve plot for a given set of groups
############################################################

create_lorenz_plot <- function(data, gen_groups, title_subtitle) {
  # Combine input generation groups with the reference groups
  plot_groups <- c(gen_groups, "Fourth Gen+ White", "Black")
  
  plot_data <- data[group %in% plot_groups]
  
  # Emphasize "Fourth Gen+ White" by giving it a thicker line
  # We'll map size to group and define sizes manually.
  ggplot(plot_data, aes(x = p, y = L, color = group, shape = group, linetype = group, size = group, alpha = group)) +
    geom_line() +
    # Perfect equality line
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    
    # Distinguish groups by color, shape, linetype
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
                         "Arab Third Gen" = "#CC79A7",
                         "Fourth Gen+ White" = "#000000",
                         "Black" = "#009E73"
                       )) +
    scale_shape_manual(name = "Group",
                       values = c(
                         "Hispanic First Gen" = 16,
                         "Asian First Gen" = 17,
                         "Arab First Gen" = 15,
                         "Hispanic Second Gen" = 16,
                         "Asian Second Gen" = 17,
                         "Arab Second Gen" = 15,
                         "Hispanic Third Gen" = 16,
                         "Asian Third Gen" = 17,
                         "Arab Third Gen" = 15,
                         "Fourth Gen+ White" = 18,
                         "Black" = 19
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
                            "Arab Third Gen" = "solid",
                            "Fourth Gen+ White" = "dashed",
                            "Black" = "solid"
                          )) +
    # Make Fourth Gen+ White line thicker and others slightly thinner
    scale_size_manual(name = "Group",
                      values = c(
                        "Hispanic First Gen" = 0.8,
                        "Asian First Gen" = 0.8,
                        "Arab First Gen" = 0.8,
                        "Hispanic Second Gen" = 0.8,
                        "Asian Second Gen" = 0.8,
                        "Arab Second Gen" = 0.8,
                        "Hispanic Third Gen" = 0.8,
                        "Asian Third Gen" = 0.8,
                        "Arab Third Gen" = 0.8,
                        "Fourth Gen+ White" = 1.5,
                        "Black" = 0.8
                      )) +
    # Slight transparency to see overlapping lines better
    scale_alpha_manual(name = "Group",
                       values = c(
                         "Hispanic First Gen" = 1,
                         "Asian First Gen" = 1,
                         "Arab First Gen" = 1,
                         "Hispanic Second Gen" = 1,
                         "Asian Second Gen" = 1,
                         "Arab Second Gen" = 1,
                         "Hispanic Third Gen" = 1,
                         "Asian Third Gen" = 1,
                         "Arab Third Gen" = 1,
                         "Fourth Gen+ White" = 1,
                         "Black" = 0.9
                       )) +
    # More breaks on axes to see subtle differences
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    
    labs(
      title = title_subtitle$title,
      subtitle = title_subtitle$subtitle,
      x = "Cumulative share of population",
      y = "Cumulative share of SES"
    ) +
    theme_bw() +
    # Remove this line if you don't have a custom theme
    theme_customs() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      legend.margin = margin()
    ) +
    guides(
      color = guide_legend(nrow = 1, override.aes = list(size = 3)),
      shape = guide_legend(nrow = 1, override.aes = list(size = 3)),
      linetype = "none",
      alpha = "none",
      size = "none"
    )
}

############################################################
# First Generation Lorenz Plot
############################################################

first_gen_lorenz_plot <- function(data) {
  gen_groups <- c("Hispanic First Gen", "Asian First Gen", "Arab First Gen")
  title_subtitle <- list(
    title = "First Generation Lorenz Curves (Whole Period)",
    subtitle = "Compared to Fourth Gen+ White and Black"
  )
  p <- create_lorenz_plot(data, gen_groups, title_subtitle)
  # Save the plot
  ggsave(paste0(figures_wd,"/16-first_gen_lorenz_curve.png"), width = 10, height = 6, units = "in")
  ggsave(paste0(thesis_plots,"/16-first_gen_lorenz_curve.png"), width = 10, height = 6, units = "in")
  p
}

############################################################
# Second Generation Lorenz Plot
############################################################

second_gen_lorenz_plot <- function(data) {
  gen_groups <- c("Hispanic Second Gen", "Asian Second Gen", "Arab Second Gen")
  title_subtitle <- list(
    title = "Second Generation Lorenz Curves (Whole Period)",
    subtitle = "Compared to Fourth Gen+ White and Black"
  )
  p <- create_lorenz_plot(data, gen_groups, title_subtitle)
  # Save the plot
  ggsave(paste0(figures_wd,"/17-second_gen_lorenz_curve.png"), width = 10, height = 6, units = "in")
  ggsave(paste0(thesis_plots,"/17-second_gen_lorenz_curve.png"), width = 10, height = 6, units = "in")
  p
}

############################################################
# Third Generation Lorenz Plot
############################################################

third_gen_lorenz_plot <- function(data) {
  gen_groups <- c("Hispanic Third Gen", "Asian Third Gen", "Arab Third Gen")
  title_subtitle <- list(
    title = "Third Generation Lorenz Curves (Whole Period)",
    subtitle = "Compared to Fourth Gen+ White and Black"
  )
  p <- create_lorenz_plot(data, gen_groups, title_subtitle)
  # Save the plot
  ggsave(paste0(figures_wd,"/18-third_gen_lorenz_curve.png"), width = 10, height = 6, units = "in")
  ggsave(paste0(thesis_plots,"/18-third_gen_lorenz_curve.png"), width = 10, height = 6, units = "in")

  p
}

############################################################
# Example usage (uncomment and run if lorenz_by_group is ready)
############################################################
first_gen_lorenz_plot(lorenz_by_group)
second_gen_lorenz_plot(lorenz_by_group)
third_gen_lorenz_plot(lorenz_by_group)