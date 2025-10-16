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
                   .(median_ses_lw     = median(ses_lw_std, na.rm = TRUE),
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

# Create plot for First Generation only with fixed legend
first_gen_plot <- function(data) {
  # Filter data for first generation and reference group
  plot_data <- data[group %in% c("Hispanic First Gen", "Asian First Gen", 
                                "Arab First Gen", "Fourth Gen+ White", "Black")]
  
  ggplot(plot_data, aes(x = year_interval, y = median_ses_lw, 
                        color = group, 
                        shape = group,
                        linetype = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(name = "Group",
      values = c(
        "Hispanic First Gen" = "#D55E00",      # Orange-red
        "Asian First Gen" = "#0072B2",         # Deep blue
        "Arab First Gen" = "#CC79A7",          # Pink
        "Fourth Gen+ White" = "#000000",       # Black
        "Black" = "#009E73"                    # Teal
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
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.margin = margin()) +
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 3)),
           shape = guide_legend(nrow = 1, override.aes = list(size = 3)),
           fill = "none",
           linetype = "none") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
}

# Create and display the plot
first_gen_ses <- first_gen_plot(ses_by_group)
print(first_gen_ses)
ggsave(paste0(figures_wd,"/12-SES-median-firstgens.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/12-SES-median-firstgens.png"), width = 10, height = 6, units = "in")

# Create plot for Second Generation only with fixed legend
second_gen_plot <- function(data) {
  # Filter data for second generation and reference group
  plot_data <- data[group %in% c("Hispanic Second Gen", "Asian Second Gen", 
                                "Arab Second Gen", "Fourth Gen+ White", "Black")]
  
  ggplot(plot_data, aes(x = year_interval, y = median_ses_lw, 
                        color = group, 
                        shape = group,
                        linetype = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(name = "Group",
      values = c(
        "Hispanic Second Gen" = "#D55E00",     # Orange-red
        "Asian Second Gen" = "#0072B2",        # Deep blue
        "Arab Second Gen" = "#CC79A7",         # Pink
        "Fourth Gen+ White" = "#000000",       # Black
        "Black" = "#009E73"                    # Teal
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
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.margin = margin()) +
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 3)),
           shape = guide_legend(nrow = 1, override.aes = list(size = 3)),
           fill = "none",
           linetype = "none") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
}

# Create and display the plot
second_gen_ses <- second_gen_plot(ses_by_group)
print(second_gen_ses)
ggsave(paste0(figures_wd,"/13-SES-median-secondgens.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/13-SES-median-econdgens.png"), width = 10, height = 6, units = "in")

# Create plot for Third Generation only with fixed legend
third_gen_plot <- function(data) {
  # Filter data for third generation and reference group
  plot_data <- data[group %in% c("Hispanic Third Gen", "Asian Third Gen", 
                                "Arab Third Gen", "Fourth Gen+ White", "Black")]
  
  ggplot(plot_data, aes(x = year_interval, y = median_ses_lw, 
                        color = group, 
                        shape = group,
                        linetype = group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(name = "Group",
      values = c(
        "Hispanic Third Gen" = "#D55E00",      # Orange-red
        "Asian Third Gen" = "#0072B2",         # Deep blue
        "Arab Third Gen" = "#CC79A7",          # Pink
        "Fourth Gen+ White" = "#000000",       # Black
        "Black" = "#009E73"                    # Teal
    )) +
    scale_shape_manual(name = "Group",
      values = c(
        "Hispanic Third Gen" = 16,     # Circle
        "Asian Third Gen" = 17,        # Triangle
        "Arab Third Gen" = 15,         # Square
        "Fourth Gen+ White" = 18,      # Diamond
        "Black" = 19                   # Circle with plus
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
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.margin = margin()) +
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 3)),
           shape = guide_legend(nrow = 1, override.aes = list(size = 3)),
           fill = "none",
           linetype = "none") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
}

# Create and display the plot
third_gen_ses <- third_gen_plot(ses_by_group)
print(third_gen_ses)
ggsave(paste0(figures_wd,"/14-SES-median-thirdgens.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/14-SES-median-thirdgens.png"), width = 10, height = 6, units = "in")


# Calculate mean SES by year and racial/ethnic group using direct classifications
ses_by_demo <- CPS[, 
  .(median_ses_lw     = median(ses_lw_std, na.rm = TRUE),
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

# Create plot function
demographic_ses_plot <- function(data) {
  ggplot(data, aes(x = year_interval, y = median_ses_lw, 
                   color = group, 
                   shape = group,
                   linetype = group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(name = "Race/Ethnicity",
      values = c(
        "Hispanic" = "#D55E00",    # Orange-red
        "Arab" = "#CC79A7",        # Pink
        "White" = "#000000",       # Black
        "Black" = "#009E73",        # Teal
        "Asian" = "#0072B2"        # Deep blue
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
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.margin = margin()) +
    guides(color = guide_legend(override.aes = list(size = 3)),
           shape = guide_legend(override.aes = list(size = 3)),
           fill = "none",
           linetype = guide_legend()) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
}

# Create and display the plot
demo_plot <- demographic_ses_plot(ses_by_demo)
print(demo_plot)

ggsave(paste0(figures_wd,"/15-SES-median-selfreport-race-ethn.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/15-SES-median-selfreport-race-ethn.png"), width = 10, height = 6, units = "in")
s