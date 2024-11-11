###################################
# Counting Script
# First version: Nov 6, 2024
# Edited: Nov 6th. 2024
###################################

# Upload the data
CPS <- fread(file.path(dropbox_dir,"CPS.csv.gz"))

library(survey)
library(srvyr)
library(Hmisc)

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255")
scales::show_col(safe_colorblind_palette)

CPS <- CPS[!is.na(hwtfinl) | !is.na(asecwt)]
### Arabs

# objective
CPS_mean_state_0bj_Arab <- CPS[!is.na(hwtfinl), .( MeanArab = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T), 
                                                     SDArab   = sqrt(wtd.var(Arab_Obj, w =hwtfinl, na.rm = T))), by = .(year, statefip)]
fwrite(CPS_mean_state_0bj_Arab, file.path(datasets, 'CPS_Arabs_bystate_DataTable.csv.gz'))

CPS_mean_reg_0bj_Arab <- CPS[!is.na(hwtfinl),.( MeanArab = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year, region)]
CPS_mean_0bj_Arab <- CPS[!is.na(hwtfinl), .(MeanArab = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year)]


#----------------- Plotting -----------------#
# Arabs Nationally

ggplot(CPS_mean_0bj_Arab, aes(year, MeanArab)) +
  geom_point() + theme_customs() +
  scale_color_manual(values = safe_colorblind_palette) +
  stat_summary(geom = 'line') +
  labs(x = "Year", y = "Percent Objectively Arab") +
  ggtitle("Arabs Second Gen+ in the US Over Time") +
  scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5)) +
  theme_customs()
# scale_y_continuous(limits = c(0.08,0.17), breaks = seq(0.08,0.2,0.02))
ggsave(paste0(figures_wd,"/01-Arab_all_Obj.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/01-Arab_all_Obj.png"), width = 10, height = 6, units = "in")

# Arabs by Region
ggplot(CPS_mean_reg_0bj_Arab, aes(year, MeanArab)) +
  geom_point(aes(color = factor(region))) +theme_customs() +
  # geom_smooth(aes(color = factor(region)))+
  scale_color_manual(values = safe_colorblind_palette, labels = c("New England", "M. Atlantic", "E. N. Central",
                                                                  "W. N. Central", "S. Atlantic", "E. S. Central",
                                                                  "W. S. Central", "Mountain", "Pacific"),
                     name = "Region") +
  stat_summary(geom = 'line', aes(color = factor(region))) +
  labs(x = "Year", y = "Percent Objectively Arab") +
  ggtitle("Objectively Arab in the US Over Time: By Region") +
  scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5)) +
  scale_y_continuous(limits = c(0.0,0.045), breaks = seq(0.0,0.045,0.01)) +
  theme_customs()
ggsave(paste0(figures_wd,"/02-Arab.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/02-Arab.png"), width = 10, height = 6, units = "in")


# Keep observations that are between the ages of 16 and 18
# Before: 10781814, after: 8367999
# CPS <- CPS[age<=17]
# CPS <- CPS[year<=2017 & year>=1996]
# Before: 8367999, after: 8367999

CPS <- CPS[!is.na(bpl) | !is.na(fbpl) | !is.na(mbpl) |
             !is.na(fbpl_pop) | !is.na(fbpl_mom) |
             !is.na(mbpl_pop) | !is.na(mbpl_mom) |
             !is.na(mbpl) | !is.na(mbpl)]

CPS <- CPS[!is.na(bpl) | !is.na(fbpl) | !is.na(mbpl)]
CPS <- CPS[!is.na(hispan)]
CPS <- CPS[citizen<9]


# Generate Education Variables

CPS[,':=' (DadGradCollege = ifelse(educ99_pop >= 15, 1, 0),
           MomGradCollege = ifelse(educ99_mom >= 15, 1, 0),
           GraduateCollege = ifelse(educ99 >= 15, 1, 0),
           DadProf = ifelse(educ99_pop >= 16, 1, 0),
           MomProf = ifelse(educ99_mom >= 16, 1, 0))]

CPS[,':=' (DadYearEduc = case_when(educ99_pop == 2 ~ 2,
                                   educ99_pop == 10 ~ 4,
                                   educ99_pop == 11 ~ 1,
                                   educ99_pop == 12 ~ 2,
                                   educ99_pop == 13 ~ 3,
                                   educ99_pop == 14 ~ 4,
                                   educ99_pop == 20 ~ 6,
                                   educ99_pop == 21 ~ 5,
                                   educ99_pop == 22 ~ 6,
                                   educ99_pop == 30 ~ 8,
                                   educ99_pop == 30 ~ 7,
                                   educ99_pop == 31 ~ 8,
                                   educ99_pop == 40 ~ 9,
                                   educ99_pop == 50 ~ 10,
                                   educ99_pop == 60 ~ 11,
                                   educ99_pop == 70 ~ 12,
                                   educ99_pop == 71 ~ 12,
                                   educ99_pop == 72 ~ 12,
                                   educ99_pop == 73 ~ 12,
                                   educ99_pop == 80 ~ 13,
                                   educ99_pop == 81 ~ 13,
                                   educ99_pop == 90 ~ 14,
                                   educ99_pop == 91 ~ 14,
                                   educ99_pop == 92 ~ 14,
                                   educ99_pop == 100 ~ 15,
                                   educ99_pop == 110 ~ 16,
                                   educ99_pop == 111 ~ 16,
                                   educ99_pop == 120 ~ 16,
                                   educ99_pop == 121 ~ 16,
                                   educ99_pop == 122 ~ 16,
                                   educ99_pop == 123 ~ 18,
                                   educ99_pop == 124 ~ 21,
                                   educ99_pop == 125 ~ 21),
           MomYearEduc = case_when(educ99_mom == 2 ~ 2,
                                   educ99_mom == 10 ~ 4,
                                   educ99_mom == 11 ~ 1,
                                   educ99_mom == 12 ~ 2,
                                   educ99_mom == 13 ~ 3,
                                   educ99_mom == 14 ~ 4,
                                   educ99_mom == 20 ~ 6,
                                   educ99_mom == 21 ~ 5,
                                   educ99_mom == 22 ~ 6,
                                   educ99_mom == 30 ~ 8,
                                   educ99_mom == 30 ~ 7,
                                   educ99_mom == 31 ~ 8,
                                   educ99_mom == 40 ~ 9,
                                   educ99_mom == 50 ~ 10,
                                   educ99_mom == 60 ~ 11,
                                   educ99_mom == 70 ~ 12,
                                   educ99_mom == 71 ~ 12,
                                   educ99_mom == 72 ~ 12,
                                   educ99_mom == 73 ~ 12,
                                   educ99_mom == 80 ~ 13,
                                   educ99_mom == 81 ~ 13,
                                   educ99_mom == 90 ~ 14,
                                   educ99_mom == 91 ~ 14,
                                   educ99_mom == 92 ~ 14,
                                   educ99_mom == 100 ~ 15,
                                   educ99_mom == 110 ~ 16,
                                   educ99_mom == 111 ~ 16,
                                   educ99_mom == 120 ~ 16,
                                   educ99_mom == 121 ~ 16,
                                   educ99_mom == 122 ~ 16,
                                   educ99_mom == 123 ~ 18,
                                   educ99_mom == 124 ~ 21,
                                   educ99_mom == 125 ~ 21))]

# CPS[, LogFamilyEarnings:= case_when(ftotval_pop >0 ~ log(ftotval_pop),
#                                     ftotval_pop <0 ~ log(ftotval_pop))]
# CPS[, LogFamilyEarnings:= case_when(ftotval >0 ~ log(ftotval),
#                                     ftotval <0 ~ log(ftotval))]

# CPS[, ':=' (
#             Loginctot = case_when(inctot > 0 ~ log(inctot)),
#             Logincwage = case_when(incwage > 0 ~ log(incwage)),
#             Loghourwage = case_when(hourwage > 0 ~ log(hourwage)),
#             Logearnweek = case_when(earnweek > 0 ~ log(earnweek))
#             )
#     ]
#CrossTable(CPS$GraduateCollege)

# Cross Table
# CrossTable(CPS$FirstGen,CPS$Hispanic)
# CrossTable(CPS$SecondGen,CPS$Hispanic)
# CrossTable(CPS$ThirdGen,CPS$Hispanic)


#summarise(CPS, mean = mean(age, na.rm = TRUE))
#summary(CPS$age)

# Generate Poor Health Variable
# CPS[, PoorHealth:= case_when(health <= 4 ~ 0,
#                        health == 5 ~ 1)]
# Generate Health Variable
# CPS[, ":=" (Health_Child = health,
#             Health_Dad = health_pop,
#             Health_Mom = health_mom)]
# summarise(CPS, mean_Child = mean(Health_Child, na.rm = TRUE),
#           mean_Dad = mean(Health_Dad, na.rm = TRUE),
#           mean_Mom = mean(Health_Mom, na.rm = TRUE))
# CrossTable(CPS$Type)
# Save the Data

# Arabs
CPS_arabs <- CPS[FirstGen_Arab == 1 | SecondGen_Arab == 1 | ThirdGen_Arab == 1]
fwrite(CPS_arabs, file.path(datasets, 'CPS_DataTable_Arabs.csv.gz'))

# Create data set of
# percent arabs by county

CPS_mean_reg_0bj_arabs_county <- CPS[!is.na(hwtfinl),.(Arabs = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year, county)]
fwrite(CPS_mean_reg_0bj_arabs_county, file.path(datasets, 'CPS_Arabs_by_county.csv.gz'))

#----------------- Map -----------------#
# Arabs in 2020 by State
# Required Libraries
# Required Libraries
library(ggspatial)
library(ggplot2)
library(dplyr)
library(sf)
library(maps)
library(tigris)
library(patchwork)  # For combining plots

# Separate Alaska, Hawaii, and contiguous US
contiguous_us <- map_data %>% filter(!NAME %in% c("Alaska", "Hawaii"))

# Transform Alaska with more extreme shifting
alaska <- map_data %>% 
  filter(NAME == "Alaska") %>%
  # Transform to geographic coordinates
  st_transform(4326) %>% 
  st_shift_longitude() %>%
  # Transform back and adjust position
  st_transform(st_crs(map_data)) %>%
  mutate(geometry = geometry * 0.35) %>%
  # Much larger shifting values to move Alaska completely out
  mutate(geometry = geometry + c(-8500000, -4500000))

# Transform Hawaii
hawaii <- map_data %>% 
  filter(NAME == "Hawaii") %>%
  st_transform(st_crs(map_data)) %>%
  mutate(geometry = geometry * 0.5) %>%
  mutate(geometry = geometry + c(5000000, -1000000))

# Create plots for each region
mainland_plot <- ggplot(contiguous_us) +
  geom_sf(aes(fill = Arab_Category), color = "white", size = 0.2) +
  scale_fill_manual(values = arab_palette, name = "Percent Arab") +
  labs(title = "Percent Arabs by State (2020)"#,
       #subtitle = "CPS Data, Weighted Means"
       ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

alaska_plot <- ggplot() +
  geom_sf(data = alaska, aes(fill = Arab_Category), color = "white", size = 0.2) +
  scale_fill_manual(values = arab_palette, guide = "none") +
  theme_void() +
  coord_sf(datum = NA)

hawaii_plot <- ggplot() +
  geom_sf(data = hawaii, aes(fill = Arab_Category), color = "white", size = 0.2) +
  scale_fill_manual(values = arab_palette, guide = "none") +
  theme_void() +
  coord_sf(datum = NA)

# Combine the plots with adjusted inset positions
final_map <- mainland_plot +
  inset_element(alaska_plot, left = 0.01, bottom = 0.1, right = 0.3, top = 0.3) +  # Adjusted inset position
  inset_element(hawaii_plot, left = 0.01, bottom = 0.02, right = 0.15, top = 0.15)

print(final_map)

ggsave(paste0(figures_wd,"/03-Arab-Map-2020.png"), width = 10, height = 6, units = "in")
ggsave(paste0(thesis_plots,"/03-Arab-Map-2020.png"), width = 10, height = 6, units = "in")

#----------------- Table -----------------#
# Arabis in 2020 by State

CPS_mean_state_0bj_Arab  <- as.data.frame(CPS_mean_state_0bj_Arab)

CPS_mean_state_0bj_Arab_2020  <- CPS_mean_state_0bj_Arab |> 
  filter(year == 2020)  |> 
  mutate(state_name = fips(statefip, to = "Name"))

CPS_mean_state_0bj_Arab_2020 <- CPS_mean_state_0bj_Arab_2020 |>
  rename(
    State = state_name,
    `Proportion Arab` = MeanArab,
    `SD Arab` = SDArab
  )

CPS_mean_state_0bj_Arab_2020 <- CPS_mean_state_0bj_Arab_2020 |>
  select(State, `Proportion Arab`, `SD Arab`)

CPS_mean_state_0bj_Arab_2020 <- CPS_mean_state_0bj_Arab_2020 |>
  mutate(`Percent Arab` = scales::percent(`Proportion Arab`, accuracy = 0.01),
         `SD Arab` = scales::percent(`SD Arab`, accuracy = 0.01))
CPS_mean_state_0bj_Arab_2020 <- CPS_mean_state_0bj_Arab_2020 |>
  select(State, `Percent Arab`, `SD Arab`)
knitr::kable(CPS_mean_state_0bj_Arab_2020, "latex", align = "lcc",
             booktabs = T,
             escape = T,
             longtable = T, 
             caption = "Percent Arab by State \\label{tab:tab-01}") |>
  column_spec(1, bold = T)  |> 
  kable_classic(full_width = F) |> 
  kable_styling(#bootstrap_options = c("hover", "condensed"), 
                latex_options = c(#"scale_down", 
                "repeat_header",
                "HOLD_position")) |> 
  footnote(number = c("This table shows the proportion of Arab Americans in each state in 2020. The standard deviation is also reported.", 
                      "Source: Author's calculations from the Current Population Survey (CPS) 2020."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T
  ) |> 
  save_kable(file.path(tables_wd,"tab01-arab-bystate.tex")) |> 
  save_kable(file.path(thesis_tabs,"tab01-arab-bystate.tex")) 
