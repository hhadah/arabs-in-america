###################################
# Counting Script
# First version: Nov 6, 2024
# Edited: Nov 6th. 2024
###################################

library(survey)
library(srvyr)

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255")
scales::show_col(safe_colorblind_palette)

CPS <- CPS[!is.na(hwtfinl) | !is.na(asecwt)]
### Arabs

# objective
CPS_mean_reg_0bj_Arab <- CPS[!is.na(hwtfinl),.( MeanArab = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year, statefip)]
fwrite(CPS_mean_reg_0bj_Arab, "CPS_Arabs_bystate_DataTable.csv")

CPS_mean_reg_0bj_Arab <- CPS[!is.na(hwtfinl),.( MeanArab = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year, region)]
CPS_mean_0bj_Arab <- CPS[!is.na(hwtfinl), .(MeanArab = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year)]


ggplot(CPS_mean_0bj_Arab, aes(year, MeanArab)) +
  geom_point() + theme_customs() +
  scale_color_manual(values = safe_colorblind_palette) +
  stat_summary(geom = 'line') +
  labs(x = "Year", y = "Percent Objectively Arab") +
  ggtitle("Objectively Arab in the US Over Time") +
  scale_x_continuous(limits = c(1994, 2022), breaks = seq(1995, 2020, 5))# +
# scale_y_continuous(limits = c(0.08,0.17), breaks = seq(0.08,0.2,0.02))
ggsave("Arab_all_Obj.png", width = 10, height = 4, units = "in")


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
  scale_y_continuous(limits = c(0.0,0.045), breaks = seq(0.0,0.045,0.01))
ggsave("Arab.png", width = 10, height = 4, units = "in")


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

fwrite(CPS_arabs, "CPS_DataTable_Arabs.csv")

# Create data set of
# percent arabs by county

CPS_mean_reg_0bj_arabs_county <- CPS[!is.na(hwtfinl),.(Arabs = weighted.mean(Arab_Obj, w =hwtfinl, na.rm = T)), by = .(year, county)]
fwrite(CPS_mean_reg_0bj_arabs_county, "CPS_Arabs_by_county.csv")
