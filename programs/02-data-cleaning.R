###################################
# CPS Cleaning Script
# First version: May 24, 2022
# Edited: Nov 6th. 2024
###################################

# Upload the data
CPS <- fread(file.path(dropbox_dir,"cps_00062.csv"))

# CPS <- setDT(CPS)
names(CPS) <- tolower(names(CPS))

# Generate Hispanic Variable
CPS[,':=' (Hispanic = ifelse(hispan >= 100 & hispan < 901 & (race == 100), 1, 0))]
CPS[,':=' (Black = ifelse(Hispanic != 1 & race == 200, 1, 0))]
CPS[,':=' (White = ifelse(Hispanic != 1 & race == 100, 1, 0))]
CPS[,':=' (Hispanic_Dad = ifelse(hispan_pop >= 100 & hispan_pop < 901 & (race_pop == 100), 1, 0),
           Hispanic_Mom = ifelse(hispan_mom >= 100 & hispan_mom < 901 & (race_mom == 100), 1, 0))]
CPS[,':=' (White_Dad = ifelse(Hispanic_Dad != 1 & race_pop == 100, 1, 0),
           White_Mom = ifelse(Hispanic_Mom != 1 & race_mom == 100, 1, 0))]
CPS[,':=' (Asian = ifelse((race     >= 650 & race     <= 652) & Hispanic     == 0, 1, 0))]

CPS[,':=' (Arab = ifelse((bpl       <= 60019 & bpl       >= 53000), 1, 0))]

CPS[,':=' (NativeBorn =                ifelse((bpl            <= 12090 & bpl            >= 09900 & bpl       != 11000), 1, 0))]
CPS[,':=' (NativeBorn_Dad =            ifelse((fbpl           <= 12090 & fbpl           >= 09900 & fbpl      != 11000), 1, 0))]
CPS[,':=' (NativeBorn_Mom =            ifelse((mbpl           <= 12090 & mbpl           >= 09900 & mbpl      != 11000), 1, 0))]
CPS[,':=' (NativeBorn_MatGrandMother = ifelse((mbpl_mom       <= 12090 & mbpl_mom       >= 09900 & mbpl_mom  != 11000), 1, 0))]
CPS[,':=' (NativeBorn_PatGrandMother = ifelse((mbpl_pop       <= 12090 & mbpl_pop       >= 09900 & mbpl_pop  != 11000), 1, 0))]
CPS[,':=' (NativeBorn_MatGrandFather = ifelse((fbpl_mom       <= 12090 & fbpl_mom       >= 09900 & fbpl_mom  != 11000), 1, 0))]
CPS[,':=' (NativeBorn_PatGrandFather = ifelse((fbpl_pop       <= 12090 & fbpl_pop       >= 09900 & fbpl_pop  != 11000), 1, 0))]

# Generate a dummy variable that is 
# equal to one if a person is born 
# in a Spanish speaking country 
# and zero other wise

CPS[, ':=' (
  SpanishSpeakingPOB =                ifelse((bpl       <= 31000 & bpl       >= 20000) | bpl       == 11000 | bpl            == 43800, 1, 0),
  SpanishSpeakingPOB_Father =         ifelse((fbpl      <= 31000 & fbpl      >= 20000) | fbpl      == 11000 | fbpl           == 43800, 1, 0),
  SpanishSpeakingPOB_Mother =         ifelse((mbpl      <= 31000 & mbpl      >= 20000) | mbpl      == 11000 | mbpl           == 43800, 1, 0),
  SpanishSpeakingPOB_MatGrandMother = ifelse((mbpl_mom  <= 31000 & mbpl_mom  >= 20000) | mbpl_mom  == 11000 | mbpl_mom       == 43800, 1, 0),
  SpanishSpeakingPOB_PatGrandMother = ifelse((mbpl_pop  <= 31000 & mbpl_pop  >= 20000) | mbpl_pop  == 11000 | mbpl_pop       == 43800, 1, 0),
  SpanishSpeakingPOB_MatGrandFather = ifelse((fbpl_mom  <= 31000 & fbpl_mom  >= 20000) | fbpl_mom  == 11000 | fbpl_mom       == 43800, 1, 0),
  SpanishSpeakingPOB_PatGrandFather = ifelse((fbpl_pop  <= 31000 & fbpl_pop  >= 20000) | fbpl_pop  == 11000 | fbpl_pop       == 43800, 1, 0),
  
  ArabPOB =                ifelse((bpl       <= 60019 & bpl       >= 53000), 1, 0),
  ArabPOB_Father =         ifelse((fbpl      <= 60019 & fbpl      >= 53000), 1, 0),
  ArabPOB_Mother =         ifelse((mbpl      <= 60019 & mbpl      >= 53000), 1, 0),
  ArabPOB_MatGrandMother = ifelse((mbpl_mom  <= 60019 & mbpl_mom  >= 53000), 1, 0),
  ArabPOB_PatGrandMother = ifelse((mbpl_pop  <= 60019 & mbpl_pop  >= 53000), 1, 0),
  ArabPOB_MatGrandFather = ifelse((fbpl_mom  <= 60019 & fbpl_mom  >= 53000), 1, 0),
  ArabPOB_PatGrandFather = ifelse((fbpl_pop  <= 60019 & fbpl_pop  >= 53000), 1, 0),
  
  AsianPOB =                ifelse((bpl       <= 51800 & bpl       >= 50000), 1, 0),
  AsianPOB_Father =         ifelse((fbpl      <= 51800 & fbpl      >= 50000), 1, 0),
  AsianPOB_Mother =         ifelse((mbpl      <= 51800 & mbpl      >= 50000), 1, 0),
  AsianPOB_MatGrandMother = ifelse((mbpl_mom  <= 51800 & mbpl_mom  >= 50000), 1, 0),
  AsianPOB_PatGrandMother = ifelse((mbpl_pop  <= 51800 & mbpl_pop  >= 50000), 1, 0),
  AsianPOB_MatGrandFather = ifelse((fbpl_mom  <= 51800 & fbpl_mom  >= 50000), 1, 0),
  AsianPOB_PatGrandFather = ifelse((fbpl_pop  <= 51800 & fbpl_pop  >= 50000), 1, 0),
  
  BlackChild =   ifelse((race     == 200 | race     == 801) & Hispanic     == 0, 1, 0),
  Black_Dad  = ifelse((race_pop == 200 | race_pop == 801) & Hispanic_Dad == 0, 1, 0),
  Black_Mom  = ifelse((race_mom == 200 | race_mom == 801) & Hispanic_Mom == 0, 1, 0),
  
  AsianChild =   ifelse((race     >= 650 & race     <= 652) & Hispanic     == 0, 1, 0),
  Asian_Dad  = ifelse((race_pop >= 650 & race_pop <= 652) & Hispanic_Dad == 0, 1, 0),
  Asian_Mom  = ifelse((race_mom >= 650 & race_mom <= 652) & Hispanic_Mom == 0, 1, 0),
  
  SameSex_Couple = case_when(sex_mom==sex_mom2 ~ 1,
                             sex_pop==sex_pop2 ~ 1,
                             TRUE ~ 0)
  )]



# Generate a dummy variable for the different generations of immigrants:
#   
# 1. First generation is a person that was born 
# in a Spanish speaking country
# 2. Second generation is a person 
# that was born in the US and one of 
# their parents was born in a Spanish speaking country
# 3. Third generation is a person that 
# was born in the US and both parents were 
# born in the US and at least one grandparent 
# was born in a Spanish speaking country
# 4. Fourth generation is a person that 
# was born in the US, both parents were 
# born in the US, all grandparent s
# were born in the US and the 
# kid identefies as Hispanic

CPS[, ':=' (
  FirstGen = ifelse(SpanishSpeakingPOB == 1  
                    & (SpanishSpeakingPOB_Father == 1 &
                         SpanishSpeakingPOB_Mother == 1)
                    & (citizen != 3), 1, 0),
  SecondGen = ifelse(NativeBorn == 1 & 
                       (SpanishSpeakingPOB_Father == 1 |
                          SpanishSpeakingPOB_Mother == 1)
                     & (citizen != 3 | citizen != 2 | citizen != 1)
                     , 1, 0),
  ThirdGen = ifelse((  NativeBorn == 1 &
                       NativeBorn_Dad == 1 &
                       NativeBorn_Mom == 1)
                    & (SpanishSpeakingPOB_MatGrandMother == 1 |
                         SpanishSpeakingPOB_PatGrandMother == 1 |
                         SpanishSpeakingPOB_MatGrandFather == 1 |
                         SpanishSpeakingPOB_PatGrandFather == 1)
                    , 1, 0),
  FourthGen = ifelse((  NativeBorn == 1 &
                        NativeBorn_Dad == 1 &
                        NativeBorn_Mom == 1)
                     &   (  NativeBorn_MatGrandMother == 1 &
                            NativeBorn_PatGrandMother == 1 &
                            NativeBorn_MatGrandFather == 1 &
                            NativeBorn_PatGrandFather == 1) &
                       (Hispanic_Dad == 1 | Hispanic_Mom == 1),
                     1,0),
  FourthGen_White = ifelse(         (NativeBorn == 1 &
                                    NativeBorn_Dad == 1 &
                                    NativeBorn_Mom == 1)
                           &        (NativeBorn_MatGrandMother == 1 &
                                    NativeBorn_PatGrandMother == 1 &
                                    NativeBorn_MatGrandFather == 1 &
                                    NativeBorn_PatGrandFather == 1) & (Hispanic_Dad == 0 & Hispanic_Mom == 0 & White_Dad == 1 & White_Mom == 1), 1,0),
  
  FirstGen_Asian = ifelse(AsianPOB== 1  
                       & (AsianPOB_Father == 1 &
                         AsianPOB_Mother == 1)
                       & (citizen != 3), 1, 0),
  SecondGen_Asian = ifelse(NativeBorn== 1 & 
                          (AsianPOB_Father == 1 |
                          AsianPOB_Mother == 1)
                        & (citizen != 3 | citizen != 2 | citizen != 1)
                     , 1, 0),
  ThirdGen_Asian = ifelse((NativeBorn== 1 &
                           NativeBorn_Dad == 1 &
                           NativeBorn_Mom == 1)
                    & (AsianPOB_MatGrandMother == 1 |
                         AsianPOB_PatGrandMother == 1 |
                         AsianPOB_MatGrandFather == 1 |
                         AsianPOB_PatGrandFather == 1)
                    , 1, 0),
  FourthGen_Asian = ifelse((NativeBorn== 1 &
                        NativeBorn_Dad == 1 &
                          NativeBorn_Mom == 1)
                     & (  NativeBorn_MatGrandMother == 1 &
                          NativeBorn_PatGrandMother == 1 &
                          NativeBorn_MatGrandFather == 1 &
                          NativeBorn_PatGrandFather == 1) &
                       (Asian_Dad == 1 | Asian_Mom == 1),
                     1,0),
  
  FirstGen_Arab = ifelse(ArabPOB== 1  
                          & (ArabPOB_Father == 1 &
                               ArabPOB_Mother == 1)
                          & (citizen != 3), 1, 0),
  SecondGen_Arab = ifelse(NativeBorn== 1 & 
                             (ArabPOB_Father == 1 |
                                ArabPOB_Mother == 1)
                           & (citizen != 3 | citizen != 2 | citizen != 1)
                           , 1, 0),
  ThirdGen_Arab = ifelse(     (NativeBorn== 1 &
                               NativeBorn_Dad == 1 &
                               NativeBorn_Mom == 1)
                          &   (ArabPOB_MatGrandMother == 1 |
                               ArabPOB_PatGrandMother == 1 |
                               ArabPOB_MatGrandFather == 1 |
                               ArabPOB_PatGrandFather == 1)
                          , 1, 0)
)
]


# Generate a Type varialbe
CPS[, Type := case_when(FirstGen == 1 ~ "First Generation",
                        SecondGen == 1 ~ "Second Generation",
                        ThirdGen == 1 ~ "Third Generation",
                        FourthGen == 1 ~ "Fourth Generation+ Hispanic",
                        FourthGen_White == 1 ~ "Fourth Generation+ White")]

CPS[, Type_Asian := case_when(FirstGen_Asian == 1 ~  "First Generation Asian",
                              SecondGen_Asian == 1 ~ "Second Generation Asian",
                              ThirdGen_Asian == 1 ~  "Third Generation Asian",
                              FourthGen_Asian == 1 ~ "Fourth Generation+ Asian",
                              FourthGen_White == 1 ~ "Fourth Generation+ White")]

CPS[, Type_Arab := case_when(FirstGen_Arab == 1 ~  "First Generation",
                             SecondGen_Arab == 1 ~ "Second Generation",
                             ThirdGen_Arab == 1 ~  "Third Generation",
                             FourthGen_White == 1 ~ "Fourth Generation+ White")]

CPS[, Type_Black := case_when(
  Black_Dad == 1 & Black_Mom == 1 ~ "Black Father-Black Mother",
  Black_Dad == 1 & White_Mom == 1 ~ "Black Father-White Mother",
  White_Dad == 1 & Black_Mom == 1 ~ "White Father-Black Mother",
  White_Dad == 1 & White_Mom == 1 ~ "White Father-White Mother"
)]

CPS[, Interracial_Black := case_when(
  Black_Dad == 1 & Black_Mom == 1 ~ 0,
  Black_Dad == 1 & White_Mom == 1 ~ 1,
  White_Dad == 1 & Black_Mom == 1 ~ 1,
  White_Dad == 1 & White_Mom == 1 ~ 0
)]
# Generate a Type varialbe
CPS[, Hispanic_Obj := ifelse(FirstGen == 1 | SecondGen == 1 | ThirdGen == 1 | FourthGen == 1, 1, 0)]
CPS[, Asian_Obj := ifelse(FirstGen_Asian == 1 | SecondGen_Asian == 1 | ThirdGen_Asian == 1 | FourthGen_Asian == 1, 1, 0)]
CPS[, Arab_Obj := ifelse(FirstGen_Arab == 1 | SecondGen_Arab == 1 | ThirdGen_Arab == 1, 1, 0)]