### Dissertation Replication Code
setwd("~/...")

### 1 Set-up
set.seed(1)
## 1.1 load packages
library(haven)
library(dplyr)
library(lme4)
library(performance)
library(tidyr)
library(VIM)
library(lmtest)
library(EFA.dimensions)
library(psych)
library(kableExtra)
library(stargazer)
library(plm)
library(car)
library(tableone)
library(labelled)
library(ggplot2)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(tmap)
library(scales)

## 1.2 Create functions
# function: regression assumptions testing plm models
regression_diagnostics_plm <- function(model) {
  # homoscedasticity
  print(bptest(model))
  # autocorrelation of adjacent residuals
  print(pdwtest(model))
  # normal distribution of residuals
  hist(resid(model), breaks = 20)
}
# function: regression assumptions testing lm models
regression_diagnostics <- function(model) {
  # homoscedasticity
  print(bptest(model))
  # autocorrelation of adjacent residuals
  print(dwtest(model))
  # outliers
  print(outlierTest)
  # normal distribution of residuals
  hist(resid(model), breaks = 20)
}

#===============================================================================

### 2. Data preparation

## 2.1 Prepare Main dataset

## 2.1.1 load and merge data sets

## load data: Eder and Forting-Rittberger (2017)
data <- read_sav("~/.../ZA5990_v1-0-1.sav", encoding = "latin1")
# number of municipalities and elections
municipalities <- subset(data, level == 4 & election_dummy == 1)
length(municipalities$entity)
length(unique(municipalities$entity))

## load data: hand collected Hesse data set
hesse_raw <- read.csv("~/.../Hesse_Council_Data.csv", header = TRUE)
# create new variables
hesse_raw$rat_percentW <- hesse_raw$rat_women / hesse_raw$rat
hesse_raw$rat_LEFT <- (hesse_raw$rat_spd_seats + hesse_raw$rat_pds_seats + hesse_raw$rat_green_seats) / hesse_raw$rat 
hesse_raw$rat_MINOR <- (hesse_raw$rat_others_seats + hesse_raw$rat_group_seats) / hesse_raw$rat
hesse_raw$gms <- hesse_raw$gkz + 6000000
hesse_raw$inh_sqkm2 <- as.numeric(hesse_raw$inh_sqkm2)

# merge the datasets
data[setdiff(names(hesse_raw), names(data))] <- NA
hesse_raw[setdiff(names(data), names(hesse_raw))] <- NA
complete <- rbind(data, hesse_raw)

#===============================================================================

## 2.1.2 Prepare necessary variables

# fill missing values with the last known value
complete <- complete %>% 
  group_by(gms) %>% 
  fill(entity, voting_system, voting_list, inh_sqkm2)

# subset to election years
elections <- subset(complete, election_dummy == 1)

# create dependent variable
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag1 = dplyr::lag(rat_percentW, n = 1, default = NA)) %>%
  as.data.frame()
elections$change <- elections$rat_percentW - elections$lag1

# create electoral controls
# change in minor parties
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_minor = dplyr::lag(rat_MINOR, n = 1, default = NA)) %>% as.data.frame()
elections$change_minor <- elections$rat_MINOR - elections$lag_minor

# change in left party presence
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_left = dplyr::lag(rat_LEFT, n = 1, default = NA)) %>%  as.data.frame()
elections$change_left <- elections$rat_LEFT - elections$lag_left

# change in average party magnitude
elections$parties <- NA
for (i in 1:nrow(elections)) {
  elections$parties[i] <- sum(elections[i, c("rat_cdu_seats", "rat_spd_seats", "rat_pds_seats", "rat_fdp_seats", "rat_green_seats", "rat_group_seats", "rat_others_seats")] != 0)
}
elections$magnitude <- (elections$rat_cdu_seats + elections$rat_spd_seats + elections$rat_pds_seats + elections$rat_fdp_seats + elections$rat_green_seats + elections$rat_group_seats + elections$rat_others_seats)/elections$parties
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_magnitude = dplyr::lag(magnitude, n = 1, default = NA)) %>% as.data.frame()
elections$change_magnitude <- elections$magnitude - elections$lag_magnitude

# legislative turnover
# create lagged variables of party seats
# cdu
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_cdu = dplyr::lag(rat_cdu_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_cdu <- elections$rat_cdu_seats - elections$lag_cdu
# spd
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_spd = dplyr::lag(rat_spd_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_spd <- elections$rat_spd_seats - elections$lag_spd
# pds
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_pds = dplyr::lag(rat_pds_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_pds <- elections$rat_pds_seats - elections$lag_pds
# fdp
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_fdp = dplyr::lag(rat_fdp_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_fdp <- elections$rat_fdp_seats - elections$lag_fdp
# greens
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_green = dplyr::lag(rat_green_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_green <- elections$rat_green_seats - elections$lag_green
# voting groups
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_group = dplyr::lag(rat_group_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_group <- elections$rat_group_seats - elections$lag_group
# others
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_other = dplyr::lag(rat_others_seats, n = 1, default = NA)) %>% as.data.frame()
elections$change_other <- elections$rat_others_seats - elections$lag_other
# calculate legislative turnover
elections$turnover <- abs(elections$change_cdu) + abs(elections$change_spd) + abs(elections$change_pds) + abs(elections$change_fdp) + abs(elections$change_green) + abs(elections$change_group) + abs(elections$change_other)
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_turnover = dplyr::lag(turnover, n = 1, default = NA)) %>% as.data.frame()

# change in assembly size
elections <- elections %>% group_by(entity) %>% dplyr::mutate(lag_assembly = dplyr::lag(rat, n = 1, default = NA)) %>%
  as.data.frame()
elections$change_assembly <- elections$rat - elections$lag_assembly

# recode PR variable
elections$voting_system <- as.factor(ifelse(elections$voting_system == 2, 1, 0))



#===============================================================================

## 2.1.3 Add survey data
  
## load data: ALLBUS survey (GESIS, 2021) 
allbus_original <- read_sav("~/.../ZA5276_v1-1-0.sav", encoding = "latin1")
allbus <- subset(allbus_original, year == 2004 | year == 2008)

## 2.1.3.1 Prepare data

## recode federal state codes
allbus$land[allbus$land== 10] <- 1
allbus$land[allbus$land== 20] <- 2
allbus$land[allbus$land== 30] <- 3
allbus$land[allbus$land== 40] <- 4
allbus$land[allbus$land== 50] <- 5
allbus$land[allbus$land== 60] <- 6
allbus$land[allbus$land== 70] <- 7
allbus$land[allbus$land== 80] <- 8
allbus$land[allbus$land== 90] <- 9
allbus$land[allbus$land== 100] <- 10
allbus$land[allbus$land== 111] <- 11
allbus$land[allbus$land== 112] <- 11
allbus$land[allbus$land== 120] <- 12
allbus$land[allbus$land== 130] <- 13
allbus$land[allbus$land== 140] <- 14
allbus$land[allbus$land== 150] <- 15
allbus$land[allbus$land== 160] <- 16

# subset the relevant variables
allbus_efa <- allbus[, c("year", "fr01", "fr02", "fr04a", "fr06", "land")]

# missing data
length(which(is.na(allbus_efa$fr01))) / nrow(data) # 0.3%
length(which(is.na(allbus_efa$fr02))) / nrow(data) # 1%
length(which(is.na(allbus_efa$fr04a))) / nrow(data) # 0.6%
length(which(is.na(allbus_efa$fr06))) / nrow(data) # 0.9%

# imputation
allbus_efa <- hotdeck(allbus_efa, variable = "fr01")
allbus_efa <- hotdeck(allbus_efa, variable = "fr02")
allbus_efa <- hotdeck(allbus_efa, variable = "fr04a")
allbus_efa <- hotdeck(allbus_efa, variable = "fr06")

# adjust variable scores
allbus_efa$fr02 <- 5 - allbus_efa$fr02
allbus_efa$fr04a <- 5 - allbus_efa$fr04a
allbus_efa$fr06 <- 5 - allbus_efa$fr06
allbus_efa_complete <- allbus_efa[complete.cases(allbus_efa), ]
allbus_matrix <- allbus_efa_complete[, c("fr01", "fr02", "fr04a", "fr06")]
allbus_year <- allbus_efa_complete[, c("year", "land")]

# recode the variables as numeric
allbus_matrix$fr01 <- as.numeric(allbus_matrix$fr01)
allbus_matrix$fr02 <- as.numeric(allbus_matrix$fr02)
allbus_matrix$fr04a <- as.numeric(allbus_matrix$fr04a)
allbus_matrix$fr06 <- as.numeric(allbus_matrix$fr06)

## 2.1.3.2 Exploratory factor analysis

# test for factorability: given
table(allbus_matrix)
FACTORABILITY(allbus_matrix, corkind = 'polychoric')

# conduct EFA
fafit <- fa(allbus_matrix, nfactors = 1, cor = "poly", rotate = "varimax")
kbl(fafit$loadings[,1:1], digits=3, booktabs=TRUE, linesep="", 
    col.names = c("Factor 1"), caption = "Exploratory Factor Analysis Loadings") %>% 
  kable_styling(full_width = FALSE, latex_options = c("striped","bordered", "hold_position"))

# compute factor scores
factor_scores <- factor.scores(allbus_matrix, fafit$loadings)

# add factor scores to original data frame
allbus_matrix <- cbind(allbus_matrix, factor_scores$scores)

# bind it back together with year and land
allbus_EFA <- cbind(allbus_matrix, allbus_year)

# normalise factor scores
allbus_EFA$MR2 <- rescale(allbus_EFA$MR1)
summary(allbus_EFA$MR2)

# find the mean for each federal state and create data set that links state and mean value
allbus_hierarchy <- allbus_EFA %>%
  group_by(land, year) %>%
  summarize(mean_hierarchy = mean(MR2))

# create a hierarchy variable in main data set 
elections$hierarchy <- NA
for (i in 1:nrow(elections)) {
  land_i <- elections$rgs_land[i]
  year_i <- elections$year[i]
  # find the corresponding row in allbus_hierarchy
  row_match <- which(as.character(allbus_hierarchy$land) == as.character(land_i) & 
                       allbus_hierarchy$year == ifelse(year_i < 2007, 2004, 2008))
  
  # If a match is found, fill in the value column with the corresponding value 
  if (length(row_match) > 0) {
    elections$hierarchy[i] <- allbus_hierarchy$mean_hierarchy[row_match]
  }
}

# create hierarchy variable using a different cut of survey responses: 2004 survey wave values for all years until 2008 and 2008 survey wave values for all other years
elections$hierarchy2 <- NA
for (i in 1:nrow(elections)) {
  land_i <- elections$rgs_land[i]
  year_i <- elections$year[i]
  # find the corresponding row in allbus_hierarchy
  row_match <- which(as.character(allbus_hierarchy$land) == as.character(land_i) & 
                       allbus_hierarchy$year == ifelse(year_i < 2008, 2004, 2008))
  
  # if a match is found, fill in the value column with the corresponding value
  if (length(row_match) > 0) {
    elections$hierarchy2[i] <- allbus_hierarchy$mean_hierarchy[row_match]
  }
}

# create alternative hierarchy variable with another different cut: 2004 survey wave values for all years until 2004 and 2008 survey wave values for all other years 
elections$hierarchy3 <- NA
for (i in 1:nrow(elections)) {
  land_i <- elections$rgs_land[i]
  year_i <- elections$year[i]
  # find the corresponding row in allbus_hierarchy
  row_match <- which(as.character(allbus_hierarchy$land) == as.character(land_i) & 
                       allbus_hierarchy$year == ifelse(year_i < 2005, 2004, 2008))
  
  # if a match is found, fill in the value column with the corresponding value
  if (length(row_match) > 0) {
    elections$hierarchy3[i] <- allbus_hierarchy$mean_hierarchy[row_match]
  }
}

# subset to only municipal elections
municipal <- subset(elections, level==4)
length(unique(municipal$entity))

# create period variable
municipal$period <- cut(municipal$year,
                        breaks = c(2000, 2001, 2006, 2010, 2015, 2016),
                        labels = c(1, 2, 3, 4, 5))

#===============================================================================

### 2.2 Prepare second dataset (for testing H2)

## 2.1.1 Subset and load data

# subset to Hessean elections
hesse <- subset(municipal, rgs_land == 6)

## 4. load data: Baskaran and Hessami (2018)
meeting_topics <- read_dta("~/.../meeting_topics_data.dta")

## 2.1.2 Prepare variables

# adjust the geographical code of municipalities
meeting_topics$gms <- as.numeric(meeting_topics$gkz) + 6000000

# create new speaking rate variables in the main data set
hesse$speaking_men <- NA 
hesse$speaking_women <- NA

for (i in 1:nrow(hesse)) {
  gms_i <- hesse$gms[i]
  year_i <- hesse$year[i]
  # find the corresponding row in meeting_topics
  row_match <- which(as.character(meeting_topics$gms) == as.character(gms_i) & 
                       meeting_topics$jahr == ifelse(year_i == 2016, 2011, 
                                                     ifelse(year_i == 2011, 2006, 
                                                            ifelse(year_i == 2006, 2001, NA))))
  
  # if a match is found, fill in the value column with the corresponding value
  if (length(row_match) > 0) {
    hesse$speaking_men[i] <- meeting_topics$man_topic_bup[row_match]
    hesse$speaking_women[i] <- meeting_topics$women_topic_bup[row_match]
  }
}

# create alternative variable for robustness checks
hesse$speaking_gap <- hesse$speaking_women - hesse$speaking_men
summary(hesse$speaking_women)

#===============================================================================

### 3 Descriptive statistics

## 3.1 Figure 1
# adjust data format
complete$female <- as.numeric(complete$rat_percentW)
complete$year <- as.numeric(complete$year)
complete$levels <- factor(complete$level)

# obtain average hierarchy score per federal state
representation <- complete %>%
  filter(year >= 2000 & year <= 2013) %>%
  group_by(year, levels) %>%
  summarize(avg_proportion = mean(female))


### FIGURE 1 ### 
tiff("Figure1.jpg", width = 14, height = 8, units = "cm", res = 500)
ggplot(representation, aes(x = year, y = avg_proportion, group = levels, color = levels)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(name = "Level of government", palette = "RdBu", labels = c("National assembly", "State assembly", "District assembly", "Municipal assembly")) +
  labs(x = "Year", y = "% Women in assemblies (mean)") +
  theme(text = element_text(family = "serif"))
dev.off()

# 3.2 Figure 2
# load shapefile of Germany
Output.Areas <- readOGR("~/.../B-2022-AI003-3--AI0306--2023-04-28",  "B-2022-AI003-3--AI0306--2023-04-28")

# compute average hierarchy score per federal state
hierarchy <- allbus_EFA %>%
  group_by(land) %>%
  summarize(mean_hierarchy = mean(MR2))

# adjust data format
hierarchy$land <- as.numeric(hierarchy$land)
hierarchy$mean_hierarchy <- as.numeric(hierarchy$mean_hierarchy)
hierarchy$land[hierarchy$land == 1] <- "01"
hierarchy$land[hierarchy$land == 2] <- "02"
hierarchy$land[hierarchy$land == 3] <- "03"
hierarchy$land[hierarchy$land == 4] <- "04"
hierarchy$land[hierarchy$land == 5] <- "05"
hierarchy$land[hierarchy$land == 6] <- "06"
hierarchy$land[hierarchy$land == 7] <- "07"
hierarchy$land[hierarchy$land == 8] <- "08"
hierarchy$land[hierarchy$land == 9] <- "09"

# join data to the shapefile
OA.hierarchy <- merge(Output.Areas, hierarchy , by.x="schluessel", by.y="land")
range(OA.hierarchy$mean_hierarchy)

### FIGURE 2 ###
breaks <- seq(0, 1, length.out = 5)
map1 <- tm_shape(OA.hierarchy) +
  tm_fill("mean_hierarchy", 
          style = "cont", 
          breaks = breaks, 
          palette = "Blues", 
          title = " ") +
          #labels = c(" ", "Weaker hierarchy", " ", "Stronger hierarchy", " ")) +
  tm_borders(alpha = .4) +
  tm_layout(frame = FALSE, 
            legend.format = list(text.separator = "-"),
            legend.position = c("right", "bottom"),
            legend.outside = TRUE,
            legend.outside.size = 0.4,
            legend.title.fontfamily = "serif",
            legend.text.size = 0.8,
            legend.text.fontfamily = "serif",
            legend.bg.color = "white",
            legend.bg.alpha = 0.5,
            legend.frame = FALSE)
print(map1)
tmap_save(map1, "Figure2.jpg")


## 3.3 Table 1 
### TABLE 1 ###
var_label(municipal$rat_percentW) <- "% Seats held by women"
var_label(municipal$rat_LEFT) <- "% Seats held by left parties"
var_label(municipal$rat_MINOR) <- "% Seats held by minor parties"
var_label(municipal$turnover) <- "Legislative turnover (party level)"
var_label(municipal$magnitude) <- "Average party magnitude"
var_label(municipal$inh_sqkm2) <- "Inhabitants per km²"
var_label(municipal$voting_system) <- "Proportional representation"
listVars <- c("rat_percentW", "rat_LEFT", "rat_MINOR", "turnover", "magnitude", "inh_sqkm2" , "voting_system")
catVars <- c("voting_system")
table1 <- CreateTableOne(vars = listVars, data = municipal, factorVars = catVars)
table2 <- print(table1, catDigits=2, explain = TRUE, varLabels = TRUE )
write.csv(table2, "Figure3.csv")


#===============================================================================

## 4. Analysis
## 4.1 Study 1

# test for correlation within entities
icc(lmer(change ~ hierarchy + lag1 + change_left + change_magnitude + change_minor + voting_system + inh_sqkm2 + (1|entity), data = municipal))
# 0.040

# time FE: reduced
MH <- plm(change ~ hierarchy + lag1, data = municipal, index = c("year"), model = "within")
regression_diagnostics_plm(MH)
vcov_hc4_1 <- vcovHC(MH, type = "HC4")
# time FE: full
MH_full <- plm(change ~ hierarchy + lag1 + change_left + change_minor + change_magnitude + turnover + voting_system + inh_sqkm2, data = municipal, index = c("year"), model = "within")
regression_diagnostics_plm(MH_full)
vcov_hc4_2 <- vcovHC(MH_full, type = "HC4")

# BE: reduced
MH_BE <- plm(change ~ hierarchy + lag1, data = municipal, index = c("entity"), model = "between")
# BE: full
MH_full_BE <- plm(change ~ hierarchy + lag1 + change_left + change_minor + change_magnitude + turnover + voting_system + inh_sqkm2, data = municipal, index = c("entity"), model = "between")

### TABLE 2 ### 
stargazer(MH, MH_BE, MH_full, MH_full_BE, type = "text",
          se = list(sqrt(diag(vcov_hc4_1)), NULL, sqrt(diag(vcov_hc4_2)), NULL), 
          t = list(as.numeric(c(coef(MH) / sqrt(diag(vcov_hc4_1)))),
                   as.numeric(coef(MH_BE)), 
                   as.numeric(c(coef(MH_full) / sqrt(diag(vcov_hc4_2)))),
                   as.numeric(coef(MH_full_BE))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("year", "as.factor(year)"))

## 4.2 Study 2
## note: lm() models were used due to restricted availability of outlier tests for plm() models

# time FE: reduced
MS <- lm(change ~ speaking_women + lag1 + as.factor(year)-1, data = hesse)
regression_diagnostics(MS)
vcov_hc4_3 <- vcovHC(MS, type = "HC4")
# time FE: full
MS_full <- lm(change ~ speaking_women + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2 + as.factor(year)-1, data = hesse)
regression_diagnostics(MS_full)
vcov_hc4_4 <- vcovHC(MS_full, type = "HC4")

# BE: full
MS_BE <- plm(change ~ speaking_women + lag1, data = hesse, index = ("entity"), model = "between")
# BE: full
MS_full_BE <- plm(change ~ speaking_women + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2, data = hesse, index = ("entity"), model = "between")

# remove outliers
influencePlot(MS_full)
hesse$row_num <- row.names(hesse)
hesse_no <- hesse[!hesse$row_num %in% c("1281", "1171", "2494"), ]

# time FE: outliers removed
MS_no <- lm(change ~ speaking_women + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2 + as.factor(year)-1, data = hesse_no)
vcov_hc4_5 <- vcovHC(MS_full, type = "HC4")
# BE: outliers removed
MS_no_BE <- plm(change ~ speaking_women + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2, data = hesse_no, index = ("entity"), model = "between")

### TABLE 3 ###
stargazer(MS, MS_BE, MS_full, MS_full_BE, MS_no, MS_no_BE,
          type = "text",
          se = list(sqrt(diag(vcov_hc4_3)), NULL, sqrt(diag(vcov_hc4_4)), NULL, sqrt(diag(vcov_hc4_5)), NULL), 
          t = list(
                   as.numeric(c(coef(MS) / sqrt(diag(vcov_hc4_3)))),
                   as.numeric(coef(MS_BE)), 
                   as.numeric(c(coef(MS_full) / sqrt(diag(vcov_hc4_4)))), 
                   as.numeric(coef(MS_full_BE)),
                   as.numeric(c(coef(MS_no) / sqrt(diag(vcov_hc4_5)))),
                   as.numeric(coef(MS_no_BE))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("year", "as.factor(year)"))
#===============================================================================


## 5. Supplementary Analysis

## 5.1 Alternative variable definitions study 1
## 5.1.1 independent variable hierarchy2

# time FE: full
MH2_full <- plm(change ~ hierarchy2 + lag1 + change_left + change_minor + change_magnitude + turnover + voting_system + inh_sqkm2, data = municipal, index = c("year"), model = "within")
regression_diagnostics_plm(MH2_full)
vcov_hc4_1 <- vcovHC(MH2_full, type = "HC4")
# BE: full
MH2_full_BE <- plm(change ~ hierarchy2 + lag1 + change_left + change_minor + change_magnitude + turnover + voting_system + inh_sqkm2, data = municipal, index = c("entity"), model = "between")

## 5.1.2 independent variable hierarchy3

# time FE: full
MH3_full <- plm(change ~ hierarchy3 + lag1 + change_left + change_minor + change_magnitude + turnover + voting_system + inh_sqkm2, data = municipal, index = c("year"), model = "within")
regression_diagnostics_plm(MH3_full)
vcov_hc4_2 <- vcovHC(MH3_full, type = "HC4")
# BE: full
MH3_full_BE <- plm(change ~ hierarchy3 + lag1 + change_left + change_minor + change_magnitude + turnover + voting_system + inh_sqkm2, data = municipal, index = c("entity"), model = "between")

### TABLE B1 ### 
stargazer(MH2_full, MH2_full_BE, MH3_full, MH3_full_BE, type = "text",
          se = list(sqrt(diag(vcov_hc4_1)), NULL, sqrt(diag(vcov_hc4_2)), NULL), 
          t = list(as.numeric(c(coef(MH2_full) / sqrt(diag(vcov_hc4_1))),
                   as.numeric(coef(MH2_full_BE)), 
                   as.numeric(c(coef(MH3_full) / sqrt(diag(vcov_hc4_2)))),
                   as.numeric(coef(MH3_full_BE)))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("year", "as.factor(year)"))


## 5.2 Alternative variable descriptions: study 2

# time FE: reduced
MS2 <- lm(change ~ speaking_gap + lag1 + as.factor(year)-1, data = hesse)
regression_diagnostics(MS2)
vcov_hc4_3 <- vcovHC(MS2, type = "HC4")
# time FE: full
MS2_full <- lm(change ~ speaking_gap + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2 + as.factor(year)-1, data = hesse)
regression_diagnostics(MS2_full)
vcov_hc4_4 <- vcovHC(MS2_full, type = "HC4")

# BE: full
MS2_BE <- plm(change ~ speaking_gap + lag1, data = hesse, index = ("entity"), model = "between")
# BE: full
MS2_full_BE <- plm(change ~ speaking_gap + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2, data = hesse, index = ("entity"), model = "between")

# remove outliers
influencePlot(MS2_full)
hesse$row_num <- row.names(hesse)
hesse_no <- hesse[!hesse$row_num %in% c("1171", "1281", "2494"), ]

# time FE: outliers removed
MS2_no <- lm(change ~ speaking_gap + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2 + as.factor(year)-1, data = hesse_no)
vcov_hc4_5 <- vcovHC(MS2_full, type = "HC4")
# BE: outliers removed
MS2_no_BE <- plm(change ~ speaking_gap + lag1 + change_left + change_minor + change_magnitude + turnover + inh_sqkm2, data = hesse_no, index = ("entity"), model = "between")


### TABLE B2 ###
stargazer(MS2, MS2_BE, MS2_full, MS2_full_BE, MS2_no, MS2_no_BE,
          type = "text",
          se = list(sqrt(diag(vcov_hc4_3)), NULL, sqrt(diag(vcov_hc4_4)), NULL, sqrt(diag(vcov_hc4_5)), NULL), 
          t = list(as.numeric(c(coef(MS2) / sqrt(diag(vcov_hc4_3)))), 
                   as.numeric(coef(MS2_BE)),
                   as.numeric(c(coef(MS2_full) / sqrt(diag(vcov_hc4_4)))), 
                   as.numeric(coef(MS2_full_BE)),
                   as.numeric(c(coef(MS2_no) / sqrt(diag(vcov_hc4_5)))),
                   as.numeric(coef(MS2_no_BE))), 
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = c("year", "as.factor(year)"))
