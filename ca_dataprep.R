## CA data prep ## 

library(htmltools)
library(data.table)

load("data/ca_data1.Rdata")

filenames <- list.files("data", full.names = TRUE)
c19cw <- fread(filenames[[4]]) %>% 
  filter(ST_ABBR == "CA") %>% 
  select(5, 8:11, 14:15) %>% 
  mutate(geo = as.numeric(FIPS), .keep = "unused")

vars <- c("Diagnosed diabetes among adults aged >=18 years",
          "Cancer (excluding skin cancer) among adults aged >=18 years", 
          "Coronary heart disease among adults aged >=18 years",
          "Current asthma among adults aged >=18 years", 
          "No leisure-time physical activity among adults aged >=18 years", 
          "Chronic kidney disease among adults aged >=18 years")
places <- fread(filenames[[5]]) %>% 
  filter(StateAbbr == "CA" & Year == 2019 & Measure %in% vars) %>% 
  select("LocationName", "MeasureId", "Data_Value") %>% 
  pivot_wider(id_cols = "LocationName", names_from = "MeasureId", values_from = "Data_Value") %>% 
  mutate(geo = as.numeric(LocationName), .keep = "unused")

ca_data <- ca_data1 %>% 
  mutate(geo = as.numeric(geo)) %>% 
  left_join(., places, by = "geo") %>% 
  left_join(., c19cw, by = "geo") %>% 
  rename(
    C19_Score = `Max Possible Score`
  )

pal <- colorNumeric(
  palette = "magma",
  domain = ca_data$LSI, 
  na.color = NA)

ca_nogeo <- ca_data %>% st_drop_geometry()
labs <- lapply(seq(nrow(ca_nogeo)), function(i) {
  paste0( '<p>', "<b>", ca_nogeo[i, "NAMELSAD10"], "</b>", '<br>', 
          "Tract Number: ", "<b>", ca_nogeo[i, "geo"], "</b>", '<br>',
          "Local Social Inequity: ", "<b>", ca_nogeo[i, "LSI"], "</b>", '<br>',
          "Life Expectancy: ", "<b>", ca_nogeo[i, "LifeExpectancy"], "</b>",'<br>', 
          "Pct Non-Hispanic Black:  ", "<b>", ca_nogeo[i, "NHBlackPct"], "%", "</b>",'<br>',
          "Pct Hispanic: ", "<b>", ca_nogeo[i, "HispanicPct"], "</b>", "%", '<br>',
          "Pct In Poverty: ", "<b>", ca_nogeo[i, "PovertyPct"], "</b>", "%", '<br>',
          "Binge Drinking Prevalence: ", "<b>", ca_nogeo[i, "BingeDrinkingPrevalence"], "%", "</b>", '<br>',
          "Smoking Prevalence: ", "<b>", ca_nogeo[i, "SmokingPrevalence"], "%", "</b>", '<br>',
          "Diabetes Prevalence: ", "<b>", ca_nogeo[i, "DiabetesPrevalence_2018"], "%", "</b>", '<br>',
          "Pct Unemployed: ", "<b>", ca_nogeo[i, "Unemployment_2019"], "%", "</b>", '<br>',
          "Pct Uninsured: ", "<b>", ca_nogeo[i, "NoInsurancePct"], "%", "</b>", '<br>',
          "Pct Physically Inactive: ", "<b>", ca_nogeo[i, "PhysicallyInactivePrevalence_2018"], "%", "</b>", '<br>',
          '</p>' ) 
})

credentials <- data.frame(
  user = c("hedj2022"),
  password = c("rarity2022"),
  stringsAsFactors = FALSE
)













