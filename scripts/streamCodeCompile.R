# Join EscEstSppHeader_area24 to WaterbodyValues 
# source script
# Feb 2024




# Load libraries --------------------------
library(tidyverse)
library(readxl)
library(here)



# Helpers --------------------------
analysis_year <- 2023
analysis_area1 <- 24
analysis_area2 <- 23


# ================= READ DATA =================

# AUC Data Analysis table --------------------------
dataTable <- list.files(here("data", "sil-auc-db", paste0("area", analysis_area1)), pattern="^AUC_DataAnalysisTable", full.names=T) %>%
  purrr::set_names(
    list.files(here("data", "sil-auc-db", paste0("area", analysis_area1)), pattern = "^AUC_DataAnalysisTable", full.names = F)) %>%
  map(~readxl::read_excel(path = .x, trim_ws=T), id = "path") %>%
  list_rbind(names_to = "file_source") %>%
  print()
  

# Esc Estimates Spp table --------------------------
escEst <- list.files(here("data", "sil-auc-db", paste0("area", analysis_area1)), pattern="^EscEstSppHeader", full.names=T) %>%
  map(~readxl::read_excel(path = .x, trim_ws=T), id = "path") %>% 
  list_rbind() 


# Waterbodies Values lookup table --------------------------
wbdyVals <- readxl::read_excel(here("data", "sil-auc-db", "WaterbodiesValues.xlsx")) %>% 
  select(WATERSHED_CDE, AREA, NAME) %>%
  rename(WatershedCode = WATERSHED_CDE) %>%
  filter(!is.na(WatershedCode), !is.na(NAME)) %>% 
  group_by(WatershedCode) %>% 
  mutate(n=n(),
         system = case_when(n>1 ~ paste0("system", seq(1,length(n),by=1)),
                            n==1 ~ "system1")) %>%
  group_by(WatershedCode) %>% 
  pivot_wider(names_from=system, values_from=NAME) %>% 
  arrange(AREA, desc(n))  %>%
  unite("SystemName", contains("system"), sep = " / ", remove=T, na.rm=T) %>% 
  mutate(SystemName = str_to_title(SystemName))



##################################### 


# ================= JOIN =================
escEstNamed <- left_join(escEst,
                         wbdyVals,
                         by="WatershedCode", na_matches="never", relationship="many-to-one") %>%
  mutate(SystemName = case_when(WatershedCode=="930-355300-00000-00000-0000-0000-000-000-000-000-000-SYS" ~ "Bedwell/Ursus SYS",
                                TRUE ~ SystemName)) %>% 
  print()











