## change the data into the format 

energy_data <- read_csv("Test_dataset/Banes_et_al_2014_all.csv")

energy_data <- as.matrix(energy_data)
t(energy_data) %>% as.data.frame(.) -> energy_data

energy_data$treatment <- rownames(energy_data)

energy_data %>% separate(
  treatment,
  into = c("Treatment_control", "ecosystem_functions"),
  sep = "-", 
  remove = TRUE
) %>% 
  dplyr::mutate(
    ecosystem_functions = stringr::str_remove(ecosystem_functions, "\\.\\.\\.[0-9]+$")
  ) %>%
  mutate(
   V1 =  ifelse(V1 == "X", "diversity_values", "values_functions" )
  ) %>% 
  mutate(
    ecosystem_functions = case_when(
      ecosystem_functions == "tot" ~ "total_energy fluxes", 
      ecosystem_functions == "Om" ~ "omnivory",
      ecosystem_functions %in% c("De", " De") ~ "detritivory",
      ecosystem_functions == "Pr" ~ "predation", 
      ecosystem_functions == "He" ~ "herbivory", 
      TRUE ~ ecosystem_functions 
    )
  ) %>% 
  pivot_longer(
    cols = V2:V9,
    names_to = "point_id",
    values_to = "value"
  ) %>% 
  pivot_wider(
    id_cols = c(Treatment_control, ecosystem_functions, point_id),
    names_from = V1,
    values_from = value
  ) %>% 
  select(!point_id) %>% 
  mutate(
    Treatment_control = ifelse(Treatment_control == "Jungle rubber ", "Jungle rubber",Treatment_control )  
  ) %>% 
  arrange(Treatment_control, ecosystem_functions) %>% 
  mutate(
    organism_groups = case_when(
      ecosystem_functions == "total_energy fluxes" ~ "macroinvertebrates", 
      ecosystem_functions == "omnivory" ~ "omnivores",
      ecosystem_functions == "detritivory" ~ "detritivores",
      ecosystem_functions == "predation" ~ "predators",
      ecosystem_functions == "herbivory" ~ "herbivores",
      TRUE ~ NA
    )
  ) -> energy_data_cleaned



#energy_long %>% 
  




energy_data_cleaned$soil_organisms = "invertebrate"

energy_data_cleaned$identification_method = "morphology"

energy_data_cleaned$Sample_location = "Leaf litter"

energy_data_cleaned$Soil_depth = 0
energy_data_cleaned$drivers = "land use"
energy_data_cleaned$Treatment_Number = 4
energy_data_cleaned$diversity_indice = "species richness"
energy_data_cleaned$DNA_sequence = 0

energy_data_cleaned %>% 
dplyr::select(drivers,Treatment_Number, Treatment_control,soil_organisms, 
                                            identification_method, Sample_location, Soil_depth, organism_groups, 
                                          diversity_indice,DNA_sequence, diversity_values, 
              ecosystem_functions, values_functions)%>% 
    dplyr::arrange(organism_groups, ecosystem_functions, Treatment_control) %>% 
  mutate(
    diversity_values = as.numeric(diversity_values), 
    values_functions = as.numeric(values_functions)
  ) %>%
  mutate(
    diversity_values = 10^diversity_values, 
    values_functions = 10^values_functions
  ) %>% write.csv("Banes_et_al_2024_cleaned2.csv")
