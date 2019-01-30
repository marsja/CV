############################################################################################################
# This script requires tidyverse and rorcid (install.packages(c("tidyverse", "rorcid))
# It fetches education from Orcid using the Orcid id (rid)
# Creates a 

library(tidyverse)

rid = "0000-0002-2379-9201"
edu <- rorcid::orcid_educations(rid)

edu_data <- edu[[rid]]$`education-summary` %>%
  
  mutate(Year = glue::glue("{month.abb[as.numeric(`start-date.month.value`)]} {`start-date.year.value`} \\
                           - {month.abb[as.numeric(`end-date.month.value`)]} {`end-date.year.value`} " )) %>%
      
  select("role-title", 
         "Year",
         "organization.name",
         "department-name"
  ) 

edu_data$theses.titles <- c("Attention capture by sudden and unexpected changes: a multisensory perspective.",
                   "Attention Capture: The Impact of Change in Spatial Sound Source on Behavior.",
                   "Attention Capture: Studying the Distracting Effect of One’s Own Name.")

edu_data$theses.urls <- c("http://umu.diva-portal.org/smash/record.jsf?pid=diva2%3A1156775",
                 "",
                 "http://urn.kb.se/resolve?urn=urn:nbn:se:umu:diva-46607")
edu_data$theses.sv <- c("Dr. Jessica K-Ljungerg, Professor Gregory Neely, and Dr. Patrik Hansson",
                        "Dr. Jessica K-Ljungberg",
                        "Dr. Jessica K-Ljungberg")