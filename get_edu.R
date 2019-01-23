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
         "organization.name"
  ) 