


# Purpose:
# Pull LEA in 5th grade All students and subgroups for three counties.



library(MCOE)
library(tidyverse)
library(here)
library(ggthemes)
library(googledrive)
library(googlesheets4)
library(glue)
library(ggrepel)
library(readxl)
library(vroom)
library(janitor)

### Setup -------

con <- MCOE::mcoe_sql_con()


drive_auth(email = "ddobrowski@montereycoe.org")
gs4_auth(token = drive_token())



sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"

codebook <- read_sheet(sheet_id,
                       col_types = "ccccccD") %>%
    filter(table == "CAASPP",
           field_name == "Subgroup_ID"
    )




### Collect data -------

entit <- vroom("sb_ca2019entities_csv.txt") %>%
    clean_names("none") %>%
    mutate(District_Code = as.character(District_Code),
           Test_Year = as.character(Test_Year))


caaspp <- tbl(con, "CAASPP") %>%
    filter(County_Code == "27"|County_Code == "33"|County_Code == "43",
           Grade == "5",
           Test_Year >= 2019,
           Test_Id == 1,
           School_Code == "0000000"
           #        CountyName == "Monterey",
           #        CharterSchool == "All",
        #           DASS == "All"
    ) %>%
    #    select(AcademicYear, ReportingCategory, Seal_of_Biliteracy_Count, Seal_of_Biliteracy_Rate) %>%
   #    head(50) %>% 
    collect() %>%
     mutate(Subgroup_ID = as.character(Subgroup_ID),
            Test_Year = as.character(Test_Year)) %>%
    left_join(codebook, by = c("Subgroup_ID" = "variable"))


caaspp2 <- caaspp %>%
    select(County_Code:School_Code, Subgroup_ID, definition, Students_Tested, Percentage_Standard_Met_and_Above ,Test_Year, Grade) %>%
    left_join(entit )

sheet_write(caaspp2)





