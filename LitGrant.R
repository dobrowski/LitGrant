


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
library(gt)

### Setup -------


districts <- c("Val Verde",
               "Alisal",
               "Alum Rock")

caaspp, 

con <- MCOE::mcoe_sql_con()


drive_auth(email = "ddobrowski@montereycoe.org")
gs4_auth(token = drive_token())



sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"

codebook <- read_sheet(sheet_id,
                       col_types = "ccccccD") %>%
    filter(table == "CAASPP",
           field_name == "Subgroup_ID"
    )

ss <- "https://docs.google.com/spreadsheets/d/1Vea9wakHGqHIX34uwbx36aixJ5tHLDbgOjXM3tZm1t4/edit#gid=400258878"


### Collect data -------

entit <- vroom("sb_ca2019entities_csv.txt") %>%
    clean_names("none") %>%
    mutate(District_Code = as.character(District_Code),
           Test_Year = as.character(Test_Year))


caaspp <- tbl(con, "CAASPP") %>%
    filter(County_Code == "27"|County_Code == "33"|County_Code == "43",
           District_Code == "65961"| District_Code == "75242"  |District_Code == "69369",
           Grade == "5",
           Test_Year >= 2019,
           Test_Id == 1,
  #         School_Code == "0000000"
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


caaspp.all <- caaspp2 %>%
    filter(definition == "All Students") %>%
    select(County_Code:School_Code,Percentage_Standard_Met_and_Above) %>%
    janitor::clean_names("snake")


sheet_write(caaspp2, ss = ss, sheet = "All LEAs")


paste0(districts, collapse = "|")

leas <- caaspp2 %>% 
    filter(str_detect(District_Name,paste0(districts, collapse = "|")))

sheet_write(leas, ss = ss, sheet = "LEAs Considered")
           





undup <- tbl(con, "UPC") %>%
    filter(county_code == "27"|county_code == "33"|county_code == "43",
           district_code == "65961"| district_code == "75242"  |district_code == "69369",
           academic_year == max(academic_year),
           school_type == "Elementary Schools (Public)",
           charter_school_y_n == "No"
           #        CountyName == "Monterey",
           #        CharterSchool == "All",
           #           DASS == "All"
    ) %>%
    #    select(AcademicYear, ReportingCategory, Seal_of_Biliteracy_Count, Seal_of_Biliteracy_Rate) %>%
    #    head(50) %>% 
    collect() 




undup %>%
    left_join(caaspp.all) %>%
    filter(low_grade %notin% c(5,6,7,8,9)  ) %>%
    select(county_name:school_name, total_enrollment, unduplicated_frpm_eligible_count, english_learner_el, percentage_standard_met_and_above) %>%
    mutate(frpm.perc = unduplicated_frpm_eligible_count/total_enrollment,
           el.perc = english_learner_el/ total_enrollment,
           percentage_standard_met_and_above = as.numeric(percentage_standard_met_and_above)/100) %>%
    select(-unduplicated_frpm_eligible_count, -english_learner_el) %>%
    arrange(county_name,desc(percentage_standard_met_and_above)) %>%
    gt(rowname_col = "school_name", groupname_col = c("county_name", "district_name")) %>%
    fmt_percent(columns = c(percentage_standard_met_and_above, frpm.perc,el.perc), decimals = 0)  %>%
    cols_label(total_enrollment = "Total\nEnrollment",
        frpm.perc = "Percent Free \n and Reduced \nPrice Meal",
               el.perc = "Percent English\nLearner",
        percentage_standard_met_and_above = "Percent Met \nand Above"
    ) %>%
    data_color(columns = c(percentage_standard_met_and_above),
               colors = scales::col_numeric(palette = c(
                    "orange",  "purple"),
                   domain = NULL)) %>%
    data_color(columns = c( frpm.perc,el.perc),
               colors = scales::col_numeric(palette = c(
                     "purple", "orange"),
                   domain = NULL)) %>%
    gtsave("Schools Table.png")
