```{r, echo = FALSE, message=FALSE, warning=FALSE}
# clean raw survey data
survey_data <- survey_data_raw %>% 
  rename(province = cps19_province) %>% 
  mutate(age_group = case_when(
    age < 25 ~ "15 to 24 years",
    age < 35 ~ "25 to 34 years",
    age < 45 ~ "35 to 44 years",
    age < 55 ~ "45 to 54 years",
    age < 65 ~ "55 to 64 years",
    age < 75 ~ "65 to 74 years",
    age >= 75 ~ "75 years and over"
  )) %>% 
  mutate(age_level = case_when(
    age < 25 ~ 0,
    age < 35 ~ 1,
    age < 45 ~ 2,
    age < 55 ~ 3,
    age < 65 ~ 4,
    age < 75 ~ 5,
    age >= 75 ~ 6
  )) %>% 
  mutate(sex = case_when(
    cps19_gender == "A woman" ~ "Female",
    cps19_gender == "A man" ~ "Male"
  )) %>% 
  mutate(edu = case_when(
    cps19_education == "Don't know/ Prefer not to answer" ~ 0,
    cps19_education == "No schooling" ~ 0,
    cps19_education == "Some elementary school" ~ 0,
    cps19_education == "Completed elementary school" ~ 1,
    cps19_education == "Some secondary/ high school" ~ 1,
    cps19_education == "Completed secondary/ high school" ~ 2,
    cps19_education == "Some technical, community college, CEGEP, College Classique" ~ 3,
    cps19_education == "Completed technical, community college, CEGEP, College Classique" ~ 4,
    cps19_education == "Some university" ~ 5,
    cps19_education == "Bachelor's degree" ~ 6,
    cps19_education == "Master's degree" ~ 7,
    cps19_education == "Professional degree or doctorate" ~ 7
  )) %>% 
  mutate(vote_liberal = case_when(
    cps19_votechoice == "Liberal Party" ~ 1,
    cps19_votechoice != "Liberal Party" ~ 0
  )) %>% 
  mutate(vote_conserv = case_when(
    cps19_votechoice == "Conservative Party" ~ 1,
    cps19_votechoice != "Conservative Party" ~ 0
  )) %>% 
  mutate(vote_ndp = case_when(
    cps19_votechoice == "New Democratic" ~ 1,
    cps19_votechoice != "New Democratic" ~ 0
  )) %>% 
  mutate(vote_bq = case_when(
    cps19_votechoice == "Bloc Quebecois" ~ 1,
    cps19_votechoice != "Bloc Quebecois" ~ 0
  )) %>% 
  mutate(vote_green = case_when(
    cps19_votechoice == "Green Party" ~ 1,
    cps19_votechoice != "Green Party" ~ 0
  )) %>% 
  mutate(vote_people = case_when(
    cps19_votechoice == "People's Party" ~ 1,
    cps19_votechoice != "People's Party" ~ 0
  )) %>% 
  mutate(vote_other = case_when(
    cps19_votechoice == "Another party (please specify)" ~ 1,
    cps19_votechoice != "Another party (please specify)" ~ 0
  )) %>% 
  mutate(income_level = case_when(
    cps19_income_number < 25000 ~ "Less than $25,000",
    cps19_income_number < 50000 ~ "$25,000 to $49,999",
    cps19_income_number < 75000 ~ "$50,000 to $74,999",
    cps19_income_number < 100000 ~ "$75,000 to $99,999",
    cps19_income_number < 125000 ~ "$100,000 to $124,999",
    cps19_income_number > 125000 ~ "$125,000 or more"
  )) %>% 
  mutate(income = case_when(
    cps19_income_number < 25000 ~ 0,
    cps19_income_number < 50000 ~ 1,
    cps19_income_number < 75000 ~ 2,
    cps19_income_number < 100000 ~ 3,
    cps19_income_number < 125000 ~ 4,
    cps19_income_number > 125000 ~ 5
  )) %>%
  rename(votechoice = cps19_votechoice) %>% 
  select(age_group, sex, income_level, province, edu, income, age_level, votechoice, vote_liberal, vote_conserv, vote_ndp, vote_bq, vote_green, vote_people, vote_other) %>% 
  filter(province != "Northwest Territories" & province !=  "Nunavut" & province !=  "Yukon")

survey_data$income_level <- as.factor(survey_data$income_level)
survey_data$income_level <- factor(survey_data$income_level, levels(survey_data$income_level)[c(6, 3, 4, 5, 1, 2)])
survey_data$age_group <- as.factor(survey_data$age_group)

# clean census data 
census_data_labels <- as_tibble(str_split(census_data_labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

census_data_labels <- census_data_labels %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

add_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

notes <- census_data_labels %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>%
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)

notes <- 
  notes %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

# writing data into data frame
census_data <- census_data_raw %>% 
  select(agegr10, sex, ttlincg2, prv, ehg3_01) %>% 
  mutate_at(.vars = vars(agegr10:ehg3_01),
            .funs = funs(eval(parse(text = notes %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull())))) %>% 
  clean_names() %>% 
  rename(age_group = agegr10,
         income_level = ttlincg2,
         province = prv,
         education_level = ehg3_01
  ) %>% 
  mutate_at(vars(age_group:education_level), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated"|.=="Don't know", "NA", .))) %>% 
  mutate(edu = case_when(
    education_level == "NA" ~ 0,
    education_level == "Less than high school diploma or its equivalent" ~ 1,
    education_level == "High school diploma or a high school equivalency certificate" ~ 2,
    education_level == "Trade certificate or diploma" ~ 3,
    education_level == "College/CEGEP/other non-university certificate or diploma" ~ 4,
    education_level == "University certificate or diploma below the bachelor's level" ~ 5,
    education_level == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ 6,
    education_level == "University certificate, diploma, degree above the BA level" ~ 7
  )) %>% 
  mutate(income = case_when(
    income_level == "Less than $25,000" ~ 0,
    income_level == "$25,000 to $49,999" ~ 1,
    income_level == "$50,000 to $74,999" ~ 2,
    income_level == "$75,000 to $99,999" ~ 3,
    income_level == "$100,000 to $124,999" ~ 4,
    income_level == "$125,000 or more" ~ 5
  )) %>%
  mutate(age_level = case_when(
    age_group == "15 to 24 years" ~ 0,
    age_group == "25 to 34 years" ~ 1,
    age_group == "35 to 44 years" ~ 2,
    age_group == "45 to 54 years" ~ 3,
    age_group == "55 to 64 years" ~ 4,
    age_group == "65 to 74 years" ~ 5,
    age_group == "75 years and over" ~ 6
  )) %>% 
  select(-education_level)

census_data$province[census_data$province == "Newfoundland and Labroador"] <- "Newfoundland and Labrador"
census_data$age_group <- as.factor(census_data$age_group)

kable(head(survey_data), caption = 'The Information of part of Respondents')
```
