library(bs4Dash)
library(reactable)
library(shinydashboard)
library(leaflet)
library(webr)
library(dplyr)
# importing the library
library("devtools")
devtools::install_github("arthur-shaw/susoapi")
library("susoapi")
library("tidyverse")
library("haven")
#library("plyr")

library(DBI)
library(RMySQL)
library(ggplot2)
# creating a database connection
connectDB <- function(){
  connection <- dbConnect(RMySQL::MySQL(), 
                          dbname = Sys.getenv("MYSQL_SURVEY_DB"), 
                          host = Sys.getenv("MYSQL_HOST"), 
                          port = 3306, 
                          user = Sys.getenv("MYSQL_USER"), 
                          password = Sys.getenv("MYSQL_PWD"))
  return(connection)
}

fetchWholeTable <- function(table){
  d2 <- connectDB()
  
  data <- dbReadTable(d2, table)
  
  dbDisconnect(d2)
  
  data
}

fetchQuery <- function(q){
  d2 <- connectDB()
  
  data <- dbGetQuery(d2, q)
  
  dbDisconnect(d2)
  
  data
}


set_credentials(
  server = "http://hqsurveys.sib.org.bz",
  workspace = "hbs",
  user = "gapi",
  password = "API4statistics!"
)


consumption <- fetchWholeTable('hbs_consumption_pattern_roster')


households <- fetchWholeTable('hbs_household')


listing <- fetchWholeTable('hbs_listing_roster')


hh_val <- 
  households |>
  group_by(urban_rural) |>
  summarise(
    `Response %` = paste0(50, '%'),
    `Complete %` = paste0(45, '%'),
    Completed = sum(FINALRESULT_hh %in% 1, na.rm = T),
    Partial = sum(FINALRESULT_hh %in% 2, na.rm = T),
    `Vacant Dwelling` = sum(FINALRESULT_hh %in% 3, na.rm = T),
    Refusal = sum(FINALRESULT_hh %in% 4, na.rm = T),
    `A.N.F` = sum(FINALRESULT_hh %in% 5, na.rm = T),
    `N.S.R` = sum(FINALRESULT_hh %in% 6, na.rm = T),
    `No Contact` = sum(FINALRESULT_hh %in% 7, na.rm = T),
    `Vacant Lot` = sum(FINALRESULT_hh %in% 8, na.rm = T),
    `U.C/N.L` = sum(FINALRESULT_hh %in% 9, na.rm = T),
    Other = sum(FINALRESULT_hh %in% 10, na.rm = T)
  )


# query <- "show tables";
# result <- dbGetQuery(connection,query);
# print(result)

# dbGetQuery(connection, "SHOW GLOBAL VARIABLES LIKE 'default_storage_engine'")
# tibble::as_tibble(dbGetQuery(connection, "SHOW ENGINES"))
# Change storage engine
# dbExecute(connection, "SET SESSION storage_engine=myisam")

# belize_hbs_4 <- read_sav('./SPSS_FOLDER/belize_hbs_4.sav', user_na = TRUE);

household <- "SELECT * FROM hbs_household";
HHresults <- dbGetQuery(connection, household);

listingroster <- "SELECT * FROM hbs_listing_roster";
LRresults <- dbGetQuery(connection, listingroster);

# National Tab

malecount <- "SELECT COUNT(*) AS sum FROM hbs_listing_roster WHERE sex = 1";
avgmale <- dbGetQuery(connection, malecount);

femalecount <- "SELECT COUNT(*) AS sum FROM hbs_listing_roster WHERE sex = 2";
avgfemale <- dbGetQuery(connection, femalecount);

questnum <- nrow(HHresults);

nationaldatatable <- data.frame(
  Questionnaires = questnum,
  Respondent_HHS = 10,
  Avg_Males = avgmale[1,1],
  Avg_Females = avgfemale[1,1],
  Avg_HH_Size = avgmale[1,1] + avgfemale[1,1]
)


# Urban/Rural Tab


Umalecount <- "SELECT COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.urban_rural = 1
                JOIN hbs_household b ON b.interview__id = l.interview__id";
Uavgmale <- dbGetQuery(connection, Umalecount);

Rmalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.urban_rural = 2
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Ravgmale <- dbGetQuery(connection, Rmalecount);


Ufemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.urban_rural = 1
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Uavgfemale <- dbGetQuery(connection, Ufemalecount);

Rfemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.urban_rural = 2
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Ravgfemale <- dbGetQuery(connection, Rfemalecount);


RUquestnum <- belize_hbs_4 |> group_by(urban_rural) |> summarise(n = n())

rural_urban <- c("rural", "urban");

URdatatable <- data.frame(
  rural_urban,
  Questionnaires = c(RUquestnum[1,2], 30),
  Respondent_HHS = c(14, 10),
  Avg_Males = c(Ravgmale[1,1], Uavgmale[1,1]),
  Avg_Females = c(Ravgfemale[1,1], Uavgfemale[1,1]),
  Avg_HH_Size = c(Ravgmale[1,1] + Ravgfemale[1,1], Uavgmale[1,1] + Uavgfemale[1,1])
)

# colnames(URdatatable) <- c('Rural_Urban', 'Questionnaires','Respondent_HHS','Avg_Males', 'Avg_Females', 'Avg_HH_Size')



# District Tab


Belizemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.district = 3
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Belizeavgmale <- dbGetQuery(connection, Umalecount);

Cayomalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.district = 4
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Cayoavgmale <- dbGetQuery(connection, Rmalecount);

Corozalmalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.district = 1
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Corozalavgmale <- dbGetQuery(connection, Umalecount);

OWmalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.district = 2
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
OWavgmale <- dbGetQuery(connection, Rmalecount);

SCmalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.district = 5
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
SCavgmale <- dbGetQuery(connection, Umalecount);

Toledomalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 1 AND b.district = 6
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Toledoavgmale <- dbGetQuery(connection, Rmalecount);


Belizefemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.district = 3
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Belizeavgfemale <- dbGetQuery(connection, Umalecount);

Cayofemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.district = 4
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Cayoavgfemale <- dbGetQuery(connection, Rmalecount);

Corozalfemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.district = 1
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Corozalavgfemale <- dbGetQuery(connection, Umalecount);

OWfemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.district = 2
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
OWavgfemale <- dbGetQuery(connection, Rmalecount);

SCfemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.district = 5
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
SCavgfemale <- dbGetQuery(connection, Umalecount);

Toledofemalecount <- "SELECT l.COUNT(*) AS sum 
                FROM hbs_listing_roster WHERE l.sex = 2 AND b.district = 6
                JOIN hbs_belize_4 b ON b.interview__id = l.interview__id";
Toledoavgfemale <- dbGetQuery(connection, Rmalecount);




Dquestnum <- belize_hbs_4 |> group_by(district) |> summarise(n = n())
# PD <- data |> group_by(Class, Survived) |> summarise(n = sum(Freq));
# Rquestnum <- belize_hbs_4 |> group_by(urban_rural) |> nrow()

district_list <- c("Belize", "Cayo", "Corozal", "Orange Walk", "Stann Creek", "Toledo");

URdatatable <- data.frame(
  district_list,
  Questionnaires = c(Dquestnum[1,2], 30, 12, 41, 122, 12),
  Respondent_HHS = c(14, 10, 12, 34, 4, 11),
  Avg_Males = c(Belizeavgmale[1,1], Cayoavgmale[1,1], Corozalavgmale[1,1], 
                OWavgmale, SCavgmale, Toledoavgmale),
  Avg_Females = c(Belizeavgfemale[1,1], Cayoavgfemale[1,1], Corozalavgfemale[1,1], 
                  OWavgfemale, SCavgfemale, Toledoavgfemale),
  Avg_HH_Size = c(Belizeavgmale[1,1] + Belizeavgfemale[1,1],
                  Cayoavgmale[1,1] + Cayoavgfemale[1,1],
                  Corozalavgmale[1,1] + Corozalavgfemale[1,1],
                  OWavgmale[1,1] + OWavgfemale[1,1],
                  SCavgmale[1,1] + SCavgfemale[1,1],
                  Toledoavgmale[1,1] + Toledoavgfemale[1,1])
)




# loading the dataset
#data <- as.data.frame(Titanic);
# Building a table with the data for the plot
PD <- result7 |> group_by(sex) |> summarise(sum(as.integer(as.character(sex))))




get_household_metrics <- function(level){

  # if (level == 'National'){
  #   level_grouped <- 
  #     households 
  # } else if (level %in% c('DISTRICT', 'URBAN_RURAL')) {
  #   level_grouped <- 
  #     households |>
  #     group_by(!!as.name(level))
  # } else {
  #   level_grouped <- 
  #     households |>
  #     group_by(DISTRICT, !!as.name(level))
  # }
  
  #level_summary <- 
    
    level_grouped |> 
    mutate(
      males_hh_members = as.numeric(males_hh_members),
      females_hh_members = as.numeric(females_hh_members),
      total_hh_members = as.numeric(total_hh_members)
    ) |>
    summarise(
      Questionnaires = n(),
      `Respondent HHs` = sum(FINALRESULT %in% c('Completed', 'Partially Completed'), na.rm = T),
      `Avg Males` = mean(males_hh_members, na.rm = T) |> round(1),
      `Avg Females` = mean(females_hh_members, na.rm = T) |> round(1),
      `Avg HH Size` = mean(total_hh_members, na.rm = T) |> round(1)
    )
  
  return(level_summary)
}


listing <- 
  listing |>
  left_join(
    households_vars, by = 'interview__key'
  )

hh_vars <- 
  households |>
  group_by(urban_rural) |>
  summarise(
    Questionnaires = n(),
    Respondents = sum(FINALRESULT_hh %in% c('1', '2'), na.rm = T)
  )

listing_vars <- 
listing |>
  group_by(urban_rural, interview__key) |>
  summarise(
    Males = sum(sex == 1, na.rm = T),
    Females = sum(sex == 2, na.rm = T)
  ) |>
  group_by(urban_rural) |>
  summarise(
    'Avg Males' = mean(Males, na.rm = T),
    `Avg Females` = mean(Females, na.rm = T)
  )


hh_vars |>
  left_join(
    listing_vars,
    by = 'urban_rural'
  )

