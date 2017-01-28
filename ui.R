library(shiny)
library(highcharter)
library(leaflet)
require(ggthemes)
require(tidyr)
require(dplyr)
require(ggplot2)

d <- read.csv(file = "cities_r2.csv")
df <-separate(data = d, col = location, into = c("lat","lng"),sep = ",")

df.city <- df
df.state <- df.city %>% 
        group_by(state_name) %>% 
        summarize(Total = n(), 
                  population_total = sum(population_total), 
                  population_male = sum(population_male), 
                  population_female = sum(population_female),
                  Male_Percent = population_male/population_total * 100, 
                  Female_Percent = population_female/population_total * 100,
                  total_graduates = sum(total_graduates), 
                  male_graduates = sum(male_graduates), 
                  female_graduates = sum(female_graduates),
                  Grads_percent = total_graduates/population_total * 100, 
                  Male_Grads_Percent = male_graduates/population_male * 100, 
                  Female_Grads_Percent = female_graduates/population_female * 100,
                  X0.6_population_total = sum(X0.6_population_total),
                  X0.6_population_male = sum(X0.6_population_male),
                  X0.6_population_female = sum(X0.6_population_female),
                  literates_total = sum(literates_total),
                  literates_male = sum(literates_male),
                  literates_female = sum(literates_female),
                  sex_ratio = mean(sex_ratio),
                  child_sex_ratio = mean(child_sex_ratio),
                  lat = mean(as.numeric(lat)),
                  lng = mean(as.numeric(lng)),
                  effective_literacy_rate_total = mean(effective_literacy_rate_total),
                  effective_literacy_rate_male = mean(effective_literacy_rate_male),
                  effective_literacy_rate_female = mean(effective_literacy_rate_female)
        )

df.city <- mutate(df.city,
                  Male_Percent = population_male/population_total * 100, 
                  Female_Percent = population_female/population_total * 100,
                  Grads_percent = total_graduates/population_total * 100, 
                  Male_Grads_Percent = male_graduates/population_male * 100, 
                  Female_Grads_Percent = female_graduates/population_female * 100
)



list_select <- c("Population" = 1, "Male Population" = 2, "Female Population" = 3, 
                 "Male Percentage" = 4, "Female Percentage" = 5, "Graduates" = 6, 
                 "Male Graduates" = 7, "Female Graduates" = 8, "Graduates Percentage" = 9, 
                 "Male Graduates Percentage" = 10, "Female Graduates Percentage" = 11,
                 "Total Literates" = 12, "Literates_Male" =13, "Literates_Female" = 14,
                 "Sex_Ratio" = 15, "Child_Sex_Ratio" = 16, "Effective_Literacy_Rate_total" = 17,
                 "Effective_Literacy_Rate_Male" = 18, "Effective_Literacy_Rate_Female" = 19)

var <- c(
        "Total Population" = "tp",
        "Male Population" = "mp",
        "Female Populaion" = "fp",
        "Total Population Age(0-6)"= "tc",
        "Male Population Age(0-6)" = "mc",
        "Female Population Age(0-6)"="fc",
        "Total Graduates" = "TG",
        "Male Graduates" = "MG",
        "Female Graduates" = "FG"
)


fluidPage(theme ='styles.css',
        navbarPage(title = "Exploratory Data Analysis of Indian Cities",
                   tabPanel(title="Introduction",
                                 sidebarLayout(sidebarPanel(
                                         h3(" Columns in the Dataset"),
                                         includeHTML("include.html")
                                 ),
                                 mainPanel( h3("Dataset Description"),includeHTML("include1.html")))
                                 
                            ),
                   tabPanel(title="Maps", sidebarLayout(
                           sidebarPanel(
                                   radioButtons(inputId = "state",label="",choices = c("State"=1,"City"=2)),p(),
                                   selectInput("selectMap", label ="Select Statistic", 
                                                    choices = var, 
                                                    selected = 1),p(),checkboxInput("night","Night Mode",FALSE)),
                           mainPanel(leafletOutput("mymap",height = 700))
                   )),
                   tabPanel(title = "Plots",
                            sidebarLayout(sidebarPanel( selectInput(inputId = "select", label = h3("Select Statistic"), 
                                                                    choices = list_select, 
                                                                    selected = 1),
                                                        radioButtons(inputId = "radio", label = h3("Select"),
                                                                     choices = list("State" = 1, "City" = 2), 
                                                                     selected = 1),
                                                        numericInput("num", label = h3("Top"), value = 20),
                                                        actionButton("goButton","Plot")),
                                          mainPanel(highchartOutput(outputId = "plot1",height = 700)
                                          ))),
                   tabPanel(title = "TreeMap",
                            sidebarLayout(sidebarPanel( selectInput(inputId = "select2", label = h3("Select Statistic"), 
                                                                    choices = list_select, 
                                                                    selected = 1),
                                                        radioButtons(inputId = "radio2", label = h3("Select"),
                                                                     choices = list("State" = 1, "City" = 2), 
                                                                     selected = 1),
                                                        numericInput("num2", label = h3("Top"), value = 20),
                                                        actionButton("goButton2","Plot")),
                                          mainPanel(highchartOutput(outputId = "treeMap",height = 700)
                                          ))),
                   tabPanel(title = "Data Grid",tabsetPanel(tabPanel(title = "States",
                                                                     sidebarLayout(
                                                                             sidebarPanel( checkboxGroupInput(inputId ='show_vars1', 'Columns in dataset to show:',
                                                                                                              names(df.state), selected = names(df.city))),
                                                                             mainPanel(dataTableOutput('mytable1'))
                                                                     )   
                                                                     ),tabPanel(title = "Cities"),
                                                            sidebarLayout(
                                                                    sidebarPanel( checkboxGroupInput(inputId ='show_vars2', 'Columns in dataset to show:',
                                                                                                     names(df.city), selected = names(df.city))),
                                                                    mainPanel(dataTableOutput('mytable2'))
                                                            ))
                         
)
)
)


