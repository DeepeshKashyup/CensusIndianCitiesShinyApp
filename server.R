library(shiny)
library(highcharter)
library(leaflet)
require(ggthemes)
require(tidyr)
require(dplyr)
require(ggplot2)

d <- read.csv(file = "cities_r2.csv")
dff <-separate(data = d, col = location, into = c("lat","lng"),sep = ",")
dff$name_of_city <- trimws(dff$name_of_city)

df.city <- dff
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


function(input, output, session) {
        #data <- reactiveValues({})
        
        
        output$value <- renderPrint({ input$select })
        

        
        output$mymap <- renderLeaflet({
                
                
                if(as.numeric(input$state)==1){
                        df <- df.state
                        df$name_of_city<-df$state_name
                }else{
                        df <- df.city
                }
                
                if(input$selectMap == 'tp')
                {   
                        y <- df$population_total
                        content <- paste("<b>",df$name_of_city,"</b></br>","<b>Total population :</b>",y)
                            radius <- sqrt(df$population_total)*50
                        pal <- colorNumeric(
                                palette = "RdYlBu",
                                domain = df$population_total
                        )
                        stat <- "Population"
                }
                else if(input$selectMap == 'mp')
                {       y <- df$population_male
                        content <- paste("<b>",df$name_of_city,"</b></br>","<b>Male population :</b>",y)
                        radius <- sqrt(df$population_male)*50
                        pal <- colorNumeric(
                                palette = "RdYlBu",
                                domain = df$population_male
                        )
                        
                        stat <- "Male Population"
                }
                else if(input$selectMap == 'fp')
                {       y <- df$population_female
                        content <- paste("<b>",df$name_of_city,"</b></br>","<b>Female population :</b>",y)
                        radius <- sqrt(df$population_female)*50
                        pal <- colorNumeric(
                                palette = "RdYlBu",
                                domain = df$population_female
                        )
                        
                        stat <- "Female Population"
                }
                else if(input$selectMap == 'tc')
                {       y <- df$X0.6_population_total
                content <- paste("<b>",df$name_of_city,"</b></br>","<b>Total Population(0-6) :</b>",y)
                radius <- sqrt(df$X0.6_population_total)*50
                pal <- colorNumeric(
                        palette = "RdYlBu",
                        domain = df$X0.6_population_total
                )
                stat <- "Total Child Population"
                }
                else if(input$selectMap == 'mc')
                {       y <- df$X0.6_population_male
                content <- paste("<b>",df$name_of_city,"</b></br>","<b>Total Male Population(0-6) :</b>",y)
                radius <- sqrt(y)*50
                pal <- colorNumeric(
                        palette = "RdYlBu",
                        domain = y
                )
                
                stat <- "Male Child Population"
                }
                else if(input$selectMap == 'fc')
                {       y <- df$X0.6_population_female
                content <- paste("<b>",df$name_of_city,"</b></br>","<b>Total Female Population(0-6)  :</b>",y)
                radius <- sqrt(y)*50
                pal <- colorNumeric(
                        palette = "RdYlBu",
                        domain = y
                )
                stat <- "Female Child Population"
                }
                else if(input$selectMap == 'TG')
                {       y <- df$total_graduates
                content <- paste("<b>",df$name_of_city,"</b></br>","<b>Total Graduates :</b>",y)
                radius <- sqrt(y)*50
                pal <- colorNumeric(
                        palette = "RdYlBu",
                        domain = y
                )
                stat <- "Total Graduates"
                }
                else if(input$selectMap == 'MG')
                {       y <- df$male_graduates
                        content <- paste("<b>",df$name_of_city,"</b></br>","<b>Male Graduates :</b>",y)
                        radius <- sqrt(y)*50
                        pal <- colorNumeric(
                        palette = "RdYlBu",
                        domain = y
                )
                        stat <- "Total Male Graduates"
                }
                else if(input$selectMap == 'FG')
                {       y <- df$female_graduates
                        content <- paste("<b>",df$name_of_city,"</b></br>","<b>Female Graduates :</b>",y)
                        radius <- sqrt(y)*50
                        pal <- colorNumeric(
                        palette = "RdYlBu",
                        domain = y
                )
                        stat <- "Total Female Graduates"
                }
               
                
                m <- leaflet(data =df ,height = 400) %>%setView(lng= 78.0419,lat = 17.1750,zoom=5)%>%
                        addTiles()%>%
                        addCircles(~lng,~lat,weight =1,radius = radius,color=pal(y),popup=content)%>%
                        addLegend(pal =pal,values = y,title =stat , opacity =1,position="bottomright")

                if(input$night == TRUE){
                        m <- m%>%addProviderTiles("CartoDB.DarkMatterNoLabels",options = providerTileOptions(noWrap=TRUE))
                }
                
                m
        })
        
        
        
        
        
        output$plot1 <- renderHighchart({
                input$goButton
                n<-isolate(input$num)
                
                if(as.numeric(input$radio) == 1){
                        if(input$select == 1){
                                
                                
                                da <- df.state %>%
                                        arrange(desc(population_total))
                                
                                dp <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dp$state_name)%>%
                                        hc_add_series(dp, type = "column",
                                                      hcaes(x = state_name, y = population_total, color = state_name))%>%
                                        hc_title(text = "States by Population")%>%
                                        hc_yAxis(title = list(text = "Population"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        else if(input$select ==2){
                             
                                da <- df.state %>%
                                        arrange(desc(population_male))
                                
                                dm <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dm$state_name)%>%
                                        hc_add_series(dm, type = "column",
                                                      hcaes(x = state_name, y = population_male, color = state_name))%>%
                                        hc_title(text = "States by Male Population")%>%
                                        hc_yAxis(title = list(text = "Male Population"))%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select ==3){
                                
                                
                                da <- df.state  %>%
                                        arrange(desc(population_female))
                                
                                de <- da[1:n,]
                                
                                
                                highchart() %>% hc_xAxis(categories = de$state_name)%>%
                                        hc_add_series(de, type = "column",
                                                      hcaes(x = state_name, y = population_female, color = state_name))%>%
                                        hc_title(text = "States by Female Population")%>%
                                        hc_yAxis(title = list(text = "Female Population"))%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select ==4){
                                da <- df.state  %>%
                                        arrange(desc(Male_Percent))
                                
                                dmp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dmp$state_name)%>%
                                        hc_add_series(dmp, type = "column",
                                                      hcaes(x = state_name, y = Male_Percent, color = state_name))%>%
                                        hc_title(text = "States by Males Percentage")%>%
                                        hc_yAxis(title = list(text = "Males Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==5){
                                da <- df.state  %>%
                                        arrange(desc(Female_Percent))
                                
                                dfp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dfp$state_name)%>%
                                        hc_add_series(dfp, type = "column",
                                                      hcaes(x = state_name, y = Female_Percent, color = state_name))%>%
                                        hc_title(text = "States by Females Percentage")%>%
                                        hc_yAxis(title = list(text = "Females Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==6){
                                da <- df.state  %>%
                                        arrange(desc(total_graduates))
                                
                                dg <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dg$state_name)%>%
                                        hc_add_series(dg, type = "column",
                                                      hcaes(x = state_name, y = total_graduates, color = state_name))%>%
                                        hc_title(text = "States by Number Of Graduates")%>%
                                        hc_yAxis(title = list(text = "Total Graduates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==7){
                                da <- df.state  %>%
                                        arrange(desc(male_graduates))
                                
                                dmg <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dmg$state_name)%>%
                                        hc_add_series(dmg, type = "column",
                                                      hcaes(x = state_name, y = male_graduates, color = state_name))%>%
                                        hc_title(text = "States by Male Graduates")%>%
                                        hc_yAxis(title = list(text = "Male Graduates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==8){
                                da <- df.state  %>%
                                        arrange(desc(female_graduates))
                                
                                dfg <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dfg$state_name)%>%
                                        hc_add_series(dfg, type = "column",
                                                      hcaes(x = state_name, y = female_graduates, color = state_name))%>%
                                        hc_title(text = "States by Female Population")%>%
                                        hc_yAxis(title = list(text = "Female Graduates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==9){
                                da <- df.state  %>%
                                        arrange(desc(Grads_percent))
                                
                                dgp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dgp$state_name)%>%
                                        hc_add_series(dgp, type = "column",
                                                      hcaes(x = state_name, y = Grads_percent, color = state_name))%>%
                                        hc_title(text = "States by Graduates Percentage")%>%
                                        hc_yAxis(title = list(text = "Graduates Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==10){
                                da <- df.state  %>%
                                        arrange(desc(Male_Grads_Percent))
                                
                                dmgp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dmgp$state_name)%>%
                                        hc_add_series(dmgp, type = "column",
                                                      hcaes(x = state_name, y = Male_Grads_Percent, color = state_name))%>%
                                        hc_title(text = "States by Male Graduates Percentage")%>%
                                        hc_yAxis(title = list(text = "Males Graduates Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==11){
                                da <- df.state  %>%
                                        arrange(desc(Female_Grads_Percent))
                                
                                dfgp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dfgp$state_name)%>%
                                        hc_add_series(dfgp, type = "column",
                                                      hcaes(x = state_name, y = Female_Grads_Percent, color = state_name))%>%
                                        hc_title(text = "States by Female Graduates Percentage")%>%
                                        hc_yAxis(title = list(text = "Females Graduates Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==12){
                                da <- df.state  %>%
                                        arrange(desc(literates_total))
                                
                                dl <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dl$state_name)%>%
                                        hc_add_series(dl, type = "column",
                                                      hcaes(x = state_name, y = literates_total, color = state_name))%>%
                                        hc_title(text = "States by Total Literates")%>%
                                        hc_yAxis(title = list(text = "Total Literates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==13){
                                da <- df.state  %>%
                                        arrange(desc(literates_male))
                                
                                dlm <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dlm$state_name)%>%
                                        hc_add_series(dlm, type = "column",
                                                      hcaes(x = state_name, y = literates_male, color = state_name))%>%
                                        hc_title(text = "States by Male Literates")%>%
                                        hc_yAxis(title = list(text = "Male Literates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==14){
                                da <- df.state  %>%
                                        arrange(desc(literates_female))
                                
                                dlf <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dlf$state_name)%>%
                                        hc_add_series(dlf, type = "column",
                                                      hcaes(x = state_name, y = literates_female, color = state_name))%>%
                                        hc_title(text = "States by Female Literates")%>%
                                        hc_yAxis(title = list(text = "Female Literates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==15){
                                da <- df.state  %>%
                                        arrange(desc(sex_ratio))
                                
                                ds <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = ds$state_name)%>%
                                        hc_add_series(ds, type = "column",
                                                      hcaes(x = state_name, y = sex_ratio, color = state_name))%>%
                                        hc_title(text = "States by Sex Ratio")%>%
                                        hc_yAxis(title = list(text = "Sex_Ratio"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==16){
                                da <- df.state  %>%
                                        arrange(desc(child_sex_ratio))
                                
                                dcs <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dcs$state_name)%>%
                                        hc_add_series(dcs, type = "column",
                                                      hcaes(x = state_name, y = child_sex_ratio, color = state_name))%>%
                                        hc_title(text = "States by Child Sex Ratio")%>%
                                        hc_yAxis(title = list(text = "Child_Sex_Ratio"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==17){
                                da <- df.state  %>%
                                        arrange(desc(effective_literacy_rate_total))
                                
                                del <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = del$state_name)%>%
                                        hc_add_series(del, type = "column",
                                                      hcaes(x = state_name, y = effective_literacy_rate_total, color = state_name))%>%
                                        hc_title(text = "States by Total Effective Literacy Rate")%>%
                                        hc_yAxis(title = list(text = "Effective_Literacy_Rate_total"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==18){
                                da <- df.state  %>%
                                        arrange(desc(effective_literacy_rate_male))
                                
                                delm <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = delm$state_name)%>%
                                        hc_add_series(delm, type = "column",
                                                      hcaes(x = state_name, y = effective_literacy_rate_male, color = state_name))%>%
                                        hc_title(text = "States by Total Effective Male Literacy Rate")%>%
                                        hc_yAxis(title = list(text = "Effective_Literacy_Rate_Male"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else {
                                da <- df.state  %>%
                                        arrange(desc(effective_literacy_rate_female))
                                
                                delf <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = delf$state_name)%>%
                                        hc_add_series(delf, type = "column",
                                                      hcaes(x = state_name, y = effective_literacy_rate_female, color = state_name))%>%
                                        hc_title(text = "States by Total Effective Female Literacy Rate")%>%
                                        hc_yAxis(title = list(text = "Effective_Literacy_Rate_Female"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        
                }else{
                        if(input$select == 1){
                             
                                
                                da <- df.city%>%
                                        arrange(desc(population_total))
                                
                                dp <- da[1:n,]
                                
                                
                                
                                highchart() %>% 
                                        hc_xAxis(categories = dp$name_of_city)%>%
                                        hc_add_series(dp, type = "column",
                                                      hcaes(x = name_of_city, y = population_total, color = name_of_city))%>%
                                        hc_title(text = "Cities by Population")%>%
                                        hc_yAxis(title = list(text = "Population"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        else if(input$select ==2){
                              
                                
                                da <- df.city %>%
                                        arrange(desc(population_male))
                                
                                dm <- da[1:n,]
                                
                                highchart() %>% 
                                        hc_xAxis(categories = dm$name_of_city)%>%
                                        hc_add_series(dm, type = "column",
                                                      hcaes(x = name_of_city, y = population_male, color = name_of_city))%>%
                                        hc_title(text = "Cities by Male Population")%>%
                                        hc_yAxis(title = list(text = "Male Population"))%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select ==3){
                                
                                
                                da <- df.city  %>%
                                        arrange(desc(population_female))
                                
                                de <- da[1:n,]
                                
                                
                                highchart() %>% 
                                        hc_xAxis(categories = de$name_of_city)%>%
                                        hc_add_series(de, type = "column",
                                                      hcaes(x = name_of_city, y = population_female, color = name_of_city))%>%
                                        hc_title(text = "Citites by Female Population")%>%
                                        hc_yAxis(title = list(text = "Female Population"))%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select ==4){
                                da <- df.city  %>%
                                        arrange(desc(Male_Percent))
                                
                                dmp <- da[1:n,]
                                
                                highchart() %>% 
                                        hc_xAxis(categories = dmp$name_of_city)%>%
                                        hc_add_series(dmp, type = "column",
                                                      hcaes(x = name_of_city, y = Male_Percent, color = name_of_city))%>%
                                        hc_title(text = "Cities by Males Percentage")%>%
                                        hc_yAxis(title = list(text = "Males Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==5){
                                da <- df.city  %>%
                                        arrange(desc(Female_Percent))
                                
                                dfp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dfp$name_of_city)%>%
                                        hc_add_series(dfp, type = "column",
                                                      hcaes(x = name_of_city, y = Female_Percent, color = name_of_city))%>%
                                        hc_title(text = "Cities by Females Percentage")%>%
                                        hc_yAxis(title = list(text = "Females Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==6){
                                da <- df.city  %>%
                                        arrange(desc(total_graduates))
                                
                                dg <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dg$name_of_city)%>%
                                        hc_add_series(dg, type = "column",
                                                      hcaes(x = name_of_city, y = total_graduates, color = name_of_city))%>%
                                        hc_title(text = "Cities by Number Of Graduates")%>%
                                        hc_yAxis(title = list(text = "Total Graduates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==7){
                                da <- df.city  %>%
                                        arrange(desc(male_graduates))
                                
                                dmg <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dmg$name_of_city)%>%
                                        hc_add_series(dmg, type = "column",
                                                      hcaes(x = name_of_city, y = male_graduates, color = name_of_city))%>%
                                        hc_title(text = "Cities by Male Graduates")%>%
                                        hc_yAxis(title = list(text = "Male Graduates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==8){
                                da <- df.city  %>%
                                        arrange(desc(female_graduates))
                                
                                dfg <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dfg$name_of_city)%>%
                                        hc_add_series(dfg, type = "column",
                                                      hcaes(x = name_of_city, y = female_graduates, color = name_of_city))%>%
                                        hc_title(text = "Cities by Female Population")%>%
                                        hc_yAxis(title = list(text = "Female Graduates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==9){
                                da <- df.city  %>%
                                        arrange(desc(Grads_percent))
                                
                                dgp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dgp$name_of_city)%>%
                                        hc_add_series(dgp, type = "column",
                                                      hcaes(x = name_of_city, y = Grads_percent, color = name_of_city))%>%
                                        hc_title(text = "Cities by Graduates Percentage")%>%
                                        hc_yAxis(title = list(text = "Graduates Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==10){
                                da <- df.city  %>%
                                        arrange(desc(Male_Grads_Percent))
                                
                                dmgp <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dmgp$name_of_city)%>%
                                        hc_add_series(dmgp, type = "column",
                                                      hcaes(x = name_of_city, y = Male_Grads_Percent, color = name_of_city))%>%
                                        hc_title(text = "Cities by Male Graduates Percentage")%>%
                                        hc_yAxis(title = list(text = "Males Graduates Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==11){
                                da <- df.city  %>%
                                        arrange(desc(Female_Grads_Percent))
                                
                                dfgp <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dfgp$name_of_city)%>%
                                        hc_add_series(dfgp, type = "column",
                                                      hcaes(x = name_of_city, y = Female_Grads_Percent, color = name_of_city))%>%
                                        hc_title(text = "Cities by Female Graduates Percentage")%>%
                                        hc_yAxis(title = list(text = "Females Graduates Percentage"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==12){
                                da <- df.city  %>%
                                        arrange(desc(literates_total))
                                
                                dl <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dl$name_of_city)%>%
                                        hc_add_series(dl, type = "column",
                                                      hcaes(x = name_of_city, y = literates_total, color = name_of_city))%>%
                                        hc_title(text = "Cities by Total Literates")%>%
                                        hc_yAxis(title = list(text = "Total Literates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==13){
                                da <- df.city  %>%
                                        arrange(desc(literates_male))
                                
                                dlm <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dlm$name_of_city)%>%
                                        hc_add_series(dlm, type = "column",
                                                      hcaes(x = name_of_city, y = literates_male, color = name_of_city))%>%
                                        hc_title(text = "Cities by Male Literates")%>%
                                        hc_yAxis(title = list(text = "Male Literates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==14){
                                da <- df.city  %>%
                                        arrange(desc(literates_female))
                                
                                dlf <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = dlf$name_of_city)%>%
                                        hc_add_series(dlf, type = "column",
                                                      hcaes(x = name_of_city, y = literates_female, color = name_of_city))%>%
                                        hc_title(text = "Cities by Female Literates")%>%
                                        hc_yAxis(title = list(text = "Female Literates"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==15){
                                da <- df.city  %>%
                                        arrange(desc(sex_ratio))
                                
                                ds <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = ds$name_of_city)%>%
                                        hc_add_series(ds, type = "column",
                                                      hcaes(x = name_of_city, y = sex_ratio, color = name_of_city))%>%
                                        hc_title(text = "Cities by Sex Ratio")%>%
                                        hc_yAxis(title = list(text = "Sex_Ratio"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==16){
                                da <- df.city  %>%
                                        arrange(desc(child_sex_ratio))
                                
                                dcs <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = dcs$name_of_city)%>%
                                        hc_add_series(dcs, type = "column",
                                                      hcaes(x = name_of_city, y = child_sex_ratio, color = name_of_city))%>%
                                        hc_title(text = "Cities by Child Sex Ratio")%>%
                                        hc_yAxis(title = list(text = "Child_Sex_Ratio"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==17){
                                da <- df.city  %>%
                                        arrange(desc(effective_literacy_rate_total))
                                
                                del <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = del$name_of_city)%>%
                                        hc_add_series(del, type = "column",
                                                      hcaes(x = name_of_city, y = effective_literacy_rate_total, color = name_of_city))%>%
                                        hc_title(text = "Cities by Total Effective Literacy Rate")%>%
                                        hc_yAxis(title = list(text = "Effective_Literacy_Rate_total"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select ==18){
                                da <- df.city  %>%
                                        arrange(desc(effective_literacy_rate_male))
                                
                                delm <- da[1:n,]
                                
                                highchart() %>% hc_xAxis(categories = delm$name_of_city)%>%
                                        hc_add_series(delm, type = "column",
                                                      hcaes(x = name_of_city, y = effective_literacy_rate_male, color = name_of_city))%>%
                                        hc_title(text = "Cities by Total Effective Male Literacy Rate")%>%
                                        hc_yAxis(title = list(text = "Effective_Literacy_Rate_Male"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else {
                                da <- df.city  %>%
                                        arrange(desc(effective_literacy_rate_female))
                                
                                delf <- da[1:n,]
                                
                                highchart() %>%hc_xAxis(categories = delf$name_of_city)%>%
                                        hc_add_series(delf, type = "column",
                                                      hcaes(x = name_of_city, y = effective_literacy_rate_female, color = name_of_city))%>%
                                        hc_title(text = "Cities by Total Effective Female Literacy Rate")%>%
                                        hc_yAxis(title = list(text = "Effective_Literacy_Rate_Female"))%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                }
                
                
                
                
                
                
        })
        
        
        
        output$treeMap <- renderHighchart({
                input$goButton2
                n<-isolate(input$num2)
                
                if(as.numeric(input$radio2) == 1){
                        if(input$select2 == 1){
                                
                                
                                da <- df.state %>%
                                        arrange(desc(population_total))
                                
                                dp <- da[1:n,]
                                
                                hchart(dp, type = "treemap",
                                       hcaes(x = state_name, value = population_total, color = population_total))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Population") %>%
                                        hc_add_theme(hc_theme_google())
                                
                                
                                
                        }
                        else if(input$select2 ==2){
                              
                                
                                da <- df.state %>%
                                        arrange(desc(population_male))
                                
                                dm <- da[1:n,]
                                
                                hchart(dm, type = "treemap",
                                       hcaes(x = state_name, value = population_male, color = population_male))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Male Population")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select2 ==3){
                                
                                
                                da <- df.state  %>%
                                        arrange(desc(population_female))
                                
                                de <- da[1:n,]
                                
                                hchart(de, type = "treemap",
                                       hcaes(x = state_name, value = population_female, color = population_female))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Female Population")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select2 ==4){
                                da <- df.state  %>%
                                        arrange(desc(Male_Percent))
                                
                                dmp <- da[1:n,]
                             
                                hchart(dmp, type = "treemap",
                                       hcaes(x = state_name, value = Male_Percent, color = Male_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Males Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==5){
                                da <- df.state  %>%
                                        arrange(desc(Female_Percent))
                                
                                dfp <- da[1:n,]
                                
                                hchart(dfp, type = "treemap",
                                       hcaes(x = state_name, value = Female_Percent, color = Female_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Females Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                                
                        }
                        
                        else if(input$select2 ==6){
                                da <- df.state  %>%
                                        arrange(desc(total_graduates))
                                
                                dg <- da[1:n,]
                                
                                hchart(dg, type = "treemap",
                                       hcaes(x = state_name, value = total_graduates, color = total_graduates))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Number Of Graduates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==7){
                                da <- df.state  %>%
                                        arrange(desc(male_graduates))
                                
                                dmg <- da[1:n,]
                               
                                hchart(dmg, type = "treemap",
                                       hcaes(x = state_name, value = male_graduates, color = male_graduates))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Number Of Male Graduates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==8){
                                da <- df.state  %>%
                                        arrange(desc(female_graduates))
                                
                                dfg <- da[1:n,]
                                
                                hchart(dfg, type = "treemap",
                                       hcaes(x = state_name, value = female_graduates, color = female_graduates))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Number Of Female Graduates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==9){
                                da <- df.state  %>%
                                        arrange(desc(Grads_percent))
                                
                                dgp <- da[1:n,]
                             
                                hchart(dgp, type = "treemap",
                                       hcaes(x = state_name, value = Grads_percent, color = Grads_percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Graduates Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        
                        else if(input$select2 ==10){
                                da <- df.state  %>%
                                        arrange(desc(Male_Grads_Percent))
                                
                                dmgp <- da[1:n,]
                            
                                hchart(dmgp, type = "treemap",
                                       hcaes(x = state_name, value = Male_Grads_Percent, color = Male_Grads_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Male Graduates Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==11){
                                da <- df.state  %>%
                                        arrange(desc(Female_Grads_Percent))
                                
                                dfgp <- da[1:n,]
                                
                                hchart(dfgp, type = "treemap",
                                       hcaes(x = state_name, value = Female_Grads_Percent, color = Female_Grads_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Female Graduates Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==12){
                                da <- df.state  %>%
                                        arrange(desc(literates_total))
                                
                                dl <- da[1:n,]
                                hchart(dl, type = "treemap",
                                       hcaes(x = state_name, value = literates_total, color = literates_total))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Total Literates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==13){
                                da <- df.state  %>%
                                        arrange(desc(literates_male))
                                
                                dlm <- da[1:n,]
                                
                                
                                hchart(dlm, type = "treemap",
                                       hcaes(x = state_name, value = literates_male, color = literates_male))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Number of Male Literates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==14){
                                da <- df.state  %>%
                                        arrange(desc(literates_female))
                                
                                dlf <- da[1:n,]
                               
                                hchart(dlf, type = "treemap",
                                       hcaes(x = state_name, value = literates_female, color = literates_female))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Number of Female Literates")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        
                        else if(input$select2 ==15){
                                da <- df.state  %>%
                                        arrange(desc(sex_ratio))
                                
                                ds <- da[1:n,]
                               hchart(ds, type = "treemap",
                                       hcaes(x = state_name, value = sex_ratio, color = sex_ratio))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Sex Ratio")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==16){
                                da <- df.state  %>%
                                        arrange(desc(child_sex_ratio))
                                
                                dcs <- da[1:n,]
                                hchart(dcs, type = "treemap",
                                       hcaes(x = state_name, value = child_sex_ratio, color = child_sex_ratio))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Child Sex Ratio")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==17){
                                da <- df.state  %>%
                                        arrange(desc(effective_literacy_rate_total))
                                
                                del <- da[1:n,]
                                
                                hchart(del, type = "treemap",
                                       hcaes(x = state_name, value = effective_literacy_rate_total, color = effective_literacy_rate_total))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Total Effective Literacy Rate")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==18){
                                da <- df.state  %>%
                                        arrange(desc(effective_literacy_rate_male))
                                
                                delm <- da[1:n,]
                                
                                hchart(delm, type = "treemap",
                                       hcaes(x = state_name, value = effective_literacy_rate_male, color = effective_literacy_rate_male))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Total Effective Male Literacy Rate")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else {
                                da <- df.state  %>%
                                        arrange(desc(effective_literacy_rate_female))
                                
                                delf <- da[1:n,]
                            hchart(delf, type = "treemap",
                                       hcaes(x = state_name, value = effective_literacy_rate_female, color = effective_literacy_rate_female))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "States by Total Effective Female Literacy Rate")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        
                }else{
                        if(input$select2 == 1){
                              
                                da <- df.city%>%
                                        arrange(desc(population_total))
                                
                                dp <- da[1:n,]
                                
                    
                                
                                hchart(dp, type = "treemap",
                                       hcaes(x = name_of_city, value = population_total, color = population_total))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Population") %>%
                                        hc_add_theme(hc_theme_google())
                                
                                
                        }
                        else if(input$select2 ==2){
                                # dm <- df %>% 
                                # group_by(state_name) %>% 
                                # summarise(Male_Population = sum(population_male)) %>% 
                                # arrange(desc(Male_Population))
                                
                                da <- df.city %>%
                                        arrange(desc(population_male))
                                
                                dm <- da[1:n,]
                                
                                # highchart() %>% 
                                #         hc_xAxis(categories = dm$name_of_city)%>%
                                #         hc_add_series(dm, type = "column",
                                #                       hcaes(x = name_of_city, y = population_male, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Male Population")%>%
                                #         hc_yAxis(title = list(text = "Male Population"))%>%
                                #         hc_add_theme(hc_theme_google())
                                
                                hchart(dm, type = "treemap",
                                       hcaes(x = name_of_city, value = population_male, color = population_male))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Male Population")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select2 ==3){
                                
                                
                                da <- df.city  %>%
                                        arrange(desc(population_female))
                                
                                de <- da[1:n,]
                                
                                # 
                                # highchart() %>% 
                                #         hc_xAxis(categories = de$name_of_city)%>%
                                #         hc_add_series(de, type = "column",
                                #                       hcaes(x = name_of_city, y = population_female, color = name_of_city))%>%
                                #         hc_title(text = "Citites by Female Population")%>%
                                #         hc_yAxis(title = list(text = "Female Population"))%>%
                                #         hc_add_theme(hc_theme_google())
                                
                                hchart(de, type = "treemap",
                                       hcaes(x = name_of_city, value = population_female, color = population_female))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Female Population")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        else if(input$select2 ==4){
                                da <- df.city  %>%
                                        arrange(desc(Male_Percent))
                                
                                dmp <- da[1:n,]
                                # 
                                # highchart() %>% 
                                #         hc_xAxis(categories = dmp$name_of_city)%>%
                                #         hc_add_series(dmp, type = "column",
                                #                       hcaes(x = name_of_city, y = Male_Percent, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Males Percentage")%>%
                                #         hc_yAxis(title = list(text = "Males Percentage"))%>%
                                #         hc_add_theme(hc_theme_google())
                                
                                
                                hchart(dmp, type = "treemap",
                                       hcaes(x = name_of_city, value = Male_Percent, color = Male_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Males Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        
                        else if(input$select2 ==5){
                                da <- df.city  %>%
                                        arrange(desc(Female_Percent))
                                
                                dfp <- da[1:n,]
                                # 
                                # highchart() %>% hc_xAxis(categories = dfp$name_of_city)%>%
                                #         hc_add_series(dfp, type = "column",
                                #                       hcaes(x = name_of_city, y = Female_Percent, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Females Percentage")%>%
                                #         hc_yAxis(title = list(text = "Females Percentage"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                
                                hchart(dfp, type = "treemap",
                                       hcaes(x = name_of_city, value = Female_Percent, color = Female_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Females Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==6){
                                da <- df.city  %>%
                                        arrange(desc(total_graduates))
                                
                                dg <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = dg$name_of_city)%>%
                                #         hc_add_series(dg, type = "column",
                                #                       hcaes(x = name_of_city, y = total_graduates, color = name_of_city))%>%
                                #         hc_title(text = "Cities by num2ber Of Graduates")%>%
                                #         hc_yAxis(title = list(text = "Total Graduates"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(dg, type = "treemap",
                                       hcaes(x = name_of_city, value = total_graduates, color = total_graduates))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Number Of Graduates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==7){
                                da <- df.city  %>%
                                        arrange(desc(male_graduates))
                                
                                dmg <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = dmg$name_of_city)%>%
                                #         hc_add_series(dmg, type = "column",
                                #                       hcaes(x = name_of_city, y = male_graduates, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Male Graduates")%>%
                                #         hc_yAxis(title = list(text = "Male Graduates"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(dmg, type = "treemap",
                                       hcaes(x = name_of_city, value = male_graduates, color = male_graduates))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Number Of Male Graduates")%>%
                                        hc_add_theme(hc_theme_google())
                        }
                        
                        else if(input$select2 ==8){
                                da <- df.city  %>%
                                        arrange(desc(female_graduates))
                                
                                dfg <- da[1:n,]
                                # 
                                # highchart() %>% hc_xAxis(categories = dfg$name_of_city)%>%
                                #         hc_add_series(dfg, type = "column",
                                #                       hcaes(x = name_of_city, y = female_graduates, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Female Population")%>%
                                #         hc_yAxis(title = list(text = "Female Graduates"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(dfg, type = "treemap",
                                       hcaes(x = name_of_city, value = female_graduates, color = female_graduates))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Number Of Female Graduates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==9){
                                da <- df.city  %>%
                                        arrange(desc(Grads_percent))
                                
                                dgp <- da[1:n,]
                                
                                # highchart() %>% hc_xAxis(categories = dgp$name_of_city)%>%
                                #         hc_add_series(dgp, type = "column",
                                #                       hcaes(x = name_of_city, y = Grads_percent, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Graduates Percentage")%>%
                                #         hc_yAxis(title = list(text = "Graduates Percentage"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(dgp, type = "treemap",
                                       hcaes(x = name_of_city, value = Grads_percent, color = Grads_percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Graduates Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==10){
                                da <- df.city  %>%
                                        arrange(desc(Male_Grads_Percent))
                                
                                dmgp <- da[1:n,]
                                # 
                                # highchart() %>% hc_xAxis(categories = dmgp$name_of_city)%>%
                                #         hc_add_series(dmgp, type = "column",
                                #                       hcaes(x = name_of_city, y = Male_Grads_Percent, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Male Graduates Percentage")%>%
                                #         hc_yAxis(title = list(text = "Males Graduates Percentage"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                
                                hchart(dmgp, type = "treemap",
                                       hcaes(x = name_of_city, value = Male_Grads_Percent, color = Male_Grads_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Male Graduates Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==11){
                                da <- df.city  %>%
                                        arrange(desc(Female_Grads_Percent))
                                
                                dfgp <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = dfgp$name_of_city)%>%
                                #         hc_add_series(dfgp, type = "column",
                                #                       hcaes(x = name_of_city, y = Female_Grads_Percent, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Female Graduates Percentage")%>%
                                #         hc_yAxis(title = list(text = "Females Graduates Percentage"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                
                                hchart(dfgp, type = "treemap",
                                       hcaes(x = name_of_city, value = Female_Grads_Percent, color = Female_Grads_Percent))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Female Graduates Percentage")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==12){
                                da <- df.city  %>%
                                        arrange(desc(literates_total))
                                
                                dl <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = dl$name_of_city)%>%
                                #         hc_add_series(dl, type = "column",
                                #                       hcaes(x = name_of_city, y = literates_total, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Total Literates")%>%
                                #         hc_yAxis(title = list(text = "Total Literates"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(dl, type = "treemap",
                                       hcaes(x = name_of_city, value = literates_total, color = literates_total))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Total Literates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==13){
                                da <- df.city  %>%
                                        arrange(desc(literates_male))
                                
                                dlm <- da[1:n,]
                                
                                hchart(dlm, type = "treemap",
                                       hcaes(x = name_of_city, value = literates_male, color = literates_male))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Number of Male Literates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==14){
                                da <- df.city  %>%
                                        arrange(desc(literates_female))
                                
                                dlf <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = dlf$name_of_city)%>%
                                #         hc_add_series(dlf, type = "column",
                                #                       hcaes(x = name_of_city, y = literates_female, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Female Literates")%>%
                                #         hc_yAxis(title = list(text = "Female Literates"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(dlf, type = "treemap",
                                       hcaes(x = name_of_city, value = literates_female, color = literates_female))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Number of Female Literates")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==15){
                                da <- df.city  %>%
                                        arrange(desc(sex_ratio))
                                
                                ds <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = ds$name_of_city)%>%
                                #         hc_add_series(ds, type = "column",
                                #                       hcaes(x = name_of_city, y = sex_ratio, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Sex Ratio")%>%
                                #         hc_yAxis(title = list(text = "Sex_Ratio"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                hchart(ds, type = "treemap",
                                       hcaes(x = name_of_city, value = sex_ratio, color = sex_ratio))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Sex Ratio")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==16){
                                da <- df.city  %>%
                                        arrange(desc(child_sex_ratio))
                                
                                dcs <- da[1:n,]
                                
                                # highchart() %>% hc_xAxis(categories = dcs$name_of_city)%>%
                                #         hc_add_series(dcs, type = "column",
                                #                       hcaes(x = name_of_city, y = child_sex_ratio, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Child Sex Ratio")%>%
                                #         hc_yAxis(title = list(text = "Child_Sex_Ratio"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                hchart(dcs, type = "treemap",
                                       hcaes(x = name_of_city, value = child_sex_ratio, color = child_sex_ratio))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Child Sex Ratio")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==17){
                                da <- df.city  %>%
                                        arrange(desc(effective_literacy_rate_total))
                                
                                del <- da[1:n,]
                                
                                # highchart() %>% hc_xAxis(categories = del$name_of_city)%>%
                                #         hc_add_series(del, type = "column",
                                #                       hcaes(x = name_of_city, y = effective_literacy_rate_total, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Total Effective Literacy Rate")%>%
                                #         hc_yAxis(title = list(text = "Effective_Literacy_Rate_total"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                hchart(del, type = "treemap",
                                       hcaes(x = name_of_city, value = effective_literacy_rate_total, color = effective_literacy_rate_total))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Total Effective Literacy Rate")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else if(input$select2 ==18){
                                da <- df.city  %>%
                                        arrange(desc(effective_literacy_rate_male))
                                
                                delm <- da[1:n,]
                                
                                # highchart() %>% hc_xAxis(categories = delm$name_of_city)%>%
                                #         hc_add_series(delm, type = "column",
                                #                       hcaes(x = name_of_city, y = effective_literacy_rate_male, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Total Effective Male Literacy Rate")%>%
                                #         hc_yAxis(title = list(text = "Effective_Literacy_Rate_Male"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                hchart(delm, type = "treemap",
                                       hcaes(x = name_of_city, value = effective_literacy_rate_male, color = effective_literacy_rate_male))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Total Effective Male Literacy Rate")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                        
                        else {
                                da <- df.city  %>%
                                        arrange(desc(effective_literacy_rate_female))
                                
                                delf <- da[1:n,]
                                
                                # highchart() %>%hc_xAxis(categories = delf$name_of_city)%>%
                                #         hc_add_series(delf, type = "column",
                                #                       hcaes(x = name_of_city, y = effective_literacy_rate_female, color = name_of_city))%>%
                                #         hc_title(text = "Cities by Total Effective Female Literacy Rate")%>%
                                #         hc_yAxis(title = list(text = "Effective_Literacy_Rate_Female"))%>%
                                #         hc_add_theme(hc_theme_google())
                                # 
                                
                                hchart(delf, type = "treemap",
                                       hcaes(x = name_of_city, value = effective_literacy_rate_female, color = effective_literacy_rate_female))  %>%
                                        hc_credits(enabled = TRUE, text = "Sources: Census India 2011", style = list(fontSize = "10px")) %>%
                                        hc_title(text = "Cities by Total Effective Female Literacy Rate")%>%
                                        hc_add_theme(hc_theme_google())
                                
                        }
                }
                
                
                
        })
        output$mytable1 <- renderDataTable({df.state[,input$show_vars1,drop=FALSE]})
        
        output$mytable2 <- renderDataTable({df.city[,input$show_vars2,drop=FALSE]})
}