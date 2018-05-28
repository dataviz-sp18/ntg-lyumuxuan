library(broom)
library(stringr)
library(modelr)
library(forcats)
library(ggmap)
library(ggthemes)
library(tidyverse)
library(shiny)
library(leaflet)
library(plotly)

#tidy data
map_world <- map_data("world") %>%
  as.tibble() %>%
  select(-4,-6)

all_df <- read.csv("all_lifeexp.csv", check.names=FALSE)
names(all_df)[c(1)] = c("region")
all_df <- all_df %>% mutate(type = "all")

all_final <- all_df %>%
  gather('1960':'2016', key = "Year", value = "Life Expectancy") %>%
  filter(!is.na(`Life Expectancy`)) %>%
  mutate(levels = cut(`Life Expectancy`, breaks = seq(50, 90, by = 5), 
                      labels = c("50-54", "55-59",
                                 "60-64", "65-99",
                                 "70-74", "75-79",
                                 "80-84", "85-89")))

female_df <- read.csv("female.csv", check.names=FALSE)
names(female_df )[c(1)] = c("region")
female_df  <- female_df  %>% 
  mutate(type = "Female") %>% 
  select(-c("2017")) %>%
  gather('1960':'2016', key = "Year", value = "Life Expectancy") %>%
  filter(!is.na(`Life Expectancy`)) %>%
  mutate(levels = cut(`Life Expectancy`, breaks = seq(50, 90, by = 5), 
                      labels = c("50-54", "55-59",
                                 "60-64", "65-99",
                                 "70-74", "75-79",
                                 "80-84", "85-89")))

male_df <- read.csv("male.csv", check.names=FALSE)
names(male_df )[c(1)] = c("region")
male_df  <- male_df  %>% 
  mutate(type = "Male") %>% 
  select(-c("2017")) %>%
  gather('1960':'2016', key = "Year", value = "Life Expectancy") %>%
  filter(!is.na(`Life Expectancy`)) %>%
  mutate(levels = cut(`Life Expectancy`, breaks = seq(50, 90, by = 5), 
                      labels = c("50-54", "55-59",
                                 "60-64", "65-99",
                                 "70-74", "75-79",
                                 "80-84", "85-89")))

#2016 data
final_2016 <- all_final %>% filter(Year == 2016)
male_2016 <- male_df %>% filter(Year == 2016)
female_2016 <- female_df %>% filter(Year == 2016)
gd2016_df <- rbind(female_2016, male_2016)


plotMap1 <- final_2016 %>%
  ggplot(aes(map_id = region)) +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_map(aes(fill = levels), map = map_world) +
  expand_limits(x = map_world$long, y = map_world$lat) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Life expectancy around the world in 2016",
       subtitle = "",
       fill = NULL) +
  ggthemes::theme_map() +
  coord_map(projection = "mollweide", xlim = c(-180, 180)) +
  theme(title=element_text(size=15,face="bold"))


plotBar1 <- gd2016_df %>%
  ggplot(aes(type, `Life Expectancy`/245)) +
  geom_col(fill = "skyblue") +
  theme_bw() +
  labs(title = "Average Life expectancy for females and males",
       x = "2016",
       y = "Age") +
  theme(title=element_text(size=15,face="bold"))

#2010 data
final_2010 <- all_final %>% filter(Year == 2010)
male_2010 <- male_df %>% filter(Year == 2010)
female_2010 <- female_df %>% filter(Year == 2010)
gd2010_df <- rbind(female_2010, male_2010)


plotMap2 <- final_2010 %>%
  ggplot(aes(map_id = region)) +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_map(aes(fill = levels), map = map_world) +
  expand_limits(x = map_world$long, y = map_world$lat) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Life expectancy around the world in 2010",
       subtitle = "",
       fill = NULL) +
  ggthemes::theme_map() +
  coord_map(projection = "mollweide", xlim = c(-180, 180)) +
  theme(title=element_text(size=15,face="bold"))


plotBar2 <- gd2010_df %>%
  ggplot(aes(type, `Life Expectancy`/245)) +
  geom_col(fill = "skyblue") +
  theme_bw() +
  labs(title = "Average Life expectancy for females and males",
       x = "2010",
       y = "Age") +
  theme(title=element_text(size=15,face="bold"))


#2005 data
final_2005 <- all_final %>% filter(Year == 2005)
male_2005 <- male_df %>% filter(Year == 2005)
female_2005 <- female_df %>% filter(Year == 2005)
gd2005_df <- rbind(female_2005, male_2005)


plotMap3 <- final_2005 %>%
  ggplot(aes(map_id = region)) +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_map(aes(fill = levels), map = map_world) +
  expand_limits(x = map_world$long, y = map_world$lat) +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Life expectancy around the world in 2005",
       subtitle = "",
       fill = NULL) +
  ggthemes::theme_map() +
  coord_map(projection = "mollweide", xlim = c(-180, 180)) +
  theme(title=element_text(size=15,face="bold"))


plotBar3 <- gd2005_df %>%
  ggplot(aes(type, `Life Expectancy`/245)) +
  geom_col(fill = "skyblue") +
  theme_bw() +
  labs(title = "Average Life expectancy for females and males",
       x = "2005",
       y = "Age") +
  theme(title=element_text(size=15,face="bold"))



#for click event, locating which country, reference from: http://playshiny.com/2016/10/17/shiny-click-on-a-map/
which_country <- function(mapData, long, lat) {

  # calculate the difference in long and lat of the border with respect to this point
  mapData$long_diff <- mapData$long - long
  mapData$lat_diff <- mapData$lat - lat
  
  # only compare borders near the clicked point to save computing time
  mapData <- mapData[abs(mapData$long_diff) < 20 & abs(mapData$lat_diff) < 15, ]
  
  # calculate the angle between the vector from this clicked point to border and c(1, 0)
  vLong <- mapData$long_diff
  vLat <- mapData$lat_diff
  mapData$angle <- acos(vLong / sqrt(vLong^2 + vLat^2))
  
  # calculate range of the angle and select the state with largest range
  rangeAngle <- tapply(mapData$angle, mapData$region, function(x) max(x) - min(x))
  return(names(sort(rangeAngle, decreasing = TRUE))[1])
}




ui <- navbarPage("My App (click on a sepcific country to reveal the gender difference)",
           tabPanel("Year 2016", fluidRow(column(6, plotOutput("map1", click = "clickMap1")),
                                         column(6, plotOutput("bar1")))),
           tabPanel("Year 2010", fluidRow(column(6, plotOutput("map2", click = "clickMap2")),
                                          column(6, plotOutput("bar2")))),
           tabPanel("Year 2005", fluidRow(column(6, plotOutput("map3", click = "clickMap3")),
                                          column(6, plotOutput("bar3")))))
  

server <- function(input, output, session) {

#2016 plot
  output$map1 <- renderPlot({
    plotMap1
    
  })
  output$bar1 <- renderPlot({
    plotBar1
  })
  
  observeEvent(input$clickMap1, {
    xClick <- input$clickMap1$x
    yClick <- input$clickMap1$y
    country <- which_country(map_world, xClick, yClick)
    
    output$map1 <- renderPlot(
      plotMap1 + 
        geom_polygon(data = map_world[map_world$region == country,], 
                     aes(x = long, y = lat, group = group),
                        fill = "yellow") +
        annotate("text", x = xClick, y = yClick, label = country, color = "black", size = 7)
    )
    
    output$bar1 <- renderPlot({
      
      plotBar1 +
        geom_col(data = gd2016_df[gd2016_df$region == country,], 
                 aes(type, `Life Expectancy`, fill = "red")) +
        scale_fill_discrete("Life Expectancy in the selected country",
                            labels = c("")) +
        labs(title = "Life expectancy for females and males in selected country compared to mean")
        
    })
  })
  
#2010 plot 
  
  output$map2 <- renderPlot({
    plotMap2
    
  })
  output$bar2 <- renderPlot({
    plotBar2
  })
  
  observeEvent(input$clickMap2, {
    xClick <- input$clickMap2$x
    yClick <- input$clickMap2$y
    country <- which_country(map_world, xClick, yClick)
    
    output$map2 <- renderPlot(
      plotMap2 + 
        geom_polygon(data = map_world[map_world$region == country,], 
                     aes(x = long, y = lat, group = group),
                     fill = "yellow") +
        annotate("text", x = xClick, y = yClick, label = country, color = "black", size = 7)
    )
    
    output$bar2 <- renderPlot({
      
      plotBar2 +
        geom_col(data = gd2010_df[gd2010_df$region == country,], 
                 aes(type, `Life Expectancy`, fill = "red")) +
        scale_fill_discrete("Life Expectancy in the selected country",
                            labels = c("")) +
        labs(title = "Life expectancy for females and males in selected country compared to mean")
    })
  })
  

#2005 plot
  output$map3 <- renderPlot({
    plotMap3
    
  })
  output$bar3 <- renderPlot({
    plotBar3
  })
  
  observeEvent(input$clickMap3, {
    xClick <- input$clickMap3$x
    yClick <- input$clickMap3$y
    country <- which_country(map_world, xClick, yClick)
    
    output$map3 <- renderPlot(
      plotMap3 + 
        geom_polygon(data = map_world[map_world$region == country,], 
                     aes(x = long, y = lat, group = group),
                     fill = "yellow") +
        annotate("text", x = xClick, y = yClick, label = country, color = "black", size = 7)
    )
    
    output$bar3 <- renderPlot({
      
      plotBar3 +
        geom_col(data = gd2005_df[gd2005_df$region == country,], 
                 aes(type, `Life Expectancy`, fill = "red")) +
        scale_fill_discrete("Life Expectancy in the selected country",
                            labels = c("")) +
        labs(title = "Life expectancy for females and males in selected country compared to mean")
    })
  })
  
}



shinyApp(ui, server)