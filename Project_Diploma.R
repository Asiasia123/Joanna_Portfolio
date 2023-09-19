#---- DNA info ----

## Diploma project
## Title: Solar panels - energy production and usage analysis 
## Author: Joanna Wiśniewska
## Tutor: Krzysztof Ziółkowski
## WSB Merito University
## Gdańsk, June 2023

## ---- Instruction ----
### to start, click ctr+A (to mark all text in this box)
### and then click button "Run" (right upper corner of this box)

#---- Libraries used in project ----
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(datasets)
library(stringr)
library(lubridate)
library(DT)
library(plotly)


#----Data collection----
getwd()

solar_data <- read_excel(
  "C:\\Users\\Administrator\\Desktop\\WSB\\seminarium\\praca_dyplomowa\\database\\Podsumowanie uzysku energii 2022.xlsx"
                        )

#----Data wrangling----

## add new column with date (in date format)
solar_data$date <-  as.Date(solar_data$"Czas statystyczny", "%Y-%m-%d")

## choose relevant columns
SD <- data.frame( solar_data[c(7, 10, 12, 15, 16, 17, 23)] )

## change names of columns
colnames(SD) <- c("production", "export", "import", "usage_total", "usage_own", "indicator_u_o", "measure_date")

## checking NA values
any(is.na(SD)) # result: FALSE (no missing values)

head(SD)
summary(SD)


#----Visualization----

plot(SD)

## show production and export during year (daily)
plot1 <- plot_ly(data = SD, 
                  x = ~measure_date, 
                  y = ~production,
                  name = 'production',
                  type = "bar",
                  color = I('darkblue')
                 ) %>% 
        add_trace(y = ~export, 
                  name = 'export',
                  color = I('lightblue')
                 ) %>% 
        layout(title = "Production and export during year (daily)",
               yaxis = list(title = 'kWh'),
               xaxis = list(title = 'date')
               ) 
plot1

## show usage total (sum of usage own and import) during year (daily)
plot2 <- plot_ly(data = SD, 
                  x = ~measure_date, 
                  y = ~usage_own,
                  name = 'usage own',
                  type = "bar",
                  color = I('lightgreen')
                  ) %>% 
          add_trace(y = ~import, 
                    name = 'import' ,
                    color = I('darkgreen')
                   ) %>% 
          layout(title = 'Usage total (sum of usage own and import) during year (daily)',
                 yaxis = list(title = 'kWh'), 
                 xaxis = list(title = 'date'),
                 barmode = 'stack'
                 )
plot2

## count mean monthly
SD_MM <- SD %>%
  mutate(month_from_date = str_extract(measure_date, "^[0-9]{4}[-][0-9]{2}", 0))  %>%
  group_by(month_from_date) %>%
  summarise(
            mean_production = mean(production), mean_export = mean(export),
            mean_import = mean(import), mean_usage_total = mean(usage_total),
            mean_usage_own = mean(usage_own), mean_indicator_u_o = mean(indicator_u_o)
            )

## show mean for each factor (monthly) 

plot3 <- plot_ly(data = SD_MM, 
                 x = ~month_from_date,
                 y = ~mean_production, 
                 type = 'bar', 
                 name = 'mean production',
                 marker = list(color = 'khaki')
                ) %>% 
        add_trace(y = ~mean_export, 
                  name = 'mean export',
                  marker = list(color = 'springgreen')
                 ) %>% 
        add_trace(y = ~mean_import, 
                 name = 'mean import',
                 marker = list(color = 'darkseagreen')
                 ) %>% 
        add_trace(y = ~mean_usage_total, 
                 name = 'mean usage total',
                 marker = list(color = 'steelblue')
                ) %>%
        add_trace(y = ~mean_usage_own, 
                name = 'mean usage own',
                marker = list(color = 'darkblue')
                ) %>% 
        layout(title = "Mean for each factor (monthly)",
               yaxis = list(title = 'kWh'),
               xaxis = list(title = 'month')
               )
plot3

## show mean production vs. mean usage total (monthly) 

plot4 <- plot_ly(data = SD_MM, 
                  x = ~month_from_date,
                  y = ~mean_production, 
                  type = 'scatter', 
                  mode = 'lines',
                  name = 'mean production' ,
                 color = I('blue')
                ) %>% 
         add_trace(y = ~mean_usage_total, 
                  name = 'mean usage total', 
                  mode = 'lines' ,
                  color = I('green')
                  ) %>% 
        layout(title = 'Mean production vs. mean usage total (monthly)',
               yaxis = list(title = 'kWh'),
               xaxis = list(title = 'month')
               )     
plot4

## find maintenance usage level -> usage_total < 8.2 kWh

SD_sort_UT <- SD[order(SD$usage_total, decreasing  = TRUE), ]

SD_sort_UT$measure_date <- reorder(
                                  SD_sort_UT$measure_date, 
                                  -SD_sort_UT$usage_total
                                  )

plot5 <- plot_ly(data = SD_sort_UT, 
                  x = ~measure_date,
                  y = ~usage_total, 
                  type = 'bar'
                 )%>% 
        layout(title = 'Usage total desceding',
               yaxis = list(title = 'usage total [kWh]'),
               xaxis = list(title = 'date')
               )  
plot5

## show occupied and free days -> 239 occupied and 126 free days

SD_OF <- SD %>%
  mutate(month_from_date = str_extract(measure_date, "^[0-9]{4}[-][0-9]{2}", 0))  %>%
  group_by(month_from_date) %>% 
  summarise(occupied = sum(ifelse(usage_total >= 8.2, 1, 0)))

sum_of_occupied_days <- sum(SD_OF$occupied)
sum_of_occupied_days

sum_of_free_days <- length(SD$measure_date) - sum(SD_OF$occupied)
sum_of_free_days

plot6 <- plot_ly(data = SD_OF, 
                 x = ~month_from_date, 
                 y = ~occupied,
                 type = "bar",
                 marker = list(color = '#68838B')
                ) %>% 
        layout(title = 'Occupied days (monthly)',
               yaxis = list(title = 'occupied days'),
               xaxis = list(title = 'date')
               )
plot6

# show usage total with free days on zero level

SD_OCCUPIED <- mutate(SD, usage_total = ifelse(usage_total < 8.2, 0, usage_total))

summary(SD_OCCUPIED)  

plot7 <- plot_ly(data = SD_OCCUPIED, 
                 x = ~measure_date, 
                 y = ~usage_total,
                 name = 'usage_total',
                 type = "bar",
                 color = I('darkseagreen')
                 ) %>% 
  layout(title = 'Usage total in occupied days',
         yaxis = list(title = 'usage total [kWh]'),
         xaxis = list(title = 'month of a year')
  )
plot7

## show yearly figures (sum of values)

SD_CR <- SD[-6] %>%   # transform columns to rows 
  gather(key = "energy", value = "kWh", -"measure_date")

plot8 <- plot_ly(data = SD_CR,
                 x = ~energy, 
                 y = ~kWh,
                 type = 'bar',
                 color = I('plum4')
                 ) %>% 
        layout(title = 'Yearly figures (sum of values)',
               yaxis = list(title = 'kWh'),
               xaxis = list(title = 'energy distribution')
               )
plot8

## Level of indicator of usage own

SD_IND <- SD %>% 
  mutate (level_indicator_u_o = case_when(
                                          indicator_u_o > 70 ~ "high", 
                                          indicator_u_o < 30 ~ "low", 
                                          TRUE ~ 'medium'
                                          )
          ) %>% 
  count(level_indicator_u_o)
  
plot9 <- plot_ly(SD_IND, 
                 labels = ~level_indicator_u_o,
                 values = ~n, 
                 type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste('level_indicator_u_o', 'n'),
                 marker = list(colors = c("grey", 'lightgrey', 'darkgrey'), 
                               line = list(color = '#FFFFFF', width = 1)
                               ),
                 showlegend = FALSE
                 ) %>% 
         layout(title = 'Level of indicator of usage own',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                )
plot9

## check if production covers usage -> True

If_enough_solar_energy <- sum(SD$production) - sum(SD$usage_total) >= 0
If_enough_solar_energy
