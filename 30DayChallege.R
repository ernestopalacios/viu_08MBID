#Google Sheets

install.packages('googlesheets4')
install.packages('gsheet')
install.packages('tidyverse')
install.packages('modeldata')
install.packages('scales')

library(gsheet)
library(googlesheets4)


url <- "https://docs.google.com/spreadsheets/d/1-Y7VLIWI4jLWabU4OGpT7kdSe1bN2eSKZg7MX5nEf8s/edit?usp=sharing"
eerssa <- gsheet2tbl( url )



eerssa$categoria <- ifelse( eerssa$PerdidasSistemaPercent < 10, "Bajo",
                            ifelse(eerssa$PerdidasSistemaPercent < 20, "Medio","Alto"))

eerssa$categoria <- as.factor(eerssa$categoria)

eerssa$categoria <- factor( eerssa$categoria, levels = c("Bajo", "Medio", "Alto") )

str(eerssa)

library(tidyverse)
library(modeldata)

############################
# 03 - Make Over

# GGPLOT Box Plot

ggplot( eerssa, aes( x = categoria,
                     y = PerdidasSistemaPercent, fill = categoria)) +
  geom_boxplot( width = 0.5, alpha = 0.8)+
  geom_jitter( width = 0.2)+
  labs( x = " ",
        y = "Porcentaje de pérdidas de energía",
        title = "Pérdidas de energía eléctrica en los sistemas de distribución",
        subtitle = "Empresas eléctricas distribuidoras",
        caption = "Fuente: ARCERNNR Atlas del Sector eléctrico ecuatoriano 2023, pág 79",
        tag = " ")+
  
  theme_classic()+
  theme(plot.title = element_text(size = 16))+
  
  annotate("text",
           label = "0 - 10%\nHay 10 distribuidoras",
           x = 1,
           y = 13) +
  
  annotate("text",
           label = "10 - 20%\nHay 5 distribuidoras",
           x = 2,
           y = 22) + 

  annotate("text",
           label = "20 - 30%\nHay 5 distribuidoras",
           x = 3,
           y = 30) 

############################
# 04 - Waffle

install.packages("ggpol")
library(ggpol)


bt <- data.frame(
  parties = factor(c("CDU", "CSU", "AfD", "FDP", "SPD", 
                     "Linke", "Gruene", "Fraktionslos"),
                   levels = c("CDU", "CSU", "AfD", "FDP", "SPD", 
                              "Linke", "Gruene", "Fraktionslos")),
  seats   = c(200, 46, 92, 80, 153, 69, 67, 2),
  colors  = c("black", "blue", "lightblue", "yellow", 
              "red","purple", "green", "grey"),
  stringsAsFactors = FALSE)

ggplot(bt) + 
  geom_parliament(aes(seats = seats, fill = parties), color = "black") + 
  scale_fill_manual(values = bt$colors, labels = bt$parties) +
  coord_fixed() + 
  theme_void()


############################
# 05 - Divergent
# https://rfortherestofus.com/2021/10/diverging-bar-chart/

install.packages('readxl')
install.packages('writexl')

library(readxl)

# Load data from local Excel File
file_name = '2023_CALIFICACION_ACTIVIDADES_R.xlsx'
hojas <- excel_sheets(path = file_name)
hojas <- hojas[-1]  # First sheet does not contain useful info
print(hojas)

n <- length(hojas)
datalist = list()

for (i in 1:n){
  datalist[[i]] <- read_excel( file_name , sheet = hojas[[i]])
}
df_2023 = do.call(rbind, datalist)
rm(datalist)

# Elimintar los casos en los que la columna 'evento' este vacia.
df_2023 <- df_2023[complete.cases(df_2023[ , 'EVENTO']),]


# Elapsed time in minutes for each observation (activity)
# Ref: https://stackoverflow.com/questions/69527810/calculate-time-difference-between-posixct-values

library(dplyr)
library(hms)


df_2023 <- df_2023 %>%
  mutate( MINUTOS = as.numeric(difftime( df_2023$`FECHA FIN`, df_2023$`FECHA INICIO`,units = "mins")))


writexl::write_xlsx( df_2023, "df_2023.xlsx" )


# Delete observations without 'Alimentador'
# https://www.statology.org/not-in-r/
actividades2023 <- subset(df_2023, !(ALIMENTADOR %in% c('·', NA )))
str(actividades2023)


# Filter observations, only GEOPE WorkGroups are allowed
cuadrillas_Mtto <- unique(actividades2023$CUADRILLA)
cuadrillas_Mtto <- cuadrillas_Mtto[- c(1,8,10,11,12)]

geope_2023 <- subset( actividades2023, actividades2023$CUADRILLA %in% cuadrillas_Mtto  )

#example
library(scales)

# sumary of the data
ae1 <- geope_2023 %>%
  filter( !is.na(ALIMENTADOR)) %>%
  filter( !is.na(CUENTA)) %>%
  group_by( ALIMENTADOR, CUENTA) %>%
  count( name = "tiempo", wt = MINUTOS ) %>%
  filter( tiempo > 150 ) %>%
  group_by(ALIMENTADOR) %>%
  mutate( percent_time = tiempo / sum(tiempo) ) %>%
  ungroup() %>%
  mutate( percent_number = percent( percent_time,accuracy = 1 ))
view(ae1)

# Basic Plot - no diverging
ae1 %>%
  ggplot(aes(x = ALIMENTADOR,
             y = percent_time,
             fill = CUENTA)) +
  geom_col() +
  geom_text( aes(label = percent_number ),
             position = position_stack( vjust = 0.5),
             color = "white",
             fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d() + 
  labs( title = "Distribucion de tiempo en Alimentadores",
        x = NULL,
        fill = NULL) +
  theme_minimal() + 
  theme( axis.text.x = element_blank(),
         axis,title = element_blank(),
         panel.grid = element_blank(),
         legend.position = "top")

# Positive and negative for Diverging
ae1_diverging <- ae1 %>%
  mutate( percent_time = if_else( CUENTA %in% c("511.04.001","511.03.003"), percent_time, -percent_time )) %>%
  mutate( percent_number = percent(percent_time, accuracy = 1) )
view(ae1_diverging)


# Basic Diverging with negative numbers
ae1_diverging %>%
  ggplot(aes(x = ALIMENTADOR,
             y = percent_time,
             fill = CUENTA)) +
  geom_col() +
  geom_text( aes(label = percent_number ),
             position = position_stack( vjust = 0.5),
             color = "white",
             fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  scale_fill_viridis_d() + 
  labs( title = "Distribucion de tiempo en Alimentadores",
        x = NULL,
        fill = NULL) +
  theme_minimal() + 
  theme( axis.text.x = element_blank(),
         axis,title = element_blank(),
         panel.grid = element_blank(),
         legend.position = "top")

ae1_good_labels <- ae1_diverging %>%
  mutate(percent_number = abs( percent_time) ) %>%
  mutate(percent_number = percent(percent_number, accuracy = 1))
view(ae1_good_labels)

# Basic Diverging with negative numbers
ae1_good_labels %>%
  ggplot(aes(x = ALIMENTADOR,
             y = percent_time,
             fill = CUENTA)) +
  geom_col() +
  geom_text( aes(label = percent_number ),
             position = position_stack( vjust = 0.5),
             color = "white",
             fontface = "bold") +
  coord_flip() +
  scale_x_discrete() +
  
  scale_fill_manual( breaks = c("511.04.001","511.04.002","511.03.003",
                                "511.05.001","511.05.002","521.01.001"),
                     values = c(
                       "511.04.001" = "blue3",
                       "511.04.002" = "green3",
                       "511.03.003" = "darkblue",
                       "511.05.001" = "red3",
                       "511.05.002" = "darkred",
                       "521.01.001" = "orange"
                     ))+
  
  labs( title = "Distribucion de tiempo en Alimentadores") +
  theme_minimal() + 
  theme( axis.text.x = element_blank(),
         axis,title = element_blank(),
         panel.grid = element_blank(),
         legend.position = "top")

# 6  OECD - Japan Population
japan_population <- c(104.665,106.100,107.595,109.104,110.573,111.940,113.094,114.165,115.190,116.155,117.060,117.902,118.728,119.536,120.305,121.049,121.660,122.239,122.745,123.205,123.611,124.101,124.567,124.938,125.265,125.570,125.864,126.166,126.486,126.686,126.926,127.291,127.435,127.619,127.687,127.768,127.901,128.033,128.084,128.032,128.057,127.834,127.593,127.414,127.237,127.095,127.042,126.919,126.749,126.555,126.146,125.502,124.947)
year <- c(1970:2022)

japan_population_millions <- data.frame(year,japan_population)

library(ggplot2)
library(scales)
#install.packages("ggpmisc")
library(ggpmisc)

ggplot(japan_population_millions, aes( x = year, y = japan_population))+
  geom_area( fill = "pink2", alpha = 0.5 ) +
  coord_cartesian(ylim = c(80,140)) +
  geom_line( color = "red4", size = 1) +
  geom_vline(xintercept = 2010,
             linetype = 2, color = 1, linewidth = 0.5)+
  annotate( "text", x = 2005, y = 131, label = "128M en el 2010" )+
  annotate( "pointrange", x = 2010, y = 128, colour = "red",
            xmin = 2010, xmax = 2010, linewidth = 0.5) +
  labs( x = " Años ", y = "Población (millones)" ) +
  scale_y_continuous(labels = label_dollar(suffix = "M", prefix = "")) +
  theme_minimal()+
  labs( title = " Población de Japon desde 1970 hasta 2022 \n",
           caption = "Fuente: OECD (2024), Population (indicator)",
        )

############
#07 & 08  Hazards Circular - 
# data form: https://www.esfi.org/electrical-fatalities-in-the-workplace-2011-2022/

  