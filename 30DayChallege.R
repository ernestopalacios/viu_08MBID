#Google Sheets
install.packages('ggplot2')
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
setwd("~/GIT/viu_08MBID")

file_name = 'data_xls/2023_CALIFICACION_ACTIVIDADES_R.xlsx'
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

### basic plot ## 
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

######
# Final Graphic
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

#######
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
# 
# Excel


#########
# 09  Major / Minor
#
# Show difference between Corrective vs Preventive activities
# - To Do:  Filter by dia_semana and Time of avtivity

major_minor_activities <- geope_2023 %>% 
  filter( CUENTA == "511.04.001") %>%
  filter( CUADRILLA != "Líneas Energizadas (Cuadrilla  Nro.6)") %>%
  # filter( `FECHA INICIO` > as.Date() )
  filter( TIPO == "CORRECTIVO" | TIPO == "PREVENTIVO") %>%
  group_by( CUADRILLA , TIPO  ) %>%
  count( name = "t_mantenimiento", wt = MINUTOS ) %>%
  mutate( t_mantenimiento = ceiling( t_mantenimiento / 60) )
View(major_minor_activities)

ggplot( major_minor_activities,
        aes( x = CUADRILLA,
             y = t_mantenimiento,
             fill = TIPO)) +
  geom_bar( stat = "identity",
            position = "dodge")+
  theme_minimal()+
  labs( x = "\nCUADRILLAS",
        y = "Tiempo en horas \n",
        title = " Cantidad de horas dedicadas a actividades Correctivas y Preventivas en el 2023 \n",
        caption = "Órdenes de Trabajo diarias (2023)") +
  theme(plot.title = element_text(size = 18))

#######
# 10 - Physical
# Ammount of tools by WorkGroup
# Load data from local Excel File

library(readxl)
library(dplyr)

setwd("~/GIT/viu_08MBID")
file_bienes <- "data_xls/20211129_BIENES_A_CARGO.xlsx"

df_herramientas <- read_excel(file_bienes)
View(df_herramientas)

# Filter columns and personel of interest

herr_cuadrillas <- df_herramientas %>%
  select( responsable, descripcion, cantidad ) %>%
  group_by( responsable ) %>%
  count( name = "herramientas" )
View(herr_cuadrillas)

herr_jzz <- herr_cuadrillas %>%
  filter( responsable == 'LITUMA CORDOVA CESAR RUBEN'     | 
          responsable == 'GUZMAN BARROS MARCO FERNANDO'     | 
          responsable == 'BARRAZUETA GONZAGA SERVIO GUILLERMO' |
          responsable == 'ALEJANDRO PACHAR AGUSTIN EDUARDO' |
          responsable == 'RIVERA GUAMAN SEGUNDO PATRICIO' |
          responsable == 'RIOS RIOS FRANCISCO FERNANDO' |
          responsable == 'LOZANO SIGCHO NAUN ENRIQUE' |
          responsable == 'QUIROGA ORDONEZ CARLOS HERNAN' |
          responsable == 'CACAY LUZURIAGA ASDRUBAL HUMBERTO' |
          responsable == 'AMARI ORDONEZ JUNIOR IVAN' 
        ) %>%
  mutate( cuadrilla = case_when( (responsable == 'LITUMA CORDOVA CESAR RUBEN' ) ~ 'Cuadrilla Gualaquiza',
                                 (responsable == 'GUZMAN BARROS MARCO FERNANDO' ) ~ 'Cuadrilla Gualaquiza',
                                 (responsable == 'BARRAZUETA GONZAGA SERVIO GUILLERMO') ~ 'Cuadrilla Yantzaza',
                                 (responsable == 'ALEJANDRO PACHAR AGUSTIN EDUARDO' ) ~ 'Cuadrilla Yantzaza',
                                 (responsable == 'RIVERA GUAMAN SEGUNDO PATRICIO' ) ~ 'Cuadrilla Zamora',
                                 (responsable == 'RIOS RIOS FRANCISCO FERNANDO' ) ~ 'Cuadrilla Zamora',
                                 (responsable == 'LOZANO SIGCHO NAUN ENRIQUE' ) ~ 'Cuadrilla Yacuambi',
                                 (responsable == 'QUIROGA ORDONEZ CARLOS HERNAN' ) ~ 'Cuadrilla Guayzimi',
                                 (responsable == 'CACAY LUZURIAGA ASDRUBAL HUMBERTO' ) ~ 'Cuadrilla Guayzimi',
                                 (responsable == 'AMARI ORDONEZ JUNIOR IVAN' ) ~ 'Cuadrilla El Pangui'
                                 )) %>%
  group_by( cuadrilla ) %>% 
  count( name = "herramientas_cuadrilla" , wt = herramientas)

herr_jzz <- herr_jzz[ order(-herr_jzz$herramientas_cuadrilla), ]

herr_jzz$orden <- c(6:1)

View(herr_jzz)

# Plot : https://r-graph-gallery.com/web-horizontal-barplot-with-labels-the-economist.html
install.packages('shadowtext')

library(grid)
library(tidyverse)
library(shadowtext)
library('ggplot2')

# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"

#---------------
ggplot( herr_jzz,
               aes( y = reorder(cuadrilla, orden),
                    x = herramientas_cuadrilla))+
  geom_col(fill = BLUE, width = 0.6) + 
  
  scale_x_continuous(
    limits = c(0, 175),
    breaks = seq(0, 200, by = 25), 
    expand = c(0, 5), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)
  ) +
  
  geom_shadowtext(
    data = subset(herr_jzz, herramientas_cuadrilla < 25),
    aes(herramientas_cuadrilla, y = cuadrilla, label = cuadrilla),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLUE,
    bg.colour = "white",
    bg.r = 0.2,
    family = "Econ Sans Cnd",
    size = 7
  ) +
  geom_text(
    data = subset(herr_jzz, herramientas_cuadrilla >= 25),
    aes(0, y = cuadrilla, label = cuadrilla),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "Econ Sans Cnd",
    size = 7
  ) +
  labs(
    title = "Herramientas por Cuadrilla",
    subtitle = "Cantidad de herramientas en inventario al 2022\n",
    caption = "Fuente: EERSSA 2022"
  ) + 
  theme(
    plot.title = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "Econ Sans Cnd",
      size = 20
    ),
    plot.caption = element_text(
      family = "Econ Sans Cnd", 
      face = "bold",
      size = 14
    )
  )

######
# 12 Mobile

mtto <- data.frame( one = 1, mantenimiento = 0.23)

ggplot( mtto,
        aes( x = mantenimiento,
             y = 1 )) +
  geom_col(fill = 'darkgreen', width = 0.6) 
  