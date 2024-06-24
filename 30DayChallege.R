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

# Carga de datos desde archivo de Excel
hojas <- excel_sheets(path = '2023_CALIFICACION_ACTIVIDADES_R.xlsx')
hojas <- hojas[-1]  # la primera hoja no contiene datos útiles
print(hojas)

n <- length(hojas)
datalist = list()

for (i in 1:n){
  datalist[[i]] <- read_excel('2023_CALIFICACION_ACTIVIDADES_R.xlsx', sheet = hojas[[i]])
}
df_2023 = do.call(rbind, datalist)
rm(datalist)

# Elimintar los casos en los que la columna 'evento' este vacia.
df_2023 <- df_2023[complete.cases(df_2023[ , 'EVENTO']),]


# Calcular la diferencia de tiempo entre las columnas de inicio/fin de actividad
# referencia: https://stackoverflow.com/questions/69527810/calculate-time-difference-between-posixct-values

library(dplyr)
library(hms)


df_2023 <- df_2023 %>%
  mutate( MINUTOS = as.numeric(difftime( df_2023$`FECHA FIN`, df_2023$`FECHA INICIO`,units = "mins")))

view(df_2023)

writexl::write_xlsx( df_2023, "df_2023.xlsx" )


# Eliminar los casos en los que la columna 'Alimentador' este vacia
actividades2023 <- df_2023[complete.cases(df_2023[,3]),]
str(actividades2023)

# Filtrar para ocupar solo las Cuadrillas de GEOPE



