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

set.seed(1234)
school_quality <-
  tibble(
    id = seq(1, 300, 1),
    school = rep(c(
      "Sabin", "Vernon", "Faubion", "Irvington", "Alameda", "Beverly Cleary"
    ), 50),
    opinion = sample(c("Very bad", "Bad", "Good", "Very Good"), 300, replace = TRUE)
  )
view(school_quality)

school_quality_summary <- school_quality %>% 
  group_by(school, opinion) %>% 
  count(name = "n_answers") %>% 
  
  group_by(school) %>% 
  mutate(percent_answers = n_answers / sum(n_answers)) %>% 
  ungroup() %>% 
  mutate(percent_answers_label = percent(percent_answers, accuracy = 1))
view(school_quality_summary)

ae1 <- geope_2023 %>%
  filter( !is.na(ALIMENTADOR)) %>%
  filter( !is.na(CUENTA)) %>%
  group_by( ALIMENTADOR, CUENTA) %>%
  count( name = "tiempo", wt = MINUTOS ) %>%
  filter( tiempo > 150 )
view(ae1)