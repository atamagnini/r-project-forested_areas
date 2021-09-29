library(tidyr)
library(dplyr)
library(sp)
library(zoo)
library(ggplot2)
library(gganimate)
library(rworldmap)
library(rworldxtra)
library(rgeos)
library(viridis)
library(gifski)
library(rgdal)
library(data.table)
library(ggthemes)
library(geosphere)
library(scales)
library(jpeg)
library(ggpubr)
library(r2d3)
library(jsonlite)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)

#leer dataframe
dataset <- read.csv("~/data.csv", stringsAsFactors = FALSE)

#eliminar filas y columnas N/A
dataset <- dataset[1:217, -c(3:34)]
dataset <- dataset[, -c(30:32)]

#valores ausentes
glimpse(dataset)
dataset[dataset == ".."] <- NA
dataset[dataset == "0"] <- NA
any(is.na(dataset))

##transforma df a matriz numerica
dataset2 <- as.matrix(as.data.frame(lapply(dataset[-c(1:2)], as.numeric)))
dataset3 <- cbind(dataset[,1:2],dataset2[,1:27])

#renombrar col
dataset4 <- as.data.frame(dataset3)
names(dataset4) <- substring(names(dataset4), 2, 5)
colnames(dataset4)[1] <- "Country"
colnames(dataset4)[2] <- "Country Code"
colnames(dataset4)

#imputar valores ausentes
##identificar filas con NA
ausentes <- rowSums(is.na(dataset4))
print(ausentes)
##quitar filas con todos sus valores ausentes
dataset5 <- dataset4[-c(which(ausentes == 27)), ]
##reenumerar filas
rownames(dataset5) <- 1:nrow(dataset5)
##valores ausentes restantes a imputar
ausentes2 <- (rowSums(is.na(dataset5)))
print(ausentes2)
##cuáles son las filas con esos valores ausentes
ausentes3 <- which(ausentes2 > 0)
print(ausentes3)
##imputar con valor más cercano a la derecha
dataset6 <- as.data.frame(t(apply(dataset5, 1, function(x) na.locf0(x, fromLast = TRUE))))
##renombrar columnas
names(dataset6) <- substring(names(dataset5), 1, 5)
colnames(dataset6)[1] <- "Country"
colnames(dataset6)[2] <- "id"
colnames(dataset6)
##verificar si hay mas datos ausentes
ausentes4 <- which(rowSums(is.na(dataset6)) > 0)
print(ausentes4)
##completar ausente con valor mas cercano a la izquierda
dataset7 <- as.data.frame(t(apply(dataset6, 1, na.locf)))
##verificar si siguen habiendo val aus
ausentes5 <- which(rowSums(is.na(dataset7)) > 0)
print(ausentes5)

##line plot
###wide data to long data
dataset8 <- pivot_longer(dataset7, -c(1:2), names_to = "Year", values_to = "Radio")
colnames(dataset8)[4] <- "Sq.Km."
glimpse(dataset8)
dataset8$Year <- as.numeric(as.character(dataset8$Year))
dataset8$Sq.Km. <- as.numeric(as.character(dataset8$Sq.Km.))
dataset8 <- dataset8 %>%
  mutate(Mil.Sq.Km. = Sq.Km./1000000)
dataset8$Mil.Sq.Km. <- as.numeric(as.character(dataset8$Mil.Sq.Km.))
glimpse(dataset8)

##Plot
g <- dataset8 %>%
  group_by(Year) %>%
  summarise(Mil.Sq.Km. = sum(Mil.Sq.Km.)) %>%
  ggplot(aes(x = Year, y = Mil.Sq.Km., group = 1,
             text = paste0("Year: ", Year, "</br></br>Millons Square Kilometers: ", round(Mil.Sq.Km., 2)))) +
  geom_point(size = 1.6, color = "dark green", shape = 23, fill = "dark green") +
  labs(title = 'World forested area',
       subtitle = 'Millons of square kilometers of forested area 1990-2016') +
  scale_x_yearmon(format = "%Y", n = 27) +
  theme_minimal() +
  theme(plot.title = element_text(size= 18, face = "bold", hjust = 0),
        axis.text.x = element_text(angle = 60, hjust = 1, colour = 'grey30',
                                   size = 9, face = 'italic'),
        axis.text.y = element_text(colour = 'grey30', face = 'italic', size = 8),
        axis.title.y = element_text(face = 'bold', size = 9, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 13, face = 'italic', hjust = 0),
        )
ggplotly(g, tooltip = "text") %>% layout(dragmode = "pan",
                                         hoverlabel = list(bgcolor = "white"),
                                         xaxis = list(range = c(1990, 2016)),
                       title = list(text = paste0('World forested area',
                                                  '<br>',
                                                  '<sup>',
                                                  'Millons of square kilometers of forested area 1990-2016',
                                                  '<sup>')),
                       annotations = list (text = "Source:World Development Indicators",
                                           showarrow = F, 
                                           xref = "paper", x = 1,
                                           yref = "paper", y = -0.3,
                                           font = list(size = 10)))
