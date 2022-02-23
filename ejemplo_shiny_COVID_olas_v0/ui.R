# Pedro Concejero febrero 2022
# Ejemplo de app shiny para taller R-ladies, R-madRid y UTAD
#
# Ejemplo de representación series temporales COVID-19 (olas) por grupo edad y sexo
# Y de la gravedad por grupo de edad y sexo

library(shiny)
library(ggplot2)
library(tidyverse)
library(tsibble)
library(feasts)

# Cargamos los datos desde el repositorio Inst. Salud Carlos III (ISCIII) 

# Cargamos los datos 
# También necesario en ui.R porque se extraen de aquí los rangos de edad

isciii <- read_csv("https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv")

# Para el barplot agregamos (sumamos) todos los datos (casos) hasta la fecha de descarga

fech_max <- max(isciii$fecha)

cond <- {isciii$grupo_edad != "NC" &
  isciii$sexo != "NC"} # excluimos datos de sexo NC

data_para_plot <- isciii[cond, ]

# Extraemos el vector de :

edades <- unique(data_para_plot$grupo_edad)

# distinguimos variables "a nivel de intervalo" ("continuas" para barplot (ggplot))

nums <- sapply(data_para_plot, is.numeric)
continuas <- names(data_para_plot)[nums]

# y variables "categóricas" ("discretas" para barplot(ggplot))
cats <- sapply(data_para_plot, is.character)
categoricas <- names(data_para_plot)[cats]


shinyUI(
  navbarPage("Shiny Visualización COVID-19 en España",
                   tabPanel("Introducción",
                            mainPanel(
                              h1("Ejemplo Visualización con R-shiny", align = "center"),
                              h1("Taller R-Ladies, R-madRid, UTAD", align = "center"),
                              h3("Propuesto por Pedro Concejero, 24 Febrero 2022", align = "center"),
                              p(""),
                              h2("IMPORTANTE", align = "center"),
                              h2("Recomendable resolución superior a 1280x1024 para visualizar gráficos \n
                                 sin tener que hacer scroll lateral", 
                                 align = "center"),
                              p("El propósito es ilustrar el desarrollo de una app. shiny con un ejemplo real
                                y, desafortunadamente, todavía preocupante -esperemos que cada día menos.
                                El conjunto de datos de contagios, hospitalizaciones, ingresos en UCI y fallecimientos
                                se descarga al ejecutar la app. desde el Instituto de Salud Carlos III(*).
                                Se trata de los datos consolidados hasta un día antes de la descarga.
                                A partir de este conjunto de datos se realizan dos visualizaciones interactivas, 
                                cada una en una pestaña:"),
                              p(""),
                              p("- Serie temporal de la variable elegida, en función de grupo de edad y fechas"),
                              p("- Barplot o gráfico de barras por grupo edad y sexo con el total de casos eligiendo la gravedad"),
                              p(""),
                              h4("(*) Muchas gracias a Mariluz Congosto por este enlace"),
                              HTML("<p><a href='https://twitter.com/congosto'> Twitter de Mariluz </a> </p>")
                              )),
             tabPanel("Olas COVID -series temporales",
                      sidebarPanel(
                        selectInput(inputId = 'y2', 
                                    'Elige variable para eje Y', 
                                    choices = c("total_contagiados",
                                                "total_hospitalizados",
                                                "total_uci",
                                                "total_fallecidos"), 
                                    selected = "total_contagiados"),
                        selectInput(inputId = 'edad', 
                                    'Elige grupo de edad', 
                                    edades,
                                    edades[[3]])
                        ),
                      mainPanel(
                        plotOutput(outputId = 'plot2',
                                   height = 1000,
                                   width = 1200
                        ))
                        
                      ),
                      
             tabPanel("Barplot",
                      sidebarPanel(
                        
                        selectInput(inputId = 'y', 
                                    'Elige lo que se representará en barplot', 
                                    choices = c("total_contagiados",
                                                "total_hospitalizados",
                                                "total_uci",
                                                "total_fallecidos"), 
                                    selected = "total_contagiados")),
                     
                     mainPanel(
                       plotOutput(outputId = 'plot',
                                  height = 1000,
                                  width = 1200)
             ))
             
  
))



