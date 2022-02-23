# Pedro Concejero febrero 2022
# Ejemplo de app shiny para taller R-ladies, R-madRid y UTAD
#
# Ejemplo de representación series temporales COVID-19 (olas) por grupo edad y sexo
# Y de la gravedad por grupo de edad y sexo


library(shiny)
# En esta versión 2 añadimos shinythemes
library(shinythemes)
library(ggplot2)
library(tsibble)
library(feasts)
library(smooth)

# Cargamos los datos 

isciii <- read_csv("https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv")

# Para barplot:

#names(isciii)[4] <- "fecha"

fech_max <- max(isciii$fecha)

cond <- {isciii$fecha < fech_max & 
  isciii$grupo_edad != "NC" &
  isciii$sexo != "NC" }

# cond <- {isciii$grupo_edad != "NC" &
#  isciii$sexo != "NC" }


data_para_plot <- isciii[cond, ]

# Seguimos:

shinyServer(function(input, output) {
  
    output$plot2 <- renderPlot({
      
      # series temporales

      olas_covid <- isciii %>%
        filter(grupo_edad == input$edad &
                 fecha >= input$dateRange[1] &
                 fecha < input$dateRange[2]) %>%
        group_by(sexo, fecha) %>%
        summarise(total_contagiados = sum(num_casos),
                  total_hospitalizados = sum(num_hosp),
                  total_uci = sum(num_uci),
                  total_fallecidos = sum(num_def))  %>%
        as_tsibble(key = c(sexo),
                   index = fecha) 
      
        autoplot(olas_covid, 
                 get(input$y2),
                 colour = "grey90")  + geom_smooth(method = "loess", span = 0.05) +
          ylab(input$y2) +
          xlab(paste(input$y2, "\n",
        "por COVID-19 en España entre ",
        input$dateRange[1]," y ",input$dateRange[2]))
        
        })
    

    # Para el barplot: 
    
    output$plot <- renderPlot({
      
      # dodged bar charts
      
      p <- ggplot(data_para_plot)
      
      if (input$y == 'total_contagiados')
        p <- p + aes(grupo_edad, num_casos,
                     fill = sexo)
      
      if (input$y == 'total_hospitalizados')
        p <- p + aes(grupo_edad, num_hosp,
                     fill = sexo)
      
      if (input$y == 'total_uci')
        p <- p + aes(grupo_edad, num_uci,
                     fill = sexo)
      
      if (input$y == 'total_fallecidos')
        p <- p + aes(grupo_edad, num_def,
                     fill = sexo)
      
      p <- p + geom_bar(position = "dodge",
                        stat = "identity") + coord_flip()
      
      title <- paste(input$y, 
                     "por COVID-19 en España",
                     "\n",
                     "entre",
                     min(isciii$fecha),
                     "y",
                     max(isciii$fecha))
      
      p <- p + ggtitle(paste(title, "\n", "por género"))
      
      print(p)
      
    })
    
    
})

