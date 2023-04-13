
# APLICACIÓN SHINY TRABAJO FINAL R INTERMEDIO -----

# 1. LIBRERÍAS  -------

library(shiny)
library(highcharter)
library(sf)
library(haven)
library(tidyverse)
library(ggrepel)
library(kableExtra)
library(shinythemes)
library(bslib)
library(shinydashboard)


# 2. PREPROCESAMIENTO ------

#Traemos la data. Redondeamos valores con 4 decimales y los convertimos a porcentajes. 
#Quitamos la variable no_seguro (no forma parte del índice)
#Cambiamos a los nombres de las variables a nombres más intuitivos
data <- read_dta("https://github.com/mariana-otero/Fundamentos-de-R/raw/main/data_completa.dta")
data <- data |>
  mutate_at(vars(2:12), ~. *100) |> 
  mutate_if(is.numeric, ~ round(., 2))|> 
  select(-(no_seguro)) |> 
  rename("brecha_pisos_vivienda" = pisos, "brecha_agua_potable"=agua, "brecha_desague"=desague, 
         "brecha_electricidad"=electricidad, "brecha_combustible_cocina"=comb_coc,"brecha_servicios_salud"=salud_asist,
         "brecha_escolaridad"=escol_jefe,"brecha_matrícula"=matri_esc, "brecha_bienes_duarables"=bien)

#Traemos el shapefile. Convertimos nombres de las regiones a mayúsculas y reemplazamos espacios en blanco por "_"
shapefile <- read_sf("https://github.com/mariana-otero/Fundamentos-de-R/raw/main/per_admbnda_adm1_ign_20200714.json") |> 
  select(geometry,
         region = ADM1_ES) |> as.data.frame ()
shapefile$region <- toupper(shapefile$region)
shapefile$region <- gsub(" ", "_", shapefile$region)

#Creamos una sub_base para cada año
data2017 <- data |> 
  filter(año==2017)
data2018 <- data |> 
  filter(año==2018)
data2019 <- data |> 
  filter(año==2019)
data2020 <- data |> 
  filter(año==2020)
data2021 <- data |> 
  filter(año==2021)

#Colocamos las coordenadas en la data para el mapa de calor
data2017 <- merge(data2017, shapefile, by = "region")
data2018 <- merge(data2018, shapefile, by = "region")
data2019 <- merge(data2019, shapefile, by = "region")
data2020 <- merge(data2020, shapefile, by = "region")
data2021 <- merge(data2021, shapefile, by = "region")

#juntamos en uno solo
datamapa <- rbind (data2017, data2018,data2019,data2020,data2021)

#Lista de regiones para los filtros
lista_regiones<-data |> 
  select(region) |> 
  distinct() |> 
  arrange(region) |> 
  drop_na()
#Lista de años
lista_años <-data |> 
  select(año) |> 
  distinct() |> 
  arrange(año) |> 
  drop_na()
  
# 3. UI: USER INTERFACE -----

# 3.1 ELEMENTOS DE LA UI

PRIMER_PANEL <- sidebarLayout(
           sidebarPanel(
             selectInput("region",
                         "Seleccione el departamento",
                         choices= lista_regiones,
                         selected = "LIMA"),
             br(),
             sliderInput("Slider",
                         "Seleccione el periodo",
                         value = c(2017,2021),
                         min = 2017, max = 2021, step = 1),
         
             selectInput("año",
                         "Seleccione el año para ver el TOP 10",
                         choices = lista_años,
                         selected = 2017),
           ),
           
           mainPanel(
             dashboardBody(
             fluidRow(
               style="background-color: AliceBlue;",
               box(
                 width = 8, status = "primary", solidHeader = TRUE,
                 title = "Evolución del Índice normalizado de Cobertura de Servicios Básicos",
                 highchartOutput("output1", width = "100%", height = 400)
               ),
             
               box(
                 width = 4, status = "warning",  solidHeader = TRUE,
                 title = "TOP 10: Departamentos con mejor cobertura",
                 tableOutput("output2")
               )),
  
             fluidRow(
               box(
               width = 12, status = "info",
               title = "Definición de las indicadores empleados",
               tableOutput("output3"))
               )
             )
           ))

SEGUNDO_PANEL <- sidebarLayout(
  sidebarPanel(
    selectInput("año",
                "Seleccione el año",
                choices = lista_años,
                selected = 2017),
  ),
  mainPanel(
    fluidRow(
      column(10,plotOutput("output4", height = "1000"))
    )
  )
  )

TERCER_PANEL <- sidebarLayout(
              sidebarPanel(
                 selectInput("region2",
                             "Seleccione el departamento",
                             choices= lista_regiones,
                             selected = "LIMA"),
            ),
                mainPanel(
                  dashboardBody(
                  fluidRow(
                  tabBox(
                    title = tagList(shiny::icon("chart-line"), "Evolución de indicadores"),
                         width = "100px",
                    tabPanel("Bienes Durables", plotOutput("output5")),
                    tabPanel("Servicios Básicos",plotOutput("output6")),
                    tabPanel("Educación", plotOutput("output7")),
                    tabPanel("Salud", plotOutput("output8")),
                  )
                  )
                  )
                )
          )


#3.2 UI

ui <- fluidPage(
        theme = shinytheme("flatly"),
        titlePanel("Cobertura de Servicios Básicos en Hogares Peruanos"),
        
        navbarPage(
          
          "Dashboard",
          tabPanel (tagList(shiny::icon("temperature-three-quarters"),"Índice"),
                    PRIMER_PANEL
                    ),
          tabPanel(tagList(shiny::icon("earth-americas"),"Mapa de Calor"),
                    SEGUNDO_PANEL),
          tabPanel(tagList(shiny::icon("chart-line"),"Indicadores"),
                   TERCER_PANEL)
          
        )  
        
)


# 4. SERVIDOR ----- 
# El servidor maneja la lógica de la aplicación, 
# procesa los datos y crea la salida que se muestra en la UI. 

server <- function(input, output) {
  
  output$output1 <- renderHighchart({
    data |>
      filter(region==input$region,
             año>=input$Slider[1], 
             año<=input$Slider[2]) |> 
      hchart(type = "column",
             hcaes(x = año, y = pc1_norm),
             palette = "Ocean") |> 
      hc_subtitle(text = "Tasas de cobertura re-escaladas del 0 al 100%",
                  style = list(color = "#666666", fontSize = "14px")) |>
      hc_yAxis(title = list(text = "Tasa de cobertura (%)"), min=0, max=100) |>
      hc_xAxis(title = list(text = "Año")) |>
      hc_credits(enabled = TRUE,
                 position = list(x = -50, y = -10),
                 text = "Fuente: ENAHO(2023)",
                 style = list(color = "#666666", fontSize = "12px")) 
  })
  
  
  output$output2<- renderTable({
    data |> 
      filter(año == input$año) %>%
      group_by(region) %>%
      arrange(desc(pc1_norm), tolower(region)) %>%
      select("Regiones" = region, "% de cobertura normalizada" = pc1_norm) %>%
      as.data.frame() %>%
      head(10)
  }, digits = 1)
  
  output$output3<- function(){
    tabla <- data.frame(
      Variable = c("año", "brecha_pisos_vivienda", "brecha_agua_potable", "brecha_desague", "brecha_electricidad", 
                   "brecha_combustible_cocina","brecha_servicios_salud","brecha_escolaridad","brecha_matrícula", 
                   "brecha_bienes_duarables"),
      Descripción = c("Año de la encuesta", 
                      "Promedio de hogares que cuentan con pisos de barro, tierra u otro material natural", 
                      "Promedio de hogares que no cuentan con agua potable en su vivienda", 
                      "Promedio de hogares que no cuentan con acceso a sanieamiento adecuado en su vivienda", 
                      "Promedio de hogares que no cuentan con conexión de electricidad en su vivienda", 
                      "Promedio de hogares que cocinan con carbón, leña u otro combustible contaminante", 
                      "Promedio de hogares que no acceden a servicios de salud por falta de dinero, distancia y/o falta de seguro", 
                      "Promedio de hogares con jefe de familia con secundaria incompleta", 
                      "Promedio de hogares con niños y niñas de hasta 14 años que no están matriculados en la escuela",
                      "Promedio de hogares que no cuentan con bienes durables en la vivienda (radio, tv, teléfono, etc)")
    )
    
    tabla |>
      kable() |>
      kable_styling()
    
    
  }  

  output$output4<- renderPlot({
    datamapa |> 
      filter(año==input$año) |> 
      ggplot() +
      geom_sf(aes(geometry = geometry, fill = pc1_norm))+
      geom_sf_label(aes(geometry = geometry, label = pc1_norm),
                    size = 5, fontface = "bold", fill = "white", label.padding = unit(0.1, "lines")) +
      labs(title = "Índice normalizado de Cobertura en Servicios Básicos",
           subtitle = "% de cobertura (color más oscuro = mayor cobertura)",
           caption = "Fuente: ENAHO",
           align = "center",
      )+
      scale_fill_gradient(low = "#132B43", high= "#56B1F7", name="índice", 
                          labels = c("0", "20", "40", "60", "80", "100"),
                          breaks = c(0,20,40,60,80,100))+
      theme(
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.text = element_text(color = "transparent"),
        axis.ticks = element_line(color = "transparent"),
        panel.grid = element_line(color = "transparent"),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 14)
      ) 
  })

  output$output5 <- renderPlot({
    data %>%
      filter(region==input$region2,
             ) %>%
      ggplot(aes(x = año, y = brecha_bienes_duarables )) +
      geom_line(color = "blue") +
      geom_point(size = 3) +
      geom_text(aes(label = round(brecha_bienes_duarables , 2)), vjust = -0.5) +
      labs(x = "Años",
           y = "Tasa de hogares sin acceso (%)",
           title = "Evolución del indicador",
           subtitle = "Bienes durables",
           caption = "Fuente: ENAHO (2023)") +
      theme_classic()
    
  })

  output$output6 <- renderPlot({
    data %>%
      filter(region==input$region2,
             ) %>%
      gather(variable, value, brecha_agua_potable, brecha_desague, brecha_electricidad, 
             brecha_combustible_cocina, brecha_pisos_vivienda) %>%
      ggplot(aes(x = año, y = value, color = variable)) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(aes(label = round(value, 2)), vjust = -0.5) +
      scale_color_manual(values = c(brecha_agua_potable = "blue", brecha_desague = "red", brecha_electricidad = "green", 
                                    brecha_combustible_cocina = "purple", brecha_pisos_vivienda = "orange")) +
      labs(x = "Años",
           y = "Tasa de hogares sin acceso (%)",
           title = "Evolución de los indicadores",
           subtitle = "Agua potable, Desagüe, Electricidad, Combustible de cocina y Pisos de la vivienda",
           caption = "Fuente: ENAHO (2023)") +
      theme_classic()
  })
  
  output$output7 <- renderPlot({
    data %>%
      filter(region==input$region2,
             ) %>%
      gather(variable, value, brecha_escolaridad, brecha_matrícula ) %>%
      ggplot(aes(x = año, y = value, color = variable)) +
      geom_line() +
      geom_point(size = 3) +
      geom_text(aes(label = round(value, 2)), vjust = -0.5) +
      scale_color_manual(values = c(brecha_escolaridad = "blue", brecha_matrícula = "red")) +
      labs(x = "Años",
           y = "Tasa de hogares sin acceso (%)",
           title = "Evolución de los indicadores",
           subtitle = "Escolaridad jefe del hogar y Matrícula escolar",
           caption = "Fuente: ENAHO (2023)") +
      theme_classic()
  })    
  
  output$output8 <- renderPlot({
    data %>%
      filter(region==input$region2,
             ) %>%
      ggplot(aes(x = año, y = brecha_servicios_salud )) +
      geom_line(color = "blue") +
      geom_point(size = 3) +
      geom_text(aes(label = round(brecha_servicios_salud , 2)), vjust = -0.5) +
      labs(x = "Años",
           y = "Tasa de hogares sin acceso (%)",
           title = " Evolución de los indicadores",
           subtitle = "Acceso a servicios de salud",
           caption = "Fuente: ENAHO (2023)") +
      theme_classic()
  })
  
}

# 5. EJECUCIÓN DE APLICACIÓN ----- 
shinyApp(ui = ui, server = server)

#https://mariana-otero.shinyapps.io/R_intermedio/
