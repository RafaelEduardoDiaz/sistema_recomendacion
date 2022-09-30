## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(dashboardthemes)
library(data.table)
library(dplyr)
library(DT)
library(formattable)
#library(bslib)
library(jpeg)
library(fst)

#recommender_systems/

# 1. Informacion general de los juegos
df_juegos <- read.fst(path = "df_juegos.fst", as.data.table = TRUE)

# 2. Informacion detallada del usuario
df_user_info <- as.data.table(readRDS(file = "df_user_info.RDS"))

# 3. Informacion de los juegos y de las reseñas del usuarios
df_total <- as.data.table(readRDS(file = "df_total.RDS"))

# 4. Informacion predicciones recomendacion usuario
df_predict_user <- as.data.table(readRDS(file = "df_predict_user.RDS"))


#df_juegos[order(-recommendations),.(appid, name, recommendations, rating, genres, release_date)][1:50]
#tail(df_total[author.steamid %in% df_total[author.steamid %in% df_predict_user[appid == 933110]$author.steamid][appid == 813780]$author.steamid][,.(n = .N), by = author.steamid][order(-n)], 50)
#df_predict_user[author.steamid == '76561198308102103'][,.(appid, name, genres, release_date)]
#df_total[author.steamid == '76561198308102103'][,.(appid, name, voted_up, genres, release_date)]


# ==================================================================== #
#  _    _                 _____       _             __                 #
# | |  | |               |_   _|     | |           / _|                #
# | |  | |___  ___ _ __    | |  _ __ | |_ ___ _ __| |_ __ _  ___ ___   #
# | |  | / __|/ _ \ '__|   | | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \  #
# | |__| \__ \  __/ |     _| |_| | | | ||  __/ |  | || (_| | (_|  __/  #
#  \____/|___/\___|_|    |_____|_| |_|\__\___|_|  |_| \__,_|\___\___|  #
# ==================================================================== #


# header +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
header <- dashboardHeader(title = shinyDashboardLogo(theme = "poor_mans_flatly", mainText = "",
                                                     badgeText = "ver 1.0.0",
                                                     boldText = tags$a(href='https://store.steampowered.com/?l=spanish',
                                                                       tags$img(src='Steam_2016_logo_black.png',width="120", height="45"))
))

# Sidebar ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sidebar <- dashboardSidebar(
  h5("La fecha de hoy es ", textOutput("currentTime", container = span)),
  sidebarUserPanel("Usuario",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Imagen extraida de la web
                   image = "https://toppng.com/uploads/preview/user-account-management-logo-user-icon-11562867145a56rus2zwu.png"),
  sidebarMenu(
    menuItem("Home", tabName = "base", icon = icon("house"),
             menuSubItem('Introducción', tabName = "dashboard", icon = icon("rocket")),
             menuSubItem('Estadísticas',tabName = 'stats',icon = icon('chart-pie'))),
    menuItem("Datos del usuario", tabName = "get_user_info", icon = icon("users")),
    menuItem("Recomendaciones", tabName = "get_recommendation", icon = icon("robot"))
  )
)

# body +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
body <- dashboardBody(
  shinyDashboardThemes(theme = "poor_mans_flatly"),
  tabItems(
    # Primer tab content: Descripcion
    tabItem(tabName = "dashboard",
            h2(strong("Objetivo de la App")),
            h3("Predecir los gustos de los usuarios y recomendar videojuegos que podrían ser de su interés."),
            tags$hr(),
            h2(strong("Ventajas de la herramienta propuesta")),
            h3("• Mejoran las conversiones: los sistemas de recomendación permiten transformar a un visitante en un cliente."),
            h3("• Aprovechan la venta cruzada; las recomendaciones aprovechan las compras anteriores del usuario y los productos que tenga en la cesta para sugerirle productos relacionados."),
            h3("• Consiguen fidelizar al cliente; Son capaces de memorizar los gustos de los usuarios y con ello, mostrar sugerencias de mayor calidad y precisión."),
            h3("• Actualizaciones periodicas del comportamiento de los usuarios."),
            h3("• Escalabilidad"),
            h2(strong("Versionamiento")),
            h3("Versión App 1.0.0"),
            h3("R versión 4.2.1"),
            h3("RStudio versión 2022.7.0.548"),
            h2(strong("Contacto")),
            h3("Cualquier inquietud comuniquese a:"),
            h3("Email:",tags$a(href="rafael14diaz@hotmail.com", "rafael14diaz@hotmail.com"))
      ),
    # Segundo tab content: Ingresar CU
    tabItem(tabName = "stats", strong(h2("Resúmen de los videojuegos")),
            fluidRow(
              h2("Sistema de Recomendación Steam", align = "center"),
              tags$hr(),
              DT::dataTableOutput(outputId = "juegos"))
    ),
    
    # Segundo tab content: Informacion del usuario
    tabItem(tabName = "get_user_info",
            fluidRow(
              h2("Sistema de Recomendación Steam", align = "center"),
              tags$hr(),
              DT::dataTableOutput(outputId = "usuarios_dt")
            )
          ),
    # Tercer tab content: Recomendacion
    tabItem(tabName = "get_recommendation",
            fluidRow(
              h2("Sistema de Recomendación Steam", align = "center"),
              tags$br(),
              uiOutput("avatar"),
              tags$br(),
              sidebarPanel(selectInput(inputId = 'id_user',label = 'Seleccion steam id', choices=unique(df_predict_user$author.steamid))
                           #,actionButton(inputId = "busca","Buscar", icon("play"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           ),
              tags$br(),
              box(width = 12, solidHeader = TRUE,
                tabsetPanel(
                  tabPanel(title = "Juegos reseñados", formattableOutput(outputId = 'jugados_dt')),
                  tabPanel(title = "Juegos recomendados", DT::DTOutput(outputId = 'recomendados_dt'))
                )
              )
            )
          )
  )
)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# UI +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# ==================================================================== #
#   _____                                                              #
#  / ____|                                                             #
# | (___   ___ _ ____   _____ _ __                                     #
#  \___ \ / _ \ '__\ \ / / _ \ '__|                                    #
#  ____) |  __/ |   \ V /  __/ |                                       #
# |_____/ \___|_|    \_/ \___|_|                                       #
# ==================================================================== #

# Server +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
server <- function(input, output, session) {
  output$currentTime <- renderText({invalidateLater(1000L, session); format(Sys.time())})

  observe({test <<- paste0("https://google.com")})
  
  output$frame <- renderUI({tags$iframe(src = test, height = 600, width = 1400)})
  
  output$avatar <- renderUI({
    box(title = "Información del Usuario", solidHeader = TRUE, status = "primary",
      h2("Persona Name: ", df_user_info[steamid == input$id_user,.(personaname)]$personaname, align = "left"),
      h3("Real Name: ", df_user_info[steamid == input$id_user,.(realname)]$realname, align = "left"),
      h3("Country: ", df_user_info[steamid == input$id_user,.(loccountrycode)]$loccountrycode, align = "left"),
      tags$img(src = df_user_info[steamid == input$id_user]$avatarfull),
      h4("Community Visibility State: ", df_user_info[steamid == input$id_user,.(communityvisibilitystate)]$communityvisibilitystate, align = "left"),
      h4("Time Created: ", df_user_info[steamid == input$id_user,.(timecreated)]$timecreated, align = "left"),
      strong(h5("ID User: ", df_user_info[steamid == input$id_user,.(steamid)]$steamid, align = "left")),
      )
    })
  
  # Tabla hoja 1
  output$juegos <- renderDT({
    df_juegos <- df_juegos %>% mutate(year = year(release_date))

    tabla <- df_juegos %>% 
      mutate(name = paste0('<a href="https://store.steampowered.com/app/',df_juegos$appid,'">',df_juegos$name,'</a>'),
             image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
      arrange(-recommendations) %>% 
      select(image, appid, name, genres, developers, year, release_date, price, recommendations, rating)
    
    datatable(data = tabla, rownames = TRUE, escape = FALSE, class = "cell-border stripe",
              filter = list(position = 'top', clear = FALSE, plain = TRUE),
              options = list(dom = 'Bfltip',scrollX = FALSE, autoWidth = TRUE, pageLength = 25, lengthMenu = c(5, 10, 25, 50, 100, 500, 1000, 2000, 3000, 4000),
                             initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) %>%
      formatCurrency("price") %>% formatPercentage('rating', 3) %>%
      formatStyle('price',
                  background = styleColorBar(tabla$rating, 'lightgreen'),
                  backgroundSize = '95% 50%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'left')
  })
  

  # Tabla hoja 2
  output$usuarios_dt <- renderDT({
    
    tabla <- df_user_info %>% 
      mutate(steamid = paste0('<a href=',profileurl,'>',steamid,'</a>'),
             avatar = paste0('<img src="', avatarfull, '" width="100" height="80"></img>')) %>% 
      arrange(-price) %>% 
      select(avatar, steamid, communityvisibilitystate, personaname, realname, price, num_reviews, playtime_forever, timecreated, country = loccountrycode, language)
    
    datatable(data = tabla, rownames = TRUE, escape = FALSE, class = "cell-border stripe",
              filter = list(position = 'top', clear = FALSE, plain = TRUE),
              options = list(dom = 'Bfltip',scrollX = FALSE, autoWidth = TRUE, pageLength = 25, lengthMenu = c(10, 25, 50, 100, 1000, 5000, 10000, 15000),
                             initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}"))) %>%
      formatCurrency("price")
  })
  

  # 1. Tabla jugados
  output$jugados_dt <- renderFormattable({
    tabla <- df_total[author.steamid == input$id_user] %>% 
      mutate(image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
      arrange(-rating) %>% mutate(id = 1:n()) %>%
      select(id, image, name, voted_up, genres, developers, release_date, recommendations, rating)
    
    formattable(tabla,
                list(voted_up = formatter("span",
                                          style = ~ style(color = ifelse(voted_up, "green", "red")),
                                          ~ icontext(icon = ifelse(voted_up, "arrow-up", "arrow-down"), voted_up))
                ))
    #datatable(data = tabla, rownames = TRUE, escape = FALSE, class = "nowrap hover row-border",
    #          options = list(dom = 'Bfltip',scrollX = FALSE, autoWidth = TRUE, pageLength = 10, lengthMenu = c(5, 10, 25, 50, 100, 1000),
    #                         initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))
  })
  
  
  # 2. Tabla recomendaciones
  output$recomendados_dt <- DT::renderDT({
    tabla <- df_predict_user[author.steamid == input$id_user] %>% 
             mutate(image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
             arrange(-rating) %>% 
             select(image, name, genres, developers, release_date, recommendations, rating)
    
    datatable(data = tabla, rownames = TRUE, escape = FALSE, class = "nowrap hover row-border",
              filter = list(position = 'top', clear = FALSE, plain = TRUE),
              options = list(dom = 'Bfltip',scrollX = FALSE, autoWidth = TRUE, pageLength = 10, lengthMenu = c(3, 5, 10),
                             initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))
  })
}

shinyApp(ui, server)