rm(list = ls()); gc()

## Cargo mis paquetes
library(dplyr)
library(jsonlite)
library(data.table)
library(fst)
library(crayon)

# === === === === === === === === ===
# === 0. Elementos en Steam       ===
# === === === === === === === === ===

# Se descarga todos los elementos de steam esto incluye "game, application, tool, demo, dlc, music"

## Lista de videojuegos
lista_juegos <- as.data.table(fromJSON("http://api.steampowered.com/ISteamApps/GetAppList/v0002")$applist) %>%
                rename(appid = apps.appid, name = apps.name) %>% arrange(appid) %>% filter(name != "") %>%
                distinct()

lista_juegos #151.784

saveRDS(object = lista_juegos, file = "data/lista_juegos.RDS")

# ==============================================================================================================================================================

# === === === === === === === === ===
# === 1. Lista Video juegos Steam ===
# === === === === === === === === ===


rm(list = ls()); gc(T)
lista_juegos <- readRDS(file = "data/lista_juegos.RDS"); dim(lista_juegos) #151784

## Creo los enlaces
links <- paste0("http://store.steampowered.com/api/appdetails?appids=",lista_juegos$appid,"&cc=us&l=en")
head(links)

## Creo mi funcion para obtener la lista de juegos y filtre por type = game
get_game <- function(url){
  df_games <- tryCatch(df_games <- fromJSON(url), error = function(mensaje) {
    message("Se ha generado un error")
    list(`1` = data.frame(success = FALSE))}
  )
  if(df_games[[1]]$success){
    res <- df_games[[1]]$data
    if(res$type != "game"){df = NULL} else {
      df = data.frame(type = res$type, name = res$name, steam_appid = res$steam_appid, required_age = as.integer(res$required_age), is_free = res$is_free,
                      supported_languages = gsub(pattern = "<br>", replacement = " ",
                                                 x = gsub(pattern = "<strong>", replacement = "",
                                                          x = gsub(pattern = "\\/", replacement = "",
                                                                   x = gsub(pattern = "\\*", replacement = "",
                                                                            x = ifelse(is.null(res$supported_languages), NA_character_, res$supported_languages))))),
                      header_image = res$header_image, developers = paste0(res$developers, collapse = ";"), publisher = res$publisher,
                      price = ifelse(is.null(res$price_overview$initial), NA_real_, res$price_overview$initial) / 100,
                      metacritic_score = ifelse(is.null(res$metacritic$score), NA_real_, res$metacritic$score),
                      categories = paste0(res$categories$description, collapse = ";"), genres = paste0(res$genres$description, collapse = ";"),
                      recommendations = ifelse(is.null(res$recommendations$total), NA_integer_, res$recommendations$total),
                      release_date = res$release_date$date)
    }
  } else {df = NULL}
  return(list(df = df, status = df_games[[1]]$success))
}


# Inicio del proceso
n <- ceiling(nrow(lista_juegos) / 200);print(n)
my_list <- suppressWarnings(split(1:nrow(lista_juegos), rep(rep(1:n), rep(200, n))))

# Creo un contador para que cada 200 llamadas, haga una pausa sys.sleep(), para hacer nuevamente una llamada
Sys.time()
df_juegos_total = NULL
contador <- 0
val <- system.time({
  for(j in 1:n){
    df_juegos = NULL
    for(i in my_list[[j]]){
      result <- get_game(url = links[i])
      df_juegos <- bind_rows(result$df, df_juegos)
      contador = contador + 1
      print(paste("fila:",contador,"-","Estatus:", ifelse(result$status,"succes","fail  "),"-","app id:", lista_juegos$appid[i]))
    }
    df_juegos_total <- bind_rows(df_juegos, df_juegos_total)
    saveRDS(object = df_juegos_total, file = "data/df_juegos.RDS")
    cat(green("finaliza lista",j, "\n"))
    Sys.sleep(210)
    beepr::beep(2)
  }
  beepr::beep(8)
})
Sys.time()

# ==============================================================================================================================================================

# === === === === === === === === ===
# === 2. Reseñas Usuarios Steam   ===
# === === === === === === === === ===

rm(list = ls()); gc(T)

library(dplyr)
library(jsonlite)
library(lubridate)
library(data.table)
library(crayon)

# Cargo el total de juegos y filtro por aquellos que sean pagos
df_juegos <- as.data.table(readRDS(file = "data/df_juegos.RDS")) #76612
df_juegos <- df_juegos[is_free == FALSE] #67531
df_juegos <- df_juegos[!is.na(recommendations)] #12233
df_juegos <- df_juegos[release_date >= '2012-01-01'] #11362
#df_juegos <- df_juegos[recommendations >= 100 & recommendations <= 1000] #3776


# Creo mi funcion para obtener las reseñas de los usuarios para
get_users <- function(steam_appid, conteo = 0){# solo reseñas de españa y latam
  df_reseñas <- NULL
  #url <- paste0("https://store.steampowered.com/appreviews/",steam_appid,"?json=1&filter=updated&language=all&purchase_type=steam&&num_per_page=100&cursor=*")
  url <- paste0("https://store.steampowered.com/appreviews/",steam_appid,"?json=1&filter=updated&language=spanish,latam&num_per_page=100&cursor=*")
  result <- fromJSON(url)
  conteo <- conteo + 1
  print(paste("Request:",conteo))
  if((conteo %% 200) == 0) Sys.sleep(230)

  df_general <- data.frame(appid = steam_appid,
                           review_score = result$query_summary$review_score,
                           total_positive = result$query_summary$total_positive,
                           total_negative = result$query_summary$total_negative,
                           total_reviews = result$query_summary$total_reviews)
  
  reseñas <- as.data.table(result$reviews)
  if(df_general$total_reviews > 1){
    reseñas$review <- NULL
    
    reseñas <- mutate(.data = reseñas,
                      author.last_played = as_datetime(author.last_played),
                      timestamp_created = as_datetime(timestamp_created),
                      timestamp_updated = as_datetime(timestamp_updated),
                      weighted_vote_score = as.numeric(weighted_vote_score),
                      appid = steam_appid)
    df_reseñas <- bind_rows(df_reseñas, reseñas)
    #print(paste("Filas reseñas:",nrow(df_reseñas)))
    
    cursor <- gsub(pattern = "[+]", replacement = "%2B", x = result$cursor)
  }
  
  n <- ifelse(result$query_summary$total_reviews %% 100 == 0, floor((result$query_summary$total_reviews-1) / 100), floor(result$query_summary$total_reviews / 100)) 
  
  # Continue el bucle solo si los juegos tienen más de 100 reseñas
  if(n > 0){
    for(i in 1:n){
      #url <- paste0("https://store.steampowered.com/appreviews/",steam_appid,"?json=1&filter=updated&language=all&purchase_type=steam&&num_per_page=100&cursor=*")
      result <- fromJSON(paste0("https://store.steampowered.com/appreviews/",steam_appid,"?json=1&filter=updated&language=spanish,latam&num_per_page=100&cursor=",cursor))
      result$success
      result$query_summary$num_reviews
      conteo <- conteo + 1
      print(paste("Request:",conteo))
      if((conteo %% 200) == 0) Sys.sleep(230)
      
      reseñas <- as.data.table(result$reviews)
      reseñas <- mutate(.data = reseñas,
                        author.last_played = as_datetime(author.last_played),
                        timestamp_created = as_datetime(timestamp_created),
                        timestamp_updated = as_datetime(timestamp_updated),
                        weighted_vote_score = as.numeric(weighted_vote_score),
                        appid = steam_appid)
      
      df_reseñas <- bind_rows(df_reseñas, reseñas)
      #print(paste0("Filas reseñas: ",nrow(df_reseñas)))
      cursor <- gsub(pattern = "[+]", replacement = "%2B", x = result$cursor)
    } 
  }
  return(list(df_general = df_general, df_reseñas = df_reseñas, conteo = conteo))
}

# Creo un bucle que vaya iterando sobre cada id del videojuego
val2 <- system.time({
  df_users_total <- NULL
  conteo <- 0
  for(i in length(df_juegos$steam_appid)){
    res <- get_users(steam_appid = df_juegos$steam_appid[i], conteo = conteo)
    df_users <- res[[2]]
    df_users_total <- bind_rows(df_users_total, df_users)
    saveRDS(object = df_users_total, file = "data/df_users.RDS")
    cat(bgYellow("finaliza juego:",i,"- appi:",df_juegos$steam_appid[i],"\n"))
    conteo <- res$conteo
    if((conteo %% 200) == 0) Sys.sleep(230)
  }
}) #

lubridate::seconds_to_period(round(val2[3]))

# ==============================================================================================================================================================

# === === === === === === === === ===
# === 3. Información de usuarios  ===
# === === === === === === === === ===

library(steamR)
#df_predict_user <- as.data.table(readRDS(file = "recommender_systems/df_predict_user.RDS"))[,.(steamid = unique(author.steamid))]
df_predict_user <- readRDS(file = "data/df_users.RDS")[,.(steamid = unique(author.steamid))]

lotes <- ceiling(nrow(df_predict_user) / 100)
vector <- split(1:nrow(df_predict_user), sort(rep_len(1:lotes, nrow(df_predict_user))))

system.time({
  df_user_info <- NULL
  for(i in 1:lotes){
    result <- as.data.frame(bind_rows(
      get_player_summaries(steam_key = '0A8A131FD4C9EA7AA1E6CD851566F49C', steam64_id = df_predict_user$steamid[vector[[i]]])
    ))
    df_user_info <- bind_rows(df_user_info, result)
    cat("Finaliza request:",i,"\n")
  }
})
lubridate::seconds_to_period(114)

df_user_info$avatar <- df_user_info$avatarmedium <- df_user_info$avatarhash <- NULL
df_user_info$timecreated <- lubridate::as_datetime(df_user_info$timecreated)
setDT(df_user_info)

table(df_user_info$profilestate) # 1 Perfil configurado, 2 perfil no configurado
table(df_user_info$communityvisibilitystate) #1 privado, 2 parcial, 3 publico

df_user_info$personastate <- df_user_info$personastateflags <- df_user_info$locstatecode
df_user_info$commentpermission

saveRDS(object = df_user_info[,.(steamid, communityvisibilitystate = fifelse(communityvisibilitystate == 1, "private",fifelse(communityvisibilitystate == 3,"public","friends")),
                                 personaname, realname, profileurl, avatarfull, primaryclanid, timecreated, loccountrycode, loccityid)],
        file = "recommender_systems/df_user_info.RDS")

write.fst(x = df_users_total, path = "data/df_users.fst")


# **************************************************************************************************************************************************************
# ==============================================================================================================================================================
# **************************************************************************************************************************************************************

# 1. Cargo la data de las reseñas de los usuarios
df_users <- readRDS(file = "data/df_users.RDS")

# 2. Cargo la información de los juegos
df_juegos <- readRDS(file = "data/df_juegos.RDS")
df_juegos <- df_juegos[is_free == FALSE][!is.na(recommendations)][release_date >= '2012-01-01']
df_juegos <- df_juegos[,.(steam_appid, name, header_image, developers, price, genres, release_date)]
df_juegos <- inner_join(x = df_users[,.(recommendations = .N,positive = sum(voted_up == TRUE),
                                        negative = sum(voted_up == FALSE), rating = sum(voted_up)/.N), by = appid][recommendations >= 25],
                        y = df_juegos, by = c("appid"="steam_appid"))


# Guardo la data
write.fst(x = df_juegos, path = "recommender_systems/df_juegos.fst")

id_user <- unique(df_users[appid %in% df_juegos$appid]$author.steamid)
length(id_user)


# ==============================================================================================================================================================
library(countrycode)
df_users <- readRDS(file = "data/df_users.RDS")

df_user_info <- readRDS(file = "recommender_systems/df_user_info.RDS")
df_users <- left_join(x = df_users, y = df_juegos[,.(appid, price)], by = "appid")
df_users <- df_users[,.(steamid = author.steamid, num_reviews = max(author.num_reviews, na.rm = TRUE),
                     playtime_forever = sum(author.playtime_forever, na.rm = TRUE), price = sum(price, na.rm = TRUE),
                     language), by = author.steamid] %>% distinct()

df_users <- df_users[,head(.SD, 1), by = steamid]

df_user_info$loccountrycode <- countrycode(df_user_info$loccountrycode, origin = 'iso2c',  destination = "cldr.name.es")

df_user_info <- left_join(x = df_user_info, y = df_users, by = "steamid")
df_user_info$author.steamid <- NULL

saveRDS(object = df_user_info, file = "recommender_systems/df_user_info.RDS")

uniqueN(df_user_info$steamid)
