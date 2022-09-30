rm(list = ls()); gc(); dev.off()
## Cargo mis paquetes
library(data.table)
library(dplyr)
library(fst)
library(stringr)
library(ggplot2)
library(DT)
library(recommenderlab)
library(kableExtra)

## Cargo las bases de datos de juegos
df_juegos <- read.fst(path = "recommender_systems/df_juegos.fst", as.data.table = TRUE)
df_juegos <- df_juegos[,.(appid, name, header_image, developers, release_date, genres)]


## Cargo la base de datos de usuarios
df_users <- readRDS(file = "data/df_users.RDS")
df_users <- df_users[order(appid, author.steamid),.(appid, author.steamid, voted_up)]
df_users <- df_users[appid %in% df_juegos$appid]
uniqueN(df_users$author.steamid)

## Uno ambos data
df_total <- left_join(x = df_users, y = df_juegos, by = "appid")
rm(df_users, df_juegos); invisible(gc())

length(unique(df_total$author.steamid)) #919081 usuarios

df_total <- left_join(x = df_total,
          y = df_total[,.(recommendations = .N, positive = sum(voted_up), negative = sum(voted_up == FALSE), rating = round(sum(voted_up) / .N, 4)), by = appid],
          by = "appid")

uniqueN(df_total$appid)


## Creo el SteamRatings y filtro por aquellos usuarios con almenos 10 reseñas
steamRatings <- as(df_total[,.(author.steamid, appid, voted_up = as.numeric(voted_up))],"realRatingMatrix") # 919081 (usuarios) x 3691 (peliculas) x 1933138 (ratings)
steamRatings_binarize <- binarize(steamRatings, minRating=1) # 919081 (usuarios) x 3691 (peliculas) x 1797727 (ratings)
steamRatings_binarize <- steamRatings_binarize[rowCounts(steamRatings_binarize) > 10] # 14058 (usuarios) x 3691 (peliculas) x 259499 (ratings)


set.seed(456) # Creador del esquema de evaluacion con un train del 0.8, validacion cruzada de 10 capas
esquema <- evaluationScheme(steamRatings_binarize, method='cross-validation', k=5, given=10, train = 0.8)


algoritmos <- list(RANDOM = list(name = "RANDOM", param = NULL),
                   POPULAR = list(name = "POPULAR", param = NULL),
                   ALS = list(name="ALS"),
                   AR = list(name="AR", param = list(supp = 0.001, conf = 0.35, maxlen = 2)),
                   UBCF = list(name="UBCF", param=list(method="Jaccard")),
                   IBCF = list(name="IBCF", param=list(method="Jaccard")),
                   HYBRID = list(name = "HYBRID", param =
                                   list(recommenders = list(
                                     AR = list(name = "AR", param = list(supp = 0.001, conf = 0.35, maxlen = 2)), IBCF = list(name = "IBCF", param = list(method="Jaccard"))),
                                     weights = c(0.75, 0.25)))
)


Sys.time()
system.time(resultados <- evaluate(x = esquema, method = algoritmos, n = c(3, 5, 7, 10, 15)))
Sys.time()
lubridate::seconds_to_period(27194) #"7H 33M 14S"
df_result <- avg(resultados); #saveRDS(object = resultados, "resultados.RDS")
as.data.frame(df_result$RANDOM)

resultados <- readRDS("resultados.RDS")

recommenderlab::summary(resultados)

kbl() %>% kable_classic_2(full_width = F)

bind_rows(as.data.frame(df_result$RANDOM), as.data.frame(df_result$POPULAR), as.data.frame(df_result$ALS),
          as.data.frame(df_result$AR), as.data.frame(df_result$UBCF), as.data.frame(df_result$IBCF),
          as.data.frame(df_result$HYBRID)) %>%
  kbl(caption = "Algoritmos") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("RANDOM", 1,  5) %>%
  pack_rows("POPULAR",6, 10) %>%
  pack_rows("ALS",   11, 15) %>%
  pack_rows("AR",    16, 20) %>%
  pack_rows("UBCF",  21, 25) %>%
  pack_rows("IBCF",  26, 30) %>%
  pack_rows("HYBRID",31, 35)
  
par(mfrow = c(1,2))
plot(resultados, legend="topleft", annotate=T)
plot(resultados,"prec/rec")

# Ajusto el modelo
algoritmos_final <- list(AR = list(name="AR", param = list(supp = 0.001, conf = 0.35, maxlen = 2)),
                         IBCF = list(name="IBCF", param=list(method="Jaccard")))

system.time({model <- Recommender(getData(esquema,"train"), method = "HYBRID",
                                  parameter = list(recommenders = algoritmos_final, weights = c(.75, .25)))}) #20M 35S

# Realizo la predicción en Test
system.time(prediccion_top <- predict(object = model, newdata = getData(esquema, "known"), n = 20, type="topNList"))
prediccion <- as(prediccion_top, "list")

# Verifico las metricas generales
round(calcPredictionAccuracy(prediccion_top, getData(esquema, "unknown"), given = 10), 5)

# Metricas por usuario
metrics_user <- as.data.table(calcPredictionAccuracy(prediccion_top, getData(esquema, "unknown"), given = 0, byUser = TRUE))
metrics_user$steamid <- unique(getData.frame(getData(esquema, "unknown"))$user)
metrics_user[steamid =="76561198219643357"]


dt_train <- as.data.table(getData.frame(getData(esquema, "train")))
dt_test <- as.data.table(getData.frame(getData(esquema, "unknown")))

uniqueN(dt_train$user) #11244
uniqueN(dt_test$user) #2814
uniqueN(df_steam_ratings$user) #14058
11244+2814

unique_games <- distinct(df_total[,.(appid, name, genres)])
user_df_sample <- left_join(df_steam_ratings[user == "76561198219643357",.(user, appid = as.integer(item))], unique_games, by = "appid") %>%
  mutate(esquema = "total")#27 juegos

bind_rows(
  left_join(dt_test[user == "76561198219643357",.(user, appid = as.integer(item))], unique_games, by = "appid") %>% mutate(esquema = "test"), #17 juegos
  distinct(df_total[appid %in% prediccion[[which(metrics_user$steamid == "76561198219643357")]], .(appid, name, genres)]) %>%
    mutate(user = "76561198219643357", name, genres, esquema = "predict")) %>% arrange(appid)

user_df_predict <- distinct(df_total[appid %in% prediccion[[which(metrics_user$steamid == "76561198219643357")]], .(appid, name, genres)]) %>%
  mutate(user = "76561198219643357", name, genres, esquema = "predict")

bind_rows(user_df_sample, user_df_predict) %>% arrange(appid)
bind_rows(user_df_t, user_df_predict) %>% arrange(appid)

# Realizo la prediccion sobre todo el conjunto de datos
system.time(prediccion_total <- as(predict(object = model, newdata = steamRatings_binarize, n = 10, type="topNList"), "list"))
length(prediccion_total)


df_steam_ratings <- as.data.table(getData.frame(from = steamRatings_binarize))
df_predict_user <- distinct(df_steam_ratings[,.(user)])


df_predict_user <- left_join(x = data.frame(appid = unlist(prediccion_total), author.steamid = rep(x = df_predict_user$user, each = 10)) %>% mutate(appid = as.integer(appid)),
                             y = distinct(df_total[,.(appid, name, header_image, genres, developers, release_date, recommendations, positive, negative, rating)]),
                            by = "appid")

ejemplo_usuario <- filter(df_predict_user, author.steamid == "76561197960291325")

tabla2 <- ejemplo_usuario %>% 
  mutate(image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
  arrange(-rating) %>% 
  #top_n(5,wt = rating) %>% 
  select(image, name, genres, developers, release_date, recommendations, rating)

datatable(data = tabla2, class = "nowrap hover row-border", rownames = TRUE,
          escape = FALSE, options = list(dom = 't',scrollX = FALSE, autoWidth = TRUE,
                                         initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))


saveRDS(object = df_total,file = "recommender_systems/df_total.RDS")
saveRDS(object = df_predict_user,file = "recommender_systems/df_predict_user.RDS")


df_user_info <- as.data.table(readRDS(file = "recommender_systems/df_user_info.RDS"))
df_total <- as.data.table(readRDS(file = "recommender_systems/df_total.RDS"))
df_total <- df_total[author.steamid %in% df_user_info$steamid]
saveRDS(object = df_total,file = "recommender_systems/df_total.RDS")
