## Cargo mis paquetes
library(data.table)
library(dplyr)
library(feather)
library(stringr)
library(ggplot2)
library(DT)
library(recommenderlab)
library(kableExtra)

## Cargo las bases de datos de juegos
df_juegos <- readRDS(file = "data/df_juegos.RDS")
df_juegos <- df_juegos[,.(appid = steam_appid, name, header_image, developers, release_date, genres)]

head(df_juegos) %>%
  kbl(caption = "Base 1. Información de los videojuegos") %>%
  kable_classic(full_width = F, html_font = "Cambria")

## Cargo la base de datos de usuarios
df_users <- readRDS(file = "data/df_users.RDS")
df_users <- df_users[order(appid, author.steamid),.(appid, author.steamid, voted_up)]

head(df_users) %>%
  kbl(caption = "Base 2. Información de los usuarios") %>%
  kable_classic(full_width = F, html_font = "Cambria")


## Uno ambos data
df_total <- left_join(x = df_users, y = df_juegos, by = "appid")
rm(df_users, df_juegos); invisible(gc())

length(unique(df_total$author.steamid)) #817226 usuarios

df_total <- left_join(x = df_total,
                      y = df_total[,.(recommendations = .N, positive = sum(voted_up), negative = sum(voted_up == FALSE), rating = round(sum(voted_up) / .N, 4)), by = appid],
                      by = "appid")

dumies <- fastDummies::dummy_columns(.data = distinct(df_total[,.(appid, genres)])[,2], select_columns = "genres", split = ";")[,-1]
colnames(dumies) <- gsub(pattern = "genres_", replacement = "", x = colnames(dumies))
dumies <- bind_cols(distinct(df_total[,.(appid)]), dumies)
head(dumies,3)


res <- as.data.frame(str_split_fixed(string = df_total$genres, pattern = ";", n = 11)); df_total$genres <- NULL
df_total <- left_join(x = df_total, y = dumies, by = "appid")


## Algunos descriptivos
df_total %>% ggplot(aes(x = voted_up, fill = factor(voted_up))) +
  geom_bar(color = "grey20") + scale_fill_brewer(palette = "YlGnBu") + guides(fill = FALSE) + theme_bw()

prop.table(table(df_total$voted_up))

df_total %>% 
  group_by(author.steamid) %>% 
  summarize(number_of_ratings_per_user = n()) %>% 
  ggplot(aes(number_of_ratings_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "grey20") + coord_cartesian(c(0, 15))


df_total %>% 
  group_by(appid) %>% 
  summarize(number_of_ratings_per_game = n()) %>%
  arrange(desc(number_of_ratings_per_game)) %>%
  filter(number_of_ratings_per_game < 100) %>%
  ggplot(aes(number_of_ratings_per_game)) + 
  geom_bar(fill = "orange", color = "grey20", width = 1)


generos <- as.data.frame(sort(prop.table(table(c(res[,1], res[,2], res[,3], res[,4], res[,5], res[,6], res[,7], res[,8], res[,9], res[,10], res[,11]))[-1])))
colnames(generos) <- c("title", "percentage")

ggplot(data=filter(generos, percentage > 0.0005), aes(x=title, y=percentage, fill = percentage)) + geom_bar(stat="identity") + 
  coord_flip() + scale_fill_distiller(palette = 'YlGnBu', direction = 1) + labs(y = 'Percentage', x = 'Genre') + theme_minimal()
sort(prop.table(table(c(res[,1], res[,2], res[,3], res[,4], res[,5], res[,6], res[,7], res[,8], res[,9], res[,10], res[,11]))[-1]))

df_total[,.(n = .N), by = appid][order(-n)] # Peliculas con más de 100 reseñas
df_total[,.(n = .N), by = author.steamid][order(-n)]

datatable(df_total[author.steamid == '76561199136513271',.(author.steamid, appid, name, voted_up, release_date)])



steamRatings <- as(df_total[,.(author.steamid, appid, voted_up = as.numeric(voted_up))],"realRatingMatrix") # 817226 (usuarios) x 3776 (peliculas) x 1267743 (ratings)
steamRatings_binarize <- binarize(steamRatings, minRating=1) # 817226 (usuarios) x 3776 (peliculas) x 986289 (ratings)
## Filtro por aquellos usuarios con almenos 10 reseñas
steamRatings_binarize <- steamRatings_binarize[rowCounts(steamRatings_binarize) > 10] # 4377 (usuarios) x 3776 (peliculas) x 91905 (ratings)

set.seed(456) # Creador del esquema de evaluacion con un train del 0.9, validacion cruzada de 10 capas
esquema <- evaluationScheme(steamRatings_binarize, method='cross-validation', k=5, given=10, train = 0.8)


# 1) Artículos populares (POPULAR) es un algoritmo no personalizado que recomienda a todos los usuarios los artículos más
#    populares que aún no han calificado.

# 2) Elementos elegidos aleatoriamente (ALEATORIO) crea recomendaciones aleatorias que se pueden utilizar como referencia
#    para la evaluación del algoritmo de recomendación.

# 3) El filtrado colaborativo basado en el usuario (UBCF) predice las calificaciones agregando las calificaciones de los usuarios que
#    tienen un historial de calificación similar al del usuario activo.

# 4) El filtrado colaborativo basado en elementos (IBCF) utiliza la similitud de elemento a elemento en función de las calificaciones
#    de los usuarios para encontrar elementos similares a los elementos que le gustan al usuario activo.

algoritmos <- list(RANDOM = list(name="RANDOM", param=NULL),
                   POPULAR = list(name="POPULAR", param=NULL),
                   ALS = list(name="ALS"),
                   AR = list(name="AR", param = list(supp = 0.001, conf = 0.2, maxlen = 2)),
                   UBCF = list(name="UBCF", param=list(method="Jaccard")),
                   IBCF = list(name="IBCF", param=list(method="Jaccard"))) 

system.time(resultados <- evaluate(x = esquema, method = algoritmos, n = c(5,10)))
lubridate::seconds_to_period(4003) # 46 M 20S
avg(resultados)

par(mfrow = c(1,2))
plot(resultados, legend="topleft", annotate=T)
plot(resultados,"prec/rec")

#HybridRecommender

algoritmos_final <- list(POPULAR = list(name="POPULAR", param=NULL),
                         AR = list(name="AR", param = list(supp = 0.001, conf = 0.2, maxlen = 2)),
                         UBCF = list(name="UBCF", param=list(method="Jaccard")),
                         IBCF = list(name="IBCF", param=list(method="Jaccard"))) 

system.time({recom <- Recommender(getData(esquema, "train"), method = "HYBRID",
                                  parameter = list(recommenders = algoritmos_final, weights = c(.05, .6, .1, .25)))})
lubridate::seconds_to_period(370) #6M 10S
getModel(recom)

system.time(prediccion_top <- predict(object = recom, newdata = getData(esquema, "known"), n = 5, type="topNList"))
prediccion <- as(prediccion_top, "list");lubridate::seconds_to_period(209) #3M 29S

distinct(df_total[,.(appid, name)])[appid %in% prediccion[[1]]]

round(calcPredictionAccuracy(prediccion_top, getData(esquema, "unknown"), given = 10), 5)
cosa <- calcPredictionAccuracy(prediccion_top, getData(esquema, "unknown"), given = 10, byUser = TRUE)
round(apply(cosa, 2, mean), 5)

system.time(prediccion_total <- as(predict(object = recom, newdata = steamRatings_binarize, n = 5, type="topNList"), "list"))
length(prediccion_total)

df_steam_ratings <- as.data.table(getData.frame(from = steamRatings_binarize))
df_predict_user <- distinct(df_steam_ratings[,.(user)])

df_predict_user <- left_join(x = data.frame(appid = unlist(prediccion_total), author.steamid = rep(x = df_predict_user$user, each = 5)) %>% mutate(appid = as.integer(appid)),
                             y = left_join(x = df_juegos, y = distinct(df_total[,.(appid, recommendations, positive, negative, rating)]), by = "appid"),
                             by = "appid")

datatable(as.data.frame(t(df_predict_user[1:5,-2]))[,1:2])
