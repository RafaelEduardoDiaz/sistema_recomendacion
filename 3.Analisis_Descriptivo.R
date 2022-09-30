## Cargo mis paquetes
library(data.table)
library(dplyr)
library(feather)
library(stringr)
library(ggplot2)
library(DT)
library(recommenderlab)
library(kableExtra)
library(fst)

## Cargo las bases de datos de juegos
df_juegos <- read.fst(path = "recommender_systems/df_juegos.fst", as.data.table = TRUE)
df_juegos <- df_juegos[,.(appid, name, header_image, developers, release_date, genres, recommendations, rating, price)]


## Cargo la base de datos de usuarios
df_users <- readRDS(file = "data/df_users.RDS")
df_users <- df_users[order(appid, author.steamid),.(appid, author.steamid, voted_up)]
df_users <- df_users[appid %in% df_juegos$appid]
uniqueN(df_users$author.steamid)

## Uno ambos data
df_total <- left_join(x = df_users, y = df_juegos, by = "appid")
#rm(df_users, df_juegos); invisible(gc())

length(unique(df_total$author.steamid)) #919081 usuarios

df_total <- left_join(x = df_total,
                      y = df_total[,.(recommendations = .N, positive = sum(voted_up), negative = sum(voted_up == FALSE), rating = round(sum(voted_up) / .N, 4)), by = appid],
                      by = "appid")

uniqueN(df_total$appid)


head(df_juegos) %>%
  kbl(caption = "Base 1. Información de los videojuegos") %>%
  kable_classic(full_width = F, html_font = "Cambria")


head(df_users) %>%
  kbl(caption = "Base 2. Información de las reseñas") %>%
  kable_classic(full_width = F, html_font = "Cambria")

head(df_user_info) %>% select(-avatarfull, -profileurl, -primaryclanid, -loccityid) %>%
  kbl(caption = "Base 3. Información de los usuarios") %>%
  kable_classic(full_width = F, html_font = "Cambria")

head(df_predict_user[,c(1:3,5)]) %>% 
  kbl(caption = "Base 4. Información de las predicciones") %>%
  kable_classic(full_width = F, html_font = "Cambria")



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
  filter(number_of_ratings_per_game < 250) %>%
  ggplot(aes(number_of_ratings_per_game)) + 
  geom_bar(fill = "orange", color = "grey20", width = 1)


generos <- as.data.frame(sort(prop.table(table(c(res[,1], res[,2], res[,3], res[,4], res[,5], res[,6], res[,7], res[,8], res[,9], res[,10], res[,11]))[-1])))
colnames(generos) <- c("title", "percentage")

ggplot(data=filter(generos, percentage > 0.0005), aes(x=title, y=percentage, fill = percentage)) + geom_bar(stat="identity") + 
  coord_flip() + scale_fill_distiller(palette = 'YlGnBu', direction = 1) + labs(y = 'Percentage', x = 'Genre') + theme_minimal()
sort(round(sort(prop.table(table(c(res[,1], res[,2], res[,3], res[,4], res[,5], res[,6], res[,7], res[,8], res[,9], res[,10], res[,11]))[-1])) * 100, 3))

df_total[,.(n = .N), by = appid][order(-n)] # Peliculas con más de 100 reseñas
df_total[,.(n = .N), by = author.steamid][order(-n)]

#datatable(df_total[author.steamid == '76561199136513271',.(author.steamid, appid, name, voted_up, release_date)])



tabla1 <- df_juegos %>% 
  mutate(image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
  arrange(-recommendations) %>% 
  top_n(5,wt = recommendations) %>% 
  select(image, name, genres, release_date, recommendations, rating, price)

tabla1$rating <- round(tabla1$rating, 3)

datatable(data = tabla1, class = "nowrap hover row-border", rownames = TRUE,
          escape = FALSE, options = list(dom = 't',scrollX = FALSE, autoWidth = TRUE,
                                         initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))


tabla2 <- df_juegos %>% 
  mutate(image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
  arrange(rating) %>% 
  top_n(6,wt = rating) %>% .[2:6] %>%
  select(image, name, genres, release_date, recommendations, rating, price)

datatable(data = tabla2, class = "nowrap hover row-border", rownames = TRUE,
          escape = FALSE, options = list(dom = 't',scrollX = FALSE, autoWidth = TRUE,
                                         initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))


tabla3 <- df_juegos %>% 
  mutate(image = paste0('<img src="', header_image, '" width="100" height="80"></img>')) %>% 
  arrange(-rating) %>% 
  top_n(6,wt = -rating) %>%
  select(image, name, genres, release_date, recommendations, rating, price)

tabla3$rating <- round(tabla3$rating, 3)

datatable(data = tabla3, class = "nowrap hover row-border", rownames = TRUE,
          escape = FALSE, options = list(dom = 't',scrollX = FALSE, autoWidth = TRUE,
                                         initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});","}")))

