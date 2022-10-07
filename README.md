# Sistema de Recomendación TFM

El objetivo principal de este trabajo es elaborar el mejor sistema de recomendación en videojuegos, basado en filtros colaborativos que permitan lograr un crecimiento en ventas, además de mejorar la experiencia y satisfacción del usuario en la compra de videojuegos.

## Descripción

Este proyecto encontrara 3 scripts con el código.

+ 1. Extraer la información de las APIS de Steam. Esto incluye:
  - la información general de los juegos, como id, nombre, precio, lenguaje que soporta,...
  - La información de las reseñas, (si le recomienda o no el videojuego).
  - La información de los usuarios, como alias, avatar, nombre real, país, ciudad, idioma, ...
+ 2. Código para ajustar el modelo de recomendación, a partir de una matriz binaria de ratings, donde se probaron 7 algoritmos de filtros colaborativos, bajo validación cruzada de 5 capasa. Se calcularon métricas como: precision, recall, AUC, ...
+ 3. Análisis descriptivo de la base.

Por otra parte, también, se encuentra la presentación y las memorias en:
+ **TFM Grupo 5 - Sistema de Recomendación Steam.pdf** - Corresponde a la ppt del proyecto
+ **TFM_Sistema de Recomendación_Grupo 5.pdf** - Corresponde a las memorias del TFM

Y los estados financieros, más calculo de viabilidad del proyecto en: **Estados Financieros AnalyticSify.xlsx**

Finalmente, en la carpeta **recommender_systems**, se encuentra el código para desplegar el modelo con R Shiny

### Dependencies

* R y Rstudio
* Paquete Recommenderlab

## Ayuda

Este pryecto se contruyo bajo la version de R 4.2.1 y la versión del paquete recommenderlab 1.0.2

## Autor

ex. Rafael Eduardo Díaz

## Version History

* 0.1: Lanzamiento inicial

## Licencias

Este proyecto esta protegido bajo Creative Commons (CC).
