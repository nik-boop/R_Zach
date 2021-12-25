#install.packages("readxl")
#install.packages('tidyverse')
#install.packages("stringi")
library("readxl")
library('tidyverse')
library('stringr')
library('ggplot2')
library('dplyr')

# ---Обявляем переменные пути
workpath = ""
rootdir = "NikKor"
workdir = "Магазин"
marcetname = "Магазин"

# -- Переходим в рабучую директорию
setwd(paste(workpath, rootdir, workdir, 'Анализ', sep='/'))

getwd()

# -- Получаем информациию о товарах
goodslist <- read_excel('goods.xlsx')

# -- Получаем список имен файлов с информацией
listin <- list.files(pattern = '.in')
listout <- list.files(pattern = '.out')


# -- Получаем данные из файлов
list_in <- list()
list_out <- list()

sapply(listin,
       function(x){
         list_in[x]<<-list(read.csv(x))
       }, simplify=FALSE)

sapply(listout,
       function(x){
         list_out[x]<<-list(read.csv(x))
       }, simplify=FALSE)

# -- Список имен магазинов
namelist <- sapply(strsplit(listin, "_"), function(x) x [1])

marcetin <- attributes(list_in)[["names"]]
marcetout <- attributes(list_out)[["names"]]

# -- Количество товаров
gcol <- goodslist$name%>%length()
data <- data.frame(good = rep(goodslist$name,list_out[[marcetout]][,1]%>%length()))

# -- Функция построения графиков
main <- function(n){
  mnumber <- n
  
  # -- объем продаж
  pls<- sapply(1:length(attributes(list_out[[mnumber]])[["names"]]),
               function(x){list_out[[mnumber]][,x]})
  # -- выручка
  plv <- sapply(1:length(attributes(list_out[[mnumber]])[["names"]]),
                function(x){list_out[[mnumber]][,x]*goodslist$sele_prise[x]})
  # -- прибыль
  plp <- sapply(1:length(attributes(list_out[[mnumber]])[["names"]]),
                function(x){
                  list_out[[mnumber]][,x]*goodslist$sele_prise[x] -
                    list_in[[mnumber]][,x]*goodslist$sb[x] -
                    (list_in[[mnumber]][,x]-list_out[[mnumber]][,x])*goodslist$util[x]
                })
  # -- списание
  plw <- sapply(1:length(attributes(list_out[[mnumber]])[["names"]]),
                function(x){
                  (list_in[[mnumber]][,x]-list_out[[mnumber]][,x])*goodslist$util[x]
                })
  # -- рентабельность
  plr <- sapply(1:length(attributes(list_out[[mnumber]])[["names"]]),
                function(x){
                  (list_out[[mnumber]][,x]*goodslist$sele_prise[x] -
                     list_in[[mnumber]][,x]*goodslist$sb[x] -
                     (list_in[[mnumber]][,x]-list_out[[mnumber]][,x])*goodslist$util[x]) / 
                    (list_in[[mnumber]][,x]*goodslist$sb[x]+
                       (list_in[[mnumber]][,x]-list_out[[mnumber]][,x])*goodslist$util[x])
                })
  
  # -- Преобразование данных в data.frame
  pls <- data.frame(pls)
  names(pls) <- goodslist$name
  pls <- cbind(pls, day = 1:length(pls[,product]))
  
  plv <- data.frame(plv)
  names(plv) <- goodslist$name
  plv <- cbind(plv, day = 1:length(plv[,product]))
  
  plp <- data.frame(plp)
  names(plp) <- goodslist$name
  plp <- cbind(plp, day = 1:length(plp[,product]))
  
  plw <- data.frame(plw)
  names(plw) <- goodslist$name
  plw <- cbind(plw, day = 1:length(plw[,product]))
  
  plr <- data.frame(plr)
  names(plr) <- goodslist$name
  plr <- cbind(plr, day = 1:length(plr[,product]))
  
  # -- Построение графиков по всем товарам
           
 for (product in 1:(goodslist$name%>%length())){
   
   a <- ggplot(
     data = pls,
     aes(x = day,
         y = pls[goodslist$name[product]]%>%unlist())) + 
     geom_line() + 
     geom_point() + 
     labs(y = goodslist$name[product], title = paste(namelist[n], "Обем продаж",goodslist$name[product])) + 
     scale_x_continuous(breaks = 1:7, minor_breaks = 1:7)
   
   a%>% print()
   
   a <- ggplot(
     data = plv,
     aes(x = day,
         y = plv[goodslist$name[product]]%>%unlist())) + 
     
     geom_line() + 
     geom_point() + 
     labs(y = goodslist$name[product], title = paste(namelist[n], "Выручка", goodslist$name[product])) + 
     scale_x_continuous(breaks = 1:7, minor_breaks = 1:7)
   
   a%>% print()
   
   a <- ggplot(
     data = plp,
     aes(x = day,
         y = plp[goodslist$name[product]]%>%unlist())) + 
     
     geom_line() + 
     geom_point() + 
     labs(y = goodslist$name[product], title = paste(namelist[n], "Прибыль", goodslist$name[product])) + 
     scale_x_continuous(breaks = 1:7, minor_breaks = 1:7)
   
   a%>% print()
   
   a <- ggplot(
     data = plw,
     aes(x = day,
         y = plw[goodslist$name[product]]%>%unlist())) + 
     
     geom_line() + 
     geom_point() + 
     labs(y = goodslist$name[product], title = paste(namelist[n], "Списание", goodslist$name[product])) + 
     scale_x_continuous(breaks = 1:7, minor_breaks = 1:7)
   
   a%>% print()
   
   a <- ggplot(
     data = plr,
     aes(x = day,
         y = plr[goodslist$name[product]]%>%unlist())) +
     
     geom_line() + 
     geom_point() + 
     labs(y = goodslist$name[product], title = paste(namelist[n], "Рентабельность", goodslist$name[product])) + 
     scale_x_continuous(breaks = 1:7, minor_breaks = 1:7)
   
   a%>% print()
 }
  
  
}

# -- Список имен магазинов
for (i in 1:length(listin)){
  print(paste(i, ":", namelist[i]))
}



# -- Запуск основной функции
main(scan())
