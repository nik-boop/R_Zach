workpath = ""
rootdir = "NikKor"
workdir = "Магазин"
marcetname = "Магазин"


workpath <- paste(workpath, rootdir, workdir, sep='/')



setwd("/")
dir.create(rootdir)

dir.create(workpath)
setwd(workpath)
dir.create(paste(workpath, 'Анализ', sep='/'))
goodslist <- read_excel(paste0(workpath, '/Анализ/','Goods.xlsx'))

# -------Создание папок магазинов-------
dir_m = list()

for (i in 1:10){
  dir_m[i] = paste0(marcetname, '', i)
  dir.create(paste(workpath, dir_m[i], sep='/'))
  
}

# -------Генерация данных-------

generate_data <- function (way = '', goods = goodslist, file.name = "Магазин",
                            days = 7, sale.level = 70, supplies = NaN, sale.dev = 0.05) {
  
  col <- list()
  sale.level = sale.level / 100
  tabl = data.frame('День' = 1:days)
  
  for (i in 1:length(goods$name)) {
    if (typeof(supplies) != "list") {
      col[i] <- list(as.integer(runif(n = days, min = goods$min[i], max = goods$max[i])))
    }
    else {
      col[i] <- list(
        as.integer(supplies[[i]] * runif(n = days, 
                                      min = (sale.level - sale.dev*sale.level**2),
                                      max = (sale.level + sale.dev - sale.dev*sale.level**2)
                                      )
                   )
        )
    }
    
    tabl[i] = col[i]
    colnames(x = tabl)[i] = goods$name[i]
  }
  print(tabl)
  
  name_file <- ifelse(typeof(supplies) != "list",
                      paste0(way, file.name, '.in'),
                      paste0(way, file.name, '.out'))
  
  write.csv(x = tabl,
            file = name_file,
            row.names=FALSE
            )
  
  return(col)
}

sapply(c(1:10), function(i){
  var <- generate_data(paste(workpath, dir_m[i], sep='/', end=''))
  generate_data(paste(workpath, dir_m[i], sep='/', end=''), supplies = var)
  file.copy(paste(workpath, paste0(dir_m[i],'/Магазин.in'), sep='/'),
            paste0(workpath,'/', 'Анализ/', paste0(dir_m[i], '_', marcetname, end='.in')), overwrite=TRUE)
  file.copy(paste(workpath, paste0(dir_m[i],'/Магазин.out'), sep='/'),
            paste0(workpath,'/', 'Анализ/', paste0(dir_m[i], '_', marcetname, end='.out')), overwrite=TRUE)})





