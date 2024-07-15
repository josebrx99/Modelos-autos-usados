library(dplyr); library(rvest); library(httr); library(xml2); library(curl); library("writexl"); library(readr)


# Creando carpeta para guardas archivos
nombre_carpeta = "temporal"
if (!dir.exists(nombre_carpeta)) {
  dir.create(nombre_carpeta)
}

# 1. Extraer las fichas de producto de las urls de las principales marcas
marcas = c("chevrolet", "renault","mazda", "kia", "nissan", "toyota", "ford", "mercedes-benz", 
           "volkswagen", "audi", "bmw","honda", "hyundai","suzuki")
for (iteracion in 1:5) {
  # Se hacen varios iteraciones o intentos para extraer las ofertas de cada pagina, ya que aveces
  # se bloquea la conexion con el sitio web
  enlaces_fichas = c()
  for (j in 1:length(marcas)) {
    seq = seq(49, 42*48, 48) # hasta 42 paginas por marca
    urls = c()
    urls_seq = c()
    for (i in 1:length(seq)) {
      urls_seq[i] = paste0("https://carros.mercadolibre.com.co/", marcas[j], "/valle-del-cauca/cali", "/_Desde_",seq[i],"_NoIndex_True")
    }
    urls = c(urls, urls_seq, paste0("https://carros.mercadolibre.com.co/", marcas[j], "/valle-del-cauca/cali/_NoIndex_True"))
    
    enlaces_temp = c()
    for (i in 1:length(urls)) {
      tryCatch({
        url = urls[i]
        pag = read_html(url)
        enlaces_f = pag %>% html_nodes(".ui-search-result__content") %>% html_nodes("a") %>% html_attr("href")
        enlaces_temp = c(enlaces_f, enlaces_temp)
      }, error=function(e){})
    }
    enlaces_fichas = c(enlaces_fichas, enlaces_temp)
    cat(marcas[j], length(enlaces_temp), "\n") # Numero de ofertas
  }
  write.csv2(enlaces_fichas, paste0("temporal/iter_", paste0(iteracion, ".csv")), fileEncoding = "UTF-8", quote = FALSE, na = "") # Se guardan los enlaces de todas las paginas
}
archivos_csv = list.files(path = "temporal", pattern = "\\.csv$", full.names = TRUE)
enlaces_fichas = do.call(rbind, lapply(archivos_csv, read.csv, sep = ";"))
enlaces_fichas = unique(enlaces_fichas[,2])
file.remove(archivos_csv)


# 1.1. Extrayendo fichas de productos para marcas que solo tienen una paginacion
urls = c("https://carros.mercadolibre.com.co/jac/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D2839844%26applied_value_name%3DJAC%26applied_value_order%3D32%26applied_value_results%3D12%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/zotye/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D2103728%26applied_value_name%3DZotye%26applied_value_order%3D61%26applied_value_results%3D6%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/skoda/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D2317930%26applied_value_name%3DSkoda%26applied_value_order%3D52%26applied_value_results%3D3%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/volvo/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D60658%26applied_value_name%3DVolvo%26applied_value_order%3D59%26applied_value_results%3D14%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/subaru/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D60285%26applied_value_name%3DSubaru%26applied_value_order%3D54%26applied_value_results%3D19%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/ssangyong/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D378447%26applied_value_name%3DSsangyong%26applied_value_order%3D53%26applied_value_results%3D20%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/mitsubishi/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D1138%26applied_value_name%3DMitsubishi%26applied_value_order%3D43%26applied_value_results%3D42%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/peugeot/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D60279%26applied_value_name%3DPeugeot%26applied_value_order%3D47%26applied_value_results%3D35%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/valle-del-cauca/cali/_BRAND_113321_NoIndex_True#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D113321%26applied_value_name%3DMustang%26applied_value_order%3D44%26applied_value_results%3D1%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/marca-ram/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D2710997%26applied_value_name%3DRAM%26applied_value_order%3D49%26applied_value_results%3D4%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/porsche/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D56870%26applied_value_name%3DPorsche%26applied_value_order%3D48%26applied_value_results%3D10%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/mg/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D64522%26applied_value_name%3DMG%26applied_value_order%3D41%26applied_value_results%3D3%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/opel/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D61705%26applied_value_name%3DOPEL%26applied_value_order%3D46%26applied_value_results%3D3%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/mini/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D65127%26applied_value_name%3DMINI%26applied_value_order%3D42%26applied_value_results%3D24%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/land-rover/valle-del-cauca/cali/_NoIndex_True#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D66655%26applied_value_name%3DLand+Rover%26applied_value_order%3D35%26applied_value_results%3D2%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/lexus/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D71552%26applied_value_name%3DLexus%26applied_value_order%3D37%26applied_value_results%3D6%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/citroen/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D389169%26applied_value_name%3DCitro%C3%ABn%26applied_value_order%3D12%26applied_value_results%3D10%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/zhidou/valle-del-cauca/cali/_NoIndex_True#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D17651956%26applied_value_name%3DZhidou%26applied_value_order%3D60%26applied_value_results%3D2%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/jeep/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D60395%26applied_value_name%3DJeep%26applied_value_order%3D33%26applied_value_results%3D37%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/valle-del-cauca/cali/_BRAND_17525505_NoIndex_True#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D17525505%26applied_value_name%3Dcupra%26applied_value_order%3D13%26applied_value_results%3D2%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/great-wall/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D1119493%26applied_value_name%3DGreat+Wall%26applied_value_order%3D25%26applied_value_results%3D4%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/fiat/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D67781%26applied_value_name%3DFIAT%26applied_value_order%3D21%26applied_value_results%3D13%26is_custom%3Dfalse%26view_more_flag%3Dtrue",
         "https://carros.mercadolibre.com.co/dodge/valle-del-cauca/cali/#applied_filter_id%3DBRAND%26applied_filter_name%3DMarca%26applied_filter_order%3D3%26applied_value_id%3D66708%26applied_value_name%3DDodge%26applied_value_order%3D17%26applied_value_results%3D31%26is_custom%3Dfalse%26view_more_flag%3Dtrue")
enlaces_fichas2 = c()
for (i in 1:length(urls)) {
    tryCatch({
      url = urls[i]
      pag = read_html(url)
      enlaces_f = pag %>% html_nodes(".ui-search-result__content") %>% html_nodes("a") %>% html_attr("href")
      enlaces_fichas2 = c(enlaces_fichas2, enlaces_f)
    }, error=function(e){})
}
enlaces_fichas_final = c(enlaces_fichas,enlaces_fichas2) # Se unen las paginas de las marcas con varias paginaciones y las que tienen una sola pagina
cat(rep("=", 50), "\n")
cat("Total de autos:", length(enlaces_fichas_final))
enlaces_fichas_final = unique(enlaces_fichas_final)
cat("Total de autos despues de borrar duplicados:", length(enlaces_fichas_final))


# 2. Crawler para cada ficha de producto
analizar_ficha = function(enlace) {
  pag_ficha = read_html(enlace)
  nombre = pag_ficha %>% html_nodes(".ui-pdp-title") %>% html_text()
  precio = pag_ficha %>% html_nodes(xpath = '//*[@id="price"]/div/div/div/span/span/span[2]') %>% html_text()
  id = pag_ficha %>% html_nodes(xpath = '//*[@id="denounce"]/div/p/span') %>% html_text()
  ciudad = pag_ficha %>% html_nodes(xpath = '//*[@id="seller_profile"]/ul/div[2]/div/p') %>% html_text() 
  if (length(ciudad) == 0) {
    ciudad = pag_ficha %>% html_nodes(xpath = '//*[@id="seller_profile"]/ul/div[1]/div/p') %>% html_text() 
  } else {
    ciudad = ciudad
  }
  fecha = pag_ficha %>% html_nodes("#header > div > div.ui-pdp-header__subtitle > span") %>% html_text()
  atributos_columns = c("Marca", "Modelo", "Año", "Versión", "Color", "Tipo de combustible", "Puertas", "Transmisión", "Motor",
                        "Tipo de carrocería", "Kilómetros")
  atributos = pag_ficha %>% html_nodes(".andes-table") %>% html_table()
  
  for (i in 1:8) {         # Reintentar k veces extraer la url
    if (length(atributos) == 0) {
      pag_ficha = read_html(enlace)
      atributos = pag_ficha %>% html_nodes(".andes-table") %>% html_table() 
    } else {
      break
    }
  }
  if (length(atributos) != 0) { 
    atributos = as.data.frame(atributos)
    atributos = t(atributos)
    for (i in 1:11) {
      if (atributos[1,i] != atributos_columns[i]) {
        atributos = cbind(atributos[ ,1:i-1], c(atributos_columns[i], NA),atributos[ ,(i):ncol(atributos)]) 
      } else {
        next
      }
    }  
    atributos = as.data.frame(atributos)
    colnames(atributos) = atributos[1,]
    atributos = atributos[-1, ]
    
  } else {
    atributos = as.data.frame(t(rep(0, 11))); colnames(atributos) = atributos_columns
  }
  estado = ifelse(atributos[1,1] == 0, 0, 1)
  
  data = data.frame(nombre = nombre, 
                   precio = precio, 
                   ciudad = ciudad, 
                   atributos,
                   id = id,
                   fecha = fecha,
                   enlace = enlace,
                   estado = estado)
  names(data)[9] = "Tipo de combustible"
  names(data)[13] = "Tipo de carrocería"
}

## Se escrapean las urls, guardando las ofertas a las que no se pudo estraer informacion
## Debido a bloqueo
enlaces_fichas = enlaces_fichas_final
df = analizar_ficha(enlaces_fichas[1])
df = data.frame()
for (j in 1:length(enlaces_fichas)) {
  time = NULL
  stime = Sys.time()
  tryCatch({
    registro = analizar_ficha(enlaces_fichas[j])
    df = rbind(df, registro)
    ftime = Sys.time()
    time[j] = as.numeric(ftime - stime)*registro[,18]
  }, error=function(e){})
  rownames(df) = NULL
  print(j)
}
df = df[!duplicated(df$enlace),]

write.csv2(df, "temporal/autos.csv", fileEncoding = "UTF-8", quote = FALSE, na = "", row.names=FALSE)


# 3. Se recalculan las ofertas en las que no fue posible obtener información
missing = df %>% filter(Marca == 0) %>% select(enlace); df = df %>% filter(Marca != 0)
missing = missing$enlace
enlaces_fichas_miss = missing
df_miss = analizar_ficha(enlaces_fichas_miss[1])
df_miss = data.frame()
for (j in 1:length(enlaces_fichas_miss)) {
  time = NULL
  stime = Sys.time()
  tryCatch({
    registro = analizar_ficha(enlaces_fichas_miss[j])
    df_miss = rbind(df_miss, registro)
    ftime = Sys.time()
    time[j] = as.numeric(ftime - stime)*registro[,18]
  }, error=function(e){})
  rownames(df_miss) = NULL
  print(j)
}
df_miss = df_miss[!duplicated(df_miss$enlace),]
df_final = rbind(df, df_miss)


# 4. Guardar la data
rownames(df_final) = c(1:nrow(df_final))
options(encoding="utf-8")
write.csv2(df, "autos.csv", fileEncoding = "UTF-8", quote = FALSE, na = "")


















