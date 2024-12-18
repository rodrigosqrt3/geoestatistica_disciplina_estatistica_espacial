## ----include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, comment = NA, message = FALSE)


## /* Estilos Gerais */
## body {
##     font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
##     line-height: 1.8;
##     color: #2d3748;
##     max-width: 1200px;
##     margin: 0 auto;
##     padding: 0 20px;
##     background-color: #ffffff;
## }
## 
## /* Cabeçalho do Documento */
## .title {
##     color: #1a365d;
##     font-size: 2.8em;
##     text-align: center;
##     margin: 1.5em 0 0.3em;
##     font-weight: 700;
##     letter-spacing: -0.5px;
## }
## 
## .subtitle {
##     color: #4a5568;
##     text-align: center;
##     font-size: 1.6em;
##     margin-bottom: 0.5em;
##     font-weight: 400;
## }
## 
## .author, .date {
##     color: #718096;
##     text-align: center;
##     font-size: 1.1em;
##     margin: 0.3em 0;
## }
## 
## /* Cabeçalhos */
## h1, h2, h3, h4, h5, h6 {
##     color: #2d3748;
##     font-weight: 600;
##     margin-top: 1.5em;
##     margin-bottom: 0.8em;
## }
## 
## h1 { font-size: 2.2em; border-bottom: 2px solid #edf2f7; padding-bottom: 0.3em; }
## h2 { font-size: 1.8em; }
## h3 { font-size: 1.5em; }
## h4 { font-size: 1.3em; }
## 
## /* Sumário (TOC) */
## #TOC {
##     background-color: #f8fafc;
##     border-radius: 8px;
##     padding: 25px;
##     margin: 20px 0;
##     box-shadow: 0 1px 3px rgba(0,0,0,0.1);
## }
## 
## #TOC::before {
##     content: "Sumário";
##     font-size: 1.2em;
##     font-weight: 600;
##     color: #2d3748;
##     display: block;
##     margin-bottom: 1em;
## }
## 
## .tocify {
##     border: none;
##     border-radius: 8px;
## }
## 
## .tocify .tocify-item a {
##     color: #4a5568;
##     padding: 8px 15px;
##     transition: all 0.2s ease;
## }
## 
## .tocify .tocify-item a:hover {
##     background-color: #edf2f7;
##     color: #2b6cb0;
## }
## 
## /* Código */
## pre {
##     background-color: #f8fafc !important;
##     border-radius: 8px !important;
##     padding: 1em !important;
##     border: 1px solid #e2e8f0 !important;
##     box-shadow: 0 1px 2px rgba(0,0,0,0.05);
## }
## 
## code {
##     font-family: 'Fira Code', 'Consolas', monospace;
##     font-size: 0.9em;
##     padding: 0.2em 0.4em;
##     border-radius: 4px;
##     background-color: #edf2f7;
##     color: #4a5568;
## }
## 
## /* Tabelas */
## table {
##     width: 100%;
##     margin: 2em 0;
##     border-collapse: collapse;
##     border-spacing: 0;
##     border-radius: 8px;
##     overflow: hidden;
##     box-shadow: 0 1px 3px rgba(0,0,0,0.1);
## }
## 
## thead {
##     background-color: #f8fafc;
## }
## 
## th, td {
##     padding: 12px 15px;
##     border: 1px solid #e2e8f0;
## }
## 
## th {
##     background-color: #f8fafc;
##     font-weight: 600;
##     color: #2d3748;
## }
## 
## tr:nth-child(even) {
##     background-color: #f8fafc;
## }
## 
## /* Links */
## a {
##     color: #3182ce;
##     text-decoration: none;
##     transition: color 0.2s ease;
## }
## 
## a:hover {
##     color: #2c5282;
##     text-decoration: underline;
## }
## 
## /* Figuras */
## .figure {
##     margin: 2em 0;
##     text-align: center;
## }
## 
## .figure img {
##     max-width: 100%;
##     height: auto;
##     border-radius: 8px;
##     box-shadow: 0 1px 3px rgba(0,0,0,0.1);
## }
## 
## .figure-caption {
##     color: #718096;
##     font-size: 0.9em;
##     margin-top: 1em;
##     text-align: center;
## }
## 
## /* Botões de download */
## .btn-default {
##     background-color: #edf2f7;
##     border: 1px solid #e2e8f0;
##     border-radius: 6px;
##     padding: 8px 16px;
##     color: #4a5568;
##     transition: all 0.2s ease;
## }
## 
## .btn-default:hover {
##     background-color: #e2e8f0;
##     color: #2d3748;
## }

## ----echo=FALSE, out.width="60%", fig.align='center'-------------------------------------------------
knitr::include_graphics("D:/Estatística Espacial/Seminário 2/647de83e216797e556dd43ba_d9c51afa.png")


## ----------------------------------------------------------------------------------------------------
library(terra)        
library(gstat)        
library(sp)           
library(ggplot2)      
library(geobr)    
library(MODISTools)
library(dplyr)
library(leaflet)
library(INLA)
library(spdep)
library(sf)
library(patchwork)
library(plotly)
library(scales)
library(raster)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(tidyr)


## ----------------------------------------------------------------------------------------------------
criar_grid_pontos <- function(center_lat, center_lon, km_ao_redor, espacamento_km = 2.5) {
  # Converter km para graus (aproximadamente)
  deg_spacing <- espacamento_km / 111  # 1 grau ≈ 111 km
  deg_around <- km_ao_redor / 111
  
  lats <- seq(center_lat - deg_around, center_lat + deg_around, by = deg_spacing)
  lons <- seq(center_lon - deg_around, center_lon + deg_around, by = deg_spacing)
  
  pontos <- expand.grid(latitude = lats, longitude = lons)
  return(pontos)
}


## ----------------------------------------------------------------------------------------------------
download_ndvi_espacial <- function(center_lat, center_lon, date, km_ao_redor = 25, save_csv = TRUE) {
  pontos_grid <- criar_grid_pontos(center_lat, center_lon, km_ao_redor)
  resultados <- data.frame()
  
  for (i in 1:nrow(pontos_grid)) {
    cat(sprintf("Baixando ponto %d de %d\n", i, nrow(pontos_grid)))
    
    tryCatch({
      data <- mt_subset(
        product = "MOD13Q1",
        lat = pontos_grid$latitude[i],
        lon = pontos_grid$longitude[i],
        band = "250m_16_days_NDVI",
        start = date,
        end = date,
        km_lr = 0.5,
        km_ab = 0.5,
        internal = TRUE
      )
      
      if (nrow(data) > 0) {
        point_data <- data.frame(
          longitude = pontos_grid$longitude[i],
          latitude = pontos_grid$latitude[i],
          ndvi = data$value[1] * 0.0001  # converter para valores reais de NDVI
        )
        resultados <- rbind(resultados, point_data)
      }
    }, error = function(e) {
      cat(sprintf("Erro no ponto %d: %s\n", i, e$message))
    })
  }
  
  # !NA
  resultados <- resultados[!is.na(resultados$ndvi), ]
  if(save_csv) {
    filename <- sprintf("ndvi_data_%s.csv", gsub("-", "", date))
    write.csv(resultados, filename, row.names = FALSE)
    cat(sprintf("\nDados salvos em: %s\n", filename))
  }
  
  return(resultados)
}


## ----------------------------------------------------------------------------------------------------
# data <- download_ndvi_espacial(
#   center_lat = -30.03,
#   center_lon = -51.23,
#   date = "2020-01-01",
#   km_ao_redor = 25,  
#   save_csv = TRUE 
# )

# Faltou um pedaço da cidade ao sul, nova busca para adicioná-los e concatená-los.

# dados_sul_cidade <- download_ndvi_espacial(
#   center_lat = -30.27,  
#   center_lon = -51.15,  
#   date = "2020-01-01",
#   km_ao_redor = 5,        
#   save_csv = FALSE      
# )

# dados_novos <- download_ndvi_espacial(
#    center_lat = -30.15,  
#    center_lon = -51.20,  
#    date = "2020-01-01",
#    km_ao_redor = 10,        
#    save_csv = TRUE      
#  )

# dados_novos2 <- download_ndvi_espacial(
#   center_lat = -30.10,
#   center_lon = -51.30,
#   date = "2020-01-01",
#   km_ao_redor = 15,
#   save_csv = TRUE
# )

# dados_novos3 <- download_ndvi_espacial(
#   center_lat = -30.00,
#   center_lon = -51.10,
#   date = "2020-01-01",
#   km_ao_redor = 10,
#   save_csv = TRUE
# )

# dados_novos4 <- download_ndvi_espacial(
#   center_lat = -30.20,
#   center_lon = -51.05,
#   date = "2020-01-01",
#   km_ao_redor = 10,
#   save_csv = TRUE
# )

# dados_novos5 <- download_ndvi_espacial(
#   center_lat = -30.06,
#   center_lon = -51.00,
#   date = "2020-01-01",
#   km_ao_redor = 15,  
#   save_csv = TRUE 
# )

# dados_novos6 <- download_ndvi_espacial(
#    center_lat = -30.12,
#    center_lon = -51.20,
#    date = "2020-01-01",
#    km_ao_redor = 15,  
#    save_csv = TRUE 
# )

# dados_novos7 <- download_ndvi_espacial(
#    center_lat = -29.95,
#    center_lon = -51.25,
#    date = "2020-01-01",
#    km_ao_redor = 15,  
#    save_csv = TRUE 
# )

# dados_novos8 <- download_ndvi_espacial(
#    center_lat = -30.00,
#    center_lon = -51.15,
#    date = "2020-01-01",
#    km_ao_redor = 15,  
#    save_csv = TRUE 
# )

# dados_completos <- rbind(dados_completos, dados_novosx)
 
# dados_completos <- dados_completos[!duplicated(dados_completos[c("latitude", "longitude")]),]

# write.csv(dados_completos, "D:/Estatística Espacial/Seminário 2/complete_ndvi_data_updated.csv", row.names = FALSE)

dados_completos <- read.csv("D:/Estatística Espacial/Seminário 2/complete_ndvi_data_updated.csv")


## ----echo=FALSE--------------------------------------------------------------------------------------
descritivas_ndvi <- dados_completos %>%
  summarise(
    `Média` = mean(ndvi, na.rm = TRUE),
    `Mediana` = median(ndvi, na.rm = TRUE),
    `Desvio Padrão` = sd(ndvi, na.rm = TRUE),
    `Mínimo` = min(ndvi, na.rm = TRUE),
    `Máximo` = max(ndvi, na.rm = TRUE),
    `1º Quartil` = quantile(ndvi, 0.25, na.rm = TRUE),
    `3º Quartil` = quantile(ndvi, 0.75, na.rm = TRUE),
    `Amplitude Interquartil` = IQR(ndvi, na.rm = TRUE),
    `Coeficiente de Variação` = (`Desvio Padrão` / `Média`) * 100,
    `N` = n()
  ) %>%
  pivot_longer(everything(), names_to = "Estatística", values_to = "Valor") %>%
  mutate(Valor = ifelse(Estatística == "N", as.integer(Valor), number(Valor, big.mark = ".", decimal.mark = ",")))

tabela_descritiva <- descritivas_ndvi %>%
  kable(
    format = "html", 
    col.names = c("Estatística", "Valor"),
    align = c("l", "c"), 
    caption = "Estatísticas Descritivas do NDVI",
    escape = FALSE 
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE, 
    position = "center" 
  ) %>%
  row_spec(0, bold = TRUE, background = "#4CAF50", color = "white") %>% 
  column_spec(1, width = "10em", bold = TRUE) %>% 
  column_spec(2, width = "6em") %>% 
  row_spec(1:9, background = "#F8F8FF") %>% 
  row_spec(10, bold = TRUE, background = "#D3D3D3") %>% 
  add_header_above(c(" " = 2)) 

tabela_descritiva


## ----------------------------------------------------------------------------------------------------
dados_hist <- hist(dados_completos$ndvi, breaks = 30, plot = FALSE)
hist_df <- data.frame(
  x = dados_hist$mids,
  count = dados_hist$counts,
  breaks_left = head(dados_hist$breaks, -1),  
  breaks_right = tail(dados_hist$breaks, -1)  
)

hist_df$tooltip <- sprintf(
  "Intervalo: %.3f - %.3f<br>Frequência: %d",
  hist_df$breaks_left,
  hist_df$breaks_right,
  hist_df$count
)

hist_ndvi <- ggplot(hist_df, aes(x = x, y = count, text = tooltip)) +
  geom_col(fill = "#69b3a2", color = "black", alpha = 0.8) +
  labs(title = "Distribuição do NDVI",
       x = "NDVI",
       y = "Frequência") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_continuous(labels = label_number(accuracy = 0.001))

hist_ndvi_plotly <- ggplotly(hist_ndvi, tooltip = "text")
hist_ndvi_plotly


## ----------------------------------------------------------------------------------------------------
boxplot_ndvi <- ggplot(dados_completos, aes(x = "", y = ndvi)) +
  geom_boxplot(fill = "#404080", color = "black", outlier.colour = "red", outlier.shape = 1, alpha = 0.8) +
  labs(title = "Boxplot do NDVI",
       y = "NDVI",
       x = NULL) +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

boxplot_ndvi_plotly <- ggplotly(boxplot_ndvi)
boxplot_ndvi_plotly


## ----------------------------------------------------------------------------------------------------
calcular_moran_global <- function(data, shapefile, grid_size = 0.005) {
  set.seed(sqrt(2))
  pontos_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  
  shapefile <- st_transform(shapefile, 4326)
  bbox <- st_bbox(shapefile)
  
  grid <- st_make_grid(shapefile, 
                      cellsize = c(grid_size, grid_size), 
                      what = "polygons") %>%
    st_sf() %>%
    st_cast("POLYGON")
  
  grid <- st_intersection(grid, shapefile) %>%
    st_make_valid() %>%
    st_cast("POLYGON")
  
  grid <- grid[!st_is_empty(grid), ]
  
  point_grid_index <- st_intersects(pontos_sf, grid, sparse = FALSE)
  data$grid_id <- max.col(point_grid_index)
  
  grid_data <- data %>%
    group_by(grid_id) %>%
    summarise(
      ndvi_mean = mean(ndvi, na.rm = TRUE)
    ) %>%
    filter(!is.na(ndvi_mean))
  
  grid$ndvi_mean <- NA
  grid$ndvi_mean[grid_data$grid_id] <- grid_data$ndvi_mean
  
  grid <- grid[!is.na(grid$ndvi_mean), ]
  
  nb <- poly2nb(grid, queen = TRUE)
  
  if (any(card(nb) == 0)) {
    warning("Há ilhas (células sem vizinhos) na matriz de vizinhança. Isso pode causar erros nos cálculos.")
  }
  
  w <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  moran_test <- moran.test(grid$ndvi_mean, w, zero.policy = TRUE)
  
  moran_perm <- moran.mc(grid$ndvi_mean, w, nsim = 999, zero.policy = TRUE)
  
  moran_scatterplot <- moran.plot(grid$ndvi_mean, w, zero.policy = TRUE, spChk = FALSE, labels = FALSE, xlab = "NDVI", ylab = "NDVI Espacialmente Defasado", quiet = TRUE)
  
  return(list(
    moran_test = moran_test,
    moran_perm = moran_perm,
    moran_scatterplot = moran_scatterplot,
    grid = grid,
    weights = w
  ))
}

calcular_moran_local <- function(moran_resultados) {
  set.seed(sqrt(2))
  local_moran <- localmoran(moran_resultados$grid$ndvi_mean, moran_resultados$weights, zero.policy = TRUE)
  
  moran_resultados$grid$local_i <- local_moran[, "Ii"]
  moran_resultados$grid$p_value <- local_moran[, "Pr(z != E(Ii))"]
  
  return(moran_resultados)
}

plot_moran_resultados <- function(moran_resultados, shapefile) {
  max_abs_value <- max(abs(moran_resultados$grid$local_i), na.rm = TRUE)
  
  pal_moran <- colorNumeric(
    palette = "RdBu",
    domain = c(-max_abs_value, max_abs_value), 
    na.color = "transparent"
  )
  
  pal_pvalue <- colorNumeric(
    palette = "Reds",
    domain = c(0, 1),
    na.color = "transparent"
  )
  
  m1 <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = moran_resultados$grid,
                fillColor = ~pal_moran(local_i),
                fillOpacity = 0.8,
                weight = 0.5,
                color = "#444444",
                popup = ~paste("Local Moran's I:", round(local_i, 3), "<br>P-value:", round(p_value, 3))) %>%
    addPolygons(data = shapefile,
                weight = 2,
                opacity = 1,
                color = "#444444",
                fillOpacity = 0,
                dashArray = "3") %>%
    addLegend(pal = pal_moran,
              values = c(-max_abs_value, 0, max_abs_value), # Valores para a legenda
              title = "Local Moran's I",
              position = "bottomright")
  
  m2 <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = moran_resultados$grid,
                fillColor = ~pal_pvalue(p_value),
                fillOpacity = 0.8,
                weight = ifelse(moran_resultados$grid$p_value < 0.05, 2, 0.5), 
                color = ifelse(moran_resultados$grid$p_value < 0.05, "black", "#444444"), 
                group = "significativo",
                popup = ~paste("Local Moran's I:", round(local_i, 3), "<br>P-value:", round(p_value, 3))) %>%
    addPolygons(data = shapefile,
                weight = 2,
                opacity = 1,
                color = "#444444",
                fillOpacity = 0,
                dashArray = "3") %>%
    addLegend(pal = pal_pvalue,
              values = moran_resultados$grid$p_value,
              title = "P-value",
              position = "bottomright") %>%
    addLayersControl(overlayGroups = c("significativo"),
                     options = layersControlOptions(collapsed = FALSE))
  
  return(list(moran_map = m1, pvalue_map = m2))
}

poa_shape <- read_municipality(code_muni = 4314902, year = 2020)

grid_size <- 0.005

moran_resultados <- calcular_moran_global(dados_completos, poa_shape, grid_size = grid_size)
moran_resultados <- calcular_moran_local(moran_resultados)

print(moran_resultados$moran_test)
print(moran_resultados$moran_perm)

plot_moran_resultados(moran_resultados, poa_shape)$moran_map
plot_moran_resultados(moran_resultados, poa_shape)$pvalue_map


## ----------------------------------------------------------------------------------------------------
criar_modelo_ndvi <- function(data) {
  set.seed(sqrt(2))
  sp_data <- data
  coordinates(sp_data) <- ~longitude+latitude
  proj4string(sp_data) <- CRS("+proj=longlat +datum=WGS84")
  
  vgm <- variogram(ndvi ~ 1, sp_data)
  
  fit <- fit.variogram(vgm, model = vgm("Sph"))
  
  grid <- expand.grid(
    longitude = seq(min(data$longitude), max(data$longitude), length = 50),
    latitude = seq(min(data$latitude), max(data$latitude), length = 50)
  )
  coordinates(grid) <- ~longitude+latitude
  gridded(grid) <- TRUE
  proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
  
  # krigagem
  kriged <- krige(ndvi ~ 1, sp_data, grid, model = fit)
  
  return(list(variogram = vgm, fitted_model = fit, kriged_data = kriged))
}


## ----------------------------------------------------------------------------------------------------
plot_ndvi_resultados <- function(model_resultados, shapefile) {
  
  kriged_df <- as.data.frame(model_resultados$kriged_data)
  
  r <- raster(nrows = 50, ncols = 50)
  extent(r) <- extent(min(kriged_df$longitude), max(kriged_df$longitude),
                     min(kriged_df$latitude), max(kriged_df$latitude))
  r <- rasterize(kriged_df[, c("longitude", "latitude")], r, kriged_df$var1.pred)
  crs(r) <- CRS("+proj=longlat +datum=WGS84")
  
  shapefile_sf <- st_transform(shapefile, 4326)
  
  r_masked <- mask(r, as(shapefile_sf, "Spatial"))
  
  pal <- colorNumeric(
    palette = colorRampPalette(c("#a50026", "#fdae61", "#d9ef8b", "#006837"))(100),
    domain = values(r_masked),
    na.color = "transparent"
  )
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%  # Better base map
    addRasterImage(r_masked, 
                  colors = pal, 
                  opacity = 0.8) %>%
    addPolygons(data = shapefile_sf,
                weight = 2,
                opacity = 1,
                color = "#444444",
                fillOpacity = 0,
                dashArray = "3") %>%
    addLegend(pal = pal,
              values = values(r_masked),
              title = "NDVI Values",
              position = "bottomright",
              labFormat = labelFormat(digits = 2)) %>%
    addScaleBar(position = "bottomleft") %>%
    addMiniMap(position = "bottomleft", 
               toggleDisplay = TRUE)
  
  plot(model_resultados$variogram, 
       model_resultados$fitted_model, 
       main = "NDVI Variogram",
       pch = 19)
  
  return(m)
}


## ----------------------------------------------------------------------------------------------------
model <- criar_modelo_ndvi(dados_completos)


## ----------------------------------------------------------------------------------------------------
validar_krigagem <- function(model) {
  variogram_fit <- model$fitted_model
  
  kriged_data <- model$kriged_data
  predictions <- kriged_data$var1.pred
  variance <- kriged_data$var1.var
  
  nugget <- variogram_fit$psill[1]  # Efeito pepita
  sill <- sum(variogram_fit$psill)  # Patamar total
  range <- variogram_fit$range[2]   # Alcance
  
  structural_variance <- (sill - nugget) / sill
  
  mean_variance <- mean(variance, na.rm = TRUE)
  sd_variance <- sd(variance, na.rm = TRUE)
  
  resultados <- data.frame(
    Metric = c("Nugget", "Sill", "Range", "Structural Variance Ratio",
               "Mean Kriging Variance", "SD Kriging Variance"),
    Value = c(nugget, sill, range, structural_variance,
              mean_variance, sd_variance)
  )
  
  return(list(
    metrics = resultados,
    prediction_summary = summary(predictions),
    variance_summary = summary(variance)
  ))
}

criar_avaliacao_krigagem <- function(metrics) {
  assessment <- data.frame(
    Metrica = c(
      "Efeito Pepita (Nugget)",
      "Patamar (Sill)",
      "Alcance (Range)",
      "Razão de Variância Estrutural",
      "Média da Variância de Krigagem",
      "Desvio Padrão da Variância"
    ),
    Valor = c(
      format(metrics$Value[1], digits = 4),
      format(metrics$Value[2], digits = 4),
      format(metrics$Value[3], digits = 4),
      paste0(format(metrics$Value[4] * 100, digits = 2), "%"),
      format(metrics$Value[5], digits = 4),
      format(metrics$Value[6], digits = 4)
    )
  )
  
  styled_table <- assessment %>%
    kable(format = "html", 
          align = "c", 
          col.names = c("Métrica", "Valor")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE,
                  position = "center") %>%
    column_spec(1, bold = TRUE, color = "white", background = "#4CAF50") %>%
    column_spec(2, color = "#4CAF50", background = "#F0F0F0", width = "5cm") %>%
    add_header_above(c("Avaliação da Krigagem" = 2))
  
  return(styled_table)
}

criar_interpretacao_krigagem <- function(metrics) {
  structural_var_ratio <- metrics$Value[4]
  nugget <- metrics$Value[1]
  mean_variance <- metrics$Value[5]
  
  interpretation <- data.frame(
    Metrica = c(
      "Qualidade do Modelo",
      "Dependência Espacial",
      "Confiabilidade das Predições"
    ),
    Avaliacao = c(
      ifelse(structural_var_ratio > 0.75, "Excelente", 
             ifelse(structural_var_ratio > 0.5, "Boa", "Regular")),
      ifelse(nugget < 0.05, "Forte", 
             ifelse(nugget < 0.15, "Moderada", "Fraca")),
      ifelse(mean_variance < 0.05, "Alta", 
             ifelse(mean_variance < 0.15, "Moderada", "Baixa"))
    )
  )
  
  styled_table <- interpretation %>%
    kable(format = "html", 
          align = "c", 
          col.names = c("Critério", "Avaliação")) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE,
                  position = "center") %>%
    column_spec(1, bold = TRUE, color = "white", background = "#4CAF50") %>%
    column_spec(2, color = "#4CAF50", background = "#F0F0F0", width = "5cm") %>%
    add_header_above(c("Interpretação dos Resultados" = 2))
  
  return(styled_table)
}


## ----echo=FALSE--------------------------------------------------------------------------------------
validacao <- validar_krigagem(model)
tabela_metricas <- criar_avaliacao_krigagem(validacao$metrics)
tabela_interpretacao <- criar_interpretacao_krigagem(validacao$metrics)
tabela_metricas
tabela_interpretacao


## ----------------------------------------------------------------------------------------------------
poa_shape <- read_municipality(code_muni = 4314902, year = 2020)
plot_ndvi_resultados(model, poa_shape)


## ----------------------------------------------------------------------------------------------------
criar_ndvi_icar <- function(data, shapefile) {
  set.seed(sqrt(2))
  pontos_sf <- st_as_sf(data, coords = c("longitude", "latitude"), 
                        crs = 4326)
  
  shapefile <- st_transform(shapefile, 4326)
  
  bbox <- st_bbox(shapefile)
  grid_size <- 0.005
  
  grid <- st_make_grid(shapefile, 
                      cellsize = c(grid_size, grid_size), 
                      what = "polygons") %>%
    st_sf() %>%
    st_cast("POLYGON")
  
  grid <- st_intersection(grid, shapefile) %>%
    st_make_valid() %>%
    st_cast("POLYGON")
    
  grid <- grid[!st_is_empty(grid), ]
  
  grid_buffered <- st_buffer(grid, dist = grid_size/50)
  
  nb <- poly2nb(st_geometry(grid_buffered), 
                queen = TRUE, 
                snap = grid_size/5)
  
  gc <- spdep::n.comp.nb(nb)
  if (gc$nc > 1) {
    cat("Encontrados", gc$nc, "componentes desconectados. Usando o maior componente.\n")
    biggest <- which.max(table(gc$comp.id))
    keep <- gc$comp.id == biggest
    grid <- grid[keep,]
    nb <- poly2nb(st_geometry(grid), 
                  queen = TRUE, 
                  snap = grid_size/5)
  }
  
  W <- nb2mat(nb, style = "B", zero.policy = TRUE)
  
  point_grid_index <- st_intersects(pontos_sf, grid, sparse = FALSE)
  
  if(all(!point_grid_index)) {
    stop("Nenhum ponto no grid, cheque seus dados.")
  }
  
  data$grid_id <- max.col(point_grid_index)
  
  grid_data <- data %>%
    group_by(grid_id) %>%
    summarise(
      ndvi_mean = mean(ndvi, na.rm = TRUE),
      ndvi_sd = sd(ndvi, na.rm = TRUE),
      n = n()
    ) %>%
    filter(!is.na(ndvi_mean))  # Remove cells with no data
  
  n_cells <- nrow(grid)
  
  full_grid_data <- data.frame(
    grid_id = 1:n_cells,
    ndvi_mean = NA,
    ndvi_sd = NA,
    n = 0
  )
  
  full_grid_data[grid_data$grid_id, c("ndvi_mean", "ndvi_sd", "n")] <- 
    grid_data[, c("ndvi_mean", "ndvi_sd", "n")]
  
  formula <- ndvi_mean ~ 1 + f(grid_id, model = "besag", graph = W)
  
  model <- try(inla(
    formula,
    family = "gaussian",
    data = full_grid_data,
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE),
    control.family = list(
      hyper = list(
        prec = list(
          prior = "pc.prec",
          param = c(1, 0.01)  # Ajustar estes valores
        )
      )
    ),
    control.inla = list(strategy = "gaussian")
  ))
  if(inherits(model, "try-error")) {
    stop("O modelo INLA falhou. Cheque seus dados e estrutura espacial.")
  }
  
  return(list(
    model = model,
    grid = grid,
    data = full_grid_data,
    W = W
  ))
}

plot_icar_resultados <- function(icar_resultados, shapefile) {
  grid <- icar_resultados$grid
  grid$fitted <- icar_resultados$model$summary.fitted.values$mean
  
  pal <- colorNumeric(
    palette = colorRampPalette(c("#a50026", "#fdae61", "#d9ef8b", "#006837"))(100),
    domain = grid$fitted,
    na.color = "transparent"
  )
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = grid,
                fillColor = ~pal(fitted),
                fillOpacity = 0.8,
                weight = 0.5,
                color = "#444444") %>%
    addPolygons(data = shapefile,
                weight = 2,
                opacity = 1,
                color = "#444444",
                fillOpacity = 0,
                dashArray = "3") %>%
    addLegend(pal = pal,
              values = grid$fitted,
              title = "NDVI Values (ICAR)",
              position = "bottomright",
              labFormat = labelFormat(digits = 2)) %>%
    addScaleBar(position = "bottomleft") %>%
    addMiniMap(position = "bottomleft", 
               toggleDisplay = TRUE)
  
  return(m)
}


## ----------------------------------------------------------------------------------------------------
icar_model <- criar_ndvi_icar(dados_completos, poa_shape)


## ----fig.align='center', echo=FALSE------------------------------------------------------------------
dic <- icar_model$model$dic$dic
waic <- icar_model$model$waic$waic
log_likelihood <- icar_model$model$mlik[1]

tabela <- data.frame(
  Métrica = c("DIC", "WAIC", "Log-verossimilhança"),
  Valor = c(dic, waic, log_likelihood)
)

tabela_kable <- tabela %>%
  kable(format = "html", align = "c", col.names = c("Métrica", "Valor")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, bold = TRUE, color = "white", background = "#4CAF50") %>%
  column_spec(2, color = "#4CAF50", background = "#F0F0F0", width = "5cm") %>%
  add_header_above(c("Resumo do Modelo" = 2))

tabela_kable


## ----------------------------------------------------------------------------------------------------
plot_icar_resultados(icar_model, poa_shape)


## ----------------------------------------------------------------------------------------------------
criar_ndvi_sar <- function(data, shapefile, rho = 0.5) {
  set.seed(sqrt(2))
  pontos_sf <- st_as_sf(data, coords = c("longitude", "latitude"), 
                        crs = 4326)
  
  shapefile <- st_transform(shapefile, 4326)
  
  bbox <- st_bbox(shapefile)
  grid_size <- 0.005
  
  grid <- st_make_grid(shapefile, 
                      cellsize = c(grid_size, grid_size), 
                      what = "polygons") %>%
    st_sf() %>%
    st_cast("POLYGON")
  
  grid <- st_intersection(grid, shapefile) %>%
    st_make_valid() %>%
    st_cast("POLYGON")
    
  grid <- grid[!st_is_empty(grid), ]
  
  grid_buffered <- st_buffer(grid, dist = grid_size/50)
  
  nb <- poly2nb(st_geometry(grid_buffered), 
                queen = TRUE, 
                snap = grid_size/5)
  
  gc <- spdep::n.comp.nb(nb)
  if (gc$nc > 1) {
    cat("Encontrados", gc$nc, "componentes desconectados. Usando o maior componente.\n")
    biggest <- which.max(table(gc$comp.id))
    keep <- gc$comp.id == biggest
    grid <- grid[keep,]
    nb <- poly2nb(st_geometry(grid), 
                  queen = TRUE, 
                  snap = grid_size/5)
  }
  
  W <- nb2mat(nb, style = "B", zero.policy = TRUE)
  
  point_grid_index <- st_intersects(pontos_sf, grid, sparse = FALSE)
  
  if(all(!point_grid_index)) {
    stop("Nenhum ponto no grid, cheque seus dados.")
  }
  
  data$grid_id <- max.col(point_grid_index)
  
  grid_data <- data %>%
    group_by(grid_id) %>%
    summarise(
      ndvi_mean = mean(ndvi, na.rm = TRUE),
      ndvi_sd = sd(ndvi, na.rm = TRUE),
      n = n()
    ) %>%
    filter(!is.na(ndvi_mean))
  
  n_cells <- nrow(grid)
  
  full_grid_data <- data.frame(
    grid_id = 1:n_cells,
    ndvi_mean = NA,
    ndvi_sd = NA,
    n = 0
  )
  
  full_grid_data[grid_data$grid_id, c("ndvi_mean", "ndvi_sd", "n")] <- 
    grid_data[, c("ndvi_mean", "ndvi_sd", "n")]
  
  full_grid_data$grid_id.1 <- full_grid_data$grid_id

  formula <- ndvi_mean ~ 1 + f(grid_id, model = "besag", graph = W) + 
    f(grid_id.1, model = "generic1", Cmatrix = INLA::inla.as.sparse(diag(n_cells) - rho * W))
  
  model <- try(inla(
    formula,
    family = "gaussian",
    data = full_grid_data,
    control.predictor = list(compute = TRUE),
    control.compute = list(dic = TRUE, waic = TRUE),
    control.inla = list(strategy = "gaussian")
  ))
  
  if(inherits(model, "try-error")) {
    stop("O modelo INLA falhou. Cheque seus dados e estrutura espacial.")
  }
  
  return(list(
    model = model,
    grid = grid,
    data = full_grid_data,
    W = W
  ))
}

plot_sar_resultados <- function(sar_resultados, shapefile) {
  grid <- sar_resultados$grid
  grid$fitted <- sar_resultados$model$summary.fitted.values$mean
  
  pal <- colorNumeric(
    palette = colorRampPalette(c("#a50026", "#fdae61", "#d9ef8b", "#006837"))(100),
    domain = grid$fitted,
    na.color = "transparent"
  )
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = grid,
                fillColor = ~pal(fitted),
                fillOpacity = 0.8,
                weight = 0.5,
                color = "#444444") %>%
    addPolygons(data = shapefile,
                weight = 2,
                opacity = 1,
                color = "#444444",
                fillOpacity = 0,
                dashArray = "3") %>%
    addLegend(pal = pal,
              values = grid$fitted,
              title = "NDVI Values (SAR)",  # Changed title to reflect SAR model
              position = "bottomright",
              labFormat = labelFormat(digits = 2)) %>%
    addScaleBar(position = "bottomleft") %>%
    addMiniMap(position = "bottomleft", 
               toggleDisplay = TRUE)
  
  return(m)
}


## ----------------------------------------------------------------------------------------------------
sar_model <- criar_ndvi_sar(dados_completos, poa_shape)


## ----fig.align='center', echo=FALSE------------------------------------------------------------------
dic <- sar_model$model$dic$dic
waic <- sar_model$model$waic$waic
log_likelihood <- sar_model$model$mlik[1]

tabela <- data.frame(
  Métrica = c("DIC", "WAIC", "Log-verossimilhança"),
  Valor = c(dic, waic, log_likelihood)
)

tabela_kable <- tabela %>%
  kable(format = "html", align = "c", col.names = c("Métrica", "Valor")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center") %>%
  column_spec(1, bold = TRUE, color = "white", background = "#4CAF50") %>%
  column_spec(2, color = "#4CAF50", background = "#F0F0F0", width = "5cm") %>%
  add_header_above(c("Resumo do Modelo" = 2))

tabela_kable


## ----------------------------------------------------------------------------------------------------
plot_sar_resultados(sar_model, poa_shape)

