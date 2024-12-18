# Bibliotecas necessárias
library(terra)        # Para manipulação de dados espaciais
library(gstat)        # Para geoestatística
library(sp)           # Para dados espaciais
library(ggplot2)      # Para visualização
library(geobr)        # Para shapefiles do Brasil
library(MODISTools)

# Função para criar grade de pontos
create_points_grid <- function(center_lat, center_lon, km_around, spacing_km = 2.5) {
  # Converter km para graus (aproximadamente)
  deg_spacing <- spacing_km / 111  # 1 grau ≈ 111 km
  deg_around <- km_around / 111
  
  # Criar sequências de lat/lon
  lats <- seq(center_lat - deg_around, center_lat + deg_around, by = deg_spacing)
  lons <- seq(center_lon - deg_around, center_lon + deg_around, by = deg_spacing)
  
  # Criar grade de pontos
  points <- expand.grid(latitude = lats, longitude = lons)
  return(points)
}

# Função para baixar dados NDVI para múltiplos pontos
download_ndvi_spatial <- function(center_lat, center_lon, date, km_around = 25, save_csv = TRUE) {
  # Criar grade de pontos
  points_grid <- create_points_grid(center_lat, center_lon, km_around)
  
  # Inicializar dataframe para resultados
  results <- data.frame()
  
  # Baixar dados para cada ponto
  for (i in 1:nrow(points_grid)) {
    cat(sprintf("Baixando ponto %d de %d\n", i, nrow(points_grid)))
    
    tryCatch({
      data <- mt_subset(
        product = "MOD13Q1",
        lat = points_grid$latitude[i],
        lon = points_grid$longitude[i],
        band = "250m_16_days_NDVI",
        start = date,
        end = date,
        km_lr = 0.5,
        km_ab = 0.5,
        internal = TRUE
      )
      
      if (nrow(data) > 0) {
        # Pegar o primeiro valor válido para este ponto
        point_data <- data.frame(
          longitude = points_grid$longitude[i],
          latitude = points_grid$latitude[i],
          ndvi = data$value[1] * 0.0001  # Converter para valores reais de NDVI
        )
        results <- rbind(results, point_data)
      }
    }, error = function(e) {
      cat(sprintf("Erro no ponto %d: %s\n", i, e$message))
    })
  }
  
  # Remover valores NA
  results <- results[!is.na(results$ndvi), ]
  if(save_csv) {
    filename <- sprintf("ndvi_data_%s.csv", gsub("-", "", date))
    write.csv(results, filename, row.names = FALSE)
    cat(sprintf("\nDados salvos em: %s\n", filename))
  }
  
  return(results)
}

# Função para criar modelo geoestatístico
create_ndvi_model <- function(data) {
  # Converte para objeto espacial
  sp_data <- data
  coordinates(sp_data) <- ~longitude+latitude
  proj4string(sp_data) <- CRS("+proj=longlat +datum=WGS84")
  
  # Calcula o variograma
  vgm <- variogram(ndvi ~ 1, sp_data)
  
  # Ajuste simples do modelo
  fit <- fit.variogram(vgm, model = vgm("Sph"))
  
  # Cria a grade de predição
  grid <- expand.grid(
    longitude = seq(min(data$longitude), max(data$longitude), length = 50),
    latitude = seq(min(data$latitude), max(data$latitude), length = 50)
  )
  coordinates(grid) <- ~longitude+latitude
  gridded(grid) <- TRUE
  proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
  
  # Realiza a krigagem
  kriged <- krige(ndvi ~ 1, sp_data, grid, model = fit)
  
  return(list(variogram = vgm, fitted_model = fit, kriged_data = kriged))
}


# Função para plotar resultados
plot_ndvi_results <- function(model_results, shapefile) {
  # Carrega bibliotecas necessárias
  require(raster)
  require(leaflet)
  require(sf)
  
  # Converte dados krigados para dataframe
  kriged_df <- as.data.frame(model_results$kriged_data)
  
  # Cria raster
  r <- raster(nrows = 50, ncols = 50)
  extent(r) <- extent(min(kriged_df$longitude), max(kriged_df$longitude),
                      min(kriged_df$latitude), max(kriged_df$latitude))
  r <- rasterize(kriged_df[, c("longitude", "latitude")], r, kriged_df$var1.pred)
  crs(r) <- CRS("+proj=longlat +datum=WGS84")
  
  # Converte shapefile para SF e garante o mesmo CRS
  shapefile_sf <- st_transform(shapefile, 4326)
  
  # Cria paleta de cores
  pal <- colorNumeric(
    palette = colorRampPalette(c("#ff0000", "#ffff00", "#00ff00"))(100),
    domain = values(r),
    na.color = "transparent"
  )
  
  # Cria mapa Leaflet
  m <- leaflet() %>%
    addTiles() %>%
    addRasterImage(r, colors = pal, opacity = 0.7) %>%
    addPolygons(data = shapefile_sf,
                weight = 2,
                opacity = 1,
                color = "black",
                fillOpacity = 0) %>%
    addLegend(pal = pal,
              values = values(r),
              title = "NDVI",
              position = "bottomright")
  
  # Plotar variograma em uma nova janela
  plot(model_results$variogram, model_results$fitted_model, main = "Variograma NDVI")
  
  # Retorna o mapa Leaflet
  return(m)
}
  
# Criar mapa Leaflet
leaflet() %>%
    addTiles() %>%
    addRasterImage(r, colors = pal, opacity = 0.7) %>%
    addPolygons(data = shapefile, 
                weight = 2,
                opacity = 1,
                color = "black",
                fillOpacity = 0) %>%
    addLegend(pal = pal,
              values = values(r),
              title = "NDVI",
              position = "bottomright")


# Definir dados de Porto Alegre
complete_data <- read.csv("D:/Estatística Espacial/Seminário 2/complete_ndvi_data.csv")

# data <- download_ndvi_spatial(
#   center_lat = -30.03,
#   center_lon = -51.23,
#   date = "2020-01-01",
#   km_around = 25,  # Aumentei para 25km
#   save_csv = TRUE  # Isso vai salvar os dados
# )

# southern_data <- download_ndvi_spatial(
#   center_lat = -30.27,  # Latitude próxima ao limite sul
#   center_lon = -51.15,  # Longitude central no sul
#   date = "2020-01-01",
#   km_around = 5,        # Raio pequeno para cobrir a área que falta
#   save_csv = FALSE      # Para evitar sobrescrever outros dados
# )

# Criar modelo
model <- create_ndvi_model(complete_data)
poa_shape <- read_municipality(code_muni = 4314902, year = 2020)
plot_ndvi_results(model, poa_shape)

