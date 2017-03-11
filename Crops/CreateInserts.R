tmp <- read.csv('./crops.csv')

str <- "INSERT INTO layers(id, name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download)\nVALUES"

id <- 818
layer_type <- "'layer'"
active <- "'f'"
layer_provider <- "'cartodb'"
opacity <- '1'
created_at <- 'now()'
updated_at <- 'now()'
locate_layer <- "'t'"
published <- "'t'"
zoom_max <- "'100'"
zoom_min <- '0'
download <- "'f'"
legend <- paste0("'", '{"min":"<-1","mid":"0","max":">1","type":"choropleth", "bucket": ["#533104","#BF832D","#DEC17D","#FFFFFF","#C7ECE5","#35968D","#003C30"]}', "'")

for (i in 1:nrow(tmp)){
  name <- paste0("'", tmp$lab[i], "'")
  slug <- paste0("'VS_Crops_", tmp$lab[i], "'")
  
  ##Need to fix the css and the legend
  info <- paste0("'", '{"description":"Percentage of Households Growing ', tmp$lab[i], '","source":"<p>Vital Signs</p>","link":"http://www.vitalsigns.org"}', "'")
  css <- paste0("'#crops{marker-fill-opacity: 0.8;marker-line-color: #FFF;marker-line-width: 1;marker-line-opacity: 1;marker-width: 10;marker-fill: #FFFFB2;marker-allow-overlap: true;}#crops [ ", tmp$col[i], " <= 1] {marker-fill: #B10026;}#crops [ ", tmp$col[i], " <= 0.966666666666667] {marker-fill: #E31A1C;}#crops [ ", tmp$col[i], " <= 0.9] {marker-fill: #FC4E2A;}#crops [ ", tmp$col[i], " <= 0.633333333333333] {marker-fill: #FD8D3C;}#crops [ ", tmp$col[i], " <= 0.533333333333333] {marker-fill: #FEB24C;}#crops [ ", tmp$col[i], " <= 0.322033898305085] {marker-fill: #FED976;}#crops [ ", tmp$col[i], " <= 0.206896551724138] {marker-fill: #FFFFB2;}'")
  query <- paste0("'SELECT l.the_geom_webmercator, c.", tmp$col[i], " FROM crops c JOIN vs_landscape_march_2017 l ON c.country = l.country AND c.landscape=l.landscape_no'")
  
  substr <- paste(as.character(id), name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download, sep = ', ')
  substr <- paste0('(', substr, ')')
  
  str <- paste0(str, substr, ',\n')
  
  id <- id + 1
}

cat(str, file='insertscript.sql')

str <- paste0(str, ';')


str <- "INSERT INTO agrupations (id, layer_id, layer_group_id)\nVALUES"
id <- 1251
for (i in 819:853){
  str <- paste0(str, '(',as.character(id), ",", as.character(i), ",666),\n")
  id <- id + 1
}