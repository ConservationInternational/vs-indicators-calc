#insert script for layers table

tmp <- read.csv('./crops.csv')

str <- "INSERT INTO layers(name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download)\nVALUES"

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
legend <- paste0("'", '{"type":"choropleth","bucket":[  {"color": "#FFFFB2", "min-value":"0%", "max-value":"1%"},  {"color": "#FED976", "max-value":"4%"},  {"color": "#FEB24C", "max-value":"12%"},  {"color": "#FD8D3C", "max-value":"25%"},  {"color": "#FC4E2A", "max-value":"40%"},  {"color": "#E31A1C", "max-value":"60%"},  {"color": "#B10026", "max-value":"100%"}]}', "'")

for (i in 1:nrow(tmp)){
  name <- paste0("'", tmp$lab[i], "'")
  slug <- paste0("'VS_Crops_", gsub(' ', '_', tmp$lab[i]), "'")
  
  ##Need to fix the css and the legend
  info <- paste0("'", '{"description":"Percentage of Households Growing ', tmp$lab[i], '","source":"<p>Vital Signs</p>","link":"http://www.vitalsigns.org"}', "'")
  css <- gsub('coloradjus', tmp$col[i], "'#crops {polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}#crops [ coloradjus <= 1] {polygon-fill: #B10026;  }#crops [ coloradjus <= 0.6] {polygon-fill: #E31A1C;  }#crops [ coloradjus <= 0.4] {polygon-fill: #FC4E2A;  }#crops [ coloradjus <= 0.25] {polygon-fill: #FD8D3C;  }#crops [ coloradjus <= 0.12] {polygon-fill: #FEB24C;  }#crops [ coloradjus <= 0.04] {polygon-fill: #FED976;  }#crops [ coloradjus <= 0.01] {polygon-fill: #FFFFB2;}'")
  query <- paste0("'SELECT l.the_geom_webmercator, c.", tmp$col[i], " FROM crops c JOIN vs_landscape_march_2017 l ON c.country = l.country AND c.landscape=l.landscape_no'")
  
  substr <- paste(name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download, sep = ', ')
  substr <- paste0('(', substr, ')')
  
  str <- paste0(str, substr, ',\n')

}

cat(str, file='insertscript.sql')


#Insert script for 'agrupations' table

str <- "INSERT INTO agrupations (ayer_id, layer_group_id)\nVALUES"
for (i in 818:853){
  str <- paste0(str, '(', as.character(i), ",666),\n")
  id <- id + 1
}

cat(str)