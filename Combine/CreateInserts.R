#insert script for layers table

library(RColorBrewer)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/Combine/')

tmp <- read.csv('codes.csv', stringsAsFactors = F)

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


for (i in 1:nrow(tmp)){
  
  vals <- read.csv(paste0('S3 Files/', tmp$Table[i], '.csv'))[ , tmp$Col[i] ]
  
  val_q <-  vals %>%
    quantile(probs=seq(0,1,(1/6)), na.rm=T) %>% unique
  
  #Create color groups for multiple possible values
  if (length(unique(val_q)) > 5 | length(unique(vals)) > 7){
    
    val <-  vals %>%
      quantile(probs=seq(0,1,(1/6)), na.rm=T) %>% unique
  
    val_sig <- signif(val, 2)
    
    if(tmp$Unit[i]=='Percentage'){
      if(max(val) <= 1){
        val_sig <- val_sig*100
      }
      val_sig <- paste0(val_sig, '%')
    }
    
    colorRange <- c(tmp$MinCol[i], tmp$MidCol[i], tmp$MaxCol[i]) %>% na.omit
    colors <- colorRampPalette(colorRange)(length(val)-1)
    
    
    legend <- paste0('{"type":"choropleth","bucket":[ {"color": "', colors[1], '", "min-value":"', val_sig[1], '", "max-value":"', val_sig[2], '"}')
    for (j in 2:length(colors)){
      legend <- paste0(legend, ',\n{"color": "', colors[j], '", "max-value":"', val_sig[j+1], '"}')
    }
    legend <- paste0("'", legend, "]}'")
    
    
    css <- paste0("#", tmp$Table[i], '[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}\n')
    for (j in length(val):2){
      css <- paste0(css, "#", tmp$Table[i], " [", tolower(tmp$Col[i]), ' <= ', val[j], "][zoom >= 9] {polygon-fill: ", colors[j-1], ";}\n")
    }
    css <- paste0(css, '\n\n')
    
    css <- paste0(css, "#", tmp$Table[i], '[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}\n')
    for (j in length(val):2){
      css <- paste0(css, "#", tmp$Table[i], " [", tolower(tmp$Col[i]), ' <= ', val[j], "][zoom < 9] {marker-fill: ", colors[j-1], ";}\n")
    }
    
    css <- paste0("'", css, "'")
    
    
    
  } else{  #Create one to one color-unit mappings for when there are < 5 unique values
    
    val <- unique(vals)
    val <- val[order(val)]
    val <- round(val, 4)
    
    val_sig <- signif(val, 3)
    
    if(tmp$Unit[i]=='Percentage'){
      if(max(val) <= 1){
        val_sig <- val_sig*100
      }
      val_sig <- paste0(val_sig, '%')
    }
    
    
    colorRange <- c(tmp$MinCol[i], tmp$MidCol[i], tmp$MaxCol[i]) %>% na.omit
    colors <- colorRampPalette(colorRange)(length(val))

  
    legend <- paste0('{"type":"custom","data":[{"value": "', colors[1], '", "name":"', val_sig[1], '"}')
    for (j in 2:length(colors)){
      legend <- paste0(legend, ',\n{"value": "', colors[j], '", "name": "', val_sig[j], '"}')
    }
    legend <- paste0("'", legend, "]}'")

    
    css <- paste0("#", tmp$Table[i], '[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}\n')
    for (j in length(val):1){
      css <- paste0(css, "#", tmp$Table[i], " [", tolower(tmp$Col[i]), ' = ', val[j], "][zoom >= 9] {polygon-fill: ", colors[j], ";}\n")
    }
    css <- paste0(css, '\n\n')
    
    css <- paste0(css, "#", tmp$Table[i], '[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}\n')
    for (j in length(val):1){
      css <- paste0(css, "#", tmp$Table[i], " [", tolower(tmp$Col[i]), ' = ', val[j], "][zoom < 9] {marker-fill: ", colors[j], ";}\n")
    }
    
    css <- paste0("'", css, "'")
  
  }
  
  name <- paste0("'", tmp$Name[i], "'")
  slug <- paste0("'", tolower(tmp$Col[i]), "'")
  
  ##Need to fix the css and the legend
  info <- paste0("'", '{"description":"', tmp$Legend[i], '",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', "'")
  
  query <- paste0("$$SELECT l.the_geom_webmercator, round(c.", tolower(tmp$Col[i]), "::numeric, 4) AS \"", tolower(tmp$Col[i]), "\" 
                  FROM ", tolower(tmp$Table[i]), " c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no")
  
  if (sum(is.na(vals)) > 0){
    query <- paste0(query, "WHERE ", tolower(tmp$Col[i]), " <> 'NA'$$")
  } else{
    query <- paste0(query, '$$')
  }
  
  substr <- paste(name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download, sep = ', ')

  substr <- paste0('(', substr, ')')
  
  str <- paste0(str, substr, ',\n')

}

str <- substr(str,1,nchar(str)-2)

cat(str, file='insertscript.sql')









#Insert script for 'agrupations' table

str <- "INSERT INTO agrupations (layer_id, layer_group_id)\nVALUES"


start <- 1008

for (i in 1:nrow(tmp)){
  str <- paste0(str, '(', start, ",", tmp$sub_id[i], "),\n")
  start <- start + 1
}


#update
str <- ''

for (i in 1:nrow(tmp)){
  
  vals <- read.csv(paste0('S3 Files/', tmp$Table[i], '.csv'))[ , tmp$Col[i] ]
  
  query <- paste0("$$SELECT l.the_geom_webmercator, round(c.", tolower(tmp$Col[i]), "::numeric, 4) AS \"", tolower(tmp$Col[i]), "\" 
                  FROM ", tolower(tmp$Table[i]), " c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no")
  
  if (sum(is.na(vals)) > 0){
    query <- paste0(query, "WHERE ", tolower(tmp$Col[i]), " <> 'NA'$$")
  } else{
    query <- paste0(query, '$$')
  }
  
  
  str <- paste0(str, 'UPDATE layers\nSET query = ', query,
                '\nWHERE id = ', tmp$layer_id[i], ';\n\n')

}

cat(str, file='updatescript.sql')

cat('UPDATE layers\nSET updated_at = now()\nWHERE id in (', paste0(tmp$layer_id, collapse=','), ')')
