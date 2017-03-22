#insert script for layers table

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

test_nums <- sample(1:nrow(tmp), 5)

for (i in sample(1:nrow(tmp), 5)){
  val <- read.csv(paste0('S3 Files/', tmp$Table[i], '.csv'))[ , tmp$Col[i] ] %>%
    quantile(probs=seq(0,1,(1/7)), na.rm=T)
  
  val_sig <- signif(val, 3)
  
  if(tmp$Unit[i]=='Percentage'){
    if(max(val) <= 1){
      val_sig <- val_sig*100
    }
    val_sig <- paste0(val_sig, '%')
  }
  
  name <- paste0("'", tmp$Name[i], "'")
  slug <- paste0("'", tolower(tmp$Col[i]), "'")
  
  ##Need to fix the css and the legend
  info <- paste0("'", '{"description":"', tmp$Legend[i], '",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', "'")
  
  legend <- paste0("'", '{"type":"choropleth","bucket":[  
                   {"color": "#FFFFB2", "min-value":"', val_sig[1], '", "max-value":"', val_sig[1], '"},  
                   {"color": "#FED976", "max-value":"', val_sig[2], '"},  
                   {"color": "#FEB24C", "max-value":"', val_sig[3], '"},  
                   {"color": "#FD8D3C", "max-value":"', val_sig[4], '"},  
                   {"color": "#FC4E2A", "max-value":"', val_sig[5], '"},  
                   {"color": "#E31A1C", "max-value":"', val_sig[6], '"},  
                   {"color": "#B10026", "max-value":"', val_sig[7], '"}]}', "'")
  
  css <- gsub('crops', tolower(tmp$Table[i]),
         gsub('coloradjus', tolower(tmp$Col[i]),
              paste0("'
              #crops [zoom >= 9]{polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}
              #crops [coloradjus >= ", val[1], "][zoom <= 9] {polygon-fill: #B10026;  }
              #crops [coloradjus >= ", val[2], "][zoom <= 9] {polygon-fill: #E31A1C;  }
              #crops [coloradjus >= ", val[3], "][zoom <= 9] {polygon-fill: #FC4E2A;  }
              #crops [coloradjus >= ", val[4], "][zoom <= 9] {polygon-fill: #FD8D3C;  }
              #crops [coloradjus >= ", val[5], "][zoom <= 9] {polygon-fill: #FEB24C;  }
              #crops [coloradjus >= ", val[6], "][zoom <= 9] {polygon-fill: #FED976;  }
              #crops [coloradjus >= ", val[7], "][zoom <= 9] {polygon-fill: #FFFFB2;}
              
              #crops [zoom < 9]{marker-fill: #FFFFB2;  marker-opacity: 0.8; marker-line-color: #FFF; marker-line-width: 0.5; marker-line-opacity: 1;}
              #crops [coloradjus < ", val[1], "][zoom > 9] {marker-fill: #B10026;  }
              #crops [coloradjus < ", val[2], "][zoom > 9] {marker-fill: #E31A1C;  }
              #crops [coloradjus < ", val[3], "][zoom > 9] {marker-fill: #FC4E2A;  }
              #crops [coloradjus < ", val[4], "][zoom > 9] {marker-fill: #FD8D3C;  }
              #crops [coloradjus < ", val[5], "][zoom > 9] {marker-fill: #FEB24C;  }
              #crops [coloradjus < ", val[6], "][zoom > 9] {marker-fill: #FED976;  }
              #crops [coloradjus < ", val[7], "][zoom > 9] {marker-fill: #FFFFB2;}
              '")))
  
  query <- paste0("'SELECT l.the_geom_webmercator, c.", tolower(tmp$Col[i]), " 
                  FROM ", tolower(tmp$Table[i]), " c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no'")
  
  substr <- paste(name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download, sep = ', ')

  substr <- paste0('(', substr, ')')
  
  str <- paste0(str, substr, ',\n')

}

cat(str, file='insertscript.sql')









#Insert script for 'agrupations' table

str <- "INSERT INTO agrupations (layer_id, layer_group_id)\nVALUES"
for (i in 861:865){
  str <- paste0(str, '(', i, ",690),\n")
}

cat(str)