INSERT INTO layers(name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download)
VALUES('Beans', 'beans', 'layer', 'f', '{"description":"Percent Of Households Growing Beans",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '
              #crops_landscape [zoom >= 9]{polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}
              #crops_landscape [beans >= 0][zoom <= 9] {polygon-fill: #B10026;  }
              #crops_landscape [beans >= 0][zoom <= 9] {polygon-fill: #E31A1C;  }
              #crops_landscape [beans >= 0.0333333333333333][zoom <= 9] {polygon-fill: #FC4E2A;  }
              #crops_landscape [beans >= 0.149659863945578][zoom <= 9] {polygon-fill: #FD8D3C;  }
              #crops_landscape [beans >= 0.414285714285714][zoom <= 9] {polygon-fill: #FEB24C;  }
              #crops_landscape [beans >= 0.571428571428572][zoom <= 9] {polygon-fill: #FED976;  }
              #crops_landscape [beans >= 0.84679802955665][zoom <= 9] {polygon-fill: #FFFFB2;}
              
              #crops_landscape [zoom < 9]{marker-fill: #FFFFB2;  marker-opacity: 0.8; marker-line-color: #FFF; marker-line-width: 0.5; marker-line-opacity: 1;}
              #crops_landscape [beans < 0][zoom > 9] {marker-fill: #B10026;  }
              #crops_landscape [beans < 0][zoom > 9] {marker-fill: #E31A1C;  }
              #crops_landscape [beans < 0.0333333333333333][zoom > 9] {marker-fill: #FC4E2A;  }
              #crops_landscape [beans < 0.149659863945578][zoom > 9] {marker-fill: #FD8D3C;  }
              #crops_landscape [beans < 0.414285714285714][zoom > 9] {marker-fill: #FEB24C;  }
              #crops_landscape [beans < 0.571428571428572][zoom > 9] {marker-fill: #FED976;  }
              #crops_landscape [beans < 0.84679802955665][zoom > 9] {marker-fill: #FFFFB2;}
              ', 1, 'SELECT l.the_geom_webmercator, c.beans 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no', now(), now(), 't', 't', '{"type":"choropleth","bucket":[  
                   {"color": "#FFFFB2", "min-value":"0%", "max-value":"0%"},  
                   {"color": "#FED976", "max-value":"0%"},  
                   {"color": "#FEB24C", "max-value":"3.33%"},  
                   {"color": "#FD8D3C", "max-value":"15%"},  
                   {"color": "#FC4E2A", "max-value":"41.4%"},  
                   {"color": "#E31A1C", "max-value":"57.1%"},  
                   {"color": "#B10026", "max-value":"84.7%"}]}', '100', 0, 'f'),
('Yields', 'mean_yield_quantile', 'layer', 'f', '{"description":"Mean Yield Quartle Across All Crops",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '
              #agintensification_landscape [zoom >= 9]{polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}
              #agintensification_landscape [mean_yield_quantile >= 1.85833333333333][zoom <= 9] {polygon-fill: #B10026;  }
              #agintensification_landscape [mean_yield_quantile >= 2.02837662337662][zoom <= 9] {polygon-fill: #E31A1C;  }
              #agintensification_landscape [mean_yield_quantile >= 2.2193820861678][zoom <= 9] {polygon-fill: #FC4E2A;  }
              #agintensification_landscape [mean_yield_quantile >= 2.32021468007493][zoom <= 9] {polygon-fill: #FD8D3C;  }
              #agintensification_landscape [mean_yield_quantile >= 2.54204958380868][zoom <= 9] {polygon-fill: #FEB24C;  }
              #agintensification_landscape [mean_yield_quantile >= 2.61273015873016][zoom <= 9] {polygon-fill: #FED976;  }
              #agintensification_landscape [mean_yield_quantile >= 2.91675898931001][zoom <= 9] {polygon-fill: #FFFFB2;}
              
              #agintensification_landscape [zoom < 9]{marker-fill: #FFFFB2;  marker-opacity: 0.8; marker-line-color: #FFF; marker-line-width: 0.5; marker-line-opacity: 1;}
              #agintensification_landscape [mean_yield_quantile < 1.85833333333333][zoom > 9] {marker-fill: #B10026;  }
              #agintensification_landscape [mean_yield_quantile < 2.02837662337662][zoom > 9] {marker-fill: #E31A1C;  }
              #agintensification_landscape [mean_yield_quantile < 2.2193820861678][zoom > 9] {marker-fill: #FC4E2A;  }
              #agintensification_landscape [mean_yield_quantile < 2.32021468007493][zoom > 9] {marker-fill: #FD8D3C;  }
              #agintensification_landscape [mean_yield_quantile < 2.54204958380868][zoom > 9] {marker-fill: #FEB24C;  }
              #agintensification_landscape [mean_yield_quantile < 2.61273015873016][zoom > 9] {marker-fill: #FED976;  }
              #agintensification_landscape [mean_yield_quantile < 2.91675898931001][zoom > 9] {marker-fill: #FFFFB2;}
              ', 1, 'SELECT l.the_geom_webmercator, c.mean_yield_quantile 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no', now(), now(), 't', 't', '{"type":"choropleth","bucket":[  
                   {"color": "#FFFFB2", "min-value":"1.86", "max-value":"1.86"},  
                   {"color": "#FED976", "max-value":"2.03"},  
                   {"color": "#FEB24C", "max-value":"2.22"},  
                   {"color": "#FD8D3C", "max-value":"2.32"},  
                   {"color": "#FC4E2A", "max-value":"2.54"},  
                   {"color": "#E31A1C", "max-value":"2.61"},  
                   {"color": "#B10026", "max-value":"2.92"}]}', '100', 0, 'f'),
('Food As A Percentage Of Total Spending', 'food_as_percent_total_spending', 'layer', 'f', '{"description":"Percentage Of All Expenditures On Food, Household Mean",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '
              #foodsecurity_landscape [zoom >= 9]{polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}
              #foodsecurity_landscape [food_as_percent_total_spending >= 29.7211623669306][zoom <= 9] {polygon-fill: #B10026;  }
              #foodsecurity_landscape [food_as_percent_total_spending >= 41.50310270331][zoom <= 9] {polygon-fill: #E31A1C;  }
              #foodsecurity_landscape [food_as_percent_total_spending >= 44.2474328241066][zoom <= 9] {polygon-fill: #FC4E2A;  }
              #foodsecurity_landscape [food_as_percent_total_spending >= 46.9422090784521][zoom <= 9] {polygon-fill: #FD8D3C;  }
              #foodsecurity_landscape [food_as_percent_total_spending >= 50.5552278265954][zoom <= 9] {polygon-fill: #FEB24C;  }
              #foodsecurity_landscape [food_as_percent_total_spending >= 52.9564052597161][zoom <= 9] {polygon-fill: #FED976;  }
              #foodsecurity_landscape [food_as_percent_total_spending >= 56.9406601385614][zoom <= 9] {polygon-fill: #FFFFB2;}
              
              #foodsecurity_landscape [zoom < 9]{marker-fill: #FFFFB2;  marker-opacity: 0.8; marker-line-color: #FFF; marker-line-width: 0.5; marker-line-opacity: 1;}
              #foodsecurity_landscape [food_as_percent_total_spending < 29.7211623669306][zoom > 9] {marker-fill: #B10026;  }
              #foodsecurity_landscape [food_as_percent_total_spending < 41.50310270331][zoom > 9] {marker-fill: #E31A1C;  }
              #foodsecurity_landscape [food_as_percent_total_spending < 44.2474328241066][zoom > 9] {marker-fill: #FC4E2A;  }
              #foodsecurity_landscape [food_as_percent_total_spending < 46.9422090784521][zoom > 9] {marker-fill: #FD8D3C;  }
              #foodsecurity_landscape [food_as_percent_total_spending < 50.5552278265954][zoom > 9] {marker-fill: #FEB24C;  }
              #foodsecurity_landscape [food_as_percent_total_spending < 52.9564052597161][zoom > 9] {marker-fill: #FED976;  }
              #foodsecurity_landscape [food_as_percent_total_spending < 56.9406601385614][zoom > 9] {marker-fill: #FFFFB2;}
              ', 1, 'SELECT l.the_geom_webmercator, c.food_as_percent_total_spending 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no', now(), now(), 't', 't', '{"type":"choropleth","bucket":[  
                   {"color": "#FFFFB2", "min-value":"29.7%", "max-value":"29.7%"},  
                   {"color": "#FED976", "max-value":"41.5%"},  
                   {"color": "#FEB24C", "max-value":"44.2%"},  
                   {"color": "#FD8D3C", "max-value":"46.9%"},  
                   {"color": "#FC4E2A", "max-value":"50.6%"},  
                   {"color": "#E31A1C", "max-value":"53%"},  
                   {"color": "#B10026", "max-value":"56.9%"}]}', '100', 0, 'f'),
('Field Sizes', 'median_field_size', 'layer', 'f', '{"description":"Median Field Size",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '
              #agintensification_landscape [zoom >= 9]{polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}
              #agintensification_landscape [median_field_size >= 0.125][zoom <= 9] {polygon-fill: #B10026;  }
              #agintensification_landscape [median_field_size >= 0.2081][zoom <= 9] {polygon-fill: #E31A1C;  }
              #agintensification_landscape [median_field_size >= 0.36165][zoom <= 9] {polygon-fill: #FC4E2A;  }
              #agintensification_landscape [median_field_size >= 0.601785714285714][zoom <= 9] {polygon-fill: #FD8D3C;  }
              #agintensification_landscape [median_field_size >= 1.30714285714286][zoom <= 9] {polygon-fill: #FEB24C;  }
              #agintensification_landscape [median_field_size >= 1.83571428571428][zoom <= 9] {polygon-fill: #FED976;  }
              #agintensification_landscape [median_field_size >= 2.26588571428571][zoom <= 9] {polygon-fill: #FFFFB2;}
              
              #agintensification_landscape [zoom < 9]{marker-fill: #FFFFB2;  marker-opacity: 0.8; marker-line-color: #FFF; marker-line-width: 0.5; marker-line-opacity: 1;}
              #agintensification_landscape [median_field_size < 0.125][zoom > 9] {marker-fill: #B10026;  }
              #agintensification_landscape [median_field_size < 0.2081][zoom > 9] {marker-fill: #E31A1C;  }
              #agintensification_landscape [median_field_size < 0.36165][zoom > 9] {marker-fill: #FC4E2A;  }
              #agintensification_landscape [median_field_size < 0.601785714285714][zoom > 9] {marker-fill: #FD8D3C;  }
              #agintensification_landscape [median_field_size < 1.30714285714286][zoom > 9] {marker-fill: #FEB24C;  }
              #agintensification_landscape [median_field_size < 1.83571428571428][zoom > 9] {marker-fill: #FED976;  }
              #agintensification_landscape [median_field_size < 2.26588571428571][zoom > 9] {marker-fill: #FFFFB2;}
              ', 1, 'SELECT l.the_geom_webmercator, c.median_field_size 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no', now(), now(), 't', 't', '{"type":"choropleth","bucket":[  
                   {"color": "#FFFFB2", "min-value":"0.125", "max-value":"0.125"},  
                   {"color": "#FED976", "max-value":"0.208"},  
                   {"color": "#FEB24C", "max-value":"0.362"},  
                   {"color": "#FD8D3C", "max-value":"0.602"},  
                   {"color": "#FC4E2A", "max-value":"1.31"},  
                   {"color": "#E31A1C", "max-value":"1.84"},  
                   {"color": "#B10026", "max-value":"2.27"}]}', '100', 0, 'f'),
('Satisfied With Water Quality', 'satisfied_drinking_water', 'layer', 'f', '{"description":"Percentage Of Households Satisfied With Their Drinking Water",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '
              #watsan_landscape [zoom >= 9]{polygon-fill: #FFFFB2;  polygon-opacity: 0.8; line-color: #FFF; line-width: 0.5;line-opacity: 1;}
              #watsan_landscape [satisfied_drinking_water >= 0.566666666666667][zoom <= 9] {polygon-fill: #B10026;  }
              #watsan_landscape [satisfied_drinking_water >= 0.714285714285714][zoom <= 9] {polygon-fill: #E31A1C;  }
              #watsan_landscape [satisfied_drinking_water >= 0.825714285714286][zoom <= 9] {polygon-fill: #FC4E2A;  }
              #watsan_landscape [satisfied_drinking_water >= 0.866666666666667][zoom <= 9] {polygon-fill: #FD8D3C;  }
              #watsan_landscape [satisfied_drinking_water >= 0.923809523809524][zoom <= 9] {polygon-fill: #FEB24C;  }
              #watsan_landscape [satisfied_drinking_water >= 0.938095238095238][zoom <= 9] {polygon-fill: #FED976;  }
              #watsan_landscape [satisfied_drinking_water >= 0.966666666666667][zoom <= 9] {polygon-fill: #FFFFB2;}
              
              #watsan_landscape [zoom < 9]{marker-fill: #FFFFB2;  marker-opacity: 0.8; marker-line-color: #FFF; marker-line-width: 0.5; marker-line-opacity: 1;}
              #watsan_landscape [satisfied_drinking_water < 0.566666666666667][zoom > 9] {marker-fill: #B10026;  }
              #watsan_landscape [satisfied_drinking_water < 0.714285714285714][zoom > 9] {marker-fill: #E31A1C;  }
              #watsan_landscape [satisfied_drinking_water < 0.825714285714286][zoom > 9] {marker-fill: #FC4E2A;  }
              #watsan_landscape [satisfied_drinking_water < 0.866666666666667][zoom > 9] {marker-fill: #FD8D3C;  }
              #watsan_landscape [satisfied_drinking_water < 0.923809523809524][zoom > 9] {marker-fill: #FEB24C;  }
              #watsan_landscape [satisfied_drinking_water < 0.938095238095238][zoom > 9] {marker-fill: #FED976;  }
              #watsan_landscape [satisfied_drinking_water < 0.966666666666667][zoom > 9] {marker-fill: #FFFFB2;}
              ', 1, 'SELECT l.the_geom_webmercator, c.satisfied_drinking_water 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no', now(), now(), 't', 't', '{"type":"choropleth","bucket":[  
                   {"color": "#FFFFB2", "min-value":"56.7%", "max-value":"56.7%"},  
                   {"color": "#FED976", "max-value":"71.4%"},  
                   {"color": "#FEB24C", "max-value":"82.6%"},  
                   {"color": "#FD8D3C", "max-value":"86.7%"},  
                   {"color": "#FC4E2A", "max-value":"92.4%"},  
                   {"color": "#E31A1C", "max-value":"93.8%"},  
                   {"color": "#B10026", "max-value":"96.7%"}]}', '100', 0, 'f'),
