INSERT INTO layers(name, slug, layer_type, active, info, layer_provider, css, opacity, query, created_at, updated_at, locate_layer, published, legend, zoom_max, zoom_min, download)
VALUES('Bananas', 'bananas', 'layer', 'f', '{"description":"Percent Of Households Growing Bananas",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [bananas <= 0.7][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [bananas <= 0.366666666666667][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [bananas <= 0.1][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [bananas <= 0.0333333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [bananas <= 0.7][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [bananas <= 0.366666666666667][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [bananas <= 0.1][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [bananas <= 0.0333333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.bananas::numeric, 4) AS "bananas" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE bananas <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"3.3%"},
{"color": "#D2775D", "max-value":"10%"},
{"color": "#AC2E15", "max-value":"37%"},
{"color": "#850200", "max-value":"70%"}]}', '100', 0, 'f'),
('Beans', 'beans', 'layer', 'f', '{"description":"Percent Of Households Growing Beans",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [beans <= 1][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [beans <= 0.775862068965517][zoom >= 9] {polygon-fill: #A22310;}
#Crops_Landscape [beans <= 0.533333333333333][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [beans <= 0.2][zoom >= 9] {polygon-fill: #DC917C;}
#Crops_Landscape [beans <= 0.0333333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [beans <= 1][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [beans <= 0.775862068965517][zoom < 9] {marker-fill: #A22310;}
#Crops_Landscape [beans <= 0.533333333333333][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [beans <= 0.2][zoom < 9] {marker-fill: #DC917C;}
#Crops_Landscape [beans <= 0.0333333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.beans::numeric, 4) AS "beans" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE beans <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"3.3%"},
{"color": "#DC917C", "max-value":"20%"},
{"color": "#C04420", "max-value":"53%"},
{"color": "#A22310", "max-value":"78%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Bulrush Millet', 'bulrush_millet', 'layer', 'f', '{"description":"Percent Of Households Growing Bulrush Millet",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [bulrush_millet = 0.2333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [bulrush_millet = 0.1][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [bulrush_millet = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [bulrush_millet = 0.2333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [bulrush_millet = 0.1][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [bulrush_millet = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.bulrush_millet::numeric, 4) AS "bulrush_millet" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE bulrush_millet <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#C04420", "name": "10%"},
{"value": "#850200", "name": "23.3%"}]}', '100', 0, 'f'),
('Cashew Nut', 'cashew_nut', 'layer', 'f', '{"description":"Percent Of Households Growing Cashew Nut",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [cashew_nut = 0.0345][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [cashew_nut = 0.0333][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [cashew_nut = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [cashew_nut = 0.0345][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [cashew_nut = 0.0333][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [cashew_nut = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.cashew_nut::numeric, 4) AS "cashew_nut" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE cashew_nut <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#C04420", "name": "3.33%"},
{"value": "#850200", "name": "3.45%"}]}', '100', 0, 'f'),
('Cassava', 'cassava', 'layer', 'f', '{"description":"Percent Of Households Growing Cassava",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [cassava <= 0.888888888888889][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [cassava <= 0.533333333333333][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [cassava <= 0.362068965517241][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [cassava <= 0.0666666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [cassava <= 0.888888888888889][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [cassava <= 0.533333333333333][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [cassava <= 0.362068965517241][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [cassava <= 0.0666666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.cassava::numeric, 4) AS "cassava" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE cassava <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"6.7%"},
{"color": "#D2775D", "max-value":"36%"},
{"color": "#AC2E15", "max-value":"53%"},
{"color": "#850200", "max-value":"89%"}]}', '100', 0, 'f'),
('Chilies', 'chilies', 'layer', 'f', '{"description":"Percent Of Households Growing Chilies",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [chilies = 0.2143][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [chilies = 0.069][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [chilies = 0.0333][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [chilies = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [chilies = 0.2143][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [chilies = 0.069][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [chilies = 0.0333][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [chilies = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.chilies::numeric, 4) AS "chilies" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE chilies <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "3.33%"},
{"value": "#AC2E15", "name": "6.9%"},
{"value": "#850200", "name": "21.4%"}]}', '100', 0, 'f'),
('Cocoa', 'cocoa', 'layer', 'f', '{"description":"Percent Of Households Growing Cocoa",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [cocoa = 0.5][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [cocoa = 0.4][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [cocoa = 0.0833][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [cocoa = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [cocoa = 0.5][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [cocoa = 0.4][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [cocoa = 0.0833][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [cocoa = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.cocoa::numeric, 4) AS "cocoa" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE cocoa <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "8.33%"},
{"value": "#AC2E15", "name": "40%"},
{"value": "#850200", "name": "50%"}]}', '100', 0, 'f'),
('Coconut', 'coconut', 'layer', 'f', '{"description":"Percent Of Households Growing Coconut",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [coconut = 0.7][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [coconut = 0.1667][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [coconut = 0.0667][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [coconut = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [coconut = 0.7][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [coconut = 0.1667][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [coconut = 0.0667][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [coconut = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.coconut::numeric, 4) AS "coconut" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE coconut <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "6.67%"},
{"value": "#AC2E15", "name": "16.7%"},
{"value": "#850200", "name": "70%"}]}', '100', 0, 'f'),
('Coffee', 'coffee', 'layer', 'f', '{"description":"Percent Of Households Growing Coffee",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [coffee = 0.4][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [coffee = 0.3333][zoom >= 9] {polygon-fill: #9C1C0C;}
#Crops_Landscape [coffee = 0.2931][zoom >= 9] {polygon-fill: #B43619;}
#Crops_Landscape [coffee = 0.0345][zoom >= 9] {polygon-fill: #CB6244;}
#Crops_Landscape [coffee = 0.0333][zoom >= 9] {polygon-fill: #E1A18E;}
#Crops_Landscape [coffee = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [coffee = 0.4][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [coffee = 0.3333][zoom < 9] {marker-fill: #9C1C0C;}
#Crops_Landscape [coffee = 0.2931][zoom < 9] {marker-fill: #B43619;}
#Crops_Landscape [coffee = 0.0345][zoom < 9] {marker-fill: #CB6244;}
#Crops_Landscape [coffee = 0.0333][zoom < 9] {marker-fill: #E1A18E;}
#Crops_Landscape [coffee = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.coffee::numeric, 4) AS "coffee" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE coffee <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#E1A18E", "name": "3.33%"},
{"value": "#CB6244", "name": "3.45%"},
{"value": "#B43619", "name": "29.3%"},
{"value": "#9C1C0C", "name": "33.3%"},
{"value": "#850200", "name": "40%"}]}', '100', 0, 'f'),
('Cotton', 'cotton', 'layer', 'f', '{"description":"Percent Of Households Growing Cotton",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [cotton = 0.0345][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [cotton = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [cotton = 0.0345][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [cotton = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.cotton::numeric, 4) AS "cotton" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE cotton <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#850200", "name": "3.45%"}]}', '100', 0, 'f'),
('Egg Plant', 'egg_plant', 'layer', 'f', '{"description":"Percent Of Households Growing Egg Plant",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [egg_plant = 0.3571][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [egg_plant = 0.0172][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [egg_plant = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [egg_plant = 0.3571][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [egg_plant = 0.0172][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [egg_plant = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.egg_plant::numeric, 4) AS "egg_plant" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE egg_plant <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#C04420", "name": "1.72%"},
{"value": "#850200", "name": "35.7%"}]}', '100', 0, 'f'),
('Field Peas', 'field_peas', 'layer', 'f', '{"description":"Percent Of Households Growing Field Peas",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [field_peas = 0.3793][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [field_peas = 0.3][zoom >= 9] {polygon-fill: #A22310;}
#Crops_Landscape [field_peas = 0.0667][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [field_peas = 0.0333][zoom >= 9] {polygon-fill: #DC917C;}
#Crops_Landscape [field_peas = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [field_peas = 0.3793][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [field_peas = 0.3][zoom < 9] {marker-fill: #A22310;}
#Crops_Landscape [field_peas = 0.0667][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [field_peas = 0.0333][zoom < 9] {marker-fill: #DC917C;}
#Crops_Landscape [field_peas = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.field_peas::numeric, 4) AS "field_peas" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE field_peas <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#DC917C", "name": "3.33%"},
{"value": "#C04420", "name": "6.67%"},
{"value": "#A22310", "name": "30%"},
{"value": "#850200", "name": "37.9%"}]}', '100', 0, 'f'),
('Finger Millet', 'finger_millet', 'layer', 'f', '{"description":"Percent Of Households Growing Finger Millet",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [finger_millet = 0.2333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [finger_millet = 0.1333][zoom >= 9] {polygon-fill: #A22310;}
#Crops_Landscape [finger_millet = 0.0667][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [finger_millet = 0.0333][zoom >= 9] {polygon-fill: #DC917C;}
#Crops_Landscape [finger_millet = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [finger_millet = 0.2333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [finger_millet = 0.1333][zoom < 9] {marker-fill: #A22310;}
#Crops_Landscape [finger_millet = 0.0667][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [finger_millet = 0.0333][zoom < 9] {marker-fill: #DC917C;}
#Crops_Landscape [finger_millet = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.finger_millet::numeric, 4) AS "finger_millet" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE finger_millet <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#DC917C", "name": "3.33%"},
{"value": "#C04420", "name": "6.67%"},
{"value": "#A22310", "name": "13.3%"},
{"value": "#850200", "name": "23.3%"}]}', '100', 0, 'f'),
('Groundnut', 'groundnut', 'layer', 'f', '{"description":"Percent Of Households Growing Groundnut",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [groundnut <= 0.620689655172414][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [groundnut <= 0.3][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [groundnut <= 0.1][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [groundnut <= 0.620689655172414][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [groundnut <= 0.3][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [groundnut <= 0.1][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.groundnut::numeric, 4) AS "groundnut" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE groundnut <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"10%"},
{"color": "#C04420", "max-value":"30%"},
{"color": "#850200", "max-value":"62%"}]}', '100', 0, 'f'),
('Irish Potatoes', 'irish_potatoes', 'layer', 'f', '{"description":"Percent Of Households Growing Irish Potatoes",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [irish_potatoes <= 0.8][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [irish_potatoes <= 0.233333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [irish_potatoes <= 0.8][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [irish_potatoes <= 0.233333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.irish_potatoes::numeric, 4) AS "irish_potatoes" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE irish_potatoes <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"23%"},
{"color": "#850200", "max-value":"80%"}]}', '100', 0, 'f'),
('Maize', 'maize', 'layer', 'f', '{"description":"Percent Of Households Growing Maize",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [maize <= 1][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [maize <= 0.966666666666667][zoom >= 9] {polygon-fill: #9C1C0C;}
#Crops_Landscape [maize <= 0.866666666666667][zoom >= 9] {polygon-fill: #B43619;}
#Crops_Landscape [maize <= 0.6][zoom >= 9] {polygon-fill: #CB6244;}
#Crops_Landscape [maize <= 0.5][zoom >= 9] {polygon-fill: #E1A18E;}
#Crops_Landscape [maize <= 0.361111111111111][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [maize <= 1][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [maize <= 0.966666666666667][zoom < 9] {marker-fill: #9C1C0C;}
#Crops_Landscape [maize <= 0.866666666666667][zoom < 9] {marker-fill: #B43619;}
#Crops_Landscape [maize <= 0.6][zoom < 9] {marker-fill: #CB6244;}
#Crops_Landscape [maize <= 0.5][zoom < 9] {marker-fill: #E1A18E;}
#Crops_Landscape [maize <= 0.361111111111111][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.maize::numeric, 4) AS "maize" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE maize <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"6.7%", "max-value":"36%"},
{"color": "#E1A18E", "max-value":"50%"},
{"color": "#CB6244", "max-value":"60%"},
{"color": "#B43619", "max-value":"87%"},
{"color": "#9C1C0C", "max-value":"97%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Okra', 'okra', 'layer', 'f', '{"description":"Percent Of Households Growing Okra",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [okra = 0.1429][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [okra = 0.069][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [okra = 0.0333][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [okra = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [okra = 0.1429][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [okra = 0.069][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [okra = 0.0333][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [okra = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.okra::numeric, 4) AS "okra" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE okra <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "3.33%"},
{"value": "#AC2E15", "name": "6.9%"},
{"value": "#850200", "name": "14.3%"}]}', '100', 0, 'f'),
('Onions', 'onions', 'layer', 'f', '{"description":"Percent Of Households Growing Onions",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [onions = 0.4667][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [onions = 0.1724][zoom >= 9] {polygon-fill: #A22310;}
#Crops_Landscape [onions = 0.1][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [onions = 0.0333][zoom >= 9] {polygon-fill: #DC917C;}
#Crops_Landscape [onions = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [onions = 0.4667][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [onions = 0.1724][zoom < 9] {marker-fill: #A22310;}
#Crops_Landscape [onions = 0.1][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [onions = 0.0333][zoom < 9] {marker-fill: #DC917C;}
#Crops_Landscape [onions = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.onions::numeric, 4) AS "onions" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE onions <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#DC917C", "name": "3.33%"},
{"value": "#C04420", "name": "10%"},
{"value": "#A22310", "name": "17.2%"},
{"value": "#850200", "name": "46.7%"}]}', '100', 0, 'f'),
('Orange', 'orange', 'layer', 'f', '{"description":"Percent Of Households Growing Orange",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [orange = 0.0833][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [orange = 0.0667][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [orange = 0.0333][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [orange = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [orange = 0.0833][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [orange = 0.0667][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [orange = 0.0333][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [orange = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.orange::numeric, 4) AS "orange" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE orange <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "3.33%"},
{"value": "#AC2E15", "name": "6.67%"},
{"value": "#850200", "name": "8.33%"}]}', '100', 0, 'f'),
('Paddy', 'paddy', 'layer', 'f', '{"description":"Percent Of Households Growing Paddy",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [paddy <= 0.9][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [paddy <= 0.166666666666667][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [paddy <= 0.0666666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [paddy <= 0.9][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [paddy <= 0.166666666666667][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [paddy <= 0.0666666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.paddy::numeric, 4) AS "paddy" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE paddy <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"6.7%"},
{"color": "#C04420", "max-value":"17%"},
{"color": "#850200", "max-value":"90%"}]}', '100', 0, 'f'),
('Palm Oil', 'palm_oil', 'layer', 'f', '{"description":"Percent Of Households Growing Palm Oil",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [palm_oil = 0.4333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [palm_oil = 0.3333][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [palm_oil = 0.25][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [palm_oil = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [palm_oil = 0.4333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [palm_oil = 0.3333][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [palm_oil = 0.25][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [palm_oil = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.palm_oil::numeric, 4) AS "palm_oil" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE palm_oil <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "25%"},
{"value": "#AC2E15", "name": "33.3%"},
{"value": "#850200", "name": "43.3%"}]}', '100', 0, 'f'),
('Pigeon Pea', 'pigeon_pea', 'layer', 'f', '{"description":"Percent Of Households Growing Pigeon Pea",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [pigeon_pea = 0.3667][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [pigeon_pea = 0.0333][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [pigeon_pea = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [pigeon_pea = 0.3667][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [pigeon_pea = 0.0333][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [pigeon_pea = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pigeon_pea::numeric, 4) AS "pigeon_pea" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pigeon_pea <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#C04420", "name": "3.33%"},
{"value": "#850200", "name": "36.7%"}]}', '100', 0, 'f'),
('Pineapple', 'pineapple', 'layer', 'f', '{"description":"Percent Of Households Growing Pineapple",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [pineapple = 0.1667][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [pineapple = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [pineapple = 0.1667][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [pineapple = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pineapple::numeric, 4) AS "pineapple" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pineapple <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#850200", "name": "16.7%"}]}', '100', 0, 'f'),
('Pyrethrum', 'pyrethrum', 'layer', 'f', '{"description":"Percent Of Households Growing Pyrethrum",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [pyrethrum = 0.4333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [pyrethrum = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [pyrethrum = 0.4333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [pyrethrum = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pyrethrum::numeric, 4) AS "pyrethrum" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pyrethrum <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#850200", "name": "43.3%"}]}', '100', 0, 'f'),
('Rubber', 'rubber', 'layer', 'f', '{"description":"Percent Of Households Growing Rubber",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [rubber = 0.2333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [rubber = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [rubber = 0.2333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [rubber = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.rubber::numeric, 4) AS "rubber" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE rubber <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#850200", "name": "23.3%"}]}', '100', 0, 'f'),
('Sesame Simsim', 'sesame_simsim', 'layer', 'f', '{"description":"Percent Of Households Growing Sesame Simsim",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [sesame_simsim = 0.6667][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [sesame_simsim = 0.1667][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [sesame_simsim = 0.0333][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [sesame_simsim = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [sesame_simsim = 0.6667][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [sesame_simsim = 0.1667][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [sesame_simsim = 0.0333][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [sesame_simsim = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.sesame_simsim::numeric, 4) AS "sesame_simsim" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE sesame_simsim <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "3.33%"},
{"value": "#AC2E15", "name": "16.7%"},
{"value": "#850200", "name": "66.7%"}]}', '100', 0, 'f'),
('Sorghum', 'sorghum', 'layer', 'f', '{"description":"Percent Of Households Growing Sorghum",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [sorghum <= 0.533333333333333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [sorghum <= 0.166666666666667][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [sorghum <= 0.0333333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [sorghum <= 0.533333333333333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [sorghum <= 0.166666666666667][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [sorghum <= 0.0333333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.sorghum::numeric, 4) AS "sorghum" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE sorghum <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"3.3%"},
{"color": "#C04420", "max-value":"17%"},
{"color": "#850200", "max-value":"53%"}]}', '100', 0, 'f'),
('Soyabeans', 'soyabeans', 'layer', 'f', '{"description":"Percent Of Households Growing Soyabeans",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [soyabeans = 0.3793][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [soyabeans = 0.1379][zoom >= 9] {polygon-fill: #9C1C0C;}
#Crops_Landscape [soyabeans = 0.1][zoom >= 9] {polygon-fill: #B43619;}
#Crops_Landscape [soyabeans = 0.0667][zoom >= 9] {polygon-fill: #CB6244;}
#Crops_Landscape [soyabeans = 0.0333][zoom >= 9] {polygon-fill: #E1A18E;}
#Crops_Landscape [soyabeans = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [soyabeans = 0.3793][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [soyabeans = 0.1379][zoom < 9] {marker-fill: #9C1C0C;}
#Crops_Landscape [soyabeans = 0.1][zoom < 9] {marker-fill: #B43619;}
#Crops_Landscape [soyabeans = 0.0667][zoom < 9] {marker-fill: #CB6244;}
#Crops_Landscape [soyabeans = 0.0333][zoom < 9] {marker-fill: #E1A18E;}
#Crops_Landscape [soyabeans = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.soyabeans::numeric, 4) AS "soyabeans" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE soyabeans <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#E1A18E", "name": "3.33%"},
{"value": "#CB6244", "name": "6.67%"},
{"value": "#B43619", "name": "10%"},
{"value": "#9C1C0C", "name": "13.8%"},
{"value": "#850200", "name": "37.9%"}]}', '100', 0, 'f'),
('Sugar Cane', 'sugar_cane', 'layer', 'f', '{"description":"Percent Of Households Growing Sugar Cane",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [sugar_cane = 0.0667][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [sugar_cane = 0.0333][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [sugar_cane = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [sugar_cane = 0.0667][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [sugar_cane = 0.0333][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [sugar_cane = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.sugar_cane::numeric, 4) AS "sugar_cane" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE sugar_cane <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#C04420", "name": "3.33%"},
{"value": "#850200", "name": "6.67%"}]}', '100', 0, 'f'),
('Sunflower', 'sunflower', 'layer', 'f', '{"description":"Percent Of Households Growing Sunflower",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [sunflower = 0.2][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [sunflower = 0.0667][zoom >= 9] {polygon-fill: #AC2E15;}
#Crops_Landscape [sunflower = 0.0333][zoom >= 9] {polygon-fill: #D2775D;}
#Crops_Landscape [sunflower = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [sunflower = 0.2][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [sunflower = 0.0667][zoom < 9] {marker-fill: #AC2E15;}
#Crops_Landscape [sunflower = 0.0333][zoom < 9] {marker-fill: #D2775D;}
#Crops_Landscape [sunflower = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.sunflower::numeric, 4) AS "sunflower" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE sunflower <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#D2775D", "name": "3.33%"},
{"value": "#AC2E15", "name": "6.67%"},
{"value": "#850200", "name": "20%"}]}', '100', 0, 'f'),
('Sweet Potatoes', 'sweet_potatoes', 'layer', 'f', '{"description":"Percent Of Households Growing Sweet Potatoes",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [sweet_potatoes <= 0.8][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [sweet_potatoes <= 0.333333333333333][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [sweet_potatoes <= 0.166666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [sweet_potatoes <= 0.8][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [sweet_potatoes <= 0.333333333333333][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [sweet_potatoes <= 0.166666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.sweet_potatoes::numeric, 4) AS "sweet_potatoes" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE sweet_potatoes <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"17%"},
{"color": "#C04420", "max-value":"33%"},
{"color": "#850200", "max-value":"80%"}]}', '100', 0, 'f'),
('Tea', 'tea', 'layer', 'f', '{"description":"Percent Of Households Growing Tea",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [tea = 0.2333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [tea = 0.0667][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [tea = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [tea = 0.2333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [tea = 0.0667][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [tea = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.tea::numeric, 4) AS "tea" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE tea <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#C04420", "name": "6.67%"},
{"value": "#850200", "name": "23.3%"}]}', '100', 0, 'f'),
('Timber', 'timber', 'layer', 'f', '{"description":"Percent Of Households Growing Timber",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [timber = 0.1333][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [timber = 0.1034][zoom >= 9] {polygon-fill: #A22310;}
#Crops_Landscape [timber = 0.1][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [timber = 0.0333][zoom >= 9] {polygon-fill: #DC917C;}
#Crops_Landscape [timber = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [timber = 0.1333][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [timber = 0.1034][zoom < 9] {marker-fill: #A22310;}
#Crops_Landscape [timber = 0.1][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [timber = 0.0333][zoom < 9] {marker-fill: #DC917C;}
#Crops_Landscape [timber = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.timber::numeric, 4) AS "timber" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE timber <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#DC917C", "name": "3.33%"},
{"value": "#C04420", "name": "10%"},
{"value": "#A22310", "name": "10.3%"},
{"value": "#850200", "name": "13.3%"}]}', '100', 0, 'f'),
('Tomatoes', 'tomatoes', 'layer', 'f', '{"description":"Percent Of Households Growing Tomatoes",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [tomatoes = 0.1034][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [tomatoes = 0.1][zoom >= 9] {polygon-fill: #9C1C0C;}
#Crops_Landscape [tomatoes = 0.0667][zoom >= 9] {polygon-fill: #B43619;}
#Crops_Landscape [tomatoes = 0.0517][zoom >= 9] {polygon-fill: #CB6244;}
#Crops_Landscape [tomatoes = 0.0333][zoom >= 9] {polygon-fill: #E1A18E;}
#Crops_Landscape [tomatoes = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [tomatoes = 0.1034][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [tomatoes = 0.1][zoom < 9] {marker-fill: #9C1C0C;}
#Crops_Landscape [tomatoes = 0.0667][zoom < 9] {marker-fill: #B43619;}
#Crops_Landscape [tomatoes = 0.0517][zoom < 9] {marker-fill: #CB6244;}
#Crops_Landscape [tomatoes = 0.0333][zoom < 9] {marker-fill: #E1A18E;}
#Crops_Landscape [tomatoes = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.tomatoes::numeric, 4) AS "tomatoes" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE tomatoes <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#E1A18E", "name": "3.33%"},
{"value": "#CB6244", "name": "5.17%"},
{"value": "#B43619", "name": "6.67%"},
{"value": "#9C1C0C", "name": "10%"},
{"value": "#850200", "name": "10.3%"}]}', '100', 0, 'f'),
('Wheat', 'wheat', 'layer', 'f', '{"description":"Percent Of Households Growing Wheat",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [wheat = 0.4833][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [wheat = 0.3333][zoom >= 9] {polygon-fill: #A22310;}
#Crops_Landscape [wheat = 0.1][zoom >= 9] {polygon-fill: #C04420;}
#Crops_Landscape [wheat = 0.0345][zoom >= 9] {polygon-fill: #DC917C;}
#Crops_Landscape [wheat = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [wheat = 0.4833][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [wheat = 0.3333][zoom < 9] {marker-fill: #A22310;}
#Crops_Landscape [wheat = 0.1][zoom < 9] {marker-fill: #C04420;}
#Crops_Landscape [wheat = 0.0345][zoom < 9] {marker-fill: #DC917C;}
#Crops_Landscape [wheat = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.wheat::numeric, 4) AS "wheat" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE wheat <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#DC917C", "name": "3.45%"},
{"value": "#C04420", "name": "10%"},
{"value": "#A22310", "name": "33.3%"},
{"value": "#850200", "name": "48.3%"}]}', '100', 0, 'f'),
('Yams', 'yams', 'layer', 'f', '{"description":"Percent Of Households Growing Yams",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Crops_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Crops_Landscape [yams = 0.9286][zoom >= 9] {polygon-fill: #850200;}
#Crops_Landscape [yams = 0.3793][zoom >= 9] {polygon-fill: #9C1C0C;}
#Crops_Landscape [yams = 0.2241][zoom >= 9] {polygon-fill: #B43619;}
#Crops_Landscape [yams = 0.0833][zoom >= 9] {polygon-fill: #CB6244;}
#Crops_Landscape [yams = 0.0333][zoom >= 9] {polygon-fill: #E1A18E;}
#Crops_Landscape [yams = 0][zoom >= 9] {polygon-fill: #F8DFD8;}


#Crops_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Crops_Landscape [yams = 0.9286][zoom < 9] {marker-fill: #850200;}
#Crops_Landscape [yams = 0.3793][zoom < 9] {marker-fill: #9C1C0C;}
#Crops_Landscape [yams = 0.2241][zoom < 9] {marker-fill: #B43619;}
#Crops_Landscape [yams = 0.0833][zoom < 9] {marker-fill: #CB6244;}
#Crops_Landscape [yams = 0.0333][zoom < 9] {marker-fill: #E1A18E;}
#Crops_Landscape [yams = 0][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.yams::numeric, 4) AS "yams" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE yams <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#F8DFD8", "name":"0%"},
{"value": "#E1A18E", "name": "3.33%"},
{"value": "#CB6244", "name": "8.33%"},
{"value": "#B43619", "name": "22.4%"},
{"value": "#9C1C0C", "name": "37.9%"},
{"value": "#850200", "name": "92.9%"}]}', '100', 0, 'f'),
('Area Farmed', 'mean_hh_area_farmed', 'layer', 'f', '{"description":"Mean Household Area Farmed",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 9.05967966666667][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 7.21806451612903][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 5.327][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 2.8][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 1.59255][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 0.743533333333333][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 9.05967966666667][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 7.21806451612903][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 5.327][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 2.8][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 1.59255][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [mean_hh_area_farmed <= 0.743533333333333][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_hh_area_farmed::numeric, 4) AS "mean_hh_area_farmed" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_hh_area_farmed <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0.52", "max-value":"0.74"},
{"color": "#B3CF99", "max-value":"1.6"},
{"color": "#80AB54", "max-value":"2.8"},
{"color": "#528C28", "max-value":"5.3"},
{"color": "#2B7414", "max-value":"7.2"},
{"color": "#055D00", "max-value":"9.1"}]}', '100', 0, 'f'),
('Area Owned', 'mean_area_owned', 'layer', 'f', '{"description":"Mean Household Area Owned",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [mean_area_owned <= 9.33016259259259][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [mean_area_owned <= 7.99865862068966][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [mean_area_owned <= 4.03266666666667][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [mean_area_owned <= 2.36551724137931][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [mean_area_owned <= 1.51478][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [mean_area_owned <= 0.719033333333333][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [mean_area_owned <= 9.33016259259259][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [mean_area_owned <= 7.99865862068966][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [mean_area_owned <= 4.03266666666667][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [mean_area_owned <= 2.36551724137931][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [mean_area_owned <= 1.51478][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [mean_area_owned <= 0.719033333333333][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_area_owned::numeric, 4) AS "mean_area_owned" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_area_owned <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0.4", "max-value":"0.72"},
{"color": "#B3CF99", "max-value":"1.5"},
{"color": "#80AB54", "max-value":"2.4"},
{"color": "#528C28", "max-value":"4"},
{"color": "#2B7414", "max-value":"8"},
{"color": "#055D00", "max-value":"9.3"}]}', '100', 0, 'f'),
('Commercialization', 'avg_pct_crops_any_sold', 'layer', 'f', '{"description":"Mean Percentage Of Crops Where Any Amount Was Sold",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.788333333333333][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.6][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.496774193548387][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.377777777777778][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.319175170068027][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.26264367816092][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.788333333333333][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.6][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.496774193548387][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.377777777777778][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.319175170068027][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [avg_pct_crops_any_sold <= 0.26264367816092][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.avg_pct_crops_any_sold::numeric, 4) AS "avg_pct_crops_any_sold" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE avg_pct_crops_any_sold <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"12%", "max-value":"26%"},
{"color": "#B3CF99", "max-value":"32%"},
{"color": "#80AB54", "max-value":"38%"},
{"color": "#528C28", "max-value":"50%"},
{"color": "#2B7414", "max-value":"60%"},
{"color": "#055D00", "max-value":"79%"}]}', '100', 0, 'f'),
('Field Sizes', 'median_field_size', 'layer', 'f', '{"description":"Median Field Size",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [median_field_size <= 4.1][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [median_field_size <= 2.2204][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [median_field_size <= 1.7][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [median_field_size <= 1.1][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [median_field_size <= 0.458625][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [median_field_size <= 0.2541][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [median_field_size <= 4.1][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [median_field_size <= 2.2204][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [median_field_size <= 1.7][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [median_field_size <= 1.1][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [median_field_size <= 0.458625][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [median_field_size <= 0.2541][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.median_field_size::numeric, 4) AS "median_field_size" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE median_field_size <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0.12", "max-value":"0.25"},
{"color": "#B3CF99", "max-value":"0.46"},
{"color": "#80AB54", "max-value":"1.1"},
{"color": "#528C28", "max-value":"1.7"},
{"color": "#2B7414", "max-value":"2.2"},
{"color": "#055D00", "max-value":"4.1"}]}', '100', 0, 'f'),
('Fungicides', 'fungicide_rate', 'layer', 'f', '{"description":"Percentage Of Fields Using Fungicides",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [fungicide_rate = 0.025][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [fungicide_rate = 0.0192][zoom >= 9] {polygon-fill: #458522;}
#AgIntensification_Landscape [fungicide_rate = 0.0167][zoom >= 9] {polygon-fill: #91B76B;}
#AgIntensification_Landscape [fungicide_rate = 0][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [fungicide_rate = 0.025][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [fungicide_rate = 0.0192][zoom < 9] {marker-fill: #458522;}
#AgIntensification_Landscape [fungicide_rate = 0.0167][zoom < 9] {marker-fill: #91B76B;}
#AgIntensification_Landscape [fungicide_rate = 0][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.fungicide_rate::numeric, 4) AS "fungicide_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE fungicide_rate <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#E8F4DD", "name":"0%"},
{"value": "#91B76B", "name": "1.67%"},
{"value": "#458522", "name": "1.92%"},
{"value": "#055D00", "name": "2.5%"}]}', '100', 0, 'f'),
('Herbicides', 'herbicide_rate', 'layer', 'f', '{"description":"Percentage Of Fields Using Herbicides",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [herbicide_rate <= 0.763611111111111][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [herbicide_rate <= 0.374861111111111][zoom >= 9] {polygon-fill: #357A19;}
#AgIntensification_Landscape [herbicide_rate <= 0.198387096774194][zoom >= 9] {polygon-fill: #669933;}
#AgIntensification_Landscape [herbicide_rate <= 0.137222222222222][zoom >= 9] {polygon-fill: #A7C688;}
#AgIntensification_Landscape [herbicide_rate <= 0.00333333333333333][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [herbicide_rate <= 0.763611111111111][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [herbicide_rate <= 0.374861111111111][zoom < 9] {marker-fill: #357A19;}
#AgIntensification_Landscape [herbicide_rate <= 0.198387096774194][zoom < 9] {marker-fill: #669933;}
#AgIntensification_Landscape [herbicide_rate <= 0.137222222222222][zoom < 9] {marker-fill: #A7C688;}
#AgIntensification_Landscape [herbicide_rate <= 0.00333333333333333][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.herbicide_rate::numeric, 4) AS "herbicide_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE herbicide_rate <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"0.33%"},
{"color": "#A7C688", "max-value":"14%"},
{"color": "#669933", "max-value":"20%"},
{"color": "#357A19", "max-value":"37%"},
{"color": "#055D00", "max-value":"76%"}]}', '100', 0, 'f'),
('Inorganic Fertilizer', 'pct_fields_inorg_fert', 'layer', 'f', '{"description":"Percentage Of Fields Using Inorganic Ferilizers",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.812222222222222][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.451666666666667][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.258130081300813][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.120689655172414][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.0666666666666667][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.0261111111111111][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.812222222222222][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.451666666666667][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.258130081300813][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.120689655172414][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.0666666666666667][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [pct_fields_inorg_fert <= 0.0261111111111111][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pct_fields_inorg_fert::numeric, 4) AS "pct_fields_inorg_fert" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pct_fields_inorg_fert <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"2.6%"},
{"color": "#B3CF99", "max-value":"6.7%"},
{"color": "#80AB54", "max-value":"12%"},
{"color": "#528C28", "max-value":"26%"},
{"color": "#2B7414", "max-value":"45%"},
{"color": "#055D00", "max-value":"81%"}]}', '100', 0, 'f'),
('Intercropping', 'intercrop_rate', 'layer', 'f', '{"description":"Percentage Of Fields Intercropped",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [intercrop_rate <= 0.776666666666667][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [intercrop_rate <= 0.521111111111111][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [intercrop_rate <= 0.31462962962963][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [intercrop_rate <= 0.271279877998945][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [intercrop_rate <= 0.161111111111111][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [intercrop_rate <= 0.0847619047619048][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [intercrop_rate <= 0.776666666666667][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [intercrop_rate <= 0.521111111111111][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [intercrop_rate <= 0.31462962962963][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [intercrop_rate <= 0.271279877998945][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [intercrop_rate <= 0.161111111111111][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [intercrop_rate <= 0.0847619047619048][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.intercrop_rate::numeric, 4) AS "intercrop_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE intercrop_rate <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0.67%", "max-value":"8.5%"},
{"color": "#B3CF99", "max-value":"16%"},
{"color": "#80AB54", "max-value":"27%"},
{"color": "#528C28", "max-value":"31%"},
{"color": "#2B7414", "max-value":"52%"},
{"color": "#055D00", "max-value":"78%"}]}', '100', 0, 'f'),
('Irrigation', 'pct_fields_irrigated', 'layer', 'f', '{"description":"Percentage Of Fields Irrigated",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [pct_fields_irrigated <= 0.4375][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [pct_fields_irrigated <= 0.0166666666666667][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [pct_fields_irrigated <= 0.4375][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [pct_fields_irrigated <= 0.0166666666666667][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pct_fields_irrigated::numeric, 4) AS "pct_fields_irrigated" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pct_fields_irrigated <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"1.7%"},
{"color": "#055D00", "max-value":"44%"}]}', '100', 0, 'f'),
('Number Of Fields', 'mean_hh_number_fields', 'layer', 'f', '{"description":"Mean Household Number Of Fields",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [mean_hh_number_fields <= 4.23333333333333][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [mean_hh_number_fields <= 3.43333333333333][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [mean_hh_number_fields <= 3.13333333333333][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [mean_hh_number_fields <= 2.73333333333333][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [mean_hh_number_fields <= 2.36666666666667][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [mean_hh_number_fields <= 2.1][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [mean_hh_number_fields <= 4.23333333333333][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [mean_hh_number_fields <= 3.43333333333333][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [mean_hh_number_fields <= 3.13333333333333][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [mean_hh_number_fields <= 2.73333333333333][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [mean_hh_number_fields <= 2.36666666666667][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [mean_hh_number_fields <= 2.1][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_hh_number_fields::numeric, 4) AS "mean_hh_number_fields" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_hh_number_fields <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"1.3", "max-value":"2.1"},
{"color": "#B3CF99", "max-value":"2.4"},
{"color": "#80AB54", "max-value":"2.7"},
{"color": "#528C28", "max-value":"3.1"},
{"color": "#2B7414", "max-value":"3.4"},
{"color": "#055D00", "max-value":"4.2"}]}', '100', 0, 'f'),
('Pesticides', 'pesticide_rate', 'layer', 'f', '{"description":"Percentage Of Fields Using Pesticides",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [pesticide_rate <= 0.330833333333333][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [pesticide_rate <= 0.178728354978355][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [pesticide_rate <= 0.127489177489177][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [pesticide_rate <= 0.0666666666666667][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [pesticide_rate <= 0.0501388888888889][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [pesticide_rate <= 0.010752688172043][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [pesticide_rate <= 0.330833333333333][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [pesticide_rate <= 0.178728354978355][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [pesticide_rate <= 0.127489177489177][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [pesticide_rate <= 0.0666666666666667][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [pesticide_rate <= 0.0501388888888889][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [pesticide_rate <= 0.010752688172043][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pesticide_rate::numeric, 4) AS "pesticide_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pesticide_rate <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"1.1%"},
{"color": "#B3CF99", "max-value":"5%"},
{"color": "#80AB54", "max-value":"6.7%"},
{"color": "#528C28", "max-value":"13%"},
{"color": "#2B7414", "max-value":"18%"},
{"color": "#055D00", "max-value":"33%"}]}', '100', 0, 'f'),
('Seed Purchasing', 'pct_fields_purchased_seed', 'layer', 'f', '{"description":"Percentage Of Fields Using Purchased Seeds",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.694589947089947][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.43462962962963][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.317936507936508][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.259047619047619][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.197222222222222][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.148313782991202][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.694589947089947][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.43462962962963][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.317936507936508][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.259047619047619][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.197222222222222][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [pct_fields_purchased_seed <= 0.148313782991202][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pct_fields_purchased_seed::numeric, 4) AS "pct_fields_purchased_seed" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pct_fields_purchased_seed <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"3.4%", "max-value":"15%"},
{"color": "#B3CF99", "max-value":"20%"},
{"color": "#80AB54", "max-value":"26%"},
{"color": "#528C28", "max-value":"32%"},
{"color": "#2B7414", "max-value":"43%"},
{"color": "#055D00", "max-value":"69%"}]}', '100', 0, 'f'),
('Annual Crops', 'annualcropproduction', 'layer', 'f', '{"description":"Mean Household Annual Value Of Production Of Annual Crops",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [annualcropproduction <= 1838.98344066237][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [annualcropproduction <= 588.473875763371][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgValue_Landscape [annualcropproduction <= 332.198358766492][zoom >= 9] {polygon-fill: #B43619;}
#AgValue_Landscape [annualcropproduction <= 254.259093619559][zoom >= 9] {polygon-fill: #CB6244;}
#AgValue_Landscape [annualcropproduction <= 177.088823632759][zoom >= 9] {polygon-fill: #E1A18E;}
#AgValue_Landscape [annualcropproduction <= 137.282228580512][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [annualcropproduction <= 1838.98344066237][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [annualcropproduction <= 588.473875763371][zoom < 9] {marker-fill: #9C1C0C;}
#AgValue_Landscape [annualcropproduction <= 332.198358766492][zoom < 9] {marker-fill: #B43619;}
#AgValue_Landscape [annualcropproduction <= 254.259093619559][zoom < 9] {marker-fill: #CB6244;}
#AgValue_Landscape [annualcropproduction <= 177.088823632759][zoom < 9] {marker-fill: #E1A18E;}
#AgValue_Landscape [annualcropproduction <= 137.282228580512][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.annualcropproduction::numeric, 4) AS "annualcropproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE annualcropproduction <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"95", "max-value":"140"},
{"color": "#E1A18E", "max-value":"180"},
{"color": "#CB6244", "max-value":"250"},
{"color": "#B43619", "max-value":"330"},
{"color": "#9C1C0C", "max-value":"590"},
{"color": "#850200", "max-value":"1800"}]}', '100', 0, 'f'),
('Crop Byproducts', 'cropbyproductproduction', 'layer', 'f', '{"description":"Mean Household Annual Value Of Production Of Crop Byproducts",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [cropbyproductproduction <= 1124.5016865992][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [cropbyproductproduction <= 28.1217330061122][zoom >= 9] {polygon-fill: #AC2E15;}
#AgValue_Landscape [cropbyproductproduction <= 2.74201239866476][zoom >= 9] {polygon-fill: #D2775D;}
#AgValue_Landscape [cropbyproductproduction <= 1.00618675363292][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [cropbyproductproduction <= 1124.5016865992][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [cropbyproductproduction <= 28.1217330061122][zoom < 9] {marker-fill: #AC2E15;}
#AgValue_Landscape [cropbyproductproduction <= 2.74201239866476][zoom < 9] {marker-fill: #D2775D;}
#AgValue_Landscape [cropbyproductproduction <= 1.00618675363292][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.cropbyproductproduction::numeric, 4) AS "cropbyproductproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE cropbyproductproduction <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0", "max-value":"1"},
{"color": "#D2775D", "max-value":"2.7"},
{"color": "#AC2E15", "max-value":"28"},
{"color": "#850200", "max-value":"1100"}]}', '100', 0, 'f'),
('Livestock', 'livestockproduction', 'layer', 'f', '{"description":"Mean Household Annual Value Of Production Of Livestock",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [livestockproduction <= 581.265254627658][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [livestockproduction <= 108.190271816881][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgValue_Landscape [livestockproduction <= 73.9927392151626][zoom >= 9] {polygon-fill: #B43619;}
#AgValue_Landscape [livestockproduction <= 55.2619378766806][zoom >= 9] {polygon-fill: #CB6244;}
#AgValue_Landscape [livestockproduction <= 44.1965654707145][zoom >= 9] {polygon-fill: #E1A18E;}
#AgValue_Landscape [livestockproduction <= 37.3526333916177][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [livestockproduction <= 581.265254627658][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [livestockproduction <= 108.190271816881][zoom < 9] {marker-fill: #9C1C0C;}
#AgValue_Landscape [livestockproduction <= 73.9927392151626][zoom < 9] {marker-fill: #B43619;}
#AgValue_Landscape [livestockproduction <= 55.2619378766806][zoom < 9] {marker-fill: #CB6244;}
#AgValue_Landscape [livestockproduction <= 44.1965654707145][zoom < 9] {marker-fill: #E1A18E;}
#AgValue_Landscape [livestockproduction <= 37.3526333916177][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.livestockproduction::numeric, 4) AS "livestockproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE livestockproduction <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"14", "max-value":"37"},
{"color": "#E1A18E", "max-value":"44"},
{"color": "#CB6244", "max-value":"55"},
{"color": "#B43619", "max-value":"74"},
{"color": "#9C1C0C", "max-value":"110"},
{"color": "#850200", "max-value":"580"}]}', '100', 0, 'f'),
('Livestock Byproducts', 'livestockbyproductproduction', 'layer', 'f', '{"description":"Mean Household Annual Value Of Production Of Livestock Byproducts",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [livestockbyproductproduction <= 235.933214072749][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [livestockbyproductproduction <= 50.0286123032904][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgValue_Landscape [livestockbyproductproduction <= 43.6608948744512][zoom >= 9] {polygon-fill: #B43619;}
#AgValue_Landscape [livestockbyproductproduction <= 23.6647591797806][zoom >= 9] {polygon-fill: #CB6244;}
#AgValue_Landscape [livestockbyproductproduction <= 16.4892665474061][zoom >= 9] {polygon-fill: #E1A18E;}
#AgValue_Landscape [livestockbyproductproduction <= 10.5724508050089][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [livestockbyproductproduction <= 235.933214072749][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [livestockbyproductproduction <= 50.0286123032904][zoom < 9] {marker-fill: #9C1C0C;}
#AgValue_Landscape [livestockbyproductproduction <= 43.6608948744512][zoom < 9] {marker-fill: #B43619;}
#AgValue_Landscape [livestockbyproductproduction <= 23.6647591797806][zoom < 9] {marker-fill: #CB6244;}
#AgValue_Landscape [livestockbyproductproduction <= 16.4892665474061][zoom < 9] {marker-fill: #E1A18E;}
#AgValue_Landscape [livestockbyproductproduction <= 10.5724508050089][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.livestockbyproductproduction::numeric, 4) AS "livestockbyproductproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE livestockbyproductproduction <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"6.1", "max-value":"11"},
{"color": "#E1A18E", "max-value":"16"},
{"color": "#CB6244", "max-value":"24"},
{"color": "#B43619", "max-value":"44"},
{"color": "#9C1C0C", "max-value":"50"},
{"color": "#850200", "max-value":"240"}]}', '100', 0, 'f'),
('Permanent Crops', 'permanentcropproduction', 'layer', 'f', '{"description":"Mean Household Annual Value Of Production Of Permanent Crops",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [permanentcropproduction <= 1320.68000613309][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [permanentcropproduction <= 180.792767732962][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgValue_Landscape [permanentcropproduction <= 77.6782568382012][zoom >= 9] {polygon-fill: #B43619;}
#AgValue_Landscape [permanentcropproduction <= 59.0913305516922][zoom >= 9] {polygon-fill: #CB6244;}
#AgValue_Landscape [permanentcropproduction <= 18.5602265951103][zoom >= 9] {polygon-fill: #E1A18E;}
#AgValue_Landscape [permanentcropproduction <= 8.15742397137746][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [permanentcropproduction <= 1320.68000613309][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [permanentcropproduction <= 180.792767732962][zoom < 9] {marker-fill: #9C1C0C;}
#AgValue_Landscape [permanentcropproduction <= 77.6782568382012][zoom < 9] {marker-fill: #B43619;}
#AgValue_Landscape [permanentcropproduction <= 59.0913305516922][zoom < 9] {marker-fill: #CB6244;}
#AgValue_Landscape [permanentcropproduction <= 18.5602265951103][zoom < 9] {marker-fill: #E1A18E;}
#AgValue_Landscape [permanentcropproduction <= 8.15742397137746][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.permanentcropproduction::numeric, 4) AS "permanentcropproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE permanentcropproduction <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0", "max-value":"8.2"},
{"color": "#E1A18E", "max-value":"19"},
{"color": "#CB6244", "max-value":"59"},
{"color": "#B43619", "max-value":"78"},
{"color": "#9C1C0C", "max-value":"180"},
{"color": "#850200", "max-value":"1300"}]}', '100', 0, 'f'),
('Total', 'totalagriculturalproduction', 'layer', 'f', '{"description":"Mean Household Annual Value Of Production Of All Agriculture",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [totalagriculturalproduction <= 3553.13554124502][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [totalagriculturalproduction <= 1189.16718985221][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgValue_Landscape [totalagriculturalproduction <= 562.610803632371][zoom >= 9] {polygon-fill: #B43619;}
#AgValue_Landscape [totalagriculturalproduction <= 434.102672632232][zoom >= 9] {polygon-fill: #CB6244;}
#AgValue_Landscape [totalagriculturalproduction <= 328.607012318489][zoom >= 9] {polygon-fill: #E1A18E;}
#AgValue_Landscape [totalagriculturalproduction <= 293.55745804583][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [totalagriculturalproduction <= 3553.13554124502][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [totalagriculturalproduction <= 1189.16718985221][zoom < 9] {marker-fill: #9C1C0C;}
#AgValue_Landscape [totalagriculturalproduction <= 562.610803632371][zoom < 9] {marker-fill: #B43619;}
#AgValue_Landscape [totalagriculturalproduction <= 434.102672632232][zoom < 9] {marker-fill: #CB6244;}
#AgValue_Landscape [totalagriculturalproduction <= 328.607012318489][zoom < 9] {marker-fill: #E1A18E;}
#AgValue_Landscape [totalagriculturalproduction <= 293.55745804583][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.totalagriculturalproduction::numeric, 4) AS "totalagriculturalproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE totalagriculturalproduction <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"200", "max-value":"290"},
{"color": "#E1A18E", "max-value":"330"},
{"color": "#CB6244", "max-value":"430"},
{"color": "#B43619", "max-value":"560"},
{"color": "#9C1C0C", "max-value":"1200"},
{"color": "#850200", "max-value":"3600"}]}', '100', 0, 'f'),
('Yields Quartile', 'mean_yield_quantile', 'layer', 'f', '{"description":"Mean Yield Quartle Across All Crops",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [mean_yield_quantile <= 3.25007936507936][zoom >= 9] {polygon-fill: #055D00;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.75611111111111][zoom >= 9] {polygon-fill: #2B7414;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.55518518518519][zoom >= 9] {polygon-fill: #528C28;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.4402003429577][zoom >= 9] {polygon-fill: #80AB54;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.23511904761905][zoom >= 9] {polygon-fill: #B3CF99;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.03388888888889][zoom >= 9] {polygon-fill: #E8F4DD;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [mean_yield_quantile <= 3.25007936507936][zoom < 9] {marker-fill: #055D00;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.75611111111111][zoom < 9] {marker-fill: #2B7414;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.55518518518519][zoom < 9] {marker-fill: #528C28;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.4402003429577][zoom < 9] {marker-fill: #80AB54;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.23511904761905][zoom < 9] {marker-fill: #B3CF99;}
#AgIntensification_Landscape [mean_yield_quantile <= 2.03388888888889][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_yield_quantile::numeric, 4) AS "mean_yield_quantile" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_yield_quantile <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"1.9", "max-value":"2"},
{"color": "#B3CF99", "max-value":"2.2"},
{"color": "#80AB54", "max-value":"2.4"},
{"color": "#528C28", "max-value":"2.6"},
{"color": "#2B7414", "max-value":"2.8"},
{"color": "#055D00", "max-value":"3.3"}]}', '100', 0, 'f'),
('Dietary Diversity', 'mean_diet_diversity', 'layer', 'f', '{"description":"Mean Household Diet Diversity Score",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.704910714285714][zoom >= 9] {polygon-fill: #850200;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.646666666666667][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.608095238095238][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.59][zoom >= 9] {polygon-fill: #CB6244;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.567857142857143][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.556666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.704910714285714][zoom < 9] {marker-fill: #850200;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.646666666666667][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.608095238095238][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.59][zoom < 9] {marker-fill: #CB6244;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.567857142857143][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_diet_diversity <= 0.556666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_diet_diversity::numeric, 4) AS "mean_diet_diversity" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_diet_diversity <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0.47", "max-value":"0.56"},
{"color": "#E1A18E", "max-value":"0.57"},
{"color": "#CB6244", "max-value":"0.59"},
{"color": "#B43619", "max-value":"0.61"},
{"color": "#9C1C0C", "max-value":"0.65"},
{"color": "#850200", "max-value":"0.7"}]}', '100', 0, 'f'),
('Food As A Percentage Of Total Spending', 'food_as_percent_total_spending', 'layer', 'f', '{"description":"Percentage Of All Expenditures On Food, Household Mean",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 73.928556680571][zoom >= 9] {polygon-fill: #850200;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 55.0838193995516][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 51.6545272150718][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 48.2648276108501][zoom >= 9] {polygon-fill: #CB6244;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 45.5535553695997][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 41.775152138614][zoom >= 9] {polygon-fill: #F8DFD8;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 73.928556680571][zoom < 9] {marker-fill: #850200;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 55.0838193995516][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 51.6545272150718][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 48.2648276108501][zoom < 9] {marker-fill: #CB6244;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 45.5535553695997][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [food_as_percent_total_spending <= 41.775152138614][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.food_as_percent_total_spending::numeric, 4) AS "food_as_percent_total_spending" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE food_as_percent_total_spending <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"30%", "max-value":"42%"},
{"color": "#E1A18E", "max-value":"46%"},
{"color": "#CB6244", "max-value":"48%"},
{"color": "#B43619", "max-value":"52%"},
{"color": "#9C1C0C", "max-value":"55%"},
{"color": "#850200", "max-value":"74%"}]}', '100', 0, 'f'),
('Food Consumption', 'mean_food_consumption_value', 'layer', 'f', '{"description":"Mean Household Annual Food Consumption Value",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 1369.37455277281][zoom >= 9] {polygon-fill: #850200;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 659.469669963201][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 507.486672629696][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 464.304645340751][zoom >= 9] {polygon-fill: #CB6244;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 375.219861707201][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 276.043216499762][zoom >= 9] {polygon-fill: #F8DFD8;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 1369.37455277281][zoom < 9] {marker-fill: #850200;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 659.469669963201][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 507.486672629696][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 464.304645340751][zoom < 9] {marker-fill: #CB6244;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 375.219861707201][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_food_consumption_value <= 276.043216499762][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_food_consumption_value::numeric, 4) AS "mean_food_consumption_value" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_food_consumption_value <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"210", "max-value":"280"},
{"color": "#E1A18E", "max-value":"380"},
{"color": "#CB6244", "max-value":"460"},
{"color": "#B43619", "max-value":"510"},
{"color": "#9C1C0C", "max-value":"660"},
{"color": "#850200", "max-value":"1400"}]}', '100', 0, 'f'),
('Food Shortages', 'percent_shortage_past_year', 'layer', 'f', '{"description":"Percentage Of Households Reporting Food Shortages In The Past Year",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 93.3333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 83.3333333333333][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 80][zoom >= 9] {polygon-fill: #CB6344;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 50][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 37.0967741935484][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 15.625][zoom >= 9] {polygon-fill: #850200;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 93.3333333333333][zoom < 9] {marker-fill: #F8DFD8;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 83.3333333333333][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 80][zoom < 9] {marker-fill: #CB6344;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 50][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 37.0967741935484][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [percent_shortage_past_year <= 15.625][zoom < 9] {marker-fill: #850200;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_shortage_past_year::numeric, 4) AS "percent_shortage_past_year" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_shortage_past_year <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#850200", "min-value":"0%", "max-value":"16%"},
{"color": "#9C1C0C", "max-value":"37%"},
{"color": "#B43619", "max-value":"50%"},
{"color": "#CB6344", "max-value":"80%"},
{"color": "#E1A18E", "max-value":"83%"},
{"color": "#F8DFD8", "max-value":"93%"}]}', '100', 0, 'f'),
('Food Spending', 'mean_food_spending', 'layer', 'f', '{"description":"Mean Household Annual Food Spending",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [mean_food_spending <= 1846.83735338086][zoom >= 9] {polygon-fill: #850200;}
#FoodSecurity_Landscape [mean_food_spending <= 939.963503334867][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_food_spending <= 633.581059928444][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [mean_food_spending <= 534.283922114047][zoom >= 9] {polygon-fill: #CB6244;}
#FoodSecurity_Landscape [mean_food_spending <= 398.193246304244][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_food_spending <= 367.056151645207][zoom >= 9] {polygon-fill: #F8DFD8;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [mean_food_spending <= 1846.83735338086][zoom < 9] {marker-fill: #850200;}
#FoodSecurity_Landscape [mean_food_spending <= 939.963503334867][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_food_spending <= 633.581059928444][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [mean_food_spending <= 534.283922114047][zoom < 9] {marker-fill: #CB6244;}
#FoodSecurity_Landscape [mean_food_spending <= 398.193246304244][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_food_spending <= 367.056151645207][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_food_spending::numeric, 4) AS "mean_food_spending" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_food_spending <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"270", "max-value":"370"},
{"color": "#E1A18E", "max-value":"400"},
{"color": "#CB6244", "max-value":"530"},
{"color": "#B43619", "max-value":"630"},
{"color": "#9C1C0C", "max-value":"940"},
{"color": "#850200", "max-value":"1800"}]}', '100', 0, 'f'),
('Nonfood Spending', 'mean_nonfood_spending', 'layer', 'f', '{"description":"Mean Household Annual Nonfood Spending",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 2914.38851870334][zoom >= 9] {polygon-fill: #850200;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 1371.58440596132][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 836.549085897848][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 722.586421403682][zoom >= 9] {polygon-fill: #CB6244;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 552.291389193452][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 341.132133308052][zoom >= 9] {polygon-fill: #F8DFD8;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 2914.38851870334][zoom < 9] {marker-fill: #850200;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 1371.58440596132][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 836.549085897848][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 722.586421403682][zoom < 9] {marker-fill: #CB6244;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 552.291389193452][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [mean_nonfood_spending <= 341.132133308052][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_nonfood_spending::numeric, 4) AS "mean_nonfood_spending" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_nonfood_spending <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"220", "max-value":"340"},
{"color": "#E1A18E", "max-value":"550"},
{"color": "#CB6244", "max-value":"720"},
{"color": "#B43619", "max-value":"840"},
{"color": "#9C1C0C", "max-value":"1400"},
{"color": "#850200", "max-value":"2900"}]}', '100', 0, 'f'),
('Number Of Meals', 'avg_meals', 'layer', 'f', '{"description":"Average Number Of Meals Per Day",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#FoodSecurity_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#FoodSecurity_Landscape [avg_meals <= 3.125][zoom >= 9] {polygon-fill: #850200;}
#FoodSecurity_Landscape [avg_meals <= 2.9][zoom >= 9] {polygon-fill: #9C1C0C;}
#FoodSecurity_Landscape [avg_meals <= 2.7][zoom >= 9] {polygon-fill: #B43619;}
#FoodSecurity_Landscape [avg_meals <= 2.53333333333333][zoom >= 9] {polygon-fill: #CB6244;}
#FoodSecurity_Landscape [avg_meals <= 2.36666666666667][zoom >= 9] {polygon-fill: #E1A18E;}
#FoodSecurity_Landscape [avg_meals <= 2.06666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#FoodSecurity_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#FoodSecurity_Landscape [avg_meals <= 3.125][zoom < 9] {marker-fill: #850200;}
#FoodSecurity_Landscape [avg_meals <= 2.9][zoom < 9] {marker-fill: #9C1C0C;}
#FoodSecurity_Landscape [avg_meals <= 2.7][zoom < 9] {marker-fill: #B43619;}
#FoodSecurity_Landscape [avg_meals <= 2.53333333333333][zoom < 9] {marker-fill: #CB6244;}
#FoodSecurity_Landscape [avg_meals <= 2.36666666666667][zoom < 9] {marker-fill: #E1A18E;}
#FoodSecurity_Landscape [avg_meals <= 2.06666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.avg_meals::numeric, 4) AS "avg_meals" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE avg_meals <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"1.8", "max-value":"2.1"},
{"color": "#E1A18E", "max-value":"2.4"},
{"color": "#CB6244", "max-value":"2.5"},
{"color": "#B43619", "max-value":"2.7"},
{"color": "#9C1C0C", "max-value":"2.9"},
{"color": "#850200", "max-value":"3.1"}]}', '100', 0, 'f'),
('Any Anthropometric Failutre', 'percent_composite_index_anthropometric_failure', 'layer', 'f', '{"description":"Percentage Of Children With Any Anthropometric Failure",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 100][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 77.7777777777778][zoom >= 9] {polygon-fill: #9C1C0C;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 64.2857142857143][zoom >= 9] {polygon-fill: #B43619;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 48.8888888888889][zoom >= 9] {polygon-fill: #CB6244;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 42.8571428571429][zoom >= 9] {polygon-fill: #E1A18E;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 35.7142857142857][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 100][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 77.7777777777778][zoom < 9] {marker-fill: #9C1C0C;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 64.2857142857143][zoom < 9] {marker-fill: #B43619;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 48.8888888888889][zoom < 9] {marker-fill: #CB6244;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 42.8571428571429][zoom < 9] {marker-fill: #E1A18E;}
#Nutrition_Landscape [percent_composite_index_anthropometric_failure <= 35.7142857142857][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_composite_index_anthropometric_failure::numeric, 4) AS "percent_composite_index_anthropometric_failure" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_composite_index_anthropometric_failure <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"22%", "max-value":"36%"},
{"color": "#E1A18E", "max-value":"43%"},
{"color": "#CB6244", "max-value":"49%"},
{"color": "#B43619", "max-value":"64%"},
{"color": "#9C1C0C", "max-value":"78%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Overweight', 'percent_overweight', 'layer', 'f', '{"description":"Percentage Of Children Overweight",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_overweight <= 29.1666666666667][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_overweight <= 15.7894736842105][zoom >= 9] {polygon-fill: #A22310;}
#Nutrition_Landscape [percent_overweight <= 10.7142857142857][zoom >= 9] {polygon-fill: #C04420;}
#Nutrition_Landscape [percent_overweight <= 9.67741935483871][zoom >= 9] {polygon-fill: #DC917C;}
#Nutrition_Landscape [percent_overweight <= 4.34782608695652][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_overweight <= 29.1666666666667][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_overweight <= 15.7894736842105][zoom < 9] {marker-fill: #A22310;}
#Nutrition_Landscape [percent_overweight <= 10.7142857142857][zoom < 9] {marker-fill: #C04420;}
#Nutrition_Landscape [percent_overweight <= 9.67741935483871][zoom < 9] {marker-fill: #DC917C;}
#Nutrition_Landscape [percent_overweight <= 4.34782608695652][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_overweight::numeric, 4) AS "percent_overweight" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_overweight <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"4.3%"},
{"color": "#DC917C", "max-value":"9.7%"},
{"color": "#C04420", "max-value":"11%"},
{"color": "#A22310", "max-value":"16%"},
{"color": "#850200", "max-value":"29%"}]}', '100', 0, 'f'),
('Severe Stunting', 'percent_severe_stunted', 'layer', 'f', '{"description":"Percentage Of Children Severely Stunted",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_severe_stunted <= 66.6666666666667][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_severe_stunted <= 40][zoom >= 9] {polygon-fill: #9C1C0C;}
#Nutrition_Landscape [percent_severe_stunted <= 33.3333333333333][zoom >= 9] {polygon-fill: #B43619;}
#Nutrition_Landscape [percent_severe_stunted <= 25][zoom >= 9] {polygon-fill: #CB6244;}
#Nutrition_Landscape [percent_severe_stunted <= 16.6666666666667][zoom >= 9] {polygon-fill: #E1A18E;}
#Nutrition_Landscape [percent_severe_stunted <= 14.2857142857143][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_severe_stunted <= 66.6666666666667][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_severe_stunted <= 40][zoom < 9] {marker-fill: #9C1C0C;}
#Nutrition_Landscape [percent_severe_stunted <= 33.3333333333333][zoom < 9] {marker-fill: #B43619;}
#Nutrition_Landscape [percent_severe_stunted <= 25][zoom < 9] {marker-fill: #CB6244;}
#Nutrition_Landscape [percent_severe_stunted <= 16.6666666666667][zoom < 9] {marker-fill: #E1A18E;}
#Nutrition_Landscape [percent_severe_stunted <= 14.2857142857143][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_severe_stunted::numeric, 4) AS "percent_severe_stunted" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_severe_stunted <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"7.1%", "max-value":"14%"},
{"color": "#E1A18E", "max-value":"17%"},
{"color": "#CB6244", "max-value":"25%"},
{"color": "#B43619", "max-value":"33%"},
{"color": "#9C1C0C", "max-value":"40%"},
{"color": "#850200", "max-value":"67%"}]}', '100', 0, 'f'),
('Severe Underweight', 'percent_severe_underweight', 'layer', 'f', '{"description":"Percentage Of Children Severely Underwieght",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_severe_underweight <= 44.4444444444444][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_severe_underweight <= 19.3548387096774][zoom >= 9] {polygon-fill: #AC2E15;}
#Nutrition_Landscape [percent_severe_underweight <= 11.1111111111111][zoom >= 9] {polygon-fill: #D2775D;}
#Nutrition_Landscape [percent_severe_underweight <= 4.44444444444444][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_severe_underweight <= 44.4444444444444][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_severe_underweight <= 19.3548387096774][zoom < 9] {marker-fill: #AC2E15;}
#Nutrition_Landscape [percent_severe_underweight <= 11.1111111111111][zoom < 9] {marker-fill: #D2775D;}
#Nutrition_Landscape [percent_severe_underweight <= 4.44444444444444][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_severe_underweight::numeric, 4) AS "percent_severe_underweight" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_severe_underweight <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"4.4%"},
{"color": "#D2775D", "max-value":"11%"},
{"color": "#AC2E15", "max-value":"19%"},
{"color": "#850200", "max-value":"44%"}]}', '100', 0, 'f'),
('Severe Wasting', 'percent_server_wasting', 'layer', 'f', '{"description":"Percentage Of Children Severely Wasting",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_server_wasting <= 25][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_server_wasting <= 11.1111111111111][zoom >= 9] {polygon-fill: #C04420;}
#Nutrition_Landscape [percent_server_wasting <= 4.76190476190476][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_server_wasting <= 25][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_server_wasting <= 11.1111111111111][zoom < 9] {marker-fill: #C04420;}
#Nutrition_Landscape [percent_server_wasting <= 4.76190476190476][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_server_wasting::numeric, 4) AS "percent_server_wasting" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_server_wasting <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"4.8%"},
{"color": "#C04420", "max-value":"11%"},
{"color": "#850200", "max-value":"25%"}]}', '100', 0, 'f'),
('Stunting', 'percent_stunted', 'layer', 'f', '{"description":"Percentage Of Children Stunted",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_stunted <= 100][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_stunted <= 57.1428571428571][zoom >= 9] {polygon-fill: #9C1C0C;}
#Nutrition_Landscape [percent_stunted <= 48.8888888888889][zoom >= 9] {polygon-fill: #B43619;}
#Nutrition_Landscape [percent_stunted <= 40][zoom >= 9] {polygon-fill: #CB6244;}
#Nutrition_Landscape [percent_stunted <= 31.5789473684211][zoom >= 9] {polygon-fill: #E1A18E;}
#Nutrition_Landscape [percent_stunted <= 22.2222222222222][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_stunted <= 100][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_stunted <= 57.1428571428571][zoom < 9] {marker-fill: #9C1C0C;}
#Nutrition_Landscape [percent_stunted <= 48.8888888888889][zoom < 9] {marker-fill: #B43619;}
#Nutrition_Landscape [percent_stunted <= 40][zoom < 9] {marker-fill: #CB6244;}
#Nutrition_Landscape [percent_stunted <= 31.5789473684211][zoom < 9] {marker-fill: #E1A18E;}
#Nutrition_Landscape [percent_stunted <= 22.2222222222222][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_stunted::numeric, 4) AS "percent_stunted" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_stunted <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"12%", "max-value":"22%"},
{"color": "#E1A18E", "max-value":"32%"},
{"color": "#CB6244", "max-value":"40%"},
{"color": "#B43619", "max-value":"49%"},
{"color": "#9C1C0C", "max-value":"57%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Underweight', 'percent_underweight', 'layer', 'f', '{"description":"Percentage Of Children Underweight",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_underweight <= 70][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_underweight <= 48.3870967741936][zoom >= 9] {polygon-fill: #9C1C0C;}
#Nutrition_Landscape [percent_underweight <= 26.3157894736842][zoom >= 9] {polygon-fill: #B43619;}
#Nutrition_Landscape [percent_underweight <= 21.7391304347826][zoom >= 9] {polygon-fill: #CB6244;}
#Nutrition_Landscape [percent_underweight <= 13.3333333333333][zoom >= 9] {polygon-fill: #E1A18E;}
#Nutrition_Landscape [percent_underweight <= 7.14285714285714][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_underweight <= 70][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_underweight <= 48.3870967741936][zoom < 9] {marker-fill: #9C1C0C;}
#Nutrition_Landscape [percent_underweight <= 26.3157894736842][zoom < 9] {marker-fill: #B43619;}
#Nutrition_Landscape [percent_underweight <= 21.7391304347826][zoom < 9] {marker-fill: #CB6244;}
#Nutrition_Landscape [percent_underweight <= 13.3333333333333][zoom < 9] {marker-fill: #E1A18E;}
#Nutrition_Landscape [percent_underweight <= 7.14285714285714][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_underweight::numeric, 4) AS "percent_underweight" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_underweight <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"7.1%"},
{"color": "#E1A18E", "max-value":"13%"},
{"color": "#CB6244", "max-value":"22%"},
{"color": "#B43619", "max-value":"26%"},
{"color": "#9C1C0C", "max-value":"48%"},
{"color": "#850200", "max-value":"70%"}]}', '100', 0, 'f'),
('Wasting', 'percent_wasting', 'layer', 'f', '{"description":"Percentage Of Children Wasting",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Nutrition_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Nutrition_Landscape [percent_wasting <= 40][zoom >= 9] {polygon-fill: #850200;}
#Nutrition_Landscape [percent_wasting <= 25][zoom >= 9] {polygon-fill: #A22310;}
#Nutrition_Landscape [percent_wasting <= 17.9487179487179][zoom >= 9] {polygon-fill: #C04420;}
#Nutrition_Landscape [percent_wasting <= 10.5263157894737][zoom >= 9] {polygon-fill: #DC917C;}
#Nutrition_Landscape [percent_wasting <= 3.7037037037037][zoom >= 9] {polygon-fill: #F8DFD8;}


#Nutrition_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Nutrition_Landscape [percent_wasting <= 40][zoom < 9] {marker-fill: #850200;}
#Nutrition_Landscape [percent_wasting <= 25][zoom < 9] {marker-fill: #A22310;}
#Nutrition_Landscape [percent_wasting <= 17.9487179487179][zoom < 9] {marker-fill: #C04420;}
#Nutrition_Landscape [percent_wasting <= 10.5263157894737][zoom < 9] {marker-fill: #DC917C;}
#Nutrition_Landscape [percent_wasting <= 3.7037037037037][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_wasting::numeric, 4) AS "percent_wasting" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_wasting <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"3.7%"},
{"color": "#DC917C", "max-value":"11%"},
{"color": "#C04420", "max-value":"18%"},
{"color": "#A22310", "max-value":"25%"},
{"color": "#850200", "max-value":"40%"}]}', '100', 0, 'f'),
('Business Operation', 'male_percent_operated_business', 'layer', 'f', '{"description":"Percentage Of Men Who Operate A Business",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_percent_operated_business <= 0.0666666666666667][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_percent_operated_business <= 0.0192307692307692][zoom >= 9] {polygon-fill: #C04420;}
#Gender_Landscape [male_percent_operated_business <= 0.0136363636363636][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_percent_operated_business <= 0.0666666666666667][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_percent_operated_business <= 0.0192307692307692][zoom < 9] {marker-fill: #C04420;}
#Gender_Landscape [male_percent_operated_business <= 0.0136363636363636][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_percent_operated_business::numeric, 4) AS "male_percent_operated_business" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_percent_operated_business <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"1.4%"},
{"color": "#C04420", "max-value":"1.9%"},
{"color": "#850200", "max-value":"6.7%"}]}', '100', 0, 'f'),
('Firewood Collection', 'male_percent_collects_firewood', 'layer', 'f', '{"description":"Percentage Of Men Who Collect Firewood",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_percent_collects_firewood <= 0.623399014778325][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_percent_collects_firewood <= 0.471111111111111][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_percent_collects_firewood <= 0.420727040816327][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_percent_collects_firewood <= 0.383888888888889][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_percent_collects_firewood <= 0.344827586206897][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_percent_collects_firewood <= 0.293690476190476][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_percent_collects_firewood <= 0.623399014778325][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_percent_collects_firewood <= 0.471111111111111][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_percent_collects_firewood <= 0.420727040816327][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_percent_collects_firewood <= 0.383888888888889][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_percent_collects_firewood <= 0.344827586206897][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_percent_collects_firewood <= 0.293690476190476][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_percent_collects_firewood::numeric, 4) AS "male_percent_collects_firewood" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_percent_collects_firewood <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"11%", "max-value":"29%"},
{"color": "#E1A18E", "max-value":"34%"},
{"color": "#CB6244", "max-value":"38%"},
{"color": "#B43619", "max-value":"42%"},
{"color": "#9C1C0C", "max-value":"47%"},
{"color": "#850200", "max-value":"62%"}]}', '100', 0, 'f'),
('Firewood Collection Hours', 'male_mean_hours_spent', 'layer', 'f', '{"description":"Mean Hours Spent Collecting Fuelwood By Men",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_mean_hours_spent <= 3.48015873015873][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_mean_hours_spent <= 2.86666666666667][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_mean_hours_spent <= 2.53846153846154][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_mean_hours_spent <= 2.34][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_mean_hours_spent <= 2][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_mean_hours_spent <= 1.88333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_mean_hours_spent <= 3.48015873015873][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_mean_hours_spent <= 2.86666666666667][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_mean_hours_spent <= 2.53846153846154][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_mean_hours_spent <= 2.34][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_mean_hours_spent <= 2][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_mean_hours_spent <= 1.88333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_mean_hours_spent::numeric, 4) AS "male_mean_hours_spent" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_mean_hours_spent <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"1.6", "max-value":"1.9"},
{"color": "#E1A18E", "max-value":"2"},
{"color": "#CB6244", "max-value":"2.3"},
{"color": "#B43619", "max-value":"2.5"},
{"color": "#9C1C0C", "max-value":"2.9"},
{"color": "#850200", "max-value":"3.5"}]}', '100', 0, 'f'),
('Headed Households', 'male_hh_head', 'layer', 'f', '{"description":"Percent Of Households Headed By Males",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_hh_head <= 1][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_hh_head <= 0.931034482758621][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_hh_head <= 0.896551724137931][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_hh_head <= 0.833333333333333][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_hh_head <= 0.8][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_hh_head <= 0.7][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_hh_head <= 1][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_hh_head <= 0.931034482758621][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_hh_head <= 0.896551724137931][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_hh_head <= 0.833333333333333][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_hh_head <= 0.8][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_hh_head <= 0.7][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_hh_head::numeric, 4) AS "male_hh_head" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_hh_head <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"67%", "max-value":"70%"},
{"color": "#E1A18E", "max-value":"80%"},
{"color": "#CB6244", "max-value":"83%"},
{"color": "#B43619", "max-value":"90%"},
{"color": "#9C1C0C", "max-value":"93%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Hours Worked Last Week', 'male_mean_hours_worked_last_week', 'layer', 'f', '{"description":"Mean Hours Working In The Past Week By Men",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 50.4285714285714][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 34.5833333333333][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 30.2142857142857][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 26.375][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 25.2727272727273][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 19][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 50.4285714285714][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 34.5833333333333][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 30.2142857142857][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 26.375][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 25.2727272727273][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_mean_hours_worked_last_week <= 19][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_mean_hours_worked_last_week::numeric, 4) AS "male_mean_hours_worked_last_week" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_mean_hours_worked_last_week <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"14", "max-value":"19"},
{"color": "#E1A18E", "max-value":"25"},
{"color": "#CB6244", "max-value":"26"},
{"color": "#B43619", "max-value":"30"},
{"color": "#9C1C0C", "max-value":"35"},
{"color": "#850200", "max-value":"50"}]}', '100', 0, 'f'),
('Literacy Rate', 'male_literacy_rate', 'layer', 'f', '{"description":"Percentage Of Men Who Can Read In Any Language",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_literacy_rate <= 0.885057471264368][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_literacy_rate <= 0.69347158218126][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_literacy_rate <= 0.609444444444444][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_literacy_rate <= 0.565151515151515][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_literacy_rate <= 0.517816091954023][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_literacy_rate <= 0.467936507936508][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_literacy_rate <= 0.885057471264368][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_literacy_rate <= 0.69347158218126][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_literacy_rate <= 0.609444444444444][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_literacy_rate <= 0.565151515151515][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_literacy_rate <= 0.517816091954023][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_literacy_rate <= 0.467936507936508][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_literacy_rate::numeric, 4) AS "male_literacy_rate" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_literacy_rate <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"35%", "max-value":"47%"},
{"color": "#E1A18E", "max-value":"52%"},
{"color": "#CB6244", "max-value":"57%"},
{"color": "#B43619", "max-value":"61%"},
{"color": "#9C1C0C", "max-value":"69%"},
{"color": "#850200", "max-value":"89%"}]}', '100', 0, 'f'),
('Recent Work', 'male_percent_worked_recently', 'layer', 'f', '{"description":"Percentage Of Men Who Have Worked In Past Week",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_percent_worked_recently <= 1][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_percent_worked_recently <= 0.982142857142857][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_percent_worked_recently <= 0.946540880503145][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_percent_worked_recently <= 0.852166224580018][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_percent_worked_recently <= 0.679688418577307][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_percent_worked_recently <= 0.358888888888889][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_percent_worked_recently <= 1][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_percent_worked_recently <= 0.982142857142857][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_percent_worked_recently <= 0.946540880503145][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_percent_worked_recently <= 0.852166224580018][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_percent_worked_recently <= 0.679688418577307][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_percent_worked_recently <= 0.358888888888889][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_percent_worked_recently::numeric, 4) AS "male_percent_worked_recently" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_percent_worked_recently <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"3.2%", "max-value":"36%"},
{"color": "#E1A18E", "max-value":"68%"},
{"color": "#CB6244", "max-value":"85%"},
{"color": "#B43619", "max-value":"95%"},
{"color": "#9C1C0C", "max-value":"98%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('School Attendance', 'male_percent_attended_school', 'layer', 'f', '{"description":"Percentage Of Men Who Have Attended School",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [male_percent_attended_school <= 0.936538461538462][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [male_percent_attended_school <= 0.836587301587302][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [male_percent_attended_school <= 0.780612244897959][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [male_percent_attended_school <= 0.733888888888889][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [male_percent_attended_school <= 0.69][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [male_percent_attended_school <= 0.666666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [male_percent_attended_school <= 0.936538461538462][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [male_percent_attended_school <= 0.836587301587302][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [male_percent_attended_school <= 0.780612244897959][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [male_percent_attended_school <= 0.733888888888889][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [male_percent_attended_school <= 0.69][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [male_percent_attended_school <= 0.666666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.male_percent_attended_school::numeric, 4) AS "male_percent_attended_school" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE male_percent_attended_school <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"49%", "max-value":"67%"},
{"color": "#E1A18E", "max-value":"69%"},
{"color": "#CB6244", "max-value":"73%"},
{"color": "#B43619", "max-value":"78%"},
{"color": "#9C1C0C", "max-value":"84%"},
{"color": "#850200", "max-value":"94%"}]}', '100', 0, 'f'),
('Business Operation', 'female_percent_operated_business', 'layer', 'f', '{"description":"Percentage Of Women Who Operate A Business",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_percent_operated_business <= 0.0333333333333333][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_percent_operated_business <= 0.0114942528735632][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_percent_operated_business <= 0.0333333333333333][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_percent_operated_business <= 0.0114942528735632][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_percent_operated_business::numeric, 4) AS "female_percent_operated_business" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_percent_operated_business <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"1.1%"},
{"color": "#850200", "max-value":"3.3%"}]}', '100', 0, 'f'),
('Firewood Collection', 'female_percent_collects_firewood', 'layer', 'f', '{"description":"Percentage Of Women Who Collect Firewood",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_percent_collects_firewood <= 0.772988505747126][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_percent_collects_firewood <= 0.699404761904762][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_percent_collects_firewood <= 0.677601410934744][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_percent_collects_firewood <= 0.661594202898551][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_percent_collects_firewood <= 0.620987654320988][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_percent_collects_firewood <= 0.596666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_percent_collects_firewood <= 0.772988505747126][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_percent_collects_firewood <= 0.699404761904762][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_percent_collects_firewood <= 0.677601410934744][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_percent_collects_firewood <= 0.661594202898551][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_percent_collects_firewood <= 0.620987654320988][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_percent_collects_firewood <= 0.596666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_percent_collects_firewood::numeric, 4) AS "female_percent_collects_firewood" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_percent_collects_firewood <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"47%", "max-value":"60%"},
{"color": "#E1A18E", "max-value":"62%"},
{"color": "#CB6244", "max-value":"66%"},
{"color": "#B43619", "max-value":"68%"},
{"color": "#9C1C0C", "max-value":"70%"},
{"color": "#850200", "max-value":"77%"}]}', '100', 0, 'f'),
('Firewood Collection Hours', 'female_mean_hours_spent', 'layer', 'f', '{"description":"Mean Hours Spent Collecting Fuelwood By Women",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_mean_hours_spent <= 3.92][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_mean_hours_spent <= 2.92873563218391][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_mean_hours_spent <= 2.45192307692308][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_mean_hours_spent <= 2.30769230769231][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_mean_hours_spent <= 2.08333333333333][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_mean_hours_spent <= 1.95238095238095][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_mean_hours_spent <= 3.92][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_mean_hours_spent <= 2.92873563218391][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_mean_hours_spent <= 2.45192307692308][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_mean_hours_spent <= 2.30769230769231][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_mean_hours_spent <= 2.08333333333333][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_mean_hours_spent <= 1.95238095238095][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_mean_hours_spent::numeric, 4) AS "female_mean_hours_spent" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_mean_hours_spent <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"1.8", "max-value":"2"},
{"color": "#E1A18E", "max-value":"2.1"},
{"color": "#CB6244", "max-value":"2.3"},
{"color": "#B43619", "max-value":"2.5"},
{"color": "#9C1C0C", "max-value":"2.9"},
{"color": "#850200", "max-value":"3.9"}]}', '100', 0, 'f'),
('Headed Households', 'female_hh_head', 'layer', 'f', '{"description":"Percent Of Households Headed By Females",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_hh_head <= 0.333333333333333][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_hh_head <= 0.3][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_hh_head <= 0.2][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_hh_head <= 0.166666666666667][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_hh_head <= 0.103448275862069][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_hh_head <= 0.0689655172413793][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_hh_head <= 0.333333333333333][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_hh_head <= 0.3][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_hh_head <= 0.2][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_hh_head <= 0.166666666666667][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_hh_head <= 0.103448275862069][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_hh_head <= 0.0689655172413793][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_hh_head::numeric, 4) AS "female_hh_head" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_hh_head <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"6.9%"},
{"color": "#E1A18E", "max-value":"10%"},
{"color": "#CB6244", "max-value":"17%"},
{"color": "#B43619", "max-value":"20%"},
{"color": "#9C1C0C", "max-value":"30%"},
{"color": "#850200", "max-value":"33%"}]}', '100', 0, 'f'),
('Hours Worked Last Week', 'female_mean_hours_worked_last_week', 'layer', 'f', '{"description":"Mean Hours Working In The Past Week By Women",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 44.3333333333333][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 29.8666666666666][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 24][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 20.2579365079365][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 17.5][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 10.8666666666666][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 44.3333333333333][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 29.8666666666666][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 24][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 20.2579365079365][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 17.5][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_mean_hours_worked_last_week <= 10.8666666666666][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_mean_hours_worked_last_week::numeric, 4) AS "female_mean_hours_worked_last_week" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_mean_hours_worked_last_week <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"4.4", "max-value":"11"},
{"color": "#E1A18E", "max-value":"18"},
{"color": "#CB6244", "max-value":"20"},
{"color": "#B43619", "max-value":"24"},
{"color": "#9C1C0C", "max-value":"30"},
{"color": "#850200", "max-value":"44"}]}', '100', 0, 'f'),
('Literacy Rate', 'female_literacy_rate', 'layer', 'f', '{"description":"Percentage Of Women Who Can Read In Any Language",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_literacy_rate <= 0.758620689655172][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_literacy_rate <= 0.607600732600733][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_literacy_rate <= 0.533729686880698][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_literacy_rate <= 0.450972222222222][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_literacy_rate <= 0.428571428571429][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_literacy_rate <= 0.241093474426808][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_literacy_rate <= 0.758620689655172][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_literacy_rate <= 0.607600732600733][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_literacy_rate <= 0.533729686880698][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_literacy_rate <= 0.450972222222222][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_literacy_rate <= 0.428571428571429][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_literacy_rate <= 0.241093474426808][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_literacy_rate::numeric, 4) AS "female_literacy_rate" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_literacy_rate <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"10%", "max-value":"24%"},
{"color": "#E1A18E", "max-value":"43%"},
{"color": "#CB6244", "max-value":"45%"},
{"color": "#B43619", "max-value":"53%"},
{"color": "#9C1C0C", "max-value":"61%"},
{"color": "#850200", "max-value":"76%"}]}', '100', 0, 'f'),
('Recent Work', 'female_percent_worked_recently', 'layer', 'f', '{"description":"Percentage Of Women Who Have Worked In Past Week",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_percent_worked_recently <= 1][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_percent_worked_recently <= 0.983333333333333][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_percent_worked_recently <= 0.935431799838579][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_percent_worked_recently <= 0.802777777777778][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_percent_worked_recently <= 0.644345238095238][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_percent_worked_recently <= 0.243333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_percent_worked_recently <= 1][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_percent_worked_recently <= 0.983333333333333][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_percent_worked_recently <= 0.935431799838579][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_percent_worked_recently <= 0.802777777777778][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_percent_worked_recently <= 0.644345238095238][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_percent_worked_recently <= 0.243333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_percent_worked_recently::numeric, 4) AS "female_percent_worked_recently" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_percent_worked_recently <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"24%"},
{"color": "#E1A18E", "max-value":"64%"},
{"color": "#CB6244", "max-value":"80%"},
{"color": "#B43619", "max-value":"94%"},
{"color": "#9C1C0C", "max-value":"98%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('School Attendance', 'female_percent_attended_school', 'layer', 'f', '{"description":"Percentage Of Women Who Have Attended School",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Gender_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Gender_Landscape [female_percent_attended_school <= 0.849294532627866][zoom >= 9] {polygon-fill: #850200;}
#Gender_Landscape [female_percent_attended_school <= 0.754920634920635][zoom >= 9] {polygon-fill: #9C1C0C;}
#Gender_Landscape [female_percent_attended_school <= 0.71468253968254][zoom >= 9] {polygon-fill: #B43619;}
#Gender_Landscape [female_percent_attended_school <= 0.696666666666667][zoom >= 9] {polygon-fill: #CB6244;}
#Gender_Landscape [female_percent_attended_school <= 0.635476190476191][zoom >= 9] {polygon-fill: #E1A18E;}
#Gender_Landscape [female_percent_attended_school <= 0.543033509700176][zoom >= 9] {polygon-fill: #F8DFD8;}


#Gender_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Gender_Landscape [female_percent_attended_school <= 0.849294532627866][zoom < 9] {marker-fill: #850200;}
#Gender_Landscape [female_percent_attended_school <= 0.754920634920635][zoom < 9] {marker-fill: #9C1C0C;}
#Gender_Landscape [female_percent_attended_school <= 0.71468253968254][zoom < 9] {marker-fill: #B43619;}
#Gender_Landscape [female_percent_attended_school <= 0.696666666666667][zoom < 9] {marker-fill: #CB6244;}
#Gender_Landscape [female_percent_attended_school <= 0.635476190476191][zoom < 9] {marker-fill: #E1A18E;}
#Gender_Landscape [female_percent_attended_school <= 0.543033509700176][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.female_percent_attended_school::numeric, 4) AS "female_percent_attended_school" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE female_percent_attended_school <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"36%", "max-value":"54%"},
{"color": "#E1A18E", "max-value":"64%"},
{"color": "#CB6244", "max-value":"70%"},
{"color": "#B43619", "max-value":"71%"},
{"color": "#9C1C0C", "max-value":"75%"},
{"color": "#850200", "max-value":"85%"}]}', '100', 0, 'f'),
('Agriculural Areas', 'ag_total', 'layer', 'f', '{"description":"Percentage Of Land Area With Agricultural Cover",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#LandCover_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#LandCover_Landscape [ag_total <= 97.8219697][zoom >= 9] {polygon-fill: #850200;}
#LandCover_Landscape [ag_total <= 91.6481041566667][zoom >= 9] {polygon-fill: #9C1C0C;}
#LandCover_Landscape [ag_total <= 75.0585393][zoom >= 9] {polygon-fill: #B43619;}
#LandCover_Landscape [ag_total <= 68.417119965][zoom >= 9] {polygon-fill: #CB6244;}
#LandCover_Landscape [ag_total <= 56.2035731133333][zoom >= 9] {polygon-fill: #E1A18E;}
#LandCover_Landscape [ag_total <= 41.8415629133333][zoom >= 9] {polygon-fill: #F8DFD8;}


#LandCover_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#LandCover_Landscape [ag_total <= 97.8219697][zoom < 9] {marker-fill: #850200;}
#LandCover_Landscape [ag_total <= 91.6481041566667][zoom < 9] {marker-fill: #9C1C0C;}
#LandCover_Landscape [ag_total <= 75.0585393][zoom < 9] {marker-fill: #B43619;}
#LandCover_Landscape [ag_total <= 68.417119965][zoom < 9] {marker-fill: #CB6244;}
#LandCover_Landscape [ag_total <= 56.2035731133333][zoom < 9] {marker-fill: #E1A18E;}
#LandCover_Landscape [ag_total <= 41.8415629133333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.ag_total::numeric, 4) AS "ag_total" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE ag_total <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"11%", "max-value":"42%"},
{"color": "#E1A18E", "max-value":"56%"},
{"color": "#CB6244", "max-value":"68%"},
{"color": "#B43619", "max-value":"75%"},
{"color": "#9C1C0C", "max-value":"92%"},
{"color": "#850200", "max-value":"98%"}]}', '100', 0, 'f'),
('Natural Areas', 'nr_total', 'layer', 'f', '{"description":"Percentage Of Land Area With Natural Cover (Trees, Shrubs, Savanna)",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#LandCover_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#LandCover_Landscape [nr_total <= 89.23512748][zoom >= 9] {polygon-fill: #055D00;}
#LandCover_Landscape [nr_total <= 58.1584370866667][zoom >= 9] {polygon-fill: #2B7414;}
#LandCover_Landscape [nr_total <= 43.7964268866667][zoom >= 9] {polygon-fill: #528C28;}
#LandCover_Landscape [nr_total <= 30.552527115][zoom >= 9] {polygon-fill: #80AB54;}
#LandCover_Landscape [nr_total <= 24.47935349][zoom >= 9] {polygon-fill: #B3CF99;}
#LandCover_Landscape [nr_total <= 8.351895843][zoom >= 9] {polygon-fill: #E8F4DD;}


#LandCover_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#LandCover_Landscape [nr_total <= 89.23512748][zoom < 9] {marker-fill: #055D00;}
#LandCover_Landscape [nr_total <= 58.1584370866667][zoom < 9] {marker-fill: #2B7414;}
#LandCover_Landscape [nr_total <= 43.7964268866667][zoom < 9] {marker-fill: #528C28;}
#LandCover_Landscape [nr_total <= 30.552527115][zoom < 9] {marker-fill: #80AB54;}
#LandCover_Landscape [nr_total <= 24.47935349][zoom < 9] {marker-fill: #B3CF99;}
#LandCover_Landscape [nr_total <= 8.351895843][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.nr_total::numeric, 4) AS "nr_total" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE nr_total <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"2.2%", "max-value":"8.4%"},
{"color": "#B3CF99", "max-value":"24%"},
{"color": "#80AB54", "max-value":"31%"},
{"color": "#528C28", "max-value":"44%"},
{"color": "#2B7414", "max-value":"58%"},
{"color": "#055D00", "max-value":"89%"}]}', '100', 0, 'f'),
('Decreasing Fuelwood Availability', 'fuelwood_decreasing', 'layer', 'f', '{"description":"Percentage Of Households Reporting Decreasing Fuelwood Availability",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [fuelwood_decreasing <= 1][zoom >= 9] {polygon-fill: #850200;}
#NaturalResources_Landscape [fuelwood_decreasing <= 0.941176470588235][zoom >= 9] {polygon-fill: #AC2E15;}
#NaturalResources_Landscape [fuelwood_decreasing <= 0.852287581699347][zoom >= 9] {polygon-fill: #D2775D;}
#NaturalResources_Landscape [fuelwood_decreasing <= 0.694444444444445][zoom >= 9] {polygon-fill: #F8DFD8;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [fuelwood_decreasing <= 1][zoom < 9] {marker-fill: #850200;}
#NaturalResources_Landscape [fuelwood_decreasing <= 0.941176470588235][zoom < 9] {marker-fill: #AC2E15;}
#NaturalResources_Landscape [fuelwood_decreasing <= 0.852287581699347][zoom < 9] {marker-fill: #D2775D;}
#NaturalResources_Landscape [fuelwood_decreasing <= 0.694444444444445][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.fuelwood_decreasing::numeric, 4) AS "fuelwood_decreasing" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE fuelwood_decreasing <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"69%"},
{"color": "#D2775D", "max-value":"85%"},
{"color": "#AC2E15", "max-value":"94%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Fuelwood From Natural Areas', 'fuelwood_from_natural_areas', 'layer', 'f', '{"description":"Percentage Of Households Collecting Fuelwood From Natural Areas",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 1][zoom >= 9] {polygon-fill: #055D00;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.933333333333333][zoom >= 9] {polygon-fill: #2B7414;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.566666666666667][zoom >= 9] {polygon-fill: #528C28;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.5][zoom >= 9] {polygon-fill: #80AB54;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.266666666666667][zoom >= 9] {polygon-fill: #B3CF99;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.0333333333333333][zoom >= 9] {polygon-fill: #E8F4DD;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 1][zoom < 9] {marker-fill: #055D00;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.933333333333333][zoom < 9] {marker-fill: #2B7414;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.566666666666667][zoom < 9] {marker-fill: #528C28;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.5][zoom < 9] {marker-fill: #80AB54;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.266666666666667][zoom < 9] {marker-fill: #B3CF99;}
#NaturalResources_Landscape [fuelwood_from_natural_areas <= 0.0333333333333333][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.fuelwood_from_natural_areas::numeric, 4) AS "fuelwood_from_natural_areas" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE fuelwood_from_natural_areas <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"3.3%"},
{"color": "#B3CF99", "max-value":"27%"},
{"color": "#80AB54", "max-value":"50%"},
{"color": "#528C28", "max-value":"57%"},
{"color": "#2B7414", "max-value":"93%"},
{"color": "#055D00", "max-value":"100%"}]}', '100', 0, 'f'),
('Fuelwood Shortage', 'fuelwood_shortage_past_year', 'layer', 'f', '{"description":"Percentage Of Households Reporting Fuelwood Shortages In Past Year",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 1][zoom >= 9] {polygon-fill: #850200;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.678921568627451][zoom >= 9] {polygon-fill: #A22310;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.411111111111111][zoom >= 9] {polygon-fill: #C04420;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.333333333333333][zoom >= 9] {polygon-fill: #DC917C;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.178333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 1][zoom < 9] {marker-fill: #850200;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.678921568627451][zoom < 9] {marker-fill: #A22310;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.411111111111111][zoom < 9] {marker-fill: #C04420;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.333333333333333][zoom < 9] {marker-fill: #DC917C;}
#NaturalResources_Landscape [fuelwood_shortage_past_year <= 0.178333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.fuelwood_shortage_past_year::numeric, 4) AS "fuelwood_shortage_past_year" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE fuelwood_shortage_past_year <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"18%"},
{"color": "#DC917C", "max-value":"33%"},
{"color": "#C04420", "max-value":"41%"},
{"color": "#A22310", "max-value":"68%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Fuelwood Value', 'mean_annual_fuelwood_value', 'layer', 'f', '{"description":"Mean Household Annual Value Of Fuelwood Collected",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 591.191469633751][zoom >= 9] {polygon-fill: #055D00;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 493.277700509968][zoom >= 9] {polygon-fill: #2B7414;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 195.994277539342][zoom >= 9] {polygon-fill: #528C28;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 184.500024665779][zoom >= 9] {polygon-fill: #80AB54;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 164.593487063844][zoom >= 9] {polygon-fill: #B3CF99;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 128.287282301811][zoom >= 9] {polygon-fill: #E8F4DD;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 591.191469633751][zoom < 9] {marker-fill: #055D00;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 493.277700509968][zoom < 9] {marker-fill: #2B7414;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 195.994277539342][zoom < 9] {marker-fill: #528C28;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 184.500024665779][zoom < 9] {marker-fill: #80AB54;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 164.593487063844][zoom < 9] {marker-fill: #B3CF99;}
#NaturalResources_Landscape [mean_annual_fuelwood_value <= 128.287282301811][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.mean_annual_fuelwood_value::numeric, 4) AS "mean_annual_fuelwood_value" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE mean_annual_fuelwood_value <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"80", "max-value":"130"},
{"color": "#B3CF99", "max-value":"160"},
{"color": "#80AB54", "max-value":"180"},
{"color": "#528C28", "max-value":"200"},
{"color": "#2B7414", "max-value":"490"},
{"color": "#055D00", "max-value":"590"}]}', '100', 0, 'f'),
('Non-Fuelwood Natural Resource Value', 'nonfuel_nr_annual_value', 'layer', 'f', '{"description":"Mean Household Annual Value Of Non-Fuelwood Gathered Resources",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 213.421008614201][zoom >= 9] {polygon-fill: #055D00;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 47.376267143709][zoom >= 9] {polygon-fill: #2B7414;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 25.7895224849328][zoom >= 9] {polygon-fill: #528C28;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 10.494931425164][zoom >= 9] {polygon-fill: #80AB54;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 2.77280858676208][zoom >= 9] {polygon-fill: #B3CF99;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 0.775193798449612][zoom >= 9] {polygon-fill: #E8F4DD;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 213.421008614201][zoom < 9] {marker-fill: #055D00;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 47.376267143709][zoom < 9] {marker-fill: #2B7414;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 25.7895224849328][zoom < 9] {marker-fill: #528C28;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 10.494931425164][zoom < 9] {marker-fill: #80AB54;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 2.77280858676208][zoom < 9] {marker-fill: #B3CF99;}
#NaturalResources_Landscape [nonfuel_nr_annual_value <= 0.775193798449612][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.nonfuel_nr_annual_value::numeric, 4) AS "nonfuel_nr_annual_value" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE nonfuel_nr_annual_value <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0", "max-value":"0.78"},
{"color": "#B3CF99", "max-value":"2.8"},
{"color": "#80AB54", "max-value":"10"},
{"color": "#528C28", "max-value":"26"},
{"color": "#2B7414", "max-value":"47"},
{"color": "#055D00", "max-value":"210"}]}', '100', 0, 'f'),
('Non-Fuelwood Natural Resources Decreasing', 'nonfuel_nr_decreasing', 'layer', 'f', '{"description":"Percentage Of Households Reporting Decreasing  Non-Fuelwood Gathered Resource Availability",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 1][zoom >= 9] {polygon-fill: #850200;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.9][zoom >= 9] {polygon-fill: #A22310;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.859273182957393][zoom >= 9] {polygon-fill: #C04420;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.846153846153846][zoom >= 9] {polygon-fill: #DC917C;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.585336538461539][zoom >= 9] {polygon-fill: #F8DFD8;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 1][zoom < 9] {marker-fill: #850200;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.9][zoom < 9] {marker-fill: #A22310;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.859273182957393][zoom < 9] {marker-fill: #C04420;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.846153846153846][zoom < 9] {marker-fill: #DC917C;}
#NaturalResources_Landscape [nonfuel_nr_decreasing <= 0.585336538461539][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.nonfuel_nr_decreasing::numeric, 4) AS "nonfuel_nr_decreasing" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE nonfuel_nr_decreasing <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"43%", "max-value":"59%"},
{"color": "#DC917C", "max-value":"85%"},
{"color": "#C04420", "max-value":"86%"},
{"color": "#A22310", "max-value":"90%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Percent Collecting Non-Fuelwood Natural Resources', 'collects_nonfuel_resources', 'layer', 'f', '{"description":"Percentage Of Households Collecting Non-Fuelwood Gathered Resources",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#NaturalResources_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.9][zoom >= 9] {polygon-fill: #055D00;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.785714285714286][zoom >= 9] {polygon-fill: #2B7414;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.655172413793103][zoom >= 9] {polygon-fill: #528C28;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.433333333333333][zoom >= 9] {polygon-fill: #80AB54;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.3][zoom >= 9] {polygon-fill: #B3CF99;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.0344827586206897][zoom >= 9] {polygon-fill: #E8F4DD;}


#NaturalResources_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.9][zoom < 9] {marker-fill: #055D00;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.785714285714286][zoom < 9] {marker-fill: #2B7414;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.655172413793103][zoom < 9] {marker-fill: #528C28;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.433333333333333][zoom < 9] {marker-fill: #80AB54;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.3][zoom < 9] {marker-fill: #B3CF99;}
#NaturalResources_Landscape [collects_nonfuel_resources <= 0.0344827586206897][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.collects_nonfuel_resources::numeric, 4) AS "collects_nonfuel_resources" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE collects_nonfuel_resources <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"3.4%"},
{"color": "#B3CF99", "max-value":"30%"},
{"color": "#80AB54", "max-value":"43%"},
{"color": "#528C28", "max-value":"66%"},
{"color": "#2B7414", "max-value":"79%"},
{"color": "#055D00", "max-value":"90%"}]}', '100', 0, 'f'),
('Age', 'age', 'layer', 'f', '{"description":"Average Age",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Capital_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Capital_Landscape [age <= 34.0354132457581][zoom >= 9] {polygon-fill: #850200;}
#Capital_Landscape [age <= 29.8013023088023][zoom >= 9] {polygon-fill: #9C1C0C;}
#Capital_Landscape [age <= 26.9832972582973][zoom >= 9] {polygon-fill: #B43619;}
#Capital_Landscape [age <= 25.2013888888889][zoom >= 9] {polygon-fill: #CB6244;}
#Capital_Landscape [age <= 24.4151316738817][zoom >= 9] {polygon-fill: #E1A18E;}
#Capital_Landscape [age <= 22.4856221556222][zoom >= 9] {polygon-fill: #F8DFD8;}


#Capital_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Capital_Landscape [age <= 34.0354132457581][zoom < 9] {marker-fill: #850200;}
#Capital_Landscape [age <= 29.8013023088023][zoom < 9] {marker-fill: #9C1C0C;}
#Capital_Landscape [age <= 26.9832972582973][zoom < 9] {marker-fill: #B43619;}
#Capital_Landscape [age <= 25.2013888888889][zoom < 9] {marker-fill: #CB6244;}
#Capital_Landscape [age <= 24.4151316738817][zoom < 9] {marker-fill: #E1A18E;}
#Capital_Landscape [age <= 22.4856221556222][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.age::numeric, 4) AS "age" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE age <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"20", "max-value":"22"},
{"color": "#E1A18E", "max-value":"24"},
{"color": "#CB6244", "max-value":"25"},
{"color": "#B43619", "max-value":"27"},
{"color": "#9C1C0C", "max-value":"30"},
{"color": "#850200", "max-value":"34"}]}', '100', 0, 'f'),
('Household Size', 'size', 'layer', 'f', '{"description":"Average Number Of Individuals Per Household",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Capital_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Capital_Landscape [size <= 7.89655172413793][zoom >= 9] {polygon-fill: #850200;}
#Capital_Landscape [size <= 6.58620689655172][zoom >= 9] {polygon-fill: #9C1C0C;}
#Capital_Landscape [size <= 5.75][zoom >= 9] {polygon-fill: #B43619;}
#Capital_Landscape [size <= 5.56666666666667][zoom >= 9] {polygon-fill: #CB6244;}
#Capital_Landscape [size <= 5.16666666666667][zoom >= 9] {polygon-fill: #E1A18E;}
#Capital_Landscape [size <= 4.86666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#Capital_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Capital_Landscape [size <= 7.89655172413793][zoom < 9] {marker-fill: #850200;}
#Capital_Landscape [size <= 6.58620689655172][zoom < 9] {marker-fill: #9C1C0C;}
#Capital_Landscape [size <= 5.75][zoom < 9] {marker-fill: #B43619;}
#Capital_Landscape [size <= 5.56666666666667][zoom < 9] {marker-fill: #CB6244;}
#Capital_Landscape [size <= 5.16666666666667][zoom < 9] {marker-fill: #E1A18E;}
#Capital_Landscape [size <= 4.86666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.size::numeric, 4) AS "size" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE size <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"3.6", "max-value":"4.9"},
{"color": "#E1A18E", "max-value":"5.2"},
{"color": "#CB6244", "max-value":"5.6"},
{"color": "#B43619", "max-value":"5.8"},
{"color": "#9C1C0C", "max-value":"6.6"},
{"color": "#850200", "max-value":"7.9"}]}', '100', 0, 'f'),
('Literacy Rate', 'literate', 'layer', 'f', '{"description":"Percentage Of Individuals Who Can Read In Any Language",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Capital_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Capital_Landscape [literate <= 0.764484126984127][zoom >= 9] {polygon-fill: #850200;}
#Capital_Landscape [literate <= 0.628703703703704][zoom >= 9] {polygon-fill: #9C1C0C;}
#Capital_Landscape [literate <= 0.556987734487735][zoom >= 9] {polygon-fill: #B43619;}
#Capital_Landscape [literate <= 0.531252633532045][zoom >= 9] {polygon-fill: #CB6244;}
#Capital_Landscape [literate <= 0.45025493025493][zoom >= 9] {polygon-fill: #E1A18E;}
#Capital_Landscape [literate <= 0.366441336441336][zoom >= 9] {polygon-fill: #F8DFD8;}


#Capital_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Capital_Landscape [literate <= 0.764484126984127][zoom < 9] {marker-fill: #850200;}
#Capital_Landscape [literate <= 0.628703703703704][zoom < 9] {marker-fill: #9C1C0C;}
#Capital_Landscape [literate <= 0.556987734487735][zoom < 9] {marker-fill: #B43619;}
#Capital_Landscape [literate <= 0.531252633532045][zoom < 9] {marker-fill: #CB6244;}
#Capital_Landscape [literate <= 0.45025493025493][zoom < 9] {marker-fill: #E1A18E;}
#Capital_Landscape [literate <= 0.366441336441336][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.literate::numeric, 4) AS "literate" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE literate <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0.21", "max-value":"0.37"},
{"color": "#E1A18E", "max-value":"0.45"},
{"color": "#CB6244", "max-value":"0.53"},
{"color": "#B43619", "max-value":"0.56"},
{"color": "#9C1C0C", "max-value":"0.63"},
{"color": "#850200", "max-value":"0.76"}]}', '100', 0, 'f'),
('Years Of Schooling', 'years', 'layer', 'f', '{"description":"Average Number Of Years Of Schooling Per Individual",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Capital_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Capital_Landscape [years <= 7.48575533661741][zoom >= 9] {polygon-fill: #850200;}
#Capital_Landscape [years <= 4.59634920634921][zoom >= 9] {polygon-fill: #9C1C0C;}
#Capital_Landscape [years <= 4.19550264550265][zoom >= 9] {polygon-fill: #B43619;}
#Capital_Landscape [years <= 4.05398148148148][zoom >= 9] {polygon-fill: #CB6244;}
#Capital_Landscape [years <= 3.33406084656085][zoom >= 9] {polygon-fill: #E1A18E;}
#Capital_Landscape [years <= 3.15502645502646][zoom >= 9] {polygon-fill: #F8DFD8;}


#Capital_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Capital_Landscape [years <= 7.48575533661741][zoom < 9] {marker-fill: #850200;}
#Capital_Landscape [years <= 4.59634920634921][zoom < 9] {marker-fill: #9C1C0C;}
#Capital_Landscape [years <= 4.19550264550265][zoom < 9] {marker-fill: #B43619;}
#Capital_Landscape [years <= 4.05398148148148][zoom < 9] {marker-fill: #CB6244;}
#Capital_Landscape [years <= 3.33406084656085][zoom < 9] {marker-fill: #E1A18E;}
#Capital_Landscape [years <= 3.15502645502646][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.years::numeric, 4) AS "years" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE years <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"2.4", "max-value":"3.2"},
{"color": "#E1A18E", "max-value":"3.3"},
{"color": "#CB6244", "max-value":"4.1"},
{"color": "#B43619", "max-value":"4.2"},
{"color": "#9C1C0C", "max-value":"4.6"},
{"color": "#850200", "max-value":"7.5"}]}', '100', 0, 'f'),
('Agricultural Production Inequality', 'production_inequality_gini', 'layer', 'f', '{"description":"Gini Coefficient Of Agricultural Production",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgValue_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgValue_Landscape [production_inequality_gini <= 0.945258531574131][zoom >= 9] {polygon-fill: #850200;}
#AgValue_Landscape [production_inequality_gini <= 0.568927493838897][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgValue_Landscape [production_inequality_gini <= 0.535638352143129][zoom >= 9] {polygon-fill: #B43619;}
#AgValue_Landscape [production_inequality_gini <= 0.500591069077103][zoom >= 9] {polygon-fill: #CB6244;}
#AgValue_Landscape [production_inequality_gini <= 0.474132292707344][zoom >= 9] {polygon-fill: #E1A18E;}
#AgValue_Landscape [production_inequality_gini <= 0.428402888416441][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgValue_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgValue_Landscape [production_inequality_gini <= 0.945258531574131][zoom < 9] {marker-fill: #850200;}
#AgValue_Landscape [production_inequality_gini <= 0.568927493838897][zoom < 9] {marker-fill: #9C1C0C;}
#AgValue_Landscape [production_inequality_gini <= 0.535638352143129][zoom < 9] {marker-fill: #B43619;}
#AgValue_Landscape [production_inequality_gini <= 0.500591069077103][zoom < 9] {marker-fill: #CB6244;}
#AgValue_Landscape [production_inequality_gini <= 0.474132292707344][zoom < 9] {marker-fill: #E1A18E;}
#AgValue_Landscape [production_inequality_gini <= 0.428402888416441][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.production_inequality_gini::numeric, 4) AS "production_inequality_gini" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE production_inequality_gini <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0.38", "max-value":"0.43"},
{"color": "#E1A18E", "max-value":"0.47"},
{"color": "#CB6244", "max-value":"0.5"},
{"color": "#B43619", "max-value":"0.54"},
{"color": "#9C1C0C", "max-value":"0.57"},
{"color": "#850200", "max-value":"0.95"}]}', '100', 0, 'f'),
('Income Inequality', 'income_inequality_gini', 'layer', 'f', '{"description":"Gini Coefficient Of Income Inequality",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Income_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Income_Landscape [income_inequality_gini <= 0.798565583559511][zoom >= 9] {polygon-fill: #850200;}
#Income_Landscape [income_inequality_gini <= 0.709267950281514][zoom >= 9] {polygon-fill: #9C1C0C;}
#Income_Landscape [income_inequality_gini <= 0.630525455283615][zoom >= 9] {polygon-fill: #B43619;}
#Income_Landscape [income_inequality_gini <= 0.573694071582352][zoom >= 9] {polygon-fill: #CB6244;}
#Income_Landscape [income_inequality_gini <= 0.530920450526752][zoom >= 9] {polygon-fill: #E1A18E;}
#Income_Landscape [income_inequality_gini <= 0.445316523708278][zoom >= 9] {polygon-fill: #F8DFD8;}


#Income_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Income_Landscape [income_inequality_gini <= 0.798565583559511][zoom < 9] {marker-fill: #850200;}
#Income_Landscape [income_inequality_gini <= 0.709267950281514][zoom < 9] {marker-fill: #9C1C0C;}
#Income_Landscape [income_inequality_gini <= 0.630525455283615][zoom < 9] {marker-fill: #B43619;}
#Income_Landscape [income_inequality_gini <= 0.573694071582352][zoom < 9] {marker-fill: #CB6244;}
#Income_Landscape [income_inequality_gini <= 0.530920450526752][zoom < 9] {marker-fill: #E1A18E;}
#Income_Landscape [income_inequality_gini <= 0.445316523708278][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.income_inequality_gini::numeric, 4) AS "income_inequality_gini" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE income_inequality_gini <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0.42", "max-value":"0.45"},
{"color": "#E1A18E", "max-value":"0.53"},
{"color": "#CB6244", "max-value":"0.57"},
{"color": "#B43619", "max-value":"0.63"},
{"color": "#9C1C0C", "max-value":"0.71"},
{"color": "#850200", "max-value":"0.8"}]}', '100', 0, 'f'),
('Land Area Inequality', 'area_owned_ineq_gini', 'layer', 'f', '{"description":"Gini Coefficient Of Area Owned",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#AgIntensification_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.784787061421009][zoom >= 9] {polygon-fill: #850200;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.595253718285214][zoom >= 9] {polygon-fill: #9C1C0C;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.452909917907014][zoom >= 9] {polygon-fill: #B43619;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.412679781782113][zoom >= 9] {polygon-fill: #CB6244;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.397405644788155][zoom >= 9] {polygon-fill: #E1A18E;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.364316620655371][zoom >= 9] {polygon-fill: #F8DFD8;}


#AgIntensification_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.784787061421009][zoom < 9] {marker-fill: #850200;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.595253718285214][zoom < 9] {marker-fill: #9C1C0C;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.452909917907014][zoom < 9] {marker-fill: #B43619;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.412679781782113][zoom < 9] {marker-fill: #CB6244;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.397405644788155][zoom < 9] {marker-fill: #E1A18E;}
#AgIntensification_Landscape [area_owned_ineq_gini <= 0.364316620655371][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.area_owned_ineq_gini::numeric, 4) AS "area_owned_ineq_gini" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE area_owned_ineq_gini <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0.34", "max-value":"0.36"},
{"color": "#E1A18E", "max-value":"0.4"},
{"color": "#CB6244", "max-value":"0.41"},
{"color": "#B43619", "max-value":"0.45"},
{"color": "#9C1C0C", "max-value":"0.6"},
{"color": "#850200", "max-value":"0.78"}]}', '100', 0, 'f'),
('Agricultural Costs', 'agcosts', 'layer', 'f', '{"description":"Mean Household Annual Cost Of Agriculture",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Income_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Income_Landscape [agcosts <= 183.747316773996][zoom >= 9] {polygon-fill: #055D00;}
#Income_Landscape [agcosts <= 72.523765716038][zoom >= 9] {polygon-fill: #2B7414;}
#Income_Landscape [agcosts <= 28.2132591562355][zoom >= 9] {polygon-fill: #528C28;}
#Income_Landscape [agcosts <= 19.7352644957076][zoom >= 9] {polygon-fill: #80AB54;}
#Income_Landscape [agcosts <= 9.90866944830784][zoom >= 9] {polygon-fill: #B3CF99;}
#Income_Landscape [agcosts <= 6.3953488372093][zoom >= 9] {polygon-fill: #E8F4DD;}


#Income_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Income_Landscape [agcosts <= 183.747316773996][zoom < 9] {marker-fill: #055D00;}
#Income_Landscape [agcosts <= 72.523765716038][zoom < 9] {marker-fill: #2B7414;}
#Income_Landscape [agcosts <= 28.2132591562355][zoom < 9] {marker-fill: #528C28;}
#Income_Landscape [agcosts <= 19.7352644957076][zoom < 9] {marker-fill: #80AB54;}
#Income_Landscape [agcosts <= 9.90866944830784][zoom < 9] {marker-fill: #B3CF99;}
#Income_Landscape [agcosts <= 6.3953488372093][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.agcosts::numeric, 4) AS "agcosts" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE agcosts <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"3", "max-value":"6.4"},
{"color": "#B3CF99", "max-value":"9.9"},
{"color": "#80AB54", "max-value":"20"},
{"color": "#528C28", "max-value":"28"},
{"color": "#2B7414", "max-value":"73"},
{"color": "#055D00", "max-value":"180"}]}', '100', 0, 'f'),
('Agricultural Income', 'agincome', 'layer', 'f', '{"description":"Mean Household Annual Income From Agriculture",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Income_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Income_Landscape [agincome <= 2414.34375958295][zoom >= 9] {polygon-fill: #055D00;}
#Income_Landscape [agincome <= 880.351119288562][zoom >= 9] {polygon-fill: #2B7414;}
#Income_Landscape [agincome <= 304.162194394753][zoom >= 9] {polygon-fill: #528C28;}
#Income_Landscape [agincome <= 218.935629099583][zoom >= 9] {polygon-fill: #80AB54;}
#Income_Landscape [agincome <= 176.244782349434][zoom >= 9] {polygon-fill: #B3CF99;}
#Income_Landscape [agincome <= 111.719122556032][zoom >= 9] {polygon-fill: #E8F4DD;}


#Income_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Income_Landscape [agincome <= 2414.34375958295][zoom < 9] {marker-fill: #055D00;}
#Income_Landscape [agincome <= 880.351119288562][zoom < 9] {marker-fill: #2B7414;}
#Income_Landscape [agincome <= 304.162194394753][zoom < 9] {marker-fill: #528C28;}
#Income_Landscape [agincome <= 218.935629099583][zoom < 9] {marker-fill: #80AB54;}
#Income_Landscape [agincome <= 176.244782349434][zoom < 9] {marker-fill: #B3CF99;}
#Income_Landscape [agincome <= 111.719122556032][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.agincome::numeric, 4) AS "agincome" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE agincome <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"43", "max-value":"110"},
{"color": "#B3CF99", "max-value":"180"},
{"color": "#80AB54", "max-value":"220"},
{"color": "#528C28", "max-value":"300"},
{"color": "#2B7414", "max-value":"880"},
{"color": "#055D00", "max-value":"2400"}]}', '100', 0, 'f'),
('Gross Income', 'totalincome', 'layer', 'f', '{"description":"Mean Household Annual Gross Income From All Sources",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Income_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Income_Landscape [totalincome <= 3108.54910380251][zoom >= 9] {polygon-fill: #055D00;}
#Income_Landscape [totalincome <= 1806.85332062201][zoom >= 9] {polygon-fill: #2B7414;}
#Income_Landscape [totalincome <= 1142.70955396598][zoom >= 9] {polygon-fill: #528C28;}
#Income_Landscape [totalincome <= 665.316820676866][zoom >= 9] {polygon-fill: #80AB54;}
#Income_Landscape [totalincome <= 522.99685888571][zoom >= 9] {polygon-fill: #B3CF99;}
#Income_Landscape [totalincome <= 475.868162492546][zoom >= 9] {polygon-fill: #E8F4DD;}


#Income_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Income_Landscape [totalincome <= 3108.54910380251][zoom < 9] {marker-fill: #055D00;}
#Income_Landscape [totalincome <= 1806.85332062201][zoom < 9] {marker-fill: #2B7414;}
#Income_Landscape [totalincome <= 1142.70955396598][zoom < 9] {marker-fill: #528C28;}
#Income_Landscape [totalincome <= 665.316820676866][zoom < 9] {marker-fill: #80AB54;}
#Income_Landscape [totalincome <= 522.99685888571][zoom < 9] {marker-fill: #B3CF99;}
#Income_Landscape [totalincome <= 475.868162492546][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.totalincome::numeric, 4) AS "totalincome" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE totalincome <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"360", "max-value":"480"},
{"color": "#B3CF99", "max-value":"520"},
{"color": "#80AB54", "max-value":"670"},
{"color": "#528C28", "max-value":"1100"},
{"color": "#2B7414", "max-value":"1800"},
{"color": "#055D00", "max-value":"3100"}]}', '100', 0, 'f'),
('Non-Agricultural Income', 'nonagincome', 'layer', 'f', '{"description":"Mean Household Annual Income From Non-Agricultural Sources",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#Income_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#Income_Landscape [nonagincome <= 2235.78385665276][zoom >= 9] {polygon-fill: #055D00;}
#Income_Landscape [nonagincome <= 986.388340780655][zoom >= 9] {polygon-fill: #2B7414;}
#Income_Landscape [nonagincome <= 741.577284327982][zoom >= 9] {polygon-fill: #528C28;}
#Income_Landscape [nonagincome <= 429.215391363689][zoom >= 9] {polygon-fill: #80AB54;}
#Income_Landscape [nonagincome <= 318.555677101968][zoom >= 9] {polygon-fill: #B3CF99;}
#Income_Landscape [nonagincome <= 296.004771914132][zoom >= 9] {polygon-fill: #E8F4DD;}


#Income_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#Income_Landscape [nonagincome <= 2235.78385665276][zoom < 9] {marker-fill: #055D00;}
#Income_Landscape [nonagincome <= 986.388340780655][zoom < 9] {marker-fill: #2B7414;}
#Income_Landscape [nonagincome <= 741.577284327982][zoom < 9] {marker-fill: #528C28;}
#Income_Landscape [nonagincome <= 429.215391363689][zoom < 9] {marker-fill: #80AB54;}
#Income_Landscape [nonagincome <= 318.555677101968][zoom < 9] {marker-fill: #B3CF99;}
#Income_Landscape [nonagincome <= 296.004771914132][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.nonagincome::numeric, 4) AS "nonagincome" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE nonagincome <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"59", "max-value":"300"},
{"color": "#B3CF99", "max-value":"320"},
{"color": "#80AB54", "max-value":"430"},
{"color": "#528C28", "max-value":"740"},
{"color": "#2B7414", "max-value":"990"},
{"color": "#055D00", "max-value":"2200"}]}', '100', 0, 'f'),
('Flush Toilet', 'flush_toilet', 'layer', 'f', '{"description":"Percentage Of Households With A Flush Toilet",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [flush_toilet = 0.1333][zoom >= 9] {polygon-fill: #000080;}
#WatSan_Landscape [flush_toilet = 0.0667][zoom >= 9] {polygon-fill: #4488D4;}
#WatSan_Landscape [flush_toilet = 0.0333][zoom >= 9] {polygon-fill: #92D6FF;}
#WatSan_Landscape [flush_toilet = 0][zoom >= 9] {polygon-fill: #EBEBFF;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [flush_toilet = 0.1333][zoom < 9] {marker-fill: #000080;}
#WatSan_Landscape [flush_toilet = 0.0667][zoom < 9] {marker-fill: #4488D4;}
#WatSan_Landscape [flush_toilet = 0.0333][zoom < 9] {marker-fill: #92D6FF;}
#WatSan_Landscape [flush_toilet = 0][zoom < 9] {marker-fill: #EBEBFF;}
', 1, $$SELECT l.the_geom_webmercator, round(c.flush_toilet::numeric, 4) AS "flush_toilet" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE flush_toilet <> 'NA'$$, now(), now(), 't', 't', '{"type":"custom","data":[{"value": "#EBEBFF", "name":"0%"},
{"value": "#92D6FF", "name": "3.33%"},
{"value": "#4488D4", "name": "6.67%"},
{"value": "#000080", "name": "13.3%"}]}', '100', 0, 'f'),
('Garbage Disposed Within Compound', 'dispose_garbage_within_compound', 'layer', 'f', '{"description":"Percentage Of Households That Dispose Of Garbage Within Their Compound",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [dispose_garbage_within_compound <= 1][zoom >= 9] {polygon-fill: #850200;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.8][zoom >= 9] {polygon-fill: #A22310;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.333333333333333][zoom >= 9] {polygon-fill: #C04420;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.1][zoom >= 9] {polygon-fill: #DC917C;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.0666666666666667][zoom >= 9] {polygon-fill: #F8DFD8;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [dispose_garbage_within_compound <= 1][zoom < 9] {marker-fill: #850200;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.8][zoom < 9] {marker-fill: #A22310;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.333333333333333][zoom < 9] {marker-fill: #C04420;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.1][zoom < 9] {marker-fill: #DC917C;}
#WatSan_Landscape [dispose_garbage_within_compound <= 0.0666666666666667][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.dispose_garbage_within_compound::numeric, 4) AS "dispose_garbage_within_compound" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE dispose_garbage_within_compound <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"6.7%"},
{"color": "#DC917C", "max-value":"10%"},
{"color": "#C04420", "max-value":"33%"},
{"color": "#A22310", "max-value":"80%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('No Measure Of Ensuring Safe Drinking Water', 'no_measure_safe_drinking_water', 'layer', 'f', '{"description":"Percentage Of Households That Take No Measure Of Ensuring Safe Drinking Water",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 1][zoom >= 9] {polygon-fill: #850200;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.933333333333333][zoom >= 9] {polygon-fill: #9C1C0C;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.833333333333333][zoom >= 9] {polygon-fill: #B43619;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.8][zoom >= 9] {polygon-fill: #CB6244;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.666666666666667][zoom >= 9] {polygon-fill: #E1A18E;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.5][zoom >= 9] {polygon-fill: #F8DFD8;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 1][zoom < 9] {marker-fill: #850200;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.933333333333333][zoom < 9] {marker-fill: #9C1C0C;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.833333333333333][zoom < 9] {marker-fill: #B43619;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.8][zoom < 9] {marker-fill: #CB6244;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.666666666666667][zoom < 9] {marker-fill: #E1A18E;}
#WatSan_Landscape [no_measure_safe_drinking_water <= 0.5][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.no_measure_safe_drinking_water::numeric, 4) AS "no_measure_safe_drinking_water" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE no_measure_safe_drinking_water <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"13%", "max-value":"50%"},
{"color": "#E1A18E", "max-value":"67%"},
{"color": "#CB6244", "max-value":"80%"},
{"color": "#B43619", "max-value":"83%"},
{"color": "#9C1C0C", "max-value":"93%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('No Toilet', 'no_toilet', 'layer', 'f', '{"description":"Percentage Of Households With No Toilet",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [no_toilet <= 0.7][zoom >= 9] {polygon-fill: #850200;}
#WatSan_Landscape [no_toilet <= 0.22][zoom >= 9] {polygon-fill: #AC2E15;}
#WatSan_Landscape [no_toilet <= 0.0666666666666667][zoom >= 9] {polygon-fill: #D2775D;}
#WatSan_Landscape [no_toilet <= 0.0333333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [no_toilet <= 0.7][zoom < 9] {marker-fill: #850200;}
#WatSan_Landscape [no_toilet <= 0.22][zoom < 9] {marker-fill: #AC2E15;}
#WatSan_Landscape [no_toilet <= 0.0666666666666667][zoom < 9] {marker-fill: #D2775D;}
#WatSan_Landscape [no_toilet <= 0.0333333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.no_toilet::numeric, 4) AS "no_toilet" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE no_toilet <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"3.3%"},
{"color": "#D2775D", "max-value":"6.7%"},
{"color": "#AC2E15", "max-value":"22%"},
{"color": "#850200", "max-value":"70%"}]}', '100', 0, 'f'),
('Pit Latrine', 'pit_latrine', 'layer', 'f', '{"description":"Percentage Of Households With A Pit Latrine",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [pit_latrine <= 1][zoom >= 9] {polygon-fill: #000080;}
#WatSan_Landscape [pit_latrine <= 0.966666666666667][zoom >= 9] {polygon-fill: #4488D4;}
#WatSan_Landscape [pit_latrine <= 0.9][zoom >= 9] {polygon-fill: #92D6FF;}
#WatSan_Landscape [pit_latrine <= 0.78][zoom >= 9] {polygon-fill: #EBEBFF;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [pit_latrine <= 1][zoom < 9] {marker-fill: #000080;}
#WatSan_Landscape [pit_latrine <= 0.966666666666667][zoom < 9] {marker-fill: #4488D4;}
#WatSan_Landscape [pit_latrine <= 0.9][zoom < 9] {marker-fill: #92D6FF;}
#WatSan_Landscape [pit_latrine <= 0.78][zoom < 9] {marker-fill: #EBEBFF;}
', 1, $$SELECT l.the_geom_webmercator, round(c.pit_latrine::numeric, 4) AS "pit_latrine" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE pit_latrine <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#EBEBFF", "min-value":"30%", "max-value":"78%"},
{"color": "#92D6FF", "max-value":"90%"},
{"color": "#4488D4", "max-value":"97%"},
{"color": "#000080", "max-value":"100%"}]}', '100', 0, 'f'),
('Satisfied With Water Quality', 'satisfied_drinking_water', 'layer', 'f', '{"description":"Percentage Of Households Satisfied With Their Drinking Water",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [satisfied_drinking_water <= 1][zoom >= 9] {polygon-fill: #000080;}
#WatSan_Landscape [satisfied_drinking_water <= 0.966666666666667][zoom >= 9] {polygon-fill: #3366BF;}
#WatSan_Landscape [satisfied_drinking_water <= 0.933333333333333][zoom >= 9] {polygon-fill: #66CCFF;}
#WatSan_Landscape [satisfied_drinking_water <= 0.866666666666667][zoom >= 9] {polygon-fill: #A8DBFF;}
#WatSan_Landscape [satisfied_drinking_water <= 0.733333333333333][zoom >= 9] {polygon-fill: #EBEBFF;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [satisfied_drinking_water <= 1][zoom < 9] {marker-fill: #000080;}
#WatSan_Landscape [satisfied_drinking_water <= 0.966666666666667][zoom < 9] {marker-fill: #3366BF;}
#WatSan_Landscape [satisfied_drinking_water <= 0.933333333333333][zoom < 9] {marker-fill: #66CCFF;}
#WatSan_Landscape [satisfied_drinking_water <= 0.866666666666667][zoom < 9] {marker-fill: #A8DBFF;}
#WatSan_Landscape [satisfied_drinking_water <= 0.733333333333333][zoom < 9] {marker-fill: #EBEBFF;}
', 1, $$SELECT l.the_geom_webmercator, round(c.satisfied_drinking_water::numeric, 4) AS "satisfied_drinking_water" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE satisfied_drinking_water <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#EBEBFF", "min-value":"57%", "max-value":"73%"},
{"color": "#A8DBFF", "max-value":"87%"},
{"color": "#66CCFF", "max-value":"93%"},
{"color": "#3366BF", "max-value":"97%"},
{"color": "#000080", "max-value":"100%"}]}', '100', 0, 'f'),
('Unsatisfied With Water Quality', 'unsatisfied_drinking_water', 'layer', 'f', '{"description":"Percentage Of Households Unsatisfied With Their Drinking Water",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#WatSan_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#WatSan_Landscape [unsatisfied_drinking_water <= 0.266666666666667][zoom >= 9] {polygon-fill: #850200;}
#WatSan_Landscape [unsatisfied_drinking_water <= 0.1][zoom >= 9] {polygon-fill: #C04420;}
#WatSan_Landscape [unsatisfied_drinking_water <= 0.0333333333333333][zoom >= 9] {polygon-fill: #F8DFD8;}


#WatSan_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#WatSan_Landscape [unsatisfied_drinking_water <= 0.266666666666667][zoom < 9] {marker-fill: #850200;}
#WatSan_Landscape [unsatisfied_drinking_water <= 0.1][zoom < 9] {marker-fill: #C04420;}
#WatSan_Landscape [unsatisfied_drinking_water <= 0.0333333333333333][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.unsatisfied_drinking_water::numeric, 4) AS "unsatisfied_drinking_water" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE unsatisfied_drinking_water <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"3.3%"},
{"color": "#C04420", "max-value":"10%"},
{"color": "#850200", "max-value":"27%"}]}', '100', 0, 'f'),
('Area Degraded', 'percent_degraded', 'layer', 'f', '{"description":"Percent Of Land Area Degraded Since 2003",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#LandCover_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#LandCover_Landscape [percent_degraded <= 100][zoom >= 9] {polygon-fill: #850200;}
#LandCover_Landscape [percent_degraded <= 99.53125][zoom >= 9] {polygon-fill: #9C1C0C;}
#LandCover_Landscape [percent_degraded <= 88.2291666666667][zoom >= 9] {polygon-fill: #B43619;}
#LandCover_Landscape [percent_degraded <= 43.28125][zoom >= 9] {polygon-fill: #CB6244;}
#LandCover_Landscape [percent_degraded <= 32.0208333333333][zoom >= 9] {polygon-fill: #E1A18E;}
#LandCover_Landscape [percent_degraded <= 15.1958841463415][zoom >= 9] {polygon-fill: #F8DFD8;}


#LandCover_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#LandCover_Landscape [percent_degraded <= 100][zoom < 9] {marker-fill: #850200;}
#LandCover_Landscape [percent_degraded <= 99.53125][zoom < 9] {marker-fill: #9C1C0C;}
#LandCover_Landscape [percent_degraded <= 88.2291666666667][zoom < 9] {marker-fill: #B43619;}
#LandCover_Landscape [percent_degraded <= 43.28125][zoom < 9] {marker-fill: #CB6244;}
#LandCover_Landscape [percent_degraded <= 32.0208333333333][zoom < 9] {marker-fill: #E1A18E;}
#LandCover_Landscape [percent_degraded <= 15.1958841463415][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_degraded::numeric, 4) AS "percent_degraded" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_degraded <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"2.7%", "max-value":"15%"},
{"color": "#E1A18E", "max-value":"32%"},
{"color": "#CB6244", "max-value":"43%"},
{"color": "#B43619", "max-value":"88%"},
{"color": "#9C1C0C", "max-value":"100%"},
{"color": "#850200", "max-value":"100%"}]}', '100', 0, 'f'),
('Area Improved', 'percent_improved', 'layer', 'f', '{"description":"Percent Of Land Area Improved Since 2003",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#LandCover_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#LandCover_Landscape [percent_improved <= 37.625][zoom >= 9] {polygon-fill: #055D00;}
#LandCover_Landscape [percent_improved <= 14.2606707317073][zoom >= 9] {polygon-fill: #357A19;}
#LandCover_Landscape [percent_improved <= 3.53739316239316][zoom >= 9] {polygon-fill: #669933;}
#LandCover_Landscape [percent_improved <= 1.1875][zoom >= 9] {polygon-fill: #A7C688;}
#LandCover_Landscape [percent_improved <= 0.458333333333333][zoom >= 9] {polygon-fill: #E8F4DD;}


#LandCover_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#LandCover_Landscape [percent_improved <= 37.625][zoom < 9] {marker-fill: #055D00;}
#LandCover_Landscape [percent_improved <= 14.2606707317073][zoom < 9] {marker-fill: #357A19;}
#LandCover_Landscape [percent_improved <= 3.53739316239316][zoom < 9] {marker-fill: #669933;}
#LandCover_Landscape [percent_improved <= 1.1875][zoom < 9] {marker-fill: #A7C688;}
#LandCover_Landscape [percent_improved <= 0.458333333333333][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.percent_improved::numeric, 4) AS "percent_improved" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE percent_improved <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"0.46%"},
{"color": "#A7C688", "max-value":"1.2%"},
{"color": "#669933", "max-value":"3.5%"},
{"color": "#357A19", "max-value":"14%"},
{"color": "#055D00", "max-value":"38%"}]}', '100', 0, 'f'),
('Bad Soil', 'bad_soil', 'layer', 'f', '{"description":"Percent Of Households Reporting Bad Soil",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#ErosionControl_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#ErosionControl_Landscape [bad_soil <= 0.361111111111111][zoom >= 9] {polygon-fill: #850200;}
#ErosionControl_Landscape [bad_soil <= 0.114619883040936][zoom >= 9] {polygon-fill: #A22310;}
#ErosionControl_Landscape [bad_soil <= 0.1][zoom >= 9] {polygon-fill: #C04420;}
#ErosionControl_Landscape [bad_soil <= 0.0694444444444444][zoom >= 9] {polygon-fill: #DC917C;}
#ErosionControl_Landscape [bad_soil <= 0.05][zoom >= 9] {polygon-fill: #F8DFD8;}


#ErosionControl_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#ErosionControl_Landscape [bad_soil <= 0.361111111111111][zoom < 9] {marker-fill: #850200;}
#ErosionControl_Landscape [bad_soil <= 0.114619883040936][zoom < 9] {marker-fill: #A22310;}
#ErosionControl_Landscape [bad_soil <= 0.1][zoom < 9] {marker-fill: #C04420;}
#ErosionControl_Landscape [bad_soil <= 0.0694444444444444][zoom < 9] {marker-fill: #DC917C;}
#ErosionControl_Landscape [bad_soil <= 0.05][zoom < 9] {marker-fill: #F8DFD8;}
', 1, $$SELECT l.the_geom_webmercator, round(c.bad_soil::numeric, 4) AS "bad_soil" 
                  FROM erosioncontrol_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE bad_soil <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#F8DFD8", "min-value":"0%", "max-value":"5%"},
{"color": "#DC917C", "max-value":"6.9%"},
{"color": "#C04420", "max-value":"10%"},
{"color": "#A22310", "max-value":"11%"},
{"color": "#850200", "max-value":"36%"}]}', '100', 0, 'f'),
('Erosion Control', 'erosion_control_household_percent', 'layer', 'f', '{"description":"Percent Of Households Taking Erosion Control Measures",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#ErosionControl_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#ErosionControl_Landscape [erosion_control_household_percent <= 1][zoom >= 9] {polygon-fill: #055D00;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.633333333333333][zoom >= 9] {polygon-fill: #2B7414;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.366666666666667][zoom >= 9] {polygon-fill: #528C28;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.233333333333333][zoom >= 9] {polygon-fill: #80AB54;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.103448275862069][zoom >= 9] {polygon-fill: #B3CF99;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.0333333333333333][zoom >= 9] {polygon-fill: #E8F4DD;}


#ErosionControl_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#ErosionControl_Landscape [erosion_control_household_percent <= 1][zoom < 9] {marker-fill: #055D00;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.633333333333333][zoom < 9] {marker-fill: #2B7414;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.366666666666667][zoom < 9] {marker-fill: #528C28;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.233333333333333][zoom < 9] {marker-fill: #80AB54;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.103448275862069][zoom < 9] {marker-fill: #B3CF99;}
#ErosionControl_Landscape [erosion_control_household_percent <= 0.0333333333333333][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.erosion_control_household_percent::numeric, 4) AS "erosion_control_household_percent" 
                  FROM erosioncontrol_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE erosion_control_household_percent <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"0%", "max-value":"3.3%"},
{"color": "#B3CF99", "max-value":"10%"},
{"color": "#80AB54", "max-value":"23%"},
{"color": "#528C28", "max-value":"37%"},
{"color": "#2B7414", "max-value":"63%"},
{"color": "#055D00", "max-value":"100%"}]}', '100', 0, 'f'),
('Good Soil', 'good_soil', 'layer', 'f', '{"description":"Percent Of Households Reporting Good Soil",
                 "source":"<p>Vital Signs</p>",
                 "link":"http://www.vitalsigns.org"}', 'cartodb', '#ErosionControl_Landscape[zoom >= 9]{polygon-opacity: 0.8; line-color: #666666; line-width: 0.5; line-opacity: 1;}
#ErosionControl_Landscape [good_soil <= 0.988888888888889][zoom >= 9] {polygon-fill: #055D00;}
#ErosionControl_Landscape [good_soil <= 0.764761904761905][zoom >= 9] {polygon-fill: #2B7414;}
#ErosionControl_Landscape [good_soil <= 0.537426900584795][zoom >= 9] {polygon-fill: #528C28;}
#ErosionControl_Landscape [good_soil <= 0.464367816091954][zoom >= 9] {polygon-fill: #80AB54;}
#ErosionControl_Landscape [good_soil <= 0.422222222222222][zoom >= 9] {polygon-fill: #B3CF99;}
#ErosionControl_Landscape [good_soil <= 0.396551724137931][zoom >= 9] {polygon-fill: #E8F4DD;}


#ErosionControl_Landscape[zoom < 9]{marker-opacity: 0.8; marker-line-color: #666666; marker-line-width: 0.5; marker-line-opacity: 1;}
#ErosionControl_Landscape [good_soil <= 0.988888888888889][zoom < 9] {marker-fill: #055D00;}
#ErosionControl_Landscape [good_soil <= 0.764761904761905][zoom < 9] {marker-fill: #2B7414;}
#ErosionControl_Landscape [good_soil <= 0.537426900584795][zoom < 9] {marker-fill: #528C28;}
#ErosionControl_Landscape [good_soil <= 0.464367816091954][zoom < 9] {marker-fill: #80AB54;}
#ErosionControl_Landscape [good_soil <= 0.422222222222222][zoom < 9] {marker-fill: #B3CF99;}
#ErosionControl_Landscape [good_soil <= 0.396551724137931][zoom < 9] {marker-fill: #E8F4DD;}
', 1, $$SELECT l.the_geom_webmercator, round(c.good_soil::numeric, 4) AS "good_soil" 
                  FROM erosioncontrol_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no
                  WHERE good_soil <> 'NA'$$, now(), now(), 't', 't', '{"type":"choropleth","bucket":[ {"color": "#E8F4DD", "min-value":"6.7%", "max-value":"40%"},
{"color": "#B3CF99", "max-value":"42%"},
{"color": "#80AB54", "max-value":"46%"},
{"color": "#528C28", "max-value":"54%"},
{"color": "#2B7414", "max-value":"76%"},
{"color": "#055D00", "max-value":"99%"}]}', '100', 0, 'f')