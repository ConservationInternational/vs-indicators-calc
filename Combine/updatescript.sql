UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.bananas::numeric, 4) AS "bananas" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1008;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.beans::numeric, 4) AS "beans" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1009;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.bulrush_millet::numeric, 4) AS "bulrush_millet" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1010;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.cashew_nut::numeric, 4) AS "cashew_nut" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1011;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.cassava::numeric, 4) AS "cassava" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1012;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.chilies::numeric, 4) AS "chilies" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1013;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.cocoa::numeric, 4) AS "cocoa" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1014;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.coconut::numeric, 4) AS "coconut" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1015;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.coffee::numeric, 4) AS "coffee" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1016;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.cotton::numeric, 4) AS "cotton" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1017;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.egg_plant::numeric, 4) AS "egg_plant" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1018;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.field_peas::numeric, 4) AS "field_peas" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1019;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.finger_millet::numeric, 4) AS "finger_millet" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1020;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.groundnut::numeric, 4) AS "groundnut" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1021;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.irish_potatoes::numeric, 4) AS "irish_potatoes" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1022;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.maize::numeric, 4) AS "maize" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1023;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.okra::numeric, 4) AS "okra" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1024;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.onions::numeric, 4) AS "onions" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1025;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.orange::numeric, 4) AS "orange" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1026;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.paddy::numeric, 4) AS "paddy" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1027;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.palm_oil::numeric, 4) AS "palm_oil" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1028;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pigeon_pea::numeric, 4) AS "pigeon_pea" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1029;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pineapple::numeric, 4) AS "pineapple" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1030;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pyrethrum::numeric, 4) AS "pyrethrum" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1031;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.rubber::numeric, 4) AS "rubber" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1032;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.sesame_simsim::numeric, 4) AS "sesame_simsim" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1033;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.sorghum::numeric, 4) AS "sorghum" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1034;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.soyabeans::numeric, 4) AS "soyabeans" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1035;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.sugar_cane::numeric, 4) AS "sugar_cane" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1036;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.sunflower::numeric, 4) AS "sunflower" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1037;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.sweet_potatoes::numeric, 4) AS "sweet_potatoes" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1038;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.tea::numeric, 4) AS "tea" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1039;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.timber::numeric, 4) AS "timber" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1040;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.tomatoes::numeric, 4) AS "tomatoes" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1041;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.wheat::numeric, 4) AS "wheat" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1042;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.yams::numeric, 4) AS "yams" 
                  FROM crops_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1043;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_hh_area_farmed::numeric, 4) AS "mean_hh_area_farmed" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1044;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_area_owned::numeric, 4) AS "mean_area_owned" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1045;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.avg_pct_crops_any_sold::numeric, 4) AS "avg_pct_crops_any_sold" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1046;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.median_field_size::numeric, 4) AS "median_field_size" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1047;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.fungicide_rate::numeric, 4) AS "fungicide_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1048;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.herbicide_rate::numeric, 4) AS "herbicide_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1049;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pct_fields_inorg_fert::numeric, 4) AS "pct_fields_inorg_fert" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1050;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.intercrop_rate::numeric, 4) AS "intercrop_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1051;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pct_fields_irrigated::numeric, 4) AS "pct_fields_irrigated" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1052;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_hh_number_fields::numeric, 4) AS "mean_hh_number_fields" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1053;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pesticide_rate::numeric, 4) AS "pesticide_rate" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1054;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pct_fields_purchased_seed::numeric, 4) AS "pct_fields_purchased_seed" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1055;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.annualcropproduction::numeric, 4) AS "annualcropproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1056;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.cropbyproductproduction::numeric, 4) AS "cropbyproductproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1057;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.livestockproduction::numeric, 4) AS "livestockproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1058;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.livestockbyproductproduction::numeric, 4) AS "livestockbyproductproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1059;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.permanentcropproduction::numeric, 4) AS "permanentcropproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1060;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.totalagriculturalproduction::numeric, 4) AS "totalagriculturalproduction" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1061;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_yield_quantile::numeric, 4) AS "mean_yield_quantile" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1062;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_diet_diversity::numeric, 4) AS "mean_diet_diversity" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1063;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.food_as_percent_total_spending::numeric, 4) AS "food_as_percent_total_spending" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1064;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_food_consumption_value::numeric, 4) AS "mean_food_consumption_value" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1065;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_shortage_past_year::numeric, 4) AS "percent_shortage_past_year" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1066;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_food_spending::numeric, 4) AS "mean_food_spending" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1067;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_nonfood_spending::numeric, 4) AS "mean_nonfood_spending" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1068;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.avg_meals::numeric, 4) AS "avg_meals" 
                  FROM foodsecurity_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1069;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_composite_index_anthropometric_failure::numeric, 4) AS "percent_composite_index_anthropometric_failure" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1070;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_overweight::numeric, 4) AS "percent_overweight" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1071;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_severe_stunted::numeric, 4) AS "percent_severe_stunted" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1072;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_severe_underweight::numeric, 4) AS "percent_severe_underweight" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1073;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_server_wasting::numeric, 4) AS "percent_server_wasting" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1074;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_stunted::numeric, 4) AS "percent_stunted" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1075;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_underweight::numeric, 4) AS "percent_underweight" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1076;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_wasting::numeric, 4) AS "percent_wasting" 
                  FROM nutrition_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1077;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_percent_operated_business::numeric, 4) AS "male_percent_operated_business" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1078;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_percent_collects_firewood::numeric, 4) AS "male_percent_collects_firewood" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1079;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_mean_hours_spent::numeric, 4) AS "male_mean_hours_spent" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1080;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_hh_head::numeric, 4) AS "male_hh_head" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1081;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_mean_hours_worked_last_week::numeric, 4) AS "male_mean_hours_worked_last_week" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1082;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_literacy_rate::numeric, 4) AS "male_literacy_rate" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1083;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_percent_worked_recently::numeric, 4) AS "male_percent_worked_recently" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1084;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.male_percent_attended_school::numeric, 4) AS "male_percent_attended_school" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1085;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_percent_operated_business::numeric, 4) AS "female_percent_operated_business" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1086;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_percent_collects_firewood::numeric, 4) AS "female_percent_collects_firewood" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1087;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_mean_hours_spent::numeric, 4) AS "female_mean_hours_spent" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1088;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_hh_head::numeric, 4) AS "female_hh_head" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1089;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_mean_hours_worked_last_week::numeric, 4) AS "female_mean_hours_worked_last_week" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_noWHERE female_mean_hours_worked_last_week <> 'NA'$$
WHERE id = 1090;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_literacy_rate::numeric, 4) AS "female_literacy_rate" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1091;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_percent_worked_recently::numeric, 4) AS "female_percent_worked_recently" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1092;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.female_percent_attended_school::numeric, 4) AS "female_percent_attended_school" 
                  FROM gender_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1093;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.ag_total::numeric, 4) AS "ag_total" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1094;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.nr_total::numeric, 4) AS "nr_total" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1095;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.fuelwood_decreasing::numeric, 4) AS "fuelwood_decreasing" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_noWHERE fuelwood_decreasing <> 'NA'$$
WHERE id = 1096;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.fuelwood_from_natural_areas::numeric, 4) AS "fuelwood_from_natural_areas" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1097;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.fuelwood_shortage_past_year::numeric, 4) AS "fuelwood_shortage_past_year" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_noWHERE fuelwood_shortage_past_year <> 'NA'$$
WHERE id = 1098;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.mean_annual_fuelwood_value::numeric, 4) AS "mean_annual_fuelwood_value" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1099;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.nonfuel_nr_annual_value::numeric, 4) AS "nonfuel_nr_annual_value" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1100;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.nonfuel_nr_decreasing::numeric, 4) AS "nonfuel_nr_decreasing" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_noWHERE nonfuel_nr_decreasing <> 'NA'$$
WHERE id = 1101;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.collects_nonfuel_resources::numeric, 4) AS "collects_nonfuel_resources" 
                  FROM naturalresources_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1102;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.age::numeric, 4) AS "age" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1103;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.size::numeric, 4) AS "size" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1104;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.literate::numeric, 4) AS "literate" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1105;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.years::numeric, 4) AS "years" 
                  FROM capital_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1106;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.production_inequality_gini::numeric, 4) AS "production_inequality_gini" 
                  FROM agvalue_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1107;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.income_inequality_gini::numeric, 4) AS "income_inequality_gini" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1108;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.area_owned_ineq_gini::numeric, 4) AS "area_owned_ineq_gini" 
                  FROM agintensification_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1109;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.agcosts::numeric, 4) AS "agcosts" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1110;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.agincome::numeric, 4) AS "agincome" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1111;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.totalincome::numeric, 4) AS "totalincome" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1112;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.nonagincome::numeric, 4) AS "nonagincome" 
                  FROM income_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1113;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.flush_toilet::numeric, 4) AS "flush_toilet" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1114;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.dispose_garbage_within_compound::numeric, 4) AS "dispose_garbage_within_compound" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1115;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.no_measure_safe_drinking_water::numeric, 4) AS "no_measure_safe_drinking_water" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1116;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.no_toilet::numeric, 4) AS "no_toilet" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1117;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.pit_latrine::numeric, 4) AS "pit_latrine" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1118;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.satisfied_drinking_water::numeric, 4) AS "satisfied_drinking_water" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1119;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.unsatisfied_drinking_water::numeric, 4) AS "unsatisfied_drinking_water" 
                  FROM watsan_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1120;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_degraded::numeric, 4) AS "percent_degraded" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1121;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.percent_improved::numeric, 4) AS "percent_improved" 
                  FROM landcover_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1122;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.bad_soil::numeric, 4) AS "bad_soil" 
                  FROM erosioncontrol_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1123;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.erosion_control_household_percent::numeric, 4) AS "erosion_control_household_percent" 
                  FROM erosioncontrol_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1124;

UPDATE layers
SET query = $$SELECT l.the_geom_webmercator, round(c.good_soil::numeric, 4) AS "good_soil" 
                  FROM erosioncontrol_landscape c JOIN vs_landscape_march_2017 l 
                  ON c.country = l.country AND c.landscape=l.landscape_no$$
WHERE id = 1125;

