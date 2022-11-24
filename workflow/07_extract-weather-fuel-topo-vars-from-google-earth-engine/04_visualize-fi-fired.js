var fired_daily = ee.FeatureCollection("users/mkoontz/megafired-california/fired_daily_ca_epsg3310_2003-2020"),
    fi_fired_tcf_01 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_01"),
    fi_fired_tcf_02 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_02"),
    fi_fired_tcf_03 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_03"),
    fi_fired_tcf_04 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_04"),
    fi_fired_tcf_05 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_05"),
    era5 = ee.ImageCollection("ECMWF/ERA5_LAND/HOURLY"),
    fired_events = ee.FeatureCollection("users/mkoontz/megafired-california/fired_events_ca_ewe_rank"),
    landforms_10m_ned = ee.Image("CSP/ERGo/1_0/US/landforms"),
    friction = ee.Image("Oxford/MAP/friction_surface_2019"),
    lcms = ee.ImageCollection("USFS/GTAC/LCMS/v2020-5"),
    gridmet_drought = ee.ImageCollection("GRIDMET/DROUGHT"),
    gridmet = ee.ImageCollection("IDAHO_EPSCOR/GRIDMET"),
    grip4_north_america = ee.FeatureCollection("projects/sat-io/open-datasets/GRIP4/North-America"),
    dem10_3dep = ee.Image("USGS/3DEP/10m");
    
// Following standard derivations
// Handy R package with function code included below
// See Jones (1992): Jones, H.G. 1992. Plants and microclimate: a quantitative approach to environmental plant physiology. 2nd Edition., 2nd Edn. Cambridge University Press, Cambridge. 428 p
// https://remkoduursma.github.io/plantecophys/reference/Conversions.html

// DewtoVPD() function
// function (Tdew, TdegC, Pa = 101) 
// {
//     e <- esat(Tdew, Pa)
//     esatval <- esat(TdegC)
//     return((esatval - e)/1000)
// }

// esat() function
// function (TdegC, Pa = 101) 
// {
//     a <- 611.21
//     b <- 17.502
//     c <- 240.97
//     f <- 1.0007 + 3.46 * 10^-8 * Pa * 1000
//     esatval <- f * a * (exp(b * TdegC/(c + TdegC)))
//     return(esatval)
// }

var add_vpd_to_era5 = function(img) {

  // Saturation vapor pressure (how much water can the air hold at this temperature?)
  // In units of Pa (divide by 100 to get hPa)
  var esat = img.expression("(1.0007 + (3.46 * 1e-7 * pressure_Pa)) * a * (exp(b * Tair_C / (c + Tair_C)))", 
                {'pressure_Pa': img.select('surface_pressure'), // surface pressure in Pa
                 'Tair_C': img.select('temperature_2m').subtract(273.15), // air temperature in degrees Celsius
                 'a': 611.21, // constant
                 'b': 17.502, // constant
                 'c': 240.97 // constant
                }).rename('esat');

  // Actual vapor pressure (how much water is actually in the air?)
  // In units of Pa (divide by 100 to get hPa)  
  var ea = img.expression("(1.0007 + (3.46 * 1e-7 * pressure_Pa)) * a * (exp(b * Tdew_C / (c + Tdew_C)))", 
                {'pressure_Pa': img.select('surface_pressure'), // surface pressure in Pa
                 'Tdew_C': img.select('dewpoint_temperature_2m').subtract(273.15), // dew temperature in degrees Celsius
                 'a': 611.21, // constant
                 'b': 17.502, // constant
                 'c': 240.97 // constant
                }).rename('ea');
  
  // vapor pressure deficit is the saturation vapor pressure minus the actual vapor pressure
  // vpd = esat - ea
  // divide by 100 to get units of hPa instead of Pa
  var vpd_val = esat.subtract(ea).divide(100).rename('vpd_hPa');
  var rh = ea.divide(esat).rename('rh');
  var with_vpd = img.addBands(vpd_val).addBands(esat).addBands(ea).addBands(rh);

  return(with_vpd);
};

// To check your software, compute atan2(1,-1). 
// If it equals 2.36 radians (135 degrees) then your software uses the 
// programming language convention and you can use these formulas unchanged. 
// If it equals -0.79 radians (-45 degrees) then your software follows the 
// spreadsheet convention and you must switch the arguments of atan2 in the following equations.
// var test = ee.Image(1).atan2(ee.Image(-1));
// Map.addLayer(test); ]
// Result is -0.785, so Earth Engine uses the Spreadsheet version

var add_wind_vars_to_era5 = function(img) {
  
  // https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398
  
  // Pull out separate components of wind so we can more easily work with them
  var u_wind = img.select('u_component_of_wind_10m');
  var v_wind = img.select('u_component_of_wind_10m');
  
  // abs(wind_speed) = sqrt(u^2 + v^2)
  var wind_speed = u_wind.pow(2).add(v_wind.pow(2)).sqrt().rename('wind_speed');

  // https://www.eol.ucar.edu/content/wind-direction-quick-reference
  // direction the wind is coming FROM
  // wind_dir = atan2(-Ugeo,-Vgeo) for programming language convention
  // wind_dir = atan2(-Vgeo,-Ugeo) for spreadsheet convention
  
  // https://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r
  // Dir=np.mod(180+np.rad2deg(np.arctan2(U, V)),360) for programming language convention
  // Dir=np.mod(180+np.rad2deg(np.arctan2(V, U)),360) for spreadsheet convention
  
  var deg_to_rad = ee.Image(Math.PI).divide(ee.Image(180));
  var rad_to_deg = ee.Image(180).divide(ee.Image(Math.PI));
  
  var wind_direction_rad = ee.Image(Math.PI).add(v_wind.atan2(u_wind)).mod(ee.Image(2).multiply(ee.Image(Math.PI))).rename('wind_dir_rad');
  var wind_direction_deg = ee.Image(180).add(v_wind.atan2(u_wind).multiply(rad_to_deg)).mod(ee.Image(360)).rename('wind_dir_deg');

  var aspect = ee.Terrain.aspect(dem10_3dep).rename('aspect');
  
  var wind_aspect_alignment_deg = wind_direction_deg.subtract(aspect).rename('wind_aspect_alignment_deg');
  var wind_aspect_alignment_rad = wind_aspect_alignment_deg.multiply(deg_to_rad).rename('wind_aspect_alignment_rad');
  
  var with_wind_vars = 
  img
  .addBands(wind_speed)
  .addBands(wind_direction_rad)
  .addBands(wind_direction_deg)
  .addBands(wind_aspect_alignment_rad)
  .addBands(wind_aspect_alignment_deg);
  
  return(with_wind_vars);
};


var creek_2020_09_07_did = '135921-2020-09-07';
var creek_2020_09_07 = fired_daily.filter(ee.Filter.eq('did', creek_2020_09_07_did));
var ftr = ee.Feature(creek_2020_09_07.first());

// Variables of interest from weather data
var era5_bands_of_interest = ['temperature_2m', 'surface_pressure', 'dewpoint_temperature_2m', 'volumetric_soil_water_layer_1', 'u_component_of_wind_10m', 'v_component_of_wind_10m'];
// vpd_hPa; vapor pressure deficit in hPa
// temperature_2m; air temperature in Kelvin
// surface_pressure; surface pressure in Pa
// dewpoint_temperature_2m; dewpoint temperature in Kelvin (temperature at which current amount of water in air would be at saturation vapor pressure)
// volumetric_soil_water_layer_1; volume of water in soil between 0 and 7cm deep (m^3 water per m^3 soil)
// u_component_of_wind_10m; east component of wind speed
// v_component_of_wind_10m; north component of wind speed

var era5_match = era5
    .filterBounds(ftr.geometry())
    .filterDate(ee.Date(ftr.get('date')), ee.Date(ftr.get('date')).advance(24, 'hour'))
    .select(era5_bands_of_interest)
    .map(add_vpd_to_era5)
    .map(add_wind_vars_to_era5);
    
var wind = ee.Image(era5_match.filter(ee.Filter.eq('hour', 18)).first()).select('wind_speed');
var vpd = ee.Image(era5_match.filter(ee.Filter.eq('hour', 18)).first()).select('vpd_hPa');

var fi_creek_2020_09_07_a = fi_fired_tcf_01.filter(ee.Filter.eq('did', creek_2020_09_07_did));
var fi_creek_2020_09_07_b = fi_fired_tcf_02.filter(ee.Filter.eq('did', creek_2020_09_07_did));
var fi_creek_2020_09_07_c = fi_fired_tcf_03.filter(ee.Filter.eq('did', creek_2020_09_07_did));
var fi_creek_2020_09_07_d = fi_fired_tcf_04.filter(ee.Filter.eq('did', creek_2020_09_07_did));
var fi_creek_2020_09_07_e = fi_fired_tcf_05.filter(ee.Filter.eq('did', creek_2020_09_07_did));

var fi_creek_2020_09_07 = 
  fi_creek_2020_09_07_a
  .merge(fi_creek_2020_09_07_b)
  .merge(fi_creek_2020_09_07_c)
  .merge(fi_creek_2020_09_07_d)
  .merge(fi_creek_2020_09_07_e);

var landformsVis = {
  min: 11.0,
  max: 42.0,
  palette: [
    '141414', '383838', '808080', 'EBEB8F', 'F7D311', 'AA0000', 'D89382',
    'DDC9C9', 'DCCDCE', '1C6330', '68AA63', 'B5C98E', 'E1F0E5', 'a975ba',
    '6f198c'
  ],
};

Map.addLayer(landforms_10m_ned, landformsVis, 'Landforms', false);
Map.addLayer(wind, {min: 0, max: 4}, 'ERA5 wind (m/s)', false);
Map.addLayer(vpd, {min: 0, max: 50}, 'VPD (hPa)', false);

Map.addLayer(grip4_north_america, {}, 'Large roads');

Map.addLayer(fi_creek_2020_09_07_a.merge(fi_creek_2020_09_07_b), {color: 'blue'}, 'Random locations for Creek Fire footprint', false);

Map.addLayer(creek_2020_09_07, {color: 'red'}, 'Actual Creek Fire footprint');

var barren_grass_forb_herb_mix_tm01 = lcms.select('Land_Cover').filterDate('2019-01-01', '2020-01-01').first().eq(11);
print(lcms.select('Land_Cover').filterDate('2019-01-01', '2020-01-01'));

Map.addLayer(barren_grass_forb_herb_mix_tm01, {}, 'Barren/grass/forb/herb mix from previous year');