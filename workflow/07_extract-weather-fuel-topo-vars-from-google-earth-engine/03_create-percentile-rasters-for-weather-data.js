var era5 = ee.ImageCollection("ECMWF/ERA5_LAND/HOURLY"),
    states = ee.FeatureCollection("TIGER/2018/States"),
    gridmet = ee.ImageCollection("IDAHO_EPSCOR/GRIDMET"),
    rtma = ee.ImageCollection("NOAA/NWS/RTMA"),
    dem10_3dep = ee.Image("USGS/3DEP/10m");
    
var add_vpd = function(img) {

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

var add_wind_vars = function(img) {
  
  // https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398
  
  // Pull out separate components of wind so we can more easily work with them
  var u_wind = img.select('u_component_of_wind_10m');
  var v_wind = img.select('u_component_of_wind_10m');
  
  // abs(wind_speed) = sqrt(u^2 + v^2)
  var wind_speed = u_wind.pow(2).add(v_wind.pow(2)).sqrt().rename('wind_speed');
  var with_wind_vars = img.addBands(wind_speed);

  return(with_wind_vars);
};


// Calculate Vapor Pressure Deficit given variables contained in RTMA data product
var add_vpd_to_rtma = function(img) {

  // Saturation vapor pressure (how much water can the air hold at this temperature?)
  // In units of Pa (divide by 100 to get hPa)
  var esat = img.expression("(1.0007 + (3.46 * 1e-7 * pressure_Pa)) * a * (exp(b * Tair_C / (c + Tair_C)))", 
                {'pressure_Pa': img.select('PRES'), // surface pressure in Pa
                 'Tair_C': img.select('TMP'), // air temperature in degrees Celsius
                 'a': 611.21, // constant
                 'b': 17.502, // constant
                 'c': 240.97 // constant
                }).rename('esat');

  // Actual vapor pressure (how much water is actually in the air?)
  // In units of Pa (divide by 100 to get hPa)  
  var ea = img.expression("(1.0007 + (3.46 * 1e-7 * pressure_Pa)) * a * (exp(b * Tdew_C / (c + Tdew_C)))", 
                {'pressure_Pa': img.select('PRES'), // surface pressure in Pa
                 'Tdew_C': img.select('DPT'), // dew temperature in degrees Celsius
                 'a': 611.21, // constant
                 'b': 17.502, // constant
                 'c': 240.97 // constant
                }).rename('ea');
  
  // vapor pressure deficit is the saturation vapor pressure minus the actual vapor pressure
  // vpd = esat - ea
  // divide by 100 to get units of hPa instead of Pa
  var vpd_val = esat.subtract(ea).divide(100).rename('vpd_hPa');
  var rh = ea.divide(esat).rename('rh');
  var with_vpd = img.addBands(vpd_val).addBands(esat).addBands(ea).addBands(rh).toFloat();
  
  return(with_vpd);
};

var add_wind_vars_to_rtma = function(img) {
  
  // https://confluence.ecmwf.int/pages/viewpage.action?pageId=133262398
  
  var deg_to_rad = ee.Image(Math.PI).divide(ee.Image(180));
  var rad_to_deg = ee.Image(180).divide(ee.Image(Math.PI));
  
  var wind_direction_rad = img.select('WDIR').multiply(deg_to_rad).rename('WDIR_rad');
  
  var aspect = ee.Terrain.aspect(dem10_3dep).rename('aspect');
  
  var wind_aspect_alignment_deg = img.select('WDIR').subtract(aspect).rename('wind_aspect_alignment_deg');
  var wind_aspect_alignment_rad = wind_aspect_alignment_deg.multiply(deg_to_rad).rename('wind_aspect_alignment_rad');
  
  var wind_to_gust = img.select('WIND').where(img.select('GUST'), img.select('GUST')).rename('wind_filled_gust');

  var with_wind_vars = 
  img
  // .addBands(wind_direction_rad)
  .addBands(wind_aspect_alignment_rad)
  .addBands(wind_aspect_alignment_deg)
  .addBands(wind_to_gust)
  .toFloat();
  
  return(with_wind_vars);
};

var ca = ee.Feature(states.filterMetadata('NAME', 'equals', 'California').first());
Map.addLayer(ca);

// vpd_hPa; vapor pressure deficit in hPa
// temperature_2m; air temperature in Kelvin
// surface_pressure; surface pressure in Pa
// dewpoint_temperature_2m; dewpoint temperature in Kelvin (temperature at which current amount of water in air would be at saturation vapor pressure)
// volumetric_soil_water_layer_1; volume of water in soil between 0 and 7cm deep (m^3 water per m^3 soil)
// u_component_of_wind_10m; east component of wind speed
// v_component_of_wind_10m; north component of wind speed
var era5_bands_of_interest = ['temperature_2m', 'surface_pressure', 'dewpoint_temperature_2m', 'volumetric_soil_water_layer_1', 'u_component_of_wind_10m', 'v_component_of_wind_10m'];

// We want all the bands from GRIDMET DROUGHT

// We want just some of the bands from GRIDMET
var gridmet_bands_of_interest = ['erc', 'bi', 'fm100', 'fm1000'];

// var rtma_bands_of_interest = ['TMP', 'PRES', 'DPT', 'UGRD', 'VGRD', 'WDIR', 'WIND', 'GUST', 'ACPC01'];
var rtma_bands_of_interest = ['TMP', 'PRES', 'DPT', 'UGRD', 'VGRD', 'WDIR', 'WIND', 'GUST'];

era5 = era5
.select(era5_bands_of_interest)
.map(add_vpd)
.map(add_wind_vars);

rtma = rtma
.select(rtma_bands_of_interest)
.map(add_vpd_to_rtma)
.map(add_wind_vars_to_rtma);

// print(era5.first());

var toString = function(num) {
  num = ee.Number(num).multiply(100);
  var string = num.format('p_%05d');

  return(string);
  
};

var percentile_reducer = ee.Reducer.percentile(
  {percentiles: ee.List.sequence(0, 100, 0.25), 
   outputNames: ee.List.sequence(0, 100, 0.25).map(toString)});

var percentile_reducer_maxBuckets10k = ee.Reducer.percentile(
  {percentiles: ee.List.sequence(0, 100, 0.25), 
   outputNames: ee.List.sequence(0, 100, 0.25).map(toString),
   maxBuckets: 10000});

// Create the rasters
var era5_percentiles = era5
.select(['temperature_2m', 'volumetric_soil_water_layer_1', 'vpd_hPa', 'rh', 'wind_speed'])
.filterDate('1981-01-01', '2020-12-31')
.map(function(img) {return(img.resample())})
.reduce(percentile_reducer)
.clip(ca.geometry().buffer(50000));

var gridmet_percentiles = gridmet
.select(['erc', 'bi', 'fm100', 'fm1000'])
.filterDate('1981-01-01', '2020-12-31')
.map(function(img) {return(img.resample())})
.reduce(percentile_reducer)
.clip(ca.geometry().buffer(50000));

var rtma_percentiles = rtma
.select(['TMP', 'vpd_hPa', 'rh', 'WIND', 'GUST', 'wind_filled_gust', 'wind_aspect_alignment_rad'])
.filterDate('2011-01-01', '2020-12-31')
.map(function(img) {return(img.resample())})
.reduce(percentile_reducer)
.clip(ca.geometry().buffer(50000));

// Export percentile rasters
Export.image.toDrive({
  image: era5_percentiles,
  description: 'era5-weather-percentiles_1981-01-01_2020-12-31',
  folder: 'ee',
  fileNamePrefix: 'era5-weather-percentiles_1981-01-01_2020-12-31',
  crs: "EPSG:3310",
  region: ca.geometry().buffer(50000),
  scale: 11132
});

Export.image.toDrive({
  image: gridmet_percentiles,
  description: 'gridmet-weather-percentiles_1981-01-01_2020-12-31',
  folder: 'ee',
  fileNamePrefix: 'gridmet-weather-percentiles_1981-01-01_2020-12-31',
  crs: "EPSG:3310",
  region: ca.geometry().buffer(50000),
  scale: 4638.3
});

Export.image.toDrive({
  image: rtma_percentiles,
  description: 'rtma-weather-percentiles_2011-01-01_2020-12-31',
  folder: 'ee',
  fileNamePrefix: 'rtma-weather-percentiles_2011-01-01_2020-12-31',
  crs: "EPSG:3310",
  region: ca.geometry().buffer(50000),
  scale: 2500
});
