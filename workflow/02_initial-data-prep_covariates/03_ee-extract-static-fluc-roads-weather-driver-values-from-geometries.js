// Purpose: Get fuel, weather, and topography data for each event in California FIRED database
// General vector files delineating study area (California and RESOLVE biomes)
var tiger = ee.FeatureCollection("TIGER/2018/States");
var resolve = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017");

// Weather/climate variables
var era5 = ee.ImageCollection("ECMWF/ERA5_LAND/HOURLY");
var rtma = ee.ImageCollection("NOAA/NWS/RTMA");
var gridmet_drought = ee.ImageCollection("GRIDMET/DROUGHT");
var gridmet = ee.ImageCollection("IDAHO_EPSCOR/GRIDMET");

// Topography variables
var dem10_3dep = ee.Image("USGS/3DEP/10m");
var landforms_10m_ned = ee.Image("CSP/ERGo/1_0/US/landforms");
var sa_pa_10m_3dep = ee.Image("users/mkoontz/megafired-california/terrain-rumple-index_10m-3dep");

// Some of the fuel variables
var lcms = ee.ImageCollection("USFS/GTAC/LCMS/v2020-5");
var veg_struct_rumple = ee.ImageCollection("users/mkoontz/veg-structure-rumple-california");
var prefire_ndvi_composites = ee.ImageCollection("users/mkoontz/ndvi-summer-california-composite");
var sa_pa_ndvi = ee.ImageCollection("users/mkoontz/veg-structure-rumple-california");

// Roads variables
var grip4_north_america = ee.FeatureCollection("projects/sat-io/open-datasets/GRIP4/North-America");
var tiger_roads = ee.FeatureCollection("TIGER/2016/Roads");

// FIRED events
var fired_events = ee.FeatureCollection("users/mkoontz/megafired-california/fired_events_ca_ewe_rank");
// var fired_daily_v1 = ee.FeatureCollection("users/mkoontz/megafired-california/fired_daily_ca_ewe_rank");
var fired_daily_biggest_poly = ee.FeatureCollection("users/mkoontz/megafired-california/fired_daily_ca_epsg3310_2003-2020_biggest-poly");
var fired_daily = ee.FeatureCollection("users/mkoontz/megafired-california/fired_daily_ca_epsg3310_2003-2020");

// FIRED events redistributed to fire-independent locations
var fi_fired_dxs_01 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_dxs_v4_01");
var fi_fired_dxs_02 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_dxs_v4_02");
var fi_fired_dxs_03 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_dxs_v4_03");
var fi_fired_dxs_04 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_dxs_v4_04");
var fi_fired_dxs_05 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_dxs_v4_05");
var fi_fired_mfws_01 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_mfws_v4_01");
var fi_fired_mfws_02 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_mfws_v4_02");
var fi_fired_mfws_03 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_mfws_v4_03");
var fi_fired_mfws_04 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_mfws_v4_04");
var fi_fired_mfws_05 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_mfws_v4_05");
var fi_fired_tcf_01 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_01");
var fi_fired_tcf_02 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_02");
var fi_fired_tcf_03 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_03");
var fi_fired_tcf_04 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_04");
var fi_fired_tcf_05 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tcf_v4_05");
var fi_fired_tgss_01 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tgss_v4_01");
var fi_fired_tgss_02 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tgss_v4_02");
var fi_fired_tgss_03 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tgss_v4_03");
var fi_fired_tgss_04 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tgss_v4_04");
var fi_fired_tgss_05 = ee.FeatureCollection("users/mkoontz/fired_daily_random-locations_tgss_v4_05");

// not used, in the end
var friction = ee.Image("Oxford/MAP/friction_surface_2019");
    
// Projection we'll work in (California Albers) for consistency in reducer outputs
var proj = ee.Projection("EPSG:3310");
var ca = ee.Feature(tiger.filterMetadata('NAME', 'equals', 'California').first());
var export_geo = ca.geometry().buffer(50000);

var years = ee.List.sequence(2011, 2020).map(function(i) {return ee.String(ee.Number(i).toInt())});

var tcf = 
ee.Feature(
  resolve
.filterBounds(ca.geometry())
.filterMetadata('BIOME_NAME', 'equals', 'Temperate Conifer Forests')
.map(function(ftr) {
  return ftr.intersection(ca.geometry());
}).union().first().set({id: 'tcf', biome_fullname: 'Temperate Conifer Forests', biome_shortname: 'tcf'}));
Map.addLayer(tcf, {color: 'DarkCyan'}, 'Temperate Conifer Forests biome');

var mfws = 
ee.Feature(
  resolve
.filterBounds(ca.geometry())
.filterMetadata('BIOME_NAME', 'equals', 'Mediterranean Forests, Woodlands & Scrub')
.map(function(ftr) {
  return ftr.intersection(ca.geometry());
}).union().first().set({id: 'mfws', biome_fullname: 'Mediterranean Forests, Woodlands & Scrub', biome_shortname: 'mfws'}));

var tgss = 
ee.Feature(resolve
.filterBounds(ca.geometry())
.filterMetadata('BIOME_NAME', 'equals', 'Temperate Grasslands, Savannas & Shrublands')
.map(function(ftr) {
  return ftr.intersection(ca.geometry());
}).union().first().set({id: 'tgss', biome_fullname: 'Temperate Grasslands, Savannas & Shrublands', biome_shortname: 'tgss'}));

var dxs = 
ee.Feature(
resolve
.filterBounds(ca.geometry())
.filterMetadata('BIOME_NAME', 'equals', 'Deserts & Xeric Shrublands')
.map(function(ftr) {
  return ftr.intersection(ca.geometry());
}).union().first().set({id: 'dxs', biome_fullname: 'Deserts & Xeric Shrublands', biome_shortname: 'dxs'}));

var ca_biomes = ee.FeatureCollection(years.map(function(yr) {
  var out = ee.FeatureCollection(
    [tcf.set({date: ee.String(yr).cat('-01-01')}), mfws.set({date: ee.String(yr).cat('-01-01')}), 
     tgss.set({date: ee.String(yr).cat('-01-01')}), dxs.set({date: ee.String(yr).cat('-01-01')})]);
  return out;
  })).flatten();
  
var ca_biomes_static = ca_biomes.filterMetadata('date', 'equals', '2020-01-01');

var era5_export_scale = 11132;
var dem10_export_scale = 10.2;
var dem30_export_scale = 30;
var landsat_export_scale = 30;
var lcms_export_scale = 30;
var gridmet_export_scale = 4638.3;
// var friction_export_scale = 927.67;
var generic_fine_export_scale = 100;
// var era5_export_scale = 200;
// var dem10_export_scale = 10.2;
// var landsat_export_scale = 30;
// var lcms_export_scale = 30;
// var gridmet_export_scale = 200;
var friction_export_scale = 200;

// Variables of interest from weather data
var era5_bands_of_interest = ['temperature_2m', 'surface_pressure', 'dewpoint_temperature_2m', 'volumetric_soil_water_layer_1', 'u_component_of_wind_10m', 'v_component_of_wind_10m'];
// vpd_hPa; vapor pressure deficit in hPa
// temperature_2m; air temperature in Kelvin
// surface_pressure; surface pressure in Pa
// dewpoint_temperature_2m; dewpoint temperature in Kelvin (temperature at which current amount of water in air would be at saturation vapor pressure)
// volumetric_soil_water_layer_1; volume of water in soil between 0 and 7cm deep (m^3 water per m^3 soil)
// u_component_of_wind_10m; east component of wind speed
// v_component_of_wind_10m; north component of wind speed

// We want all the bands from GRIDMET DROUGHT

// We want just some of the bands from GRIDMET
var gridmet_bands_of_interest = ['erc', 'bi', 'fm100', 'fm1000'];

// We want just some of the bands from RTMA
// var rtma_bands_of_interest = ['TMP', 'PRES', 'DPT', 'UGRD', 'VGRD', 'WDIR', 'WIND', 'GUST', 'ACPC01'];
var rtma_bands_of_interest = ['TMP', 'PRES', 'DPT', 'UGRD', 'VGRD', 'WDIR', 'WIND', 'GUST'];
// HGT    Model terrain elevation             -81   4226    m
// PRES   Pressure                            60848   105183    Pa
// TMP    Temperature                        -43.2   43.73   °C
// DPT    Dew point temperature              -81.41    30.92   °C
// UGRD   U-component of wind                -32.93    34.04   m/s
// VGRD   V-component of wind                -28.44    39.21   m/s
// SPFH   Specific humidity                   0   0.02    kg/kg
// WDIR   Wind direction (from which blowing) 0   360   deg true
// WIND   Wind speed                          0   42.46   m/s
// GUST   Wind speed (gust)                   0   58.02   m/s
// VIS    Visibility                          0   20000   m
// TCDC   Total cloud cover                   0   100   %
// ACPC01 Total precipitation                 0   1   kg/(m^2) 

//
// Set up blank dictionaries for the variables for which we want fractional cover
//

// In order to get landcover proportions, we need to make sure there
// is a property in the dictionary for every possible level of the
// category
var lcms_landcover_names_from = ee.List.sequence(1, 15).map(function(num) {
  var out = ee.String(ee.Number(num).toInt());
  return(out);
});

var lcms_landcover_names_to = ee.List.sequence(1, 15).map(function(num) {
  var out = ee.String('lcms_landcover_').cat(ee.Number(num).toInt().format('%02d'));
  return(out);
});

var lcms_landcover_dict_blank = ee.Dictionary.fromLists(lcms_landcover_names_from, ee.List.repeat(0, 15));

var lcms_landuse_names_from = ee.List.sequence(1, 7).map(function(num) {
  var out = ee.String(ee.Number(num).toInt());
  return(out);
});

var lcms_landuse_names_to = ee.List.sequence(1, 7).map(function(num) {
  var out = ee.String('lcms_landuse_').cat(ee.Number(num).toInt().format('%02d'));
  return(out);
});

var lcms_landuse_dict_blank = ee.Dictionary.fromLists(lcms_landuse_names_from, ee.List.repeat(0, 7));

var lcms_change_names_from = ee.List.sequence(1, 5).map(function(num) {
  var out = ee.String(ee.Number(num).toInt());
  return(out);
});

var lcms_change_names_to = ee.List.sequence(1, 5).map(function(num) {
  var out = ee.String('lcms_change_').cat(ee.Number(num).toInt().format('%02d'));
  return(out);
});

var lcms_change_dict_blank = ee.Dictionary.fromLists(lcms_change_names_from, ee.List.repeat(0, 5));


// Landforms
var csp_ergo_landforms_names_from = 
  ee.List(['11', '12', '13', '14', '15', '21', '22', '23', '24', '31', '32', '33', '34', '41', '42']);

var csp_ergo_landforms_names_to = csp_ergo_landforms_names_from.map(function(element) {
  var out = ee.String('csp_ergo_landforms_').cat(element);
  return(out);
});

var csp_ergo_landforms_dict_blank = 
  ee.Dictionary.fromLists(csp_ergo_landforms_names_from, 
                          ee.List.repeat(0, csp_ergo_landforms_names_from.length()));

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


// This function extracts climate variables from the ERA5 product
// It is intended to be mapped over FIRED event perimeter feature 
// in turn.
var extract_era5_megafire_drivers = function(ftr) {
  // Subset the ERA5 ImageCollection to images that overlap
  // the current fire's perimeter and burn duration
  // Calculate VPD for each image in that subsetted collection
  var era5_match = era5
    .filterBounds(ftr.geometry())
    .filterDate(ee.Date(ftr.get('date')), ee.Date(ftr.get('date')).advance(24, 'hour'))
    .select(era5_bands_of_interest)
    .map(add_vpd_to_era5)
    .map(add_wind_vars_to_era5);
  
  // Across each of the images in that subset (which now
  // includes VPD calculations), calculate the spatial 
  // average of a few key climate variables across the whole fire 
  // perimeter
  var out = era5_match.map(function(img) {
    
    // Extract ERA5 climate variables at the centroid of geometry
    var this_fired_event_current_era5_climate = 
    img
    .resample()
    .reduceRegion({
      reducer: ee.Reducer.mean(), 
      geometry: ftr.geometry().centroid(), 
      crs: proj,
      scale: generic_fine_export_scale,
      maxPixels: 1e8,
      bestEffort: true
    });
    
    // Augment the original feature to include the unique identifiers
    // "id", "did", and "date", set the geometry to null, then add in
    // the values for the 
    // Add the time stamp of the ERA5 image (from which the VPD 
    // calculation is derived) to the feature being exported 
    var augmented_ftr = ftr
      // .select(['id', 'did', 'date', 'samp_id'], null, false) // just get these three properties, don't rename, and set geometry to null
      .set('era5_datetime', img.get('system:time_start'))
      .set('era5_hour', ee.Date(img.get('system:time_start')).get('hour'))
      .set(this_fired_event_current_era5_climate)
      .toDictionary();

    return(ee.Feature(null, augmented_ftr));

  });
  
    
  return(out);

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
  .addBands(wind_direction_rad)
  .addBands(wind_aspect_alignment_rad)
  .addBands(wind_aspect_alignment_deg)
  .addBands(wind_to_gust);
  
  return(with_wind_vars);
};

// This function extracts climate variables from the RTMA product
// It is intended to be mapped over FIRED event perimeter feature 
// in turn.
var extract_rtma_megafire_drivers = function(ftr) {
  // Subset the RTMA ImageCollection to images that overlap
  // the current fire's perimeter and burn duration
  // Calculate VPD for each image in that subsetted collection
  var rtma_match = rtma
    .filterBounds(ftr.geometry())
    .filterDate(ee.Date(ftr.get('date')), ee.Date(ftr.get('date')).advance(24, 'hour'))
    .select(rtma_bands_of_interest)
    .map(add_vpd_to_rtma)
    .map(add_wind_vars_to_rtma);
  
  // Across each of the images in that subset (which now
  // includes VPD calculations), calculate the spatial 
  // average of a few key climate variables across the whole fire 
  // perimeter
  var out = rtma_match.map(function(img) {
    
    // Extracted RTMA climate variables at centroid of geometry
    var this_fired_event_current_rtma_climate = 
    img
    .resample()
    .reduceRegion({
      reducer: ee.Reducer.mean(), 
      geometry: ftr.geometry().centroid(), 
      crs: proj,
      scale: generic_fine_export_scale,
      maxPixels: 1e8,
      bestEffort: true
    });
    
    // Augment the original feature to include the unique identifiers
    // "id", "did", and "date", set the geometry to null, then add in
    // the values for the 
    // Add the time stamp of the ERA5 image (from which the VPD 
    // calculation is derived) to the feature being exported 
    var augmented_ftr = ftr
      .set('rtma_datetime', img.get('system:time_start'))
      .set('rtma_hour', ee.Date(img.get('system:time_start')).get('hour'))
      .set(this_fired_event_current_rtma_climate)
      .toDictionary();

    return(ee.Feature(null, augmented_ftr));

  });
  
    
  return(out);

};

// Extract driver data from drivers we consider "fluctuating"
// on an annual scale (e.g., LCMS landcover, LCMS change,
// NDVI, NDVI rumple index). That is, those drivers that are different
// from year to year (but not different from day to day, like the
// weather variables
var extract_daily_fluctuating_drivers = function(ftr) {
  
  var ig_year = ee.String(ee.Date(ftr.get('date')).get('year'));
  
  var this_fired_event_landcover_tm0 = lcms
  .select(['Land_Cover'], ['landcover_tm00'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();

  var this_fired_event_landcover_tm1 = lcms
  .select(['Land_Cover'], ['landcover_tm01'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-1, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  // var this_fired_event_landuse = lcms
  // .select(['Land_Use'], ['landuse_tm0'])
  // .filterBounds(ftr.geometry())
  // .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).get('year'))
  // .filterMetadata('study_area', 'equals', 'CONUS')
  // .first();
  
  var this_fired_event_landchange_tm0 = lcms
  .select(['Change'], ['change_tm00'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();

var this_fired_event_landchange_tm1 = lcms
  .select(['Change'], ['change_tm01'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-1, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  var this_fired_event_landchange_tm2 = lcms
  .select(['Change'], ['change_tm02'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-2, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  var this_fired_event_landchange_tm3 = lcms
  .select(['Change'], ['change_tm03'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-3, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  var this_fired_event_landchange_tm4 = lcms
  .select(['Change'], ['change_tm04'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-4, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  var this_fired_event_landchange_tm5 = lcms
  .select(['Change'], ['change_tm05'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-5, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  var this_fired_event_lcms = 
  this_fired_event_landcover_tm0
  .addBands(this_fired_event_landcover_tm0)
  .addBands(this_fired_event_landcover_tm1)
  // .addBands(this_fired_event_landuse)
  .addBands(this_fired_event_landchange_tm0)
  .addBands(this_fired_event_landchange_tm1)
  .addBands(this_fired_event_landchange_tm2)
  .addBands(this_fired_event_landchange_tm3)
  .addBands(this_fired_event_landchange_tm4)
  .addBands(this_fired_event_landchange_tm5);
  
  var lcms_dict = this_fired_event_lcms
    .reduceRegion({
      reducer: ee.Reducer.frequencyHistogram(),
      geometry: ftr.geometry(),
      crs: proj,
      scale: lcms_export_scale,
      maxPixels: 1e8,
      bestEffort: true
    });

  var lcms_landcover_tm0_dict = 
    ee.Dictionary(lcms_dict.get('landcover_tm00'))
    .combine(lcms_landcover_dict_blank, false)
    .rename(lcms_landcover_names_from, lcms_landcover_names_to.map(function(str) {return(ee.String(str).cat('_tm00'));}));

 var lcms_landcover_tm1_dict = 
    ee.Dictionary(lcms_dict.get('landcover_tm01'))
    .combine(lcms_landcover_dict_blank, false)
    .rename(lcms_landcover_names_from, lcms_landcover_names_to.map(function(str) {return(ee.String(str).cat('_tm01'));}));

  // var lcms_landuse_tm0_dict = 
  // ee.Dictionary(lcms_dict.get('landuse_tm0'))
  //   .combine(lcms_landuse_dict_blank, false)
  //   .rename(lcms_landuse_names_from, lcms_landuse_names_to.map(function(str) {return(ee.String(str).cat('_tm00'));}));
  
  var lcms_change_tm0_dict = 
  ee.Dictionary(lcms_dict.get('change_tm00'))
    .combine(lcms_change_dict_blank, false)
    .rename(lcms_change_names_from, lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm00'));}));

 var lcms_change_tm1_dict = 
  ee.Dictionary(lcms_dict.get('change_tm01'))
    .combine(lcms_change_dict_blank, false)
    .rename(lcms_change_names_from, lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm01'));}));

var lcms_change_tm2_dict = 
  ee.Dictionary(lcms_dict.get('change_tm02'))
    .combine(lcms_change_dict_blank, false)
    .rename(lcms_change_names_from, lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm02'));}));

var lcms_change_tm3_dict = 
  ee.Dictionary(lcms_dict.get('change_tm03'))
    .combine(lcms_change_dict_blank, false)
    .rename(lcms_change_names_from, lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm03'));}));

var lcms_change_tm4_dict = 
  ee.Dictionary(lcms_dict.get('change_tm04'))
    .combine(lcms_change_dict_blank, false)
    .rename(lcms_change_names_from, lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm04'));}));

var lcms_change_tm5_dict = 
  ee.Dictionary(lcms_dict.get('change_tm05'))
    .combine(lcms_change_dict_blank, false)
    .rename(lcms_change_names_from, lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm05'));}));
    
  // Mean composite NDVI (masked for clouds, shadows, water, snow) for 60 days prior to fire start
  // var prefire_ndvi_composite = merge_landsat_60_days_prefire(ftr).select(['ndvi']);
  var prefire_ndvi_composite = ee.Image(prefire_ndvi_composites.filter(ee.Filter.eq('ig_year', ig_year)).first());

  var prefire_ndvi_spatial_summary = 
  prefire_ndvi_composite
  .reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ftr.geometry(),
    crs: proj,
    scale: landsat_export_scale,
    maxPixels: 1e8,
    bestEffort: true
  });

  var prefire_ndvi_rumple_index = ee.Image(sa_pa_ndvi.filter(ee.Filter.eq('ig_year', ig_year)).first());
  
  var prefire_ndvi_rumple_spatial_sum = 
    prefire_ndvi_rumple_index
    .reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: ftr.geometry(),
      crs: proj,
      scale: landsat_export_scale,
      maxPixels: 1e8,
      bestEffort: true
    });

  var out = ftr
  .set(prefire_ndvi_spatial_summary)
  .set(prefire_ndvi_rumple_spatial_sum)
  .set(lcms_landcover_tm0_dict)
  .set(lcms_landcover_tm1_dict)
  // .set(lcms_landuse_tm0_dict)
  .set(lcms_change_tm0_dict)
  .set(lcms_change_tm1_dict)
  .set(lcms_change_tm2_dict)
  .set(lcms_change_tm3_dict)
  .set(lcms_change_tm4_dict)
  .set(lcms_change_tm5_dict)
  .toDictionary();

  return(ee.Feature(null, out));
};


// Extract drivers that we are considering 'static', like landform,
// elevation, terrain rumple index, road lengths, friction
var extract_daily_static_drivers = function(ftr) {
  
  // Terrain variables
  var sa_pa = 
  sa_pa_10m_3dep
  .reduceRegion({
    reducer: ee.Reducer.sum(),
    geometry: ftr.geometry(),
    crs: proj,
    scale: 10.2,
    maxPixels: 1e8,
    bestEffort: true
  });

// friction
  
  var this_fired_friction = friction
  .reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ftr.geometry(),
    crs: proj,
    scale: friction_export_scale,
    maxPixels: 1e8,
    bestEffort: true
  });
  
  // var road_lengths_within_fire_m = tiger_roads
  var road_lengths_within_fire_m = grip4_north_america
    .filterBounds(ftr.geometry())
    .map(function(road_ftr) {
      var roads_within_fire = road_ftr
      .intersection(ftr.geometry(), 100); // max error of 100 m
      var out_dict = ee.Dictionary().set('length', roads_within_fire.length());
      var out = ee.Feature(null, out_dict);
      return(out);
    });

  var total_road_length_m = road_lengths_within_fire_m.aggregate_sum('length');
  var total_road_lengths_dict = ee.Dictionary().set('road_length_m', total_road_length_m);
  
  // var this_fired_elev = dem30_srtm
  // .reduceRegion({
  //   reducer: ee.Reducer.mean(),
  //   geometry: ftr.geometry(),
  //   crs: proj,
  //   scale: dem30_export_scale,
  //   maxPixels: 1e8,
  //   bestEffort: true
  // });
  
  // var landforms_dict = landforms_30m_srtm
  //   .reduceRegion({
  //     reducer: ee.Reducer.frequencyHistogram(),
  //     geometry: ftr.geometry(),
  //     crs: proj,
  //     scale: dem30_export_scale,
  //     maxPixels: 1e8,
  //     bestEffort: true
  //   });
    
  var this_fired_elev = dem10_3dep
  .reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ftr.geometry(),
    crs: proj,
    scale: 10.2,
    maxPixels: 1e8,
    bestEffort: true
  });
  
  var landforms_dict = landforms_10m_ned
    .reduceRegion({
      reducer: ee.Reducer.frequencyHistogram(),
      geometry: ftr.geometry(),
      crs: proj,
      scale: 10,
      maxPixels: 1e8,
      bestEffort: true
    });
  
  var csp_ergo_landforms_dict = 
    ee.Dictionary(landforms_dict.get('constant'))
    .combine(csp_ergo_landforms_dict_blank, false)
    .rename(csp_ergo_landforms_names_from, csp_ergo_landforms_names_to);

  var out = ftr
  .set(sa_pa)
  .set(this_fired_elev)
  .set(this_fired_friction)
  .set(total_road_lengths_dict)
  .set(csp_ergo_landforms_dict)
  .toDictionary();

  return(ee.Feature(null, out));
};

// Extract road data
// Easier to extract this separately from other static drivers; 
// Could conceivably change in the future (road data each year, perhaps?
// Different, better road data source like specific USFS roads or timber
// company roads?)
var extract_daily_roads_drivers = function(ftr) {

var road_lengths_within_fire_m = tiger_roads
    .filterBounds(ftr.geometry())
    .map(function(road_ftr) {
      var roads_within_fire = road_ftr
      .intersection(ftr.geometry(), 100); // max error of 100 m
      var out_dict = ee.Dictionary().set('length', roads_within_fire.length());
      var out = ee.Feature(null, out_dict);
      return(out);
    });
    
  var total_road_length_m = road_lengths_within_fire_m.aggregate_sum('length');
  var total_road_lengths_dict = ee.Dictionary().set('road_length_m', total_road_length_m);
  
  var out = ftr
  .set(total_road_lengths_dict)
  .toDictionary();
    
  return(ee.Feature(null, out));

};
    
    

// Extract another set of daily-scale drivers
var extract_daily_gridmet_drivers = function(ftr) {
  
  // some weather variables
  // gridmet_drought includes three types of drought indices at various temporal
  // aggregations (14 days prior, 30 days prior, 90 days prior, 180 days prior
  // 270 days prior, 1 year prior, 2 years prior, and 5 years prior)
  // Standardized Precipitation Index (precip amount)
  // Evaporative Drought Demand Index (Potential Evapotranspiration)
  // Standardized Precipitation Evapotranspiration Index (climatic water deficit)
  // 
  // Also includes Palmer Drought Severity Index and the PDSI z-score
  
  // GRIDMET DROUGHT product has a 5-day temporal resolution
  // Every year starts wtih a 5-day set of January 5th at midnight to January 10th at midnight
  // Every non-leap year ends from December 31st at midnight to January 5th at midnight
  // Leap years end from December 30th at midnight to January 4th at midnight
  // So years after a leap year have a gap between January 4th at midnight to January 5th at midnight
  // We have to fix this in order to properly work with fires that have a burn day early in the
  // year (within first 5 days) after a leap year
 
  // 1) filter by geometry so we have to modify fewer GRIDMET images; 
  //    not sure this actually matters but it's one of those things that
  //    seems like it should matter but very possible doesn't in the way that Earth Engine works
  //    under the hood
  // 2) update the hour in each image's property of the gridmet_drought image collection to reflect
  //    they are actually at UTC, not Mountain Time
  // 3) filter to within 5 days of the burn date; this should result in 1 or 2 images
  //    (two images usually, but 1 image if it occurs in this weird time after a leap year)
  // 4) create a new property for each image that is the absolute value of the difference of that
  //    image's start time and the fire's burn date (at midnight)
  // 5) sort in ascending order based on that property, and then grab the first image from the
  //    resulting set to get the Gridmet image with the start time that is closest in time to
  //    the fire's start date (at midnight)

  // 21600000 seconds in 6 hours to offset for GRIDMET time stamps
  var gridmet_drought_match = gridmet_drought
  .filterBounds(ftr.geometry())
  .filterDate(
    ee.Date(ftr.get('date')).advance(-5, 'days'),
    ee.Date(ftr.get('date')).advance(5, 'days')) 
  .map(function(img) {
    return(img.set('date_diff', ee.Number(img.get('system:time_start')).subtract(ee.Number(21600000)).subtract(ee.Date(ftr.get('date')).millis()).abs()));
  })
  .sort('date_diff', true)
  .first();
  
  // Now just directly update the system time stamp for the image such that the hour is 0 
  // I.e., essentially setting the gridmet time to be UTC
  gridmet_drought_match = gridmet_drought_match
    .set('system:time_start', ee.Date(gridmet_drought_match.get('system:time_start')).update(null, null, null, 0, null, null, null).millis())
    .set('system:time_end', ee.Date(gridmet_drought_match.get('system:time_end')).update(null, null, null, 0, null, null, null).millis());
  
  // GRIDMET has a daily temporal resolution, so we can filter to the specific day that the daily
  // perimeter represents
  // First update the hour to be 0 in each image's time properties to reflect times actually represent
  // UTC rather than Mountain Time Zone
  var gridmet_match = gridmet
  .filterBounds(ftr.geometry())
    .filterDate(
    ee.Date(ftr.get('date')),
    ee.Date(ftr.get('date')).advance(1, 'day'))
  .first()
  .select(gridmet_bands_of_interest);
  
  gridmet_match = gridmet_match
    .set('system:time_start', ee.Date(gridmet_match.get('system:time_start')).update(null, null, null, 0, null, null, null).millis())
    .set('system:time_end', ee.Date(gridmet_match.get('system:time_end')).update(null, null, null, 0, null, null, null).millis());
  
  // Across a particular day's GRIDMET DROUGHT data, calculate the spatial 
  // average of a few key climate variables across the whole fire 
  // perimeter
  var this_fired_event_gridmet_drought = gridmet_drought_match
    .addBands(gridmet_match)
    .resample()
    .reduceRegion({
      reducer: ee.Reducer.mean(), 
      geometry: ftr.geometry().centroid(), 
      crs: proj,
      scale: generic_fine_export_scale,
      maxPixels: 1e8,
      bestEffort: true
    })
    .set('gridmet_drought_datetime', gridmet_drought_match.get('system:time_start'))
    .set('gridmet_datetime', gridmet_match.get('system:time_start'));
  
  var out = ftr
  // .select(['id', 'did', 'date', 'samp_id'], null, false) // just get these three properties, don't rename, and set geometry to null
  .set(this_fired_event_gridmet_drought)
  .toDictionary();
  
  return(ee.Feature(null, out));
  
};

// Export scales
var dem_export_scale = 10.2;
var landsat_export_scale = 30;
var lcms_export_scale = 30;
// var friction_export_scale = 927.67;
var friction_export_scale = 200;
var generic_fine_export_scale = 100;

var fired_daily_working = fired_daily
.map(function(ftr) {
  return(ftr.set('system:time_start', ee.Date(ftr.get('date')))
  .set('system:time_end', ee.Date(ftr.get('date')).advance(1, 'day')))});

// Troubleshooting individual features
// var ftr = ee.Feature(fired_daily_working.filterMetadata('system:index', 'equals', '00000000000000003f32').first());
// var ftr = ee.Feature(fired_daily_working.filterMetadata('system:index', 'equals', '00000000000000003f30').first());
// var ftr = ee.Feature(fired_daily_working.filterMetadata('system:index', 'equals', '000000000000000018f6').first());
// var ftr = ee.Feature(fired_daily_working.filterMetadata('system:index', 'equals', '00000000000000000dac').first());
// var ftr = ee.Feature(fired_daily_working.filterMetadata('system:index', 'equals', '00000000000000000dac').first());
// var ftr = ee.Feature(fired_daily_working.filterMetadata('did', 'equals', '30606-2005-10-06').first());
// var ftr = ee.Feature(fired_daily_working.filterMetadata('did', 'equals', '20285-2004-06-06').first());

var ftr = ee.Feature(fired_daily_working.filter(ee.Filter.eq('id', 11795)).first());
print(ftr);

Map.addLayer(ftr, {}, 'feature');
Map.centerObject(ftr);

// ERA5 drivers
var era5_bands = era5
    .limit(1)
    .select(era5_bands_of_interest)
    .map(add_vpd_to_era5)
    .map(add_wind_vars_to_era5)
    .first()
    .bandNames()
    .cat(ee.List(['id', 'did', 'date', 'samp_id', 'era5_datetime', 'era5_hour']));

var era5_missing = ee.List.repeat(-999, era5_bands.size());
var era5_nodata = ee.Dictionary.fromLists(era5_bands, era5_missing);
var era5_nodata_fc = ee.FeatureCollection(ee.Feature(null, era5_nodata));

var fired_era5_drivers = era5_nodata_fc.merge(fired_daily_biggest_poly.map(extract_era5_megafire_drivers).flatten());
Export.table.toDrive({collection: fired_era5_drivers, folder: 'ee', description: 'FIRED-era5-drivers_california_biggest-poly'});

// GRIDMET drivers
var gridmet_bands = gridmet
    .first()
    .select(gridmet_bands_of_interest)
    .addBands(gridmet_drought.first())
    .bandNames()
    .cat(['id', 'did', 'date', 'samp_id', 'gridmet_drought_datetime', 'gridmet_datetime']);

var gridmet_missing = ee.List.repeat(-999, gridmet_bands.size());
var gridmet_nodata = ee.Dictionary.fromLists(gridmet_bands, gridmet_missing);
var gridmet_nodata_fc = ee.FeatureCollection(ee.Feature(null, gridmet_nodata));

var fired_daily_gridmet_drivers = gridmet_nodata_fc.merge(fired_daily_biggest_poly.map(extract_daily_gridmet_drivers));
Export.table.toDrive({collection: fired_daily_gridmet_drivers, folder: 'ee', description: 'FIRED-daily-gridmet-drivers_california_biggest-poly'});

// RTMA drivers (only for fires after 2011)
var rtma_bands = rtma
    .limit(1)
    .select(rtma_bands_of_interest)
    .map(add_vpd_to_rtma)
    .map(add_wind_vars_to_rtma)
    .first()
    .bandNames()
    .cat(ee.List(['id', 'did', 'date', 'samp_id', 'rtma_datetime', 'rtma_hour']));

var rtma_missing = ee.List.repeat(-999, rtma_bands.size());
var rtma_nodata = ee.Dictionary.fromLists(rtma_bands, rtma_missing);
var rtma_nodata_fc = ee.FeatureCollection(ee.Feature(null, rtma_nodata));

var fired_daily_biggest_poly_for_rtma = 
    fired_daily_biggest_poly
    .map(function(ftr) {
      return(ftr.set('system:time_start', ee.Date(ftr.get('date')))
      .set('system:time_end', ee.Date(ftr.get('date')).advance(1, 'day')))});

var fired_rtma_drivers = rtma_nodata_fc.merge(fired_daily_biggest_poly_for_rtma.filterDate('2011-01-01', '2020-12-31').map(extract_rtma_megafire_drivers).flatten());
Export.table.toDrive({collection: fired_rtma_drivers, folder: 'ee', description: 'FIRED-rtma-drivers_california_biggest-poly'});
print(extract_rtma_megafire_drivers(fired_daily_biggest_poly_for_rtma.filterDate('2011-01-01', '2020-12-31').first()));

// Make sure to add a row of "missing" data so that all variables are represented in the final product!
// https://groups.google.com/g/google-earth-engine-developers/c/05xRvuLzyzI/m/3SEUAjxfCgAJ?pli=1
// Extra row of "missing" data for static drivers
var static_driver_bands = 
    ee.List(['surf_area', 'proj_area', 'road_length_m', 'friction', 'friction_walking_only', 'elevation', 'id', 'did', 'date', 'samp_id'])
    .cat(csp_ergo_landforms_names_to);
    
var static_drivers_missing = ee.List.repeat(-999, static_driver_bands.size());
var static_drivers_nodata = ee.Dictionary.fromLists(static_driver_bands, static_drivers_missing);
var static_drivers_nodata_fc = ee.FeatureCollection(ee.Feature(null, static_drivers_nodata));

// Extra row of "missing" data for roads drivers
var roads_driver_bands = 
    ee.List(['road_length_m', 'id', 'did', 'date', 'samp_id']);

var roads_drivers_missing = ee.List.repeat(-999, roads_driver_bands.size());
var roads_drivers_nodata = ee.Dictionary.fromLists(roads_driver_bands, roads_drivers_missing);
var roads_drivers_nodata_fc = ee.FeatureCollection(ee.Feature(null, roads_drivers_nodata));

// Extra row of "missing" data for flucutating drivers
var fluctuating_drivers_bands = 
    ee.List(['ndvi_surf_area', 'ndvi_proj_area', 'ndvi', 'id', 'did', 'date', 'samp_id'])
    .cat(lcms_landcover_names_to.map(function(str) {return(ee.String(str).cat('_tm00'));}))
    .cat(lcms_landcover_names_to.map(function(str) {return(ee.String(str).cat('_tm01'));}))
    .cat(lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm00'));}))
    .cat(lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm01'));}))
    .cat(lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm02'));}))
    .cat(lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm03'));}))
    .cat(lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm04'));}))
    .cat(lcms_change_names_to.map(function(str) {return(ee.String(str).cat('_tm05'));}));
    // .cat(lcms_landuse_names_to.map(function(str) {return(ee.String(str).cat('_tm00'));}));

var fluctuating_drivers_missing = ee.List.repeat(-999, fluctuating_drivers_bands.size());
var fluctuating_drivers_nodata = ee.Dictionary.fromLists(fluctuating_drivers_bands, fluctuating_drivers_missing);
var fluctuating_drivers_nodata_fc = ee.FeatureCollection(ee.Feature(null, fluctuating_drivers_nodata));

// ############### EXPORT COVARIATE DATA FOR EACH DAILY AREA OF INCREASE ############
var fluc_drivers_version = 'v5';
var static_drivers_version = 'v4'; // v5 uses TIGER census roads data rather than GRIP4 
var roads_drivers_version = 'v1';

// Export daily drivers that we consider 'static' (e.g., 
// landform,  elevation, terrain rumple index, road lengths, friction)
var fired_daily_static_drivers = static_drivers_nodata_fc.merge(fired_daily_working.map(extract_daily_static_drivers));
Export.table.toDrive({collection: fired_daily_static_drivers, folder: 'ee', description: 'FIRED-daily-static-drivers_california_' + static_drivers_version});

var fired_daily_roads_drivers = roads_drivers_nodata_fc.merge(fired_daily_working.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: fired_daily_roads_drivers, folder: 'ee', description: 'FIRED-daily-roads-drivers_california_' + roads_drivers_version});

print(fired_daily_roads_drivers.limit(50));

var fired_daily_fluctuating_drivers = fluctuating_drivers_nodata_fc.merge(fired_daily_working.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: fired_daily_fluctuating_drivers, folder: 'ee', description: 'FIRED-daily-fluctuating-drivers_california_' + fluc_drivers_version});

// ############### EXPORT COVARIATE DATA FOR EACH OF THE 4 BIOMES ##################

// Export daily drivers that we consider 'static' (e.g., 
// landform,  elevation, terrain rumple index, road lengths, friction)
print(ca_biomes);
print(ca_biomes_static);

var resolve_static_drivers = static_drivers_nodata_fc.merge(ca_biomes_static.map(extract_daily_static_drivers));
Export.table.toDrive({collection: resolve_static_drivers, folder: 'ee', description: 'resolve-biomes-static-drivers_california_' + static_drivers_version});

var resolve_roads_drivers = roads_drivers_nodata_fc.merge(ca_biomes_static.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: resolve_roads_drivers, folder: 'ee', description: 'resolve-biomes-daily-roads-drivers_california_' + roads_drivers_version});

var resolve_fluctuating_drivers = fluctuating_drivers_nodata_fc.merge(ca_biomes.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: resolve_fluctuating_drivers, folder: 'ee', description: 'resolve-biomes-fluctuating-drivers_california_' + fluc_drivers_version});



// ############### Fire independent polygons
// Version 4 to get drivers independent of fire (looks a lot like attempt 1 but with 500 random locations instead of 1000)
// Version 5 uses last year's LCMS data instead of this year's LCMS data and only adjusts the fluctuating drivers
// (that is, the v4 static drivers is still most up-to-date as of 2022-08-03)

var fi_fired_tcf_static_01 = static_drivers_nodata_fc.merge(fi_fired_tcf_01.map(extract_daily_static_drivers));
var fi_fired_tcf_static_02 = static_drivers_nodata_fc.merge(fi_fired_tcf_02.map(extract_daily_static_drivers));
var fi_fired_tcf_static_03 = static_drivers_nodata_fc.merge(fi_fired_tcf_03.map(extract_daily_static_drivers));
var fi_fired_tcf_static_04 = static_drivers_nodata_fc.merge(fi_fired_tcf_04.map(extract_daily_static_drivers));
var fi_fired_tcf_static_05 = static_drivers_nodata_fc.merge(fi_fired_tcf_05.map(extract_daily_static_drivers));
Export.table.toDrive({collection: fi_fired_tcf_static_01, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tcf_01'});
Export.table.toDrive({collection: fi_fired_tcf_static_02, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tcf_02'});
Export.table.toDrive({collection: fi_fired_tcf_static_03, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tcf_03'});
Export.table.toDrive({collection: fi_fired_tcf_static_04, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tcf_04'});
Export.table.toDrive({collection: fi_fired_tcf_static_05, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tcf_05'});

var fi_fired_tcf_roads_01 = roads_drivers_nodata_fc.merge(fi_fired_tcf_01.map(extract_daily_roads_drivers));
var fi_fired_tcf_roads_02 = roads_drivers_nodata_fc.merge(fi_fired_tcf_02.map(extract_daily_roads_drivers));
var fi_fired_tcf_roads_03 = roads_drivers_nodata_fc.merge(fi_fired_tcf_03.map(extract_daily_roads_drivers));
var fi_fired_tcf_roads_04 = roads_drivers_nodata_fc.merge(fi_fired_tcf_04.map(extract_daily_roads_drivers));
var fi_fired_tcf_roads_05 = roads_drivers_nodata_fc.merge(fi_fired_tcf_05.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: fi_fired_tcf_roads_01, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tcf_01'});
Export.table.toDrive({collection: fi_fired_tcf_roads_02, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tcf_02'});
Export.table.toDrive({collection: fi_fired_tcf_roads_03, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tcf_03'});
Export.table.toDrive({collection: fi_fired_tcf_roads_04, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tcf_04'});
Export.table.toDrive({collection: fi_fired_tcf_roads_05, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tcf_05'});

var fi_fired_tcf_fluc_01 = fluctuating_drivers_nodata_fc.merge(fi_fired_tcf_01.map(extract_daily_fluctuating_drivers));
var fi_fired_tcf_fluc_02 = fluctuating_drivers_nodata_fc.merge(fi_fired_tcf_02.map(extract_daily_fluctuating_drivers));
var fi_fired_tcf_fluc_03 = fluctuating_drivers_nodata_fc.merge(fi_fired_tcf_03.map(extract_daily_fluctuating_drivers));
var fi_fired_tcf_fluc_04 = fluctuating_drivers_nodata_fc.merge(fi_fired_tcf_04.map(extract_daily_fluctuating_drivers));
var fi_fired_tcf_fluc_05 = fluctuating_drivers_nodata_fc.merge(fi_fired_tcf_05.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: fi_fired_tcf_fluc_01, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tcf_01'});
Export.table.toDrive({collection: fi_fired_tcf_fluc_02, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tcf_02'});
Export.table.toDrive({collection: fi_fired_tcf_fluc_03, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tcf_03'});
Export.table.toDrive({collection: fi_fired_tcf_fluc_04, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tcf_04'});
Export.table.toDrive({collection: fi_fired_tcf_fluc_05, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tcf_05'});

var fi_fired_mfws_static_01 = static_drivers_nodata_fc.merge(fi_fired_mfws_01.map(extract_daily_static_drivers));
var fi_fired_mfws_static_02 = static_drivers_nodata_fc.merge(fi_fired_mfws_02.map(extract_daily_static_drivers));
var fi_fired_mfws_static_03 = static_drivers_nodata_fc.merge(fi_fired_mfws_03.map(extract_daily_static_drivers));
var fi_fired_mfws_static_04 = static_drivers_nodata_fc.merge(fi_fired_mfws_04.map(extract_daily_static_drivers));
var fi_fired_mfws_static_05 = static_drivers_nodata_fc.merge(fi_fired_mfws_05.map(extract_daily_static_drivers));
Export.table.toDrive({collection: fi_fired_mfws_static_01, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_mfws_01'});
Export.table.toDrive({collection: fi_fired_mfws_static_02, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_mfws_02'});
Export.table.toDrive({collection: fi_fired_mfws_static_03, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_mfws_03'});
Export.table.toDrive({collection: fi_fired_mfws_static_04, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_mfws_04'});
Export.table.toDrive({collection: fi_fired_mfws_static_05, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_mfws_05'});

var fi_fired_mfws_roads_01 = roads_drivers_nodata_fc.merge(fi_fired_mfws_01.map(extract_daily_roads_drivers));
var fi_fired_mfws_roads_02 = roads_drivers_nodata_fc.merge(fi_fired_mfws_02.map(extract_daily_roads_drivers));
var fi_fired_mfws_roads_03 = roads_drivers_nodata_fc.merge(fi_fired_mfws_03.map(extract_daily_roads_drivers));
var fi_fired_mfws_roads_04 = roads_drivers_nodata_fc.merge(fi_fired_mfws_04.map(extract_daily_roads_drivers));
var fi_fired_mfws_roads_05 = roads_drivers_nodata_fc.merge(fi_fired_mfws_05.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: fi_fired_mfws_roads_01, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_mfws_01'});
Export.table.toDrive({collection: fi_fired_mfws_roads_02, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_mfws_02'});
Export.table.toDrive({collection: fi_fired_mfws_roads_03, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_mfws_03'});
Export.table.toDrive({collection: fi_fired_mfws_roads_04, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_mfws_04'});
Export.table.toDrive({collection: fi_fired_mfws_roads_05, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_mfws_05'});

var fi_fired_mfws_fluc_01 = fluctuating_drivers_nodata_fc.merge(fi_fired_mfws_01.map(extract_daily_fluctuating_drivers));
var fi_fired_mfws_fluc_02 = fluctuating_drivers_nodata_fc.merge(fi_fired_mfws_02.map(extract_daily_fluctuating_drivers));
var fi_fired_mfws_fluc_03 = fluctuating_drivers_nodata_fc.merge(fi_fired_mfws_03.map(extract_daily_fluctuating_drivers));
var fi_fired_mfws_fluc_04 = fluctuating_drivers_nodata_fc.merge(fi_fired_mfws_04.map(extract_daily_fluctuating_drivers));
var fi_fired_mfws_fluc_05 = fluctuating_drivers_nodata_fc.merge(fi_fired_mfws_05.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: fi_fired_mfws_fluc_01, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_mfws_01'});
Export.table.toDrive({collection: fi_fired_mfws_fluc_02, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_mfws_02'});
Export.table.toDrive({collection: fi_fired_mfws_fluc_03, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_mfws_03'});
Export.table.toDrive({collection: fi_fired_mfws_fluc_04, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_mfws_04'});
Export.table.toDrive({collection: fi_fired_mfws_fluc_05, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_mfws_05'});

var fi_fired_tgss_static_01 = static_drivers_nodata_fc.merge(fi_fired_tgss_01.map(extract_daily_static_drivers));
var fi_fired_tgss_static_02 = static_drivers_nodata_fc.merge(fi_fired_tgss_02.map(extract_daily_static_drivers));
var fi_fired_tgss_static_03 = static_drivers_nodata_fc.merge(fi_fired_tgss_03.map(extract_daily_static_drivers));
var fi_fired_tgss_static_04 = static_drivers_nodata_fc.merge(fi_fired_tgss_04.map(extract_daily_static_drivers));
var fi_fired_tgss_static_05 = static_drivers_nodata_fc.merge(fi_fired_tgss_05.map(extract_daily_static_drivers));
Export.table.toDrive({collection: fi_fired_tgss_static_01, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tgss_01'});
Export.table.toDrive({collection: fi_fired_tgss_static_02, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tgss_02'});
Export.table.toDrive({collection: fi_fired_tgss_static_03, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tgss_03'});
Export.table.toDrive({collection: fi_fired_tgss_static_04, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tgss_04'});
Export.table.toDrive({collection: fi_fired_tgss_static_05, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_tgss_05'});

var fi_fired_tgss_roads_01 = roads_drivers_nodata_fc.merge(fi_fired_tgss_01.map(extract_daily_roads_drivers));
var fi_fired_tgss_roads_02 = roads_drivers_nodata_fc.merge(fi_fired_tgss_02.map(extract_daily_roads_drivers));
var fi_fired_tgss_roads_03 = roads_drivers_nodata_fc.merge(fi_fired_tgss_03.map(extract_daily_roads_drivers));
var fi_fired_tgss_roads_04 = roads_drivers_nodata_fc.merge(fi_fired_tgss_04.map(extract_daily_roads_drivers));
var fi_fired_tgss_roads_05 = roads_drivers_nodata_fc.merge(fi_fired_tgss_05.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: fi_fired_tgss_roads_01, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tgss_01'});
Export.table.toDrive({collection: fi_fired_tgss_roads_02, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tgss_02'});
Export.table.toDrive({collection: fi_fired_tgss_roads_03, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tgss_03'});
Export.table.toDrive({collection: fi_fired_tgss_roads_04, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tgss_04'});
Export.table.toDrive({collection: fi_fired_tgss_roads_05, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_tgss_05'});

var fi_fired_tgss_fluc_01 = fluctuating_drivers_nodata_fc.merge(fi_fired_tgss_01.map(extract_daily_fluctuating_drivers));
var fi_fired_tgss_fluc_02 = fluctuating_drivers_nodata_fc.merge(fi_fired_tgss_02.map(extract_daily_fluctuating_drivers));
var fi_fired_tgss_fluc_03 = fluctuating_drivers_nodata_fc.merge(fi_fired_tgss_03.map(extract_daily_fluctuating_drivers));
var fi_fired_tgss_fluc_04 = fluctuating_drivers_nodata_fc.merge(fi_fired_tgss_04.map(extract_daily_fluctuating_drivers));
var fi_fired_tgss_fluc_05 = fluctuating_drivers_nodata_fc.merge(fi_fired_tgss_05.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: fi_fired_tgss_fluc_01, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tgss_01'});
Export.table.toDrive({collection: fi_fired_tgss_fluc_02, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tgss_02'});
Export.table.toDrive({collection: fi_fired_tgss_fluc_03, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tgss_03'});
Export.table.toDrive({collection: fi_fired_tgss_fluc_04, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tgss_04'});
Export.table.toDrive({collection: fi_fired_tgss_fluc_05, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_tgss_05'});

var fi_fired_dxs_static_01 = static_drivers_nodata_fc.merge(fi_fired_dxs_01.map(extract_daily_static_drivers));
var fi_fired_dxs_static_02 = static_drivers_nodata_fc.merge(fi_fired_dxs_02.map(extract_daily_static_drivers));
var fi_fired_dxs_static_03 = static_drivers_nodata_fc.merge(fi_fired_dxs_03.map(extract_daily_static_drivers));
var fi_fired_dxs_static_04 = static_drivers_nodata_fc.merge(fi_fired_dxs_04.map(extract_daily_static_drivers));
var fi_fired_dxs_static_05 = static_drivers_nodata_fc.merge(fi_fired_dxs_05.map(extract_daily_static_drivers));
Export.table.toDrive({collection: fi_fired_dxs_static_01, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_dxs_01'});
Export.table.toDrive({collection: fi_fired_dxs_static_02, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_dxs_02'});
Export.table.toDrive({collection: fi_fired_dxs_static_03, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_dxs_03'});
Export.table.toDrive({collection: fi_fired_dxs_static_04, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_dxs_04'});
Export.table.toDrive({collection: fi_fired_dxs_static_05, folder: 'ee', description: 'fire-independent-fired_static_' + static_drivers_version + '_dxs_05'});

var fi_fired_dxs_roads_01 = roads_drivers_nodata_fc.merge(fi_fired_dxs_01.map(extract_daily_roads_drivers));
var fi_fired_dxs_roads_02 = roads_drivers_nodata_fc.merge(fi_fired_dxs_02.map(extract_daily_roads_drivers));
var fi_fired_dxs_roads_03 = roads_drivers_nodata_fc.merge(fi_fired_dxs_03.map(extract_daily_roads_drivers));
var fi_fired_dxs_roads_04 = roads_drivers_nodata_fc.merge(fi_fired_dxs_04.map(extract_daily_roads_drivers));
var fi_fired_dxs_roads_05 = roads_drivers_nodata_fc.merge(fi_fired_dxs_05.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: fi_fired_dxs_roads_01, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_dxs_01'});
Export.table.toDrive({collection: fi_fired_dxs_roads_02, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_dxs_02'});
Export.table.toDrive({collection: fi_fired_dxs_roads_03, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_dxs_03'});
Export.table.toDrive({collection: fi_fired_dxs_roads_04, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_dxs_04'});
Export.table.toDrive({collection: fi_fired_dxs_roads_05, folder: 'ee', description: 'fire-independent-fired_roads_' + roads_drivers_version + '_dxs_05'});

var fi_fired_dxs_fluc_01 = fluctuating_drivers_nodata_fc.merge(fi_fired_dxs_01.map(extract_daily_fluctuating_drivers));
var fi_fired_dxs_fluc_02 = fluctuating_drivers_nodata_fc.merge(fi_fired_dxs_02.map(extract_daily_fluctuating_drivers));
var fi_fired_dxs_fluc_03 = fluctuating_drivers_nodata_fc.merge(fi_fired_dxs_03.map(extract_daily_fluctuating_drivers));
var fi_fired_dxs_fluc_04 = fluctuating_drivers_nodata_fc.merge(fi_fired_dxs_04.map(extract_daily_fluctuating_drivers));
var fi_fired_dxs_fluc_05 = fluctuating_drivers_nodata_fc.merge(fi_fired_dxs_05.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: fi_fired_dxs_fluc_01, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_dxs_01'});
Export.table.toDrive({collection: fi_fired_dxs_fluc_02, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_dxs_02'});
Export.table.toDrive({collection: fi_fired_dxs_fluc_03, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_dxs_03'});
Export.table.toDrive({collection: fi_fired_dxs_fluc_04, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_dxs_04'});
Export.table.toDrive({collection: fi_fired_dxs_fluc_05, folder: 'ee', description: 'fire-independent-fired_fluc_' + fluc_drivers_version + '_dxs_05'});
