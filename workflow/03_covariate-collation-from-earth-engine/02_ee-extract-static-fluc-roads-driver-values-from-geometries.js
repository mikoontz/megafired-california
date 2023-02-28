// Purpose: Get fuel, weather, and topography data for each event in California FIRED database
// General vector files delineating study area (California and RESOLVE biomes)
var tiger = ee.FeatureCollection("TIGER/2018/States");
var resolve = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017");
var water = ee.Image("UMD/hansen/global_forest_change_2021_v1_9").select(['datamask']).eq(1);

// Topography variables
var dem10_3dep = ee.Image("USGS/3DEP/10m").updateMask(water);
var landforms_10m_ned = ee.Image("CSP/ERGo/1_0/US/landforms").updateMask(water);
var sa_pa_10m_3dep = ee.Image("users/mkoontz/megafired-california/terrain-rumple-index_10m-3dep").updateMask(water);

// Some of the fuel variables
var lcms = 
ee.ImageCollection("USFS/GTAC/LCMS/v2020-5")
.map(function(img) {return img.updateMask(water)});

var veg_struct_rumple = 
ee.ImageCollection("users/mkoontz/veg-structure-rumple-california")
.map(function(img) {return img.updateMask(water)});

var prefire_ndvi_composites = 
ee.ImageCollection("users/mkoontz/ndvi-summer-california-composite")
.map(function(img) {return img.updateMask(water)});

var sa_pa_ndvi = 
ee.ImageCollection("users/mkoontz/veg-structure-rumple-california")
.map(function(img) {return img.updateMask(water)});

// Roads variables
var tiger_roads = ee.FeatureCollection("TIGER/2016/Roads");

// FIRED events
var fired_events = ee.FeatureCollection("users/mkoontz/megafired-california/fired_events_ca_ewe_rank");
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

var dem10_export_scale = 10.2;
var dem30_export_scale = 30;
var landsat_export_scale = 30;
var lcms_export_scale = 30;
var generic_fine_export_scale = 100;

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

// Extract driver data from drivers we consider "fluctuating"
// on an annual scale (e.g., LCMS landcover, LCMS change,
// NDVI, NDVI rumple index). That is, those drivers that are different
// from year to year (but not different from day to day, like the
// weather variables
var extract_daily_fluctuating_drivers = function(ftr) {
  
  var ig_year = ee.String(ee.Date(ftr.get('date')).get('year'));
  
  var this_fired_event_landcover_tm1 = lcms
  .select(['Land_Cover'], ['landcover_tm01'])
  .filterBounds(ftr.geometry())
  .filterMetadata('year', 'equals', ee.Date(ftr.get('date')).advance(-1, 'year').get('year'))
  .filterMetadata('study_area', 'equals', 'CONUS')
  .first();
  
  var lcms_dict = this_fired_event_landcover_tm1
    .reduceRegion({
      reducer: ee.Reducer.frequencyHistogram(),
      geometry: ftr.geometry(),
      crs: proj,
      scale: lcms_export_scale,
      maxPixels: 1e9
    });

 var lcms_landcover_tm1_dict = 
    ee.Dictionary(lcms_dict.get('landcover_tm01'))
    .combine(lcms_landcover_dict_blank, false)
    .rename(lcms_landcover_names_from, lcms_landcover_names_to.map(function(str) {return(ee.String(str).cat('_tm01'));}));

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
    maxPixels: 1e9
  });

  var prefire_ndvi_rumple_index = ee.Image(sa_pa_ndvi.filter(ee.Filter.eq('ig_year', ig_year)).first());
  
  var prefire_ndvi_rumple_spatial_sum = 
    prefire_ndvi_rumple_index
    .reduceRegion({
      reducer: ee.Reducer.sum(),
      geometry: ftr.geometry(),
      crs: proj,
      scale: landsat_export_scale,
      maxPixels: 1e9
    });

  var out = ftr
  .set(prefire_ndvi_spatial_summary)
  .set(prefire_ndvi_rumple_spatial_sum)
  .set(lcms_landcover_tm1_dict)
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
    maxPixels: 1e10
  });

  var this_fired_elev = dem10_3dep
  .reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ftr.geometry(),
    crs: proj,
    scale: 100,
    maxPixels: 1e10
  });
  
  var landforms_dict = landforms_10m_ned
    .reduceRegion({
      reducer: ee.Reducer.frequencyHistogram(),
      geometry: ftr.geometry(),
      crs: proj,
      scale: 10,
      maxPixels: 1e10
    });
  
  var csp_ergo_landforms_dict = 
    ee.Dictionary(landforms_dict.get('constant'))
    .combine(csp_ergo_landforms_dict_blank, false)
    .rename(csp_ergo_landforms_names_from, csp_ergo_landforms_names_to);

  var out = ftr
  .set(sa_pa)
  .set(this_fired_elev)
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
  
var fired_daily_working = fired_daily
.map(function(ftr) {
  return(ftr.set('system:time_start', ee.Date(ftr.get('date')))
  .set('system:time_end', ee.Date(ftr.get('date')).advance(1, 'day')))});

// Make sure to add a row of "missing" data so that all variables are represented in the final product!
// https://groups.google.com/g/google-earth-engine-developers/c/05xRvuLzyzI/m/3SEUAjxfCgAJ?pli=1
// Extra row of "missing" data for static drivers
var static_driver_bands = 
    ee.List(['surf_area', 'proj_area', 'elevation', 'id', 'did', 'date', 'samp_id'])
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
    .cat(lcms_landcover_names_to.map(function(str) {return(ee.String(str).cat('_tm01'));}));

var fluctuating_drivers_missing = ee.List.repeat(-999, fluctuating_drivers_bands.size());
var fluctuating_drivers_nodata = ee.Dictionary.fromLists(fluctuating_drivers_bands, fluctuating_drivers_missing);
var fluctuating_drivers_nodata_fc = ee.FeatureCollection(ee.Feature(null, fluctuating_drivers_nodata));

// ############### EXPORT COVARIATE DATA FOR EACH DAILY AREA OF INCREASE ############
var fluc_drivers_version = 'v6';
var static_drivers_version = 'v5'; // v5 masks out water, sets bestEffort: to false, just uses final driver variables
var roads_drivers_version = 'v1';

// Export daily drivers that we consider 'static' (e.g., 
// landform,  elevation, terrain rumple index, road lengths, friction)
var fired_daily_static_drivers = static_drivers_nodata_fc.merge(fired_daily_working.map(extract_daily_static_drivers));
Export.table.toDrive({collection: fired_daily_static_drivers, folder: 'ee', description: 'fired-daily-static-drivers_california_' + static_drivers_version});

var fired_daily_roads_drivers = roads_drivers_nodata_fc.merge(fired_daily_working.map(extract_daily_roads_drivers));
Export.table.toDrive({collection: fired_daily_roads_drivers, folder: 'ee', description: 'fired-daily-roads-drivers_california_' + roads_drivers_version});

var fired_daily_fluctuating_drivers = fluctuating_drivers_nodata_fc.merge(fired_daily_working.map(extract_daily_fluctuating_drivers));
Export.table.toDrive({collection: fired_daily_fluctuating_drivers, folder: 'ee', description: 'fired-daily-fluctuating-drivers_california_' + fluc_drivers_version});

// ############### EXPORT COVARIATE DATA FOR EACH OF THE 4 BIOMES ##################

// Export daily drivers that we consider 'static' (e.g., 
// landform,  elevation, terrain rumple index, road lengths, friction)
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
// Version 6 fluc and version 5 static masks out water, sets bestEffort: to false, just uses final driver variables

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
