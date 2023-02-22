// Water and land area of each measured area including the FIRED perimeters, 
// the FIRED perimeters re-distributed throughout their biome, and the RESOLVE biomes
var lf = ee.ImageCollection("projects/cires-gg-earthlab/landfire-annual-disturbance/lf-disturbance-ca");
var hansen = ee.Image("UMD/hansen/global_forest_change_2021_v1_9");
var states = ee.FeatureCollection("TIGER/2018/States");
var resolve = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017");
var dem_10m = ee.Image("USGS/3DEP/10m");
var dem_1m = ee.ImageCollection("USGS/3DEP/1m");

// FIRED events
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


var water = hansen.select(['datamask']).eq(1);

var ca = states.filter(ee.Filter.inList('NAME', ['California'])).geometry();
lf = lf.map(function(img) {
  return img.clip(ca).unmask().updateMask(water);
});

// FIRST GET NON-WATER AREA OF EACH BIOME 
var tcf = 
ee.Feature(
resolve
.filterBounds(ca)
.filterMetadata('BIOME_NAME', 'equals', 'Temperate Conifer Forests')
.map(function(ftr) {
  return ftr.intersection(ca);
}).union().first().set({biome_fullname: 'Temperate Conifer Forests',
                        biome_shortname: 'tcf'}
                        ));

Map.addLayer(tcf, {color: 'DarkCyan'}, 'Temperate Conifer Forests biome');

var mfws = 
ee.Feature(
resolve
.filterBounds(ca)
.filterMetadata('BIOME_NAME', 'equals', 'Mediterranean Forests, Woodlands & Scrub')
.map(function(ftr) {
  return ftr.intersection(ca);
}).union().first().set({biome_fullname: 'Mediterranean Forests, Woodlands & Scrub',
                        biome_shortname: 'mfws'}
                        ));

var tgss = 
ee.Feature(
  resolve
.filterBounds(ca)
.filterMetadata('BIOME_NAME', 'equals', 'Temperate Grasslands, Savannas & Shrublands')
.map(function(ftr) {
  return ftr.intersection(ca);
}).union().first().set({biome_fullname: 'Temperate Grasslands, Savannas & Shrublands',
                        biome_shortname: 'tgss'}
                        ));

var dxs = 
ee.Feature(
resolve
.filterBounds(ca)
.filterMetadata('BIOME_NAME', 'equals', 'Deserts & Xeric Shrublands')
.map(function(ftr) {
  return ftr.intersection(ca);
}).union().first().set({biome_fullname: 'Deserts & Xeric Shrublands',
                        biome_shortname: 'dxs'}
                        ));

var resolve_biomes = ee.FeatureCollection([tcf, mfws, tgss, dxs]);

var resolve_nonwater_areas = water.reduceRegions({
  collection: resolve_biomes,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: resolve_nonwater_areas,
  description: 'resolve-biome-non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 


//
//
// NON-WATER AREA OF EACH FIRED EVENT (SHOULD BE ZERO!)
//
//

var fired_nonwater_areas = water.reduceRegions({
  collection: fired_daily,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fired_nonwater_areas,
  description: 'fired-non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 


//
//
// NON-WATER AREA OF EACH FIRED EVENT (SHOULD BE ZERO!)
//
//

// dxs
// 1st set
var fi_fired_dxs_01_nonwater_areas = water.reduceRegions({
  collection: fi_fired_dxs_01,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_dxs_01_nonwater_areas,
  description: 'fi_fired_dxs_01_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 2nd set
var fi_fired_dxs_02_nonwater_areas = water.reduceRegions({
  collection: fi_fired_dxs_02,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_dxs_02_nonwater_areas,
  description: 'fi_fired_dxs_02_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 3rd set
var fi_fired_dxs_03_nonwater_areas = water.reduceRegions({
  collection: fi_fired_dxs_03,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_dxs_03_nonwater_areas,
  description: 'fi_fired_dxs_03_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 4th set
var fi_fired_dxs_04_nonwater_areas = water.reduceRegions({
  collection: fi_fired_dxs_04,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_dxs_04_nonwater_areas,
  description: 'fi_fired_dxs_04_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 5th set
var fi_fired_dxs_05_nonwater_areas = water.reduceRegions({
  collection: fi_fired_dxs_05,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_dxs_05_nonwater_areas,
  description: 'fi_fired_dxs_05_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// mfws
// 1st set
var fi_fired_mfws_01_nonwater_areas = water.reduceRegions({
  collection: fi_fired_mfws_01,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_mfws_01_nonwater_areas,
  description: 'fi_fired_mfws_01_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 2nd set
var fi_fired_mfws_02_nonwater_areas = water.reduceRegions({
  collection: fi_fired_mfws_02,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_mfws_02_nonwater_areas,
  description: 'fi_fired_mfws_02_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 3rd set
var fi_fired_mfws_03_nonwater_areas = water.reduceRegions({
  collection: fi_fired_mfws_03,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_mfws_03_nonwater_areas,
  description: 'fi_fired_mfws_03_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 4th set
var fi_fired_mfws_04_nonwater_areas = water.reduceRegions({
  collection: fi_fired_mfws_04,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_mfws_04_nonwater_areas,
  description: 'fi_fired_mfws_04_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 5th set
var fi_fired_mfws_05_nonwater_areas = water.reduceRegions({
  collection: fi_fired_mfws_05,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_mfws_05_nonwater_areas,
  description: 'fi_fired_mfws_05_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// tgss
// 1st set
var fi_fired_tgss_01_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tgss_01,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tgss_01_nonwater_areas,
  description: 'fi_fired_tgss_01_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 2nd set
var fi_fired_tgss_02_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tgss_02,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tgss_02_nonwater_areas,
  description: 'fi_fired_tgss_02_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 3rd set
var fi_fired_tgss_03_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tgss_03,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tgss_03_nonwater_areas,
  description: 'fi_fired_tgss_03_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 4th set
var fi_fired_tgss_04_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tgss_04,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tgss_04_nonwater_areas,
  description: 'fi_fired_tgss_04_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 5th set
var fi_fired_tgss_05_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tgss_05,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tgss_05_nonwater_areas,
  description: 'fi_fired_tgss_05_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// tcf
// 1st set
var fi_fired_tcf_01_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tcf_01,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tcf_01_nonwater_areas,
  description: 'fi_fired_tcf_01_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 2nd set
var fi_fired_tcf_02_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tcf_02,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tcf_02_nonwater_areas,
  description: 'fi_fired_tcf_02_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 3rd set
var fi_fired_tcf_03_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tcf_03,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tcf_03_nonwater_areas,
  description: 'fi_fired_tcf_03_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 4th set
var fi_fired_tcf_04_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tcf_04,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tcf_04_nonwater_areas,
  description: 'fi_fired_tcf_04_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 

// 5th set
var fi_fired_tcf_05_nonwater_areas = water.reduceRegions({
  collection: fi_fired_tcf_05,
  reducer: ee.Reducer.sum(),
  scale: 30,
  crs: 'EPSG:3310'
});

Export.table.toDrive({
  collection: fi_fired_tcf_05_nonwater_areas,
  description: 'fi_fired_tcf_05_non-water-area-30m-pixel-count',
  folder: 'ee',
  fileFormat: 'csv'
}); 
