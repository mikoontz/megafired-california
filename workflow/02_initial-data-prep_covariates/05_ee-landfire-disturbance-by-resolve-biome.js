var lf = ee.ImageCollection("projects/cires-gg-earthlab/landfire-annual-disturbance/lf-disturbance-ca");
var hansen = ee.Image("UMD/hansen/global_forest_change_2021_v1_9");
var states = ee.FeatureCollection("TIGER/2018/States");
var resolve = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017");
var dem_10m = ee.Image("USGS/3DEP/10m");
var dem_1m = ee.ImageCollection("USGS/3DEP/1m");

// Raster attribute table for the simplified landfire disturbance stack    
//     new_val                                    new_cat              new_dist_type    new_sev_type        new_sev_name
// 1:       11                                   fire_low                       fire             low             low_sev
// 2:       12                                fire_medium                       fire          medium          medium_sev
// 3:       13                                  fire_high                       fire            high            high_sev
// 4:       14                       fire_increased_green                       fire increased_green increased_green_sev
// 5:       21                         insect_disease_low             insect_disease             low             low_sev
// 6:       22                      insect_disease_medium             insect_disease          medium          medium_sev
// 7:       23                        insect_disease_high             insect_disease            high            high_sev
// 8:       24             insect_disease_increased_green             insect_disease increased_green increased_green_sev
// 9:       31             clearcut_harvest_othermech_low clearcut_harvest_othermech             low             low_sev
// 10:      32          clearcut_harvest_othermech_medium clearcut_harvest_othermech          medium          medium_sev
// 11:      33            clearcut_harvest_othermech_high clearcut_harvest_othermech            high            high_sev
// 12:      34 clearcut_harvest_othermech_increased_green clearcut_harvest_othermech increased_green increased_green_sev
// 13:      41                               fuel_trt_low                   fuel_trt             low             low_sev
// 14:      42                            fuel_trt_medium                   fuel_trt          medium          medium_sev
// 15:      43                              fuel_trt_high                   fuel_trt            high            high_sev
// 16:      44                   fuel_trt_increased_green                   fuel_trt increased_green increased_green_sev
// 17:      51                                  other_low                      other             low             low_sev
// 18:      52                               other_medium                      other          medium          medium_sev
// 19:      53                                 other_high                      other            high            high_sev
// 20:      54                      other_increased_green                      other increased_green increased_green_sev

// fire_high_tm01_tm05
// fire_high_tm06_tm10
// fire_not_high_tm01_tm05
// fire_not_high_tm06_tm10
// insect_disease_tm01_tm10
var water = hansen.select(['datamask']).eq(1);

var ca = states.filter(ee.Filter.inList('NAME', ['California'])).geometry();
lf = lf.map(function(img) {
  return img.clip(ca).unmask().updateMask(water);
});

var years = ee.List.sequence(2011, 2020);

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

tcf = ee.FeatureCollection(years.map(function(yr) {
  return tcf.set({year: yr});
}));

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

mfws = ee.FeatureCollection(years.map(function(yr) {
  return mfws.set({year: yr});
}));


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

tgss = ee.FeatureCollection(years.map(function(yr) {
  return tgss.set({year: yr});
}));


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
                        
dxs = ee.FeatureCollection(years.map(function(yr) {
  return dxs.set({year: yr});
}));

var biomes = ee.FeatureCollection([tcf, mfws, tgss, dxs]).flatten();

var disturbance_history_imgs = ee.ImageCollection(
  biomes.map(function(ftr) {
  
  var yr = ftr.get('year');
  
  var fire_high_tm01_tm05 = 
  lf
  .filterDate(ee.Date.fromYMD(ee.Number(yr).subtract(5), 1, 1),
                  ee.Date.fromYMD(ee.Number(yr), 1, 1))
  .map(function(img) {
        var out = 
          img.eq(13);
            // .selfMask()
            // .updateMask(water);
            
        return out;
  }).or()
  .rename('fire_high_tm01_tm05');
  
  var fire_high_tm06_tm10 = 
  lf
  .filterDate(ee.Date.fromYMD(ee.Number(yr).subtract(10), 1, 1),
                  ee.Date.fromYMD(ee.Number(yr).subtract(5), 1, 1))
  .map(function(img) {
        var out = 
          img.eq(13);
            // .selfMask()
            // .updateMask(water);
            
        return out;
  }).or()
  .rename('fire_high_tm06_tm10');

  var fire_not_high_tm01_tm05 = 
  lf
  .filterDate(ee.Date.fromYMD(ee.Number(yr).subtract(5), 1, 1),
                  ee.Date.fromYMD(ee.Number(yr), 1, 1))
  .map(function(img) {
        var out = 
          img.eq(11)
          .or(img.eq(12))
          .or(img.eq(14));
          // .selfMask()
          // .updateMask(water);
          
        return out;
  }).or()
  .rename('fire_not_high_tm01_tm05');

  var fire_not_high_tm06_tm10 = 
  lf
  .filterDate(ee.Date.fromYMD(ee.Number(yr).subtract(10), 1, 1),
                  ee.Date.fromYMD(ee.Number(yr).subtract(5), 1, 1))
  .map(function(img) {
        var out = 
          img.eq(11)
          .or(img.eq(12))
          .or(img.eq(14));
          // .selfMask()
          // .updateMask(water);
          
        return out;
  }).or()
  .rename('fire_not_high_tm06_tm10');

  var insect_disease_tm01_tm10 = 
  lf
  .filterDate(ee.Date.fromYMD(ee.Number(yr).subtract(10), 1, 1),
              ee.Date.fromYMD(ee.Number(yr), 1, 1))
  .map(function(img) {
          var out =
          img.eq(21)
            .or(img.eq(22))
            .or(img.eq(23))
            .or(img.eq(24));
            // .selfMask()
            // .updateMask(water);
        
          return out;
  }).or()
  .rename('insect_disease_tm01_tm10');
  
 var out = 
 fire_high_tm01_tm05
 .addBands(fire_high_tm06_tm10)
 .addBands(fire_not_high_tm01_tm05)
 .addBands(fire_not_high_tm06_tm10)
 .addBands(insect_disease_tm01_tm10)
 .clip(ftr.geometry());
 
 return out.copyProperties(ftr);
}));

print(disturbance_history_imgs);
Map.addLayer(disturbance_history_imgs.first());
Map.addLayer(disturbance_history_imgs.first().geometry(), {}, 'img geo');

var disturbance_history = 
  disturbance_history_imgs.map(function(img) {
  
  var dict = img
      .reduceRegion({
        reducer: ee.Reducer.mean(),
        scale: 30,
        geometry: img.geometry(),
        crs: 'EPSG:5070',
        maxPixels: 1e9
      }).combine(img.toDictionary());
  
  var out = ee.Feature(null, dict);
  
  return out;
  });
  
Export.table.toDrive({
  collection: disturbance_history,
  description: 'landfire-disturbance-history-resolve',
  folder: 'ee',
  fileFormat: 'csv'
});