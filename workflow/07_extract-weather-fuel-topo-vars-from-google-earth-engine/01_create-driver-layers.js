// Export static predictor raster layers so we can see how they shape up across California

var landforms_30m_srtm = ee.Image("CSP/ERGo/1_0/Global/SRTM_landforms"),
    accessibility = ee.Image("Oxford/MAP/friction_surface_2019"),
    lcms = ee.ImageCollection("USFS/GTAC/LCMS/v2020-5"),
    tiger = ee.FeatureCollection("TIGER/2018/States"),
    dem30_srtm = ee.Image("USGS/SRTMGL1_003"),
    resolve = ee.FeatureCollection("RESOLVE/ECOREGIONS/2017"),
    fired_events = ee.FeatureCollection("users/mkoontz/FIRED/fired_events_2000-11-02_2021-03-01"),
    dem10_3dep = ee.Image("USGS/3DEP/10m"),
    landforms_10m_3dep = ee.Image("CSP/ERGo/1_0/US/landforms");
    
// Projection we'll work in (California Albers) for consistency in reducer outputs
var proj = "EPSG:3310";
var ca = ee.Feature(tiger.filterMetadata('NAME', 'equals', 'California').first());
var export_geo = ca.geometry().buffer(50000);

var tcf = 
      resolve
      .filterBounds(export_geo)
      .filter(ee.Filter.eq('BIOME_NAME', 'Temperate Conifer Forests'));

// Rumple index will be used to measure terrain complexity as well as vegetation complexity

// Calculate surface area of pixels in a DEM following methodology of 
// Jenness, J.S. (2004). Calculating landscape surface area from digital elevation models. Wildlife Society Bulletin 32 (3): 829-839, 829–839.

// Earth Engine implementation by Amy DeCastro and Michael Koontz

// we need the length of the side of a pixel (the pixel resolution)
// assume square pixels and take the square root of the pixel area
// using the ee.Image.pixelArea() ensures this will scale depending on 
// what DEM is used

var rumple_index = function(dem) {

 //// calculate the projected area of each cell in the extent
  var pix_area = ee.Image.pixelArea().rename('pa').toFloat();
  var pix_side = pix_area.sqrt().rename("s");
  
  // standardize elevation band name
  dem = dem.rename('z');
  
  // 3x3 square kernel
  var kernel = ee.Kernel.square(1, "pixels", false);
  // dwn = dem_with_neighbors
  var dwn = dem.neighborhoodToBands(kernel).addBands(pix_side).addBands(pix_area);
  
  // Starting bottom left and moving clockwise
  // triangle 1: z_0_0, z_-1_-1, z_0_-1
  // triangle 2: z_0_0, z_-1_0, z_-1_-1
  // triangle 3: z_0_0, z_-1_1, z_-1_0
  // triangle 4: z_0_0, z_0_1, z_-1_1
  // triangle 5: z_0_0, z_1_1, z_0_1
  // triangle 6: z_0_0, z_1_0, z_1_1
  // triangle 7: z_0_0, z_1_-1, z_1_0
  // triangle 8: z_0_0, z_0_-1, z_1_-1
  
  //// calculate the surface lengths for each triangle leg, a collection of hypoteneuses
  //// note that we are clipping the lengths to the bounds of the center cell by dividing by two
  //// starting from center cell to right then clockwise
  var AB = dwn.expression("(((b('s')**2) + ((b('z_-1_1') - b('z_0_1'))**2))**0.5)/2").rename('AB');
  var BC = dwn.expression("(((b('s')**2) + ((b('z_0_1') - b('z_1_1'))**2))**0.5)/2").rename('BC');
  var DE = dwn.expression("(((b('s')**2) + ((b('z_0_0') - b('z_-1_0'))**2))**0.5)/2").rename('DE');
  var EF = dwn.expression("(((b('s')**2) + ((b('z_0_0') - b('z_1_0'))**2))**0.5)/2").rename('EF');
  var GH = dwn.expression("(((b('s')**2) + ((b('z_-1_-1') - b('z_0_-1'))**2))**0.5)/2").rename('GH');
  var HI = dwn.expression("(((b('s')**2) + ((b('z_0_-1') - b('z_1_-1'))**2))**0.5)/2").rename('HI');
  var AD = dwn.expression("(((b('s')**2) + ((b('z_0_1') - b('z_0_0'))**2))**0.5)/2").rename('AD');
  var BE = dwn.expression("(((b('s')**2) + ((b('z_0_0') - b('z_0_1'))**2))**0.5)/2").rename('BE');
  var CF = dwn.expression("(((b('s')**2) + ((b('z_1_1') - b('z_1_0'))**2))**0.5)/2").rename('CF');
  var DG = dwn.expression("(((b('s')**2) + ((b('z_-1_0') - b('z_-1_-1'))**2))**0.5)/2").rename('DG');
  var EH = dwn.expression("(((b('s')**2) + ((b('z_0_0') - b('z_0_-1'))**2))**0.5)/2").rename('EH');
  var FI = dwn.expression("(((b('s')**2) + ((b('z_1_0') - b('z_1_-1'))**2))**0.5)/2").rename('FI');
  var EA = dwn.expression("(((2*(b('s')**2))+ ((b('z_0_0') - b('z_-1_1'))**2))**0.5)/2").rename('EA');
  var EC = dwn.expression("(((2*(b('s')**2))+ ((b('z_0_0') - b('z_1_1'))**2))**0.5)/2").rename('EC');
  var EG = dwn.expression("(((2*(b('s')**2))+ ((b('z_0_0') - b('z_-1_-1'))**2))**0.5)/2").rename('EG');
  var EI = dwn.expression("(((2*(b('s')**2))+ ((b('z_0_0') - b('z_1_-1'))**2))**0.5)/2").rename('EI');
  
  var tri_legs = 
  AB
  .addBands(BC)
  .addBands(DE)
  .addBands(EF)
  .addBands(GH)
  .addBands(HI)
  .addBands(AD)
  .addBands(BE)
  .addBands(CF)
  .addBands(DG)
  .addBands(EH)
  .addBands(FI)
  .addBands(EA)
  .addBands(EC)
  .addBands(EG)
  .addBands(EI);
  
  dwn = dwn.addBands(tri_legs);
  
  //// calculate triangle areas using Heron's theorem
  //// triangles are labeled as they are in Table 2 of Jenness paper
  //// start by calculating semiperimeters for each triangle
  
  var s1 = dwn.expression("((b('EA')+b('AB')+b('BE'))/2)").rename('s1');
  var s2 = dwn.expression("((b('BE')+b('BC')+b('EC'))/2)").rename('s2');
  var s3 = dwn.expression("((b('AD')+b('DE')+b('EA'))/2)").rename('s3');
  var s4 = dwn.expression("((b('EC')+b('CF')+b('EF'))/2)").rename('s4');
  var s5 = dwn.expression("((b('DE')+b('DG')+b('EG'))/2)").rename('s5');
  var s6 = dwn.expression("((b('EF')+b('FI')+b('EI'))/2)").rename('s6');
  var s7 = dwn.expression("((b('EG')+b('EH')+b('GH'))/2)").rename('s7');
  var s8 = dwn.expression("((b('EH')+b('EI')+b('HI'))/2)").rename('s8');
  
  var semiperimeters = 
  s1
  .addBands(s2)
  .addBands(s3)
  .addBands(s4)
  .addBands(s5)
  .addBands(s6)
  .addBands(s7)
  .addBands(s8);
  
  dwn = dwn.addBands(semiperimeters);
  
  //// calculate surface areas of triangles using Heron's theorem
  var t1 = dwn.expression("((b('s1')*(b('s1') - b('EA'))*(b('s1') - b('AB'))*(b('s1') - b('BE')))**0.5)").rename('t1');
  var t2 = dwn.expression("((b('s2')*(b('s2') - b('BE'))*(b('s2') - b('BC'))*(b('s2') - b('EC')))**0.5)").rename('t2');
  var t3 = dwn.expression("((b('s3')*(b('s3') - b('AD'))*(b('s3') - b('DE'))*(b('s3') - b('EA')))**0.5)").rename('t3');
  var t4 = dwn.expression("((b('s4')*(b('s4') - b('EC'))*(b('s4') - b('CF'))*(b('s4') - b('EF')))**0.5)").rename('t4');
  var t5 = dwn.expression("((b('s5')*(b('s5') - b('DE'))*(b('s5') - b('DG'))*(b('s5') - b('EG')))**0.5)").rename('t5');
  var t6 = dwn.expression("((b('s6')*(b('s6') - b('EF'))*(b('s6') - b('FI'))*(b('s6') - b('EI')))**0.5)").rename('t6');
  var t7 = dwn.expression("((b('s7')*(b('s7') - b('EG'))*(b('s7') - b('EH'))*(b('s7') - b('GH')))**0.5)").rename('t7');
  var t8 = dwn.expression("((b('s8')*(b('s8') - b('EH'))*(b('s8') - b('EI'))*(b('s8') - b('HI')))**0.5)").rename('t8');
  
  var tris = 
  t1
  .addBands(t2)
  .addBands(t3)
  .addBands(t4)
  .addBands(t5)
  .addBands(t6)
  .addBands(t7)
  .addBands(t8);
  
  dwn = dwn.addBands(tris);
  
  //// sum the triangles (this is the surface (Rumple) area of the focal cell)
  var sa = dwn.expression("(b('t1'))+(b('t2'))+(b('t3'))+(b('t4'))+(b('t5'))+(b('t6'))+(b('t7'))+(b('t8'))").rename('sa');
  
  // Use this code to make all water pixels have a surface area
  // equal to the projected area
  // This avoids nasty edge effects that result in projected
  // areas being greater than surface (rumple) areas
  var sa_nomask = ee.Image.pixelArea().where(
    {test: sa.mask(),
     value: sa}).rename('sa').toFloat();
     
  dwn = dwn.addBands(sa_nomask).addBands(pix_area);
  
  // Return lighter-weight, 2-band image with just the surface area and projected area
  // of each cell
  return(sa_nomask.addBands(pix_area));
};
// end function

// Select the bands that correspond to the different nominal names of wavelengths, and re-names so that
// bands from different sensors match (ETM+ on Landsat5 and Landsat7 and OLI on Landsat8)
// Note these imported data represent the Collection 2 products, which are new as of 2022-01-01
// Collection 1 products were released in 2016, and are being deprecated.
var ls8SR = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2");
var ls7SR = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2");
var ls5SR = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2");
ls8SR = ls8SR.select(['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'QA_PIXEL'], ['blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'QA_PIXEL']);
ls7SR = ls7SR.select(['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'QA_PIXEL'], ['blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'QA_PIXEL']);
ls5SR = ls5SR.select(['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'QA_PIXEL'], ['blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'QA_PIXEL']);

// Landsat Collection 2 scale and offset
var landsat_scale = 0.0000275;
var landsat_offset = -0.2;

// From example code snippet in Landsat 8 Collection 2
// https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2?hl=en
var apply_landsat_scale_offset = function(img) {
  var opticalBands = img.select(['blue', 'green', 'red', 'nir', 'swir1', 'swir2']).multiply(landsat_scale).add(landsat_offset);
  return img.addBands(opticalBands, null, true); // overwrite existing bands
};

// Mask Landsat surface reflectance images
// Creates a mask for clear pixels 
var lsCfmask = function(lsImg){
  var quality = lsImg.select(['QA_PIXEL']);
  var clear = quality
      .bitwiseAnd(8).eq(0) // cloud shadow
      .and(quality.bitwiseAnd(32).eq(0)) // cloud
      .and(quality.bitwiseAnd(4).eq(0)) // water
      .and(quality.bitwiseAnd(16).eq(0)); // snow
                
  return lsImg.updateMask(clear);
};

// function to prepare the Landsat 8 (OLI sensor) data for analysis
var prepOLI = function(img) {
  var orig = img;
  img = lsCfmask(img);
  img = apply_landsat_scale_offset(img).select(['nir', 'red']);
  // img = img.where(img.gt(1), ee.Image(1)); // if reflectance is less than 0, make it 0
  // img = img.where(img.lt(0), ee.Image(0)); // if reflectance is greater than 1, make it 1
  var ndvi = img.normalizedDifference(['nir', 'red']).rename('ndvi');
  
  return orig.addBands(ndvi);  
};

// function to prepare the Landsat 5 and 7 (ETM+ sensor) data for analysis
var prepETMplus = function(img) {
  var orig = img;
  img = lsCfmask(img);
  // img = apply_landsat_scale_offset(img);
  // img = etm2oli(img).select(['nir', 'red']);
  img = apply_landsat_scale_offset(img).select(['nir', 'red']);
  // img = img.where(img.gt(1), ee.Image(1));
  // img = img.where(img.lt(0), ee.Image(0));
  var ndvi = img.normalizedDifference(['nir', 'red']).rename('ndvi');
  
  return orig.addBands(ndvi);  
};

var merge_landsat_prior_summer = function(year) {
  
  // create pre-fire imagery
  var day1_of_year = ee.Date.parse('YYYY', year);
  var start_day_of_year = ee.Number(152); // start of California "image season" from Table 2 Parks et al., 2019
  var end_day_of_year   = ee.Number(258); // end of California "image season" from Table 2 Parks et al., 2019  

  var day1_of_year_minus_one = day1_of_year.advance(-1, 'year');  
  var day1_of_year_minus_two = day1_of_year.advance(-2, 'year');  
  
  // Filter Landsat collections to just imagery from growing season two years prior to fire and 
  // one year prior to fire
  // Also call the "prep" functions to apply scaling factors, the cloud/water/snow mask, and calculate NDVI
  var ls8 = ls8SR
      .filterBounds(export_geo)
      .filterDate(day1_of_year_minus_two, day1_of_year)
      .filter(ee.Filter.dayOfYear(start_day_of_year, end_day_of_year))
      .map(prepOLI);

  var ls7 = ls7SR
      .filterBounds(export_geo)
      .filterDate(day1_of_year_minus_two, day1_of_year)
      .filter(ee.Filter.dayOfYear(start_day_of_year, end_day_of_year))
      .map(prepETMplus); 
  
  var ls5 = ls5SR
      .filterBounds(export_geo)
      .filterDate(day1_of_year_minus_two, day1_of_year)
      .filter(ee.Filter.dayOfYear(start_day_of_year, end_day_of_year))
      .map(prepETMplus); 

  // Merge Landsat Collections
  var merged_landsat = ee.ImageCollection(ls8.merge(ls7).merge(ls5));

  // Get mean values of the surface reflectance for the previous growing season  
  var prefire_year1  = merged_landsat
                      .filterBounds(export_geo)
                      .filterDate(day1_of_year_minus_one, day1_of_year)
                      .filter(ee.Filter.dayOfYear(start_day_of_year, end_day_of_year))
                      .select(['blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'ndvi'])
                      .mean();

  // Get mean values of the surface reflectance for the growing season 2 years prior
  var prefire_year2  = merged_landsat
                      .filterBounds(export_geo)
                      .filterDate(day1_of_year_minus_two, day1_of_year_minus_one)
                      .filter(ee.Filter.dayOfYear(start_day_of_year, end_day_of_year))
                      .select(['blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'ndvi'])
                      .mean();
                          
  // If there's any masked pixels from the NDVI data 1 year prior to fire, then fill in data from
  // two years prior to the fire
  var out = 
  ee.Image(prefire_year1)
    .unmask(prefire_year2)
    .set('ig_year', year)
    .set('img_year', day1_of_year_minus_one.get('year'))
    .clip(export_geo);
  
  return(out);
};
  

// // Mean composite NDVI for previous growing season (2020 data reflects conditions in 2019)
// var prefire_ndvi_composite_2003 = merge_landsat_prior_summer('2003').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2003 = rumple_index(prefire_ndvi_composite_2003.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2003 = sa_pa_ndvi_2003.select('ndvi_surf_area').divide(sa_pa_ndvi_2003.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2004 = merge_landsat_prior_summer('2004').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2004 = rumple_index(prefire_ndvi_composite_2004.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2004 = sa_pa_ndvi_2004.select('ndvi_surf_area').divide(sa_pa_ndvi_2004.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2005 = merge_landsat_prior_summer('2005').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2005 = rumple_index(prefire_ndvi_composite_2005.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2005 = sa_pa_ndvi_2005.select('ndvi_surf_area').divide(sa_pa_ndvi_2005.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2006 = merge_landsat_prior_summer('2006').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2006 = rumple_index(prefire_ndvi_composite_2006.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2006 = sa_pa_ndvi_2006.select('ndvi_surf_area').divide(sa_pa_ndvi_2006.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2007 = merge_landsat_prior_summer('2007').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2007 = rumple_index(prefire_ndvi_composite_2007.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2007 = sa_pa_ndvi_2007.select('ndvi_surf_area').divide(sa_pa_ndvi_2007.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2008 = merge_landsat_prior_summer('2008').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2008 = rumple_index(prefire_ndvi_composite_2008.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2008 = sa_pa_ndvi_2008.select('ndvi_surf_area').divide(sa_pa_ndvi_2008.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2009 = merge_landsat_prior_summer('2009').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2009 = rumple_index(prefire_ndvi_composite_2009.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2009 = sa_pa_ndvi_2009.select('ndvi_surf_area').divide(sa_pa_ndvi_2009.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2010 = merge_landsat_prior_summer('2010').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2010 = rumple_index(prefire_ndvi_composite_2010.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2010 = sa_pa_ndvi_2010.select('ndvi_surf_area').divide(sa_pa_ndvi_2010.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2011 = merge_landsat_prior_summer('2011').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2011 = rumple_index(prefire_ndvi_composite_2011.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2011 = sa_pa_ndvi_2011.select('ndvi_surf_area').divide(sa_pa_ndvi_2011.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2012 = merge_landsat_prior_summer('2012').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2012 = rumple_index(prefire_ndvi_composite_2012.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2012 = sa_pa_ndvi_2012.select('ndvi_surf_area').divide(sa_pa_ndvi_2012.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2013 = merge_landsat_prior_summer('2013').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2013 = rumple_index(prefire_ndvi_composite_2013.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2013 = sa_pa_ndvi_2013.select('ndvi_surf_area').divide(sa_pa_ndvi_2013.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2014 = merge_landsat_prior_summer('2014').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2014 = rumple_index(prefire_ndvi_composite_2014.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2014 = sa_pa_ndvi_2014.select('ndvi_surf_area').divide(sa_pa_ndvi_2014.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2015 = merge_landsat_prior_summer('2015').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2015 = rumple_index(prefire_ndvi_composite_2015.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2015 = sa_pa_ndvi_2015.select('ndvi_surf_area').divide(sa_pa_ndvi_2015.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2016 = merge_landsat_prior_summer('2016').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2016 = rumple_index(prefire_ndvi_composite_2016.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2016 = sa_pa_ndvi_2016.select('ndvi_surf_area').divide(sa_pa_ndvi_2016.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2017 = merge_landsat_prior_summer('2017').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2017 = rumple_index(prefire_ndvi_composite_2017.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2017 = sa_pa_ndvi_2017.select('ndvi_surf_area').divide(sa_pa_ndvi_2017.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2018 = merge_landsat_prior_summer('2018').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2018 = rumple_index(prefire_ndvi_composite_2018.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2018 = sa_pa_ndvi_2018.select('ndvi_surf_area').divide(sa_pa_ndvi_2018.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2019 = merge_landsat_prior_summer('2019').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2019 = rumple_index(prefire_ndvi_composite_2019.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2019 = sa_pa_ndvi_2019.select('ndvi_surf_area').divide(sa_pa_ndvi_2019.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2020 = merge_landsat_prior_summer('2020').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2020 = rumple_index(prefire_ndvi_composite_2020.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2020 = sa_pa_ndvi_2020.select('ndvi_surf_area').divide(sa_pa_ndvi_2020.select('ndvi_proj_area')).clip(export_geo);

// var prefire_ndvi_composite_2021 = merge_landsat_prior_summer('2021').select(['ndvi']).clip(export_geo);
// var sa_pa_ndvi_2021 = rumple_index(prefire_ndvi_composite_2021.multiply(100)).select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area']);
// var veg_structure_rumple_2021 = sa_pa_ndvi_2021.select('ndvi_surf_area').divide(sa_pa_ndvi_2021.select('ndvi_proj_area')).clip(export_geo);

// .map(function(num) {
//   var out = ee.String('lcms_landcover_').cat(ee.Number(num).toInt().format('%02d'));
//   return(out);
// });

var dem_export_scale = 10.2;
var landsat_export_scale = 30;
var lcms_export_scale = 30;
// var friction_export_scale = 927.67;
var friction_export_scale = 200;
var generic_fine_export_scale = 100;

var years = ee.List.sequence(2003, 2021).map(function(num) {
  var out = ee.Number(num).toInt().format();
  return(out);
});

var prefire_composites = ee.ImageCollection(years.map(merge_landsat_prior_summer));

var sa_pa_ndvi = ee.ImageCollection(
  prefire_composites
  .map(function(img) {
    var ndvi = img.select('ndvi').multiply(100);
    var veg_structure_rumple = 
      rumple_index(ndvi)
      .set('ig_year', img.get('ig_year'))
      .set('img_year', img.get('img_year'))
      .select(['sa', 'pa'], ['ndvi_surf_area', 'ndvi_proj_area'])
      .clip(export_geo);
    return(veg_structure_rumple);
  }));

for (var i = 0; i < years.size().getInfo(); i++) {
  
  var landsatSR_description = ee.String("ndvi-summer-california-composite-").cat(years.get(i));
  var landsatSR_assetId = ee.String('users/mkoontz/').cat(landsatSR_description);
  var this_composite = ee.Image(prefire_composites.filter(ee.Filter.eq('ig_year', years.get(i))).first());
  
  var veg_structure_rumple_description = ee.String("veg-structure-rumple-california-").cat(years.get(i));
  var veg_structure_rumple_assetId = ee.String('users/mkoontz/').cat(veg_structure_rumple_description);
  var this_veg_structure_rumple = ee.Image(sa_pa_ndvi.filter(ee.Filter.eq('ig_year', years.get(i))).first());
  
  Export.image.toAsset({
    image: this_composite.select('ndvi'),
    description: landsatSR_description.getInfo(),
    assetId: landsatSR_assetId.getInfo(),
    pyramidingPolicy: 'mean',
    region: export_geo,
    scale: landsat_export_scale,
    crs: proj,
    maxPixels: 1e10
  });
  
  Export.image.toAsset({
    image: this_veg_structure_rumple,
    description: veg_structure_rumple_description.getInfo(),
    assetId: veg_structure_rumple_assetId.getInfo(),
    pyramidingPolicy: 'mean',
    region: export_geo,
    scale: landsat_export_scale,
    crs: proj,
    maxPixels: 1e10
  });

}

// Terrain variables
var sa_pa_10m_3dep = 
  rumple_index(dem10_3dep).select(['sa', 'pa'], ['surf_area', 'proj_area'])
  .clip(export_geo);

Export.image.toAsset({
  image: sa_pa_10m_3dep,
  description: 'terrain-rumple-index_10m-3dep',
  assetId: 'users/mkoontz/terrain-rumple-index_10m-3dep',
  pyramidingPolicy: 'mean',
  region: export_geo,
  scale: 10.2,
  crs: proj,
  maxPixels: 1.25e10
});

var rumple_index = sa_pa_10m_3dep.select('surf_area').divide(sa_pa_10m_3dep.select('proj_area'));

// Map.addLayer(prefire_ndvi_composite, {min: -1, max: 1}, 'ndvi in summer 2019');
// Map.addLayer(veg_structure_rumple, {min: 1, max: 1.1}, 'Veg structure rumple in summer 2020');

var elev_ca = dem10_3dep.clip(export_geo);

var accessibility_ca = accessibility.clip(export_geo);

var landforms_30m_srtm_ca = landforms_30m_srtm.clip(export_geo);
var narrow_valleys = landforms_30m_srtm_ca.eq(42).selfMask();
var valleys = landforms_30m_srtm_ca.eq(41).selfMask();

var tcf_ca = 
tcf.map(function(x) {
  
  return x.intersection(export_geo);
  
});

// // Export LCMS Land_Cover data
// var landcover_ca_2003 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2003)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2004 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2004)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2005 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2005)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2006 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2006)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2007 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2007)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2008 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2008)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2009 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2009)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2010 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2010)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2011 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2011)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2012 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2012)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2013 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2013)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2014 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2014)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2015 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2015)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2016 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2016)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2017 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2017)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2018 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2018)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2019 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2019)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var landcover_ca_2020 = lcms
//   .select('Land_Cover')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2020)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// // Get LCMS Change layer
// var lcms_change_ca_2003 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2003)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2004 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2004)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2005 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2005)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2006 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2006)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2007 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2007)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2008 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2008)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2009 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2009)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2010 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2010)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2011 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2011)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2012 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2012)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2013 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2013)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2014 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2014)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2015 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2015)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2016 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2016)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2017 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2017)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2018 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2018)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2019 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2019)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

// var lcms_change_ca_2020 = lcms
//   .select('Change')
//   .filterBounds(export_geo)
//   .filterMetadata('year', 'equals', 2020)
//   .filterMetadata('study_area', 'equals', 'CONUS')
//   .first()
//   .clip(export_geo);

var grip4_north_america = ee.FeatureCollection("projects/sat-io/open-datasets/GRIP4/North-America");

var grip4_ca =
grip4_north_america.map(function(x) {
  return x.intersection(export_geo);
});

// var fired_events_2015 = fired_events.filter(ee.Filter.eq('ig_year', 2015));

// Map.addLayer(tcf_ca, {}, "Temperate Conifer Forests");
// Map.addLayer(narrow_valleys, {palette: ['white', 'red']}, "Narrow valleys");
// Map.addLayer(valleys, {palette: ['white', 'blue']}, "Valleys");
// Map.addLayer(fired_events, {}, "FIRED Events");
// Map.addLayer(fired_events_2015, {}, "FIRED Events 2015");

// Export.image.toDrive({
//   image: rumple_index,
//   description: "rumple-index",
//   folder: "ee",
//   region: export_geo,
//   scale: dem_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: elev_ca,
//   description: "srtm30-elevation",
//   folder: "ee",
//   region: export_geo,
//   scale: dem_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: accessibility_ca,
//   description: "accessibility",
//   folder: "ee",
//   region: export_geo,
//   scale: friction_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: landforms_30m_srtm_ca,
//   description: "landforms",
//   folder: "ee",
//   region: export_geo,
//   scale: dem_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// // Export Land_Cover
// Export.image.toDrive({
//   image: landcover_ca_2003,
//   description: "lcms-landcover-2003",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2004,
//   description: "lcms-landcover-2004",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2005,
//   description: "lcms-landcover-2005",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2006,
//   description: "lcms-landcover-2006",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2007,
//   description: "lcms-landcover-2007",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2008,
//   description: "lcms-landcover-2008",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2009,
//   description: "lcms-landcover-2009",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2010,
//   description: "lcms-landcover-2010",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2011,
//   description: "lcms-landcover-2011",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2012,
//   description: "lcms-landcover-2012",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2013,
//   description: "lcms-landcover-2013",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2014,
//   description: "lcms-landcover-2014",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2015,
//   description: "lcms-landcover-2015",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2016,
//   description: "lcms-landcover-2016",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2017,
//   description: "lcms-landcover-2017",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2018,
//   description: "lcms-landcover-2018",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: landcover_ca_2019,
//   description: "lcms-landcover-2019",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: landcover_ca_2020,
//   description: "lcms-landcover-2020",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// // Export LCMS Change layers for each year
// Export.image.toDrive({
//   image: lcms_change_ca_2003,
//   description: "lcms_change-2003",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2004,
//   description: "lcms_change-2004",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2005,
//   description: "lcms_change-2005",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2006,
//   description: "lcms_change-2006",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2007,
//   description: "lcms_change-2007",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2008,
//   description: "lcms_change-2008",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2009,
//   description: "lcms_change-2009",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2010,
//   description: "lcms_change-2010",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2011,
//   description: "lcms_change-2011",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2012,
//   description: "lcms_change-2012",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2013,
//   description: "lcms_change-2013",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2014,
//   description: "lcms_change-2014",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2015,
//   description: "lcms_change-2015",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2016,
//   description: "lcms_change-2016",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2017,
//   description: "lcms_change-2017",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2018,
//   description: "lcms_change-2018",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: lcms_change_ca_2019,
//   description: "lcms_change-2019",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: lcms_change_ca_2020,
//   description: "lcms_change-2020",
//   folder: "ee",
//   region: export_geo,
//   scale: lcms_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// // Export NDVI composites
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2003.multiply(10000).toInt(),
//   description: "ndvi-2003",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2004.multiply(10000).toInt(),
//   description: "ndvi-2004",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2005.multiply(10000).toInt(),
//   description: "ndvi-2005",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2006.multiply(10000).toInt(),
//   description: "ndvi-2006",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2007.multiply(10000).toInt(),
//   description: "ndvi-2007",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2008.multiply(10000).toInt(),
//   description: "ndvi-2008",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2009.multiply(10000).toInt(),
//   description: "ndvi-2009",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2010.multiply(10000).toInt(),
//   description: "ndvi-2010",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2011.multiply(10000).toInt(),
//   description: "ndvi-2011",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2012.multiply(10000).toInt(),
//   description: "ndvi-2012",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2013.multiply(10000).toInt(),
//   description: "ndvi-2013",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2014.multiply(10000).toInt(),
//   description: "ndvi-2014",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2015.multiply(10000).toInt(),
//   description: "ndvi-2015",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2016.multiply(10000).toInt(),
//   description: "ndvi-2016",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: prefire_ndvi_composite_2017.multiply(10000).toInt(),
//   description: "ndvi-2017",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: prefire_ndvi_composite_2018.multiply(10000).toInt(),
//   description: "ndvi-2018",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: prefire_ndvi_composite_2019.multiply(10000).toInt(),
//   description: "ndvi-2019",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: prefire_ndvi_composite_2020.multiply(10000).toInt(),
//   description: "ndvi-2020",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export Forest Structure Rumple index
// Export.image.toDrive({
//   image: veg_structure_rumple_2003.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2003",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2004.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2004",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2005.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2005",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2006.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2006",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2007.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2007",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2008.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2008",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2009.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2009",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2010.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2010",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2011.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2011",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2012.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2012",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2013.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2013",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2014.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2014",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2015.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2015",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2016.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2016",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });
// Export.image.toDrive({
//   image: veg_structure_rumple_2017.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2017",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: veg_structure_rumple_2018.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2018",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: veg_structure_rumple_2019.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2019",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: veg_structure_rumple_2020.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2020",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.image.toDrive({
//   image: veg_structure_rumple_2021.multiply(10000).toInt(),
//   description: "veg-structure-rumple-2021",
//   folder: "ee",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10,
//   fileFormat: "GeoTIFF"
// });

// Export.table.toDrive({
//   collection: grip4_ca,
//   description: 'grip4-roads',
//   folder: 'ee',
//   fileFormat: 'geoJSON'
// });



// Export some computationally intense images to assets

// Export terrain rumple index
// Export.image.toAsset({
//   image: rumple_index,
//   description: "terrain-rumple-index-california",
//   region: export_geo,
//   scale: dem_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export cropped road network
// Export.table.toAsset({
//   collection: grip4_ca,
//   description: 'grip4-roads-california'
// });


// // Export NDVI composites
// Export.image.toAsset({
//   image: prefire_ndvi_composite_2003,
//   description: "ndvi-summer-california-2003",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2004,
//   description: "ndvi-summer-california-2004",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2005,
//   description: "ndvi-summer-california-2005",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2006,
//   description: "ndvi-summer-california-2006",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2007,
//   description: "ndvi-summer-california-2007",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2008,
//   description: "ndvi-summer-california-2008",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2009,
//   description: "ndvi-summer-california-2009",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2010,
//   description: "ndvi-summer-california-2010",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2011,
//   description: "ndvi-summer-california-2011",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2012,
//   description: "ndvi-summer-california-2012",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2013,
//   description: "ndvi-summer-california-2013",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2014,
//   description: "ndvi-summer-california-2014",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2015,
//   description: "ndvi-summer-california-2015",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2016,
//   description: "ndvi-summer-california-2016",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2017,
//   description: "ndvi-summer-california-2017",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2018,
//   description: "ndvi-summer-california-2018",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2019,
//   description: "ndvi-summer-california-2019",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2020,
//   description: "ndvi-summer-california-2020",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: prefire_ndvi_composite_2021,
//   description: "ndvi-summer-california-2021",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// // Export vegetation structure rumple index
// Export.image.toAsset({
//   image: veg_structure_rumple_2003,
//   description: "veg-structure-rumple-california-2003",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2004,
//   description: "veg-structure-rumple-california-2004",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2005,
//   description: "veg-structure-rumple-california-2005",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2006,
//   description: "veg-structure-rumple-california-2006",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2007,
//   description: "veg-structure-rumple-california-2007",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2008,
//   description: "veg-structure-rumple-california-2008",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2009,
//   description: "veg-structure-rumple-california-2009",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2010,
//   description: "veg-structure-rumple-california-2010",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2011,
//   description: "veg-structure-rumple-california-2011",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2012,
//   description: "veg-structure-rumple-california-2012",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2013,
//   description: "veg-structure-rumple-california-2013",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2014,
//   description: "veg-structure-rumple-california-2014",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2015,
//   description: "veg-structure-rumple-california-2015",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2016,
//   description: "veg-structure-rumple-california-2016",
  
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
  
// });
// Export.image.toAsset({
//   image: veg_structure_rumple_2017,
//   description: "veg-structure-rumple-california-2017",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2018,
//   description: "veg-structure-rumple-california-2018",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2019,
//   description: "veg-structure-rumple-california-2019",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2020,
//   description: "veg-structure-rumple-california-2020",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

// Export.image.toAsset({
//   image: veg_structure_rumple_2021,
//   description: "veg-structure-rumple-california-2021",
//   region: export_geo,
//   scale: landsat_export_scale,
//   crs: proj,
//   maxPixels: 1e10
// });

