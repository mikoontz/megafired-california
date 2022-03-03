# Categories of proportional landcover
# csp_ergo_landforms
# lcms_landcover
# lcms_landuse
# lcms_change

# CSP ERGO Landforms based on 10m NED DEM
csp_ergo_landforms_desc <-
  tribble(~value, ~color,	~description, 
          "11", "#141414", "Peak/ridge (warm)",
          "12", "#383838", "Peak/ridge",
          "13", "#808080"," Peak/ridge (cool)",
          "14", "#EBEB8F", "Mountain/divide",
          "15", "#F7D311", "Cliff",
          "21", "#AA0000", "Upper slope (warm)",
          "22", "#D89382", "Upper slope",
          "23", "#DDC9C9", "Upper slope (cool)",
          "24", "#DCCDCE", "Upper slope (flat)",
          "31", "#1C6330", "Lower slope (warm)",
          "32", "#68AA63", "Lower slope",
          "33", "#B5C98E", "Lower slope (cool)",
          "34", "#E1F0E5", "Lower slope (flat)",
          "41", "#a975ba", "Valley",
          "42", "#6f198c", "Valley (narrow)") 

# Landscape Change Monitoring System: Change
lcms_change_desc <-
  tribble(value, 	~color, ~description,
          "01", "#3d4551", "Stable",
          "02", "#f39268", "Slow Loss",
          "03", "#d54309", "Fast Loss",
          "04", "#00a398", "Gain",
          "05", "#1B1716", "Non-Processing Area Mask") 

# Landscape Change Monitoring System: Landcover
lcms_landcover_desc <-
  tribble(~value, ~color, ~description,
          "01",	"#005e00", "Trees",
          "02",	"#008000", "Tall Shrubs & Trees Mix (SEAK Only)",
          "03",	"#00cc00", "Shrubs & Trees Mix",
          "04",	"#b3ff1a", "Grass/Forb/Herb & Trees Mix",
          "05",	"#99ff99", "Barren & Trees Mix",
          "06",	"#b30088", "Tall Shrubs (SEAK Only)",
          "07",	"#e68a00", "Shrubs",
          "08",	"#ffad33", "Grass/Forb/Herb & Shrubs Mix",
          "09",	"#ffe0b3", "Barren & Shrubs Mix",
          "10",	"#ffff00", "Grass/Forb/Herb",
          "11",	"#AA7700", "Barren & Grass/Forb/Herb Mix",
          "12",	"#d3bf9b", "Barren or Impervious",
          "13",	"#ffffff", "Snow or Ice",
          "14",	"#4780f3", "Water",
          "15",	"#1B1716", "Non-Processing Area Mask")

# Landscape Change Monitoring System: Landuse
lcms_landuse_desc <-
  tribble(~value, ~color, ~description,
          "01", "#efff6b", "Agriculture",
          "02", "#ff2ff8", "Developed",
          "03", "#1b9d0c", "Forest",
          "04", "#97ffff", "Non-Forest Wetland",
          "05", "#a1a1a1", "Other",
          "06", "#c2b34a", "Rangeland or Pasture",
          "07", "#1B1716", "Non-Processing Area Mask") 
