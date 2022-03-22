# split fires by Jepson ecoregion

library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyr)
library(terra)
library(fasterize)

resolve <- 
  sf::st_read("data/raw/resolve-ecoregions-2017_california.geojson") %>% 
  sf::st_transform(3310)

# Descriptions are here: https://ucjeps.berkeley.edu/eflora/geography.html
jepson <- 
  sf::st_read("data/raw/jepcodes-v7.kml") %>% 
  dplyr::select(-Description) %>% 
  sf::st_transform(3310) %>% 
  dplyr::rename(code = Name)

jepson_desc <- dplyr::tribble(~name, ~code, ~description,
"North Coast Subregion", "NCo", "This subregion extends along the Pacific Coast the full length of the NW, from the Oregon border south to Bodega Bay. It is a strip of land of variable width that supports truly coastal vegetation, including predominantly coastal prairie, along with coastal marsh, coastal scrub, closed-cone-pine/cypress forest, and grand-fir/Sitka-spruce forest. In some places (e.g., the northern Mendocino coast), the NCo is reduced to coastal bluffs.",

"Klamath Ranges Subregion", "KR", "The California portion of this geologically old and distinct, serpentine-rich subregion is bounded to the north by Oregon and in the northwest by the coastal vegetation of the NCo. Its southwestern and southeastern boundaries abut the North Coast Ranges Subregion (NCoR). In the southwestern KR, the boundary with the NCoR has a geological basis, with the mostly sedimentary Franciscan Complex of the NCoR faulted against the older, plutonic and metamorphic rocks of the KR. This fault boundary generally coincides with the northwest-flowing Klamath and South Fork of the Trinity rivers. The transition in forest types across the boundary between the KR and NCoR is gradual, with the KR containing forests of globally exceptional conifer diversity.

In the east, the boundary between the predominantly metamorphic KR and the volcanic CaR lies east and north of Shasta Lake, incorporating the McCloud and Hosselkus limestone formations. In the southeast, the boundary generally excludes the chaparral and foothill-pine/blue-oak woodland vegetation of the Inner North Coast Ranges District (NCoRI) in southwestern Shasta and northwestern Tehama counties.

The KR includes the Marble, Salmon, Scott, Scott Bar, Siskiyou, and Trinity mountains, the Trinity Alps, and Mount Eddy. Red Mountain, near the point where Trinity, Shasta, and Tehama counties meet, is one of the southernmost peaks in the KR that exceeds 1500 m.",

"Outer North Coast Ranges District", "NCoRO", "This district, the largest in the NCoR, is characterized by very high rainfall, as well as by redwood, mixed-evergreen, and mixed-hardwood forests. Notable mountain peaks include Mount Lassic, Grouse Mountain, and Horse Mountain, all of which are exceeded in elevation by peaks to the east in the High North Coast Ranges District (NCoRH).",

"High North Coast Ranges District", "NCoRH", "This district is characterized by heavy snow cover, as well as by montane and subalpine conifer forests, treeless high peaks, and floristic similarities to the SNH. Major peaks of the NCoRH all rise above 1500 m (most are above 2000 m), and extend from South Fork Mountain in Humboldt County southeast to the Yolla Bolly Mountains, and from there south to Pine Mountain in Lake County. Somewhat lower, more western, and more isolated peaks similar in vegetation to South Fork Mountain (e.g., Mount Lassic, Grouse Mountain, Horse Mountain) are included instead in the NCoRO. Snow Mountain and Mount Sanhedrin are in the NCoRH.",

"Inner North Coast Ranges District", "NCoRI", "This district is characterized by low rainfall and hot, dry summers, as well as by chaparral and pine/oak woodland. It extends from the Anderson area in southwestern Shasta County, southward along the east slope of the North Coast Ranges, with a conspicuous westward bulge near the southern end of the NCoRH, to an area west of the Russian River (from north of Ukiah south to Mount St. Helena). Serpentine is widespread in the NCoR, but especially common in this district.",

"Cascade Range Foothills Subregion", "CaRF", "This subregion, in the southwestern part of CaR, is characterized by chaparral and blue-oak/foothill-pine woodland at about 100–500 m in elevation. The northern CaRF and northern NCoRI combined with the southern Kr form a continuous horeshoe-shaped band of similar foothill vegetation along the northern margin of the GV.",

"High Cascade Range Subregion", "CaRH", "This subregion (generally above 500 m) comprises the remainder of the CaR and is characterized by ponderosa-pine, montane fir/pine, and lodgepole-pine forests, with treeless alpine vegetation on Mount Shasta and Lassen Peak.",

"Sierra Nevada Foothills Subregion", "SNF", "This subregion comprises a lower, mostly narrow, north-south strip in the westernmost one-third to one-fifth of the SN, with the GV to the west, the SNH or the Mojave Desert Subregion (DMoj) to the east, and the Teh to the south. The upper elevational limit of the SNF is approximately 1500 m (in the north) to 1000 m (in the south) except near Lake Isabella, where the upper limit is approximately 1500 m.

Throughout most of its area, the SNF is characterized by blue-oak/foothill-pine woodlands (versus ponderosa-pine forest of higher elevations in the SNH) and chaparral, with some serpentine. It is best differentiated from the SNH and GV by vegetation, as opposed to climatic, topographic, geologic, or other considerations. The SNF is divided into northern, central, and southern districts, as discussed under the SN, and as defined under each.",

"Northern Sierra Nevada Foothills District", "nSNF", "This district meets the CaRF to the north (northwest of Oroville) and is bounded more or less arbitrarily in the south, where it meets the c SNF, by the Stanislaus River, which corresponds to the Calaveras-Tuolumne county line. Oroville, Auburn, and Placerville are all well within the n SNF, whereas Grass Valley, at about 800 m, is near the border with the n SNH.",

"Central Sierra Nevada Foothills District", "cSNF", "This district meets the n SNF to the north and is bounded in the south by the divide (in Fresno County) between the San Joaquin and Kings river drainages, which is approximated by Highway 168. Sonora, Incline, and Mariposa are all within the c SNF.",

"Southern Sierra Nevada Foothills District", "sSNF", "This district meets the c SNF to the northwest and the Teh to the south, at Highway 58 through Tehachapi Pass, which approximates the division between the Tehachapi Creek and Cache Creek drainages. The district runs the width of the SN at its southern end (i.e., the SNH does not extend all the way to the southern end of the SN). Like the Teh, the s SNF is complex, with gradual transitions into surrounding areas of the GV, s SNH, and DMoj.",

"High Sierra Nevada Subregion", "SNH", "This large subregion is elongate in a north-south direction, extending from Lassen and Plumas counties in the north to Kern County in the south, and is bounded by the SNF to the west and the Great Basin Province (GB) and Desert Province (D), including parts of Nevada, to the east. It is vegetationally complex, with forests of ponderosa pine, white fir, and giant sequoia in lower montane areas, forests of red fir, Jeffrey pine, and lodgepole pine in upper montane areas, forests of mountain hemlock and whitebark pine in subalpine areas, and treeless alpine areas at the highest elevations (about 3000–4400+ m).

The long border between the SNH to the west and the GB and D to the east, extending more than half the length of California, is in places difficult to define (see the CA-FP, above). The SNH is divided (as is the SNF) into northern, central, and southern districts, as discussed under the SN.",

"Northern High Sierra Nevada District", "nSNH", "This district in the north meets the CaRH of the CA-FP and the MP of the GB; the boundary with the CaRH more or less coincides with the North Fork of the Feather River, from northeastern Butte County to southwestern Lassen County. In the south, the border with the c SNH approximately follows the Calaveras-Tuolumne, Alpine-Tuolumne, and Alpine-Mono county lines to the border with the GB. Quincy, Downieville, Truckee, and Markleeville are within the n SNH.",

"Central High Sierra Nevada District", "cSNH", "This district meets the n SNH to the north, as defined above. The southern boundary, west of the Sierran crest, is the divide between the San Joaquin and Kings river drainages (as it is in the c SNF). This divide winds to the south in eastern Fresno County, reaching the Sierran crest along the Goddard Divide, near Mount Darwin (4200 m). East of the Sierran crest, the boundary with the s SNH follows Bishop Creek, down to the border with the GB at about 2000 m. Yosemite National Park and Mammoth Lakes are within the c SNH.",

"Southern High Sierra Nevada District", "sSNH", "This district meets the c SNH to the north-northwest and the s SNF to the west and south. All but the northern tip of Kings Canyon National Park and all of Sequoia National Park are included in the s SNH. In the northern part of this district are the highest mountains in California, including Mount Whitney at 4000+ m. In this area, peaks average about 3000 m, while in the southernmost part of the district this figure is 2000–2500 m. The boundary with the s SNF in the south, defined by vegetation, is convoluted and relatively indistinct. To the east, the s SNH meets the DMoj, in the south, and the GB, in the north, at the transition between montane and desert vegetation (as discussed under the CA-FP). The higher mountains of the southern part of this district (e.g., Piute Mountains, , Scodie Mountains, Breckenridge Mountain) support yellow or pinyon pines, but not the oak/pine woodland, chaparral, or desert scrub of neighboring geographic units.",

"Tehachapi Mountain Area Subregion", "Teh", "This small foothill and montane subregion, in which elevations rarely exceed 2000 m, has floristic elements of all surrounding geographic units. Highway 58 through Tehachapi Pass constitutes the boundary between this subregion and the s SNF. In the west, the subregion is bounded by the GV, where included foothill and mixed-woodland vegetation meets grassland and agricultural land. To the southwest, the subregion ends at Tejon Pass on Interstate 5, where it meets the northern part of the Western Transverse Range District (WTR). The eastern-southeastern boundary with the D is indistinct, as discussed under the CA-FP, with chaparral or pinyon/juniper woodland on the Teh side and creosote-bush scrub on the D side.",

"Sacramento Valley Subregion", "ScV", "This subregion comprises the northern, smaller, wetter, cooler area of the GV, extending from near Red Bluff in Tehama County to the salt marshes of Suisun Slough in southwestern Solano County. The boundary between the ScV and the San Joaquin Valley Subregion (SnJV) follows the northern borders of Contra Costa and San Joaquin counties, which approximately bisect 'the delta' area of the Sacramento and San Joaquin rivers.",

"San Joaquin Valley Subregion", "SnJV", "This subregion comprises the southern, larger, drier, hotter area of the GV; its northern limits are defined under the ScV, while its other boundaries equal those of the GV. Islands of higher (ca. 800 m), moister habitats in the Temblor Range and on associated ridges, located geographically in the sw SnJV, are included instead in the Inner South Coast Ranges District (SCoRI) of CW. The Caliente Range is also in the SCoRI based on floristics and topography.  These and other eastern ranges of the SCoR flank western extensions of the SnJV, such as the Carrizo Plain and San Juan Valley in eastern San Luis Obispo County, and Cuyama Valley in southernmost San Luis Obispo and northernmost Santa Barbara counties. Further north, the Livermore Valley (Alameda County) is another western extension of SnJV.",

"Central Coast Subregion", "CCo", "This subregion extends along the Pacific Coast (and San Francisco Bay) the full length of the CW, from near Bodega Bay in the north to Point Conception in the south. Like the NCo in the NW, the CCo is variable in width and coastal vegetation predominates. In places (e.g., the southern Monterey coast), the CCo is reduced to coastal bluffs. Salt marshes and coastal prairie occur around the San Francisco Bay; coastal-sage scrub is prevalent in the south. In the southern part of the CCo, from Morro Bay to San Luis Obispo, the Seven Sisters support chaparral and other non-coastal vegetation.",

"San Francisco Bay Area Subregion", "SnFrB", "This subregion occupies the northern one-third of the CW, east of the CCo. It is reasonably well defined physiographically, by features such as Mount Tamalpais, the Santa Cruz Mountains, and the northern Diablo Range, including Mount Diablo and Mount Hamilton. The southern boundary is somewhat arbitrary, following Highways 156 and 152 from the CCo east of Castroville, through Hollister and Pacheco Pass, to the GV near San Luis Reservoir. The subregion is less well defined vegetationally, encompassing a diversity of vegetation types, from very wet redwood forest to dry oak/pine woodland and chaparral.",

"South Coast Ranges Subregion", "SCoR", "This subregion is bounded by the SnFrB to the north (boundary defined under the SnFrB), CCo to the west, SW to the south, and SnJV to the east. It is divided into two districts.",

"Outer South Coast Ranges District", "SCoRO", "The boundary between this district and the Inner South Coast Ranges District (SCoRI) to the east runs along the Salinas River (approximated by Highway 101), from near Salinas south to about San Miguel in northern San Luis Obispo County, and from there up the Estrella River to the western edge of the SnJV near Shandon. The SCoRO includes the Sierra de Salinas, Santa Lucia Range, and San Rafael Mountains, and extends to as far south as the boundary between the CW and SW, which corresponds to the crest of the Santa Ynez Mountains and Mono Creek. Near the coast, there are small stands of redwood and mixed-evergreen forests in the north, and oak forests in the south, with pockets of montane conifer forest at the highest elevations. Hotter, drier, more inland slopes support primarily blue-oak/foothill-pine woodland and chaparral.",

"Inner South Coast Ranges District", "SCoRI", "Located east of the SCoRO, this district includes the southern Diablo Range from Hollister and Pacheco Pass south to (and including) San Benito Mountain, the Gabilan Range, Cholame Hills, and the higher elevations of the Temblor Range, Caliente Range, and associated ridges (isolated within the southern part of the SnJV). The SCoRI supports a mosaic of blue-oak/foothill-pine woodland, juniper woodland, chaparral, and elements of desert scrub.",

"South Coast Subregion", "SCo", "This subregion extends along the Pacific Coast, from Point Conception of the CCo (CW) to Mexico. It is comparable to the NCo and CCo of the NW and CW regions, respectively, but is hotter and drier and extends much farther inland — to San Gorgonio Pass at Banning, which marks the boundary between the CA-FP and D. Coastal-sage scrub and chaparral vegetation predominated in the SCo before urbanization.",

"Channel Islands Subregion", "ChI", "The eight major islands in the Pacific Ocean off the coast of southern California are floristically similar to the SCo, but include enough endemics to justify recognition of the ChI as a separate geographic unit. The subregion is divided into two districts. Counties are indicated below (but not on the map) for each of the eight major islands because information on this subject is commonly incorrect and/or not readily verified. Santa Barbara Island was originally in Santa Barbara County, placed in Ventura County for a period, and is presently in Santa Barbara County.",

"Northern Channel Islands District", "nChI", "This district includes the islands of San Miguel (Santa Barbara County), Santa Rosa (Santa Barbara County), Santa Cruz (Santa Barbara County), and Anacapa (Ventura County), which are separated from the mainland by the Santa Barbara Channel. These islands are geologically related to (and probably represent the westernmost peaks of) the Santa Monica Mountains, located in the southern part of the Western Transverse Ranges District (WTR).",

"Southern Channel Islands District", "sChI", "This district includes the islands of Santa Barbara (Santa Barbara County), Santa Catalina (Los Angeles County), San Clemente (Los Angeles County), and San Nicolas (Ventura County). These islands are geologically and floristically more isolated and more diverse among themselves than those of the northern group, probably in part because they were not as readily colonized from the mainland during periods of lowered sea levels that accompanied various glaciations.",

"Transverse Ranges Subregion", "TR", "This subregion, the northernmost in the SW, includes mountain ranges that are oriented in an east-west direction. The TR shares nearly all of its southern boundary with the SCo; in the easternmost extreme of this boundary the TR is separated from the Peninsular Ranges Subregion (PR) by San Gorgonio Pass (Interstate 10), which lies between the San Bernardino Mountains (TR) to the north and the San Jacinto Mountains (in PR) to the south. San Gorgonio Pass, near Banning, also marks the division between the SCo (CA-FP) to the west and the D to the east.

The TR is characterized at lower elevations by chaparral and at higher elevations by oak forest and dry montane forests of white fir, incense cedar, or Jeffrey, sugar, or lodgepole pines. The boundary between the TR and D lies between these communities and desert vegetation that includes Joshua tree or creosote bush on the D side. Some high peaks in the TR extend above treeline. The TR is divided into three districts that are progressively higher, hotter, and drier eastward.",

"Western Transverse Ranges District", "WTR", "This district meets the SN, GV, and CW to the north, the SCo to the south (a narrow strip of which separates the WTR from the Pacific Ocean), and the D and the San Gabriel Mountains District (SnGb) to the east. It includes Mount Pinos (at 2700 m, the highest point in the WTR), the Santa Ynez Mountains (south of its crest west of Mono Creek), Sierra Pelona, and the Topatopa, Santa Susana, Santa Monica, and Liebre mountains. At the north end of the San Fernando Valley, a topographic boundary with the SnGb follows Interstate 5 north to the Santa Clara River, and from there northeast through Soledad Canyon and Soledad Pass to the boundary between the WTR and D south of Palmdale.",

"San Gabriel Mountains District", "SnGb", "This district is a topographically well-defined mountain range situated northeast of Los Angeles. It is bounded by the D to the north and northeast, the WTR to the northwest and west, the SCo to the south, and the San Bernardino Mountains District (SnBr) to the east. The SnGb is separated from the SnBr by the northwest-southeast oriented Cajon Canyon, which is occupied by Highway 138 and Interstate 15. Mount San Antonio ('Old Baldy'), straddling the Los Angeles-San Bernardino county line at 3070 m, is the highest point in the SnGb. It supports alpine taxa near its summit.",

"San Bernardino Mountains District", "SnBr", "This is a topographically well-defined mountain range, east of the SnGb. This district is adjacent to the D on its north, east, and southeast boundaries, the SCo to the southwest, and the San Jacinto Mountains District (SnJt) of the PR to the south, from which the SnBr is separated by San Gorgonio Pass (D). The highest point in the SnBr is San Gorgonio Mountain (3500 m), which has the most well-developed alpine vegetation in California south of the SN. The Little San Bernardino Mountains to the southeast of the SnBr are here considered part of the Desert Mountains Subregion (DMtns) because the vegetation is more similar to the D than to the SnBr.",

"Peninsular Ranges Subregion", "PR", "This subregion occupies approximately the southeastern one-third of the SW. It includes Mount Palomar, as well as the Santa Ana, Cuyamaca, Santa Rosa, Laguna, Jacumba, Volcan, and San Jacinto mountains. The last range comprises its own district within the PR.",

"San Jacinto Mountains District", "SnJt", "This district in the PR is an area with a high level of local endemism. The San Jacinto Mountains include the highest elevations in the PR, with San Jacinto Peak at about 3300 m. The Santa Rosa Mountains to the southeast, with elevations to 2650 m, are the only other range in the PR that supports well-developed montane to subalpine forests.",

"Great Basin Province", "GB", "The Great Basin Province (GB) lies to the east of the CA-FP in the northern two-thirds of California and meets the D at its southern margin. The boundary with the CA-FP is described above; it follows the high eastern margin of the CaR and SN. The boundary with the D at its northern extent is the transition from sagebrush steppe or pinyon/juniper woodland on the GB side to creosote-bush scrub on the D side. Deep Springs and Fish Lake valleys are in the GB, Eureka and Saline valleys are in the D. Southward, the mixed vegetation of the Owens Valley is included in the GB. This province is characterized by low rainfall, hot to very hot summers, and relatively cold winters compared to much of the D. It is divided into two regions and two subregions.",

"Modoc Plateau Region", "MP", "This region, entirely north of Lake Tahoe, is a high plateau (mostly about 1300–1800 m) in the northeastern corner of California, occupying most of Modoc and Lassen counties and parts of Plumas, Shasta, Sierra, and Siskiyou counties. The MP is characterized primarily by juniper woodland and sagebrush steppe, but also has extensive areas of ponderosa-pine and Jeffrey-pine forests, and lesser areas of montane pine/fir forest. Substrates are volcanic, with faulted lava flows predominating over cones (see the CaR, above).",

"Warner Mountains Subregion", "Wrn", "The Warner Mountains, a faulted volcanic range situated mostly in eastern Modoc County, is the most outstanding topographic feature of the MP. Its highest point is Eagle Peak, which exceeds 3000 m. The Wrn is recognized as a distinct subregion because it supports a unique flora that includes an alpine component at the higher elevations.",

"East of the Sierra Nevada Region", "SNE", "This region, entirely south of Lake Tahoe, has a wide elevational range, from Owens Lake at 1100 m to White Mountain Peak at 4330 m. The part of the SNE excluding the White-Inyo Mountains Subregion (W&I) supports primarily a mosaic of sagebrush steppe, pinyon/juniper woodland, and cottonwood-dominated riparian vegetation. There are also extensive areas of Jeffrey-pine forest in the Mono Craters area, subalpine fir/pine forest on Glass Mountain (3400 m), and alpine vegetation at the top of the Sweetwater Mountains (3550 m). The SNE extends along the eastern edge of the SN to the southern limit of Owens Valley and the W&I, where there is a gradual transition to the DMoj, with creosote bush and white bur-sage dominated scrub vegetation. To the east of the junction of the W&I at Westgard Pass lies a low (1500–2000 m) outlier of the SNE that includes the Deep Springs and Fish Lake valleys.

The boundary between the SNE and CA-FP along the eastern edge of the SN is generally defined by an indefinite break between either upper montane (red-fir/lodgepole-pine) forest or Jeffrey-pine forest on the CA-FP side and either pinyon/juniper woodland or sagebrush scrub on the SNE side. As noted above, there is also Jeffrey-pine forest in the SNE (e.g., Mono Craters area). The boundary between the SNE and CA-FP is west of Highway 395 from 1800 m (south of Bishop) to 2000 m (north of Bishop).",

"White and Inyo Mountains Subregion", "W&I", "The White-Inyo Range (W&I) is considered a separate subregion because it supports subalpine bristlecone-pine and limber-pine woodlands as well as unique, treeless, alpine vegetation (White Mountain Peak 4330 m; Inyo and Waucoba peaks both around 3400 m).",

"Mojave Desert Region", "DMoj", "This region, occupying the northern two-thirds of the D, exhibits greater temperature ranges and more extreme elevational relief than the DSon to the south. Joshua tree and Mojave yucca are conspicuous, widespread members of DMoj vegetation that are absent from the DSon.",

"Desert Mountains Subregion", "DMtns", "Although the entire DMoj is a series of mountains and intervening (often wide) valleys, some ranges reach sufficient elevation (generally above 1700 m) to support pinyon/juniper woodland vegetation and are therefore recognized as a distinct subregion, the DMtns. These high ranges include, but are not limited to, the Last Chance Range, Grapevine Mountains, Panamint Range, Coso Range, Argus Range, Kingston Range, Clark Mountain Range, Ivanpah Mountains, New York Mountains, Providence Mountains, Granite Mountains, Old Woman Mountains, and Little San Bernardino Mountains (discussed below). The Panamint, Kingston, and Clark Mountain ranges and the New York Mountains also support white fir or limber pine at their highest elevations. The DMtns have unique elements but also overlap floristically with pinyon/juniper woodland vegetation of the adjacent CA-FP. Some of the eastern DMtns support taxa that occur more widely, in D or GB outside of the state, but are otherwise unknown in California.

The Little San Bernardino Mountains, across Morongo and Yucca valleys from the SnBr (of TR, CA-FP) and mostly included in Joshua Tree National Park, are included as part of the DMtns because the vegetation in this range is more similar to the D than to the SnBr, as noted above.",

"Sonoran Desert Region", "DSon", "This region, the California portion of which is also known as the Colorado Desert, occupies the southern one-third of the D, south of the DMoj. The physiographic line separating the two desert regions is not always clear, but overall the DSon is lower, warmer, and somewhat distinct floristically. Conspicuous members of the flora in the DSon that are absent from the DMoj or confined to the southeastern limits of the DMoj include blue palo verde, ocotillo, chuparosa, and ironwood.

The approximate boundary between the DMoj and DSon, from west to east, is along the south edge of the Little San Bernardino, Cottonwood, and Eagle mountains (all in the DMoj), then north along the eastern edge of the Coxcomb Mountains (DMoj) and around the Old Woman, Turtle, and Chemehuevi mountains (all in the DMoj) to the Colorado River. The Chuckwalla and Whipple mountains are in the DSon.")

jepson <-
  jepson %>% 
  dplyr::left_join(jepson_desc) %>% 
  dplyr::select(-description)

plot(jepson[, "name"])

forest_fire_jepson_codes <- c("nSNF", "cSNF", "sSNF", "nSNH", "cSNH", "sSNH", "CaRH", "CaRF", "NCoRH", "Teh", "SNE", "MP", "DMtns", "W&I", "KR")
socal_fire_jepson_codes <- c("WTR", "SCo", "PR", "DSon", "CCo", "SnFrB", "DMoj", "sChl")
valley_fire_jepson_codes <- c("SnJV", "ScV")

# read in fire data
fires <- 
  data.table::fread("data/out/analysis-ready/FIRED-daily-scale-drivers_california.csv") %>% 
  sf::st_as_sf(coords = c("x_3310", "y_3310"), crs = 3310, remove = FALSE)

events <-
  sf::st_read("data/out/fired_events_ca_ewe_rank.gpkg") %>% 
  sf::st_transform(3310) %>% 
  sf::st_centroid() %>% 
  dplyr::rename(geometry = geom) %>%
  dplyr::mutate(x_3310 = sf::st_coordinates(.)[, "X"],
                y_3310 = sf::st_coordinates(.)[, "Y"]) %>% 
  dplyr::filter(tot_hect >= 120) %>% 
  dplyr::filter(ig_date >= lubridate::ymd("2003-01-01") & ig_date <= lubridate::ymd("2020-12-31"))


fires <- 
  fires %>% 
  sf::st_join(y = jepson) %>% 
  dplyr::mutate(region = dplyr::case_when(code %in% forest_fire_jepson_codes ~ "forest",
                                          code %in% socal_fire_jepson_codes ~ "socal",
                                          code %in% valley_fire_jepson_codes ~ "valley")) %>% 
  sf::st_join(resolve)


fires %>% 
  sf::st_drop_geometry() %>% 
  group_by(region, megafire) %>% 
  tally() %>% 
  tidyr::pivot_wider(names_from = "megafire", values_from = "n") %>% 
  dplyr::mutate(proportion = megafire / (megafire + `non-megafire`))

fires %>% 
  sf::st_drop_geometry() %>% 
  group_by(BIOME_NAME, megafire) %>% 
  tally() %>% 
  tidyr::pivot_wider(names_from = "megafire", values_from = "n") %>% 
  dplyr::mutate(n = (megafire + `non-megafire`),
                proportion = megafire / n)

fires %>% 
  sf::st_drop_geometry() %>% 
  group_by(ECO_NAME, megafire) %>% 
  tally() %>% 
  tidyr::pivot_wider(names_from = "megafire", values_from = "n", values_fill = 0) %>% 
  dplyr::mutate(n = (megafire + `non-megafire`),
                proportion = megafire / n)

fires <-
  fires %>% 
  sf::st_join(y = resolve)

ca <- USAboundaries::us_states(resolution = "low", states = "California") %>% sf::st_transform(3310)

plot(st_geometry(ca))
plot(jepson[, "code"], add = TRUE)
plot(fires[, "megafire"], add = TRUE, pal = c("red", "black"), pch = 19, cex = 0.5)

library(tmap)

target_fires <- 
  fires %>% 
  dplyr::filter(tot_hect >= 120)

megafire_map <-
  tm_shape(ca) +
  tm_borders() +
  tm_shape(events) +
  tm_dots(col = "megafire", palette = c("red", "black"), title = "Fire type", legend.is.portrait = FALSE, size = 0.1) +
  tm_graticules(alpha = 0.5) +
  tm_layout(legend.outside.position = "bottom", legend.position = c(0, 0), legend.outside = TRUE, legend.outside.size = 0.05)


megafire_map_by_ig_month <-
  megafire_map + 
  tm_facets(by = "ig_month", free.coords = FALSE) +
  tm_layout(asp = 1)

megafire_map
megafire_map_by_ig_month

tmap_save(tm = megafire_map, filename = "figs/megafire-map.png", dpi = 300, width = 180, units = "mm")
tmap_save(tm = megafire_map_by_ig_month, filename = "figs/megafire-map-by-ig-month.png", dpi = 300, width = 180, units = "mm")

r_template <- fasterize::raster(ca, res = c(25000, 25000)) %>% terra::rast()
r_mecdf <- 
  terra::rasterize(x = terra::vect(events), y = r_template, fun = mean, field = "mecdf", na.rm = TRUE) %>% 
  setNames("mecdf")

r_fires <- 
  terra::rasterize(x = terra::vect(events), y = r_template, fun = sum) %>% 
  setNames("n")

r_mecdf_df <- as.data.frame(r_mecdf, xy = TRUE, cells = TRUE)
r_fires_df <- as.data.frame(r_fires, xy = TRUE, cells = TRUE)
r_df <- left_join(r_mecdf_df, r_fires_df)
plot(r_mecdf, col = viridis::viridis(10))

library(ggplot2)
ewe_score_map_gg <-
ggplot(r_df %>% mutate(alpha = ifelse(n > 10, yes = 10, no = n)), aes(x = x, y = y, fill = mecdf, alpha = alpha)) +
  geom_tile() +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_b() +
  labs(x = "Easting (m)",
       y = "Northing (m)")

ggsave(filename = "figs/ewe-score-map.png", width = 180, units = "mm")

r_template <- fasterize::raster(ca, res = c(50000, 50000)) %>% terra::rast()

r_fires <- terra::rasterize(x = terra::vect(events), y = r_template, fun = sum)
plot(r_fires)


r_megafires <- terra::rasterize(x = terra::vect(events[events$megafire == "megafire", ]), y = r_template, fun = sum)

r_nonmegafires <- terra::rasterize(x = terra::vect(target_fires[events$megafire != "megafire", ]), y = r_template,  fun = sum)

plot(r_fires, col = viridis::viridis(10))
plot(r_megafires, col = viridis::viridis(10))
r_prop_mega <- r_megafires / r_fires

plot(r_prop_mega, col = viridis::viridis(100))
plot(r_fires)
plot(r_megafires)
?terra::rasterize
target_fires %>% 
  st_drop_geometry() %>% 
  group_by(ig_month, megafire) %>% 
  summarize(mean_lat = mean(y_3310)) %>% 
  pivot_wider(names_from = "megafire", values_from = "mean_lat")

fires
names(fires)

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

fires_working <-
  fires %>% 
  dplyr::mutate(rumple_index = surf_area / proj_area) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-lcms_landuse_01, -lcms_landuse_02, -lcms_landuse_04, -lcms_landuse_07,
                -lcms_landcover_02, -lcms_landcover_06, -lcms_landcover_02, -lcms_landcover_12, -lcms_landcover_13, -lcms_landcover_14, -lcms_landcover_15, 
                -lcms_change_05,
                -name_frap, -olap_frap, -ig_date, -last_date, -date, -ends_with("rank"), -frp_90, -aoir_max, -aoi_max, -pred_aoi, -act_aoi, -c_area_tm1, -date_frp, -date_aoir, -date_aoi, -e_day_aoir, -e_day_aoi, -event_dur, -model_aoir, -event_modis_lc, -daily_modis_lc,
                -max_wind_speed, -min_wind_speed, -max_rh, -min_rh, -max_temp, -min_temp, -max_soil_water, -min_soil_water, -max_vpd, -min_vpd,
                -ndvi_proj_area, -ndvi_surf_area, -proj_area, -surf_area, -road_length_m,
                -bi, -erc, -fm100, -fm1000, -pdsi, -starts_with("spi"), -starts_with("eddi"), -surf_area_ha, -proj_area_ha)

fires_working
