library(dplyr)

# The land-cover map for 2015 consists of 10 land-cover
# classes (1: coniferous forest, 2: mixed forest, 3:
# deciduous forest, 4: barren, 5: rangeland, 6:
# cropland, 7: built-up, 8: wetlands, 9: water, 10:
# snow and ice).
# Land-cover change maps exist for cropland (cropland =
# 1, non-cropland = 0) and forest (forest = 1,
# non-forest = 0) for 6 time steps (1987, 1995, 2000,
# 2005, 2010, 2015). All Caucasus land-cover and
# land-cover change maps are stored as GeoTIFF format
# at 30 meter spatial resolution and projected to
# Lambert Azimuthal Equal Area (Datum: WGS 1984,
# latitude of origin: 42.5, central meridian: 43.5).

krasnodar_res_2015 <-
  tibble::tribble(
    ~Value, ~PixelCount, ~AreaM,
    1, 1621411, 1459269900,
    2, 2588726, 2329853400,
    3, 14144751, 12730275900,
    4, 1384821, 1246338900,
    5, 9642042, 8677837800,
    6, 18519150, 16667235000,
    7, 197450, 177705000,
    8, 1256681, 1131012900,
    9, 486126, 437513400,
    10, 112299, 101069100
  )

krasnodar_res_2015 |>
  mutate(
    land_cover = case_when(
      Value == 1 ~ "Coniferous forest",
      Value == 2 ~ "Mixed forest",
      Value == 3 ~ "Deciduous forest",
      Value == 4 ~ "Barren",
      Value == 5 ~ "Rangeland",
      Value == 6 ~ "Cropland",
      Value == 7 ~ "Built-up",
      Value == 8 ~ "Wetlands",
      Value == 9 ~ "Water",
      Value == 10 ~ "Snow and ice"
    )
  ) |>
  mutate(
    land_cover_groups =
      case_when(
        stringr::str_detect(land_cover, "forest") ~ "Forest",
        TRUE ~ land_cover
      )
  ) |>
  group_by(land_cover_groups) |>
  dplyr::summarise(
    PixelCount = sum(PixelCount),
    AreaM = sum(AreaM)
  ) |>
  ungroup() |>
  dplyr::mutate(
    pct = 100 * PixelCount / sum(PixelCount),
    area_pct = 100 * AreaM / sum(AreaM)
  ) |>
  arrange(pct)
