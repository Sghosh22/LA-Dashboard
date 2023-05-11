






#map is correct color but the legend is listing the incorrect colors




colpal_hbwpk_rs5 = leaflet::colorFactor(
  palette = tazs.shape_hbwpk_rs5_col$Color
  ,levels = levels((tazs.shape_hbwpk_rs5_col$Range)))



colors = tazs.shape_hbwpk_rs5 %>%
  st_drop_geometry() %>%
  select(Range, Color) %>%
  unique()

library(scales)
pal <- rgb(colors$Color)
show_col(pal)

pal <- rgb(ddf$r, ddf$g, ddf$b)
show_col(colors$Color)



# tazs.shape_hbwpk_rs5 =  merge(x = tazs.shape
#                               ,y = hbwpk_rs5, by = c("TAZ"))  %>%
#   st_transform(crs =4326)

ColorPalette <- function(taz){
  df_col_1 = select(taz, Color, Range) %>%
    st_drop_geometry() %>%
    unique()%>%
    mutate(Range = fct_inorder(Range))

  return(df_col_1)

}

colors = tazs.shape_hbwpk_rs5 %>%
  st_drop_geometry() %>%
  select(Range, Color) %>%
  unique()



tazs.shape_hbwpk_rs5_col = ColorPalette(tazs.shape_hbwpk_rs5)

colpal_hbwpk_rs5 = leaflet::colorFactor(
  palette = tazs.shape_hbwpk_rs5_col$Color
  ,levels = levels(tazs.shape_hbwpk_rs5_col$Range)
  )


tazs.shape_hbwpk_rs5 %>%
  filter(Range %in% c())


mapview(tazs.shape_hbwpk_rs5
        ,zcol = 'Range'
        ,col.region = colpal_hbwpk_rs5(tazs.shape_hbwpk_rs5$Range)
        ,layer.name = str_glue("HBW - Production User Benefits (mins)"))

leaflet() %>%
  addPolygons(data = tazs.shape_hbwpk_rs5
              ,fillColor = ~colpal_hbwpk_rs5(tazs.shape_hbwpk_rs5$Range)
              ,group = "id_1"
              ,fillOpacity = .8
              ,color = "black"
              ,popup = tazs.shape_hbwpk_rs5$Range) %>%
  addLegend(pal = colpal_hbwpk_rs5
            ,values = tazs.shape_hbwpk_rs5$Range
            ,group = "id_1"
            ,position = "bottomleft")































colors = tazs.shape_hbwpk_rs5 %>%
  st_drop_geometry() %>%
  select(Range, Color) %>%
  unique()

temp_date = tazs.shape_hbwpk_rs5 %>%
  mutate(Range = fct_relevel(Range, colors$Range))

colors_2 = temp_date %>%
  st_drop_geometry() %>%
  select(Range, Color) %>%
  unique() %>%
  mutate(Range_lcl = as.numeric(Range))


colpal_hbwpk_rs5 = leaflet::colorFactor(
  palette = colors_2$Color
  ,levels = levels(colors_2$Range)
)

mapview(temp_date
        ,zcol = 'Range'
        ,col.region = colpal_hbwpk_rs5(temp_date$Range)
        ,layer.name = str_glue("HBW - Production User Benefits (mins)"))

# leaflet() %>%
#   addPolygons(data = tazs.shape_hbwpk_rs5
#               ,fillColor = ~colpal_hbwpk_rs5(tazs.shape_hbwpk_rs5$Range)
#               ,group = "id_1"
#               ,fillOpacity = .8
#               ,color = "black"
#               ,popup = tazs.shape_hbwpk_rs5$Range) %>%
#   addLegend(pal = colpal_hbwpk_rs5
#             ,values = tazs.shape_hbwpk_rs5$Range
#             ,group = "id_1"
#             ,position = "bottomleft")


























function(){
temp_date = tazs.shape_hbwpk_rs5 %>%
  arrange(plot.bins) %>%
  mutate(Range = fct_inorder(Range))

colpal_hbwpk_rs5 = leaflet::colorFactor(
  palette = unique(temp_date$Color)
  ,levels = levels(unique(temp_date$Range))
)

mapview(temp_date
        ,zcol = 'Range'
        ,col.region = colpal_hbwpk_rs5(temp_date$Range)
        ,layer.name = str_glue("HBW - Production User Benefits (mins)"))
}
























