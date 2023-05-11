
#replace duplicate tazs.shape_hbwpk_rs5 with correct data and correct names

maps_comb = list(
  list(tazs.shape_hbwpk_rs5
       ,'HBW - Production User Benefits (mins)')
  ,list(tazs.shape_hbwpk_rs5
        ,'Test_1')
  ,list(tazs.shape_hbwpk_rs5
        ,'Test_2')
) %>%
  map(~{
    #this forces data to always been arrange by plot bin
    #then sets Range factor levels to concede with -to+ ordering

    temp_date = .x[[1]] %>%
      arrange(plot.bins) %>%
      mutate(Range = fct_inorder(Range))

    temp_colpal = leaflet::colorFactor(
      palette = unique(temp_date$Color)
      ,levels = levels(unique(temp_date$Range))
    )

    mapview(temp_date
            ,zcol = 'Range'
            ,col.region = temp_colpal(temp_date$Range)
            ,layer.name = .x[[2]]
            ,homebutton = F)
  }) %>%
  reduce(`+`)

maps_comb_pro = maps_comb@map %>%
  leaflet::hideGroup(c("Test_1", "Test_2"))








