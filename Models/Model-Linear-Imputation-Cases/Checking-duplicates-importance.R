working_df %>% select(x_axis) %>% unique() %>% nrow()
working_df  %<>% groupdata2::group(n = 3, method = "greedy") %>%
    rename("id" = .groups)
working_df  %<>% group_by(id) %>%
    summarise_all(first) %>%
    select(-id)