add_percentile_columns <- function(df, percentiles = c(0.25, 0.5, 0.75), group.cols = c("variable", "emulator", "category", "year")) {
  p_names <- map_chr(percentiles, ~ paste0("p", .x * 100))

  p_funs <- map(percentiles, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

  group.cols <- enquo(group.cols) # need to quote

  df.percentiles <- df %>%
    group_by_at(vars(!!group.cols)) %>%
    summarize_at(vars(value), .funs = p_funs)

  return(df %>% left_join(df.percentiles))
}

normalise_iamc_long <- function(df, starting.year) {

  # normalise the values in column "value" to the year "starting.year = 1"
  # - by (model, scenario, region, variable, unit)

  df.temp <- df %>%
    left_join(
      df %>% filter(year==starting.year) %>% rename(value.start=value) %>%
        select(model, scenario, region, variable, unit, value.start),
      by = c("model", "scenario", "region", "variable", "unit")
    ) %>%
    mutate(value=value/value.start)

  return(
    df.temp %>% select(-value.start)
  )

}

iamc_wide_to_long <- function(df, upper.to.lower = F) {

  # function assumes all five basic IAMC columns are there, and nothing more

  if (upper.to.lower) {
    df <- df %>%
      rename(
        model = Model,
        scenario = Scenario,
        region = Region,
        variable = Variable,
        unit = Unit
      )
  }

  first.year <- colnames(df)[6] # assumes all five basic IAMC columns are there, and nothing more
  last.year <- colnames(df)[length(colnames(df))]

  df <- df %>%
    pivot_longer(
      cols = first.year:last.year,
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year))

  return(df)
}

unique_variable <- function(df){
  return(
    df %>% pull(variable) %>% unique() %>% sort()
  )
}
unique_region <- function(df){
  return(
    df %>% pull(region) %>% unique() %>% sort()
  )
}
unique_ms <- function(df){
  return(
    df %>% mutate(`Model-scenario`=paste0(model,"-",scenario)) %>% pull(`Model-scenario`) %>% unique() %>% sort()
  )
}
save_ggplot = function(p,f,h=150,w=150,format="png-pdf"){
  if(format=="png-pdf"){
    ggsave(
      plot = p,
      file = paste0(f,".png"),
      height = h,
      width = w,
      unit = "mm"
    )
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = grDevices::cairo_pdf,
      height = h,
      width = w,
      unit = "mm"
    )
  } else if (format=="png") {
    ggsave(
      plot = p,
      file = paste0(f,".png"),
      height = h,
      width = w,
      unit = "mm"
    )
  } else if (format=="pdf") {
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = grDevices::cario_pdf,
      height = h,
      width = w,
      unit = "mm"
    )
  }
}
