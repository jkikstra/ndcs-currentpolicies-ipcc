library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("ggsci")
library("patchwork")
library("ggthemes")

here::i_am("NDCs_IPCC_AR6.Rproj")

source(here("utils.R"))

# ar6 scenario data locations ====

ar6.data.location <- "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Scenario data/Scenario Databases/AR6_Scenarios_Database_World_v1.1"
ar6.data.file <- "AR6_Scenarios_Database_World_v1.1.csv"
ar6.meta.file <- "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"

# figures sent on 04.05.2023 ====
for (climate.model in c("FaIRv1.6.2","MAGICCv7.5.3")){
  variables <- c(
    paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|",climate.model,"|50.0th Percentile"),
    paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|",climate.model,"|5.0th Percentile"),
    paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|",climate.model,"|95.0th Percentile")
  )

  # ar6 scenario data read in ====

  ar6.data <- vroom(
    file.path(
      ar6.data.location,
      ar6.data.file
    )
  ) %>% filter(
    Variable %in% variables
  ) %>% iamc_wide_to_long()

  ar6.meta <- read_excel(file.path(ar6.data.location, ar6.meta.file),
                         sheet = "meta_Ch3vetted_withclimate") %>%
    select(Model, Scenario, Category, IMP_marker, COVID, Subset_Ch4)

  ar6.meta %>% pull(Subset_Ch4) %>% unique()

  ar6 <- ar6.data %>%
    left_join(ar6.meta)

  ar6.ndc <- ar6 %>%
    filter(
      Subset_Ch4 %in% c("NDCs announced prior to COP26", "Trend from implemented policies")
    )

  # plot without climate uncertainty
  p.ndc.p50 <- ggplot(
    ar6.ndc %>% mutate(ms=paste0(Model,Scenario)) %>%
      filter(Variable==paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|",climate.model,"|50.0th Percentile")),
    aes(x=year,y=value,group=ms,colour=Subset_Ch4)
  ) +
    geom_line() +
    scale_color_colorblind() +
    theme_classic() +
    theme_hc() +
    labs(
      title = "Median temperature for each scenario.",
      y = "Global Temperature above pre-industrial (degree C)",
      x = NULL
    )

  p.ndc.p50



  # plot with climate uncertainty
  p.ndc.p595 <- ggplot(
    ar6.ndc %>% mutate(ms=paste0(Model,Scenario)),
    aes(x=year,group=ms,colour=Subset_Ch4)
  ) +
    geom_ribbon(
      data=. %>%
        mutate(Variable=ifelse(
          grepl(Variable, pattern="|5.0th", fixed=T), "5th Percentile",
          ifelse(
            grepl(Variable, pattern="|50.0th", fixed=T), "50th Percentile",
            ifelse(
              grepl(Variable, pattern="|95.0th", fixed=T), "95th Percentile",
              NA
            )
          )
        )) %>% pivot_wider(values_from = value, names_from = Variable),
      aes(fill=Subset_Ch4,
          ymin=`5th Percentile`,
          ymax=`95th Percentile`),
      alpha=0.1
    ) +
    geom_line(data=. %>%
                filter(Variable==paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|",climate.model,"|50.0th Percentile")),
              aes(y=value)) +
    scale_color_colorblind() +
    scale_fill_colorblind() +
    theme_classic() +
    theme_hc() +
    labs(
      title = "5-95th percentile temperature range for each scenario.",
      y = "Global Temperature above pre-industrial (degree C)",
      x = NULL
    )

  p.ndc.p595

  p.2100.ranges <-
    ggplot(
      data = ar6.ndc %>% group_by(Subset_Ch4,Variable) %>% filter(year==2100) %>%
        summarise(
          p5=quantile(value,0.05), #type 7 is default, in R, as well as in numpy
          p50=quantile(value,0.50),
          p95=quantile(value,0.95)
        ) %>%
        pivot_longer(cols=p5:p95, names_to = "Percentile (Scenario range)", values_to = "Temperature in 2100") %>%
        mutate(Variable=ifelse(
          grepl(Variable, pattern="|5.0th", fixed=T), "5th Percentile",
          ifelse(
            grepl(Variable, pattern="|50.0th", fixed=T), "50th Percentile",
            ifelse(
              grepl(Variable, pattern="|95.0th", fixed=T), "95th Percentile",
              NA
            )
          )
        )) %>%
        rename(`Percentile (Climate range)`=Variable) %>%
        pivot_wider(names_from = `Percentile (Climate range)`, values_from = `Temperature in 2100`)
    ) +
    facet_grid(Subset_Ch4~.) +
    geom_pointrange(
      aes(x=`Percentile (Scenario range)`,
          ymin = `5th Percentile`,
          y = `50th Percentile`,
          ymax = `95th Percentile`
      )
    ) +
    geom_text(aes(x=`Percentile (Scenario range)`,
                  y = `50th Percentile`,
                  label = round(`50th Percentile`,digits=1)
    ),
    colour="red",
    position = position_nudge(x=0.1)
    ) +
    theme_classic() +
    theme_hc() +
    labs(
      title = "Disentangling climate uncertainty (ranges; 5th-95th percentile) and scenario ranges (x-axis)",
      y = "Global Temperature above pre-industrial (degree C)"
    )
  p.2100.ranges


  p.ndcs.celine <- ((p.ndc.p50 | p.ndc.p595) / p.2100.ranges) +
    plot_annotation(tag_levels = "A") +
    plot_layout(heights = c(1,2))

  p.ndcs.celine


  save_ggplot(p = p.ndcs.celine,
              f = here(paste0("ndcs-curpol-ipcc-celine-",climate.model)),
              h=400, w=300)
}


# updated, clearer figure sent on 05.05.2023 ====
variables <- c(
  paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|","FaIRv1.6.2","|50.0th Percentile"),
  paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|","FaIRv1.6.2","|5.0th Percentile"),
  paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|","FaIRv1.6.2","|95.0th Percentile"),
  paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|","MAGICCv7.5.3","|50.0th Percentile"),
  paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|","MAGICCv7.5.3","|5.0th Percentile"),
  paste0("AR6 climate diagnostics|Surface Temperature (GSAT)|","MAGICCv7.5.3","|95.0th Percentile")
)

ar6.data <- vroom(
  file.path(
    ar6.data.location,
    ar6.data.file
  )
) %>% filter(
  Variable %in% variables
) %>% iamc_wide_to_long(upper.to.lower = T)

ar6.meta <- read_excel(file.path(ar6.data.location, ar6.meta.file),
                       sheet = "meta_Ch3vetted_withclimate") %>%
  select(Model, Scenario, Category, IMP_marker, COVID, Subset_Ch4)

ar6.meta %>% pull(Subset_Ch4) %>% unique()

ar6 <- ar6.data %>%
  left_join(ar6.meta %>% rename(
    model=Model, scenario=Scenario
  ))

ar6.ndc <- ar6 %>%
  filter(
    Subset_Ch4 %in% c("NDCs announced prior to COP26", "Trend from implemented policies")
  )

ar6.ndc.climemu <- ar6.ndc %>%
  add_climate_emulator_col()


ar6.ndc.climemu.stats <- ar6.ndc.climemu %>% group_by(Subset_Ch4,variable,emulator) %>% filter(year==2100) %>%
  summarise(
    p5=quantile(value,0.05), #type 7 is default, in R, as well as in numpy
    p50=quantile(value,0.50),
    p95=quantile(value,0.95)
  ) %>%
  pivot_longer(cols=p5:p95, names_to = "Percentile (Scenario range)", values_to = "Temperature in 2100") %>%
  mutate(variable=ifelse(
    grepl(variable, pattern="|5.0th", fixed=T), "5th Percentile",
    ifelse(
      grepl(variable, pattern="|50.0th", fixed=T), "50th Percentile",
      ifelse(
        grepl(variable, pattern="|95.0th", fixed=T), "95th Percentile",
        NA
      )
    )
  )) %>%
  rename(`Percentile (Climate range)`=variable) %>%
  pivot_wider(names_from = `Percentile (Climate range)`, values_from = `Temperature in 2100`)

p.ranges.3.5.2 <- ggplot(
  data = ar6.ndc.climemu.stats
) +
  facet_grid(Subset_Ch4~.) +
  geom_pointrange(
    aes(x=`Percentile (Scenario range)`,
        ymin = `5th Percentile`,
        y = `50th Percentile`,
        ymax = `95th Percentile`,
        colour = emulator
    ),
    position = position_dodge2(width=0.2)
  ) +

  # p5
  geom_text(aes(x=`Percentile (Scenario range)`,
                y = `50th Percentile`,
                label = round(`50th Percentile`,digits=1)
  ),
  data=. %>% filter(`Percentile (Scenario range)`=="p5",
                    emulator=="FaIR"),
  colour="black",
  fontface="bold",
  position = position_nudge(x=-0.2)
  ) +
  # p50
  geom_text(aes(x=`Percentile (Scenario range)`,
                y = `50th Percentile`,
                label = round(`50th Percentile`,digits=1)
  ),
  data=. %>% filter(`Percentile (Scenario range)`=="p50",
                    emulator=="MAGICC"),
  colour="black",
  fontface="bold",
  position = position_nudge(x=0.2)
  ) +
  #  p95
  geom_text(aes(x=`Percentile (Scenario range)`,
                y = `50th Percentile`,
                label = round(`50th Percentile`,digits=1)
  ),
  data=. %>% filter(`Percentile (Scenario range)`=="p95",
                    emulator=="MAGICC"),
  colour="black",
  fontface="bold",
  position = position_nudge(x=0.2)
  ) +
  scale_color_jco() +
  theme_classic() +
  theme_hc() +
  labs(
    title = "Disentangling climate uncertainty (ranges; 5th-95th percentile) and scenario ranges (x-axis)",
    y = "Global Temperature above pre-industrial (degree C)\nwith climate uncertainty (5th-50th-95th percentile temp variables)"
  )
p.ranges.3.5.2

save_ggplot(p = p.ranges.3.5.2,
            f = here("section-3.5.2-bothclimatemodels"),
            h=200, w=200)
