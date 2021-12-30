## =============================================================================
## analysis.R
##
## Copyright (c) 2021, Aaron Ceross <aaron.ceross@eng.ox.ac.uk>
##
## Analysis script for <Exploring the impact of COVID-19 policy on
## medical device access to markets>
##
## =============================================================================

library(tidyverse)
library(tidyquant)
library(zoo)
library(RColorBrewer)

if (!dir.exists("figures/"))
    dir.create("figures/")

## -----------------------------------------------------------------------------
## GUDID Analysis
## -----------------------------------------------------------------------------


GUDID <- readr::read_csv("data/GUDID.csv")

GUDID %>%
    dplyr::mutate(yearmonth = zoo::as.yearmon(date)) %>%
    dplyr::group_by(dev_type, yearmonth) %>%
    dplyr::summarise(n = n()) %>%
    ggplot(aes(yearmonth, n, fill = dev_type)) +
    geom_bar(stat = "identity"
           , position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_y_continuous(trans = "log10", labels = scales::comma) +
    scale_fill_brewer(name = "Device Type"
                    , palette = "Set2"
                    , direction = -1
                    , breaks = c("covid", "ppe", "other")
                    , labels = c("COVID", "PPE", "Other")
                        ) +
   geom_vline(xintercept = 2020.17, color = "darkred") +
   annotate("text"
          , x = 2020.07
          , y = 100000
          , label = "WHO declares\nCOVID-19 pandemic"
          , color = "darkred") +
   theme_minimal() +
   theme(axis.line = element_line(size = 0.3)
       , axis.text = element_text(colour = "black", size = 10)
       , axis.title = element_text(size = 18)
       , legend.text = element_text(size = 15)
       , legend.title = element_text(size = 16)
       , legend.position = "top"
       , legend.box = "horizontal"
         ) +
   xlab("Date") +
   ylab("Count")
ggsave("figures/gudid_compare.pdf")

gudid_roll_stats <- GUDID %>%
    dplyr::filter(dev_type != "other") %>%
    dplyr::select(dev_type, date) %>%
    dplyr::group_by(dev_type, date) %>%
    dplyr::summarise(count = n()) %>%
    tq_mutate(
          select     = count,
          mutate_fun = rollapply,
          width      = 10,
          align      = "right",
          FUN        = mean,
          na.rm      = TRUE,
          col_rename = "mean_10"
      ) %>%
    tq_mutate(
        select     = count,
        mutate_fun = rollapply,
        width      = 20,
        align      = "right",
        FUN        = mean,
        na.rm      = TRUE,
        col_rename = "mean_20"
    )

dev_labs <- c("COVID Devices", "PPE")
names(dev_labs) <- c("covid", "ppe")

gudid_roll_stats %>%
    dplyr::filter(date > "2019-06-30") %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(y = mean_10), color = palette_light()[[1]]) +
    geom_line(aes(y = mean_20), color = palette_light()[[2]]) +
    facet_wrap(~ dev_type
             , scales = "free_y"
             , nrow = 2
             , labeller = labeller(dev_type = dev_labs)
               ) +
    theme_minimal() +
    theme(axis.line.x.bottom  = element_line(colour = "black")
        , axis.ticks.x.bottom = element_line(colour = "black")
        , axis.ticks.y.left   = element_line(colour = "black")
        , axis.line.y.left    = element_line(colour = "black")
          ) +
    scale_fill_manual() +
    labs(x = "Date", y = "Count")
ggsave("figures/gudid_covid_mov_avg.pdf")

## -----------------------------------------------------------------------------
## ARTG Analysis
## -----------------------------------------------------------------------------

ARTG <- readr::read_csv("data/ARTG.csv") %>%
    dplyr::mutate(
        dev_type = case_when(
            ppe == TRUE ~ "ppe",
            covid == TRUE ~ "covid",
            TRUE ~ "other"
        )
    )

ARTG %>%
    dplyr::mutate(yearmonth = as.yearmon(date)) %>%
    dplyr::group_by(dev_type, yearmonth) %>%
    dplyr::summarise(n = n()) %>%
    ggplot(aes(yearmonth, n, fill = dev_type)) +
    geom_bar(stat = "identity"
           , position = position_dodge2(width = 0.9, preserve = "single")
             ) +
    geom_vline(xintercept = 2020.17, color = "darkred") +
    annotate("text"
           , x = 2020.12
           , y = 600
           , label = "WHO declares\nCOVID-19\npandemic"
           , color = "darkred") +
    scale_fill_brewer(name = "Device Type"
                    , palette = "Set2"
                    , direction = -1
                    , breaks=c("covid", "ppe", "other")
                    , labels = c("COVID", "PPE", "Other")) +
    theme_minimal() +
    theme(axis.line = element_line(size = 0.3)
        , axis.text = element_text(colour = "black", size = 10)
        , axis.title = element_text(size = 18)
        , legend.text = element_text(size = 15)
        , legend.title = element_text(size = 16)
        , legend.position = "top"
        , legend.box = "horizontal"
          ) +
    labs(x = "Date", y = "Count")
ggsave("figures/artg_compare1.pdf")

artg_roll_stats <- ARTG %>%
    dplyr::filter(dev_type != "other") %>%
    dplyr::select(dev_type, date) %>%
    dplyr::group_by(dev_type, date) %>%
    dplyr::summarise(count = n()) %>%
    tq_mutate(
          select     = count,
          mutate_fun = rollapply,
          width      = 10,
          align      = "right",
          FUN        = mean,
          na.rm      = TRUE,
          col_rename = "mean_10"
      ) %>%
    tq_mutate(
          select     = count,
          mutate_fun = rollapply,
          width      = 20,
          align      = "right",
          FUN        = mean,
          na.rm      = TRUE,
          col_rename = "mean_20"
      )


## dev_labs <- c("COVID Devices", "PPE")
## names(dev_labs) <- c("covid", "ppe")

artg_roll_stats %>%
    dplyr::filter(date > "2019-06-30") %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(y = mean_10), color = palette_light()[[1]]) +
    geom_line(aes(y = mean_20), color = palette_light()[[2]]) +
    facet_wrap(~ dev_type
             , scales = "free_y"
             , nrow = 2
             , labeller = labeller(dev_type = dev_labs)
               ) +
    theme_minimal() +
    theme(axis.line.x.bottom  = element_line(colour = "black")
        , axis.ticks.x.bottom = element_line(colour = "black")
        , axis.ticks.y.left   = element_line(colour = "black")
        , axis.line.y.left    = element_line(colour = "black")
          ) +
    scale_fill_manual(name = "Smething") +
    labs(x = "Date", y = "Count")
ggsave("figures/artg_covid_mov_avg.pdf")
