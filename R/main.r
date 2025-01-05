#############################################
# VISUALIZE CLIMATE DATA WITH R
# Milos Popovic 2024/11/01
#############################################

install.packages("devtools")

devtools::install_github(
    "https://github.com/ErikKusch/KrigR"
)

# 1. INSTALL & LOAD LIBRARIES
#----------------------------

install.packages("pacman")

pacman::p_load(
    KrigR, rgeoboundaries,
    terra, tidyverse,
    gganimate
)

sessionInfo()

# 2. COUNTRY BORDERS
#-------------------

country_sf <- rgeoboundaries::gb_adm0(
    country = "CHE"
)

# 3. QUERY TEMPERATURE DATA
#---------------------------

API_User <- "***************" # PLEASE INSERT YOUR EMAIL
API_Key <- "*****************" # PLEASE INSERT YOUR API TOKEN

KrigR::Meta.List()
vars_df <- KrigR::Meta.Variables(
    "reanalysis-era5-land-monthly-means"
)

fix(vars_df)

KrigR::Meta.QuickFacts(
    "reanalysis-era5-land-monthly-means"
)

start_date <- "2014-01-01 00:00"
end_date <- "2023-12-31 24:00"

temperature_raw <- KrigR::CDownloadS(
    Type = "monthly_averaged_reanalysis",
    Variable = "2m_temperature",
    DataSet = "reanalysis-era5-land-monthly-means",
    DateStart = start_date,
    DateStop = end_date,
    TZone = "CET",
    FUN = "mean",
    TResolution = "month",
    TStep = 1,
    Dir = getwd(),
    FileName = "temperature_raw",
    Extent = as(country_sf, "Spatial"),
    API_User = API_User,
    API_Key = API_Key
)

terra::plot(temperature_raw[[120]])

# 4. MODEL
#---------

covariates_ls <- KrigR::CovariateSetup(
    Training = temperature_raw,
    Target = 0.01,
    Covariates = "GMTED2010",
    Keep_Global = FALSE,
    FileExtension = "tif"
)

# 5. KRIGGING
#------------

temperature_krigged <- KrigR::Kriging(
    Data = temperature_raw,
    Covariates_training = covariates_ls$Training,
    Covariates_target = covariates_ls$Target,
    Equation = "GMTED2010",
    nmax = 25,
    Cores = 10,
    FileName = "temperature_krigged",
    FileExtension = "nc",
    Dir = getwd(),
    Compression = 9,
    Keep_Temporary = FALSE,
    verbose = TRUE
)

terra::plot(temperature_krigged[[120]])

temperature_krigged <- terra::rast(
    "temperature_krigged.nc"
)

temperature_predicted <- temperature_krigged |>
    terra::crop(
        country_sf,
        snap = "in",
        mask = TRUE
    ) |>
    terra::project("EPSG:3035")

terra::plot(temperature_predicted[[120]])

# 6. LAYER NAMES
#---------------

months_vector <- seq(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = "month"
)

names(temperature_predicted) <- months_vector

july_temperature <- terra::subset(
    temperature_predicted,
    grep(
        "-07-01",
        names(
            names(temperature_predicted)
        )
    )
)

years <- 2014:2023
names(july_temperature) <- years

# 7. RASTER TO DATAFRAME
#-----------------------

july_temperature_df <- as.data.frame(
    july_temperature,
    xy = TRUE, na.rm = TRUE
)

head(july_temperature_df)

july_temperature_long <- july_temperature_df |>
    tidyr::pivot_longer(
        !c(x, y),
        names_to = "year",
        values_to = "value"
    )

head(july_temperature_long)

# 8. YEAR TO INTEGER AND K to C
#------------------------------

july_temperature_long$year <- as.integer(
    july_temperature_long$year
)

july_temperature_long$celsius <-
    july_temperature_long$value - 273.15

july_temperature_long <- july_temperature_long |>
    dplyr::select(
        -value
    )

# 9. BREAKS
#----------

vmin <- min(july_temperature_long$celsius)
vmax <- max(july_temperature_long$celsius)

breaks <- classInt::classIntervals(
    july_temperature_long$celsius,
    n = 14,
    style = "equal"
)$brks

# 10. COLORS AND THEME
#---------------------

cols <- hcl.colors(
    n = length(breaks),
    palette = "Spectral",
    rev = TRUE
)

theme_for_the_win <- function(){
    theme_void() +
    theme(
        legend.position = "bottom",
        legend.title = element_text(
            size = 50, color = "grey10"
        ),
        legend.text = element_text(
            size = 30, color = "grey10"
        ),
        plot.title = element_text(
            size = 60, color = "grey10",
            hjust = .5, vjust = -1
        ),
        plot.subtitle = element_text(
            size = 70, color = "#c43c4e",
            hjust = .5, vjust = -1
        ), # plot.subtitle
        plot.margin = unit(
            c(
                t = 0, r = 0,
                l = 0, b = 0
            ), "lines"
        )
    )    
}

# 11. MAP
#--------

july_map <- ggplot() +
    geom_raster(
        data = july_temperature_long,
        aes(
            x = x, y = y,
            fill = celsius
        )
    ) +
    scale_fill_gradientn(
        name = "Celsius degree",
        colors = cols,
        limits = c(vmin, vmax),
        breaks = breaks,
        labels = round(
            breaks, 0
        )
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(
                1,
                units = "cm"
            ),
            barwidth = unit(
                30,
                units = "cm"
            ),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = TRUE
        )
    ) +
    coord_sf(crs = "EPSG:3035") +
    labs(
        title = "Average July temperature (2014-2023)",
        subtitle = "{round(as.integer(frame_time), 0)}"
    ) +
    theme_for_the_win()

timelapse_july_map <- july_map +
    gganimate::transition_time(
        year
    ) +
    gganimate::ease_aes(
        "linear",
        interval = .1
    )

# 12. ANIMATE
#------------

animated_july_map <- gganimate::animate(
    timelapse_july_map,
    nframes = 100,
    duration = 20,
    start_pause = 3,
    end_pause = 30,
    height = 1200,
    width = 1200,
    units = "px",
    renderer = gifski_renderer(
        loop = TRUE
    )
)

gganimate::anim_save(
    "july_temperature.gif",
    animated_july_map
)

# 13. SEASONAL AGGREGATION
#-------------------------

summer_months <- c(
    "06", "07", "08"
)

dates <- terra::time(
    temperature_predicted
)

summer_layers <- which(
    format(
        dates, "%m"
    ) %in% summer_months
)

year <- format(
    dates, "%Y"
)

season_factor <- paste(
    year[summer_layers], "summer", "_"
)

summer_temperature <- terra::tapp(
    temperature_predicted[[summer_layers]],
    season_factor, mean, na.rm = TRUE
)

names(summer_temperature) <- years

# raster to dataframe

summer_temperature_df <- as.data.frame(
    summer_temperature,
    xy = TRUE, na.rm = TRUE
)

head(summer_temperature_df)

summer_temperature_long <- summer_temperature_df |>
    tidyr::pivot_longer(
        !c(x, y),
        names_to = "year",
        values_to = "value"
    )

head(summer_temperature_long)

# year to integer and Kelvin to Celsius

summer_temperature_long$year <- as.integer(
    summer_temperature_long$year
)

summer_temperature_long$celsius <-
    summer_temperature_long$value - 273.15

summer_temperature_long <- summer_temperature_long |>
    dplyr::select(
        -value
    )

# breaks

vmin <- min(summer_temperature_long$celsius)
vmax <- max(summer_temperature_long$celsius)

breaks <- classInt::classIntervals(
    summer_temperature_long$celsius,
    n = 14,
    style = "equal"
)$brks

# colors

cols <- hcl.colors(
    n = length(breaks),
    palette = "Spectral",
    rev = TRUE
)

# map

summer_map <- ggplot() +
    geom_raster(
        data = summer_temperature_long,
        aes(
            x = x, y = y,
            fill = celsius
        )
    ) +
    scale_fill_gradientn(
        name = "Celsius degree",
        colors = cols,
        limits = c(vmin, vmax),
        breaks = breaks,
        labels = round(
            breaks, 0
        )
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(
                1,
                units = "cm"
            ),
            barwidth = unit(
                30,
                units = "cm"
            ),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = TRUE
        )
    ) +
    coord_sf(crs = "EPSG:3035") +
    labs(
        title = "Average Summer Temperature (2014-2023)",
        subtitle = "{round(as.integer(frame_time), 0)}"
    ) +
    theme_for_the_win()

timelapse_summer_map <- summer_map +
    gganimate::transition_time(
        year
    ) +
    gganimate::ease_aes(
        "linear",
        interval = .1
    )

# animate

animated_summer_map <- gganimate::animate(
    timelapse_summer_map,
    nframes = 100,
    duration = 20,
    start_pause = 3,
    end_pause = 30,
    height = 1200,
    width = 1200,
    units = "px",
    renderer = gifski_renderer(
        loop = TRUE
    )
)

gganimate::anim_save(
    "summer_temperature.gif",
    animated_summer_map
)

# 14. TEMPERATURE ANOMALY
#------------------------

temperature_predicted <- temperature_predicted - 273.15

temperature_2014_2022 <- terra::subset(
    temperature_predicted,
    !grepl(
        "2023",
        names(temperature_predicted)
    )
) |>
    terra::app(
        mean,
        na.rm = TRUE
    )

temperature_2023 <- terra::subset(
    temperature_predicted,
    grepl(
        "2023",
        names(temperature_predicted)
    )
) |>
    terra::app(
        mean,
        na.rm = TRUE
    )

temperature_anomaly <- temperature_2023 - temperature_2014_2022

temperature_anomaly_df <- as.data.frame(
    temperature_anomaly,
    xy = TRUE, na.rm = TRUE
)

names(temperature_anomaly_df)[3] <- "value"

vmin <- min(temperature_anomaly_df$value)
vmax <- max(temperature_anomaly_df$value)

breaks <- classInt::classIntervals(
    temperature_anomaly_df$value,
    n = 14,
    style = "equal"
)$brks


cols <- hcl.colors(
    n = length(breaks),
    palette = "Spectral",
    rev = TRUE
)

temperature_anomaly_map <- ggplot() +
    geom_raster(
        data = temperature_anomaly_df,
        aes(
            x = x, y = y,
            fill = value
        )
    ) +
    scale_fill_gradientn(
        name = "Celsius degree difference",
        colors = colorRampPalette(
            cols,
            bias = 2
        )(length(cols)),
        limits = c(vmin, vmax),
        breaks = breaks,
        labels = round(
            breaks, 1
        )
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(
                1,
                units = "cm"
            ),
            barwidth = unit(
                30,
                units = "cm"
            ),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = .5,
            nrow = 1,
            byrow = TRUE
        )
    ) +
    coord_sf(crs = "EPSG:3035") +
    labs(
        title = "Temperature anomaly",
        subtitle = ""
    ) +
    theme_for_the_win()

ggsave(
    filename = "temeprature_anomaly.png",
    width = 6000, height = 6000,
    units = "px", device = "png",
    bg = "white", 
    temperature_anomaly_map
)
