#' Set the theme for a figure
#'
#' @param fig a figure to set the theme for
#' @param theme theme
#' @example man-roxygen/ex-theme.R
#' @export
set_theme <- function(fig, theme) {
  if (is.function(theme))
    theme <- theme()

  if (!is.null(fig$x$modeltype) && fig$x$modeltype == "GridPlot") {
    for (ii in seq_along(fig$x$spec$figs))
      fig$x$spec$figs[[ii]]$x$spec$theme <- theme
  } else {
    fig$x$spec$theme <- theme
  }

  fig
}

#' Themes
#' @rdname themes
#' @export
bk_default_theme <- function() {
  list(
    discrete = list(
      glyph = pal_bk_glyph(),
      fill_color = pal_tableau("Tableau10"),
      line_color = pal_tableau("Tableau10"),
      text_color = pal_tableau("Tableau10"),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      line_dash = pal_bk_line_dash(),
      line_width = pal_bk_line_width(),
      size = pal_size()
    ),
    continuous = list(
      glyph = pal_bk_glyph(),
      fill_color = pal_gradient(),
      line_color = pal_gradient(),
      text_color = pal_gradient(),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      line_dash = pal_bk_line_dash(),
      line_width = pal_bk_line_width(),
      size = pal_size()
    ),
    ungrouped = list(fill_color = "black", line_color = "black",
      text_color = "black", fill_alpha = 0.5, line_alpha = 1,
      size = 20, glyph = 1, line_dash = NULL, line_width = 1),
    plot = NULL,
    # axis = list(axis_label_text_font_size = "12pt"),
    grid = NULL,
    legend = NULL
  )
}

#' Themes
#' @rdname themes
#' @export
#' @importFrom scales shape_pal hue_pal
bk_ggplot_theme <- function() {
  gg_shape_pal <- function() {
    function(n) {
      unname(unlist(lapply(marker_dict[as.character(scales::shape_pal()(n))],
        function(x) x$glyph)))
    }
  }

  list(
    discrete = list(
      glyph = gg_shape_pal(),
      fill_color = scales::hue_pal(),
      line_color = scales::hue_pal(),
      text_color = scales::hue_pal(),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      # line_dash = ,
      # line_width = ,
      size = pal_size() # ggplot2::scale_size_discrete()$palette
    ),
    continuous = list(
      fill_color = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
      line_color = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
      text_color = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
      fill_alpha = 1,
      line_alpha = 1,
      text_alpha = 1,
      # line_dash = ,
      # line_width = ,
      size = pal_size() # ggplot2::scale_size_continuous()$palette
    ),
    gradient = pal_gradient(c("#132B43", "#56B1F7"), space = "Lab"),
    ungrouped = list(fill_color = "black", line_color = "black",
      text_color = "black", fill_alpha = 1, line_alpha = 1,
      size = 10, glyph = 16, line_dash = NULL, line_width = 1),
    plot = list(background_fill_color = "#E6E6E6",
      outline_line_color = "white"),
    grid = list(grid_line_color = "white",
      minor_grid_line_color = "white",
      minor_grid_line_alpha = 0.4),
    axis = list(axis_line_color = "white",
      major_label_text_color = "#7F7F7F",
      major_tick_line_color = "#7F7F7F",
      minor_tick_line_alpha = 0,
      axis_label_text_font_style = "normal",
      num_minor_ticks = 2)
  )
}

#' Themes
#' @rdname themes
#' @export
#' @importFrom scales shape_pal hue_pal
bk_dark_theme <- function() {
    ggShapePal <- function() {
      function(n) {
        unname(unlist(lapply(
          marker_dict[as.character((scales::shape_pal())(n))],
          function(x) x[["glyph"]]
        )))
      }
    }
    list(
      discrete = list(
        glyph = ggShapePal(),
        fill_color = scales::hue_pal(),
        line_color = scales::hue_pal(),
        text_color = scales::hue_pal(),
        fill_alpha = 1,
        line_alpha = 1,
        text_alpha = 1,
        size = rbokeh::pal_size()
      ),
      continuous = list(
        fill_color = rbokeh::pal_gradient(c(
          "#132B43",
          "#56B1F7"
        ),
        space = "Lab"
        ),
        line_color = rbokeh::pal_gradient(c(
          "#132B43",
          "#56B1F7"
        ),
        space = "Lab"
        ),
        text_color = rbokeh::pal_gradient(c(
          "#132B43",
          "#56B1F7"
        ),
        space = "Lab"
        ),
        fill_alpha = 1,
        line_alpha = 1,
        text_alpha = 1,
        size = rbokeh::pal_size()
      ),
      gradient = rbokeh::pal_gradient(c(
        "#132B43",
        "#56B1F7"
      ),
      space = "Lab"
      ),
      ungrouped = list(
        fill_color = "black",
        line_color = "black",
        text_color = "black",
        fill_alpha = 1, # 0.5
        line_alpha = 1,
        size = 50, # 20
        glyph = 16, # 1
        line_dash = NULL,
        line_width = 1
      ),
      plot = list(
        background_fill_color = "#20262b",
        border_fill_color = "#15191c",
        outline_line_color = "#e0e0e0",
        outline_line_alpha = 0.25
      ),
      grid = list(
        grid_line_color = "#e0e0e0",
        grid_line_alpha = 0.25
      ),
      axis = list(
        major_tick_line_alpha = 0,
        major_tick_line_color = "#e0e0e0",
        minor_tick_line_alpha = 0,
        minor_tick_line_color = "#e0e0e0",
        axis_line_alpha = 0,
        axis_line_color = "#e0e0e0",
        major_label_text_color = "#e0e0e0",
        major_label_text_font = "Helvetica",
        major_label_text_font_size = "12.3pt",
        axis_label_standoff = 10,
        axis_label_text_color = "#e0e0e0",
        axis_label_text_font = "Helvetica",
        axis_label_text_font_size = "15pt", # 12pt
        axis_label_text_font_style = "normal"
      ),
      legend = list(
        spacing = 8,
        glyph_width = 15,
        label_standoff = 8,
        label_text_color = "#e0e0e0",
        label_text_font = "Helvetica",
        label_text_font_size = "12.3pt",
        border_line_alpha = 0,
        background_fill_alpha = 0.25,
        background_fill_color = "#20262b"
      ),
      theme = list(
        text_color = "#e0e0e0",
        text_font = "Helvetica",
        text_font_size = "13.8pt",
      )
    )
  }
