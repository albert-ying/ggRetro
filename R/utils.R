#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param x string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#' @param y func_regex Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#' @param scale_x
#' @param scale_y
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom ggplot2 geom_point ggplot_build scale_x_continuous scale_y_continuous theme element_line element_blank aes unit
#' @export
#-----------------------------------------------------------------------------
base_breaks <- function(x, y, scale_x = T, scale_y = T) {
  if (scale_x) {
    b1 = pretty(x)
    sx = scale_x_continuous(breaks=b1)
  } else {
    b1 = as.factor(x) |> as.numeric()
    sx = NULL
  }
  if (scale_y) {
    b2 = pretty(y)
    sy = scale_y_continuous(breaks=b2)
  } else {
    b2 = as.factor(y) |> as.numeric()
    sy = NULL
  }
  d = data.frame(x=c(min(b1), max(b1)), y=c(min(b2), max(b2)))
  list(
    sx, sy, geom_rangeframe(data = d, aes(x=x, y=y), inherit.aes = FALSE), ggthemes::theme_tufte(base_size = 18, base_family = "Helvetica"), theme(axis.ticks = element_line(size = 0.25, color = "black"), axis.ticks.length = unit(.6, "lines"), panel.grid.minor = element_blank())
  )
}

#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param lab string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#'                   stats and geoms and some other misc stuff.
#' @importFrom stringr str_replace str_to_title
#' @export
#-----------------------------------------------------------------------------
smart_lab = function(lab) {
  str_replace(lab, "_", " ") |> str_to_title()
}
#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param p string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#' @param i Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#' @param  smart_label Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#'                   stats and geoms and some other misc stuff.
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom ggplot2 geom_point ggplot_build
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @export
#-----------------------------------------------------------------------------

base_mode = function(p, i = 1, smart_label = T) {
  # px = p + geom_point()
  px = p
  p_tb = ggplot_build(px)$data |>
    bind_rows() |>
    as_tibble()
  if (class(p_tb$x)[1] != "mapped_discrete" & class(p_tb$y)[1] != "mapped_discrete") {
    print("Both numeric")
    np = p + base_breaks(p_tb$x, p_tb$y)
  } else if (class(p_tb$x)[1] != "mapped_discrete") {
    print("x numeric")
    np = p + base_breaks(p_tb$x, p_tb$y |> round(), scale_y = F)
  } else if (class(p_tb$y)[1] != "mapped_discrete") {
    print("y numeric")
    np = p + base_breaks(p_tb$x |> round(), p_tb$y, scale_x = F)
  } else {
    print("no numeric")
    np = p + geom_rangeframe()
  }
  if (smart_label) {
    np$labels = np$labels |>
      map(smart_lab)
  }
  return(np)
}

#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param p  plot
#' @param facets a vector of column names used for facet
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom ggplot2 geom_point ggplot_build
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr group_by_at group_split group_keys
#' @export
#-----------------------------------------------------------------------------
base_facet = function(
  p,
  facets,
  label_format_number = "{var.name} = {var.value}",
  label_format_string = "{var.value}",
  label_column = NA,
  smart_label = T,
  guides = "collect",
  ...
) {
  px = p 
  raw_data = px$data
  datakey = raw_data |>
    group_by_at(facets) |>
    group_keys()
  datals = raw_data |>
    group_by_at(facets) |>
    group_split()
  plot_list = map(1:length(datals), ~ {
    psub = p
    psub$data = datals[[.x]]
    for (c in 1:length(facets)) {
      var.name = facets[c] |> smart_lab()
      var.value = datakey[[.x, c]]
      if (is.numeric(var.value)) {
        name = glue(label_format_number)
      } else {
        name = glue(label_format_string)
      }
      if (c == 1) {
        facet.name = name } else {
        facet.name = str_c(facet_name, "\n", name)
      }
    }
    psub = base_mode({
      psub +
        labs(subtitle = facet.name)
    })
    return(psub)
  })
  wrap_plots(plot_list, guides = guides, ...)
}

#-----------------------------------------------------------------------------
#' better_ggplot_default()
#' @importFrom ggthemes geom_rangeframe theme_tufte
#' @importFrom ggplot2 geom_point ggplot_build
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom ggtext element_markdown
#' @importFrom ggsci scale_color_npg scale_fill_npg
#' @export
#-----------------------------------------------------------------------------

oh_my_ggplot = function() {
  ## change global theme settings (for all following plots)
  ## fall back theme
  theme_set(
   theme_ipsum(plot_title_size = 28, subtitle_size = 22, base_size = 18, axis_title_size = 24, strip_text_size = 22, base_family = "Helvetica", axis_title_just = "mc") +
     theme(
       plot.title.position = "plot", plot.caption.position = "plot", legend.position = "right", plot.margin = margin(25, 25, 10, 25),
       axis.ticks = element_line(color = "grey92"), panel.grid.major = element_blank(),
       legend.text = element_text(color = "grey30"),
       plot.subtitle = element_text(color = "grey30"),
       plot.caption = element_text(margin = margin(t = 15))
     ) +
   theme(plot.title = element_markdown(), plot.subtitle = element_markdown(), plot.caption = element_markdown(margin = margin(t = 15)), axis.title.x = element_markdown(), axis.title.y = element_markdown())
  )
  
}

#-----------------------------------------------------------------------------
# debug
#-----------------------------------------------------------------------------
if (FALSE) {
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  oh_my_ggplot()

  base_mode({
    ggplot(mtcars) +
      geom_line(aes(mpg, wt, color = as.factor(carb))) +
      labs(title="hello") +
      theme_bw() +
      facet_wrap(~am) +
      better_color_legend()
  })
}
