#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param x string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#' @param y func_regex Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#' @param scale_x
#' @param scale_y
#' @importFrom ggplot2 geom_point ggplot_build scale_x_continuous scale_y_continuous theme element_line element_blank aes unit
#' @export
#-----------------------------------------------------------------------------
theme_tufte <- function (ticks = TRUE) {
    ret <- theme(legend.background = element_blank(), legend.key = element_blank(),               
            panel.background = element_blank(), panel.border = element_blank(),                
            strip.background = element_blank(), plot.background = element_blank(),             
            axis.line = element_blank(), panel.grid = element_blank())                         
    if (!ticks) {
        ret <- ret + theme(axis.ticks = element_blank())                                       
    }
    ret
}
#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param x string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#' @param y func_regex Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#' @param scale_x
#' @param scale_y
#' @importFrom ggthemes geom_rangeframe 
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
    sx,
    sy,
    geom_rangeframe(data = d, aes(x=x, y=y), inherit.aes = FALSE),
    theme_tufte(),
    theme(
      axis.ticks = element_line(size = 0.25, color = "black"),
      axis.ticks.x = element_line(size = 0.25, color = "black"),
      axis.ticks.y = element_line(size = 0.25, color = "black"),
      axis.ticks.length = unit(.6, "lines"),
      panel.grid.minor = element_blank()
    )
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
#' @importFrom stringr str_c
#' @importFrom purrr map
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
  guides = "auto",
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
  layer_datals = map(px$layers, ~{
    if(is.data.frame(.x$data)) {
      if (any(facets %in% colnames(.x$data))) {
        .x$data |>
          right_join(datakey) |>
          group_by_at(facets) |>
          group_split()
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
  })
  plot_list = map(1:length(datals), ~ {
    psub = unserialize(serialize(px,NULL))
    psub$data = datals[[.x]]
    for (i in 1:length(psub$layers)) {
      if (any(!is.na(layer_datals[[i]]))) {
        psub$layers[[i]]$data = layer_datals[[i]][[.x]]
      }
    }
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
        facet.name = str_c(facet.name, "<br>", name)
      }
    }
    pfacet = base_mode({
      psub +
        labs(subtitle = facet.name)
    })
    return(pfacet)
  })
  wrap_plots(plot_list, guides = guides, ...)
}

#-----------------------------------------------------------------------------
# debug
#-----------------------------------------------------------------------------
if (FALSE) {
  library(ggRetro)
  library(ggplot2)
  library(dplyr)
  library(stringr)
  oh_my_ggplot()

  annot_tb = data.frame(x = c(18,24), y = c(4.5,3.0), am = c(0,1), lab = c("Hi", "There"))
  p = mtcars |>
    # mutate(carb = as.factor(carb)) |>
    ggplot() +
    geom_point(aes(mpg, wt, fill = carb)) +
    geom_text(data = annot_tb, aes(x, y, label = lab))
  p
  p |>
    base_mode()

  p |>
    base_facet(c("am", "vs"), guides = "auto", nrow = 2)
  
  facets = c("am", "vs")

  base_facet(p2,"am")
}
