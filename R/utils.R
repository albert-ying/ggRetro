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
theme_tufte2 <- function (ticks = TRUE) {
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
#' @importFrom scales label_wrap
#' @importFrom ggplot2 geom_point ggplot_build scale_x_continuous scale_y_continuous theme element_line element_blank aes unit coord_cartesian
#' @export
#-----------------------------------------------------------------------------
base_breaks <- function(x, y, scale_x = T, scale_y = T, x_lab_fun = function(x){x}, y_lab_fun = function(x){x}, n_wrap = 10) {
  if (scale_x) {
    b1 = pretty(x)
    # if (x_lab_fun == "auto") {
    #   sx = scale_x_continuous(breaks=b1)
    # } else {
    sx = scale_x_continuous(breaks=b1, labels=x_lab_fun)
    # }
  } else {
    b1 = as.factor(x) |> as.numeric()
    # if (x_lab_fun == "auto") {
    sx = scale_x_discrete(labels = function(x) {
      unlist(lapply(strwrap(x, width = n_wrap, simplify = FALSE),
        paste0,
        collapse = "<br>"
      ))
    })
    # } else {
    #   sx = scale_x_discrete(labels = x_lab_fun)
    # }
  }
  if (scale_y) {
    b2 = pretty(y)
    # if (y_lab_fun == "auto") {
    #   sy = scale_y_continuous(breaks=b2)
    # } else {
    sy = scale_y_continuous(breaks=b2, labels=y_lab_fun)
    # }
  } else {
    b2 = as.factor(y) |> as.numeric()
    # if (y_lab_fun == "auto") {
    sy = scale_y_continuous(labels = function(x) {
      unlist(lapply(strwrap(x, width = n_wrap, simplify = FALSE),
        paste0,
        collapse = "<br>"
      ))
    })
    # } else {
    #   sy = scale_y_discrete(labels = y_lab_fun)
    # }
  }
  d = data.frame(x=c(min(b1), max(b1)), y=c(min(b2), max(b2)))
  list(
    sx,
    sy,
    geom_rangeframe(data = d, aes(x=x, y=y), size = 0.7, inherit.aes = FALSE),
    theme_tufte2(),
    theme(
      axis.ticks = element_line(size = 0.7, color = "black"),
      axis.ticks.x = element_line(size = 0.7, color = "black"),
      axis.ticks.y = element_line(size = 0.7, color = "black"),
      axis.ticks.length = unit(.6, "lines"),
      panel.grid.minor = element_blank()
    ),
    coord_cartesian(clip = "off")
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
  str = str_replace(lab, "_", " ")
  gsub("\\b([a-z])", "\\U\\1", str, perl = TRUE)
}
#-----------------------------------------------------------------------------
#' Mimic Base R break
#' @param p string with which to prefix names of \code{ggplot2} functions in order to name the pipe-enabled functions. default: "add_".
#'               Note: You could set this the empty string, in which case the new functions would mask the name of the library function
#' @param i Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#' @param  smart_label Regular expression to filter the list of ggplot functions to make pipe-enabled.  The default regex will capture all
#'                   stats and geoms and some other misc stuff.
#' @importFrom ggthemes geom_rangeframe
#' @importFrom ggplot2 geom_point ggplot_build
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows select
#' @export
#-----------------------------------------------------------------------------

base_mode = function(
  p,
  i = 1,
  smart_label = T,
  x_lab_fun = function(x){x},
  y_lab_fun = function(x){x},
  n_wrap = 10
) {
  # px = p + geom_point()
  px = p
  options(warn = -1)
  p_tb = ggplot_build(px)$data |>
    map(~ {.x[,colnames(.x) %in% c("x", "y", "xmin", "xmax", "ymin", "ymax")]}) |>
    bind_rows() |>
    as_tibble()
  if (class(p_tb$x)[1] != "mapped_discrete" & class(p_tb$y)[1] != "mapped_discrete") {
    print("Both numeric")
    np = p + base_breaks(c(p_tb$x, p_tb$xmin, p_tb$xmax), c(p_tb$y, p_tb$ymin, p_tb$ymax), x_lab_fun = x_lab_fun, y_lab_fun = y_lab_fun, n_wrap = n_wrap)
  } else if (class(p_tb$x)[1] != "mapped_discrete") {
    print("x numeric")
    np = p + base_breaks(c(p_tb$x, p_tb$xmin, p_tb$xmax), p_tb$y |> round(), scale_y = F, x_lab_fun = x_lab_fun, y_lab_fun = y_lab_fun, n_wrap = n_wrap) + theme(axis.ticks.y = element_blank())
  } else if (class(p_tb$y)[1] != "mapped_discrete") {
    print("y numeric")
    np = p + base_breaks(p_tb$x |> round(), c(p_tb$y, p_tb$ymin, p_tb$ymax), scale_x = F, x_lab_fun = x_lab_fun, y_lab_fun = y_lab_fun, n_wrap = n_wrap) + theme(axis.ticks.x = element_blank())
  } else {
    print("no numeric")
    # np = p + geom_rangeframe()
    np = p + base_breaks(round(p_tb$x), round(p_tb$y), scale_x = F, scale_y = F, x_lab_fun = x_lab_fun, y_lab_fun = y_lab_fun, n_wrap = n_wrap) + theme(axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
  }
  options(warn = 0)
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
#' @importFrom purrr map transpose
#' @importFrom patchwork wrap_plots
#' @importFrom dplyr group_by_at group_split group_keys
#' @importFrom ggtext element_markdown
#' @export
#-----------------------------------------------------------------------------
base_facet = function(
  p,
  facets,
  scales = "free",
  label_format_number = "{var.name} = {var.value}",
  label_format_string = "{var.value}",
  label_column = NA,
  smart_label = T,
  guides = "collect",
  nrow = "auto",
  ncol = "auto",
  x_lab_fun = function(x){x},
  y_lab_fun = function(x){x},
  after_dat = NA,
  after_fun = NA,
  ...
) {
  px = p 
  raw_data = px$data
  datakey = raw_data |>
    group_by_at(facets) |>
    group_keys()
  # compute nrow ncol of patchwork plot, if not given
  nplot = nrow(datakey)
  if (nrow == "auto" & ncol == "auto") {
    nrow = floor(sqrt(nplot))
    ncol = ceiling(nplot / nrow)
  } else if (nrow == "auto") {
    nrow = ceiling(nplot / ncol)
  } else if (ncol == "auto") {
    ncol = ceiling(nplot / nrow)
  }
  # Given N plot, nrow and ncol, which plots are at left edge
  left_edge = seq(1, nplot, by = ncol)
  bottom_edge = seq(nplot-ncol+1, nplot, by = 1)
  # Get main data, split by group
  datals = raw_data |>
    group_by_at(facets) |>
    group_split()
  # Get data at other layers, split by group
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
  # handel "after_dat"
  if (is.list(after_dat)) {
    if (is.data.frame(after_dat)) {
      after_dat_ls = after_dat |>
        right_join(datakey) |>
        group_by_at(facets) |>
        group_split()
    } else {
      after_dat_ls = after_dat |>
        map(~{.x |>
          right_join(datakey) |>
          group_by_at(facets) |>
          group_split()}) |>
          purrr::transpose()
    }
  }
  ## Get ancors
  options(warn = -1)
  cord_set = ggplot_build(px)$data |>
    map(~ {
      cbind(x = .x$x, y = .x$y, xmin = .x$xmin, xmax = .x$xmax, ymin = .x$ymin, ymax = .x$ymax) |>
        as_tibble()
    }) |> bind_rows()
  x_max = max(c(cord_set$x, cord_set$xmax))
  x_min = min(c(cord_set$x, cord_set$xmin))
  y_max = max(c(cord_set$y, cord_set$ymax))
  y_min = min(c(cord_set$y, cord_set$ymin))
  options(warn = 0)

  plot_list = map(1:length(datals), ~ {
    psub = unserialize(serialize(px,NULL))
    psub$data = datals[[.x]]
    # Change the data in each layers in place
    for (i in 1:length(psub$layers)) {
      if (any(!is.na(layer_datals[[i]]))) {
        psub$layers[[i]]$data = layer_datals[[i]][[.x]]
      }
    }
    # get fact label
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
        facet.name = str_c(facet.name, "<br><br>", name)
      }
    }
    pfacet = psub
    if (!.x %in% bottom_edge) {
      pfacet = pfacet +
        labs(x = "") +
        theme(plot.margin = margin(b = 0))
    }
    if (!.x %in% left_edge) {
      pfacet = pfacet +
        labs(y = "") +
        theme(plot.margin = margin(l = 0, r = 0))
    }
    if (scales == "fixed") {
      pfacet = pfacet +
        geom_blank(aes(x = x_min, y = y_min)) +
        geom_blank(aes(x = x_max, y = y_max))
    }
    pfacet = base_mode(pfacet, x_lab_fun = x_lab_fun, y_lab_fun = y_lab_fun) +
      labs(subtitle = facet.name)
    if (is.function(after_fun)) {
      if (is.list(after_dat)) {
        data = after_dat_ls[[.x]]
        pfacet = pfacet + after_fun(data)
      } else {
        pfacet = pfacet + after_fun()
      }
    }
    return(pfacet)
  })
  wrap_plots(plot_list, guides = guides, ncol = ncol, nrow = nrow, ...) &
    theme(plot.subtitle = element_markdown(hjust = 0.5, margin = margin(t = 10)))
}

#-----------------------------------------------------------------------------
#' Helper after_fun : pvalue
#' @param data
#' @importFrom ggpubr stat_pvalue_manual
#' @export
#-----------------------------------------------------------------------------
add_pval = function(data) {
  tryCatch(
    {
      stat_pvalue_manual(
        data,
        label = "p.adj.signif",
        inherit.aes = F,
        hide.ns = T,
        label.size = 8,
        bracket.size = 0.6,
        tip.length = 0.02,
        step.increase = 0.05,
        bracket.nudge.y = 1,
        vjust = 0.65 
      )
    },
    error = function(err) {
   }
  )
}
#-----------------------------------------------------------------------------
# debug
#-----------------------------------------------------------------------------
if (FALSE) {
  library(ggRetro)
  library(ggplot2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(glue)
  library(patchwork)
  library(ggthemes)
  library(ohmyggplot)
  library(hrbrthemes)
  library(ggtext)
  oh_my_ggplot()
  annot_tb = data.frame(x = c(18,24), y = c(4.5,3.0), am = c(0,1), lab = c("Hi", "There"))
  update_geom_defaults("point",list(fill = "gray28", size=3, stroke=.6, shape=21))
  update_geom_defaults("smooth",list(color = "firebrick", fill = "firebrick", alpha = 0.05))
  p = mtcars |>
    # mutate(carb = as.factor(carb)) |>
    ggplot(aes(as.character(am), wt)) +
    geom_point()
    # geom_text(data = annot_tb, aes(x, y, label = lab))
    # geom_smooth(se = T)
  p2 = base_mode(p) + ggplot2::coord_cartesian(clip = "off")

  ggsave("./test.pdf", p2, w = 10, h = 8)

  facets = c("am", "vs")
  scales = "free_"
  label_format_number = "{var.name} = {var.value}"
  label_format_string = "{var.value}"
  label_column = NA
  smart_label = T
  guides = "auto"
  nrow = "auto"
  ncol = "auto"

  p |> base_facet(c("am", "vs"), scales = "free")

  face = p + facet_wrap(~am, scale = "fixed")
  bface = base_mode(face)
  ggplot_build(bface)
  bface$layers[[3]]$aes_params
  ggplot_build(p)
    base_mode()

  p |>
    base_facet(c("am", "vs"), guides = "auto", nrow = 2, scales = "fixed")
  
  facets = c("am", "vs")

  base_facet(p2,"am")
}

