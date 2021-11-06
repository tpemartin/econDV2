#' Generate geom function for secondary axis y on the right
#'
#' @param geom_fn a geom function
#'
#' @return
#' @export
#'
#' @examples
#' geom_line_right <- geom_sec_axis(geom_line)
geom_sec_axis <- function(geom_fn){
  function(..., mapping, name_left=waiver(), name_right=waiver(), labels_left=waiver(), labels_right=waiver(), breaks_left=waiver(), breaks_right=waiver(), expand=waiver()){
    list(
      {
        limits_left = range(breaks_left)
        limits_right =range(breaks_right)
        .GlobalEnv$transfer_fn <- function(x) scales::rescale(x, to=limits_right, from=limits_left)
        .GlobalEnv$scale_fn <- function(y) scales::rescale(y, to=limits_left, from=limits_right)


        mapping_scaled = scale_mapping_y(mapping)
        geom_right_scaled <- {
          geom_fn(
            mapping=mapping_scaled,
            ...
          )
        }
      },
      {
        scale_y_continuous(
          name=name_left,
          limits = limits_left,
          breaks = breaks_left,
          labels = labels_left,
          sec.axis = sec_axis(
            trans=transfer_fn, # map: first axis breaks to second axis breaks
            name=name_right,
            labels=labels_right # secondary axis labels
          ),
          expand = expand
        )
      })
  }
}

#' Generate scale_x function with flexible ticks and labels setting
#'
#' @param scale_x a scale_x function
#'
#' @return
#' @export
#'
#' @examples
#' axis_x_date_custom <- axis_x_custom(scale_x_date)
axis_x_custom <- function(scale_x){
  function(
    breaks, labels = breaks,
    ticks_major, ticks_minor=NULL,
    ticks_major_length = 3,
    minor_major_tickLength_ratio = 0.7,
    text_size = 16,
    text_top_margin = 12,
    major_tick_size = 0.5,
    minor_tick_size = 0.5, ...
  ){
    list(
      scale_x(
        breaks=breaks,
        labels=labels, ...
      ),
      theme(
        axis.ticks.length.x = unit(0,"mm"),
        axis.text.x = element_text(
          margin = margin(
            text_top_margin #input$margin
          ),
          size=text_size #input$textSize
        )
      ),
      geom_rug(
        data=data.frame(
          ticks_major=ticks_major
        ),
        mapping=aes(
          x=ticks_major
        ),
        outside=TRUE, # draw rug outside the plot panel
        size=major_tick_size, #input$majorsize
        length=grid::unit(
          ticks_major_length,
          "mm"
        )
      ),
      if(!is.null(ticks_minor)){
        geom_rug(
          data=data.frame(
            ticks_minor=ticks_minor
          ),
          mapping=aes(
            x=ticks_minor
          ),
          outside = TRUE,
          size=minor_tick_size,
          length=grid::unit(
            minor_major_tickLength_ratio*ticks_major_length,
            "mm"
          )
        )
      } else {
        NULL
      },
      coord_cartesian(clip="off")
    )
  }
}


# helpers -----------------------------------------------------------------


scale_mapping_y <- function(mapping)
{
  mapping$y |> rlang::quo_get_expr() -> y_source_expr
  rlang::quo(
    scale_fn(!!y_source_expr)
  ) -> new_quo
  rlang::quo_set_env(
    new_quo,
    env=rlang::quo_get_env(mapping$y)
  ) -> mapping$y
  mapping
}

# {
#   limits_left = range(breaks_left)
#   limits_right =range(breaks_right)
#   transfer_fn <- function(x) scales::rescale(x, to=limits_right, from=limits_left)
#   scale_fn <- function(y) scales:rescale(y, to=limits_left, from=limits_right)
# }

geom_line_right <- function(..., mapping, name_left=waiver(), name_right=waiver(), labels_left=waiver(), labels_right=waiver(), breaks_left=waiver(), breaks_right=waiver(), expand=waiver()){
  list(
    {
      limits_left = range(breaks_left)
      limits_right =range(breaks_right)
      .GlobalEnv$transfer_fn <- function(x) scales::rescale(x, to=limits_right, from=limits_left)
      .GlobalEnv$scale_fn <- function(y) scales::rescale(y, to=limits_left, from=limits_right)


      mapping_scaled = scale_mapping_y(mapping)
      geom_right_scaled <- {
        geom_line(
          mapping=mapping_scaled,
          ...
        )
      }
    },
    {
      scale_y_continuous(
        name=name_left,
        limits = limits_left,
        breaks = breaks_left,
        labels = labels_left,
        sec.axis = sec_axis(
          trans=transfer_fn, # map: first axis breaks to second axis breaks
          name=name_right,
          labels=labels_right # secondary axis labels
        ),
        expand = expand
      )
    })
}
