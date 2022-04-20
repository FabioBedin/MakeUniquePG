#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)




DuplicateToUnique <- function(proteinGroups) {
  PG_subset <- proteinGroups %>% dplyr::select(`Protein IDs`, `Gene names`, `id`)

  PG_unique <- PG_subset %>%
    dplyr::rename(gene_name_unique = `Gene names`, protein_id = `Protein IDs`) %>%
    janitor::get_dupes(gene_name_unique) %>%
    dplyr::mutate(gene_name_unique = dplyr::case_when(
      gene_name_unique != "" ~ paste0(gene_name_unique, "(", str_extract(protein_id, "[^;]*"), ")"),
      TRUE ~ str_extract(protein_id, "[^;]*")
    )) %>%
    dplyr::select(gene_name_unique, id)

  return(PG_unique)

}


MakeUniqueGeneNameinPG <- function(proteinGroups) {
  PG_unique <- DuplicateToUnique(proteinGroups)

  PG_final <- dplyr::left_join(proteinGroups, PG_unique, by = "id") %>%
    dplyr::mutate(`Gene names` = dplyr::case_when(
      !is.na(gene_name_unique) ~ gene_name_unique,
      TRUE ~ `Gene names`
    )) %>%
    dplyr::select(-gene_name_unique)

  return(PG_final)
}


ListDupes <- function(proteinGroups) {
  dupe_list <- proteinGroups %>%
    tibble::as_tibble() %>%
    dplyr::select(`Gene names`) %>%
    janitor::get_dupes(`Gene names`) %>%
    dplyr::filter(!`Gene names` == "") %>%
    dplyr::select(`Gene names`) %>%
    dplyr::distinct()

  return(dupe_list)
}

EmptyNames <- function(proteinGroups) {
  empty_list <- proteinGroups %>%
    dplyr::select(`Gene names`, `Protein IDs`) %>%
    janitor::get_dupes(`Gene names`) %>%
    dplyr::filter(`Gene names` == "") %>%
    dplyr::filter(!str_detect(`Protein IDs`, "CON_") & !str_detect(`Protein IDs`, "REV_")) %>%
    nrow()
}

BeforeTable <- function(proteinGroups) {
  a <- ListDupes(proteinGroups) %>% pull(`Gene names`)

  before_table <- proteinGroups %>%
    dplyr::select(starts_with(c("Gene names", "Protein IDs"))) %>%
    dplyr::filter(`Gene names` %in% a) %>%
    dplyr::arrange(`Gene names`) %>%
    DT::datatable(style = "bootstrap4", options = list(
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', width = '100px', targets = 1)),
      scrollX = 500,
      pageLength = 10))

  return(before_table)
}


AfterTable <- function(proteinGroups) {
  a <- ListDupes(proteinGroups) %>% pull(`Gene names`)

  PG <- MakeUniqueGeneNameinPG(proteinGroups)

  after_table <- PG %>%
    dplyr::select(starts_with(c("Gene names", "Protein IDs"))) %>%
    dplyr::filter(str_detect(`Gene names`, "\\(")) %>%
    dplyr::arrange(`Gene names`) %>%
    DT::datatable(style = "bootstrap4", options = list(
      columnDefs = list(list(className = 'dt-center', targets = 1)),
      scrollX = 500,
      pageLength = 10))

  return(after_table)
}


eChartFunction <- function(proteinGroups, gene) {

  plot <- MakeUniqueGeneNameinPG(proteinGroups) %>%
    select(`Gene names`,
           `Sequence coverage [%]`,
           `Unique sequence coverage [%]`,
           `Sequence length`) %>%
    filter(str_detect(`Gene names`, paste0(gene,"\\("))) %>%
    group_by(`Gene names`) %>%
    echarts4r::e_charts(x = `Gene names`) %>%
    # e_theme_custom('{"color":["#6c757d","#28a745","#ffc107"]}') %>%
    echarts4r::e_theme_custom('{
    "color": [
        "#28a745",
        "#007bff",
        "#ffc107",
        "#8dc1a9",
        "#ea7e53",
        "#eedd78",
        "#73a373",
        "#73b9bc",
        "#7289ab",
        "#91ca8c",
        "#f49f42"
    ],
    "backgroundColor": "#343a40",
    "textStyle": {},
    "title": {
        "textStyle": {
            "color": "#eeeeee"
        },
        "subtextStyle": {
            "color": "#aaaaaa"
        }
    },
    "line": {
        "itemStyle": {
            "borderWidth": 1
        },
        "lineStyle": {
            "width": 2
        },
        "symbolSize": 4,
        "symbol": "circle",
        "smooth": false
    },
    "radar": {
        "itemStyle": {
            "borderWidth": 1
        },
        "lineStyle": {
            "width": 2
        },
        "symbolSize": 4,
        "symbol": "circle",
        "smooth": false
    },
    "bar": {
        "itemStyle": {
            "barBorderWidth": 0,
            "barBorderColor": "#ccc"
        }
    },
    "pie": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "scatter": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "boxplot": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "parallel": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "sankey": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "funnel": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "gauge": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        }
    },
    "candlestick": {
        "itemStyle": {
            "color": "#fd1050",
            "color0": "#0cf49b",
            "borderColor": "#fd1050",
            "borderColor0": "#0cf49b",
            "borderWidth": 1
        }
    },
    "graph": {
        "itemStyle": {
            "borderWidth": 0,
            "borderColor": "#ccc"
        },
        "lineStyle": {
            "width": 1,
            "color": "#aaa"
        },
        "symbolSize": 4,
        "symbol": "circle",
        "smooth": false,
        "color": [
            "#28a745",
            "#dc3545",
            "#ffc107",
            "#8dc1a9",
            "#ea7e53",
            "#eedd78",
            "#73a373",
            "#73b9bc",
            "#7289ab",
            "#91ca8c",
            "#f49f42"
        ],
        "label": {
            "color": "#eee"
        }
    },
    "map": {
        "itemStyle": {
            "areaColor": "#eee",
            "borderColor": "#444",
            "borderWidth": 0.5
        },
        "label": {
            "color": "#000"
        },
        "emphasis": {
            "itemStyle": {
                "areaColor": "rgba(255,215,0,0.8)",
                "borderColor": "#444",
                "borderWidth": 1
            },
            "label": {
                "color": "rgb(100,0,0)"
            }
        }
    },
    "geo": {
        "itemStyle": {
            "areaColor": "#eee",
            "borderColor": "#444",
            "borderWidth": 0.5
        },
        "label": {
            "color": "#000"
        },
        "emphasis": {
            "itemStyle": {
                "areaColor": "rgba(255,215,0,0.8)",
                "borderColor": "#444",
                "borderWidth": 1
            },
            "label": {
                "color": "rgb(100,0,0)"
            }
        }
    },
    "categoryAxis": {
        "axisLine": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisTick": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisLabel": {
            "show": true,
            "color": "#eeeeee"
        },
        "splitLine": {
            "show": true,
            "lineStyle": {
                "color": [
                    "#aaaaaa"
                ]
            }
        },
        "splitArea": {
            "show": false,
            "areaStyle": {
                "color": [
                    "#eeeeee"
                ]
            }
        }
    },
    "valueAxis": {
        "axisLine": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisTick": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisLabel": {
            "show": true,
            "color": "#eeeeee"
        },
        "splitLine": {
            "show": true,
            "lineStyle": {
                "color": [
                    "#aaaaaa"
                ]
            }
        },
        "splitArea": {
            "show": false,
            "areaStyle": {
                "color": [
                    "#eeeeee"
                ]
            }
        }
    },
    "logAxis": {
        "axisLine": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisTick": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisLabel": {
            "show": true,
            "color": "#eeeeee"
        },
        "splitLine": {
            "show": true,
            "lineStyle": {
                "color": [
                    "#aaaaaa"
                ]
            }
        },
        "splitArea": {
            "show": false,
            "areaStyle": {
                "color": [
                    "#eeeeee"
                ]
            }
        }
    },
    "timeAxis": {
        "axisLine": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisTick": {
            "show": true,
            "lineStyle": {
                "color": "#eeeeee"
            }
        },
        "axisLabel": {
            "show": true,
            "color": "#eeeeee"
        },
        "splitLine": {
            "show": true,
            "lineStyle": {
                "color": [
                    "#aaaaaa"
                ]
            }
        },
        "splitArea": {
            "show": false,
            "areaStyle": {
                "color": [
                    "#eeeeee"
                ]
            }
        }
    },
    "toolbox": {
        "iconStyle": {
            "borderColor": "#999"
        },
        "emphasis": {
            "iconStyle": {
                "borderColor": "#666"
            }
        }
    },
    "legend": {
        "textStyle": {
            "color": "#eeeeee"
        }
    },
    "tooltip": {
        "axisPointer": {
            "lineStyle": {
                "color": "#eeeeee",
                "width": "1"
            },
            "crossStyle": {
                "color": "#eeeeee",
                "width": "1"
            }
        }
    },
    "timeline": {
        "lineStyle": {
            "color": "#eeeeee",
            "width": 1
        },
        "itemStyle": {
            "color": "#dd6b66",
            "borderWidth": 1
        },
        "controlStyle": {
            "color": "#eeeeee",
            "borderColor": "#eeeeee",
            "borderWidth": 0.5
        },
        "checkpointStyle": {
            "color": "#e43c59",
            "borderColor": "#c23531"
        },
        "label": {
            "color": "#eeeeee"
        },
        "emphasis": {
            "itemStyle": {
                "color": "#a9334c"
            },
            "controlStyle": {
                "color": "#eeeeee",
                "borderColor": "#eeeeee",
                "borderWidth": 0.5
            },
            "label": {
                "color": "#eeeeee"
            }
        }
    },
    "visualMap": {
        "color": [
            "#bf444c",
            "#d88273",
            "#f6efa6"
        ]
    },
    "dataZoom": {
        "backgroundColor": "rgba(47,69,84,0)",
        "dataBackgroundColor": "rgba(255,255,255,0.3)",
        "fillerColor": "rgba(167,183,204,0.4)",
        "handleColor": "#a7b7cc",
        "handleSize": "100%",
        "textStyle": {
            "color": "#eeeeee"
        }
    },
    "markPoint": {
        "label": {
            "color": "#eee"
        },
        "emphasis": {
            "label": {
                "color": "#eee"
            }
        }
    }
}') %>%
    echarts4r::e_bar(
      `Sequence coverage [%]`,
      name = "Sequence coverage [%]",
      itemStyle = list(borderColor = "black", borderWidth = '1'),
      stack = "name"
    ) %>%
    echarts4r::e_bar(
      `Unique sequence coverage [%]`,
      name = "Unique sequence coverage [%]",
      itemStyle = list(borderColor = "black", borderWidth = '1'),
      stack = "name2"
    ) %>%
    echarts4r::e_bar(
      `Sequence length`,
      name = "Sequence length",
      itemStyle = list(borderColor = "black", borderWidth = '1'),
      stack = "length"
    ) %>%
    echarts4r::e_flip_coords() %>%
    echarts4r:: e_labels(
      position = "insideRight",
      formatter = htmlwidgets::JS("function(params){return(params.value[0] + '')}")
    )

  return(plot)
}


