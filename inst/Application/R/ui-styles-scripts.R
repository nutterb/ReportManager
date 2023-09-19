UI_STYLES_AND_SCRIPTS <- 
  tagList(
    useShinyjs(), 
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "collapsible-div.css"),
    tags$script(src = "collapsible-div.js"),
    shiny::tags$script(type = "text/javascript",
                       src = "https://cdn.jsdelivr.net/momentjs/latest/moment.min.js"),
    shiny::tags$script(type = "text/javascript",
                       src = "https://cdn.jsdelivr.net/npm/daterangepicker/daterangepicker.min.js"),
    # I'm not sure if I need this or not
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('pager',function(page) {
      $('#'+'test').find('table').DataTable().page(page).draw(false);
    })")),
    shiny::tags$link(rel = "stylesheet",
                     type = "text/css",
                     href = "https://cdn.jsdelivr.net/npm/daterangepicker/daterangepicker.css"),
    tags$style("body {margin:0px; padding:0px;}"),
    tags$style("label {color:black}"),
    tags$style(".nowrap { white-space: nowrap;}")
  )