# ==============================================================================
# Vestal House Project — Anello Data Solutions LLC
# ==============================================================================

# ---- Plans module: high-resolution zoomable viewer for the SD sheet set ----
# Sheets are 200-DPI PNG renders (7200x4800 px) of the Griffiths SD set in
# plans/, served via addResourcePath("plans", "plans") in app.R.
# Viewer: scroll wheel zooms about the cursor, drag pans, double-click resets.

PLAN_SHEETS <- c(
  "A100 — Basement Plan"     = "A100_basement_plan.png",
  "A101 — First Floor Plan"  = "A101_first_floor_plan.png",
  "A102 — Second Floor Plan" = "A102_second_floor_plan.png",
  "A201 — Elevations"        = "A201_elevations.png"
)

mod_plans_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        div(class = "d-flex align-items-center gap-3 flex-wrap",
          selectInput(ns("sheet"), NULL, choices = PLAN_SHEETS, width = "300px"),
          div(class = "btn-group",
            actionButton(ns("zoom_in"), "+", class = "btn-sm"),
            actionButton(ns("zoom_out"), "−", class = "btn-sm"),
            actionButton(ns("reset"), "Reset", class = "btn-sm")
          ),
          uiOutput(ns("full_link"), inline = TRUE),
          span(class = "text-muted small",
               "Wheel = zoom · drag = pan · double-click = reset")
        )
      ),
      div(id = ns("viewport"),
          style = paste("height: 78vh; overflow: hidden; cursor: grab;",
                        "background: #f4f4f4; position: relative;",
                        "border-radius: 0 0 6px 6px;"),
          tags$img(id = ns("sheet_img"),
                   src = paste0("plans/", PLAN_SHEETS[[1]]),
                   style = paste("position: absolute; top: 0; left: 0;",
                                 "transform-origin: 0 0; max-width: none;",
                                 "user-select: none; -webkit-user-drag: none;"))
      )
    ),
    tags$script(HTML(sprintf("
(function() {
  var vp = document.getElementById('%s');
  var img = document.getElementById('%s');
  var scale = 1, tx = 0, ty = 0, dragging = false, lastX = 0, lastY = 0;

  function apply() {
    img.style.transform = 'translate(' + tx + 'px,' + ty + 'px) scale(' + scale + ')';
  }
  function fit() {
    // Fit sheet width to viewport at load/reset
    var w = img.naturalWidth || 7200;
    scale = vp.clientWidth / w;
    tx = 0; ty = 0;
    apply();
  }
  function zoomAt(cx, cy, factor) {
    var newScale = Math.min(Math.max(scale * factor, 0.02), 4);
    factor = newScale / scale;
    // Keep the point under the cursor fixed while zooming
    tx = cx - factor * (cx - tx);
    ty = cy - factor * (cy - ty);
    scale = newScale;
    apply();
  }

  img.addEventListener('load', fit);
  if (img.complete) fit();

  vp.addEventListener('wheel', function(e) {
    e.preventDefault();
    var rect = vp.getBoundingClientRect();
    zoomAt(e.clientX - rect.left, e.clientY - rect.top,
           e.deltaY < 0 ? 1.2 : 1 / 1.2);
  }, { passive: false });

  vp.addEventListener('mousedown', function(e) {
    dragging = true; lastX = e.clientX; lastY = e.clientY;
    vp.style.cursor = 'grabbing'; e.preventDefault();
  });
  window.addEventListener('mousemove', function(e) {
    if (!dragging) return;
    tx += e.clientX - lastX; ty += e.clientY - lastY;
    lastX = e.clientX; lastY = e.clientY;
    apply();
  });
  window.addEventListener('mouseup', function() {
    dragging = false; vp.style.cursor = 'grab';
  });
  vp.addEventListener('dblclick', fit);

  // Touch support (pinch + one-finger pan)
  var lastDist = null;
  vp.addEventListener('touchstart', function(e) {
    if (e.touches.length === 1) {
      dragging = true; lastX = e.touches[0].clientX; lastY = e.touches[0].clientY;
    }
  }, { passive: true });
  vp.addEventListener('touchmove', function(e) {
    if (e.touches.length === 1 && dragging) {
      tx += e.touches[0].clientX - lastX; ty += e.touches[0].clientY - lastY;
      lastX = e.touches[0].clientX; lastY = e.touches[0].clientY;
      apply();
    } else if (e.touches.length === 2) {
      var dx = e.touches[0].clientX - e.touches[1].clientX;
      var dy = e.touches[0].clientY - e.touches[1].clientY;
      var dist = Math.sqrt(dx * dx + dy * dy);
      if (lastDist) {
        var rect = vp.getBoundingClientRect();
        zoomAt((e.touches[0].clientX + e.touches[1].clientX) / 2 - rect.left,
               (e.touches[0].clientY + e.touches[1].clientY) / 2 - rect.top,
               dist / lastDist);
      }
      lastDist = dist;
      e.preventDefault();
    }
  }, { passive: false });
  vp.addEventListener('touchend', function() { dragging = false; lastDist = null; });

  // Hooks for Shiny buttons / sheet switching
  window['plansViewer_%s'] = {
    zoom: function(f) {
      zoomAt(vp.clientWidth / 2, vp.clientHeight / 2, f);
    },
    reset: fit,
    setSrc: function(src) { img.src = src; }
  };
})();
", ns("viewport"), ns("sheet_img"), id)))
  )
}

mod_plans_server <- function(id, p) {
  moduleServer(id, function(input, output, session) {
    hook <- sprintf("window['plansViewer_%s']", id)

    observeEvent(input$sheet, {
      shinyjs_run <- sprintf("%s.setSrc('plans/%s');", hook, input$sheet)
      session$sendCustomMessage("plans-eval", shinyjs_run)
    })
    observeEvent(input$zoom_in,
      session$sendCustomMessage("plans-eval", sprintf("%s.zoom(1.4);", hook)))
    observeEvent(input$zoom_out,
      session$sendCustomMessage("plans-eval", sprintf("%s.zoom(1/1.4);", hook)))
    observeEvent(input$reset,
      session$sendCustomMessage("plans-eval", sprintf("%s.reset();", hook)))

    output$full_link <- renderUI({
      tags$a(href = paste0("plans/", input$sheet), target = "_blank",
             class = "small", "Open full-resolution PNG in new tab")
    })
  })
}
