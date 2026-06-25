# =========================================================
# Shiny App: Exploring Influential and Leverage Points
# Requires: shiny only (all plots use base R graphics)
# =========================================================

# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
# Run the shinylive::export line to populate the docs folder 
# so that shinylive works from github
#shinylive::export(appdir = "../RegressionLeverageInfluence/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)

# ---------------------------
# Data generator
# ---------------------------
generate_data <- function(n) {
  x      <- 1:n
  y      <- x + rnorm(n, sd = n / 4)
  colour <- rep("black", n)
  
  x      <- c(x, mean(x))
  y      <- c(y, mean(y))
  colour <- c(colour, "red")
  
  data.frame(x = x, y = y, colour = colour,
             stringsAsFactors = FALSE)
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #f7f7f7;
        padding: 15px;
        border-radius: 8px;
        border: 1px solid #ddd;
      }
      #resample {
        background-color: #569BBD;
        color: white;
        border: none;
        padding: 6px 12px;
        margin-top: 4px;
        margin-bottom: 10px;
        font-size: 14px;
      }
      #resample:hover {
        background-color: #3E7C99;
        color: white;
      }
      #resample:active {
        transform: scale(0.97);
      }
    "))
  ),
  
  titlePanel("Exploring Influential and Leverage Points"),
  
  fluidRow(
    
    # ---------------------------
    # SIDEBAR
    # ---------------------------
    column(
      width = 3,
      div(class = "sidebar",
          
          p(strong("Diagnosing Influential Data points")),
          p("Click anywhere on the left plot to move the red point and observe how it affects the regression and diagnostics."),
          p("Change the sample size or Resample to reset the data to examine influence with different data sets"),
          #hr(),
          sliderInput("sample_size", "Sample size:",
                      min = 5, max = 100, value = 10),
          
          actionButton("resample", "Resample"),
          
          tags$hr(),
          
          radioButtons("choice", "Diagnostic to plot",
                       choiceNames = c(
                         "Residual vs. Fitted",
                         "Normal Q-Q",
                         "Scale-Location",
                         "Cook's distance",
                         "Residual vs Leverage",
                         "Cook's d vs Leverage"
                       ),
                       choiceValues = 1:6,
                       selected = 1),
          
          radioButtons("model_choice", "Model for diagnostics",
                       choices = c(
                         "All data (red point included)" = "all",
                         "Black points only"             = "black"
                       ),
                       selected = "all"),
          
          helpText("_________________________"),
          helpText("Glenn Tattersall, PhD"),
          helpText("For use in BIOL 3P96 - Biostatistics")
      )
    ),
    
    # ---------------------------
    # MAIN PANEL
    # ---------------------------
    column(
      width = 9,
      #p(HTML("&#x1F449; Click anywhere on the left plot to move the <span style='color:red;'>red point (&#9650)</span> and observe how it affects the regression and diagnostics."),
      #  style = "text-align:center; color:#555; font-style:italic; font-weight: bold; margin-bottom:6px;"),
      fluidRow(
        
        # Left: raw data plot + summary table
        column(
          width = 6,
          plotOutput("rawPlot", height = "500px", click = "plot_click"),
          tags$div(
            style = "background-color:#f9f9f9;
                     padding:12px;
                     border-radius:6px;
                     border:1px solid #ddd;
                     margin-top:10px;",
            tableOutput("effect_table")
          )
        ),
        
        # Right: diagnostic plot + interpretation
        column(
          width = 6,
          plotOutput("diagnostic_plot", height = "500px"),
          tags$div(
            style = "background-color:#f9f9f9;
                     padding:12px;
                     border-radius:6px;
                     border:1px solid #ddd;
                     margin-top:10px;",
            strong("How to interpret this plot:"),
            br(),
            textOutput("diag_explanation")
          )
        )
      )
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  diag_titles <- c(
    "Residual vs. Fitted",
    "Normal Q-Q",
    "Scale-Location",
    "Cook's distance",
    "Residual vs Leverage",
    "Cook's d vs Leverage"
  )
  
  # ---------------------------
  # Reactive data store
  # ---------------------------
  plot_data <- reactiveVal(generate_data(10))
  
  observeEvent(input$sample_size, {
    plot_data(generate_data(input$sample_size))
  })
  
  observeEvent(input$resample, {
    plot_data(generate_data(input$sample_size))
  })
  
  # Clicking the raw plot moves the red point
  observeEvent(input$plot_click, {
    dat <- plot_data()
    dat[nrow(dat), "x"] <- input$plot_click$x
    dat[nrow(dat), "y"] <- input$plot_click$y
    plot_data(dat)
  })
  
  # ---------------------------
  # Fitted models
  # ---------------------------
  mods <- reactive({
    dat <- plot_data()
    req(nrow(dat) > 2)
    list(
      mod_all   = lm(y ~ x, data = dat),
      mod_black = lm(y ~ x, data = dat[dat$colour == "black", ])
    )
  })
  
  # Active model (based on radio button)
  active_mod <- reactive({
    if (input$model_choice == "all") mods()$mod_all else mods()$mod_black
  })
  
  model_label <- reactive({
    if (input$model_choice == "all") " — All" else " — Black"
  })
  
  # ---------------------------
  # Raw data plot (base R)
  # ---------------------------
  output$rawPlot <- renderPlot({
    
    dat    <- plot_data()
    black  <- dat[dat$colour == "black", ]
    red    <- dat[dat$colour == "red",   ]
    
    n      <- input$sample_size
    xlim   <- c(min(dat$x) - n,     max(dat$x) + n)
    ylim   <- c(min(dat$y) - 2 * n, max(dat$y) + 2 * n)
    
    # Helper: compute 95% CI band over a fine x grid for a given model
    ci_band <- function(mod, x_seq) {
      as.data.frame(predict(mod,
                            newdata = data.frame(x = x_seq),
                            interval = "confidence",
                            level    = 0.95))
    }
    
    x_seq     <- seq(xlim[1], xlim[2], length.out = 200)
    ci_all    <- ci_band(mods()$mod_all,   x_seq)
    ci_black  <- ci_band(mods()$mod_black, x_seq)
    
    plot(dat$x, dat$y,
         col  = dat$colour,
         pch  = ifelse(dat$colour == "red", 17, 16),
         cex  = ifelse(dat$colour == "red", 2,  1.2),
         xlim = xlim, ylim = ylim,
         xlab = "x", ylab = "y",
         main = paste0("Data and fitted regression lines", model_label()),
         las  = 1)
    
    # Shaded 95% CI — all data (red, semi-transparent)
    polygon(c(x_seq, rev(x_seq)),
            c(ci_all$lwr, rev(ci_all$upr)),
            col    = adjustcolor("red",   alpha.f = 0.15),
            border = NA)
    
    # Shaded 95% CI — black only (grey, semi-transparent)
    polygon(c(x_seq, rev(x_seq)),
            c(ci_black$lwr, rev(ci_black$upr)),
            col    = adjustcolor("black", alpha.f = 0.10),
            border = NA)
    
    # Regression lines
    lines(x_seq, ci_all$fit,   col = "red",   lwd = 2)
    lines(x_seq, ci_black$fit, col = "black", lwd = 2)
    
    # Redraw points on top of shaded bands
    points(dat$x, dat$y,
           col = dat$colour,
           pch = ifelse(dat$colour == "red", 17, 16),
           cex = ifelse(dat$colour == "red", 2,  1.2))
    
    legend("topleft",
           legend = c("All data fit", "Black only fit", "Influential point"),
           col    = c("red", "black", "red"),
           lty    = c(1, 1, NA),
           pch    = c(NA, NA, 17),
           lwd    = 2, bty = "n", cex = 0.85)
  })
  
  # ---------------------------
  # Summary table
  # ---------------------------
  output$effect_table <- renderTable({
    
    req(mods())
    dat     <- plot_data()
    n_black <- sum(dat$colour == "black")
    n_all   <- nrow(dat)
    
    cf_all   <- coef(mods()$mod_all)
    cf_black <- coef(mods()$mod_black)
    r2_all   <- summary(mods()$mod_all)$r.squared
    r2_black <- summary(mods()$mod_black)$r.squared
    
    df <- data.frame(
      Model     = c("All data", "Black only", "\u0394 (All \u2212 Black)"),
      n         = c(n_all, n_black, n_all - n_black),
      Intercept = c(cf_all["(Intercept)"],
                    cf_black["(Intercept)"],
                    cf_all["(Intercept)"] - cf_black["(Intercept)"]),
      Slope     = c(cf_all["x"],
                    cf_black["x"],
                    cf_all["x"] - cf_black["x"]),
      R2        = c(r2_all, r2_black, r2_all - r2_black),
      stringsAsFactors = FALSE
    )
    df
    
  }, digits = 3)
  
  # ---------------------------
  # Diagnostic data
  # ---------------------------
  diag_data <- reactive({
    
    mod <- active_mod()
    dat <- plot_data()
    n   <- length(resid(mod))
    
    fitted_vals <- fitted(mod)
    resids      <- resid(mod)
    std_resids  <- rstandard(mod)
    cooksd      <- cooks.distance(mod)
    hat         <- hatvalues(mod)
    
    lev_thresh  <- 2 * mean(hat)
    cook_thresh <- 4 / n
    
    # Index of the red point within this model's data.
    # integer(0) when black-only model is active — no red point present.
    if (input$model_choice == "all") {
      red_idx <- which(dat$colour == "red")
    } else {
      red_idx <- integer(0)
    }
    
    list(
      df = data.frame(
        obs           = seq_along(fitted_vals),
        fitted        = fitted_vals,
        resid         = resids,
        stdresid      = std_resids,
        cooksd        = cooksd,
        hat           = hat,
        high_leverage = hat    > lev_thresh,
        high_cook     = cooksd > cook_thresh,
        stringsAsFactors = FALSE
      ),
      red_idx = red_idx
    )
  })
  
  # ---------------------------
  # Diagnostic plots (base R)
  # ---------------------------
  output$diagnostic_plot <- renderPlot({
    
    req(mods())
    dd      <- diag_data()
    d       <- dd$df
    ri      <- dd$red_idx          # row index of red point; integer(0) if absent
    has_red <- length(ri) > 0
    
    choice <- as.numeric(input$choice)
    title  <- paste0(diag_titles[choice], model_label())
    
    # Overlay the red triangle — called after each plot is drawn
    mark_red <- function(x_val, y_val) {
      points(x_val, y_val, col = "red", pch = 17, cex = 2)
    }
    
    # ── 1: Residual vs Fitted ──────────────────────────────────
    if (choice == 1) {
      plot(d$fitted, d$resid,
           xlab = "Fitted values", ylab = "Residuals",
           main = title, pch = 16, col = "steelblue", las = 1)
      abline(h = 0, lty = 2, col = "grey50")
      lines(lowess(d$fitted, d$resid), col = "red", lwd = 2)
      if (has_red) mark_red(d$fitted[ri], d$resid[ri])
    }
    
    # ── 2: Normal Q-Q ─────────────────────────────────────────
    if (choice == 2) {
      # qqnorm invisibly returns the plotted (x, y) coordinates
      # in the same row-order as the input, so [ri] is safe
      qq <- qqnorm(d$stdresid, main = title,
                   pch = 16, col = "steelblue", las = 1)
      qqline(d$stdresid, col = "red", lwd = 2)
      if (has_red) mark_red(qq$x[ri], qq$y[ri])
    }
    
    # ── 3: Scale-Location ─────────────────────────────────────
    if (choice == 3) {
      sqrt_abs_resid <- sqrt(abs(d$stdresid))
      plot(d$fitted, sqrt_abs_resid,
           xlab = "Fitted values",
           ylab = expression(sqrt("|Standardised residuals|")),
           main = title, pch = 16, col = "steelblue", las = 1)
      lines(lowess(d$fitted, sqrt_abs_resid), col = "red", lwd = 2)
      if (has_red) mark_red(d$fitted[ri], sqrt_abs_resid[ri])
    }
    
    # ── 4: Cook's distance ────────────────────────────────────
    if (choice == 4) {
      cook_thresh <- 4 / nrow(d)
      plot(d$obs, d$cooksd,
           type = "h", lwd = 2, col = "steelblue",
           xlab = "Observation", ylab = "Cook's distance",
           main = title, las = 1,
           ylim = c(0, max(d$cooksd) * 1.1))
      abline(h = cook_thresh, lty = 2, col = "red", lwd = 1.5)
      points(d$obs, d$cooksd, pch = 16, col = "steelblue")
      if (has_red) mark_red(d$obs[ri], d$cooksd[ri])
      legend("topright",
             legend = paste0("Threshold = ", round(cook_thresh, 3)),
             lty = 2, col = "red", bty = "n", cex = 0.85)
    }
    
    # ── 5: Residual vs Leverage ───────────────────────────────
    if (choice == 5) {
      lev_thresh <- 2 * mean(d$hat)
      plot(d$hat, d$stdresid,
           xlab = "Leverage", ylab = "Standardised residuals",
           main = title, pch = 16, col = "steelblue", las = 1)
      abline(h = 0,          lty = 2, col = "grey50")
      abline(v = lev_thresh, lty = 2, col = "red", lwd = 1.5)
      if (has_red) mark_red(d$hat[ri], d$stdresid[ri])
      legend("topright",
             legend = paste0("Leverage threshold = ", round(lev_thresh, 3)),
             lty = 2, col = "red", bty = "n", cex = 0.85)
    }
    
    # ── 6: Cook's d vs Leverage ───────────────────────────────
    if (choice == 6) {
      plot(d$hat, d$cooksd,
           xlab = "Leverage", ylab = "Cook's distance",
           main = title, pch = 16, col = "steelblue", las = 1)
      lev_thresh  <- 2 * mean(d$hat)
      cook_thresh <- 4 / nrow(d)
      abline(v = lev_thresh,  lty = 2, col = "red",    lwd = 1.5)
      abline(h = cook_thresh, lty = 2, col = "orange", lwd = 1.5)
      if (has_red) mark_red(d$hat[ri], d$cooksd[ri])
      legend("topright",
             legend = c(
               paste0("Leverage threshold = 2\u00d7mean(lev) = ", round(lev_thresh,  3)),
               paste0("Cook's D threshold = 4/n = ",              round(cook_thresh, 3))
             ),
             lty = 2, col = c("red", "orange"), bty = "n", cex = 0.85)
    }
  })
  
  # ---------------------------
  # Interpretation text
  # ---------------------------
  output$diag_explanation <- renderText({
    
    choice <- as.numeric(input$choice)
    d      <- diag_data()$df
    n      <- nrow(d)
    cook_thresh <- round(4 / n,           3)
    lev_thresh  <- round(2 * mean(d$hat), 3)
    
    switch(choice,
           "1" = paste0(
             "Residual vs Fitted: Random scatter is ideal. ",
             "Patterns (e.g., curves) suggest model misspecification, such as non-linearity. ",
             "Points far from zero indicate large residuals (potential outliers). ",
             "Clusters or structure imply the model is not capturing systematic variation."
           ),
           "2" = paste0(
             "Normal Q-Q: Points should follow the diagonal. ",
             "Systematic deviations (S-shape or heavy tails) indicate non-normal residuals. ",
             "Extreme points far from the line suggest outliers or influential observations. ",
             "Departures mainly at the ends indicate issues in the tails rather than the center."
           ),
           "3" = paste0(
             "Scale-Location: Look for constant spread. ",
             "A funnel (increasing spread) indicates heteroscedasticity (non-constant variance). ",
             "Horizontal banding is ideal. ",
             "If variance increases with fitted values, model assumptions are violated."
           ),
           "4" = paste0(
             "Influential if Cook's Distance > threshold = 4/n \u2248 ", cook_thresh, ". ",
             "Cook's D combines leverage and residual size to quantify overall influence. ",
             "Points above the threshold can disproportionately affect model estimates. ",
             "Even moderate values may matter in small samples."
           ),
           "5" = paste0(
             "High leverage + large residual = influential. Leverage threshold = 2 \u00d7 mean leverage \u2248 ", lev_thresh, ". ",
             "Leverage reflects how extreme a point is in x-space. ",
             "High leverage alone is not problematic unless paired with a large residual. ",
             "Points with high leverage and large residuals are most concerning."
           ),
           "6" = paste0(
             "Cook's D vs Leverage: points further from the origin have greater influence. ",
             "Dashed lines mark the leverage and Cook's D thresholds. ",
             "Points in the upper-right quadrant combine high leverage with high influence. ",
             "This plot integrates leverage and residual magnitude into a single diagnostic view."
           )
    )
  })
}

shinyApp(ui, server)