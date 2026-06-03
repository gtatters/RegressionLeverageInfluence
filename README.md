# Exploring Influential and Leverage Points

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

A single red point sits among a cloud of black data points. You can click
anywhere on the scatter plot to move the red point and instantly see how the
fitted regression line responds. Six standard regression diagnostic plots update
in real time so you can watch leverage and influence change as you drag the
point to different positions.

## Key concepts illustrated

- **Leverage** — how unusual a point's x-value is relative to the rest of the data
- **Influence** — how much a point actually changes the fitted line
- **Cook's distance** — a combined measure of leverage and influence
- **Residuals vs Leverage** and **Cook's d vs Leverage** plots

## How to use

1. Set the sample size with the slider and press **Resample** for a new dataset.
2. Click anywhere on the left-hand scatter plot to move the red point.
3. Choose a diagnostic plot from the radio buttons to see how that measure changes.
4. Switch between fitting the model to all data (red point included) or black
points only to compare the two fitted lines.

## Learning goals

- Understand that a point can have high leverage without being influential
- See that a point far from the center of x *and* far from the regression line
has the most influence
- Interpret Cook's distance and residual-vs-leverage plots

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).