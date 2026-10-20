# Gotchas

Places where the natural first approach fails. Add an entry only when that happened in real use: the problem as the move that failed, then what works (or that nothing does yet).

## `points` doesn't shrink Seurat DotPlot dots
Use `mapped point sizes: 0.5` instead: the dots are sized from the data, so a fixed point size has no effect. Relative sizes and the size legend stay correct.

## `points` stops shrinking dots below about 0.5 mm
ggplot2's default point outline (`stroke = 0.5`) sets the minimum size. No toolkit setting reaches the outline yet.

## Line-width settings don't change colour-bar ticks
`axis lines` and `lines` don't reach them; they stay at about 0.38 pt. No toolkit setting reaches them yet.

## The before/after safety check shows Euler plots as changed
`eulerr::euler()` uses random numbers, so its layout differs at every render. Compare Euler plots by eye, or set a seed before the `euler()` call (a change to the analysis code, so it's your call).

## A chunk that draws several plots gets only its first plot assembled
Combine them into one plot at the end of the chunk: `gridExtra::grid.arrange()` for Euler plots, `patchwork::wrap_plots()` for ggplot2 plots, and `grid.arrange()` of each pheatmap's `$gtable` (drawn with `silent = TRUE`). Moving the legend to annotations then keeps only the last plot's legend.
