# Gotchas

Places where the natural first approach fails. Add an entry only when that happened in real use: the problem as the move that failed, then what works (or that nothing does yet).

## `points` doesn't shrink Seurat DotPlot dots
Use `mapped point sizes: 0.5` instead: the dots are sized from the data, so a fixed point size has no effect. Relative sizes and the size legend stay correct.

## `points` stops shrinking dots below about 0.5 mm
ggplot2's default `stroke` (0.5) adds a fixed amount to every dot, and a border to shape 19; shape 16 alone does not help, because the stroke still enlarges the dot. The toolkit now gives solid dots (shapes 15–20, shape and size not mapped) no stroke whenever `points` is set, so the size alone sets the dot.

## Line-width settings don't change colour-bar ticks
ggplot2 keeps them as a separate legend element (`legend.ticks`), so they stayed at about 0.38 pt. The toolkit now sets them with `axis lines`: one width for axis lines, axis ticks and colour-bar ticks.

## A figure plot changes at every render, although nothing changed
Something in it uses random numbers, e.g. `eulerr::euler()` layouts, jitter, repelled labels or subsampling. Seed it: `set.seed()` right before, or the function's `seed` argument. The safety check then passes, and draft and final figures match.

## A chunk that draws several plots gets only its first plot assembled
Combine them into one plot at the end of the chunk: `gridExtra::grid.arrange()` for Euler plots, `patchwork::wrap_plots()` for ggplot2 plots, and `grid.arrange()` of each pheatmap's `$gtable` (drawn with `silent = TRUE`). Moving the legend to annotations then keeps only the last plot's legend.
