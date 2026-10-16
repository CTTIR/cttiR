# Bundled front-end libraries

For v0.0.1 the annotation canvas (`atcanvas`) is implemented as a self-contained
HTML5 canvas in `inst/htmlwidgets/atcanvas.js`, with no external JavaScript
libraries. The widget therefore has no third-party front-end code to attribute
and works fully offline (no CDN requests).

## Planned for a later release

The locked design targets OpenSeadragon (BSD-3-Clause) for gigapixel deep-zoom
and Annotorious (BSD-3-Clause) for W3C Web Annotation drawing. When those are
vendored here, their exact versions and licences will be recorded in this file,
and attribution added to the README Acknowledgements. The R interface in
`R/widget-canvas.R` and the JS message protocol are already shaped for that
swap; see STATE.md ("Deferred to v0.0.2").
