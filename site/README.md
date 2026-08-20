# lipdverse dev site

The front end served at https://lipdverse.org/dev/. Static: the browser reads
the export's parquet directly over HTTP range requests, so there is no API and
no server process.

    site/index.html      shell, styles, theme
    site/app.js          routing, collection and dataset views, search, downloads
    site/map.js          self-hosted Natural Earth map, zoom/pan
    site/plot.js         inline-SVG timeseries plots
    site/search.js       vocabulary facets
    site/downloads.json  which releases have a published zip/bib
    site/.htaccess.dev   the routing rule, deployed to dev/.htaccess

Not in git, because they are generated or vendored:

    site/lib/hyparquet/       MIT, npm hyparquet, plain ES modules
    site/data/*.geojson       Natural Earth 110m, TopoJSON -> GeoJSON at build
    site/data/values-index.json   datasetId -> row range in values.parquet

Deploy with rsync to /www/cefns.nau.edu/seses/lipdverse/dev/site/ and remember
the edge cache in front of lipdverse.org when testing.
