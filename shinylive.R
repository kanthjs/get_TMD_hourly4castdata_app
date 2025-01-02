library(shinylive)

shinylive::export(appdir = "app",
       destdir = "docs")
#

library(httpuv)

runStaticServer("docs", port = 8080)
