usethis::use_directory("srcjs", TRUE)
fs::dir_create("srcjs/src")
fs::dir_create("srcjs/dist")
fs::dir_create("srcjs/src/components")
fs::dir_create("srcjs/src/inputs")
fs::dir_create("srcjs/src/utils")

fs::file_create("srcjs/README.md")
fs::file_create("srcjs/src/index.ts")

currwd <- getwd()
setwd("srcjs")
on.exit(setwd(currwd))

npm::npm_init()
npm::npm_install(
  scope = "prod",
  c(
    "@types/rstudio-shiny",
    "jquery",
    "sortablejs"
  )
)
npm::npm_install(
  scope = "dev",
  c(
    "typescript",
    "esbuild",
    "eslint",
    "eslint-config-standard",
    "eslint-plugin-import",
    "eslint-plugin-n",
    "eslint-plugin-promise",
    "jest",
    "jest-environment-jsdom",
    "open-props"
  )
)
# "eslint",
# "eslint-config-prettier",
# "eslint-plugin-prettier",
# "@typescript-eslint/eslint-plugin",
# "@typescript-eslint/parser",
# "prettier",
# "prettier-plugin-organize-imports",
