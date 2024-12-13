use_desc_config_tests <- function() {

  desc::desc_set(
    "Config/testthat/edition",
    3
  )

  desc::desc_set(
    "Config/tests/needs",
    paste0(
      "testthat (>= 3.0.0),\n",
      "spelling,\n",
      "httptest2,\n",
      "shinytest2\n"
    )
  )

}

use_desc_config <- function(config, packages) {



}
