#  ------------------------------------------------------------------------
#
# Title : Google Drive Script
#  File : gdrive.R
#    By : Jimmy Briggs <jimmy.briggs@noclocks.dev>
#  Date : 2024-11-23
#
#  ------------------------------------------------------------------------

library(googledrive)
options(gargle_verbosity = "debug")
options(gargle_oauth_email = "jimmy.briggs@noclocks.dev")

drive_auth_configure(
  api_key = "AIzaSyDMubKVOUKDlkOUxtD4PXJiCQK5KXXAj6c"
)

drive_auth(
  email = "jimmy.briggs@noclocks.dev",
  path = "data-raw/noclocks-gcp-gdrive-credentials.json"
)

drive_user()
drive_find(pattern = "UNLOCKED")

drive_download(
  file =
)

file <- drive_get(as_id("12gfiWFR7hm3TW_wkFZpzeDw9X1b3qnSz4gk1HuBA9Ls"))
drive_download(file, path = "data-raw/data/original/UNLOCKED-1047-Commonwealth-Market-Survey.xlsm", type = "application/vnd.google-apps.spreadsheet"
)
file |>
  dplyr::pull(drive_resource) |>
  purrr::pluck(1) |>
  purrr::pluck("mimeType")
