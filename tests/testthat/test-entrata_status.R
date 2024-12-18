mocks_dir <- fs::path(root_mock_path, "status")
mock_files <- fs::dir_ls(mocks_dir, type = "file", glob = "*.json")

httptest2::capture_requests(
  entrata_status(request_id = "TEST")
)

test_that("entrata_status() works properly", {

  httptest2::with_mock_api({
    status_resp_content <- entrata_status(request_id = "TEST")
    status_resp <- httr2::last_response()
    status_req <- httr2::last_request()
  })

  expect_httr2_request(status_req)
  expect_httr2_response(status_resp)
  expect_entrata_api_response(status_resp)
  expect_true(httr2::resp_status(status_resp) == 200)

})
