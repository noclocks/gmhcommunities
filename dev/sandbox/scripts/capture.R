httptest2::capture_requests({
  real_resp <- httr2::request("http://httpbin.org/cookies/set") |>
    httr2::req_url_query(token = "12345") |>
    # httpbin normally does a 302 redirect after this request,
    # but let's prevent that just to illustrate
    httr2::req_options(followlocation = FALSE) |>
    httr2::req_perform()
})
httr2::resp_headers(real_resp)
