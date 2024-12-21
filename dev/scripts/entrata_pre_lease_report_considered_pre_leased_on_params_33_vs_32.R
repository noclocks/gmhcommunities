
pre_lease_data_lst_32 <- entrata_pre_lease_report(consider_pre_leased_on = 32)
pre_lease_data_lst_33 <- entrata_pre_lease_report(consider_pre_leased_on = 33)
pre_lease_data_lst_332 <- entrata_pre_lease_report(consider_pre_leased_on = "332")

pre_lease_summary_32 <- pre_lease_data_lst_32$summary
pre_lease_summary_33 <- pre_lease_data_lst_33$summary
pre_lease_summary_332 <- pre_lease_data_lst_332$summary

sum(pre_lease_summary_32$available_count) # incorrect
sum(pre_lease_summary_33$available_count) # incorrect
sum(pre_lease_summary_332$available_count) # correct

sum(pre_lease_summary_32$avg_scheduled_rent) # maybe?
sum(pre_lease_summary_33$avg_scheduled_rent) # maybe?
sum(pre_lease_summary_332$avg_scheduled_rent) # incorrect

sum(pre_lease_summary_32$scheduled_rent_total) # maybe?
sum(pre_lease_summary_33$scheduled_rent_total) # maybe?
sum(pre_lease_summary_332$scheduled_rent_total) # incorrect

available_counts <- pre_lease_summary_332$available_count

pre_lease_summary <- pre_lease_summary_32 |>
  dplyr::mutate(
    available_count = available_counts
  )

pre_lease_details_32 <- pre_lease_data_lst_32$details
pre_lease_details_33 <- pre_lease_data_lst_33$details
pre_lease_details_332 <- pre_lease_data_lst_332$details

sum(pre_lease_details_32$scheduled_rent_total)
sum(pre_lease_details_33$scheduled_rent_total)
sum(pre_lease_details_332$scheduled_rent_total)

# all equal

pre_lease_details <- pre_lease_details_332
