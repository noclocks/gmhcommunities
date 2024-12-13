library(dm)

conn <- db_connect()

mkt_dm <- dm::dm_from_con(conn)
names(mkt_dm)
mkt_dm$

mkt_dm |> dm::dm_examine_constraints()
