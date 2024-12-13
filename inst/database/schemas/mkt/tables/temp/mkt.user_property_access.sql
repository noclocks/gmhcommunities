CREATE TABLE mkt.user_property_access (
  user_id INT NOT NULL REFERENCES auth.users(user_id),
  gmh_property_id BIGINT NOT NULL REFERENCES gmh.properties(gmh_property_id),
  entrata_property_id BIGINT NOT NULL REFERENCES entrata.properties(entrata_property_id),
  mkt_property_id TEXT NOT NULL REFERENCES mkt.properties(property_id),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, property_id)
)
