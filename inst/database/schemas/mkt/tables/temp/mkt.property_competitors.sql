CREATE TABLE mkt.property_competitors (
  property_id TEXT,
  competitor_id TEXT,
  PRIMARY KEY (property_id, competitor_id),
  FOREIGN KEY (property_id) REFERENCES mkt.properties(property_id),
  FOREIGN KEY (competitor_id) REFERENCES mkt.properties(property_id)
);

INSERT INTO mkt.property_competitors (property_id, competitor_id)
VALUES
  ('739085', 'boylston'),
  ('739085', 'bower'),
  ('739085', 'vanness');
