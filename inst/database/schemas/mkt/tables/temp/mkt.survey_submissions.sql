CREATE TABLE mkt.survey_submissions (
  submission_id SERIAL PRIMARY KEY,
  property_id TEXT NOT NULL REFERENCES mkt.properties(property_id),
  leasing_week_id INT NOT NULL REFERENCES mkt.leasing_weeks(leasing_week_id),
  submitted_by INT NOT NULL REFERENCES auth.users(user_id),
  submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
