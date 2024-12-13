CREATE TABLE mkt.survey_submissions (
    submission_id SERIAL PRIMARY KEY,
    property_id TEXT,
    submitted_by INT,
    submission_date DATE,
    leasing_week_id INT,
    FOREIGN KEY (property_id) REFERENCES mkt.properties(property_id),
    FOREIGN KEY (submitted_by) REFERENCES auth.users(user_id),
    FOREIGN KEY (leasing_week_id) REFERENCES mkt.leasing_week(leasing_week_id)
);
