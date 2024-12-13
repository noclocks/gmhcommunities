DROP TABLE IF EXISTS mkt.surveys;

CREATE TABLE IF NOT EXISTS mkt.surveys (
    survey_id INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    user_id INT REFERENCES auth.users(user_id),
    leasing_week_id INT REFERENCES mkt.leasing_weeks(leasing_week_id),
    survey_date DATE NOT NULL,
    survey_status TEXT NOT NULL CHECK (status IN ('Draft', 'Submitted', 'Approved')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
