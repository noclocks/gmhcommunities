-- Survey Logs
CREATE TABLE logs.survey_responses (
    log_id SERIAL PRIMARY KEY,
    survey_id INT REFERENCES mkt.surveys(survey_id),
    user_id INT REFERENCES auth.users(user_id),
    changes JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
