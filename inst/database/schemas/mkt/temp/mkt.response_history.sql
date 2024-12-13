CREATE TABLE mkt.response_history (
    history_id SERIAL PRIMARY KEY,
    response_id INT REFERENCES mkt.survey_responses(response_id) ON DELETE CASCADE,
    field_name TEXT NOT NULL,
    old_value TEXT,
    new_value TEXT,
    changed_by INT REFERENCES auth.users(user_id), -- User who made the change
    changed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
