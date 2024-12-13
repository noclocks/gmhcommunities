-- Audit Logs
CREATE TABLE logs.audit (
    log_id SERIAL PRIMARY KEY,
    table_name TEXT NOT NULL,
    action TEXT NOT NULL,
    user_id INT REFERENCES auth.users(user_id),
    old_value JSONB,
    new_value JSONB,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

