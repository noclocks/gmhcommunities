-- Auth Logs
CREATE TABLE logs.auth (
    log_id SERIAL PRIMARY KEY,
    user_id INT REFERENCES auth.users(user_id),
    action TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
