DROP TABLE IF EXISTS mkt.history;

CREATE TABLE IF NOT EXISTS mkt.history (
  history_id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  response_id INT REFERENCES mkt.responses(response_id) ON DELETE CASCADE,
  field_name TEXT NOT NULL,
  prior_value TEXT,
  new_value TEXT,
  changed_by INT REFERENCES auth.users(user_id) ON DELETE SET NULL,
  changed_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
):
