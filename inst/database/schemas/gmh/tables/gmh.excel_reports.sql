CREATE TABLE gmh.excel_reports (
  report_id SERIAL PRIMARY KEY,
  report_name TEXT NOT NULL,
  report_file_name TEXT NOT NULL,
  report_data BYTEA NOT NULL,
  report_version INTEGER DEFAULT 1,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
