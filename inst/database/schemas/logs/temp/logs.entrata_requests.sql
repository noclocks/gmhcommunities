-- Entrata Request/Response Logs
CREATE TABLE logs.entrata_requests (
    request_id SERIAL PRIMARY KEY,
    endpoint TEXT NOT NULL,
    request_payload JSONB,
    response_payload JSONB,
    status_code INT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
