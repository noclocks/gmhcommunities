DROP TABLE IF EXISTS mkt.leasing_week CASCADE;

-- Create the leasing_week table
CREATE TABLE mkt.leasing_weeks (
    leasing_week_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    start_date DATE NOT NULL,                  -- Start date of the leasing week
    end_date DATE GENERATED ALWAYS AS (start_date + INTERVAL '6 days') STORED, -- Calculated end date
    week_number INT GENERATED ALWAYS AS (EXTRACT(WEEK FROM start_date)) STORED, -- Week number
    year INT GENERATED ALWAYS AS (EXTRACT(YEAR FROM start_date)) STORED,        -- Year of the leasing week
    quarter INT GENERATED ALWAYS AS (EXTRACT(QUARTER FROM start_date)) STORED, -- Quarter of the leasing week
    fiscal_week INT GENERATED ALWAYS AS (
    CASE
        WHEN EXTRACT(MONTH FROM start_date) >= 10 THEN
            EXTRACT(WEEK FROM start_date) - EXTRACT(WEEK FROM DATE '2024-10-01') + 1
        ELSE
            EXTRACT(WEEK FROM start_date) + (52 - EXTRACT(WEEK FROM DATE '2024-10-01')) + 1
    END
    ) STORED,
    is_current BOOLEAN DEFAULT FALSE           -- Indicates the current leasing week
);
