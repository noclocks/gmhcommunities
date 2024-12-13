CREATE TABLE mkt.fees (
    fee_id SERIAL PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    leasing_week_id INT REFERENCES mkt.leasing_weeks(leasing_week_id),
    five_month_term_available BOOLEAN,
    five_month_term_premium DECIMAL(5,2),
    five_month_term_quantity INT,
    ten_month_term_available BOOLEAN,
    ten_month_term_premium DECIMAL(5,2),
    ten_month_term_quantity INT
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
