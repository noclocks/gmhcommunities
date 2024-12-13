/*
property_id,property_name,leasing_week_start,five_month_term_available,five_month_term_premium,five_month_term_quantity,ten_month_term_available,ten_month_term_premium,ten_month_term_quantity
739085,1047 Commonwealth Avenue,2024-12-01,FALSE,0,NA,FALSE,0,NA
boylston,1330 Boylston,2024-12-01,FALSE,0,NA,FALSE,0,NA
bower,Bower,2024-12-01,FALSE,0,NA,FALSE,0,NA
vanness,Van Ness,2024-12-01,FALSE,0,NA,FALSE,0,NA
*/

CREATE TABLE mkt.short_term_leases (
    lease_id SERIAL PRIMARY KEY,
    property_id TEXT REFERENCES mkt.properties(property_id),
    leasing_week_id REFERENCES mkt.leasing_weeks(leasing_week_id),
    lease_term_months INT NOT NULL CHECK (lease_term_months IN (5, 10)),
    is_available BOOLEAN DEFAULT FALSE,
    premium DECIMAL(10, 2) DEFAULT 0.00,
    quantity INT DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
