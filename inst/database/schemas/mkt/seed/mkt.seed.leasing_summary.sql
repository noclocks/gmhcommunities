/*
CREATE TABLE mkt.leasing_summary (
    summary_id SERIAL PRIMARY KEY,
    submission_id INT REFERENCES mkt.survey_submissions(submission_id),
    property_id TEXT REFERENCES mkt.properties(property_id),
    leasing_week_id INT REFERENCES mkt.leasing_weeks(leasing_week_id),
    reporting_cycle TEXT CHECK (reporting_cycle IN ('Monday-Sunday', 'Saturday-Friday', 'Sunday-Saturday')) DEFAULT 'Monday-Sunday',
    lease_launch_date DATE,
    renewal_launch_date DATE,
    current_occupancy DECIMAL(5,2),
    last_year_occupancy DECIMAL(5,2),
    current_pre_lease DECIMAL(5,2),
    last_year_pre_lease DECIMAL(5,2),
    total_renewals INT,
    total_new_leases INT,
    total_leases_weekly INT,
    traffic_weekly INT,
    current_incentive TEXT DEFAULT 'None',
    incentive_amount DECIMAL(5,2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
*/
