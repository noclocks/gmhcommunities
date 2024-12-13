-- Insert initial leasing week
INSERT INTO mkt.leasing_week (start_date) VALUES ('2024-01-01');

-- Generate leasing weeks in batches
DO $$
DECLARE
    batch_start DATE := '2024-01-08'; -- Start from the next week after the initial insert
    batch_end DATE := '2024-03-31';  -- End date for generating weeks
BEGIN
    WHILE batch_start <= batch_end LOOP
        INSERT INTO mkt.leasing_week (start_date)
        VALUES (batch_start)
        ON CONFLICT DO NOTHING; -- Avoid duplicate entries
        -- Increment the batch_start date by 7 days
        batch_start := batch_start + INTERVAL '7 days';
    END LOOP;
END $$;

DO $$
DECLARE
    batch_start DATE := '2024-04-01'; -- Start from the next week after the initial insert
    batch_end DATE := '2024-06-30';  -- End date for generating weeks
BEGIN
    WHILE batch_start <= batch_end LOOP
        INSERT INTO mkt.leasing_week (start_date)
        VALUES (batch_start)
        ON CONFLICT DO NOTHING; -- Avoid duplicate entries
        -- Increment the batch_start date by 7 days
        batch_start := batch_start + INTERVAL '7 days';
    END LOOP;
END $$;

DO $$
DECLARE
    batch_start DATE := '2024-07-01'; -- Start from the next week after the initial insert
    batch_end DATE := '2024-09-30';  -- End date for generating weeks
BEGIN
    WHILE batch_start <= batch_end LOOP
        INSERT INTO mkt.leasing_week (start_date)
        VALUES (batch_start)
        ON CONFLICT DO NOTHING; -- Avoid duplicate entries
        -- Increment the batch_start date by 7 days
        batch_start := batch_start + INTERVAL '7 days';
    END LOOP;
END $$;

DO $$
DECLARE
    batch_start DATE := '2024-10-01'; -- Start from the next week after the initial insert
    batch_end DATE := '2024-12-31';  -- End date for generating weeks
BEGIN
    WHILE batch_start <= batch_end LOOP
        INSERT INTO mkt.leasing_week (start_date)
        VALUES (batch_start)
        ON CONFLICT DO NOTHING; -- Avoid duplicate entries
        -- Increment the batch_start date by 7 days
        batch_start := batch_start + INTERVAL '7 days';
    END LOOP;
END $$;

-- Verify the table data
SELECT * FROM mkt.leasing_week ORDER BY start_date;

-- Verify the is_current flag
SELECT * FROM mkt.leasing_week WHERE is_current = TRUE;
