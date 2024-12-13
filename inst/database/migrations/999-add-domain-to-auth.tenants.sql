-- Add the `domain` column to `auth.tenants`
ALTER TABLE auth.tenants
ADD COLUMN domain TEXT UNIQUE;

-- Optional: Update existing tenants with sample domains
UPDATE auth.tenants
SET domain = CASE
    WHEN name = 'GMH Communities' THEN 'gmhcommunities.com'
    WHEN name = 'No Clocks' THEN 'noclocks.dev'
    ELSE NULL
END;

-- Ensure no NULL values for the `domain` column if required
ALTER TABLE auth.tenants
ALTER COLUMN domain SET NOT NULL;
