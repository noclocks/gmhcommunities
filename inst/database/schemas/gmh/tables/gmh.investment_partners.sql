-- Schema: gmh
-- Table: investment_partners

CREATE TABLE gmh.investment_partners (
    partner_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    partner_name TEXT NOT NULL,
    tenant_id UUID REFERENCES auth.tenants(tenant_id) ON DELETE CASCADE,
    portfolio_id UUID REFERENCES gmh.portfolios(portfolio_id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    modified_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.investment_partners IS 'GMH Equity Investment Partners.';
COMMENT ON COLUMN gmh.investment_partners.partner_id IS 'Unique identifier for the investment partner.';
COMMENT ON COLUMN gmh.investment_partners.tenant_id IS 'Tenant identifier for the investment partner.';
COMMENT ON COLUMN gmh.investment_partners.portfolio_id IS 'Portfolio identifier for the investment partner.';
COMMENT ON COLUMN gmh.investment_partners.name IS 'Name of the investment partner.';
COMMENT ON COLUMN gmh.investment_partners.created_at IS 'Timestamp when the investment partner was created.';
COMMENT ON COLUMN gmh.investment_partners.modified_at IS 'Timestamp when the investment partner was last modified.';
