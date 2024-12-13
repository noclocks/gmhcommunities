CREATE TABLE mkt.tenant_properties (
  tenant_id INT NOT NULL REFERENCES auth.tenants(tenant_id),
  property_id TEXT NOT NULL REFERENCES mkt.properties(property_id),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (tenant_id, property_id)
);
