DROP TABLE IF EXISTS mkt.locations CASCADE;

CREATE TABLE IF NOT EXISTS mkt.locations (
    location_id               INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    property_id               TEXT REFERENCES mkt.properties(property_id) ON UPDATE CASCADE ON DELETE SET NULL,
    property_name             TEXT NOT NULL,
    is_competitor             BOOLEAN NOT NULL,
    address                   TEXT NOT NULL,
    city                      TEXT,
    state                     TEXT,
    postal_code               TEXT,
    country                   TEXT NOT NULL DEFAULT 'USA',
    region                    TEXT,
    phone_number              TEXT,
    email                     TEXT,
    property_image            TEXT,
    latitude                  NUMERIC(9,6) NOT NULL CHECK (latitude BETWEEN -90 AND 90),
    longitude                 NUMERIC(9,6) NOT NULL CHECK (longitude BETWEEN -180 AND 180),
    location_geometry         GEOGRAPHY(POINT, 4326) GENERATED ALWAYS AS (ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)) STORED,
    gmaps_address             TEXT,
    gmaps_name                TEXT,
    gmaps_place_id            TEXT,
    gmaps_rating              NUMERIC(2,1) CHECK (gmaps_rating >= 0 AND gmaps_rating <= 5),
    gmaps_num_of_reviews      INT CHECK (gmaps_num_of_reviews >= 0),
    gmaps_place_types         TEXT,
    gmaps_icon                TEXT,
    gmaps_website             TEXT,
    gmaps_url                 TEXT,
    map_layer                 TEXT,
    map_marker_icon           TEXT,
    map_marker_icon_color     TEXT NOT NULL DEFAULT 'white',
    map_marker_color          TEXT,
    map_popup_html            TEXT,
    created_at                TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at                TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_locations_geog ON mkt.locations USING GIST (location_geometry);
CREATE INDEX IF NOT EXISTS idx_locations_property_id ON mkt.locations(property_id);
