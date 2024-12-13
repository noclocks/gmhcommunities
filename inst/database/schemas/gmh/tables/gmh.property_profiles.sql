CREATE TABLE gmh.property_profiles (
  property_id BIGINT PRIMARY KEY REFERENCES gmh.properties(property_id),
  profile_name TEXT,
  profile_image_url TEXT,
  profile_icon_url TEXT,
  profile_website_url TEXT,
  profile_booking_url TEXT,
  profile_description TEXT,
  profile_phone TEXT,
  profile_email TEXT,
  profile_address TEXT
);
