# Market Survey API

## Endpoints

### Survey Management

- `POST` `/api/surveys` - Create a new survey.
- `GET` `/api/surveys` - Retrieve all surveys.
- `GET` `/api/surveys/:id` - Retrieve a single survey.
- `PUT` `/api/surveys/:id` - Update a survey.
- `DELETE` `/api/surveys/:id` - Delete a survey.

- `GET` `/api/surveys/:property_id` - Retrieve all surveys for a property.
- `GET` `/api/surveys/:tenant_id` - Retrieve all surveys for a tenant.
- `GET` `/api/surveys/:id/sections` - Retrieve all sections for a survey.

### Survey Responses

- `POST` `/api/surveys/:id/responses` - Create a new survey response.
- `GET` `/api/surveys/:id/responses` - Retrieve all responses for a survey.

### Survey Sections

- `POST` `/api/surveys/:id/sections` - Create a new survey section.
- `GET` `/api/surveys/:id/sections` - Retrieve all sections for a survey.

### Survey Analytics

- `GET` `/api/surveys/:id/analytics` - Retrieve analytics for a survey.

  
