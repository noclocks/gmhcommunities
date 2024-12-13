# HTTP Mocked Responses

> [!NOTE]
> This folder is used during testing to mock HTTP responses.

## Folders

The folders in this directory are named after the API endpoints they mock.

GMH Communities Entrata API Naming Conventions:

```plaintext
gmhcommunities.entrata.com/<endpoint>/<endpoint>-<method>-<requestId>-<params>.json
```

for example, a mocked response for the `/properties` endpoint using `getProperties` method and
request ID of `"99"` and no method params:

```plaintext
gmhcommunities.entrata.com/properties/properties-getProperties-99-noParams.json
```

The JSON files in this folder should contain the mocked responses for the corresponding HTTP requests.
