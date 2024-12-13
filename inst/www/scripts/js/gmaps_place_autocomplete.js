function initAutocomplete() {
    var autocomplete = new google.maps.places.Autocomplete(document.getElementById('my_address'), { types: ['geocode'] });
    autocomplete.setFields(['address_components', 'formatted_address', 'geometry', 'icon', 'name']);
    autocomplete.addListener('place_changed', function () {
        var place = autocomplete.getPlace();
        if (!place.geometry) {
            return;
        }

        var addressPretty = place.formatted_address;
        var address = '';
        if (place.address_components) {
            address = [
                (place.address_components[0] && place.address_components[0].short_name || ''),
                (place.address_components[1] && place.address_components[1].short_name || ''),
                (place.address_components[2] && place.address_components[2].short_name || ''),
                (place.address_components[3] && place.address_components[3].short_name || ''),
                (place.address_components[4] && place.address_components[4].short_name || ''),
                (place.address_components[5] && place.address_components[5].short_name || ''),
                (place.address_components[6] && place.address_components[6].short_name || ''),
                (place.address_components[7] && place.address_components[7].short_name || '')
            ].join(' ');
        }
        var address_number = ''
        address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
        var coords = place.geometry.location;
        //console.log(address);
        Shiny.onInputChange('jsValue', address);
        Shiny.onInputChange('jsValueAddressNumber', address_number);
        Shiny.onInputChange('jsValuePretty', addressPretty);
        Shiny.onInputChange('jsValueCoords', coords);
    });
}
