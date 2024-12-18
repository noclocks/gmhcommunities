function initAutocomplete(inputId) {

    var autocomplete = new google.maps.places.Autocomplete(document.getElementById(inputId));

    autocomplete.addListener('place_changed', function () {
        var place = autocomplete.getPlace();
        Shiny.setInputValue('jsName', place.name);
        Shiny.setInputValue('jsAddress', place.formatted_address);
    });

}
