// address-binding.js
var addressBinding = new Shiny.InputBinding();

$.extend(addressBinding, {
  find: function(scope) {
    return $(scope).find(".address-input");
  },
  initialize: function(el) {
    var autocomplete = new google.maps.places.Autocomplete(el);
    autocomplete.addListener("place_changed", function() {
      $(el).trigger("change");
    });
  },
  getValue: function(el) {
    return {
      value: $(el).val(),
      place: $(el).data("place")
    };
  },
  subscribe: function(el, callback) {
    $(el).on("change.addressInput", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".addressInput");
  }
});

Shiny.inputBindings.register(addressBinding);
