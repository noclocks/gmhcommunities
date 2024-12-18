// email-binding.js
var emailBinding = new Shiny.InputBinding();

$.extend(emailBinding, {
  find: function(scope) {
    return $(scope).find(".email-input");
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.emailInput", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".emailInput");
  }
});

Shiny.inputBindings.register(emailBinding);
