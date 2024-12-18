// star-rating-binding.js
var starRatingBinding = new Shiny.InputBinding();

$.extend(starRatingBinding, {
  find: function(scope) {
    return $(scope).find(".star-rating-input");
  },
  getValue: function(el) {
    return $(el).attr("data-value");
  },
  setValue: function(el, value) {
    $(el).attr("data-value", value);
    this._updateStars(el);
  },
  subscribe: function(el, callback) {
    $(el).on("click.starRating", ".star-icon", function(e) {
      $(el).attr("data-value", $(this).data("value"));
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".starRating");
  }
});

Shiny.inputBindings.register(starRatingBinding);
