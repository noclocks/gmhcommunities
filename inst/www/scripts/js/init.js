var init = function () {

  // Add custom JS here
  $(document).on("shiny:connected", function (event) {
    console.log("(init.js): Shiny App Connected");
  });

  $(document).on("shiny:sessioninitialized", function (event) {
    console.log("(init.js): Shiny App Session Initialized");
  });

  $(document).on("shiny:disconnected", function (event) {
    console.log("(init.js): Shiny App Disconnected");
  });

  $(document).on("shiny:busy", function (event) {
    console.log("(init.js): Shiny App Busy");
  });

  $(document).on("shiny:idle", function (event) {
    console.log("(init.js): Shiny App Idle");
  });

};

export { init };
