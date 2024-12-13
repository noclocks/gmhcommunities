function keepAlive() {
  Shiny.onInputChange('keepAlive', Math.random());
}

// run keepAlive every 60 seconds
setInterval(keepAlive, 60000);
