function addCssPropPort(app) {
  app.ports.setCssProp.subscribe(([selector, prop, value]) => {
    for (const $el of document.querySelectorAll(selector)) {
      $el.style.setProperty(prop, value);
    }
  });
}
