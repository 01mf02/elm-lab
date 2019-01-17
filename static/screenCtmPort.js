function addScreenCtmPort(app) {
  app.ports.requestScreenCtm.subscribe(function(id) {
    let svg = document.getElementById(id);
    if (svg !== null) {
      let ctm = svg.getScreenCTM().inverse();
      //console.log(ctm);
      app.ports.receiveScreenCtm.send({
        a: ctm.a, b: ctm.b, c: ctm.c, d: ctm.d, e: ctm.e, f: ctm.f
      });
    } else {
      app.ports.receiveScreenCtm.send({
        error : true
      });
    }
  });
}
