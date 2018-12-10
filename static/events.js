// returns the youngest ancestor with a given tag
// (or null if no ancestor has the tag)
function ancestorWithTag(node, tag) {
  if (node == null)
    return null;
  if (node.tagName === tag)
    return node;
  else
    return ancestorWithTag(node.parentNode, tag);
}

var observer = new MutationObserver(function (mutations) {
  mutations.forEach(function (mutation) {
    if (mutation.type === 'childList') {
      Array
        .from(mutation.addedNodes)
        .filter(function (node) {
          return node.tagName === 'svg'; 
        })
        .forEach(function (node) {
	  if (node.tagName === 'svg') {
            var pt = node.createSVGPoint();
            node.addEventListener('mousemove', function (event) {
              pt.x = event.clientX;
              pt.y = event.clientY;
              pt = pt.matrixTransform(node.getScreenCTM().inverse());
              //alert(pt.x);
              var svgClickEvent = new CustomEvent('svgmousemove', {
                detail: {
                  x: pt.x,
                  y: pt.y
                }
              });
              event.currentTarget.dispatchEvent(svgClickEvent);
            });
	  }

	  var svg = ancestorWithTag(node, "svg");
	  if (svg == null)
	    return;

	  pt = svg.createSVGPoint();
          node.addEventListener('click', function (event) {                
            pt.x = event.clientX;
            pt.y = event.clientY;
            pt = pt.matrixTransform(node.getScreenCTM().inverse());
            //alert(pt.x);
            var svgClickEvent = new CustomEvent('svgclick', {
              detail: {
                x: pt.x,
                y: pt.y
              }
            });
            event.currentTarget.dispatchEvent(svgClickEvent);
          });

        });
    }
  });
});

observer.observe(document.body, { childList: true, subtree: true });
