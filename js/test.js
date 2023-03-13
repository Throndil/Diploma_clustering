// Start of the tx-annotate.js file
function test(el) {
    el.on("plotly_click", function(d) {
      var pt = d.points[0];
      var cd = pt.customdata;
      var num = cd[1] ? cd[1] : "No";
      console.log(num, cd[0]);
    });
  }
  