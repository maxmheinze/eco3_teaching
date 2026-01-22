// plotly-resize.js
(function () {
  function resizeAll() {
    document.querySelectorAll('.js-plotly-plot').forEach(el => {
      try { Plotly.Plots.resize(el); } catch (_) {}
    });
  }
  Reveal.on('ready', resizeAll);
  Reveal.on('slidechanged', resizeAll);
  Reveal.on('resize', resizeAll);
  window.addEventListener('resize', resizeAll);
})();

