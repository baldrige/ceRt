// Google Analytics (GA4) for supremecourt.report.
//
// Loaded on every page via a single line in <head>:
//     <script async src='/analytics.js'></script>
//
// Keeping the whole gtag bootstrap (and the Measurement ID) in this one file
// means any later change -- a new stream, consent mode, IP handling -- is a
// one-file edit that needs NO page re-render. The root-absolute src resolves
// from both the site root and /cases/ pages (custom domain served at root).
(function () {
  var ID = 'G-R7NQL34H3X';
  var s = document.createElement('script');
  s.async = true;
  s.src = 'https://www.googletagmanager.com/gtag/js?id=' + ID;
  document.head.appendChild(s);
  window.dataLayer = window.dataLayer || [];
  function gtag() { dataLayer.push(arguments); }
  window.gtag = gtag;
  gtag('js', new Date());
  gtag('config', ID);
})();
