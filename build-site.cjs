// Compile the Markdown sources using Pandoc, preserving the site's existing navigation.
const fs = require('node:fs');
const cp = require('node:child_process');
process.chdir(__dirname);
const pages = [['index', 'Home'], ['slides', 'Slides'], ['workshop', 'Workshop'],
  ['code', 'Code'], ['data', 'Data'], ['resources', 'Resources'], ['codebook', 'Codebook']];
const escape = s => s.replaceAll('&', '&amp;').replaceAll('"', '&quot;').replaceAll('<', '&lt;');
const footer = 'EPRC Methods Workshop Series · University of Florida · October 22, 2026';
for (const [slug, name] of pages) {
  const input = slug === 'workshop' ? 'workshop-generated.md' : `${slug}.qmd`;
  const numbers = [];
  // Keep explicit LaTeX tags in the download, but render HTML numbers outside MathML.
  const source = fs.readFileSync(input, 'utf8').replace(/\n\\tag\{(\d+)\}/g, (_, n) => {
    numbers.push(Number(n));
    return '';
  });
  let body = cp.execFileSync('pandoc', ['-f', 'markdown', '-t', 'html5',
    '--wrap=none', '--mathml', '--syntax-highlighting=pygments'], {encoding: 'utf8', input: source});
  body = body.replace(/<p>(<math display="block"[\s\S]*?<\/math>)<\/p>/g,
    '<div class="equation">$1</div>');
  if (numbers.length) {
    let index = 0;
    body = body.replace(/<div class="equation">(<math[\s\S]*?<\/math>)<\/div>/g, (_, math) => {
      const n = numbers[index++];
      if (n !== index) throw new Error('Equation numbering must be consecutive');
      return `<div class="numbered-equation" id="equation-${n}"><div class="equation">${math}</div><span class="equation-number" aria-label="Equation ${n}">(${n})</span></div>`;
    });
    if (index !== numbers.length) throw new Error('An equation did not render');
  }
  if (!/<h1\b/.test(body) && slug !== 'workshop') body = `<h1>${name}</h1>\n` + body;
  const headings = [...body.matchAll(/<h([23]) id="([^"]+)">([\s\S]*?)<\/h\1>/g)]
    .filter(h => h[1] === '2' || /^([89]\.\d|Inference|Assess Parallel Trends)/.test(h[3]))
    .map(h => [h[0], h[2], h[3], h[1]]);
  const toc = slug === 'workshop' ? `<aside class="lab-toc" aria-label="On this page"><strong>In this lab</strong><nav>${headings.map(h => `<a href="#${escape(h[1])}"${h[3] === '3' ? ' class="toc-subsection"' : ''}>${h[2]}</a>`).join('')}</nav></aside>` : '';
  const nav = pages.slice(0,6).map(([p,n]) => `<a href="${p}.html"${p === slug ? ' aria-current="page"' : ''}>${n}</a>`).join('\n')
    + '\n<a href="https://docs.google.com/forms/d/e/1FAIpQLSfQf3xoxfmVeBs_7whEkxC_gdUGPMZTuGgbZzwtnPwMSvIz3w/viewform">Survey</a>';
  const html = `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>${name} | EPRC DID Workshop</title><link rel="stylesheet" href="styles.css">
<link rel="stylesheet" href="syntax.css"><script src="workshop.js" defer></script></head>
<body><a class="skip-link" href="#main">Skip to content</a>
<header class="site-header"><nav class="nav" aria-label="Main navigation">
<a class="brand" href="index.html"><span class="brand-mark">DID</span><span>EPRC Workshop</span></a>
<div class="nav-links">${nav}</div></nav></header>
<main id="main" class="document ${slug === 'workshop' ? 'lab-layout' : ''}">
${toc}<article>${body}</article></main><footer class="footer"><div class="footer-inner">${footer}</div></footer></body></html>`;
  fs.writeFileSync(`${slug}.html`, html.replace(/[\t ]+$/gm, ''));
}
// Pandoc emits token classes; a small stylesheet keeps all code readable without JS.
fs.writeFileSync('syntax.css', 'code span.co {color:#54636e;} code span.st {color:#12643d;} code span.fu {color:#194f9a;} code span.kw,code span.cf {color:#a22e58;font-weight:600;} code span.dv,code span.fl {color:#8b4312;} code span.op {color:#323b45;}\n');
console.log(`Built ${pages.length} pages.`);
