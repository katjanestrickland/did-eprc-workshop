document.querySelectorAll('div.sourceCode').forEach((block, index) => {
  const pre = block.querySelector('pre');
  if (!pre) return;
  const button = document.createElement('button');
  button.type = 'button';
  button.className = 'copy-code';
  button.textContent = 'Copy';
  button.title = 'Copy R code';
  button.setAttribute('aria-label', `Copy R code block ${index + 1}`);
  const status = document.createElement('span');
  status.className = 'copy-status';
  status.setAttribute('aria-live', 'polite');
  button.addEventListener('click', async () => {
    try {
      await navigator.clipboard.writeText(pre.textContent);
      status.textContent = 'Copied';
    } catch {
      const range = document.createRange();
      range.selectNodeContents(pre);
      const selection = window.getSelection();
      selection.removeAllRanges();
      selection.addRange(range);
      status.textContent = 'Code selected; use your browser copy command';
    }
    setTimeout(() => { status.textContent = ''; }, 3000);
  });
  block.prepend(button, status);
});
document.querySelectorAll('article table').forEach(table => {
  const wrap = document.createElement('div');
  wrap.className = 'table-wrap';
  wrap.tabIndex = 0;
  table.before(wrap);
  wrap.append(table);
});
