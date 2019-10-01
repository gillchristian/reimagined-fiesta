const input = `input goes here`;

const output = `output goes here`;

const r = input
  .split('\n')
  .filter(l => /#/.test(l))
  .map(s => s.replace(/#/g, '').trim());

console.log('In:', JSON.stringify(r, null, 2));
console.log('Length:', r.length);

const rO = output
  .split('Routine')
  .map(t => t.trim())
  .filter(Boolean)
  .map(t => /[0-9]{4}-[0-9]{2}-[0-9]{2}/.exec(t))
  .map(ss => (ss ? ss[0] : ss));

console.log('Out:', JSON.stringify(rO, null, 2));
console.log('Length:', rO.length);

const missing = r.map(d => [d, rO.includes(d)]).filter(([, is]) => !is);

console.log(missing);
